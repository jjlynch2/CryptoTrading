#connect to websocket
binance_websocket_connect <- function(websocket_url_binance) {
	ws_binance <- WebSocket$new(websocket_url_binance)
	ws_binance$onClose(function(event) {
		cat("Client disconnected\n")
		start_binance_websocket() #restart if closed
	})
	ws_binance$onOpen(function(event) {
		cat("Client connected\n")
	})		
	return(ws_binance)	
}

#disconnect from websocket
binance_websocket_disconnect <- function(ws_binance) {
	ws_binance$close()
}

#construct json to send
construct_binance_json <- function(binance_ticker) {
	qu <- NULL
	for(i in 1:length(binance_ticker)) {
		temp <- paste(
			#paste(tolower(binance_ticker[i]),"@","aggTrade",sep=""),
			#'","',
			paste(tolower(binance_ticker[i]),"@","bookTicker",sep=""),
		sep = "")
		qu <- c(qu, temp)
	}
	qu <- paste(qu, collapse='","')

	binance_json <- paste(
		'{"method":"SUBSCRIBE",',
		'"params":["', 
		qu,
		'"],',
		'"id":',
		1,
		'}', 
		sep = "")
	return(binance_json)
}

#subscribe to channel
subscribe_binance <- function(ws_binance, json) {
	ws_binance$send(json)
}

#start websocket stream and save to database
start_binance_websocket <- function(db = BD:::db, host_db = BD:::host_db, db_port=BD:::db_port, db_user=BD:::db_user, db_password=BD:::db_password, binance_ticker=BD:::binance_ticker, websocket_url_binance=BD:::websocket_url_binance) {
	#connect to websocket
	qu <- NULL
	for(i in 1:length(binance_ticker)) {
		#qu <- c(qu, paste(binance_ticker[i], "@aggTrade/", binance_ticker[i], "@bookTicker", sep=""))
		qu <- c(qu, paste(binance_ticker[i], "@bookTicker", sep=""))
	}
	qu <- paste(qu, collapse="/")
	
	wss <- paste(websocket_url_binance, "/stream?streams=", qu, sep="")
	
	ws_binance <- binance_websocket_connect(wss)
	Sys.sleep(5)
	#construct query
	ws_construct <- construct_binance_json(binance_ticker)
	#subscribe to channel
	subscribe_binance(ws_binance, ws_construct)
	#connect to database
	con<-dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)
	#wait for messages and write to database
	ws_binance$onMessage(function(event) {
		socket_data_temp <- fromJSON(event$data, simplify = TRUE)
		#if(any(socket_data_temp[[1]] == paste(tolower(binance_ticker), "@", "aggTrade", sep=""))) {
		#	socket_data <- as.data.frame(socket_data_temp[[2]])
		#	socket_data$T <- as.POSIXct(socket_data$T/1000, origin="1970-01-01", tz = "UTC")
		#	coln <- colnames(socket_data)
		#	coln <- c(coln[1:8], "time", coln[10:11])
		#	colnames(socket_data) <- coln
		#	if(any(dbListTables(con) == "binance_ticker")) {
		#		dbWriteTable(con, "binance_ticker", socket_data, append = TRUE)
		#	} else {
		#		dbWriteTable(con, "binance_ticker", socket_data)
		#	}
		#} else if (any(socket_data_temp[[1]] == paste(tolower(binance_ticker), "@", "bookTicker", sep=""))) {
		if (any(socket_data_temp[[1]] == paste(tolower(binance_ticker), "@", "bookTicker", sep=""))) {
			socket_data <- as.data.frame(socket_data_temp[[2]])
			dbWriteTable(con, paste(binance_ticker[which(paste(tolower(binance_ticker), "@", "bookTicker", sep="") == socket_data_temp[[1]])], "_bookticker", sep=""), socket_data, overwrite = TRUE)
		}
	})
}


#pull latest candle from database!
ws_binance_ticker <- function(con, binance_ticker, granularity_limit) {
	time_filter <- .POSIXct(Sys.time()-granularity_limit, "UTC")
	parsed_results <- dbSendQuery(con, 'SELECT * FROM binance_ticker WHERE time >= $1 and s = $2', list(time_filter, binance_ticker))
	results <- dbFetch(parsed_results)
	dbClearResult(parsed_results)
	return(results)
}

#pull best bid ask from database
ws_binance_bookticker <- function(con, binance_ticker) {
	results <- dbReadTable(con, paste(binance_ticker, "_bookticker", sep=""))
	return(results)
}

ws_trade_data_binance_rolling <- function(con, binance_ticker, granularity_limit) {
	current_trades <- ws_binance_ticker(con, binance_ticker, granularity_limit)
	volume_trade <- round(sum(as.numeric(current_trades$q)), digits = 6)
	#determines candle
	if(nrow(current_trades) == 0) {
		print("DATABASE OUT OF DATE")
	}
#MAKE SURE THIS IS CORRECT	

	mt <- which(current_trades$time == max(current_trades$time))
	mit <- which(current_trades$time == min(current_trades$time))
	if(mt[1] > mit[1]) {
		close_trade <- as.numeric(current_trades[nrow(current_trades),5])
		open_trade <- as.numeric(current_trades[1,5])
	} else {
		close_trade <- as.numeric(current_trades[1,5])
		open_trade <- as.numeric(current_trades[nrow(current_trades),5])
	}

	candle <- NULL
	if(close_trade > open_trade) {candle <- "green"} #calculate if last trade bucket was green
	if(close_trade < open_trade) {candle <- "red"} #calculate if last trade bucket was red
	if(close_trade == open_trade) {candle <- "orange"}
	buy_volume <- sum(as.numeric(current_trades[current_trades$m == FALSE,6])) 
	sell_volume <- sum(as.numeric(current_trades[current_trades$m == TRUE,6])) 
	buy_sell_volume_ratio <- round((buy_volume - sell_volume) / (buy_volume + sell_volume), digits = 6) #buy_volume / sell_volume	
	return(list(volume_trade, candle, buy_sell_volume_ratio, close_trade))
}

#best bid / ask from order book
ws_trade_data_binance_rolling_best_bid_ask <- function(con, binance_ticker) {
	current_trades <- ws_binance_bookticker(con, binance_ticker)
	bid <- as.numeric(current_trades$b)
	ask <- as.numeric(current_trades$a)
	return(c(bid,ask))
}

#used to pull data from database for calculating the buy sell volume ratio from stored data for ML
ws_trade_data_binance_timeframe <- function(con, binance_ticker, granularity_limit, granularity_count) {
	current_trades <- ws_binance_ticker(con, binance_ticker, granularity_limit = granularity_count * granularity_limit) #count multiplied by limit for entire timeframe
	current_trades[,6] <- as.numeric(current_trades[,6])
	time_filter <- seq(from=.POSIXct(min(current_trades$time), "UTC"), to=.POSIXct(max(current_trades$time), "UTC"), by=paste(granularity_limit/60, "min"))
	
	if(length(time_filter) == 1) {
		df <- list(current_trades)
	} else {
		df <- split(current_trades, cut(current_trades$time, breaks = c(time_filter)))
	}
	
	buy_volume_t <- unlist(lapply(df, function(x) sum(x[x$m == FALSE,6])))
	sell_volume_t <- unlist(lapply(df, function(x) sum(x[x$m == TRUE,6])))
	
	#safety check for 0 data.
	buy_volume_t <- buy_volume_t[buy_volume_t != 0]
	sell_volume_t <- sell_volume_t[sell_volume_t != 0]
	
	total_volume_t <- buy_volume_t + sell_volume_t
	buy_sell_volume_ratio <- as.data.frame(round((buy_volume_t - sell_volume_t) / total_volume_t, digits=6))
	return(buy_sell_volume_ratio)
}