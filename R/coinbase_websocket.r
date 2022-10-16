#connect to websocket
coinbase_websocket_connect_ticker <- function(websocket_url_coinbase) {
	ws_coinbase <- WebSocket$new(websocket_url_coinbase)
	ws_coinbase$onClose(function(event) {
		cat("Client disconnected\n")
		start_coinbase_websocket_ticker() #restart if closed
	})
	ws_coinbase$onOpen(function(event) {
		cat("Client connected\n")
	})		
	return(ws_coinbase)	
}

#connect to websocket
coinbase_websocket_connect_orderbook <- function(websocket_url_coinbase) {
	ws_coinbase <- WebSocket$new(websocket_url_coinbase)
	ws_coinbase$onClose(function(event) {
		cat("Client disconnected\n")
		start_coinbase_websocket_orderbook() #restart if closed
	})
	ws_coinbase$onOpen(function(event) {
		cat("Client connected\n")
	})		
	return(ws_coinbase)	
}

#disconnect from websocket
coinbase_websocket_disconnect <- function(ws_coinbase) {
	ws_coinbase$close()
}

#construct json to send
construct_coinbase_json <- function(coinbase_ticker, channel) {
	coinbase_json <- paste(
		'{"type":"subscribe",',
		'"product_ids":["', 
		coinbase_ticker, 
		'"],',
		'"channels":["',
		channel,
		'"]}', 
		sep = "")
	return(coinbase_json)
}

#subscribe to channel
subscribe_coinbase <- function(ws_coinbase, json) {
	ws_coinbase$send(json)
}

#start websocket ticker stream and save to database
start_coinbase_websocket_ticker <- function(db = BD:::db, host_db = BD:::host_db, db_port=BD:::db_port, db_user=BD:::db_user, db_password=BD:::db_password, coinbase_ticker=BD:::coinbase_ticker, websocket_url_coinbase=BD:::websocket_url_coinbase){
	#connect to websocket
	ws_coinbase_1 <- coinbase_websocket_connect_ticker(websocket_url_coinbase)
	Sys.sleep(5)
	#construct query
	ws_construct_1 <- construct_coinbase_json(coinbase_ticker, "ticker")

	#connect to database
	con<-dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)
	#wait for messages and write to database
	ws_coinbase_1$onMessage(function(event) {
		socket_data <- data.frame(fromJSON(event$data))
		socket_data$time <- as.POSIXct(strptime(socket_data$time, "%Y-%m-%dT%H:%M:%OS", tz="UTC"))
		if(any(dbListTables(con) == "coinbase_ticker")) {
			dbWriteTable(con, "coinbase_ticker", socket_data, append = TRUE)
		} else {
			dbWriteTable(con, "coinbase_ticker", socket_data)
		}
	})	
	
	#subscribe to channel
	subscribe_coinbase(ws_coinbase_1, ws_construct_1)
}

#start websocket level 2 orderbook stream and save to database
start_coinbase_websocket_orderbook <- function(db = BD:::db, host_db = BD:::host_db, db_port=BD:::db_port, db_user=BD:::db_user, db_password=BD:::db_password, coinbase_ticker=BD:::coinbase_ticker, websocket_url_coinbase=BD:::websocket_url_coinbase){
	#connect to websocket
	ws_coinbase_2 <- coinbase_websocket_connect_orderbook(websocket_url_coinbase)
	Sys.sleep(5)
	#construct query
	ws_construct_2 <- construct_coinbase_json(coinbase_ticker, "level2_50")

	#connect to database
	con<-dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)

	ws_coinbase_2$onMessage(function(event) {
		socket_data <- fromJSON(event$data)
		if(socket_data$type == "snapshot") {
			LOB_asks <- do.call(rbind.data.frame, socket_data$asks)
			LOB_bids <- do.call(rbind.data.frame, socket_data$bids)
			colnames(LOB_asks) <- c("price", "quantity")
			colnames(LOB_bids) <- c("price", "quantity")
			LOB_asks_global <<- LOB_asks
			LOB_bids_global <<- LOB_bids
		} else if (socket_data$type == "l2update") {
			changes <- do.call(rbind.data.frame, socket_data$changes)
			colnames(changes) <- c("side", "price", "quantity")
			if(any(changes$side == "sell")) {
				asks <- changes[changes$side == "sell",2:3]
				LOB_asks <- LOB_asks_global
				for(i in 1:nrow(asks)) {
					if(as.numeric(asks[i,2]) == 0) {
						LOB_asks <- LOB_asks[LOB_asks$price != asks[i,1],]
					} else if(any(LOB_asks$price == asks[i,1])) {
						LOB_asks[LOB_asks$price == asks[i,1],] <- asks[i,]
					} else {
						LOB_asks <- rbind(LOB_asks, asks[i,])
					}
				}
				dbWriteTable(con, paste(coinbase_ticker, "level2_asks", sep=""), LOB_asks, overwrite = TRUE)
				LOB_asks_global <<- LOB_asks
			} 
			
			if(any(changes$side == "buy")) {
				bids <- changes[changes$side == "buy",2:3]
				LOB_bids <- LOB_bids_global
				for(i in 1:nrow(bids)) {
					if(as.numeric(bids[i,2]) == 0) {
						LOB_bids <- LOB_bids[LOB_bids$price != bids[i,1],]
					} else if(any(LOB_bids$price == bids[i,1])) {
						LOB_bids[LOB_bids$price == bids[i,1],] <- bids[i,]
					} else {
						LOB_bids <- rbind(LOB_bids, bids[i,])
					}
				}
				dbWriteTable(con, paste(coinbase_ticker, "level2_bids", sep=""), LOB_bids, overwrite = TRUE)
				LOB_bids_global <<- LOB_bids
			}
		}
	})	
	Sys.sleep(5)
	#subscribe to channel
	subscribe_coinbase(ws_coinbase_2, ws_construct_2)
}

#pull latest candle from database!
ws_coinbase_ticker <- function(con, coinbase_ticker, granularity_limit) {
	time_filter <- .POSIXct(Sys.time()-granularity_limit, "UTC")
	parsed_results <- dbSendQuery(con, 'SELECT * FROM coinbase_ticker WHERE time >= $1 and product_id = $2', list(time_filter, coinbase_ticker))
	results <- dbFetch(parsed_results)
	dbClearResult(parsed_results)
	return(results)
}

ws_coinbase_coinbase_LOB_data <- function(con, coinbase_ticker, granularity_limit) {
	time_filter <- .POSIXct(Sys.time()-granularity_limit, "UTC")
	parsed_results <- dbSendQuery(con, 'SELECT * FROM coinbase_lob_data WHERE time >= $1 and coinbase_ticker = $2', list(time_filter, coinbase_ticker))
	results <- dbFetch(parsed_results)
	dbClearResult(parsed_results)
	return(results)
}

#helper function to save metrics for ML
save_LOB_metrics <- function(LOB_timer = BD:::LOB_timer, lobpercent = BD:::lobpercent, db = BD:::db, host_db = BD:::host_db, db_port=BD:::db_port, db_user=BD:::db_user, db_password=BD:::db_password, coinbase_ticker=BD:::coinbase_ticker) {
	con <- dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)
	repeat {
		LOB_metrics <- ws_trade_data_coinbase_rolling_LOB_metrics(con, percent = lobpercent, coinbase_ticker)
		if(any(dbListTables(con) == "coinbase_lob_data")) {
			dbWriteTable(con, "coinbase_lob_data", LOB_metrics, append = TRUE)
		} else {
			dbWriteTable(con, "coinbase_lob_data", LOB_metrics)
		}
		Sys.sleep(LOB_timer)
	}
}

#orderbook imbalance
ws_trade_data_coinbase_rolling_LOB_metrics <- function(con, percent, coinbase_ticker) {
#MAKE SURE THIS IS CORRECT	
	bid <- dbReadTable(con, paste(coinbase_ticker, "level2_bids", sep=""))
	ask <- dbReadTable(con, paste(coinbase_ticker, "level2_asks", sep=""))
	bid$price <- as.numeric(bid$price)
	bid$quantity <- as.numeric(bid$quantity)
	ask$price <- as.numeric(ask$price)
	ask$quantity <- as.numeric(ask$quantity)

	#helper variables
	best_bid <- max(bid$price)
	best_ask <- min(ask$price)
	best_bid_volume <- bid[bid$price == max(bid$price),2]
	best_ask_volume <- ask[ask$price == max(ask$price),2]
	bid_volume <- sum(bid[bid[,1] >= (best_bid - (best_bid * percent)),2])
	ask_volume <- sum(ask[ask[,1] <= (best_ask + (best_ask * percent)),2])
	bid_price <- sum(bid[bid[,1] >= (best_bid - (best_bid * percent)),1])
	ask_price <- sum(ask[ask[,1] <= (best_ask + (best_ask * percent)),1])
	
	#imbalance
	bid_ask_LOB_volume_imbalance <- (bid_volume - ask_volume) / (bid_volume + ask_volume)
	
	#differences
	bid_ask_LOB_volume_difference <- bid_volume - ask_volume  #should include signed data for directionality?
	bid_ask_LOB_best_price_spread <- best_bid - best_ask #should include signed data for directionality?
	bid_ask_LOB_price_difference <- bid_price - ask_price

	#total volume
	bid_ask_LOB_volume_total <- bid_volume + ask_volume
	bid_ask_LOB_volume_bid <- bid_volume
	bid_ask_LOB_volume_ask <- ask_volume
	
	#Level 1 price and volume
	bid_ask_LOB_best_bid_price <- best_bid
	bid_ask_LOB_best_bid_volume <- best_bid_volume
	bid_ask_LOB_best_ask_volume <- best_ask_volume
	bid_ask_LOB_best_ask_price <- best_ask

	#mean data
	bid_ask_LOB_mean_bid_price <- mean(bid[bid[,1] >= (best_bid - (best_bid * percent)),1])
	bid_ask_LOB_mean_ask_price <- mean(ask[ask[,1] <= (best_ask + (best_ask * percent)),1])
	bid_ask_LOB_mean_bid_volume <- mean(bid[bid[,1] >= (best_bid - (best_bid * percent)),2])
	bid_ask_LOB_mean_ask_volume <- mean(ask[ask[,1] <= (best_ask + (best_ask * percent)),2])
	
	#mid price
	mid_price <- (best_bid + best_ask) / 2
	time <- .POSIXct(Sys.time(),"UTC")
	LOB_metrics <- data.frame(coinbase_ticker, time, 
						bid_ask_LOB_volume_imbalance, 
						bid_ask_LOB_volume_difference, 
						bid_ask_LOB_best_price_spread, 
						bid_ask_LOB_price_difference,
						bid_ask_LOB_volume_total, 
						bid_ask_LOB_volume_bid, 
						bid_ask_LOB_volume_ask, 
						bid_ask_LOB_best_bid_price,
						bid_ask_LOB_best_bid_volume, 
						bid_ask_LOB_best_ask_volume,
						bid_ask_LOB_best_ask_price,
						bid_ask_LOB_mean_bid_price,
						bid_ask_LOB_mean_ask_price,
						bid_ask_LOB_mean_bid_volume,
						bid_ask_LOB_mean_ask_volume
	)
	return(LOB_metrics)
}

#best bid / ask from level 2 orderbook
ws_trade_data_coinbase_rolling_best_bid_ask <- function(con, coinbase_ticker) {
#MAKE SURE THIS IS CORRECT	
	bid <- dbReadTable(con, paste(coinbase_ticker, "level2_bids", sep=""))
	ask <- dbReadTable(con, paste(coinbase_ticker, "level2_asks", sep=""))
	bid <- max(as.numeric(bid$price))
	ask <- min(as.numeric(ask$price))
	return(c(bid,ask))
}

#returns rolling lob imbalance
ws_trade_data_coinbase_rolling_lob <- function(con, coinbase_ticker, granularity_limit) {
	lob_metrics <- ws_coinbase_coinbase_LOB_data(con, coinbase_ticker, granularity_limit)
	lob_imbalance <- round(mean(as.numeric(lob_metrics$bid_ask_LOB_volume_imbalance)), digits = 6)
	bid_ask_LOB_volume_bid <- round(mean(as.numeric(lob_metrics$bid_ask_LOB_volume_bid)), digits = 6)
	bid_ask_LOB_volume_ask <- round(mean(as.numeric(lob_metrics$bid_ask_LOB_volume_ask)), digits = 6)

	#determines candle
	if(nrow(lob_metrics) == 0) {
		print("DATABASE OUT OF DATE")
	}

	return(c(lob_imbalance, bid_ask_LOB_volume_bid, bid_ask_LOB_volume_ask))
}

#rolling trade data
ws_trade_data_coinbase_rolling <- function(con, coinbase_ticker, granularity_limit) {
	current_trades <- ws_coinbase_ticker(con, coinbase_ticker, granularity_limit)
	#determines candle
	if(nrow(current_trades) == 0) {
		retry <- 1
		repeat {
			print(paste("DATABASE OUT OF DATE. RETRY: ", retry, "...", sep=""))
			current_trades <- ws_coinbase_ticker(con, coinbase_ticker, granularity_limit)
			retry <- retry + 1
			if(nrow(current_trades) > 0) {
				break
			} else if (retry > 10) {
				break
			}
		}
	}
#MAKE SURE THIS IS CORRECT	
	mt <- which(current_trades$time == max(current_trades$time))
	mit <- which(current_trades$time == min(current_trades$time))
	if(mt[1] > mit[1]) {
		close_trade <- as.numeric(current_trades[nrow(current_trades),4])
		open_trade <- as.numeric(current_trades[1,4])
	} else {
		close_trade <- as.numeric(current_trades[1,4])
		open_trade <- as.numeric(current_trades[nrow(current_trades),4])
	}
	
	candle <- NULL
	if(close_trade > open_trade) {candle <- "green"} #calculate if last trade bucket was green
	if(close_trade < open_trade) {candle <- "red"} #calculate if last trade bucket was red
	if(close_trade == open_trade) {candle <- "orange"}
#MAKE SURE THIS IS CORRECT	
	buy_volume <- round(sum(as.numeric(current_trades[current_trades$side == "buy",15])), digits = 6)
	sell_volume <- round(sum(as.numeric(current_trades[current_trades$side == "sell",15])), digits = 6)
	buy_sell_volume_ratio <- round((buy_volume - sell_volume) / (buy_volume + sell_volume), digits = 6) #buy_volume / sell_volume

	return(c(buy_volume, sell_volume, candle, buy_sell_volume_ratio, close_trade))
}

#calculate historical volume and close data
ws_trade_data_coinbase_timeframe_historical <- function(con, coinbase_ticker, granularity_limit, granularity_count, update_limit, alg_start, hist_start_time) {
	ran <- FALSE
	if(!alg_start) { #only conditions if the algorithm is on its first iteration
		alg_start <- TRUE
		start_time <- Sys.time()
		d1 <- ws_get_trade_volume(con, coinbase_ticker, granularity_count, granularity_limit)
		buy_volume_sd <- round(sd(d1[,1]), digits = 6)
		buy_volume_mean <- round(mean(d1[,1]), digits = 6)
		sell_volume_sd <- round(sd(d1[,2]), digits = 6)
		sell_volume_mean <- round(mean(d1[,2]), digits = 6)
		total_volume_sd <- round(sd(d1[,3]), digits = 6)
		total_volume_mean <- round(mean(d1[,3]), digits = 6)
		buy_sell_volume_ratio_mean <- round(mean(d1[,5]), digits = 6)
		buy_sell_volume_ratio_sd <- round(sd(d1[,5]), digits = 6)
		close_price <- d1[,4]
		
		d2 <- ws_get_lob_volume(con, coinbase_ticker, granularity_limit, granularity_count)
		bid_ask_volume_ratio_mean <- round(mean(d2[,1]), digits = 6)
		bid_ask_volume_ratio_sd <- round(sd(d2[,1]), digits = 6)
		bid_volume_mean <- round(mean(d2[,2]), digits = 6)
		bid_volume_sd <- round(sd(d2[,2]), digits = 6)
		ask_volume_mean <- round(mean(d2[,3]), digits = 6)
		ask_volume_sd <- round(sd(d2[,3]), digits = 6)
		ran <- TRUE
	} else {
		if(as.numeric(difftime(Sys.time(), hist_start_time, units="secs")) >= update_limit) {
			start_time <- Sys.time()
			d1 <- ws_get_trade_volume(con, coinbase_ticker, granularity_count, granularity_limit)
			buy_volume_sd <- round(sd(d1[,1]), digits = 6)
			buy_volume_mean <- round(mean(d1[,1]), digits = 6)
			sell_volume_sd <- round(sd(d1[,2]), digits = 6)
			sell_volume_mean <- round(mean(d1[,2]), digits = 6)
			total_volume_sd <- round(sd(d1[,3]), digits = 6)
			total_volume_mean <- round(mean(d1[,3]), digits = 6)
			buy_sell_volume_ratio_mean <- round(mean(d1[,5]), digits = 6)
			buy_sell_volume_ratio_sd <- round(sd(d1[,5]), digits = 6)
			close_price <- d1[,4]
			
			d2 <- ws_get_lob_volume(con, coinbase_ticker, granularity_limit, granularity_count)
			bid_ask_volume_ratio_mean <- round(mean(d2[,1]), digits = 6)
			bid_ask_volume_ratio_sd <- round(sd(d2[,1]), digits = 6)
			bid_volume_mean <- round(mean(d2[,2]), digits = 6)
			bid_volume_sd <- round(sd(d2[,2]), digits = 6)
			ask_volume_mean <- round(mean(d2[,3]), digits = 6)
			ask_volume_sd <- round(sd(d2[,3]), digits = 6)
			ran <- TRUE
		} 
	}
	if(!ran) {
		return(NULL)
	} else {
		return(list(buy_volume_sd, buy_volume_mean, sell_volume_sd, sell_volume_mean, total_volume_sd, total_volume_mean, alg_start, start_time, close_price, buy_sell_volume_ratio_mean, buy_sell_volume_ratio_sd, bid_ask_volume_ratio_mean, bid_ask_volume_ratio_sd, bid_volume_mean, bid_volume_sd, ask_volume_mean, ask_volume_sd)) #return as list to avoid transforming sys time to Unix time
	}
}

#helper function to get historical trade volume from database
ws_get_trade_volume <- function(con, coinbase_ticker, granularity_count, granularity_limit) {
	current_trades <- ws_coinbase_ticker(con, coinbase_ticker, granularity_limit = (granularity_count * granularity_limit))
	current_trades <- current_trades[,c(13,12,15,4)]
	current_trades[,3] <- as.numeric(current_trades[,3])
	current_trades[,4] <- as.numeric(current_trades[,4])
	time_filter <- seq(from=.POSIXct(min(current_trades$time), "UTC"), to=.POSIXct(max(current_trades$time), "UTC"), by=paste(granularity_limit/60, "min"))
	
	if(length(time_filter) == 1) {
		df <- list(current_trades)
	} else {
		df <- split(current_trades, cut(current_trades$time, breaks = c(time_filter)))
	}
	
	buy_volume_t <- unlist(lapply(df, function(x) sum(x[x$side == "buy",3])))
	sell_volume_t <- unlist(lapply(df, function(x) sum(x[x$side == "sell",3])))
	total_volume_t <- buy_volume_t + sell_volume_t
	close_data_t <- unlist(lapply(df, function(x) x[nrow(x),4]))

	#safety check for 0 data. this is automatically removed for close_data_t since it returns NA
	buy_volume_t <- buy_volume_t[buy_volume_t != 0]
	sell_volume_t <- sell_volume_t[sell_volume_t != 0]
	total_volume_t <- total_volume_t[total_volume_t != 0]
	buy_sell_volume_ratio <- data.frame(round((buy_volume_t - sell_volume_t) / total_volume_t, digits=6))

	save_volume <- cbind(buy_volume_t, sell_volume_t, total_volume_t, close_data_t, buy_sell_volume_ratio)
	return(save_volume)
}

#used to pull data from database for calculating the average lob imbalance ratio over time bucket
ws_get_lob_volume <- function(con, coinbase_ticker, granularity_limit, granularity_count) {
	lob_metrics <- ws_coinbase_coinbase_LOB_data(con, coinbase_ticker, granularity_limit = granularity_count * granularity_limit) #count multiplied by limit for entire timeframe
	time_filter <- seq(from=.POSIXct(min(lob_metrics$time), "UTC"), to=.POSIXct(max(lob_metrics$time), "UTC"), by=paste(granularity_limit/60, "min"))
	
	if(length(time_filter) == 1) {
		df1 <- list(lob_metrics$bid_ask_LOB_volume_imbalance)
		df2 <- list(lob_metrics$bid_ask_LOB_volume_bid)
		df3 <- list(lob_metrics$bid_ask_LOB_volume_ask)

	} else {
		df1 <- split(lob_metrics$bid_ask_LOB_volume_imbalance, cut(lob_metrics$time, breaks = c(time_filter)))
		df2 <- split(lob_metrics$bid_ask_LOB_volume_bid, cut(lob_metrics$time, breaks = c(time_filter)))
		df3 <- split(lob_metrics$bid_ask_LOB_volume_ask, cut(lob_metrics$time, breaks = c(time_filter)))
	}
	
	lob_imbalance <- data.frame(unlist(lapply(df1, mean)))
	bid_ask_LOB_volume_bid <- data.frame(unlist(lapply(df2, mean)))
	bid_ask_LOB_volume_ask <- data.frame(unlist(lapply(df3, mean)))

	return(cbind(lob_imbalance, bid_ask_LOB_volume_bid, bid_ask_LOB_volume_ask))
}