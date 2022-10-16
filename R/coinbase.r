#get historical data API call
get_historical_coinbase <- function(api_url_coinbase, coinbase_ticker, granularity) {
	URL_built <- paste(api_url_coinbase, coinbase_ticker, paste("/candles?granularity=", granularity,sep=""), sep="")
	historical_data <- api_call(URL_built)
	historical_data <- do.call(rbind.data.frame, historical_data)
	return(historical_data)
}
#get historical data API call

#pulls historical data using granularity limit for historical volume data
historical_data_coinbase <- function(api_url_coinbase, coinbase_ticker, granularity_limit, granularity_count, update_limit, alg_start, hist_start_time) {
	ran <- FALSE
	if(!alg_start) { #only conditions if the algorithm is on its first iteration
		alg_start <- TRUE
		start_time <- Sys.time()
		d1 <- get_historical_coinbase(api_url_coinbase, coinbase_ticker, granularity_limit)
		volume_sd <- round(sd(d1[1:granularity_count,6]), digits = 6)
		volume_mean <- round(mean(d1[1:granularity_count,6]), digits = 6)
		ran <- TRUE
	} else {
		if(as.numeric(difftime(Sys.time(), hist_start_time, units="secs")) >= update_limit) {
			start_time <- Sys.time()
			d1 <- get_historical_coinbase(api_url_coinbase, coinbase_ticker, granularity_limit)
			volume_sd <- round(sd(d1[1:granularity_count,6]), digits = 6)
			volume_mean <- round(mean(d1[1:granularity_count,6]), digits = 6)
			ran <- TRUE
		} 
	}
	if(!ran) {
		return(NULL)
	} else {
		return(list(volume_sd, volume_mean, alg_start, start_time, d1[1:granularity_count,5])) #return as list to avoid transforming sys time to Unix time
	}
}
#pulls historical data using granularity limit for historical volume data

#get current price on order book
get_current_coinbase <- function(api_url_coinbase, coinbase_ticker) {
	URL_built <- paste(api_url_coinbase, coinbase_ticker, "/book?level=1", sep="")
	orderbook <- api_call(URL_built)
	bid <- orderbook$bids[[1]][[1]]
	ask <- orderbook$asks[[1]][[1]]
	return(c(bid,ask))
}

#get current trades API call
get_trades_coinbase <- function(api_url_coinbase, coinbase_ticker, granularity_limit) {	
	URL_built <- paste(api_url_coinbase, coinbase_ticker, "/trades", sep="")
	trades <- api_call(URL_built)
	trades <- do.call(rbind.data.frame, trades)
	trades$time <- strptime(trades$time, "%Y-%m-%dT%H:%M:%OS")
	after <- trades[which(trades[,1] == min(trades$time)),2]
	
	repeat {
		Sys.sleep(1) #Wait 1 sec per call
		URL_built <- paste(api_url_coinbase, coinbase_ticker, "/trades", "?after=",after[1], sep="")
		trades_temp <- api_call(URL_built)
		trades_temp <- do.call(rbind.data.frame, trades_temp)
		trades_temp$time <- strptime(trades_temp$time, "%Y-%m-%dT%H:%M:%OS")
		after <- trades_temp[which(trades_temp[,1] == min(trades_temp$time)),2]
		trades <- rbind(trades, trades_temp)
		if(as.numeric(max(trades[,1]) - min(trades[,1])) >= (granularity_limit / 60)) {
			break
		}
	}
	stop_time <- max(trades[,1]) - granularity_limit
	trades <- trades[trades[,1] >= stop_time,]
	return(trades)
}
#get current trades API call

trade_data_coinbase <- function(api_url_coinbase, coinbase_ticker, granularity_limit) {
	current_trades <- get_trades_coinbase(api_url_coinbase, coinbase_ticker, granularity_limit)
	volume_trade <- round(sum(as.numeric(current_trades[,4])), digits = 6)
	
	#determines candle
	open_trade <- as.numeric(current_trades[current_trades$time == min(current_trades$time),3])
	close_trade <- as.numeric(current_trades[current_trades$time == max(current_trades$time),3])
	candle <- NULL
	if(close_trade > open_trade) {candle <- "green"} #calculate if last trade bucket was green
	if(close_trade < open_trade) {candle <- "red"} #calculate if last trade bucket was red
	if(close_trade == open_trade) {candle <- "orange"}
	#determines candle

	buy_volume <- sum(as.numeric(current_trades[current_trades[,5] == "buy",4])) 
	sell_volume <- sum(as.numeric(current_trades[current_trades[,5] == "sell",4]))
	buy_sell_volume_ratio <- round((buy_volume - sell_volume) / (buy_volume + sell_volume), digits = 6) #buy_volume / sell_volume
	return(list(volume_trade, candle, buy_sell_volume_ratio, close_trade))
}
