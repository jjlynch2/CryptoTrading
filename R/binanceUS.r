#get current price
get_current_binance <- function(api_url_binance, binance_ticker) {
	URL_built <- paste(api_url_binance, "ticker/bookTicker?symbol=", binance_ticker, sep="")
	bestBook <- api_call(URL_built)
	bestBook <- do.call(cbind.data.frame, bestBook)
	bestBook$askPrice <- as.numeric(bestBook$askPrice)
	bestBook$bidPrice <- as.numeric(bestBook$bidPrice)
	return(bestBook)
}

#get historical data
get_historical_binance <- function(api_url_binance, binance_ticker, granularity_limit_binance, granularity_count) {
	URL_built <- paste(api_url_binance, "klines", "?symbol=", binance_ticker, "&limit=", granularity_count, "&interval=", granularity_limit_binance, sep="")
	klines <- api_call(URL_built)
	klines <- do.call(rbind.data.frame, klines)
	return(klines)
}
#get historical data

#pulls historical data using granularity limit for historical volume data
historical_data_binance <- function(api_url_binance, binance_ticker, granularity_limit_binance, granularity_count, update_limit, alg_start, hist_start_time) {
	ran <- FALSE
	if(!alg_start) { #only conditions if the algorithm is on its first iteration
		alg_start <- TRUE
		start_time <- Sys.time()
		d1 <- get_historical_binance(api_url_binance, binance_ticker, granularity_limit_binance = granularity_limit_binance, granularity_count = granularity_count)
		volume_sd <- round(sd(d1[,6]), digits = 6)
		volume_mean <- round(mean(d1[,6]), digits = 6)
		ran <- TRUE
	} else { 
		if(as.numeric(difftime(Sys.time(), hist_start_time, units="secs")) >= update_limit) {
			cat("Updating previous data ...\n")
			start_time <- Sys.time()
			d1 <- get_historical_binance(api_url_binance, binance_ticker, granularity_limit_binance = granularity_limit_binance, granularity_count = granularity_count)
			volume_sd <- round(sd(d1[,6]), digits = 6)
			volume_mean <- round(mean(d1[,6]), digits = 6)
			ran <- TRUE
		} 
	}
	if(!ran) {
		return(NULL)
	} else {
		return(list(volume_sd, volume_mean, alg_start, start_time, rev(d1[,5]))) #return as list to avoid transforming sys time to Unix time
	}
}
#pulls historical data using granularity limit for historical volume data

#get current trades
get_trades_binance <- function(api_url_binance, binance_ticker, granularity_limit) {
	URL_built <- paste(api_url_binance, "aggTrades", "?symbol=", binance_ticker, "&limit=1000", sep="")
	trades <- api_call(URL_built)
	trades <- do.call(rbind.data.frame, trades)
	trades$T <- as.POSIXct(trades$T/1000, origin="1970-01-01")
	stop_time <- max(trades[,6]) - granularity_limit
	trades <- trades[trades[,6] >= stop_time,]
	return(trades)
}
#get current trades

#Calculate the volume for the last 1000 trades OR granularity_limit if that limit is less than 1000 (API defaults to 1000 unfortunately so the time can't be exact)
trade_data_binance <- function(api_url_binance, binance_ticker, granularity_limit) {
	current_trades <- get_trades_binance(api_url_binance, binance_ticker, granularity_limit)
	volume_trade <- sum(as.numeric(current_trades$q))
	#determines candle
	open_trade <- as.numeric(current_trades[current_trades$T == min(current_trades$T),2])
	close_trade <- as.numeric(current_trades[current_trades$T == max(current_trades$T),2])
	candle <- NULL
	if(close_trade > open_trade) {candle <- "green"} #calculate if last trade bucket was green
	if(close_trade < open_trade) {candle <- "red"} #calculate if last trade bucket was red
	if(close_trade == open_trade) {candle <- "orange"}
	#determines candle
	buy_volume <- sum(as.numeric(current_trades[current_trades[,7] == FALSE,3]))
	sell_volume <- sum(as.numeric(current_trades[current_trades[,7] == TRUE,3])) 
	buy_sell_volume_ratio <- round((buy_volume - sell_volume) / (buy_volume + sell_volume), digits = 6)

	return(list(volume_trade, candle, buy_sell_volume_ratio, close_trade))
}