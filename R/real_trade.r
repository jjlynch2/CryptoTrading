

#library(digest)
#library(httr)


#binance_api_key = ""
#binance_api_secret_key = ""
#api_url_binance <- "https://api.binance.us" #api url
#trading_ticker = "BTCUSD"

#place_order_binance(path = "/api/v3/order/test", binance_api_key, binance_api_secret_key, api_url_binance, trading_ticker, side, type, timeInForce = "GTC", quantity, price, recvWindow = 5000) {
#	params_temp <- paste("symbol=", trading_ticker, "&side=", side, "&type=", type, "&timeInForce=", timeInForce, "&quantity=", quantity, price=price, "&recvWindow=", recvWindow, "&timestamp=", round(as.numeric(as.POSIXct(Sys.time()))), sep="")
#	params_signed <- hmac(key = binance_api_secret_key, params_temp, algo = "sha256")	
#	params_query <- paste("?", params_temp, "&signature=", params_signed, sep="")
#	url_order <- paste(api_url_binance, path, params_query, sep="")
#	config <- add_headers('X-MBX-APIKEY' = binance_api_key)
#	res <- POST(url = url_order, config = config)
#}



#limit_order_binance() {
#	place_order_binance()
#	if(failed) {
#		adjust limit based on percentage
#	}
#	if(percentage > slippage) {
#		cancel order
#	}
#}

#while(TRUE) {
#	mid_price <- bid + (bid - ask)
#	place_order_binance()
#	Sys.sleep(5) #give order 5 seconds?
#	if(success) {
#		return()
#	} else {
#		price <- mid_price + mid_price * 0.005 #half percent increments?
#	}
#	if(price > mid_price + mid_price * p_limit) {
#		return(failed)
#	}
#}