.onLoad = function(libname, pkgname){
	#SQL database
	assign("db", "crypto", envir = topenv())
	assign("host_db", "192.168.0.1", envir = topenv())
	assign("db_port", 5432, envir = topenv())
	assign("db_user", "crypto", envir = topenv())
	assign("db_password", "", envir = topenv())

	#algorithm variables
	assign("binance_ticker", "ETHUSD", envir = topenv())
	assign("coinbase_ticker", "ETH-USD", envir = topenv())
	assign("paper_trade", TRUE, envir = topenv())
	assign("trading_with_money", FALSE, envir = topenv())
	assign("available_funds", 1000, envir = topenv())
	assign("coinbase_fee", 0.005, envir = topenv())
	assign("binance_fee", 0.001, envir = topenv())
	assign("trailing_stop_percentage", 0.02, envir = topenv())
	
	#buy sell volume options
	assign("standard_deviations_bs_sell", 1, envir = topenv())
	assign("standard_deviations_bs_buy", 1, envir = topenv())
	assign("buy_sell_volume_ratio_limit_buy", 9999, envir = topenv())
	assign("buy_sell_volume_ratio_limit_sell", 9999, envir = topenv())
	
	#volume options
	assign("volume_sd_limit_sell", 1, envir = topenv())
	assign("volume_sd_limit_buy", 1, envir = topenv())
	
	#time options
	assign("granularity_limit", 900, envir = topenv())
	assign("granularity_limit_binance", "15m", envir = topenv())
	assign("granularity_count", 192, envir = topenv()) #576 = 5m; 192 = 15m
	assign("update_limit", 900, envir = topenv()) #update limit should always match granularity
	
	#LOB ratio options
	assign("bid_ask_volume_imbalance_ratio_buy", 9999, envir = topenv())
	assign("bid_ask_volume_imbalance_ratio_sell", 9999, envir = topenv())
	assign("standard_deviations_lob_sell", 1, envir = topenv())
	assign("standard_deviations_lob_buy", 1, envir = topenv())
	
	#LOB volume options
	assign("bid_volume_buy", 9999, envir = topenv())
	assign("ask_volume_sell", 9999, envir = topenv())
	assign("bid_volume_limit_buy", 1, envir = topenv())
	assign("ask_volume_limit_sell", 1, envir = topenv())

	#websocket LOB options
	assign("LOB_timer", 2, envir = topenv()) #timer between metric calculations in seconds
	assign("lobpercent", 0.005, envir = topenv()) #timer between metric calculations in seconds

	#REST API
	assign("api_url_coinbase", "https://api.pro.coinbase.com/products/", envir = topenv())
	assign("api_url_binance", "https://api.binance.us/api/v3/", envir = topenv())
  
	#Websocket API
  	assign("websocket_url_coinbase", "wss://ws-feed.exchange.coinbase.com", envir = topenv())
	assign("websocket_url_binance", "wss://stream.binance.us:9443", envir = topenv())
	
	#API keys
	assign("binance_api_key", "" , envir = topenv())
	assign("binance_api_secret_key", "" , envir = topenv())
	assign("coinbase_api_key", "testkey2", envir = topenv())
	assign("coinbase_api_secret_key", "testkey2", envir = topenv())
	
	#rate limit for looping
	assign("rate_limit", 2, envir = topenv()) #controls the CPU utilization on postgres server
}




