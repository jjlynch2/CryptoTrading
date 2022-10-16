###################################################BD algorithm################################################
BD_algorithm <- function(coinbase_ticker = BD:::coinbase_ticker, binance_ticker = BD:::binance_ticker, paper_trade = BD:::paper_trade, trading_with_money = BD:::trading_with_money, available_funds = BD:::available_funds, coinbase_fee = BD:::coinbase_fee, binance_fee = BD:::binance_fee, trailing_stop_percentage = BD:::trailing_stop_percentage, standard_deviations_bs_sell = BD:::standard_deviations_bs_sell, standard_deviations_bs_buy = BD:::standard_deviations_bs_buy, buy_sell_volume_ratio_limit_buy = BD:::buy_sell_volume_ratio_limit_buy, buy_sell_volume_ratio_limit_sell = BD:::buy_sell_volume_ratio_limit_sell, bid_ask_volume_imbalance_ratio_buy = BD:::bid_ask_volume_imbalance_ratio_buy, bid_ask_volume_imbalance_ratio_sell = BD:::bid_ask_volume_imbalance_ratio_sell, standard_deviations_lob_sell = BD:::standard_deviations_lob_sell, standard_deviations_lob_buy = BD:::standard_deviations_lob_sell, volume_sd_limit_sell = BD:::volume_sd_limit_sell, volume_sd_limit_buy = BD:::volume_sd_limit_buy, rate_limit = BD:::rate_limit, granularity_limit = BD:::granularity_limit, granularity_limit_binance = BD:::granularity_limit_binance, granularity_count = BD:::granularity_count, update_limit = BD:::update_limit, db = BD:::db, host_db = BD:::host_db, db_port = BD:::db_port, db_user = BD:::db_user, db_password = BD:::db_password, api_url_coinbase = BD:::api_url_coinbase, api_url_binance = BD:::api_url_binance, websocket_url_coinbase = BD:::websocket_url_coinbase, websocket_url_binance = BD:::websocket_url_binance, bid_volume_buy = BD:::bid_volume_buy, ask_volume_sell = BD:::ask_volume_sell, bid_volume_limit_buy = BD:::bid_volume_limit_buy, ask_volume_limit_sell = BD:::ask_volume_limit_sell, optional_buy_signals = list(macd = TRUE, arbitrage = FALSE)) {
	beep(sound = 8)
	#safety check for future trading
	if(paper_trade == TRUE && trading_with_money == TRUE) { #mismatch in option
		cat("Please check paper_trade and trading_with_money options\n")
		break
	}
	if(paper_trade == FALSE && trading_with_money == FALSE) { #mismatch in option
		cat("Please check paper_trade and trading_with_money options\n")
		break
	}
	if(paper_trade == FALSE && trading_with_money == TRUE) { #safety to avoid real trading
		pr <- "real money! careful!"
		break
	}
	if(paper_trade == TRUE && trading_with_money == FALSE) { #safety to avoid real trading
		cat("Paper trading starting ...\n")
		pr <- "paper trading"
	}
	
	#connect to database
	print("Connecting to database")
	con <- ws_database_connect(db, host_db, db_port, db_user, db_password)

	#other variables
	last_bought_price <- NULL #last bought price
	last_poll_price <- 0 #last price during polling

	alg_start <- FALSE #used for historical data initial condition
	algorithm_start_time <- Sys.time() #start time of algorithm
	hist_start_time <- NULL
	#other variables

	#local variables
	crypto_bought <- 0 #crypto owned
	total_fees <- 0 #fees paid
	total_trades_bought <- 0 #total trades bought
	total_trades_sold <- 0 #total trades sold
	#local variables

	#start of algorithm
	repeat {
		#gather historical data and update		
		hd <- ws_trade_data_coinbase_timeframe_historical(con, coinbase_ticker, granularity_limit, granularity_count, update_limit, alg_start, hist_start_time) #pulls historical data
		if(!is.null(hd)) {
			buy_volume_sd <- hd[[1]]
			buy_volume_mean <- hd[[2]] 
			sell_volume_sd <- hd[[3]] 
			sell_volume_mean <- hd[[4]] 
			total_volume_sd <- hd[[5]]
			total_volume_mean <- hd[[6]]
			alg_start <- hd[[7]]
			hist_start_time <- hd[[8]]
			close_data <- hd[[9]] 
			buy_sell_volume_ratio_mean <- hd[[10]]
			buy_sell_volume_ratio_sd <- hd[[11]]
			bid_ask_volume_ratio_mean <- hd[[12]]
			bid_ask_volume_ratio_sd <- hd[[13]]
			bid_volume_mean <- hd[[14]]
			bid_volume_sd <- hd[[15]]
			ask_volume_mean <- hd[[16]]
			ask_volume_sd <- hd[[17]]

			#calculate limits
			buy_sell_volume_ratio_limit_buy <- round(buy_sell_volume_ratio_mean + buy_sell_volume_ratio_sd * standard_deviations_bs_buy, digits = 6)
			buy_sell_volume_ratio_limit_sell <- round(buy_sell_volume_ratio_mean - buy_sell_volume_ratio_sd * standard_deviations_bs_sell, digits = 6)
			volume_limit_buy <- round(buy_volume_mean + buy_volume_sd * volume_sd_limit_buy, digits = 6)
			volume_limit_sell <- round(sell_volume_mean + sell_volume_sd * volume_sd_limit_sell, digits = 6)
			bid_ask_volume_imbalance_ratio_buy <- round(bid_ask_volume_ratio_mean + bid_ask_volume_ratio_sd * standard_deviations_lob_buy, digits = 6)
			bid_ask_volume_imbalance_ratio_sell <- round(bid_ask_volume_ratio_mean - bid_ask_volume_ratio_sd * standard_deviations_lob_sell, digits = 6)
			bid_volume_buy <- round(bid_volume_mean + bid_volume_sd * bid_volume_limit_buy, digits = 6)
			ask_volume_sell <- round(ask_volume_mean - ask_volume_sd * ask_volume_limit_sell, digits = 6)
		}
		
		#gather current trade data
		td <- ws_trade_data_coinbase_rolling(con, coinbase_ticker, granularity_limit) #SQL call
		buy_volume_trade <- as.numeric(td[1]) #buy volume of trades
		sell_volume_trade <- as.numeric(td[2]) #sell volume of trades
		candle <- td[3] #candle type for trades
		buy_sell_volume_ratio <- as.numeric(td[4]) # buy/sell ratio for trades
		rolling_close_data <- as.numeric(td[5])
		
		#gather current lob data
		ld <- ws_trade_data_coinbase_rolling_lob(con, coinbase_ticker, granularity_limit)
		bid_ask_volume_imbalance_ratio <-  as.numeric(ld[1]) #bid ask imbalance ratio from lob
		bid_volume <-  as.numeric(ld[2]) #bid volume from lob
		ask_volume <-  as.numeric(ld[3]) #ask volume from lob
		
		#gather current binance price
		current <- ws_trade_data_binance_rolling_best_bid_ask(con, binance_ticker) 
		fee <- binance_fee
	
		#only use if trade and data exchanges are different and arbitrage is enabled
		if(optional_buy_signals$arbitrage) {
			current2 <- ws_trade_data_coinbase_rolling_best_bid_ask(con, coinbase_ticker)
			bidArbitrage <- current[1] > current2[1]
			askArbitrage <- current[2] < current2[2]
		} else {
			bidArbitrage <- NA
			askArbitrage <- NA
		}

		#calls macd before color options if enabled\n		
		if(optional_buy_signals$macd && length(close_data) >= 36) {
			macd_signal <- MACD_test(c(close_data, rolling_close_data))
		} else {
			macd_signal <- NA
		}

		#color logic
		g <- make_style("grey")
		orange <- make_style("orange")
		if(available_funds > 0) {		
			af <- make_style("green")
		} else if (available_funds < 0) {
			af <- make_style("red")
		} else {
			af <- make_style("grey")
		}
		if(crypto_bought > 0) {		
			cb <- make_style("green")
		} else if (crypto_bought == 0) {
			cb <- make_style("grey")
		}
		if(total_fees > 0) {
			tf <- make_style("red")
		} else {
			tf <- make_style("grey")
		}
		if(buy_volume_trade > volume_limit_buy) {
			vm_buy <- make_style("green")
		} else {
			vm_buy <- make_style("grey")
		}
		if(sell_volume_trade > volume_limit_sell) {
			vm_sell <- make_style("green")
		} else {
			vm_sell <- make_style("grey")
		}
		if(bid_ask_volume_imbalance_ratio > bid_ask_volume_imbalance_ratio_buy) {
			lobvrlb <- make_style("green")
		} else {
			lobvrlb <- make_style("grey")
		}
		if(bid_ask_volume_imbalance_ratio < bid_ask_volume_imbalance_ratio_sell) {
			lobvrls <- make_style("green")
		} else {
			lobvrls <- make_style("grey")
		}		
		if(buy_sell_volume_ratio > buy_sell_volume_ratio_limit_buy) {
			bsvrlb <- make_style("green")
		} else {
			bsvrlb <- make_style("grey")
		}
		if(buy_sell_volume_ratio < buy_sell_volume_ratio_limit_sell) {
			bsvrls <- make_style("green")
		} else {
			bsvrls <- make_style("grey")
		}
		if(bid_volume_buy >= bid_volume) {
			bv_buy <- make_style("grey")
		} else {
			bv_buy <- make_style("green")
		}
		if(ask_volume_sell >= ask_volume) {
			av_sell <- make_style("grey")
		} else {
			av_sell <- make_style("green")
		}
		ca <- make_style(candle)
		if(is.na(macd_signal)) {
			mac <- make_style("grey")
		} else if(macd_signal) {
			mac <- make_style("green")
		} else {
			mac <- make_style("grey")
		}
		if(is.na(askArbitrage)) {
			aa <- make_style("grey")
		} else if (askArbitrage) {
			aa <- make_style("green")
		} else {
			aa <- make_style("grey")
		}
		
		#color logic

		cat("\014", orange("Rate limit ", rate_limit), "\n", orange("Runtime: "), g(round(as.numeric(difftime(Sys.time(), algorithm_start_time,units="mins")), digits=2), " minutes"), "\n", orange("Funding: "), g(pr), "\n", orange("Trading exchange: "), g("Binance"), "\n", orange("Trading crypto: "), g(binance_ticker), "\n", orange("Data exchange: "), g("Coinbase"), "\n", orange("Data crypto: "), g(coinbase_ticker), "\n", orange("Time bucket: "), g(round((granularity_limit/60)), " minutes"), "\n", orange("Timeframe: "), g(((granularity_count * granularity_limit)/86400), " days"), "\n\n", orange("Available funds: "), af("$", available_funds), "\n", orange("Crypto portfolio: "), cb(crypto_bought), " ", g(binance_ticker, " at ", "$",last_bought_price), "\n", orange("Total buys: "), g(total_trades_bought), "\n", orange("Total sells: "), g(total_trades_sold), "\n", orange("Total fees: "), tf(total_fees), "\n\n", sep="")
		#update information

		#paper trade
		if(paper_trade == TRUE && trading_with_money == FALSE) {
			if(available_funds > 0) { #if funds are available check for buying conditions
				#update if attempting to buy
				cat(orange("Current best bid: "), g(current[1]), "\n", orange("Current best ask: "), g(current[2]), "\n\n", orange("Current buy volume: "), vm_buy(buy_volume_trade), "\n", orange("Historical buy volume limit: "), g(volume_limit_buy), "\n", orange("Trade volume imbalance ratio: "), bsvrlb(buy_sell_volume_ratio), "\n", orange("Trade volume imbalance ratio limit: "),  g(buy_sell_volume_ratio_limit_buy), "\n\n", orange("Candle: "), ca(candle), "\n", orange("MACD crossover: "), mac(macd_signal), "\n", orange("Arbitrage signal: "), aa(askArbitrage), "\n\n", orange("Current bid volume: "), bv_buy(bid_volume), "\n", orange("Historical bid volume: "), g(bid_volume_buy), "\n", orange("LOB volume imbalance ratio: "), lobvrlb(bid_ask_volume_imbalance_ratio), "\n", orange("LOB volume imbalance ratio limit: "), g(bid_ask_volume_imbalance_ratio_buy), "\n\n\n", sep="")
				#calls buy condition
				condition <- buy_condition(buy_volume_trade, volume_limit_buy, candle, buy_sell_volume_ratio, buy_sell_volume_ratio_limit_buy, bid_ask_volume_imbalance_ratio, bid_ask_volume_imbalance_ratio_buy, bid_volume, bid_volume_buy, macd_signal, askArbitrage) #checks buy condition
				
				if(condition) { #if buy condition met
					beep(sound = 2) #play sound for buying
					cat(orange("\n\nCondition met! Buying crypto ...\n\n"))
					cat(orange("       (\n      8=========D\n       (\n\n"))
					
					pt <- paper_trade_buy(total_fees, available_funds, fee, total_trades_bought, crypto_bought, current[2], binance_ticker)
					total_fees <- pt[[1]] #total fees
					available_funds <- pt[[2]] #available funds
					crypto_bought <- pt[[3]] #crypto bought
					total_trades_bought <- pt[[4]] #total trades
					last_bought_price <- pt[[5]] #purchase price
	
					if(condition[1]) {
						buy_signal = "Normal conditions"
					}
									
					temp_trade <- data.frame(time = .POSIXct(Sys.time(), "UTC"), type = "buy", signal = buy_signal, trade_exchange = "Binance", data_exchange = "Coinbase", ticker = binance_ticker, crypto_amount = crypto_bought, crypto_price = last_bought_price, usd_price = pt[[7]], fees = pt[[6]])
					if(any(dbListTables(con) == "paper_trades")) {
						dbWriteTable(con, "paper_trades", temp_trade, append = TRUE)
					} else {
						dbWriteTable(con, "paper_trades", temp_trade)
					}
				}
			}
			
			#if crypto is owned check for selling conditions
			if(crypto_bought > 0) {
				#update if attempting to buy
				cat(orange("Current best bid: "), g(current[1]), "\n", orange("Current best ask: "), g(current[2]), "\n", orange("Current sell volume: "), vm_sell(sell_volume_trade), "\n", orange("Historical sell volume limit: "), g(volume_limit_sell), "\n", orange("Trailing stop loss: "), g(trailing_stop_percentage * 100, "%"), "\n", orange("Trade volume imbalance ratio: "), bsvrls(buy_sell_volume_ratio), "\n", orange("Trade volume imbalance ratio limit: "),  g(buy_sell_volume_ratio_limit_sell), "\n\n", orange("Candle: "), ca(candle), "\n", orange("Arbitrage signal: "), aa(askArbitrage), "\n\n", orange("Current ask volume: "), av_sell(ask_volume), "\n", orange("Historical ask volume: "), g(ask_volume_sell), "\n", orange("LOB volume imbalance ratio: "), lobvrls(bid_ask_volume_imbalance_ratio), "\n", orange("LOB volume imbalance ratio limit: "), g(bid_ask_volume_imbalance_ratio_sell),"\n\n\n", sep="")
				#calls sell condition
				condition <- sell_condition(sell_volume_trade, volume_limit_sell, candle, buy_sell_volume_ratio, buy_sell_volume_ratio_limit_sell, bid_ask_volume_imbalance_ratio, bid_ask_volume_imbalance_ratio_sell, current[1], last_poll_price, trailing_stop_percentage, ask_volume, ask_volume_sell, bidArbitrage)
				
				if(any(condition)) { 
					beep(sound = 1) #play sound for selling

					cat(orange("\n\nCondition met! Selling crypto ...\n\n"))

					pt <- paper_trade_sell(total_fees, available_funds, fee, total_trades_sold, crypto_bought, current[1], binance_ticker)
					total_fees <- pt[[1]] #total fees
					available_funds <- pt[[2]] #available funds
					crypto_bought <- pt[[3]] #crypto bought
					total_trades_sold <- pt[[4]] #total trades
					last_bought_price <- pt[[5]] #purchase price

					
					if(condition[1]) {
						sell_signal = "Normal conditions"
					} else {
						sell_signal = "Stop loss"
					}
									
					#save trade data
					temp_trade <- data.frame(time = Sys.time(), type = "sell", signal = sell_signal, trade_exchange = "Binance", data_exchange = "Coinbase", ticker = binance_ticker, crypto_amount = pt[[7]], crypto_price = last_bought_price, usd_price = available_funds, fees = pt[[6]])		
					if(any(dbListTables(con) == "paper_trades")) {
						dbWriteTable(con, "paper_trades", temp_trade, append = TRUE)
					} else {
						dbWriteTable(con, "paper_trades", temp_trade)
					}

					if(available_funds <= 0) {
						cat("Lost all of your money!!!!!!!!!!!!!!!!!!!!\n") 
						beep(sound = 9)
						break
					}
					
					last_poll_price <- 0 #reset price for next trade
				}
				
				#only update if it is greater so its a trailing stop loss?
				if(current[1] > last_poll_price) {
					last_poll_price <- current[1] #update to new price
				}
			}
		} else if(paper_trade == FALSE && trading_with_money == TRUE) {
			break
		}
		Sys.sleep(rate_limit) #API rate limit 
	}
}