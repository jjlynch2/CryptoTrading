
# #	con<-dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)
# #	ticker <- "ADA-USD"
# #	granularity_limit = 300 #300 = 5mins
# #RFP <- function(con, ticker, granularity_limit) {
	# #example
	# #ranger(Spces ~ ., data = iris
	
	# #con<-dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)

	
	# LOB <- dbReadTable(con, "coinbase_lob_data")
	# TR <- dbReadTable(con, "coinbase_ticker")
	
	# lob_metrics <- LOB[LOB$coinbase_ticker == ticker,]
	# trades <- TR[TR$product_id == ticker,]
	
	# #calculates metrics over time bucket
	# lob_trade_imbalance <- NULL
	# while(TRUE) {
		# time_filter <- .POSIXct(max(lob_metrics$time)-granularity_limit, "UTC")
		
		# #LOB average
		# temp_candle <- lob_metrics[lob_metrics[,2] >= time_filter,]
		# imbalance_t <- round(mean(as.numeric(temp_candle[,3])), digits = 6)
		
		# #trades summation			
		# temp_candle <- trades[trades[,13] >= time_filter,]
		# buy_volume_t <- sum(as.numeric(temp_candle[temp_candle[,12] == "buy",15]))
		# sell_volume_t <- sum(as.numeric(temp_candle[temp_candle[,12] == "sell",15]))
		
		# #best bid ask over the candle period
		# bb <- max(as.numeric(temp_candle$best_bid))
		# ba <- min(as.numeric(temp_candle$best_ask))
		
		# #remove time bucket
		# lob_metrics <- lob_metrics[lob_metrics[,2] < time_filter,]
		# trades <- trades[trades[,13] < time_filter,]

		# #build row
		# temp <- c(imbalance_t, max(lob_metrics$time), time_filter, round((buy_volume_t - sell_volume_t) / (buy_volume_t + sell_volume_t), digits=6), bb, ba, NA)
		# names(temp) <- c("LOB_imbalance", "start_time", "end_time", "trade_imbalance", "best_bid", "best_ask", "TP")
		# lob_trade_imbalance <- rbind(lob_trade_imbalance, temp)

		# #exit if either LOB or trade runs out of data
		# if(nrow(lob_metrics) == 0 || nrow(trades) == 0) {
			# break()
		# }
	# }							
	
	# #calculates the predictor 
	# for(i in 2:nrow(lob_trade_imbalance)) {
		# if(lob_trade_imbalance[i,5] > lob_trade_imbalance[i-1,5]) {
			# lob_trade_imbalance[i,7] <- TRUE
		# } else {
			# lob_trade_imbalance[i,7] <- FALSE
		# }
	# }
	
	# #create train + test
	# lob_trade_imbalance <- lob_trade_imbalance[,c(1,4,7)]
	# lob_trade_imbalance <- na.omit(as.data.frame(lob_trade_imbalance))

	# train.idx <- sample(nrow(lob_trade_imbalance), 2/3 * nrow(lob_trade_imbalance))
	# lob_trade_imbalance.train <- lob_trade_imbalance[train.idx,]
	# lob_trade_imbalance.test <- lob_trade_imbalance[-train.idx,]
	
	# rf <- csrf(TP ~ ., training_data = lob_trade_imbalance.train, test_data = lob_trade_imbalance.test)

	# rf <- ranger(TP ~ ., data = lob_trade_imbalance, num.trees = 5, write.forest = TRUE)

# }