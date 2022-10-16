#paper buy
paper_trade_buy <- function(total_fees, available_funds, fee, total_trades_bought, crypto_bought, current, trading_ticker) {
	#saving for data.frame
	save_fee <- available_funds * fee
	save_funds_used <- available_funds - available_funds * fee
	#saving for data.frame
	total_fees <- total_fees + available_funds * fee #calculates and updates total fees for all trades
	available_funds <- available_funds - available_funds * fee #removes trade fee from available funds
	crypto_bought <- available_funds / current #calculates crypto purchased with last price
	available_funds <- 0 #uses all funds so this becomes $0
	cat(paste(crypto_bought, " ", trading_ticker, " at $", current, "\n", sep=""))
	total_trades_bought <- total_trades_bought + 1 #counts total trades bought
	last_bought_price <- current
	return(list(total_fees, available_funds, crypto_bought, total_trades_bought, last_bought_price, save_fee, save_funds_used))
}

#paper sell
paper_trade_sell <- function(total_fees, available_funds, fee, total_trades_sold, crypto_bought, current, trading_ticker) {
	cat(paste(crypto_bought, " ", trading_ticker, " at $", current, "\n", sep=""))
	available_funds <- crypto_bought * current
	#saving for data.frame
	save_fee <- available_funds *fee
	save_funds_used <- crypto_bought
	#saving for data.frame
	crypto_bought <- 0 #sells all crypto so this becomes $0
	total_fees <- total_fees + available_funds * fee #calculates and updates total fees for all trades
	available_funds <- available_funds - available_funds * fee #calculates available funds after selling at last trade price
	cat(paste("Available funds: $", available_funds, "\n", sep="")) 
	total_trades_sold <- total_trades_sold + 1 #counts total trades sold
	last_bought_price <- 0
	return(list(total_fees, available_funds, crypto_bought, total_trades_sold, last_bought_price, save_fee, save_funds_used))
}
