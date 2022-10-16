#buy condition
buy_condition <- function(buy_volume_trade, volume_limit_buy, candle, buy_sell_volume_ratio, buy_sell_volume_ratio_limit_buy, bid_ask_volume_imbalance_ratio, bid_ask_volume_imbalance_ratio_buy, bid_volume, bid_volume_buy, macd_signal, askArbitrage) {	
	if(!is.na(askArbitrage) && !is.na(macd_signal)) {
		condition <- buy_volume_trade > volume_limit_buy && candle == "green" && buy_sell_volume_ratio > buy_sell_volume_ratio_limit_buy && bid_ask_volume_imbalance_ratio > bid_ask_volume_imbalance_ratio_buy && bid_volume > bid_volume_buy && macd_signal && askArbitrage
	} else if(!is.na(askArbitrage) && is.na(macd_signal)) {
		condition <- buy_volume_trade > volume_limit_buy && candle == "green" && buy_sell_volume_ratio > buy_sell_volume_ratio_limit_buy && bid_ask_volume_imbalance_ratio > bid_ask_volume_imbalance_ratio_buy && bid_volume > bid_volume_buy && askArbitrage
	} else if(is.na(askArbitrage) && !is.na(macd_signal)) {
		condition <- buy_volume_trade > volume_limit_buy && candle == "green" && buy_sell_volume_ratio > buy_sell_volume_ratio_limit_buy && bid_ask_volume_imbalance_ratio > bid_ask_volume_imbalance_ratio_buy && bid_volume > bid_volume_buy && macd_signal
	} else if(is.na(askArbitrage) && is.na(macd_signal)) {
		condition <- buy_volume_trade > volume_limit_buy && candle == "green" && buy_sell_volume_ratio > buy_sell_volume_ratio_limit_buy && bid_ask_volume_imbalance_ratio > bid_ask_volume_imbalance_ratio_buy && bid_volume > bid_volume_buy
	}
	return(condition)
}
#buy condition

#sell condition
sell_condition <- function(sell_volume_trade, volume_limit_sell, candle, buy_sell_volume_ratio, buy_sell_volume_ratio_limit_sell, bid_ask_volume_imbalance_ratio, bid_ask_volume_imbalance_ratio_sell, current, last_poll_price, trailing_stop_percentage, ask_volume, ask_volume_sell, bidArbitrage) {
	if(current < last_poll_price) {
		price_difference <- last_poll_price - current 
		if((price_difference / last_poll_price) >= trailing_stop_percentage) { #return if trailing stop percentage reached
			return(c(TRUE,FALSE))
		}
	}
	
	if(!is.na(bidArbitrage)) {
		condition <- c(FALSE, sell_volume_trade > volume_limit_sell && candle == "red" && buy_sell_volume_ratio < buy_sell_volume_ratio_limit_sell && bid_ask_volume_imbalance_ratio < bid_ask_volume_imbalance_ratio_sell && ask_volume > ask_volume_sell && bidArbitrage)
	} else {
		condition <- c(FALSE, sell_volume_trade > volume_limit_sell && candle == "red" && buy_sell_volume_ratio < buy_sell_volume_ratio_limit_sell && bid_ask_volume_imbalance_ratio < bid_ask_volume_imbalance_ratio_sell && ask_volume > ask_volume_sell)
	}
	return(condition)
}
#sell condition