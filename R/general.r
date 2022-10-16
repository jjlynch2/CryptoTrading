#retries the API call 3 times before stopping
api_call <- function(URL_built) {
	response <- NULL
	attempt <- 1
	while(is.null(response) && attempt <= 5) {
		if(attempt > 1) {
			Sys.sleep(5) #sleep 5 seconds before retrying in case of rate limit
		}
		attempt <- attempt + 1
		try(
			response <- fromJSON(file = URL_built)
		)
	}
	return(response)
}

#MACD
MACD_test <- function(data) {
	macd_data <- MACD(rev(data))
	macd_signal <- macd_data[nrow(macd_data),1] > macd_data[nrow(macd_data),2]
	return(macd_signal)
}

#connect to database
ws_database_connect <- function(db, host_db, db_port, db_user, db_password) {
	con<-dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)
	return(con)
}