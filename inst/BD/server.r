server <- function(session, input, output) { 
	#showModal(modalDialog(title = "Loading source code ...", easyClose = FALSE, footer = NULL)) 
	#API stuff 
	#APIURL <- "https://sandbox.iexapis.com/stable" 
	#apikey <- readLines(system.file("Stonkalysis/server", 'apikey', package = "Stonkalysis")) 
	 
	#api_key_binance <<- readLines(con = system.file("BD", 'api_key_binance', package = "BD"))
	#api_key_coinbase <<- readLines(con = system.file("BD", 'api_key_coinbase', package = "BD"))

	#configuration / cache code 
	source(system.file("BD/server", 'api.r', package = "BD"), local=TRUE) 
	 
	#fundamental source code 
	#source(system.file("Stonkalysis/server", 'performance.r', package = "Stonkalysis"), local=TRUE) 
	#trade_data <- data.frame(version = "0.0.1", time = Sys.time(), type = "buy", signal = "Momentum", trade_exchange = "binance", data_exchange = "coinbase", ticker = "BTCSD", crypto_amount = 0.0056787, crypto_price = 560674, usd_price = 3183.89, fees = 3) 
 
	#removeModal() 
} 