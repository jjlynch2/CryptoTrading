
#output$api_key_coinbase <- renderUI({
#	textInput(inputId = "api_key_coinbase", "", value=api_key_coinbase)
#})

#output$api_key_binance <- renderUI({
#	textInput(inputId = "api_key_binance", "", value=api_key_binance)
#})


#observeEvent(input$api_key_coinbase, {
#	if(input$api_key_coinbase != api_key_coinbase) {
#		writeLines(input$api_key_coinbase, con = system.file("BD", 'api_key_coinbase', package = "BD"))
#		api_key_coinbase <<- input$api_key_coinbase
#	}
#})

#observeEvent(input$api_key_binance, {
#	if(input$api_key_binance != api_key_binance) {
#		writeLines(input$api_key_binance, con = system.file("BD", 'api_key_binance', package = "BD"))
#		api_key_binance <<- input$api_key_binance
#	}
#})


#USE A CACHE SYSTEM TO SAVE TRADE FILES INTO THE r PACKAGE AND MAKE THEM SELECTABLE!!