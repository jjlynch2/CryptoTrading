configuration_ui <- tabItem(tabName = "Configuration",
	tabPanel("Exchange API",
		fluidRow(
			column(3,
				box(
					title = "Coinbase API Key",
					solidHeader=TRUE,
					width=12,
					status="primary",
					collapsible = TRUE,
					uiOutput("api_key_coinbase"),
					actionButton("add_key_coinbase","Add API key", icon = icon("plus"))
				)
			),
			column(3,
				box(
					title = "Binance API Key",
					solidHeader=TRUE,
					width=12,
					status="primary",
					collapsible = TRUE,
					uiOutput("api_key_binance"),
					actionButton("add_key_binance","Add API key", icon = icon("plus"))
				)
			),
			column(3,
				box(
					title = "Import Trades",
					solidHeader=TRUE,
					width=12,
					status="primary",
					collapsible = TRUE,
					uiOutput("import_trades")			
				)
			),
			column(3
				box(
					title = "Delete Trades",
					solidHeader=TRUE,
					width=12,
					status="primary",
					collapsible = TRUE,
					uiOutput("delete_trades")				
				)
			)
		)
	)
)
