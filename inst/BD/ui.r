source(system.file("BD", 'libraries.r', package = "BD"), local=TRUE) 
#source(system.file("BD/ui", 'api.r', package = "BD"), local=TRUE) 

ui <- dashboardPage(skin = "blue", 
	dashboardHeader( 
		title = "BD", 
		titleWidth=150 
	), 
	dashboardSidebar( 
		sidebarMenu(id = "sidebar", 
			menuItem("Configuration", tabName = "Configuration", icon = icon("cogs"))
			#menuItem("Performance", tabName = "Configuration", icon = icon("cogs")), 
			#menuItem("Trades", tabName = "Configuration", icon = icon("cogs")) 
		), 
		width = 150 
	), 
	dashboardBody( 
 		tabItems(
			configuration_ui
	 	)
	) 
) 