BDGui <- function() {
	library(shiny)
	runApp(system.file("BD", package = "BD"), launch.browser=TRUE)
}