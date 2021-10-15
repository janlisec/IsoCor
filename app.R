# shinyApp
# rsconnect::showLogs()
#shiny::shinyApp(ui = app_ui(), server = app_server, onStart = function(){source('global.R')})
source('global.R')
#pkgload::load_all(".")
#shiny::addResourcePath(prefix = "pics", directoryPath = "www")
ic_app()
