# shinyApp
# devtools::load_all(".")
# rsconnect::deployApp(appName = c("IsoCor","test")[2], forceUpdate = TRUE)
# rsconnect::showLogs()
options(shiny.autoload.r=FALSE)
source('global.R')
ic_app()
