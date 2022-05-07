# Launch the shinyApp (Do not remove this comment)
# To deploy: 
# rsconnect::deployApp(appDir = "C:/Users/jlisec/Rpackages/Rpackage_IsoCor/isocor", appName = c("test","IsoCor")[2], forceUpdate = TRUE)
# To check for errors use: 
# rsconnect::showLogs(appName = "test", account = "jali")
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)
IsoCor::ic_app() # add parameters here (if any)
