# Launch the shinyApp (Do not remove this comment)

# To deploy: 
# rsconnect::deployApp(appDir = "C:/Users/jlisec/Documents/Rpackages/Rpackage_IsoCor/isocor", appName = c("test","IsoCor","Praktikum")[2], forceUpdate = TRUE, account = "jali")
# To upload an Info page only use the app in: c:\Users\jlisec\Documents\Rpackages\ShinyApps\ShinyInfoPage\

# To check for errors use the blue button on top of this file or: 
# rsconnect::showLogs(appName = "test", account = "jali")

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)
IsoCor::ic_app() # add parameters here (if any)
