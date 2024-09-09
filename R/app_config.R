#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "IsoCor")
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file
#'
#' @noRd
get_golem_config <- function(
  value,
  config = Sys.getenv("GOLEM_CONFIG_ACTIVE", Sys.getenv("R_CONFIG_ACTIVE", "default")),
  use_parent = TRUE
) {
  config::get(
    value = value,
    config = config,
    # Modify this if your config file is somewhere else:
    file = app_sys("golem-config.yml"),
    use_parent = use_parent
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom shinyjs useShinyjs
#' @noRd
golem_add_external_resources <- function() {
  # prepare the app environment ----
  golem::add_resource_path('www', app_sys('app/www'))
  
  # amend site header with additional information (favicon, scripts, css...)
  shiny::tags$head(
    golem::bundle_resources(
      path = app_sys('app/www'),
      app_title = get_golem_config("app_name")
    ),
    golem::favicon(ico = "BAMLogo"),
    # Add here other external resources
    shinyjs::useShinyjs(),
    # include JS to determine screen height (to ultimately adjust table heights)
    includeScript(app_sys("app/www/js/screen_height.js")),
    # include JS for setting up tracking via Matomo
    if (get_golem_config("bam_server")) {
      HTML('<noscript><p><img src="https://agw1.bam.de/piwik/matomo.php?idsite=24&amp;rec=1" style="border:0;" alt="" /></p></noscript>')
      HTML('<script type="text/javascript" src="https://agw1.bam.de/piwik/piwik.js" async defer></script>')
      includeScript(app_sys("app/www/js/tracking-live.js"))
    }
  )
}

#' @title app_status_line
#' @description Returns a status line for the bottom of the app.
#' @return HTML with info regarding app version.
#' @keywords internal
#' @noRd
app_status_line <- function() {
  shiny::HTML(
    "ver.", get_golem_config("app_version"), 
    " | ", get_golem_config("app_date"), 
    " | <a href='mailto:jan.lisec@bam.de'>jan.lisec@bam.de</a>",
    ifelse(get_golem_config("bam_server"), '| <a href="https://www.bam.de/Navigation/EN/Services/Privacy-Policy/privacy-policy.html" target="_blank" rel="noopener noreferrer">BAM Privacy policy</a>', '')
  )
}
