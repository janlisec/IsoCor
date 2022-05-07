#' @title help_the_user
#' @description Help Window: opens a modal with respective Help text (rendered from Rmd source file) for users.
#' @param filename Name of the file as string (if necessary, containing also path).
#' @return Returns the help text as HTML (opens a modal with helpt text as side effect).
#' @keywords internal
#' @importFrom markdown markdownToHTML
help_the_user <- function(filename) {
  file_in <- list.files(path = shiny::resourcePaths()["www"], pattern = paste0(filename, ".[Rr][Mm][Dd]$"), recursive = TRUE, full.names = TRUE)[1]
  help_text <- NULL
  if (!file.exists(file_in)) {
    message("[help_the_user] cant find help file: ", file_in)
  } else {
    help_text <- 
    shiny::showModal(
      shiny::modalDialog(
        shiny::withMathJax(
          shiny::HTML(
            markdown::markdownToHTML(file = file_in, fragment.only = TRUE, extensions = c("tables","autolink","latex_math","superscript"))
          )
        ),
        size = "l",
        easyClose = TRUE,
        footer = NULL,
        title = NULL
      )
    )
  }
  invisible(NULL)
}