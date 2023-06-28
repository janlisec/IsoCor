#' @title get_iso_amu.
#' @description \code{get_iso_amu} will take a string and try to identify an
#'   isotope name contained within. It will return the amu for this isotope.
#' @param x character.
#' @param isotopes Two column dataframe with isotope definitions.
#' @examples 
#' \dontrun{
#' get_iso_amu(x="198Hg")
#' get_iso_amu(x="198Hg_corr")
#' get_iso_amu(x="X_32S_corr")
#' get_iso_amu(x="15S")
#' }
#' @return A single numeric value (0 in case that no isotope could be identified).
#' @keywords internal
#' @noRd
get_iso_amu <- function(x, isotopes=data.frame("isotope"=c("198Hg","32S"), "mass"=c(197.999,31.995))) {
  x <- as.character(x[1])
  val <- 0
  l <- which(isotopes[,"isotope"] == x)[1]
  if (!is.na(l)) { 
    val <- isotopes[l,"mass"]
  } else {
    l <- unlist(sapply(isotopes[,"isotope"], function(i) {grep(i, x)}))
    if (length(l)>=1) val <- isotopes[isotopes[,"isotope"] == names(l),"mass"][1]
  }
  return(val)
}

#' @title read_clipboard
#' @description \code{read_clipboard} is a Shiny module which provides 
#'     tabular copy paste from Excel to a Shiny-App via a textAreaInput
#'     element.
#' @details The module will render a button initially. This button (when
#'     clicked) will open a textAreaInput. Here the user can paste a 
#'     tabular range from i.e. Excel and either upload this data as
#'     data.frame to the app or cancel the operation.
#' @param btn_txt The label for the button which opens the textAreaInput.
#' @param n_rows Number of rows expected in input.
#' @param n_cols Number of columns expected in input.
#' @return A data.frame containing the converted string from the textAreaInput.
#' @examples
#' \dontrun{
#' shinyApp(
#'   ui = shiny::fluidPage(read_clipboard_UI(id = "test")), 
#'   server = function(input, output, session) { 
#'     tmp <- read_clipboard_Server(id = "test") 
#'     observeEvent(tmp$d, print(tmp$d))
#'   }
#' )
#' }
#' @keywords internal
#' @noRd
read_clipboard_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::actionButton(inputId = ns("btn_main"), label = "btn_main"),
    shiny::uiOutput(outputId = ns("area_input"))
  )
}

read_clipboard_Server <- function(id, btn_txt=shiny::reactiveVal(NULL), n_rows=shiny::reactiveVal(1), n_cols=shiny::reactiveVal(1)) {
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    shinyjs::hide(id = "area_input")
    observeEvent(btn_txt(), {
      txt <- isolate(btn_txt())
      shiny::updateActionButton(inputId = "btn_main", label = ifelse(is.null(txt), "Paste from clipboard<br>(show textArea Input)", txt))
    })
    
    out <- reactiveValues(d = NULL, counter = 0)
    Err_Msg <- function(test=FALSE, message="Open Error Modal when test==FALSE", type=c("Error", "Info")[1]) {
      if (!test) {
        shiny::showModal(shiny::modalDialog(HTML(message), title = type, easyClose = TRUE))
        if (type=="Error") shiny::validate(shiny::need(expr = test, message = message, label = "Err_Msg"))
      } else {
        invisible(NULL)
      }
    }
    
    output$area_input <- shiny::renderUI({
      shiny::tagList(
        shiny::fluidRow(
          shiny::textAreaInput(
            inputId = ns("txt_textAreaInput"),
            label = "",
            placeholder = paste("copy/paste a numeric Excel range of", n_cols(), "columns and", n_rows(), "rows."),
            width="100%",
            rows=12
          )
        ),
        shiny::fluidRow(
          actionButton(ns("btn_textAreaInput"), "Click to upload data"), p(),
          actionButton(ns("btn_textAreaInput2"), "Close text input area")
        ))
    })
    
    shiny::observeEvent(input$btn_main, {
      shiny::updateTextAreaInput(inputId = "txt_textAreaInput", value = "")
      shinyjs::show(id = "area_input")
    })
    
    shiny::observeEvent(input$btn_textAreaInput2, {
      shinyjs::hide(id = "area_input")
    })
    
    shiny::observeEvent(input$btn_textAreaInput, {
      # read clipboard
      #tmp <- data.table::fread(paste(input$area_input, collapse = "\n"))
      tmp <- strsplit(input$txt_textAreaInput, "\n")[[1]]
      # correct potential error for last col being empty
      tmp <- gsub("\t$", "\t\t", tmp)
      #browser()
      Err_Msg(test = length(tmp)>=n_rows(), message = paste("The pasted data appears to have less than", n_rows(), "rows"))
      # split at "\t" and ensure equal length
      tmp <- strsplit(tmp, "\t")
      Err_Msg(test = length(unique(sapply(tmp, length)))==1, message = "The clipboard content appears to have differing number of columns")
      # convert to numeric (what is expected by downstream functions)
      tmp <- plyr::laply(tmp, function(x) {
        x <- try(as.numeric(x))
      }, .drop = FALSE)
      Err_Msg(test = all(is.finite(tmp)), message = "The clipboard content did contain missing values or non-numeric cells<br>(now converted to NA)", type="Info")
      # ensure n columns
      Err_Msg(test = ncol(tmp)>=n_cols(), message = paste("The pasted data appears to have less than", n_cols(), "columns"))
      out$d <- tmp
      out$counter <- out$counter+1
      shinyjs::hide(id = "area_input")
    }, ignoreInit = TRUE)
    
    return(out)
    
  })
}

#' @title help_the_user
#' @description Help Window: opens a modal with respective Help text 
#'     (rendered from Rmd source file) for users.
#' @param filename Name of the file as string (if necessary, containing also path).
#' @return Returns the help text as HTML (opens a modal with helpt text as side effect).
#' @keywords internal
#' @noRd
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
              #markdown::markdownToHTML(file = file_in, fragment.only = TRUE, extensions = c("tables","autolink","latex_math","superscript"))
              markdown::markdownToHTML(file = file_in, fragment.only = TRUE)
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