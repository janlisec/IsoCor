#'@title read_clipboard
#'
#'@description \code{read_clipboard} will .
#'
#'@details tbd
#'
#'@param btn_txt btn_txt.
#'@param n_rows 1.
#'@param n_cols 1.
#'
#'@return A data.frame.
#'
#'@examples
#'\dontrun{
#'shinyApp(
#'  ui = shiny::fluidPage(read_clipboard_UI(id = "test")), 
#'  server = function(input, output, session) { 
#'    tmp <- read_clipboard_Server(id = "test") 
#'    observeEvent(tmp$d, print(tmp$d))
#'  }
#')
#'}
#'
#'@keywords internal
#'
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
      shinyjs::hide(id = "area_input")
    }, ignoreInit = TRUE)
    
    observeEvent(out$d, {
      out$counter <- out$counter+1
    }, ignoreNULL = TRUE)
    
    return(out)
  
  })
}