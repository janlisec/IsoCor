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

#' @title get_spectrum
#' @description Set up a MALDIquant object from a list of tables which contain
#'     similar columns used as RT/mass and intensity source respectively.
#' @param data List of data.frames with at least two numeric columns.
#' @param rt_col Column name of the RT/mass column to be used.
#' @param int_col Column name of the intensity column to be used.
#' @param cut_range List with elements 'min' and 'max' to limit the RT/mass column.
#' @param rt_shift Vector of length(data) to align the RT/mass column.
#' @return A list of MALDIquant mass spectra objects.
#' @keywords internal
#' @noRd
#' @importFrom MALDIquant createMassSpectrum
get_spectrum <- function(data, rt_col = "Minutes", int_col = "MF", cut_range = NULL, rt_shift = NULL) {
  # checks
  shiny::validate(shiny::need(is.list(data), "[get_spectrum] data is not a list"))
  shiny::validate(shiny::need(is.list(data), "[get_spectrum] data is not named"))
  shiny::validate(shiny::need(all(sapply(data, function(x) { all(c(rt_col, int_col) %in% colnames(x)) })), "[get_spectrum] data elements do not contain required columns"))
  shiny::validate(shiny::need(all(sapply(data, function(x) { all(diff(x[,rt_col])>0) })), message = "[get_spectrum] You selected a time column with non continuous values"))
  # potential defaults
  if (is.null(rt_shift)) rt_shift <- rep(0, length(data))
  if (is.null(cut_range)) cut_range <- list("min"=0, "max"=max(sapply(data, function(x) { max(x[,rt_col], na.rm=TRUE) })))
  # create MALDIquant objects
  lapply(1:length(data), function(k) {
    x <- data[[k]]
    m <- x[, rt_col]
    m <- m-rt_shift[k]
    flt <- m>=cut_range$min & m<=cut_range$max
    m <- m[flt]
    i <- x[flt, int_col]
    suppressWarnings(
      MALDIquant::createMassSpectrum(
        mass = m,
        intensity = i,
        metaData = list(
          name = "Name",
          file = names(data)[k]
        )
      )
    )
  })
}

#' @title spec_pre_process
#' @description Baseline corrects a list of MALDIquant objects.
#' @param data List of MALDIquant objects.
#' @param hWS hWS.
#' @param BLmethod BLmethod.
#' @param wf Workflow.
#' @return A list of MALDIquant mass spectra objects.
#' @keywords internal
#' @noRd
#' @importFrom MALDIquant smoothIntensity estimateBaseline removeBaseline
spec_pre_process <- function(data, hWS = isolate(input$ic_par_halfWindowSize), BLmethod = isolate(input$ic_par_baseline_method), wf = c("IDMS", "IR-Delta")) {
  wf <- match.arg(wf)
  shiny::validate(shiny::need(all(sapply(data, inherits, "MassSpectrum")), "[MALDIquant_pre_process] Input not of class MassSpectrum"))
  if (wf=="IDMS") {
    x_s <- MALDIquant::smoothIntensity(object = data, method = "SavitzkyGolay", halfWindowSize = 10)
    x_bl <- lapply(x_s, function(y) { MALDIquant::estimateBaseline(object = y, method = "TopHat", halfWindowSize = 185) })
    out <- lapply(1:length(data), function(i) { 
      data[[i]]@intensity <- data[[i]]@intensity - x_bl[[i]][,"intensity"] 
      # set negative intensities to zero
      #x[[i]]@intensity[x[[i]]@intensity<0] <- 0
      return(data[[i]])
    })
  } else {
    if (BLmethod!="none") {
      if (BLmethod=="Dariyah") {
        # $$not implemented
      } else {
        out <- MALDIquant::removeBaseline(object = data, method = BLmethod)
      }
    } 
  }
  return(out)
}

#' @title get_peaks
#' @description Baseline corrects a list of MALDIquant objects.
#' @param data List of MALDIquant objects.
#' @param hWS hWS.
#' @param SNR SNR
#' @param wf Workflow.
#' @param hWS_IDMS hWS_IDMS.
#' @param set_noise set_noise.
#' @param k k.
#' @return A list of MALDIquant peak objects.
#' @keywords internal
#' @noRd
#' @importFrom MALDIquant smoothIntensity detectPeaks estimateNoise intensity mass
get_peaks <- function(x, hWS = isolate(input$ic_par_halfWindowSize), SNR = isolate(input$ic_par_peakpicking_SNR), wf = c("IDMS", "IR-Delta"), hWS_IDMS = input$ic_par_IDMS_halfWindowSize, set_noise = input$ic_par_peakpicking_noise, k = input$ic_par_peakpicking_k) {
  wf <- match.arg(wf)
  if (wf=="IDMS") {
    shiny::validate(shiny::need(hWS_IDMS>=0, "halfWindowSize (smoothing) should not be a negative number"))
    shiny::validate(shiny::need(hWS_IDMS<floor(length(x)/2), "halfWindowSize (smoothing) is too large"))
    k <- 5 # fix k for IDMS
    noise <- 0 # fix noise for IDMS
    x <- suppressWarnings(MALDIquant::smoothIntensity(x, method = "SavitzkyGolay", halfWindowSize = hWS_IDMS))
    out <- MALDIquant::detectPeaks(object = x, method = "MAD", SNR = 20)
  } else {
    shiny::validate(shiny::need(hWS>=0, "halfWindowSize should not be a negative number"))
    shiny::validate(shiny::need(hWS<floor(length(x)/2), "halfWindowSize for smoothing parameter is too large"))
    x <- MALDIquant::smoothIntensity(object = x, method = "MovingAverage", halfWindowSize = hWS)
    # set a minimum hWS to detect peaks
    hWS <- ifelse(hWS>0, hWS, 25)
    out <- MALDIquant::detectPeaks(object = x, method = "MAD", halfWindowSize = hWS, SNR = SNR)
    # set noise
    noise <- ifelse(set_noise, SNR*min(MALDIquant::estimateNoise(x)[,2], na.rm=TRUE), 0)
  }
  out@metaData[["pb"]] <- ldply(out@mass, function(p) { 
    find_peak_boundaries(int = MALDIquant::intensity(x), p = which.min(abs(MALDIquant::mass(x) - p)), k = k, min_scans = 5, noise = noise)
  })
  return(out)
}
