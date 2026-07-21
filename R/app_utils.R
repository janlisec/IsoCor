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
spec_pre_process <- function(data, hWS = 25, BLmethod = "TopHat", wf = c("IDMS", "IR-Delta")) {
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
get_peaks <- function(x, hWS = 25, SNR = 25, wf = c("IDMS", "IR-Delta"), hWS_IDMS = 25, set_noise = FALSE, k = 5) {
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
  out@metaData[["pb"]] <- ldply_base(out@mass, function(p) {
    find_peak_boundaries(int = MALDIquant::intensity(x), p = which.min(abs(MALDIquant::mass(x) - p)), k = k, min_scans = 5, noise = noise)
  })
  return(out)
}

#' @title ldply_base
#' @param .data list.
#' @param .fun fun.
#' @keywords internal
#' @noRd
ldply_base <- function(.data, .fun = identity) {
  result <- lapply(.data, .fun)
  df <- do.call(rbind, result)
  df <- data.frame(df, row.names = NULL, check.names = FALSE)
  return(df)
}
