#' @title iso_ratio
#' @description \code{iso_ratio} will calculate a robust estimate of an
#'  isotopic ratio between intensity values of 2 mass traces.
#' @details Within \code{\link{ic_app}} we compute estimates for isotope ratios
#'  using raw data and several processing steps. \code{iso_ratio} is internally
#'  used to perform this calculation and could be used in an external data
#'  processing pipeline without the app context.
#'  However, users would need to extract intensity vectors of isotope peaks
#'  from raw data independently.
#'  ***Note!*** All non-finite values and x==0 will be removed before calculation.
#' @param data data.frame with two columns specifying data for isotope 1 and 2 respectively.
#' @param method Method to calculate the isotope ratio.
#' @param thr Threshold between 0..1 to limit the peaks scans used in the calculation (1=all scans, 0=apex only).
#' @examples
#' peak1 <- 1 + cos(x = seq(-pi, pi, length.out = 100))
#' peak2 <- 0.05 * peak1 * runif(n = 100, min = 1, max = 1.01)
#' iso_ratio(data = cbind(peak1, peak2))
#' @return A single numeric value. The robust ratio estimate calculated from \code{data}.
#' @importFrom stats lm
#' @export
iso_ratio <- function(data, method = c("PBP", "PAI", "LRS"), thr = 1) {
  
  method <- match.arg(method)
  
  # test if thr is valid input
  stopifnot(exprs = {
    "[iso_ratio] parameter thr should be of length=1" = length(thr)==1
    "[iso_ratio] parameter thr should be of numeric" = is.numeric(thr)
    "[iso_ratio] parameter thr should be within interval {0, 1}" = thr>=0 & thr<=1
  })
  
  # methods are sensitive against missing values and 0 values
  flt <- apply(data, 1, function(x) { all(is.finite(x)) && all(x>0) })
  if (any(!flt)) {
    #message(paste("[iso_ratio] Did remove", sum(!flt), "Scans due to missing or 0 values"))
    data <- data[flt,]
  }
  
  # determine peak apex and width of peak the method should be applied to
  data <- data[data[,1] >= (1-thr)*max(data[,1]),,drop=FALSE]

  # apply your method of choice
  out <- switch(
    method,
    "PBP" = median(data[,2]/data[,1], na.rm=TRUE),
    "PAI" = sum(data[,2])/sum(data[,1]),
    #"area" = pracma::trapz(x = 1:length(data[,2]), y = data[,2]) / pracma::trapz(x = 1:length(data[,1]), y = data[,1]),
    "LRS" = lm(data[,2] ~ data[,1])$coefficients[[2]]
  )
  
  return(out)
}

#' @title mass_bias.
#' @description \code{mass_bias} will calculate a correction factor K to
#'  scale isotopic ratios and thereby account for machine variance.
#' @details Currently, 3 methods are available to calculate the mass bias,
#'   Linear, Russel and Exponential. They all depend on the atomic mass
#'   of the two ion traces and a f-value which can be provided as
#'   parameters to the function.
#' @param mi_amu atomic mass of MI isotope.
#' @param si_amu atomic mass of SI isotope
#' @param method Method to calculate the mass bias.
#' @param f_value f_value to be used within the method calculation.
#' @examples 
#' IsoCor::mass_bias(32, 34, "Linear", 0.1)
#' @return A single numeric value K to be used for scaling.
#' @export
mass_bias <- function(mi_amu = 0, si_amu = 0, method = c("Linear","Russel","Exponential"), f_value = 0) {
  
  method <- match.arg(method)
  
  # test f_value
  stopifnot(exprs = {
    "[mass_bias] parameter f_value should be of length=1" = length(f_value)==1
    "[mass_bias] parameter f_value should be of numeric" = is.numeric(f_value)
  })

  # apply method of choice
  k <- switch(
    method,
    "Linear" = 1 + f_value * (si_amu - mi_amu),
    "Russel" = ifelse(identical(mi_amu, 0), 0, si_amu/mi_amu) ^ f_value,
    "Exponential"= exp(f_value * (si_amu - mi_amu)),
    1
  )
  
  return(k)
}

#' @title read_raw_data
#' @description \code{read_raw_data} will import ICP MS data in various file formats.
#' @details Try to specify 'format' parameter to find a method suitable for your
#'  files or select 'generic' which will import a tab delimited file with 3 columns
#'  defining RT, MI and SI respectively.
#'  You may check why data import of your files fails in the app on this function
#'  and potentially extend it to handle your files.
#' @param path Valid file path.
#' @param format Character specifying the import file format.
#' @examples
#' str(IsoCor::read_raw_data(path = ""))
#' @return A data.frame.
#' @importFrom utils read.delim
#' @export
read_raw_data <- function(path, format = c("exp", "icp", "data", "generic")) {
  # pk <- 1+sin(seq(-0.5*pi,1.5*pi,length.out=61))
  # df_default <- data.frame("Minutes"=(0:60)/60, "MI"=pk, "SI"=0.1*pk)
  if (file.exists(path)) {
    if (format == "exp") {
      comment_lines <- grep("^[*]", readLines(path))
      if (length(comment_lines) >= 1) {
        df <- read.delim(path, sep = "\t", header = T, check.names = FALSE, nrows = min(comment_lines) - 2)
      } else {
        df <- read.delim(path, sep = "\t", header = T, check.names = FALSE)
      }
      if ("Time" %in% colnames(df)) {
        df[, "Time"] <- paste0(substr(df[, "Time"], 1, 8), ".", substr(df[, "Time"], 10, 12))
        df[, "Time"] <- as.POSIXct(df[, "Time"], format = "%H:%M:%OS")
        df[, "Time"] <- as.numeric(df[, "Time"] - df[1, "Time"])
        df[, "Minutes"] <- df$Time / 60
        reord <- c(1:grep("Time", colnames(df)), ncol(df), (grep("Time", colnames(df)) + 1):(ncol(df) - 1))
        df <- df[, reord]
        df <- df[, !apply(df, 2, function(x) {
          all(is.na(x))
        }), drop = FALSE]
      } else {
        df <- NULL
      }
    }
    if (format == "icp") {
      df <- read.delim(path, sep = "\t", skip = 6, check.names = FALSE, header = FALSE)
      if (is.data.frame(df) && prod(dim(df)) >= 1 && is.numeric(df[, 1]) && all(diff(df[, 1]) >= 0)) {
        ions <- strsplit(readLines(path, n = 1), "\t")[[1]][-1]
        df <- df[, !apply(df, 2, function(x) {
          all(is.na(x))
        })]
        colnames(df) <- c("Minutes", ions)
        df[, "Minutes"] <- df[, "Minutes"] / 60
      } else {
        df <- NULL
      }
    }
    if (format == "data") {
      df <- read.delim(path, sep = ",", check.names = FALSE, header = TRUE)[, -c(1:2)]
      if (is.data.frame(df) && prod(dim(df)) >= 1) {
        colnames(df)[1] <- "Minutes"
        colnames(df) <- gsub("^X", "", colnames(df))
        df[, "Minutes"] <- df[, "Minutes"] / 60
      } else {
        df <- NULL
      }
    }
    if (format == "generic") {
      df <- try(read.delim(path, sep = "\t", check.names = FALSE, header = TRUE))
      if (is.data.frame(df) && ncol(df) >= 3 && sum(apply(df, 2, is.numeric)) >= 3) {
        # no further assumptions made
      } else {
        df <- NULL
      }
    }
  } else {
    df <- NULL
  }
  return(df)
}

#' @title find_peak_boundaries
#' @description \code{find_peak_boundaries} will find the start and end point
#'     of a peak based on curve derivative.
#' @details This function provides a simple detection algorithm for peak boundaries.
#'     It will accept a numeric vector as input and determine relative to the global maximum
#'     (or a user provided local maximum) the left and right border where intensity decrease
#'     ends and intensity is increasing again.
#' @param int Numeric vector (of intensity values).
#' @param p Index of peak position (usually 'which.max(int)).
#' @param k Number of scans at peak boarder to confirm peak valley.
#' @param min_scans Minimum number of scans in front or tail.
#' @param noise A threshold value. All Values below or equal to noise will be set to zero.
#' @return A numeric vector of length 2 giving the indexes of peak start and peak end.
#' @examples
#' \dontrun{
#' x <- sin(seq(-pi,2*pi,0.01))+1
#' plot(x)
#' abline(v=find_peak_boundaries(x))
#' }
#' @export
find_peak_boundaries <- function(int=NULL, p=which.max(int), k=3, min_scans=3, noise=0) {
  int[!is.finite(int)] <- 0
  int[int<=noise] <- 0
  idx <- 1:length(int)
  n <- length(idx)
  test_front <- diff(int[1:p])<=0
  test_front <- which(rev(cumsum(as.numeric(rev(test_front))))==k)
  test_front <- ifelse(length(test_front)>=1, max(test_front)+2, 1)
  lb <- idx[max(c(min(c(p-min_scans,test_front)),1))]
  test_tail <- diff(int[p:n])>=0
  test_tail <- which(cumsum(as.numeric(test_tail))==k)
  test_tail <- ifelse(length(test_tail)>=1, p+min(test_tail)-2, n)
  rb <- idx[min(c(max(c(p+min_scans,test_tail)),n))]
  return(idx[c(lb,rb)])
}
