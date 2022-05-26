#'@title read_raw_data
#'
#'@description \code{read_raw_data} will import ICP MS data in various file formats.
#'
#'@details Try to specify 'format' parameter to find a method suitable for your 
#'  files or select 'generic' which will import a tab delimited file with 3 columns
#'  defining RT, MI and SI respectively.
#'
#'@param path Valid filepath.
#'@param format Character specifying the import file format.
#'
#'@examples 
#'str(read_raw_data(path=""))
#'
#'@return A data.frame.
#'
#'@importFrom utils read.delim
#'
#'@export
read_raw_data <- function(path, format = c("exp","icp","data","generic")) {
  #pk <- 1+sin(seq(-0.5*pi,1.5*pi,length.out=61))
  #df_default <- data.frame("Minutes"=(0:60)/60, "MI"=pk, "SI"=0.1*pk)
  if (file.exists(path)) {
    if (format=="exp") {
      comment_lines <- grep("^[*]", readLines(path))
      if (length(comment_lines)>=1) {
        df <- read.delim(path, sep="\t", header = T, check.names = FALSE, nrows = min(comment_lines)-2)
      } else {
        df <- read.delim(path, sep="\t", header = T, check.names = FALSE)
      }
      if ("Time" %in% colnames(df)) {
        df[, "Time"] <- paste0(substr(df[, "Time"], 1, 8), "." , substr(df[, "Time"], 10, 12))
        df[, "Time"] <- as.POSIXct(df[, "Time"], format = "%H:%M:%OS")
        df[, "Time"] <- as.numeric(df[, "Time"] - df[1, "Time"])
        df[, "Minutes"] <- df$Time / 60
        reord <- c(1:grep("Time",colnames(df)), ncol(df), (grep("Time",colnames(df))+1):(ncol(df)-1))
        df <- df[,reord]
        df <- df[,!apply(df, 2, function(x) {all(is.na(x))}), drop=FALSE]
      } else {
        df <- NULL
      }
    }
    if (format=="icp") {
      df <- read.delim(path, sep="\t", skip = 6, check.names = FALSE, header=FALSE)
      if (is.data.frame(df) && prod(dim(df))>=1 && is.numeric(df[,1]) && all(diff(df[,1])>=0)) {
        ions <- strsplit(readLines(path, n=1), "\t")[[1]][-1]
        df <- df[,!apply(df, 2, function(x) { all(is.na(x)) })]
        colnames(df) <- c("Minutes", ions)
        df[,"Minutes"] <- df[,"Minutes"]/60
      } else {
        df <- NULL
      }
    }  
    if (format=="data") {
      df <- read.delim(path, sep=",", check.names = FALSE, header=TRUE)[,-c(1:2)]
      if (is.data.frame(df) && prod(dim(df))>=1) {
        colnames(df)[1] <- "Minutes"
        colnames(df) <- gsub("^X","",colnames(df))
        df[,"Minutes"] <- df[,"Minutes"]/60
      } else {
        df <- NULL
      }
    }  
    if (format=="generic") {
      df <- try(read.delim(path, sep="\t", check.names = FALSE, header=TRUE))
      if (is.data.frame(df) && ncol(df)>=3 && sum(apply(df, 2, is.numeric))>=3) {
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