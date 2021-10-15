#'@title read_raw_data
#'
#'@description \code{read_raw_data} will import data in exp file format.
#'
#'@details tbd.
#'
#'@param path Valid filepath.
#'@param format Character, currently only 'exp' files are supported.
#'
#'@return A data.frame.
#'
#'@importFrom utils read.delim
#'
#'@keywords internal
read_raw_data <- function(path, format = "exp") {
  df <- read.delim(path, sep="\t", header = T, nrow = 5500, check.names = FALSE)
  df[, "Time"] <- paste0(substr(df[, "Time"], 1, 8), "." , substr(df[, "Time"], 10, 12))
  df[, "Time"] <- as.POSIXct(df[, "Time"], format = "%H:%M:%OS")
  df[, "Time"] <- as.numeric(df[, "Time"] - df[1, "Time"])
  df[, "Minutes"] <- df$Time / 60
  reord <- c(1:grep("Time",colnames(df)), ncol(df), (grep("Time",colnames(df))+1):(ncol(df)-1))
  df <- df[,reord]
  df <- df[,!apply(df, 2, function(x) {all(is.na(x))}), drop=FALSE]
  return(df)
}