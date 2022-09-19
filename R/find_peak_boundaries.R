#' @title find_peak_boundaries
#'
#' @description \code{find_peak_boundaries} will find peak boundaries, based on curve derivative.
#'
#' @details tbd.
#'
#' @param int Numeric vector (of intensity valus).
#' @param p Index of peak position (usually 'which.max(int)).
#' @param k Number of scans at peak boarder to confirm peak valley.
#' @param min_scans Minimum number of scans in front or tail.
#'
#' @return A numeric vector of length 2 giving the indexes of peak start and peak end.
#'
#' @examples
#' \dontrun{
#' x <- sin(seq(-pi,2*pi,0.01))+1
#' plot(x)
#' abline(v=find_peak_boundaries(x))
#' }
#' @keywords internal
#' @noRd
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
