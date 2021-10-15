#'@title iso_ratio
#'
#'@description \code{iso_ratio} will calculate a robust estimate of an 
#'  isotopic ratio between intensity values of 2 mass traces.
#'
#'@details tbd.
#'
#'@param data data.frame with two columns specifying data for isotope 1 and 2 respectively.
#'@param method Method to calculate the isotope ratio.
#'@param thr Threshold between 0..1 to limit the peaks scans used in the calculation (1=all scans, 0=apex only).
#'
#'@return A single numeric value given the robsut ratio estimate calculated from data.
#'
#'@importFrom pracma trapz
#'@importFrom stats lm
#'
#'@keywords internal
iso_ratio <- function(
  data = data.frame("X"=rnorm(10), "Y"=rnorm(10)),
  method = c("mean","area","slope")[1], 
  thr = 1) {
  
  # test if data and thr numeric
  # stopifnot(is.numeric(data))
  stopifnot(length(thr)==1)
  stopifnot(is.numeric(thr))
  stopifnot(thr>=0 & thr<=1)
  
  # methods are sensitive against missing values and 0 values
  flt <- apply(data, 1, function(x) {
    all(is.finite(x)) && all(x>0)
  })
  if (any(!flt)) {
    message(paste("[iso_ratio] Did remove", sum(!flt), "Scans due to missing or 0 values"))
    data <- data[flt,]
  }
  
  # determine peak apex and width of peak the method should be applied to
  data <- data[data[,1] >= (1-thr)*max(data[,1]),,drop=FALSE]

  # apply your method of choice
  out <- switch(
    method,
    "mean" = mean(data[,2]/data[,1], na.rm=TRUE),
    # "area" = sum(at_data[,2])/sum(at_data[,1]),
    "area" = trapz(x = 1:length(data[,2]), y = data[,2]) / trapz(x = 1:length(data[,1]), y = data[,1]),
    "slope"= lm(data[,2] ~ data[,1])$coefficients[[2]]
  )
  
  return(out)
}