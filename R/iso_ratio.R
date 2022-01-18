#'@title iso_ratio
#'
#'@description \code{iso_ratio} will calculate a robust estimate of an 
#'  isotopic ratio between intensity values of 2 mass traces.
#'
#'@details Within \code{\link{ic_app}} we compute estimates for isotope ratios 
#'  using raw data and several processing steps. \code{iso_ratio} is internally 
#'  used to perform this calculation and could be used in an external data 
#'  processing pipeline without the app context.
#'  However, users would need to extract intensity vectors of isotope peaks 
#'  from raw data independently.
#'  ***Note!*** All non-finite values and x==0 will be removed before calculation.
#'
#'@param data data.frame with two columns specifying data for isotope 1 and 2 respectively.
#'@param method Method to calculate the isotope ratio.
#'@param thr Threshold between 0..1 to limit the peaks scans used in the calculation (1=all scans, 0=apex only).
#'
#'@examples 
#'peak1 <- 1+cos(x = seq(-pi, pi, length.out=100))
#'peak2 <- 0.05*peak1*runif(n=100, min=1, max=1.01)
#'iso_ratio(data=cbind(peak1, peak2))
#'
#'@return A single numeric value. The robust ratio estimate calculated from \code{data}.
#'
#'@importFrom stats lm
#'
#'@export
iso_ratio <- function(
  data = data.frame("X"=rnorm(10), "Y"=rnorm(10)),
  #method = c("mean","area","slope")[1], 
  method = c("PBP","PAI","LRS")[1],
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