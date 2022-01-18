#'@title mass_bias.
#'
#'@description \code{mass_bias} will calculate a correction factor K to 
#'  scale isotopic ratios and thereby account for machine variance.
#'
#'@details tbd.
#'
#'@param mi_amu atomic mass of MI isotope.
#'@param si_amu atomic mass of SI isotope
#'@param method Method to calculate the mass bias.
#'@param f_value f_value to be used within the method calculation.
#'
#'@examples 
#'mass_bias(32, 34, "Linear", 0.1)
#'
#'@return A single numeric value K to be used for scaling.
#'
#'@export
mass_bias <- function(
  mi_amu = 0, 
  si_amu = 0,
  method = c("Linear","Russel","Exponential")[1], 
  f_value = 0) {
  
  # test f_value
  stopifnot(length(f_value)==1)
  stopifnot(is.numeric(f_value))
  
  # apply your method of choice
  k <- switch(
    method,
    "Linear" = 1 + f_value * (si_amu - mi_amu),
    "Russel" = ifelse(identical(mi_amu, 0), 0, si_amu/mi_amu) ^ f_value,
    "Exponential"= exp(f_value * (si_amu - mi_amu)),
    1
  )
  
  return(k)
  
}
