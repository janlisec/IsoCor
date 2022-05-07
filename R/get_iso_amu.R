#'@title get_iso_amu.
#'
#'@description \code{get_iso_amu} will take a string and try to identify an
#'  isotope name contained within. It will return the amu for this isotope.
#'
#'@param x character.
#'@param isotopes Two column dataframe with isotope definitions.
#'
#'@examples 
#'IsoCor:::get_iso_amu(x="198Hg")
#'IsoCor:::get_iso_amu(x="198Hg_corr")
#'IsoCor:::get_iso_amu(x="X_32S_corr")
#'IsoCor:::get_iso_amu(x="15S")
#'
#'@return A single numeric value (0 in case that no isotope could be identified).
#'
#'@keywords internal
get_iso_amu <- function(x, isotopes=data.frame("isotope"=c("198Hg","32S"), "mass"=c(197.999,31.995))) {
  x <- as.character(x[1])
  val <- 0
  l <- which(isotopes[,"isotope"] == x)[1]
  if (!is.na(l)) { 
    val <- isotopes[l,"mass"]
  } else {
    l <- unlist(sapply(isotopes[,"isotope"], function(i) {grep(i, x)}))
    if (length(l)==1) val <- isotopes[isotopes[,"isotope"] == names(l),"mass"]
  }
  return(val)
}