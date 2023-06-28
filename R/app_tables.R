#' @title tab_peaks.
#' @description \code{tab_peaks} will .
#' @param p MALDIquant peak list.
#' @param s MALDIquant spectra list..
#' @examples 
#' #tab_peaks(p = ic_mi_peaks, s = )
#' @return A data frame of peaks.
#' @keywords internal
#' @noRd
tab_peaks <- function(p, s, mb = c("none","Linear","Russel","Exponential")) {
  stopifnot(length(p)==length(s))
  mb <- match.arg(mb)
  out <- ldply(1:length(p), function(i) {
    x <- p[[i]]
    sm <- mass(s[[i]])
    rnd_time <- 2
    if (length(x@mass)==0) {
      data.frame(
        "Sample"=0L, 
        "Peak ID"=0L, 
        "RT max"=0L, 
        "RT start"=0L, 
        "RT end"=0L,
        "Scan start"=0L, 
        "Scan end"=0L,
        "Scan length"=0L,
        check.names = FALSE, stringsAsFactors = FALSE)[-1,]
    } else {
      ldply(1:length(x@mass), function(j) {
        pb <- unlist(x@metaData$pb[j,])
        data.frame(
          "Sample"=i, 
          "Peak ID"=j, 
          "RT max"=round(x@mass[j], rnd_time), 
          "RT start"=round(sm[pb[1]], rnd_time), 
          "RT end"=round(sm[pb[2]], rnd_time),
          "Scan start"=pb[1], 
          "Scan end"=pb[2],
          "Scan length"=diff(pb)+1,
          check.names = FALSE, stringsAsFactors = FALSE)
      })
    }
  })
  out <- out[order(out[,"Peak ID"]),]
  # attach columns for mass_bias correction
  out <- cbind(
    out, 
    data.frame(
      "Mass bias method"=rep(mb, nrow(out)),
      "f_value"=rep(0, nrow(out)),
      "k"=rep(1, nrow(out)),
      check.names = FALSE
    )
  )
  return(out)  
}

#' @title style_tab_idms.
#' @description \code{style_tab_idms} will .
#' @param data IDMS tab.
#' @param sh screen_height in px.
#' @return A datatable object.
#' @keywords internal
#' @noRd
style_tab_idms <- function(data, sh=975) {
  dt <- DT::datatable(
    data = data,
    "extensions" = "Buttons",,
    "options" = list(
      "server" = FALSE, 
      "dom"="Bt", 
      "autoWidth" = TRUE,
      "paging" = FALSE,
      "scrollY" = sh-570,
      "pageLength" = -1,
      "buttons" = list(
        list(
          extend = 'excel',
          title = NULL,
          text = '<i class="fa fa-file-excel-o"></i>',
          titleAttr = 'Download table as Excel',
          filename = "IDMS_table"
        )
      )
    ), 
    "selection" = list(mode="single", target="row"), 
    "rownames" = NULL
  )
  return(dt)
}