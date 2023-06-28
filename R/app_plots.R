#' @title ic_specplot
#' @description Generate an annotated plot of one to several spectra
#'     to visualize the processing results of ic_app().
#' @param opt Vector of keywords to hide/show specific elements of the plot.
#' @param xrng Numeric vector of length 2 specifying plotting range for x. 
#' @param mi_spec Main isotope spectra (list of MALDIquant objects).
#' @param si_spec Secondary isotope spectra (list of MALDIquant objects).
#' @param x_unit Unit of x axis.
#' @param ylab2 Character vector to be used for secondary axis labelling.
#' @param s_focus Index of sample within focus.
#' @param pks Data frame of peaks with columns 'Sample', 'Peak ID', 'Scan start' and 'Scan end'.
#' @param mi_pks Peaks of main isotope.
#' @param cdf Settings of current drift filter.
#' @param sel_pk Selected peak as numeric index of length one.
#' @importFrom graphics abline axTicks axis box legend lines mtext par points segments
#' @importFrom grDevices grey
#' @return An annotated plot of one to several spectra.
#' @examples 
#' if (interactive()) {
#'   utils::data("testdata", package = "IsoCor")
#'   mi_spec <- lapply(testdata, function(x) {
#'     MALDIquant::createMassSpectrum(mass = x[,"Time"]/60, intensity = x[,"32S"]) 
#'   })
#'   IsoCor:::ic_specplot(mi_spec=mi_spec)
#' }
#' @keywords internal
#' @noRd
ic_specplot <- function(
  opt = "", 
  xrng = c(0, 12),
  mi_spec = NULL,
  si_spec = NULL,
  xlab = paste0("Time [", "min", "]"),
  ylab = "Intensity [V]",
  ylab2 = paste0("32S", "/", "34S"),
  s_focus = "Sample 1",
  pks = NULL,
  mi_pks = NULL,
  cdf = c(0.1, 0.9),
  sel_pk = NULL
) {
  # get y range
  yrng <- c(0, max(sapply(c(mi_spec, si_spec), function(x) {max(x@intensity, na.rm=TRUE)})))
  # modify plot margins
  par(mar = c(4.5, 4.5, 0.5, ifelse("overlay_drift" %in% opt, 4.5, 0.5)))
  # render base plot
  plot(x = xrng, y = yrng, type = "n", xaxs = "i", xlab = xlab, ylab = ylab)
  if ("overlay_mi" %in% opt) {
    idx_all <- 1:length(mi_spec)
    cols <- 2:(length(idx_all)+1)
  } else {
    idx_all <- as.numeric(gsub("[^[:digit:]]", "", s_focus))
    cols <- rep(1, idx_all)
  }
  for (idx in idx_all) {
    if ("overlay_legend" %in% opt) {
      f_in <- sapply(mi_spec, function(x) { x@metaData$file })
      mtext(text = paste0("[",idx,"] ", f_in[idx]), side = 3, line = -1.15*idx, adj = 0.02, font = 1, col=cols[idx])
    }
    sm <- mass(mi_spec[[idx]])
    si <- intensity(mi_spec[[idx]])
    flt <- sm>=xrng[1] & sm<=xrng[2]
    lines(x = sm[flt], y = si[flt], col=cols[idx])
    if ("overlay_si" %in% opt) {
      lines(
        x = mass(si_spec[[idx]])[flt],
        y = intensity(si_spec[[idx]])[flt],
        col = cols[idx]
      )
    }
    if (!is.null(mi_pks) && length(mi_pks[[idx]]@mass)>=1 && !is.null(si_spec)) {
      if (idx==idx_all[1]) {
        peak_details <- lapply(idx_all, function (idx) {
          tmp <- pks[pks[,"Sample"]==idx,,drop=FALSE]
          lapply(tmp[,"Peak ID"], function(j) {
            pb <- c(tmp[j,"Scan start"], tmp[j,"Scan end"])
            out <- data.frame("RT"=mass(mi_spec[[idx]])[pb[1]:pb[2]], "Iso1" = intensity(mi_spec[[idx]])[pb[1]:pb[2]], "Iso2" = intensity(si_spec[[idx]])[pb[1]:pb[2]])
            out[,"Ratio"] <- out[,3]/out[,2]
            if ("correct_drift" %in% opt) {
              out[,"Ratio"] <- out[,"Ratio"]*tmp[j,"k"]
            }
            out[!is.finite(out[,"Ratio"]),"Ratio"] <- NA
            return(out)
          })
        })
        rng_R <- range(sapply(peak_details, function(y) { 
          sapply(y, function(x) { quantile(x[,"Ratio"], cdf, na.rm=TRUE)  })
        }))
      }
      # highlight peak selected in table
      if (!is.null(sel_pk) && pks[sel_pk,"Sample"]==idx) {
        sel_pk_data <- peak_details[[ifelse(length(idx_all)==1, 1, idx)]][[pks[sel_pk,"Peak ID"]]]
        lines(x = sel_pk_data[,"RT"], y = sel_pk_data[,"Iso1"], col=cols[pks[sel_pk,"Sample"]], lwd=3)
      }
      # plot symbols at peak apex
      #points(x=mass(mi_pks[[idx]]), y=intensity(mi_pks[[idx]]), col = 1, pch = 21, bg = cols[idx])
      if ("overlay_drift" %in% opt) {
        max_I <- max(yrng)
        dfs <- lapply(peak_details[[ifelse(length(idx_all)==1, 1, idx)]], function(x) {
          flt <- is.finite(x[,"Ratio"])
          y <- x[flt,"Ratio"]-rng_R[1]
          x[flt,"Ratio_norm"] <- y*(max_I/diff(rng_R))
          return(x)
        })
        for (j in 1:length(dfs)) {
          points(x=dfs[[j]][,"RT"], y=dfs[[j]][,"Ratio_norm"], pch=".", col=cols[idx])
        }
        if (idx==idx_all[1]) {
          at <- axTicks(side = 4)
          #at_val <- dfs[[1]][,"Ratio"][sapply(at, function(x) { which.min(abs(dfs[[1]][,"Ratio_norm"]-x)) })]
          at_val <- seq(rng_R[1], rng_R[2], length.out=length(at))
          at_test <- all(at_val<1)
          at_val <- round(ifelse(at_test,100,1)*at_val, ifelse(at_test,2,1))
          axis(side = 4, at=at, labels = at_val)
          mtext(text = paste0(ylab2, ifelse(at_test," [%]","")), side = 4, adj=0.5, line=3)
        }
      }
    }
    if ("overlay_pb" %in% opt) {
      pks_sam <- pks[pks[,"Sample"]==idx,]
      for (j in 1:nrow(pks_sam)) { 
        pb <- c(pks_sam[j,"Scan start"], pks_sam[j,"Scan end"])
        abline(v=sm[pb], col=cols[idx])
        mtext(text = j, side = 1, at = sm[pb[1]], adj = 0, line = -1.1, col=cols[idx])
      }
    }
  }
}

#' @title ic_deltaplot
#' @description tbd.
#' @param df data.frame containing delta values.
#' @return An annotated plot of delta values.
#' @keywords internal
#' @noRd
ic_deltaplot <- function(df) {
  df <- df[is.finite(df[,grep("Mean Delta", colnames(df))]),]
  df[,"Ratio method"] <- factor(df[,"Ratio method"], levels=c("PBP","PAI","LRS"))
  cols <- c(5:7)[as.numeric(df[,"Ratio method"])]
  pchs <- c(21,22,24)[as.numeric(df[,"Ratio method"])]
  x <- factor(df[,"Zone [%]"])
  x_ann <- levels(x)
  x <- as.numeric(x) + c(-0.05,0,0.05)[as.numeric(df[,"Ratio method"])]
  y <- df[,grep("Mean Delta", colnames(df))]
  e <- df[,grep("SD Delta", colnames(df))]
  par(mar = c(4.5, 4.5, 1.5, 0.5))
  plot(x=range(x)+c(-1,1)*0.1*diff(range(x)), y=range(rep(y,2)+rep(c(-1,1),each=length(y))*2*e), type="n", xlab="Zone [%] (values are slightly shifted to improve visibility)", ylab="Mean Delta", axes=F)
  axis(2); axis(1, at=1:length(x_ann), labels = x_ann); box()
  legend(x = "top", horiz=TRUE, pch=c(21,22,24), pt.bg=c(5:7), legend=levels(df[,"Ratio method"]))
  segments(x0 = x, y0 = y-2*e, y1 = y+2*e, col = cols)
  segments(x0 = x, y0 = y-2*e, y1 = y+2*e, col = cols)
  points(x = x, y = y, pch = pchs, bg = cols, cex=2)
  invisible(NULL)
}