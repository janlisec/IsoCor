#' @title tab_peaks.
#' @description \code{tab_peaks} will .
#' @param p MALDIquant peak list.
#' @param s MALDIquant spectra list..
#' @examples 
#' #tab_peaks(p = ic_mi_peaks, s = )
#' @return A data frame of peaks.
#' @keywords internal
#' @noRd
prep_tab_peaks <- function(p, s, mb = c("none","Linear","Russel","Exponential")) {
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

style_tab_peaks <- function(data, IDMS = FALSE, sh) {
  btn_list <- list(
    list(
      extend = 'csv',
      title = NULL,
      text = '<i class="fa fa-file-csv"></i>',
      titleAttr = 'Download table as .csv',
      filename = "Peaktable"
    ),
    list(
      extend = 'excel',
      title = NULL,
      text = '<i class="fa fa-file-excel-o"></i>',
      titleAttr = 'Download table as Excel',
      filename = "Peaktable"
    ),
    list(
      extend = "collection",
      text = 'define mass bias correction',
      action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('ic_btn_mass_bias', 1, {priority: 'event'}); }")
    ),
    list(
      extend = "collection",
      text = 'change peak type',
      action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('ic_btn_peak_type', 1, {priority: 'event'}); }")
    ),
    list(
      extend = "collection",
      text = '<i class="fa fa-question"></i>',
      titleAttr = 'Get Help on table',
      action = DT::JS(paste0("function ( e, dt, node, config ) { Shiny.setInputValue('ic_help0", ifelse(IDMS, 9, 6), "', 1, {priority: 'event'}); }"))
    )
  )
  editable <- list(target = "column", disable = list(columns = c(0:8,10)), numeric = 9)
  if (IDMS) {
    btn_list <- btn_list[-c(3,4)]
    editable <- FALSE
  }
  DT::datatable(
    data = data,
    "extensions" = "Buttons", 
    "options" = list(
      "server" = FALSE, 
      "dom" = "Bfti", 
      "autoWidth" = TRUE,
      "paging" = FALSE,
      "scrollY" = sh-570,
      "pageLength" = -1, 
      "buttons" = btn_list
    ), 
    "selection" = list(mode="single", target="row"), 
    "editable" = editable, 
    "rownames" = NULL
  )
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
    "extensions" = "Buttons",
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

#' @title prep_tab_ratios.
#' @param pks ic_table_peaks_edit().
#' @param mi_pks ic_mi_peaks().
#' @param mi_spc ic_mi_spectra().
#' @param si_spc ic_si_spectra().
#' @param isos paste(input$ic_par_si_col_name, input$ic_par_mi_col_name, sep="/").
#' @param bl_method input$ic_par_baseline_method.
#' @param zones zones().
#' @param current_coef current_coef().
#' @return A data frame of peaks.
#' @keywords internal
#' @noRd
prep_tab_ratios <- function(pks, mi_pks, mi_spc, si_spc, isos, bl_method, zones, current_coef) {
  # For every sample...
  out <- ldply(1:length(mi_pks), function(i) {
    x <- mi_pks[[i]]
    smM <- mass(mi_spc[[i]])
    siM <- intensity(mi_spc[[i]])
    smS <- mass(si_spc[[i]])
    siS <- intensity(si_spc[[i]])
    pks_sam <- pks[pks[,"Sample"]==i,,drop=FALSE]
    dfs <- lapply(pks_sam[,"Peak ID"], function(j) {
      pb <- c(pks_sam[j,"Scan start"], pks_sam[j,"Scan end"])
      return(data.frame("Iso1" = siM[pb[1]:pb[2]], "Iso2" = siS[pb[1]:pb[2]]))
    })
    ks <- as.numeric(pks[pks[,"Sample"]==i,"k"])
    if ("Type" %in% colnames(pks)) {
      ptps <- sapply(split(pks[,"Type"], pks[,"Peak ID"]), unique)
    } else {
      ptps <- rep("none", length(unique(pks[,"Peak ID"])))
    }
    # For every ratio method...
    ldply(c("PBP","PAI","LRS"), function(ratio_method) {
      # For every Zone value...
      ldply(zones, function(zone) {
        out <- data.frame(
          "Sample"=i, 
          "Isotopes"=isos,
          "BL method"=bl_method,
          "Ratio method"=ratio_method, 
          "Zone [%]"=round(100*zone), 
          check.names = FALSE, stringsAsFactors = FALSE
        )
        for (j in 1:length(dfs)) {
          out[,paste0("Ratio P", j, " (", ptps[j], ")")] <- ks[j] * iso_ratio(data = dfs[[j]], method = ratio_method, thr = zone)
          out[,paste0("Points P", j, " (", ptps[j], ")")] <- sum(dfs[[j]][,1] >= (1-zone)*max(dfs[[j]][,1], na.rm=TRUE))
        }
        return(out)
      })
    })
  })
  sam_col <- grep("Ratio P[[:digit:]] [(]sample[)]", colnames(out))
  std_col <- grep("Ratio P[[:digit:]] [(]standard[)]", colnames(out))
  dis_col <- grep("discard", colnames(out))
  if (length(sam_col)>=1) {
    for (j in 1:length(sam_col)) {
      # including per mille scaling
      out[,gsub("Ratio", "Delta", colnames(out)[sam_col[j]])] <- 1000*((out[,sam_col[j]]/apply(out[,std_col,drop=FALSE], 1, mean))*current_coef-1)
    }
  }
  # remove discarded peaks here
  if (length(dis_col)>=1) {
    out <- out[,-dis_col]
  }
  # remove calculations for method=LRS where zone=0%
  out <- out[!(out[,"Zone [%]"]==0 & out[,"Ratio method"]=="LRS"),]
  # remove calculations where Delta did not yield a finite value
  out <- out[!(out[,"Zone [%]"]==0 & out[,"Ratio method"]=="PAI"),]
  # round values
  for (cols in grep("Ratio P", colnames(out))) { out[,cols] <- round(out[,cols], 6) }
  if (any(grep("Delta P", colnames(out)))) {
    for (cols in grep("Delta P", colnames(out))) { 
      # round delta values to 3 digits
      out[,cols] <- round(out[,cols], 3) 
      # add per mille sign for delta column
      colnames(out)[cols] <- paste(colnames(out)[cols], "[\u2030]")
    }
  }
  return(out)
}

#' @title prep_tab_deltas.
#' @param df ic_table_ratios_pre().
#' @param prec Rounding precision for output columns `Mean Delta` and `SD Delta`.
#' @return A data frame of peaks.
#' @keywords internal
#' @noRd
prep_tab_deltas <- function(df, prec = 3) {
  message("ic_table_deltas_pre")
  p_cols <- grep("Delta", colnames(df))
  # for each Peak...
  out <- plyr::ldply(p_cols, function(j) {
    plyr::ldply(split(df, interaction(df[,"Ratio method"], df[,"Zone [%]"], drop=TRUE)), function(x) {
      tmp <- x[1, c("Ratio method","Zone [%]"), drop=FALSE]
      tmp[,"Mean Delta"] <- mean(x[,j])
      tmp[,"SD Delta"] <- sd(x[,j])
      tmp[,"Peak"] <- gsub("[^[:digit:]]", "", colnames(x)[j])
      return(tmp)
    }, .id = NULL)
  }, .id = NULL)
  out[,"Mean Delta"] <- round(out[,"Mean Delta"], 3)
  out[,"SD Delta"] <- round(out[,"SD Delta"], 3)
  # add per mille sign to colnames
  colnames(out) <- gsub("Delta", "Delta [\u2030]", colnames(out))
  out <- out[order(out[,"Peak"], out[,"Ratio method"], out[,"Zone [%]"]),]
  return(out)
}
