#' @title ic_app
#'
#' @description \code{ic_app} will start a shiny app that allows to upload raw
#'  data, process selectively and analyze different methods of ratio calculation
#'  between two intensity traces.
#'
#' @details The app is described in detail in
#'  <\href{https://jali.shinyapps.io/IsoCor}{this publication}>.
#'
#' @references \url{https://jali.shinyapps.io/IsoCor}
#'
#' @return A shiny app object.
#'
#' @seealso \link{iso_ratio}
#'
#' @import shiny
#' @importFrom bsplus use_bs_tooltip bs_embed_tooltip %>%
#' @importFrom DT DTOutput renderDT JS
#' @importFrom graphics abline axTicks axis box legend lines mtext par points segments
#' @importFrom grDevices grey
#' @importFrom htmltools HTML tags h2 h3 h5 div strong em p img
#' @importFrom MALDIquant transformIntensity smoothIntensity removeBaseline detectPeaks createMassSpectrum mass intensity
#' @importFrom plyr ldply
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs useShinyjs hide show enable disable toggle
#' @importFrom stats median rnorm sd quantile
#' @importFrom utils data packageDate packageVersion
#'
#' @export
ic_app <- function() {

  # prepare the app environment ----
  golem::add_resource_path(
    'www', app_sys('app/www')
  )
  shiny::tags$head(
    golem::bundle_resources(
      path = app_sys('app/www'),
      app_title = get_golem_config("app_name")
    ),
    golem::favicon(ico = "BAMLogo"),
    # Add here other external resources
    shinyjs::useShinyjs()
  )
  
  status_line <- paste0("v", get_golem_config("app_version"), " | ", get_golem_config("app_date"), " | jan.lisec@bam.de")
  tde <- new.env()
  utils::data("testdata", package = "IsoCor", envir = tde)
  utils::data("isotopes", package = "IsoCor", envir = tde)

  # set up app UI ----
  ui <- shiny::tagList(
    shinyjs::useShinyjs(),
    bsplus::use_bs_tooltip(),
    shiny::tags$head(
      tags$link(rel = "icon", href = "www/favicon.ico"),
      includeScript(app_sys("app/www/js/screen_height.js"))
    ),
    fluidPage(
      title = "IsoCor",
      fluidRow(
        column(
          width = 3,
          id = "options_panel",
          style="height: 100vh; background-color: #F5F5F5",
          column(
            width = 12,
            style = "padding-top: 10px; height: 800px;",
            fluidRow(
              shiny::div(
                class = "verticalhorizontal",
                style = "font-size:20px;",
                img(src = "www/bam_logo_20pt.gif", position = "absolute", margin = "auto", alt="BAM Logo"),
                strong("BAM"), em("IsoCor"),
                position="relative"
              )
            ),
            fluidRow(
              shiny::h4(shiny::actionLink(inputId = "ic_help01", label = "Help")),
              shiny::helpText("Please click on the word 'Help' above when you are a first time user"),
              shiny::h4(shiny::actionLink(inputId = "ic_help02", label = "Data")),
              fluidRow(
                column(width = 4, selectInput(inputId = "ic_par_libsource", label = "Data source", choices = list("testdata", "upload files"), selected = "testdata")),
                column(width = 4, selectInput(inputId = "ic_par_inputformat", label = "File format", choices = list("exp", "icp", "data", "generic"), selected = "exp")),
                column(width = 4, uiOutput(outputId = "ic_par_path_expfiles"))
              ),
              shiny::h4(shiny::actionLink(inputId = "ic_help03", label = "Import")),
              fluidRow(
                column(width = 6, selectInput(inputId = "ic_par_rt_col", label = "RT column", choices = c("")) %>% bs_embed_tooltip(title = "Select RT column.")),
                column(width = 6, textInput(inputId = "ic_par_mi_rt_unit", label = "RT unit", value = "min"))
              ),
              fluidRow(
                column(width = 4, selectInput(inputId = "ic_par_mi_col", label = "MI column", choices = c("")) %>% bs_embed_tooltip(title = "Select Master Isotope column.")),
                column(width = 4, textInput(inputId = "ic_par_mi_col_name", label = "MI Name")),
                column(width = 4, numericInput(inputId = "ic_par_mi_amu", label = "MI amu", value = 0, step = 0.0001))
              ),
              fluidRow(
                column(width = 4, selectInput(inputId = "ic_par_si_col", label = "SI column", choices = c("")) %>% bs_embed_tooltip(title = "Select Secondary Isotope column.")),
                column(width = 4, textInput(inputId = "ic_par_si_col_name", label = "SI Name")),
                column(width = 4, numericInput(inputId = "ic_par_si_amu", label = "SI amu", value = 0, step = 0.0001))
              ),
              shiny::h4(shiny::actionLink(inputId = "ic_help04", label = "Processing")),
              fluidRow(
                column(width = 4, numericInput(inputId = "ic_par_halfWindowSize", label = "Smoothing", value = 25, min=0, max=50, step=1) %>% bs_embed_tooltip(title = "Smoothing parameter: 'half window size' of peak. Set to '0' to omit this processing step.")),
                column(width = 4, selectInput(inputId = "ic_par_baseline_method", label = "Baseline Correction", choices = c("none", "SNIP", "TopHat", "ConvexHull", "median"), selected = "SNIP") %>% bs_embed_tooltip(title = "Select method for baseline estimation or 'none' to omit this processing step.")),
                column(width = 4, numericInput(inputId = "ic_par_peakpicking_SNR", label = "Peak Picking (SNR)", value = 25, min=1, max=100, step=1) %>% bs_embed_tooltip(title = "Peak picking parameter: 'Signal/Noise ratio' [range: 1..100].")),
              ),
              uiOutput(outputId = "footer")
            )
          )
        ),
        # main panel
        column(
          width = 9,
          id = "main_panel",
          fluidRow(
            column(
              width = 10,
              div(style="background-color: orange; width: 20px; text-align: center;", actionLink("sidebar_button", "", icon = icon("angle-left"), style="text-align: center")),
              plotOutput(
                outputId = "ic_specplot", 
                height = "400px", 
                dblclick = dblclickOpts(id = "ic_specplot_dblclick"), 
                brush = brushOpts(id = "ic_specplot_brush", direction = "x", resetOnNew = TRUE)
              ) %>% bs_embed_tooltip(title = "You may select a time range [Click and Drag] with the cursor to zoom. Use [Double Click] to unzoom.", placement = "bottom")
            ),
            column(
              width = 2,
              style="height: 420px; background-color: #F5F5F5",
              shiny::column(width=12, p(),
              #shiny::wellPanel(
                selectInput(inputId = "ic_par_focus_sample", label = "Focus sample", choices = list("Sample 1"=1)),
                checkboxGroupInput(
                  inputId = "ic_par_specplot", 
                  label = shiny::actionLink(inputId = "ic_help05", label = "Plot options"),
                  #label = "Plot options", 
                  choices =  list("show all samples" = "overlay_mi",
                                  "show peak boundaries"="overlay_pb",
                                  "show sample IDs" = "overlay_legend",
                                  "overlay SI trace" = "overlay_si",
                                  "overlay ratio points" = "overlay_drift",
                                  "correct ratio points" = "correct_drift"), 
                  selected = c("overlay_pb","overlay_mi","overlay_drift")
                ),
                fluidRow(
                  actionButton(inputId = "ic_par_cut_range", label = "cut range", width='45%') %>% bs_embed_tooltip(title = "Cut samples to currently visible range."),
                  actionButton(inputId = "ic_par_align_rt", label = "align rt", width='45%') %>% bs_embed_tooltip(title = "Align samples at peak maxima.")
                ),
                fluidRow(
                  actionButton(inputId = "ic_par_set_drift", label = "filter points", width='45%') %>% bs_embed_tooltip(title = "Set upper and lower quantile to filter depicted ratio points.")
                )
              )
            )
          ),
          tabsetPanel(
            tabPanel(
              title = "Peak table", p(""),
              #shiny::actionLink(inputId = "ic_help06", label = "Help Peak table"),
              DTOutput("ic_table_peaks")
            ),
            tabPanel(
              title = "Ratio table", p(""),
              #shiny::actionLink(inputId = "ic_help07", label = "Help Ratio table"),
              DTOutput("ic_table_ratios")# %>% bs_embed_tooltip(title = "Tooltip.")
            ),
            tabPanel(
              title = "Delta table", p(""),
              #DTOutput("ic_table_deltas"),
              #shiny::actionLink(inputId = "ic_help08", label = "Help Delta table"),
              fluidRow(
                #column(width = 8, plotOutput(outputId = "ic_deltaplot", height = "400px")),
                column(width = 8, DTOutput("ic_table_deltas")),
                column(width = 4, plotOutput(outputId = "ic_deltaplot2", height = "400px"))
              )
            ),
            # tabPanel(
            #   title = "Drift plot", p(""),
            #   plotOutput(outputId = "ic_driftplot", height = "400px")
            # ),
            id="ic_tabPanel_tables"
          )
        )
      )
    )
  )
  
  # Define server logic ----
  server <- function(input, output, session) {
  
    ### setup Options ############################################################
    # increase maximum file size for upload
    old_options <- options()
    on.exit(options(old_options))
    options(shiny.maxRequestSize=30*1024^2) # BrukerFlex Files are >5MB
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
  
    # load testdata on app start
    testdata <- get0(x = "testdata", envir = tde)
    isotopes <- get0(x = "isotopes", envir = tde)
    
    output$ic_par_path_expfiles <- renderUI({
      # file input as renderUI to allow a reset in case that the upload method is changed
      message("output$ic_par_path_expfiles")
      fileInput(inputId = "ic_par_path_expfiles", label = "Select Files", multiple = TRUE)
    })
    
    output$footer <- renderUI({
      div(
        style=paste0("margin-top: ", input$CurrentScreenHeight-690, "px; align: top"),
        hr(),
        helpText(status_line, align="left")
      )
    })
    
    ### setup reactive Values ####################################################
    # the editable peak table
    ic_table_peaks_edit <- shiny::reactiveVal()
    # setup plot range (min, max)
    spec_plots_xmin <- reactiveVal(0)
    spec_plots_xmax <- reactiveVal(10000)
    # the time range if cutting is applied
    cut_range <- reactiveValues("min"=NULL, "max"=NULL)
    # the rt shift applied to samples for alignment
    rt_shift <- reactiveVal(0)
    # indicator if range cut is currently applied
    status_range_cut <- reactiveVal("off")
    # indicator if alignment is currently applied
    status_align <- reactiveVal("off")
    # preset zone values
    zones <- reactiveVal(c(1,0.95,0.9,0.8))
    # preset coef value
    current_coef <- reactiveVal(1)
    # initial drift_filter
    current_drift_filter <- reactiveVal(c(0.1,0.9))
    
    
    ### internal functions #######################################################
    # define the (pre) processing steps in a functions
    MALDIquant_pre_process <- function(x) {
      # if (isolate(input$ic_par_halfWindowSize)>0) {
      #   x <- smoothIntensity(
      #     object = x, 
      #     method = "MovingAverage", 
      #     halfWindowSize = isolate(input$ic_par_halfWindowSize)
      #   )
      # }
      if (isolate(input$ic_par_baseline_method)!="none") {
        if (input$ic_par_baseline_method=="Dariyah") {
        } else {
          x <- removeBaseline(
            object = x, 
            method = isolate(input$ic_par_baseline_method)
          )
        }
      } 
      return(x)
    }
    
    # peak detection function
    MALDIquant_peaks <- function(x) {
      hWS <- isolate(input$ic_par_halfWindowSize)
      if (hWS>0 && hWS<floor(length(x)/2)) {
        x <- smoothIntensity(
          object = x, 
          method = "MovingAverage", 
          halfWindowSize = isolate(input$ic_par_halfWindowSize)
        )
      }
      # set a minimum hWS to detect peaks
      hWS <- ifelse(hWS>0, hWS, 25)
      out <- detectPeaks(
        object = x, 
        method = "MAD",
        halfWindowSize = hWS,
        SNR = isolate(input$ic_par_peakpicking_SNR)
      )
      noise <- isolate(input$ic_par_peakpicking_SNR)*min(MALDIquant::estimateNoise(x)[,2], na.rm=TRUE)
      #noise <- 0
      out@metaData[["pb"]] <- ldply(out@mass, function(p) { 
        find_peak_boundaries(
          int = intensity(x), 
          p = which.min(abs(mass(x) - p)),
          k = 3, min_scans = 5, noise = noise
        )
      })
      return(out)
    }
    
    ### reactives ################################################################
    # get input data as list of tables
    file_in <- reactive({
      req(input$ic_par_libsource)
      message("file_in")
      out <- NULL
      if (input$ic_par_libsource=="upload files") {
        if (!is.null(input$ic_par_path_expfiles)) {
          out <- try(lapply(input$ic_par_path_expfiles$datapath, function(x) {
            read_raw_data(path=x, format=input$ic_par_inputformat)
          }))
          if (inherits(x = out, what = "try-error")) {
            out <- NULL
          } else {
            names(out) <- input$ic_par_path_expfiles$name
          }
        } else {
          out <- NULL
        }
      } else {
        updateSelectInput(inputId = "ic_par_inputformat", selected="exp")
        out <- testdata
      }
      if (!is.null(out)) {
        rt_shift(rep(0, length(out)))
        updateSelectInput(inputId = "ic_par_focus_sample", choices = paste("Sample", 1:length(out)))
        if (length(out)>1) {
          enable(selector = "#ic_par_specplot input[value='overlay_mi']")
        } else {
          updateCheckboxGroupInput(inputId = "ic_par_specplot", selected = c("overlay_pb", "overlay_si"))
          disable(selector = "#ic_par_specplot input[value='overlay_mi']")
          hide(id = "ic_par_focus_sample")
        }
      } else {
        ic_table_peaks_edit(NULL)
      }
      validate(need(out, message = "No valid data"))
      return(out)
    })
    
    observeEvent(input$ic_par_specplot, {
      toggle(id = "ic_par_focus_sample", condition = !("overlay_mi" %in% input$ic_par_specplot))
    }, ignoreNULL = FALSE)
    
    # check table headers for consistency and to get colnames to allow user column selection
    file_in_cols <- reactive({
      req(file_in())
      message("file_in_cols")
      headers <- sapply(lapply(file_in(), colnames), paste, collapse="")
      validate(need(length(unique(headers))==1, message = "Files contain different headers"))
      return(colnames(file_in()[[1]]))
    })
    
    # convert input tables into MALDIquant spectra format for selected MI trace and RT column ----
    ic_mi_spectra_raw <- reactive({
      req(file_in(), input$ic_par_rt_col, input$ic_par_mi_col, cut_range$min, rt_shift())
      validate(need(all(c(input$ic_par_rt_col, input$ic_par_mi_col) %in% file_in_cols()), message = "Selected columns inconsistent with currently uploaded data"))
      validate(need(all(sapply(file_in(), function(x) { all(diff(x[,input$ic_par_rt_col])>0) })), message = "You selected a time column with non continuous values"))
      validate(need(expr = input$ic_par_halfWindowSize<floor(min(sapply(file_in(),nrow))/2), message = "Smoothing parameter is larger than 0.5 * data length"))
      message("ic_mi_spectra_raw")
      lapply(1:length(file_in()), function(k) {
        x <- file_in()[[k]]
        m <- x[, input$ic_par_rt_col]
        m <- m-rt_shift()[k]
        flt <- m>=cut_range$min & m<=cut_range$max
        m <- m[flt]
        i <- x[flt, input$ic_par_mi_col]
        suppressWarnings(
          createMassSpectrum(
            mass = m,
            intensity = i,
            metaData = list(
              name = "Name",
              file = names(file_in())[k]
            )
          )
        )
      })
    })
    
    # convert input tables into MALDIquant spectra format for selected SI trace and RT column ----
    ic_si_spectra_raw <- reactive({
      req(file_in(), input$ic_par_rt_col, input$ic_par_mi_col, cut_range$min)
      validate(need(all(sapply(file_in(), function(x) { all(diff(x[,input$ic_par_rt_col])>0) })), message = "You selected a time column with non continuous values"))
      validate(need(all(c(input$ic_par_rt_col, input$ic_par_si_col) %in% file_in_cols()), message = "Selected columns inconsistent with currently uploaded data"))
      message("ic_si_spectra_raw")
      lapply(1:length(file_in()), function(k) {
        x <- file_in()[[k]]
        m <- x[, input$ic_par_rt_col]
        m <- m-rt_shift()[k]
        flt <- m>=cut_range$min & m<=cut_range$max
        m <- m[flt]
        i <- x[flt, input$ic_par_si_col]
        suppressWarnings(
          createMassSpectrum(
            mass = m,
            intensity = i,
            metaData = list(
              name = "Name",
              file = names(file_in())[k]
            )
          )
        )
      })
    })
    
    # provide spectra based on processed raw data ----
    ic_mi_spectra <- reactive({
      req(ic_mi_spectra_raw(), input$ic_par_halfWindowSize, input$ic_par_baseline_method, input$ic_par_peakpicking_SNR)
      message("ic_mi_spectra")
      #input$ic_par_halfWindowSize
      input$ic_par_baseline_method
      #input$ic_par_peakpicking_SNR
      MALDIquant_pre_process(ic_mi_spectra_raw())
    })
  
    # provide spectra based on processed raw data ----
    ic_si_spectra <- reactive({
      req(ic_si_spectra_raw())
      message("ic_si_spectra")
      #input$ic_par_halfWindowSize
      input$ic_par_baseline_method
      #input$ic_par_peakpicking_SNR
      MALDIquant_pre_process(ic_si_spectra_raw())
    })
    
    # identify peaks in processed mi spectra ----
    ic_mi_peaks <- reactive({
      req(ic_mi_spectra())
      message("ic_mi_peaks")
      disable(id = "ic_par_align_rt")
      lapply(ic_mi_spectra(), MALDIquant_peaks)
    })
    
    # mi peak table ----
    ic_table_peaks_pre <- reactive({
      req(ic_mi_peaks())
      message("ic_table_peaks_pre")
      out <- ldply(1:length(ic_mi_peaks()), function(i) {
        x <- ic_mi_peaks()[[i]]
        sm <- mass(ic_mi_spectra()[[i]])
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
      # enable ic_par_align_rt only if consistent number of peaks are found in all samples and more than 2 samples are available
      if (length(file_in())>=2 & length(unique(table(out[,"Peak ID"])))==1) {
        enable(id = "ic_par_align_rt") 
      }
      # attach columns for mass_bias correction if consistent number of peaks are found in all samples
      #if (length(unique(table(out[,"Peak ID"])))==1) {
        out <- cbind(
          out, 
          data.frame(
            "Mass bias method"=rep(isolate(current_mb_method()), nrow(out)),
            "f_value"=rep(0, nrow(out)),
            "k"=rep(1, nrow(out)),
            check.names = FALSE
          )
        )
      #}
      return(out)
    })
    
    # total number of valid peaks based on ic_table_peaks_pre()
    # ic_n_valid_peaks <- reactive({
    #   req(ic_table_peaks_pre())
    #   df <- ic_table_peaks_pre()
    #   np <- length(unique(df[,"Peak ID"]))
    #   validate(need(length(unique(table(df[,"Peak ID"])))==1, message = "Please select processing parameters to obtain a similar number of peaks in all files"))
    #   validate(need(np>=2, message = "Please select processing parameters to obtain at least 2 peaks in all files (1 sample and 1 standard peak)"))
    #   return(np)
    # })
    
    # mi peak-type table contains selectInput's for peak type and can be modified --> this modification is evaluated here
    # ic_table_peaks_type_mod <- reactive({
    #   req(ic_n_valid_peaks())
    #   message("ic_table_peaks_type_mod")
    #   np <- ic_n_valid_peaks()
    #   out <- data.frame("Peak ID"=1:np, "Type"=as.character(NA), check.names = FALSE)
    #   for (i in 1:np) {
    #     out[i,"Type"] <- input[[paste0("ic_pt", i)]]
    #   }
    #   validate(need(all(c("sample","standard") %in% out[,"Type"]), message = "At least 1 peak must be defined as 'standard' and 1 as 'sample'"))
    #   return(out)
    # })
    
    # mi/si ratio calculation ----
    ic_table_ratios_pre <- reactive({
      req(ic_table_peaks_edit(), ic_si_spectra(), ic_mi_spectra(), zones())
      pks <- ic_table_peaks_edit()
      validate(need("Type" %in% colnames(pks), message = "Please specify processing parameters to obtain a similar number of peaks per sample."))
      validate(need(all(c("sample","standard") %in% pks[,"Type"]), message = "Please specify at least one peak as 'sample' and one peak as 'standard' in peak table."))
      message("ic_table_ratios_pre")
      # For every sample...
      out <- ldply(1:length(ic_mi_peaks()), function(i) {
        x <- ic_mi_peaks()[[i]]
        # if (input$ic_par_ratiofromraw) {
        #   smM <- mass(ic_mi_spectra()[[i]])
        #   siM <- intensity(ic_mi_spectra()[[i]])
        #   smS <- mass(ic_si_spectra()[[i]])
        #   siS <- intensity(ic_si_spectra()[[i]])
        # } else {
        smM <- mass(ic_mi_spectra()[[i]])
        siM <- intensity(ic_mi_spectra()[[i]])
        smS <- mass(ic_si_spectra()[[i]])
        siS <- intensity(ic_si_spectra()[[i]])
        # }
        pks_sam <- pks[pks[,"Sample"]==i,,drop=FALSE]
        dfs <- lapply(pks_sam[,"Peak ID"], function(j) {
          pb <- c(pks_sam[j,"Scan start"], pks_sam[j,"Scan end"])
          return(data.frame("Iso1" = siM[pb[1]:pb[2]], "Iso2" = siS[pb[1]:pb[2]]))
        })
        ks <- as.numeric(pks[pks[,"Sample"]==i,"k"])
        ptps <- sapply(split(pks[,"Type"], pks[,"Peak ID"]), unique)
        isos <- paste(input$ic_par_si_col_name, input$ic_par_mi_col_name, sep="/")
        bl_method <- input$ic_par_baseline_method
        # For every ratio method...
        ldply(c("PBP","PAI","LRS"), function(ratio_method) {
          # For every Zone value...
          ldply(zones(), function(zone) {
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
      for (j in 1:length(sam_col)) {
        #fac <- 1000*(input$ic_par_coef - 1)
        #out[,gsub("Ratio", "Delta", colnames(out)[sam_col[j]])] <- (out[,sam_col[j]]/apply(out[,std_col,drop=FALSE], 1, mean))*fac
        # including per mille scaling
        out[,gsub("Ratio", "Delta", colnames(out)[sam_col[j]])] <- 1000*((out[,sam_col[j]]/apply(out[,std_col,drop=FALSE], 1, mean))*current_coef()-1)
        # without per mille scaling
        #out[,gsub("Ratio", "Delta", colnames(out)[sam_col[j]])] <- (out[,sam_col[j]]/apply(out[,std_col,drop=FALSE], 1, mean))*current_coef()-1
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
      for (cols in grep("Delta P", colnames(out))) { 
        # round delta values to 3 digits
        out[,cols] <- round(out[,cols], 3) 
        # add per mille sign for delta column
        colnames(out)[cols] <- paste(colnames(out)[cols], "[\u2030]")
      }
      return(out)
    })
    
    # add or remove zone levels ----
    observeEvent(input$ic_btn_add_zone, {
      shinyalert(
        html = TRUE,
        text = tagList(
          helpText("Current values:", paste(100*zones(), collapse=", ")),
          numericInput(inputId = session$ns("ic_btn_add_zone_value"), label = "Enter zone value to add", value = 0, min=0, max=100, step=1)
        ),
        cancelButtonText = "Cancel", confirmButtonText = "Add", showCancelButton = TRUE, size = "xs",
        callbackR = function(value) {
          if (value) {
            tmp <- zones()
            nv <- input$ic_btn_add_zone_value/100
            if (is.numeric(nv) && is.finite(nv) && nv>=0 && nv<=1) {
              tmp <- unique(sort(c(tmp, nv), decreasing = TRUE))
              zones(tmp)
            }
          }
        }
      )
    })
    observeEvent(input$ic_btn_rem_zone, {
      selected <- NULL
      if (!is.null(input$ic_table_ratios_rows_selected)) selected <- ic_table_ratios_pre()[input$ic_table_ratios_rows_selected,"Zone [%]"]
      shinyalert(
        html = TRUE,
        text = tagList(
          div(
            style = "margin-bottom: 160px",
            selectInput(inputId = session$ns("ic_btn_rem_zone_value"), label = "Select zone value to remove", choices = 100*zones(), selected = selected)
          )
        ),
        cancelButtonText = "Cancel", confirmButtonText = "Rem", showCancelButton = TRUE, size = "xs",
        callbackR = function(value) {
          if (value) {
            tmp <- zones()
            if (length(tmp)>=2) {
              tmp <- tmp[tmp != as.numeric(input$ic_btn_rem_zone_value)/100]
              zones(tmp)
            }
          }
        }
      )
    })
    
    # set coef ----
    observeEvent(input$ic_btn_set_coef, {
      shinyalert(
        html = TRUE,
        text = tagList(
          numericInput(inputId = "ic_par_coef", label = "Add 'coef' to normalize delta", value = current_coef(), min=0.9, max=1.1, step=0.0001) %>% bs_embed_tooltip(title = "Define coef parameter for delta calculation.")
        ),
        cancelButtonText = "Cancel", confirmButtonText = "Set", showCancelButton = TRUE, size = "xs",
        callbackR = function(value) {
          if (value) {
            current_coef(input$ic_par_coef)
          }
        }
      )
    })
    
    # delta calculation in case of at least 3 input files ---
    ic_table_deltas_pre <- reactive({
      req(ic_table_ratios_pre())
      validate(need(length(ic_si_spectra())>=3, message = "This view is only available if you uploaded at least 3 replicate measurements."))
      message("ic_table_deltas_pre")
      df <- ic_table_ratios_pre()
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
    })
    
    # table of peaks of 'new sample' ----
    shiny::observeEvent(ic_table_peaks_pre(), {
      tmp <- ic_table_peaks_pre()
      if (all(table(tmp[,"Peak ID"])==max(tmp[,"Sample"]))) {
        np <- max(tmp[,"Peak ID"])
        if (length(np)==1 && np>=2) {
          type <- c("standard", rep("sample", np-2), "standard")
          if (length(type)==2) type[2] <- "sample"
          tmp$Type <- sapply(tmp[,"Peak ID"], function(i) {type[i]})
        }
        
      }
      ic_table_peaks_edit(tmp)
    })
    
    # change plot range upon user mouse interaction (click and drag) ----
    observeEvent(input$ic_specplot_brush, {
      spec_plots_xmin(input$ic_specplot_brush$xmin)
      spec_plots_xmax(input$ic_specplot_brush$xmax)
    })
    
    # change plot range upon user mouse interaction (double click) ----
    observeEvent(input$ic_specplot_dblclick, {
      req(ic_mi_spectra())
      rng <- range(sapply(ic_mi_spectra(), function(x) { range(mass(x), na.rm=TRUE) }))
      spec_plots_xmin(rng[1])
      spec_plots_xmax(rng[2])
    })
    
    # show fileUpload only when data source is set to 'upload files' ----
    observeEvent(input$ic_par_libsource, {
      toggle(id = "ic_par_path_expfiles", condition = input$ic_par_libsource=="upload files")
      toggle(id = "ic_par_inputformat", condition = input$ic_par_libsource=="upload files")
    })
    
    # reset time windows (upon new data or new RT column)
    reset_times <- function() {
      req(file_in(), file_in_cols(), input$ic_par_rt_col)
      if (input$ic_par_rt_col %in% file_in_cols()) {
        rng <- sapply(file_in(), function(x) { range(x[,input$ic_par_rt_col], na.rm=TRUE) })
        cut_range$min <- min(rng[1,])
        cut_range$max <- max(rng[2,])
        status_range_cut("off")
        updateActionButton(inputId = "ic_par_cut_range", label = "cut range")
        # ...reset display range
        spec_plots_xmin(cut_range$min)
        spec_plots_xmax(cut_range$max)
      }
    }
    
    # update column selectors when input columns change  
    observeEvent(file_in_cols(), {
      mi_selected <- switch(
        input$ic_par_inputformat, 
        "exp"=file_in_cols()[7], 
        "icp"=file_in_cols()[2],
        "generic"=file_in_cols()[2],
        "data"=file_in_cols()[2])
      si_selected <- switch(
        input$ic_par_inputformat, 
        "exp"=file_in_cols()[9], 
        "icp"=file_in_cols()[4],
        "generic"=file_in_cols()[3],
        "data"=file_in_cols()[3])
      rt_selected <- ifelse("Minutes" %in% file_in_cols(), "Minutes", file_in_cols()[1])
      updateSelectInput(inputId = "ic_par_rt_col", choices = I(file_in_cols()), selected = rt_selected)
      #updateSelectInput(inputId = "ic_par_rt_col", choices = I(file_in_cols()), selected = input$ic_par_rt_col)
      updateSelectInput(inputId = "ic_par_mi_col", choices = I(file_in_cols()), selected = mi_selected)
      updateSelectInput(inputId = "ic_par_si_col", choices = I(file_in_cols()), selected = si_selected)
      reset_times()
    })
    
    # check and update time range filters when time column is changed
    observeEvent(input$ic_par_rt_col, {
      req(file_in())
      reset_times()
    }, ignoreInit = TRUE)
    
    # update MI/SI name inputs when input columns change
    observeEvent(input$ic_par_mi_col, {
      updateTextInput(inputId = "ic_par_mi_col_name", value = input$ic_par_mi_col)
      updateNumericInput(inputId = "ic_par_mi_amu", value = get_iso_amu(x=input$ic_par_mi_col, isotopes=isotopes))
    })
    
    # update MI/SI name inputs when input columns change
    observeEvent(input$ic_par_si_col, {
      updateTextInput(inputId = "ic_par_si_col_name", value = input$ic_par_si_col)
      updateNumericInput(inputId = "ic_par_si_amu", value = get_iso_amu(x=input$ic_par_si_col, isotopes=isotopes))
    })
    
    # set cut range to displayed spectrum range when user triggers this action button
    observeEvent(input$ic_par_cut_range, {
      req(cut_range$min, input$ic_par_rt_col, spec_plots_xmin(), spec_plots_xmax())
      
      if (status_range_cut()=="off") {
        cut_range$min <- spec_plots_xmin()
        cut_range$max <- spec_plots_xmax()
        updateActionButton(inputId = "ic_par_cut_range", label = "undo cut")
        #style="background-color: #337ab7;"
        status_range_cut("on")
      } else {
        rng <- sapply(file_in(), function(x) { range(x[,input$ic_par_rt_col], na.rm=TRUE) })
        cut_range$min <- min(rng[1,])
        cut_range$max <- max(rng[2,])
        updateActionButton(inputId = "ic_par_cut_range", label = "cut range")
        #style="background-color: #337ab7;"
        status_range_cut("off")
      }
    })
    
    # set cut range to displayed spectrum range when user triggers this action button
    observeEvent(input$ic_par_align_rt, {
      if (status_align()=="off") {
        out <- apply(sapply(split(ic_table_peaks_pre(), ic_table_peaks_pre()[,"Peak ID"]), function(x) {
          x[,"RT max"]-median(x[,"RT max"])
        }), 1, median)
        rt_shift(out)
        updateActionButton(inputId = "ic_par_align_rt", label = "undo align")
        #tags$head(tags$style(HTML(".btn-default #ic_par_align_rt{background-color:#93b193}")))
        status_align("on")
      } else {
        rt_shift(rep(0, length(file_in())))
        updateActionButton(inputId = "ic_par_align_rt", label = "align rt")
        status_align("off")
        #tags$head(tags$style(HTML(".btn-default {background-color:#d45959}")))
      }
    })
    
    observeEvent(input$ic_par_set_drift, {
      shinyalert(
        html = TRUE,
        text = tagList(
          fluidRow(
            div(style = "margin-bottom: 5px", fluidRow(column(12, strong("Points filter (lower/upper quantile)")))),
            fluidRow(
              column(width = 6, align = "center", numericInput(inputId = "ic_par_quant_low", label = NULL, value = current_drift_filter()[1], min=0, max=0.25, step=0.01, width="90%") %>% bs_embed_tooltip(title = "Define lower quantile parameter to limit depicted drift value distribution.")),
              column(width = 6, align = "center", numericInput(inputId = "ic_par_quant_high", label = NULL, value = current_drift_filter()[2], min=0.75, max=1, step=0.01, width="90%") %>% bs_embed_tooltip(title = "Define upper quantile parameter to limit depicted drift value distribution."))
            )
          )
        ),
        cancelButtonText = "Cancel", confirmButtonText = "Set", showCancelButton = TRUE, size = "xs",
        callbackR = function(value) {
          if (value) {
            current_drift_filter(c(input$ic_par_quant_low,input$ic_par_quant_high))
          }
        }
      )
      
    })
 
    # peak table output and associated action buttons ----
    output$ic_table_peaks <- renderDT({
      req(ic_table_peaks_edit())
      message("output$ic_table_peaks")
      DT::datatable(
        data = ic_table_peaks_edit(),
        "extensions" = "Buttons", 
        "options" = list(
          "server" = FALSE, 
          "dom" = "Bfti", 
          "autoWidth" = TRUE,
          "paging" = FALSE,
          "scrollY" = screen_height()-595,
          "pageLength" = -1, 
          "buttons" = list(
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
              action = DT::JS(
                "function ( e, dt, node, config ) {
                Shiny.setInputValue('ic_btn_mass_bias', 1, {priority: 'event'});
                }")
            ),
            list(
              extend = "collection",
              text = 'change peak type',
              action = DT::JS(
                "function ( e, dt, node, config ) {
                Shiny.setInputValue('ic_btn_peak_type', 1, {priority: 'event'});
                }")
            ),
            list(
              extend = "collection",
              text = '<i class="fa fa-question"></i>',
              titleAttr = 'Get Help on table',
              action = DT::JS(
                "function ( e, dt, node, config ) {
                Shiny.setInputValue('ic_help06', 1, {priority: 'event'});
                }")
            )
          )
        ), 
        "selection" = list(mode="single", target="row"), 
        "editable" = list(target = "column", disable = list(columns = c(0:8,10)), numeric = 9), 
        "rownames" = NULL
      )
    })
    
    # apply mass bias correction using table action button
    current_mb_method <- reactiveVal("none")
    shiny::observeEvent(input$ic_btn_mass_bias, {
      shinyalert(
        html = TRUE,
        text = tagList(
          fluidRow(
            column(width = 6, selectInput(inputId = "ic_par_mb_method", label = "Mass bias method", choices = c("none","Linear","Russel","Exponential"), selected = current_mb_method()) %>% bs_embed_tooltip(title = "Select mass bias method.")),
            column(
              width = 6, 
              shiny::textAreaInput(
                inputId = "txt_textAreaInput",
                label = "Copy/Paste or Enter",
                placeholder = paste("You can copy/paste a numeric Excel range of exactly 1 columns and", nrow(ic_table_peaks_edit()), "rows to fill column 'f_value' automatically. Alternatively you can enter values manually."),
                width="100%",
                rows=12
              )
            )
          )
        ),
        cancelButtonText = "Cancel", confirmButtonText = "Set", showCancelButton = TRUE, size = "s",
        callbackR = function(value) {
          if (value) {
            tab <- ic_table_peaks_edit()
            # read pasted textArea
            tmp <- strsplit(input$txt_textAreaInput, "\n")[[1]]
            # correct potential error for last col being empty
            tmp <- gsub("\t$", "\t\t", tmp)
            # split at "\t" and ensure equal length
            # convert to numeric (what is expected by downstream functions)
            tmp <- plyr::laply(tmp, function(x) {
              x <- try(as.numeric(x))
            }, .drop = FALSE)
            if(prod(dim(tmp))==nrow(tab) && all(is.finite(tmp))) {
              tab[, "f_value"] <- as.vector(unlist(tmp))
            }
            tab[,"Mass bias method"] <- rep(input$ic_par_mb_method, nrow(tab))
            current_mb_method(input$ic_par_mb_method)
            ic_table_peaks_edit(tab)
          }
        }
      )
    })
    
    observeEvent(input$ic_table_peaks_rows_selected, {
      req(ic_table_peaks_edit())
      i <- input$ic_table_peaks_rows_selected
    }, ignoreNULL = FALSE)
    
    shiny::observeEvent(input$ic_btn_peak_type, {
      if (is.null(input$ic_table_peaks_rows_selected)) {
        shinyalert(text = "Please select a table row first", type = "info")
      } else {
        if (!("Type" %in% colnames(ic_table_peaks_edit()))) {
          shinyalert(text = "Peak types can only be assigned once a similar number of peaks are found in all sampels", type = "info")
        } else {
          i <- input$ic_table_peaks_rows_selected
          shinyalert(
            html = TRUE,
            text = tagList(
              shiny::selectInput(
                inputId = "pid_select_Input",
                label = paste0("Select Peak type for #", ic_table_peaks_edit()[i,"Peak ID"]),
                choices = c("standard", "sample", "discard"),
                selected = ic_table_peaks_edit()[i,"Type"]
              )
            ),
            cancelButtonText = "Cancel", confirmButtonText = "Set", showCancelButton = TRUE, size = "xs",
            callbackR = function(value) {
              if (value) {
                tab <- ic_table_peaks_edit()
                l <- which(tab[,"Peak ID"]==tab[input$ic_table_peaks_rows_selected,"Peak ID"])
                tab[l,"Type"] <- input$pid_select_Input
                ic_table_peaks_edit(tab)
              }
            }
          )
        }
      }
    })
    
    shiny::observeEvent(input$ic_table_peaks_cell_edit, {
      # convert column values to numeric
      x <- as.numeric(gsub("[^[[:digit:]-].]", "", input$ic_table_peaks_cell_edit$value))
      # replace in correct column and update 'ic_table_peaks_edit'
      tmp <- ic_table_peaks_edit()
      tmp[, input$ic_table_peaks_cell_edit$col[1] + 1] <- x
      ic_table_peaks_edit(tmp)
    })
    
    update_k <- function() {
      #message("update k in peak table")
      isolate({
        tmp <- ic_table_peaks_edit()
        if (nrow(tmp)>=1) { 
          if (current_mb_method()=="none") {
            tmp[,"k"] <- rep(1, nrow(tmp))
          } else {
            tmp[,"k"] <- round(sapply(tmp[,"f_value"], function(x) { 
              mass_bias(mi_amu = input$ic_par_mi_amu , si_amu = input$ic_par_si_amu, method = current_mb_method(), f_value = x) 
            }), 6)
          }
        }
      })
      ic_table_peaks_edit(tmp)
    }
    
    shiny::observeEvent(ic_table_peaks_edit(), {
      update_k()
    })
    shiny::observeEvent(input$ic_par_mi_amu, {
      req(ic_table_peaks_edit())
      update_k()
    })
    shiny::observeEvent(input$ic_par_si_amu, {
      req(ic_table_peaks_edit())
      update_k()
    })
    observeEvent(input$sidebar_button, {
      # collapse the options side bar to make space for figure and tables output
      shinyjs::toggle(id = "options_panel")
      shinyjs::toggleClass("main_panel", "col-sm-9")
      shinyjs::toggleClass("main_panel", "col-sm-12")
      if (input$sidebar_button%%2 == 1) {
        shiny::updateActionLink(inputId = "sidebar_button", icon = shiny::icon("angle-right"))
      } else {
        shiny::updateActionLink(inputId = "sidebar_button", icon = shiny::icon("angle-left"))
      }
      
    })
    
    screen_height <- reactiveVal(960)
    observe({
      invalidateLater(3000)
      screen_height(input$CurrentScreenHeight)
    })
    
    # ratio(s) table ----
    output$ic_table_ratios <- renderDT({
      message("output$ic_table_ratios")
      dt <- DT::datatable(
        data = ic_table_ratios_pre(),
        "extensions" = "Buttons", 
        "options" = list(
          "server" = FALSE, 
          "dom"="Bfti", 
          "autoWidth" = TRUE,
          "paging" = FALSE,
          "scrollY" = screen_height()-595,
          "pageLength" = -1, 
          "buttons" = list(
            list(
              extend = 'csv',
              title = NULL,
              text = '<i class="fa fa-file-csv"></i>',
              titleAttr = 'Download table as .csv',
              filename = "Ratiotable"
            ),
            list(
              extend = 'excel',
              title = NULL,
              text = '<i class="fa fa-file-excel-o"></i>',
              titleAttr = 'Download table as Excel',
              filename = "Ratiotable"
            ),
            list(
              extend = "collection",
              text = 'add new zone',
              action = DT::JS(
                "function ( e, dt, node, config ) {
                Shiny.setInputValue('ic_btn_add_zone', 1, {priority: 'event'});
                }")
            ),
            list(
              extend = "collection",
              text = 'rem selected zone',
              action = DT::JS(
                "function ( e, dt, node, config ) {
                Shiny.setInputValue('ic_btn_rem_zone', 1, {priority: 'event'});
                }")
            ),
            list(
              extend = "collection",
              text = 'set coef',
              action = DT::JS(
                "function ( e, dt, node, config ) {
                Shiny.setInputValue('ic_btn_set_coef', 1, {priority: 'event'});
                }")
            ),
            list(
              extend = "collection",
              text = '<i class="fa fa-question"></i>',
              titleAttr = 'Get Help on table',
              action = DT::JS(
                "function ( e, dt, node, config ) {
                Shiny.setInputValue('ic_help07', 1, {priority: 'event'});
                }")
            )
          )
        ), 
        "selection" = list(mode="single", target="row"), 
        "rownames" = NULL
      )
      #dt <- DT::formatSignif(table = dt, columns = grep("Delta", colnames(ic_table_ratios_pre())), digits = 3)
      dt <- DT::formatCurrency(table = dt, columns = grep("Delta P", colnames(ic_table_ratios_pre())), digits = 3, currency="")
      dt <- DT::formatCurrency(table = dt, columns = grep("Ratio P", colnames(ic_table_ratios_pre())), digits = 6, currency="")
      return(dt)
    })
    
    # delta(s) table ----
    output$ic_table_deltas <- renderDT({
      message("output$ic_table_deltas")
      dt <- DT::datatable(
        data = ic_table_deltas_pre(),
        "extensions" = "Buttons", 
        "options" = list(
          "server" = FALSE, 
          "dom"="Bfti", 
          "autoWidth" = TRUE,
          "paging" = FALSE,
          "scrollY" = screen_height()-595,
          "pageLength" = -1, 
          "buttons" = list(
            list(
              extend = 'csv',
              title = NULL,
              text = '<i class="fa fa-file-csv"></i>',
              titleAttr = 'Download table as .csv',
              filename = "Deltatable"
            ),
            list(
              extend = 'excel',
              title = NULL,
              text = '<i class="fa fa-file-excel-o"></i>',
              titleAttr = 'Download table as Excel',
              filename = "Deltatable"
            ),
            list(
              extend = 'collection',
              text = '<i class="fa fa-question"></i>',
              titleAttr = 'Get Help on table',
              action = DT::JS(
                "function ( e, dt, node, config ) {
                Shiny.setInputValue('ic_help08', 1, {priority: 'event'});
                }")
            )
          )
        ),
        "selection" = list(mode="single", target="row"),  
        "rownames" = NULL
      )
      dt <- DT::formatCurrency(table = dt, columns = grep("Delta", colnames(ic_table_deltas_pre())), digits = 3, currency="")
      return(dt)
    })  
    
    # spectrum plot ----
    output$ic_specplot <- renderPlot({
      req(file_in(), ic_mi_spectra(), ic_si_spectra(), input$ic_par_si_col_name, input$ic_par_mi_col_name, ic_table_peaks_edit())
      message("output$ic_specplot")
      #browser()
      ic_specplot(
        opt = input$ic_par_specplot, 
        xrng = c(spec_plots_xmin(), spec_plots_xmax()),
        mi_spec = ic_mi_spectra(),
        si_spec = ic_si_spectra(),
        xlab = paste0("Time [", input$ic_par_mi_rt_unit, "]"),
        ylab2 = paste0(input$ic_par_si_col_name, "/", input$ic_par_mi_col_name),
        s_focus = input$ic_par_focus_sample,
        pks = ic_table_peaks_edit(),
        mi_pks = ic_mi_peaks(),
        cdf = current_drift_filter(),
        sel_pk = input$ic_table_peaks_rows_selected
      )
    })
    
    # delta plot ----
    output$ic_deltaplot2 <- renderPlot({
      req(ic_table_deltas_pre())
      message("output$ic_deltaplot2")
      df <- ic_table_deltas_pre()
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
    })
    
    # help modals ----
    shiny::observeEvent(input$ic_help01, { help_the_user(filename = "01_general") })
    shiny::observeEvent(input$ic_help02, { help_the_user(filename = "02_file_upload") })
    shiny::observeEvent(input$ic_help03, { help_the_user(filename = "03_import_params") })
    shiny::observeEvent(input$ic_help04, { help_the_user(filename = "04_processing_params") })
    shiny::observeEvent(input$ic_help05, { help_the_user(filename = "05_plot_options") })
    shiny::observeEvent(input$ic_help06, { help_the_user(filename = "06_peak_table") })
    shiny::observeEvent(input$ic_help07, { help_the_user(filename = "07_ratio_table") })
    shiny::observeEvent(input$ic_help08, { help_the_user(filename = "08_delta_table") })
  
  }
  
  # Run the application
  shinyApp(ui = ui, server = server)
  
}