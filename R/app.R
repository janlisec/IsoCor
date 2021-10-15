#'@title ic_app
#'
#'@description \code{ic_app} will start a shiny app.
#'
#'@details tbd.
#'
#'@return A shiny app object.
#'
#'@importFrom bsplus use_bs_tooltip bs_embed_tooltip %>%
#'@importFrom DT DTOutput renderDT JS
#'@importFrom graphics abline axTicks axis box legend lines mtext par points segments
#'@importFrom grDevices grey
#'@importFrom htmltools HTML tags h2 h3 h5 div strong em p img
#'@importFrom MALDIquant transformIntensity smoothIntensity removeBaseline detectPeaks createMassSpectrum mass intensity
#'@importFrom plyr ldply
#'@importFrom shiny fluidPage sidebarLayout sidebarPanel fluidRow column selectInput fileInput tabsetPanel tabPanel plotOutput uiOutput mainPanel helpText numericInput actionButton checkboxInput radioButtons dblclickOpts brushOpts reactiveVal isolate reactive req need observeEvent updateSelectizeInput updateNumericInput renderPrint renderPlot renderUI shinyApp updateSelectInput validate reactiveValues updateCheckboxGroupInput updateActionButton updateTextInput checkboxGroupInput tagList textInput 
#'@importFrom shinyjs useShinyjs hide show enable disable toggle
#'@importFrom stats median rnorm sd
#'@importFrom utils data
#'
#'@export
ic_app <- function() {
  
  # Define UI for application that draws a histogram
  ui <- tagList(
    useShinyjs(),
    use_bs_tooltip(),
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(
              width = 4, 
              shiny::div(
                class = "verticalhorizontal",
                img(src = "pics/bam_logo_20pt.gif", position = "absolute", margin = "auto", alt="BAM Logo"),
                strong("BAM"), em("IsoCor"), "Tool",
                position="relative"
              )
            ),
            column(width = 8, helpText("ver 0.1.7 (2021-10-14) jan.lisec@bam.de", align="right"))
          ),
          tabsetPanel(
            tabPanel(
              title = "Data", p(""),
              fluidRow(
                column(width = 6, selectInput(inputId = "ic_par_libsource", label = "Data source", choices = list("testdata", "upload files"), selected = "testdata")),
                column(width = 6, fileInput(inputId = "ic_par_path_expfiles", label = "Select exp-Files", multiple = TRUE))
              )
            ),
            tabPanel(
              title = "Options",
              fluidRow(
                column(width = 6,
                  h3("Import"),
                  fluidRow(
                    column(width = 6, selectInput(inputId = "ic_par_rt_col", label = "RT column", choices = c("")) %>% bs_embed_tooltip(title = "Select RT column.")),
                    column(width = 6, textInput(inputId = "ic_par_mi_rt_unit", label = "RT unit", value = "min"))
                  ),
                  fluidRow(
                    column(width = 6, selectInput(inputId = "ic_par_mi_col", label = "MI column", choices = c("")) %>% bs_embed_tooltip(title = "Select Master Isotope column.")),
                    column(width = 6, textInput(inputId = "ic_par_mi_col_name", label = "MI Name"))
                  ),
                  fluidRow(
                    column(width = 6, selectInput(inputId = "ic_par_si_col", label = "SI column", choices = c("")) %>% bs_embed_tooltip(title = "Select Secondary Isotope column.")),
                    column(width = 6, textInput(inputId = "ic_par_si_col_name", label = "SI Name"))
                  ),
                  h3("Processing"),
                  numericInput(inputId = "ic_par_halfWindowSize", label = "Smoothing", value = 25, min=0, max=50, step=1) %>% bs_embed_tooltip(title = "Smoothing parameter: 'half window size' of peak. Set to '0' to omit this processing step."),
                  selectInput(inputId = "ic_par_baseline_method", label = "Baseline Correction", choices = c("none", "SNIP", "TopHat", "ConvexHull", "median"), selected = "SNIP") %>% bs_embed_tooltip(title = "Select method for baseline estimation or 'none' to omit this processing step."),
                  numericInput(inputId = "ic_par_peakpicking_SNR", label = "Peak Picking (SNR)", value = 25, min=1, max=100, step=1) %>% bs_embed_tooltip(title = "Peak picking parameter: 'Signal/Noise ratio' [range: 1..100]."),
                  fluidRow(
                    column(6, actionButton(inputId = "ic_par_cut_range", label = "cut range", width='100%') %>% bs_embed_tooltip(title = "Cut samples to currently visible range.")),
                    column(6, actionButton(inputId = "ic_par_align_rt", label = "align rt", width='100%') %>% bs_embed_tooltip(title = "Align samples at peak maxima."))
                  )
                ),
                column(
                  width = 6, 
                  h3("Display"),
                  selectInput(inputId = "ic_par_focus_sample", label = "Focus sample", choices = list("Sample 1"=1)),
                  checkboxGroupInput(
                    inputId = "ic_par_specplot", 
                    label = "Plot options", 
                    choices =  list("show peak boundaries"="overlay_pb",
                                    "show all samples" = "overlay_mi",
                                    "overlay SI trace" = "overlay_si",
                                    "show legend" = "overlay_legend",
                                    "overlay drift" = "overlay_drift"), 
                    selected = c("overlay_pb","overlay_mi")
                  ),
                  h3("Output"),
                  fluidRow(
                    column(6, numericInput(inputId = "ic_par_zone3", label = "Zone 3", value = 80, min=0, max=100, step=1) %>% bs_embed_tooltip(title = "Define zone level for output [range: 0..100].")),
                    column(6, numericInput(inputId = "ic_par_zone4", label = "Zone 4", value = 60, min=0, max=100, step=1) %>% bs_embed_tooltip(title = "Define zone level for output [range: 0..100]."))
                  ),
                  fluidRow(
                    column(6, numericInput(inputId = "ic_par_coef", label = "coef", value = 0.9997, min=0.9, max=1.1, step=0.0001) %>% bs_embed_tooltip(title = "Define coef parameter for delta calculation."))
                  ),
                  h3("Peak table"),
                  uiOutput("ic_peaks_type_msg"),
                  div(id="div_ic_pt1", class="ptcell", fluidRow(column(2, h5("1")), column(10, selectInput(inputId = "ic_pt1", label = NULL, choices = c("sample","standard","discard"), selected = "standard")))),
                  div(id="div_ic_pt2", class="ptcell", fluidRow(column(2, h5("2")), column(10, selectInput(inputId = "ic_pt2", label = NULL, choices = c("sample","standard","discard"))))),
                  div(id="div_ic_pt3", class="ptcell", fluidRow(column(2, h5("3")), column(10, selectInput(inputId = "ic_pt3", label = NULL, choices = c("sample","standard","discard"))))),
                  div(id="div_ic_pt4", class="ptcell", fluidRow(column(2, h5("4")), column(10, selectInput(inputId = "ic_pt4", label = NULL, choices = c("sample","standard","discard"))))),
                  div(id="div_ic_pt5", class="ptcell", fluidRow(column(2, h5("5")), column(10, selectInput(inputId = "ic_pt5", label = NULL, choices = c("sample","standard","discard"))))),
                  div(id="div_ic_pt6", class="ptcell", fluidRow(column(2, h5("6")), column(10, selectInput(inputId = "ic_pt6", label = NULL, choices = c("sample","standard","discard"))))),
                  div(id="div_ic_pt7", class="ptcell", fluidRow(column(2, h5("7")), column(10, selectInput(inputId = "ic_pt7", label = NULL, choices = c("sample","standard","discard"))))),
                  div(id="div_ic_pt8", class="ptcell", fluidRow(column(2, h5("8")), column(10, selectInput(inputId = "ic_pt8", label = NULL, choices = c("sample","standard","discard"))))),
                  div(id="div_ic_pt9", class="ptcell", fluidRow(column(2, h5("9")), column(10, selectInput(inputId = "ic_pt9", label = NULL, choices = c("sample","standard","discard"))))),
                  div(id="div_ic_pt10", class="ptcell", fluidRow(column(2, h5("10")), column(10, selectInput(inputId = "ic_pt10", label = NULL, choices = c("sample","standard","discard"))))),
                  tags$head(tags$style(HTML(".ptcell .h5 { text-align: center; }"))),
                  tags$head(tags$style(HTML(".ptcell .selectize-control { margin-bottom: 0px }"))),
                  tags$head(tags$style(HTML(".ptcell .control-label { margin-bottom: 0px }"))),
                  tags$head(tags$style(HTML(".ptcell .form-group { margin-bottom: 0px }")))
                )
              )
            ),
            id = "ic_par_panel",
            selected = "Options",
            type = "tabs"
          )
        ),
    
        mainPanel(
          plotOutput(
            outputId = "ic_specplot", 
            height = "400px", 
            dblclick = dblclickOpts(id = "ic_specplot_dblclick"), 
            brush = brushOpts(id = "ic_specplot_brush", direction = "x", resetOnNew = TRUE)
          ) %>% bs_embed_tooltip(title = "You may select a mass range [Click and Drag] with the cursor to zoom. Use [Double Click] to unzoom.", placement = "left"),
          tabsetPanel(
            tabPanel(
              title = "Peak table", p(""),
              DTOutput("ic_table_peaks")
            ),
            tabPanel(
              title = "Ratio table", p(""),
              DTOutput("ic_table_ratios")# %>% bs_embed_tooltip(title = "Tooltip.")
            ),
            tabPanel(
              title = "Delta table", p(""),
              DTOutput("ic_table_deltas"),
              fluidRow(
                column(width = 8, plotOutput(outputId = "ic_deltaplot", height = "400px")),
                column(width = 4, plotOutput(outputId = "ic_deltaplot2", height = "400px"))
              )
            ),
            tabPanel(
              title = "Drift plot", p(""),
              plotOutput(outputId = "ic_driftplot", height = "400px")
            ),
            id="ic_tabPanel_tables"
          )
        )
      )
    )
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
  
    ### setup Options ############################################################
    # increase maximum file size for upload
    options(shiny.maxRequestSize=30*1024^2) # BrukerFlex Files are >5MB
  
    ### setup reactive Values ####################################################
    # setup plot range (min, max)
    spec_plots_xmin <- reactiveVal(0)
    spec_plots_xmax <- reactiveVal(10000)
    # the time range if cutting is applied
    cut_range <- reactiveValues("min"=0, "max"=10000)
    # the rt shift applied to samples for alignment
    rt_shift <- reactiveVal(0)
    # indicator if range cut is currently applied
    status_range_cut <- reactiveVal("off")
    # indicator if alignment is currently applied
    status_align <- reactiveVal("off")
    
    ### internal functions #######################################################
    # define the (pre) processing steps in a functions
    MALDIquant_pre_process <- function(x) {
      if (isolate(input$ic_par_halfWindowSize)>0) {
        x <- smoothIntensity(
          object = x, 
          method = "MovingAverage", 
          halfWindowSize = isolate(input$ic_par_halfWindowSize)
        )
      }
      if (isolate(input$ic_par_baseline_method)!="none") {
        x <- removeBaseline(
          object = x, 
          method = isolate(input$ic_par_baseline_method)
        )
      } 
      return(x)
    }
    
    # peak detection function
    MALDIquant_peaks <- function(x) {
      hWS <- ifelse(isolate(input$ic_par_halfWindowSize)>0, input$ic_par_halfWindowSize, 25)
      detectPeaks(
        object = x, 
        method = "MAD",
        halfWindowSize = hWS,
        SNR = isolate(input$ic_par_peakpicking_SNR)
      )
    }
    
    ### reactives ################################################################
    # get input data as list of tables
    file_in <- reactive({
      req(input$ic_par_libsource)
      if (input$ic_par_libsource=="upload files") {
        if (!is.null(input$ic_par_path_expfiles)) {
          out <- lapply(input$ic_par_path_expfiles$datapath, read_raw_data)
          names(out) <- input$ic_par_path_expfiles$name
        } else {
          out <- NULL
        }
      } else {
        tde <- new.env()
        utils::data(testdata, envir = tde)
        out <- get0(x = "testdata", envir = tde)
        #out <- lapply(dir(path = "www", pattern = ".exp$", full.names = TRUE), read_raw_data)
        #names(out) <- dir(path = "www", pattern = ".exp$")
      }
      if (!is.null(out)) {
        rt_shift(rep(0, length(out)))
        updateSelectInput(inputId = "ic_par_focus_sample", choices = paste("Sample", 1:length(out)))
        if (length(out)>1) {
          enable(selector = "#ic_par_specplot input[value='overlay_mi']")
          show(id = "ic_par_focus_sample")
        } else {
          updateCheckboxGroupInput(
            inputId = "ic_par_specplot",
            selected = c("overlay_pb", "overlay_si")
          )
          disable(selector = "#ic_par_specplot input[value='overlay_mi']")
          hide(id = "ic_par_focus_sample")
        }
      }
      return(out)
    })
    
    # check table headers for consistency and to get colnames to allow user column selection
    file_in_cols <- reactive({
      req(file_in())
      headers <- sapply(lapply(file_in(), colnames), paste, collapse="")
      validate(need(length(unique(headers))==1, message = "Files contain different headers"))
      return(colnames(file_in()[[1]]))
    })
    
    # convert input tables into MALDIquant spectra format for selected MI trace and RT column
    ic_mi_spectra_raw <- reactive({
      req(file_in(), input$ic_par_rt_col, input$ic_par_mi_col, cut_range, rt_shift())
      validate(need(all(sapply(file_in(), function(x) { all(diff(x[,input$ic_par_rt_col])>0) })), message = "You selected a time column with non continuous values"))
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
            metaData = list(name="Name")
          )
        )
      })
    })
    
    # convert input tables into MALDIquant spectra format for selected SI trace and RT column
    ic_si_spectra_raw <- reactive({
      req(file_in(), input$ic_par_rt_col, input$ic_par_mi_col, cut_range)
      validate(need(all(sapply(file_in(), function(x) { all(diff(x[,input$ic_par_rt_col])>0) })), message = "You selected a time column with non continuous values"))
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
            metaData = list(name="Name")
          )
        )
      })
    })
    
    # provide spectra based on processed raw data
    ic_mi_spectra <- reactive({
      input$ic_par_halfWindowSize
      input$ic_par_baseline_method
      input$ic_par_peakpicking_SNR
      req(ic_mi_spectra_raw())
      MALDIquant_pre_process(ic_mi_spectra_raw())
    })
  
    # provide spectra based on processed raw data
    ic_si_spectra <- reactive({
      input$ic_par_halfWindowSize
      input$ic_par_baseline_method
      input$ic_par_peakpicking_SNR
      req(ic_si_spectra_raw())
      MALDIquant_pre_process(ic_si_spectra_raw())
    })
    
    # identify peaks in processed mi spectra
    ic_mi_peaks <- reactive({
      req(ic_mi_spectra())
      disable(id = "ic_par_align_rt")
      lapply(ic_mi_spectra(), MALDIquant_peaks)
    })
    
    # mi peak table
    ic_table_peaks_pre <- reactive({
      req(ic_mi_peaks())
      out <- ldply(1:length(ic_mi_peaks()), function(i) {
        x <- ic_mi_peaks()[[i]]
        sm <- mass(ic_mi_spectra()[[i]])
        si <- intensity(ic_mi_spectra()[[i]])
        pm <- mass(ic_mi_peaks()[[i]])
        rnd_time <- 2
        ldply(1:length(x@mass), function(j) {
          pb <- find_peak_boundaries(
            int = si, 
            p = which.min(abs(sm - pm[j])),
            k = 3, min_scans = 5
          )
          data.frame(
            "Sample"=i, 
            "Peak ID"=j, 
            "MT max"=round(x@mass[j], rnd_time), 
            "MT start"=round(sm[pb[1]], rnd_time), 
            "MT end"=round(sm[pb[2]], rnd_time),
            "Scan start"=pb[1], 
            "Scan end"=pb[2],
            "Scan length"=diff(pb),
            check.names = FALSE, stringsAsFactors = FALSE)
        })
      })
      out <- out[order(out[,"Peak ID"]),]
      # enable ic_par_align_rt only if consistent number of peaks are found in all samples and more than 2 samples are available
      if (length(file_in())>=2 & length(unique(table(out[,"Peak ID"])))==1) {
        enable(id = "ic_par_align_rt") 
      }
      return(out)
    })
    
    # mi peak-type table (to allow user a specification: c("sample","standard","discard"))
    
    max_peaks <- 10
  
    # mi peak-type table contains selectInput's for peak type and can be modified --> this modification needs to be observed here
    ic_n_valid_peaks <- reactive({
      req(ic_table_peaks_pre())
      for (i in 1:max_peaks) { hide(id = paste0("div_ic_pt", i)) }
      message("ic_n_valid_peaks")
      df <- ic_table_peaks_pre()
      np <- length(unique(df[,"Peak ID"]))
      validate(need(length(unique(table(df[,"Peak ID"])))==1, message = "Please select processing parameters to obtain a similar number of peaks in all files"))
      validate(need(np>=2, message = "Please select processing parameters to obtain at least 2 peaks in all files (1 sample and 1 standard peak)"))
      validate(need(np<=max_peaks, message = paste("Can't handle more than", max_peaks, "peaks currently. Please send an e-mail to jan.lisec@bam.de if this is required.")))
      return(np)
    })
    ic_table_peaks_type_mod <- reactive({
      req(ic_n_valid_peaks())
      message("ic_table_peaks_type_mod")
      np <- ic_n_valid_peaks()
      out <- data.frame("Peak ID"=1:np, "Type"=as.character(NA), check.names = FALSE)
      for (i in 1:np) {
        out[i,"Type"] <- input[[paste0("ic_pt", i)]]
      }
      validate(need(all(c("sample","standard") %in% out[,"Type"]), message = "At least 1 peak must be defined as 'standard' and 1 as 'sample'"))
      return(out)
    })
    
    observeEvent(ic_n_valid_peaks(), {
      type <- c("standard", rep("sample", ic_n_valid_peaks()-2), "standard")
      if (length(type)==2) type[2] <- "sample"
      for (i in 1:ic_n_valid_peaks()) {
        show(id = paste0("div_ic_pt", i))
        if (input[[paste0("ic_pt", i)]] != type[i]) {
          updateSelectInput(inputId = paste0("ic_pt", i), selected = type[i])
        }
      }
    })
    
    # mi/si ratio calculation
    ic_table_ratios_pre <- reactive({
      req(ic_table_peaks_pre(), ic_si_spectra(), ic_mi_spectra(), ic_table_peaks_type_mod())
      pks <- ic_table_peaks_pre()
      zones <- c(1, 0.95)
      if (is.numeric(input$ic_par_zone3)) {
        zones <- c(zones,input$ic_par_zone3/100)
      }
      if (is.numeric(input$ic_par_zone4)) {
        zones <- c(zones,input$ic_par_zone4/100)
      }
      # For every sample...
      out <- ldply(1:length(ic_mi_peaks()), function(i) {
        x <- ic_mi_peaks()[[i]]
        smM <- mass(ic_mi_spectra()[[i]])
        siM <- intensity(ic_mi_spectra()[[i]])
        smS <- mass(ic_si_spectra()[[i]])
        siS <- intensity(ic_si_spectra()[[i]])
        pks_sam <- pks[pks[,"Sample"]==i,,drop=FALSE]
        dfs <- lapply(pks_sam[,"Peak ID"], function(j) {
          pb <- c(pks_sam[j,"Scan start"], pks_sam[j,"Scan end"])
          return(data.frame("Iso1" = siM[pb[1]:pb[2]], "Iso2" = siS[pb[1]:pb[2]]))
        })
        ptps <- ic_table_peaks_type_mod()[,"Type"]
        isos <- paste(input$ic_par_si_col_name, input$ic_par_mi_col_name, sep="/")
        bl_method <- input$ic_par_baseline_method
        # For every ratio method...
        ldply(c("mean","area","slope"), function(ratio_method) {
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
              out[,paste0("Ratio P", j, " (", ptps[j], ")")] <- iso_ratio(data = dfs[[j]], method = ratio_method, thr = zone)
            }
            return(out)
          })
        })
      })
      sam_col <- grep("sample", colnames(out))
      std_col <- grep("standard", colnames(out))
      dis_col <- grep("discard", colnames(out))
      for (j in 1:length(sam_col)) {
        fac <- 1000*(input$ic_par_coef - 1)
        out[,gsub("Ratio", "Delta", colnames(out)[sam_col[j]])] <- (out[,sam_col[j]]/apply(out[,std_col,drop=FALSE], 1, mean))*fac
      }
      # remove discarded peaks here
      if (length(dis_col)>=1) {
        out <- out[,-dis_col]
      }
      # remove calculations for method=slope where zone<100%
      out <- out[!(out[,"Zone [%]"]<100 & out[,"Ratio method"]=="slope"),]
      # round values
      p_cols <- grep(" P", colnames(out))
      for (cols in p_cols) { out[,cols] <- round(out[,cols], 4) }
      #if (!all(is.finite(unlist(out[,p_cols])))) browser()
      return(out)
    })
    
    # delta calculation in case of at least 3 input files
    ic_table_deltas_pre <- reactive({
      req(ic_table_ratios_pre(), ic_table_peaks_type_mod())
      validate(need(length(ic_si_spectra())>=3, message = "This view is only available if you uploaded at least 3 replicate measurements."))
      df <- ic_table_ratios_pre()
      p_cols <- grep("Delta", colnames(df))
      # for each Peak...
      out <- ldply(p_cols, function(j) {
        ldply(split(df, interaction(df[,"Ratio method"], df[,"Zone [%]"], drop=TRUE)), function(x) {
          tmp <- x[1, c("Ratio method","Zone [%]"), drop=FALSE]
          tmp[,"Mean Delta"] <- mean(x[,j])
          tmp[,"RSD Delta"] <- sd(x[,j])
          tmp[,"Peak"] <- gsub("[^[:digit:]]", "", colnames(x)[j])
          return(tmp)
        }, .id = NULL)
      }, .id = NULL)
      out[,"Mean Delta"] <- round(out[,"Mean Delta"], 4)
      out[,"RSD Delta"] <- round(out[,"RSD Delta"], 4)
      out <- out[order(out[,"Peak"], out[,"Ratio method"], out[,"Zone [%]"]),]
      return(out)
    })
    
    ### observers on input fields ################################################
    # change plot range upon user mouse interaction (click and drag)
    observeEvent(input$ic_specplot_brush, {
      spec_plots_xmin(input$ic_specplot_brush$xmin)
      spec_plots_xmax(input$ic_specplot_brush$xmax)
    })
    
    # change plot range upon user mouse interaction (double click)
    observeEvent(input$ic_specplot_dblclick, {
      req(ic_mi_spectra())
      rng <- range(sapply(ic_mi_spectra(), function(x) { range(mass(x), na.rm=TRUE) }))
      spec_plots_xmin(rng[1])
      spec_plots_xmax(rng[2])
    })
    
    # show fileUpload only when data source is set to 'upload files'
    observeEvent(input$ic_par_libsource, {
      toggle(id = "ic_par_path_expfiles", condition = input$ic_par_libsource=="upload files")
    })
    
    # update column selectors when input columns change  
    observeEvent(file_in_cols, {
      updateSelectInput(inputId = "ic_par_rt_col", choices = I(file_in_cols()), selected = file_in_cols()[3])
      updateSelectInput(inputId = "ic_par_mi_col", choices = I(file_in_cols()), selected = file_in_cols()[7])
      updateSelectInput(inputId = "ic_par_si_col", choices = I(file_in_cols()), selected = file_in_cols()[9])
    })
    
    # check and update time range filters when time column is changed
    observeEvent(input$ic_par_rt_col, {
      req(file_in())
      # reset cut range and...
      rng <- sapply(file_in(), function(x) { range(x[,input$ic_par_rt_col], na.rm=TRUE) })
      cut_range$min <- min(rng[1,])
      cut_range$max <- max(rng[2,])
      status_range_cut("off")
      updateActionButton(inputId = "ic_par_cut_range", label = "cut range")
      # ...reset display range
      spec_plots_xmin(cut_range$min)
      spec_plots_xmax(cut_range$max)
    }, ignoreInit = TRUE)
    
    # update MI/SI name inputs when input columns change
    observeEvent(input$ic_par_mi_col, {
      updateTextInput(inputId = "ic_par_mi_col_name", value = input$ic_par_mi_col)
    })
    
    # update MI/SI name inputs when input columns change
    observeEvent(input$ic_par_si_col, {
      updateTextInput(inputId = "ic_par_si_col_name", value = input$ic_par_si_col)
    })
    
    # set cut range to displayed spectrum range when user triggers this action button
    observeEvent(input$ic_par_cut_range, {
      req(cut_range, input$ic_par_rt_col, spec_plots_xmin(), spec_plots_xmax())
      if (status_range_cut()=="off") {
        cut_range$min <- spec_plots_xmin()
        cut_range$max <- spec_plots_xmax()
        updateActionButton(inputId = "ic_par_cut_range", label = "undo cut")
        status_range_cut("on")
      } else {
        rng <- sapply(file_in(), function(x) { range(x[,input$ic_par_rt_col], na.rm=TRUE) })
        cut_range$min <- min(rng[1,])
        cut_range$max <- max(rng[2,])
        updateActionButton(inputId = "ic_par_cut_range", label = "cut range")
        status_range_cut("off")
      }
    })
    
    # set cut range to displayed spectrum range when user triggers this action button
    observeEvent(input$ic_par_align_rt, {
      if (status_align()=="off") {
        out <- apply(sapply(split(ic_table_peaks_pre(), ic_table_peaks_pre()[,"Peak ID"]), function(x) {
          x[,"MT max"]-median(x[,"MT max"])
        }), 1, median)
        rt_shift(out)
        updateActionButton(inputId = "ic_par_align_rt", label = "undo align")
        status_align("on")
      } else {
        rt_shift(rep(0, length(file_in())))
        updateActionButton(inputId = "ic_par_align_rt", label = "align rt")
        status_align("off")
      }
    })
    
    
    ### outputs ##################################################################
    ## UI output
    # peak-type table with user select inputs
    output$ic_peaks_type_msg <- renderUI({
      ic_n_valid_peaks()
      tagList(
        fluidRow(column(2, strong("ID")), column(10, strong("Type")))
      )
    })
    
    ## tables 
    # table of peaks of 'new sample'
    output$ic_table_peaks <- renderDT({
      ic_table_peaks_pre()
    }, server = FALSE, extensions = "Buttons", options = list("dom"="Blfrtip", "pageLength"=100, buttons = c('copy', 'csv', 'excel', 'pdf')), selection=list(mode="single", target="row"), rownames=NULL)
    
    # ratio(s) table
    output$ic_table_ratios <- renderDT({
      ic_table_ratios_pre()
    }, server = FALSE, extensions = "Buttons", options = list("dom"="Blfrtip", "pageLength"=100, buttons = c('copy', 'csv', 'excel', 'pdf')), selection=list(mode="single", target="row"), rownames=NULL)
    # delta(s) table
    output$ic_table_deltas <- renderDT({
      ic_table_deltas_pre()
    }, server = FALSE, extensions = "Buttons", options = list("dom"="Blfrtip", "pageLength"=100, buttons = c('copy', 'csv', 'excel', 'pdf')), selection=list(mode="single", target="row"), rownames=NULL)  
    
    ## plots
    # ...
    output$ic_specplot <- renderPlot({
      req(file_in(), ic_mi_spectra(), ic_si_spectra())
      xrng <- c(spec_plots_xmin(), spec_plots_xmax())
      yrng <- c(0, max(sapply(c(ic_mi_spectra(), ic_si_spectra()), function(x) {max(x@intensity)})))
      par(mar = c(4.5, 4.5, 0.5, ifelse("overlay_drift" %in% input$ic_par_specplot, 4.5, 0.5)))
      plot(x = xrng, y = yrng, type = "n", xaxs = "i", xlab=paste0("Time [", input$ic_par_mi_rt_unit, "]"), ylab="Intensity [V]")
      if ("overlay_mi" %in% input$ic_par_specplot) {
        idx_all <- 1:length(ic_mi_spectra())
        cols <- 2:(length(idx_all)+1)
      } else {
        idx_all <- as.numeric(gsub("[^[:digit:]]", "", input$ic_par_focus_sample))
        cols <- rep(1, idx_all)
      }
      for (idx in idx_all) {
        if ("overlay_legend" %in% input$ic_par_specplot) {
          #mtext(text = paste0("[",idx,"] ", names(file_in())[idx], "; n_peaks =", length(pm)), side = 3, line = -1.15*idx, adj = 0.02, font = 1, col=cols[idx])
          mtext(text = paste0("[",idx,"] ", names(file_in())[idx]), side = 3, line = -1.15*idx, adj = 0.02, font = 1, col=cols[idx])
        }
        sm <- mass(ic_mi_spectra()[[idx]])
        si <- intensity(ic_mi_spectra()[[idx]])
        flt <- sm>=spec_plots_xmin() & sm<=spec_plots_xmax()
        lines(x = sm[flt], y = si[flt], col=cols[idx])
        if ("overlay_si" %in% input$ic_par_specplot) {
          lines(
            x = mass(ic_si_spectra()[[idx]])[flt],
            y = intensity(ic_si_spectra()[[idx]])[flt],
            col=cols[idx])
        }
        if (!is.null(ic_mi_peaks()) && length(ic_mi_peaks()[[idx]]@mass)>=1) {
          pks <- ic_table_peaks_pre()
          pks_sam <- pks[pks[,"Sample"]==idx,,drop=FALSE]
          # plot symbols at peak apex
          points(x=mass(ic_mi_peaks()[[idx]]), y=intensity(ic_mi_peaks()[[idx]]), col = 1, pch = 21, bg = cols[idx])
          if ("overlay_drift" %in% input$ic_par_specplot) {
            dfs <- lapply(pks_sam[,"Peak ID"], function(j) {
              pb <- c(pks_sam[j,"Scan start"], pks_sam[j,"Scan end"])
              out <- data.frame("RT"=sm[pb[1]:pb[2]], "Iso1" = si[pb[1]:pb[2]], "Iso2" = intensity(ic_si_spectra()[[idx]])[pb[1]:pb[2]])
              out[,"Ratio"] <- out[,3]/out[,2]
              return(out)
            })
            max_I <- max(yrng)
            min_R <- min(sapply(dfs, function(x) { min(x[is.finite(x[,"Ratio"]),"Ratio"])}))
            dfs <- lapply(dfs, function(x) {
              flt <- is.finite(x[,"Ratio"])
              y <- x[flt,"Ratio"]-min_R
              x[flt,"Ratio_norm"] <- max_I*y/max(y)
              return(x)
            })
            #browser()
            for (j in 1:length(dfs)) {
              points(x=dfs[[j]][,"RT"], y=dfs[[j]][,"Ratio_norm"], pch=".", col=cols[idx])
            }
            if (idx==idx_all[1]) {
              at <- axTicks(side = 4)
              axis(side = 4, at=at, labels = round(dfs[[1]][,"Ratio"][sapply(at, function(x) { which.min(abs(dfs[[1]][,"Ratio_norm"]-x)) })], 4))
              mtext(text = paste0(input$ic_par_si_col_name, "/", input$ic_par_mi_col_name), side = 4, adj=0.5, line=3)
            }
          }
          if ("overlay_pb" %in% input$ic_par_specplot) {
            for (j in 1:nrow(pks_sam)) { 
              pb <- c(pks_sam[j,"Scan start"], pks_sam[j,"Scan end"])
              abline(v=sm[pb], col=cols[idx])
              mtext(text = j, side = 1, at = sm[pb[1]], adj = 0, line = -1.1, col=cols[idx])
            }
          }
        }
      }
    })
    
    # ...
    output$ic_deltaplot <- renderPlot({
      req(ic_table_deltas_pre())
      df <- ic_table_deltas_pre()
      df[,"Ratio method"] <- factor(df[,"Ratio method"], levels=c("mean","area","slope"))
      cols <- c(5:7)[as.numeric(df[,"Ratio method"])]
      pchs <- c(21,22,24)[as.numeric(df[,"Ratio method"])]
      x <- df[,"Zone [%]"]
      x <- x + c(-0.25,0,0.25)[as.numeric(df[,"Ratio method"])]
      y <- df[,"Mean Delta"]
      e <- df[,"RSD Delta"]
      par(mar = c(4.5, 4.5, 0.5, 0.5))
      #browser()
      plot(x=range(x), y=range(rep(y,2)+rep(c(-1,1),each=length(y))*2*e), type="n", xlab="Zone [%] (values are slightly shifted to improve visibility)", ylab="Mean Delta")
      legend(x = "top", horiz=TRUE, pch=c(21,22,24), pt.bg=c(5:7), legend=levels(df[,"Ratio method"]))
      segments(x0 = x, y0 = y-2*e, y1 = y+2*e, col = cols)
      segments(x0 = x, y0 = y-2*e, y1 = y+2*e, col = cols)
      points(x = x, y = y, pch = pchs, bg = cols, cex=2)
    })
    
    # ...
    output$ic_deltaplot2 <- renderPlot({
      req(ic_table_deltas_pre())
      df <- ic_table_deltas_pre()
      df[,"Ratio method"] <- factor(df[,"Ratio method"], levels=c("mean","area","slope"))
      cols <- c(5:7)[as.numeric(df[,"Ratio method"])]
      pchs <- c(21,22,24)[as.numeric(df[,"Ratio method"])]
      x <- factor(df[,"Zone [%]"])
      x_ann <- levels(x)
      x <- as.numeric(x) + c(-0.05,0,0.05)[as.numeric(df[,"Ratio method"])]
      y <- df[,"Mean Delta"]
      e <- df[,"RSD Delta"]
      par(mar = c(4.5, 4.5, 0.5, 0.5))
      plot(x=range(x), y=range(rep(y,2)+rep(c(-1,1),each=length(y))*2*e), type="n", xlab="Zone [%] (values are slightly shifted to improve visibility)", ylab="Mean Delta", axes=F)
      axis(2); axis(1, at=1:4, labels = x_ann); box()
      legend(x = "top", horiz=TRUE, pch=c(21,22,24), pt.bg=c(5:7), legend=levels(df[,"Ratio method"]))
      segments(x0 = x, y0 = y-2*e, y1 = y+2*e, col = cols)
      segments(x0 = x, y0 = y-2*e, y1 = y+2*e, col = cols)
      points(x = x, y = y, pch = pchs, bg = cols, cex=2)
    })
    
    # ...
    output$ic_driftplot <- renderPlot({
      req(ic_table_peaks_pre(), ic_si_spectra(), ic_mi_spectra(), ic_table_peaks_type_mod())
      pks <- ic_table_peaks_pre()
      i <- as.numeric(gsub("[^[:digit:]]", "", input$ic_par_focus_sample))
      x <- ic_mi_peaks()[[i]]
      smM <- mass(ic_mi_spectra()[[i]])
      siM <- intensity(ic_mi_spectra()[[i]])
      smS <- mass(ic_si_spectra()[[i]])
      siS <- intensity(ic_si_spectra()[[i]])
      pks_sam <- pks[pks[,"Sample"]==i,,drop=FALSE]
      dfs <- lapply(pks_sam[,"Peak ID"], function(j) {
        pb <- c(pks_sam[j,"Scan start"], pks_sam[j,"Scan end"])
        return(data.frame("RT"=smM[pb[1]:pb[2]], "Iso1" = siM[pb[1]:pb[2]], "Iso2" = siS[pb[1]:pb[2]]))
      })
      ptps <- ic_table_peaks_type_mod()[,"Type"]
      isos <- paste(input$ic_par_si_col_name, input$ic_par_mi_col_name, sep="/")
      bl_method <- input$ic_par_baseline_method
      par(mfrow=c(1,length(ptps)))  
      for (j in 1:length(ptps)) {
        plot(x=dfs[[j]][,1], y=dfs[[j]][,2], type="l", xlab="RT", ylab="Intensity (V)", main=ptps[j])
        y2 <- dfs[[j]][,3]/dfs[[j]][,2]
        flt <- is.finite(y2)
        y2_norm <- max(dfs[[j]][,2])*(y2-min(y2[flt]))/(max(y2[flt])-min(y2[flt]))
        points(x=dfs[[j]][,1], y=y2_norm, col=grey(0.9))
        #browser()
        at <- axTicks(side = 4)
        axis(side = 4, at=at, labels = round(y2[sapply(at, function(x) { which.min(abs(y2_norm-x)) })], 4))
      }
    })
  
  }
  
  # Run the application
  shinyApp(ui = ui, server = server)
  
}