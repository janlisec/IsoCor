#' @title ic_app.
#'
#' @description \code{ic_app} will start a shiny app that allows to upload raw
#'  data, process selectively and analyze different methods of ratio calculation
#'  between two intensity traces.
#'
#' @param ... Options passed to golem::with_golem_options.
#' 
#' @details The app is described in detail in \doi{10.1039/D2JA00208F}.
#'
#' @references \url{https://jali.shinyapps.io/IsoCor}
#'
#' @return A shiny app object. This will effectively launch a browser and start the app on local port 7462. 
#'
#' @seealso \link{iso_ratio}
#'
#' @import shiny
#' @importFrom DT DTOutput renderDT JS
#' @importFrom graphics abline axis box legend lines mtext par points segments
#' @importFrom grDevices grey pdf dev.off
#' @importFrom MALDIquant transformIntensity smoothIntensity removeBaseline detectPeaks createMassSpectrum mass intensity
#' @importFrom plyr ldply
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs useShinyjs hide show enable disable toggle
#' @importFrom stats median rnorm sd quantile
#' @importFrom utils data packageDate packageVersion
#'
#' @export
ic_app <- function(...) {
  # these options to shinyApp() could be made available to ic_app() in the future if required
  onStart = NULL
  options = list("port" = 7462,  "display.mode" = "normal")
  enableBookmarking = "disable"
  uiPattern = "/"
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}

# ================================
# set up app UI ----
app_ui <- function() {
  
  # components
  main_menu_ui <- shiny::tagList(
    shiny::div(
      bslib::card(
        bslib::card_header(
          shiny::actionLink(inputId = "ic_help02", label = "Data"),
        ),
        bslib::layout_column_wrap(width = 120,
          radioButtons(inputId = "ic_par_libsource", label = "Data source", choices = c("Testdata", "Upload files"), selected = "Testdata"),
          radioButtons(inputId = "ic_par_app_method", label = "Method", choices = c("IR-Delta", "IDMS"), selected = "IDMS"),
          selectInput(inputId = "ic_par_inputformat", label = "File format", choices = list("exp", "icp", "data", "generic"), selected = "exp")
        ),
        uiOutput(outputId = "ic_par_path_expfiles")
      ),
      bslib::card(
        bslib::card_header(shiny::actionLink(inputId = "ic_help03", label = "Import")),
        bslib::layout_column_wrap(width = 120,
          selectInput(inputId = "ic_par_rt_col", label = "RT column", choices = c("")) |> bslib::tooltip("Select RT column."),
          textInput(inputId = "ic_par_mi_rt_unit", label = "RT unit", value = "min"),
          shiny::HTML(""),
          selectInput(inputId = "ic_par_mi_col", label = "MI column", choices = c("")) |> bslib::tooltip("Select Master Isotope column."),
          textInput(inputId = "ic_par_mi_col_name", label = "MI Name"),
          numericInput(inputId = "ic_par_mi_amu", label = "MI amu", value = 0, step = 0.0001),
          selectInput(inputId = "ic_par_si_col", label = "SI column", choices = c("")) |> bslib::tooltip("Select Secondary Isotope column."),
          textInput(inputId = "ic_par_si_col_name", label = "SI Name"),
          numericInput(inputId = "ic_par_si_amu", label = "SI amu", value = 0, step = 0.0001)
        )
      ),
      bslib::card(
        id = "IDMS_par_section",
        bslib::card_header(shiny::actionLink(inputId = "ic_help10", label = "IDMS Parameters")),
        bslib::layout_column_wrap(width = 120,
          numericInput(inputId = "ic_par_IDMS_f", label = "IDMS f-value", value = 0.8876311),
          selectInput(inputId = "ic_par_IDMS_mb_method", label = "Mass bias", choices = c("none","Linear","Russel","Exponential"), selected = "Russel"),
          numericInput(inputId = "ic_par_IDMS_halfWindowSize", label = "Smoothing", value = 100, min=0, max=100, step=1),
        #shiny::h6("Sample related Parameters"),
          numericInput(inputId = "ic_par_IR_sample", label = "Abund. SI", value = 0.0425),
          numericInput(inputId = "ic_par_Abund_MI", label = "Abund. MI", value = 0.9499),
          numericInput(inputId = "ic_par_Inj_Amount", label = "Inj. amount", value = 0.0205),
        #shiny::h6("Spike related Parameters"),
          numericInput(inputId = "ic_par_IR_spike", label = "Abund. MI", value = 0.002),
          numericInput(inputId = "ic_par_Abund_SI", label = "Abund. SI", value = 0.998),
          numericInput(inputId = "ic_par_MF_Spike", label = "MF", value = 4.78881)
        )
      ),
      bslib::card(
        id = "Processing_par_section",
        bslib::card_header(shiny::actionLink(inputId = "ic_help04", label = "Processing")),
        bslib::layout_column_wrap(width = 120,
          numericInput(inputId = "ic_par_halfWindowSize", label = "Smoothing", value = 25, min=0, max=100, step=1) |> bslib::tooltip("Smoothing parameter: 'half window size' of peak. Set to '0' to omit this processing step."),
          selectInput(inputId = "ic_par_baseline_method", label = "BL Correction", choices = c("none", "SNIP", "TopHat", "ConvexHull", "median"), selected = "SNIP") |> bslib::tooltip("Select method for baseline estimation or 'none' to omit this processing step."),
          shiny::HTML(""),
          numericInput(inputId = "ic_par_peakpicking_SNR", label = "Peak (SNR)", value = 25, min=1, max=100, step=1) |> bslib::tooltip("Peak picking parameter: 'Signal/Noise ratio' [range: 1..100]."),
          numericInput(inputId = "ic_par_peakpicking_k", label = "Peak (k)", value = 3, min=3, max=7, step=1) |> bslib::tooltip("Peak picking parameter: 'Peak border min count' [range: 3..7]."),
          checkboxInput(inputId = "ic_par_peakpicking_noise", label = "Peak (noise)", value = TRUE) |> bslib::tooltip("Peak picking parameter: 'use noise cutoff' [TRUE/FALSE].")
        )
      )
    ),
    bslib::card_footer(class = "d-flex justify-content-bottom", app_status_line())
  )
  
  ic_plot_card <- bslib::card(
    id = "ic_plot_card",
    min_height = "450px",
    bslib::card_body(padding = 0, style = "resize: vertical;",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          position = "right", open = "open", width = "280px", gap = "10px",
          selectInput(inputId = "ic_par_focus_sample", label = "Focus sample", choices = list("Sample 1"=1)),
          checkboxGroupInput(
            inputId = "ic_par_specplot", 
            label = shiny::actionLink(inputId = "ic_help05", label = "Plot options"),
            choices = list(
              "show all samples" = "overlay_mi",
              "show peak boundaries"="overlay_pb",
              "show sample IDs" = "overlay_legend",
              "overlay SI trace" = "overlay_si",
              "overlay ratio points" = "overlay_drift",
              "correct ratio points" = "correct_drift"
            ), 
            selected = c("overlay_pb", "overlay_mi", "overlay_drift")
          ),
          actionButton(inputId = "ic_par_cut_range", label = "cut range") |> bslib::tooltip("Cut samples to currently visible range."),
          actionButton(inputId = "ic_par_align_rt", label = "align rt") |> bslib::tooltip("Align samples at peak maxima."),
          actionButton(inputId = "ic_par_set_drift", label = "filter points") |> bslib::tooltip("Set upper and lower quantile to filter depicted ratio points.")
        ),
        plotOutput(
          outputId = "ic_specplot",
          dblclick = dblclickOpts(id = "ic_specplot_dblclick"), 
          brush = brushOpts(id = "ic_specplot_brush", direction = "x", resetOnNew = TRUE)
        ) |> bslib::tooltip("You may select a time range [Click and Drag] with the cursor to zoom. Use [Double Click] to unzoom.", placement = "bottom")
      )
    )
  )
  
  ic_tables_card <- bslib::card(
    id = "ic_tables_card",
    bslib::card_body(
      tabsetPanel(
        id="ic_tabPanel_tables",
        tabPanel(
          title = "Peak table", 
          DT::DTOutput("ic_table_peaks")
        ),
        tabPanel(
          title = "Ratio table",
          DT::DTOutput("ic_table_ratios")
        ),
        tabPanel(
          title = "Delta table",
          bslib::layout_column_wrap(
            DT::DTOutput("ic_table_deltas"),
            plotOutput(outputId = "ic_deltaplot2")
          )
        )
      )
    )
  )
  
  shiny::tagList(
    golem_add_external_resources(),
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        position = "left", open = "open", width = "520px",
        shiny::div(
          class = "d-flex justify-content-between flex-column",
          main_menu_ui
        )
      ),
      ic_plot_card,
      ic_tables_card,
      title = bslib::card_title(
        style = "width: 100%;",
        shiny::div(
          class = "d-flex justify-content-between",
          shiny::div(
            img(src = "www/bam_logo_20pt.gif", alt="BAM Logo"),
            strong("BAM"), em("IsoCor"),
          ),
          shiny::actionLink(inputId = "ic_help01", label = NULL, icon = shiny::icon(name = "question"))
        )
      )
    )
  )
  
}
# ================================

# ================================
# Define server logic ----
app_server <- function(input, output, session) {

  ### setup Options ######################################################----
  # increase maximum file size for upload
  old_options <- options()
  on.exit(options(old_options))
  options(shiny.maxRequestSize=30*1024^2) # BrukerFlex Files are >5MB
  
  # store par() results
  # This is of course not useful in a shiny app, but was required from CRAN which
  # in turn led to problems on ShinyServer as par() opens the standard graphics device
  # which made the hack of pdf(NULL) neccessary... :(
  if (!get_golem_config("bam_server")) {
    grDevices::pdf(NULL)
    old_par <- par(no.readonly = TRUE)
    grDevices::dev.off()
    on.exit(expr = { par(old_par) }, add = TRUE)
  }
  
  # load app data on app start
  testdata <- IsoCor::testdata
  testdata_IDMS <- IsoCor::testdata_IDMS
  isotopes <- IsoCor::isotopes
  
  output$ic_par_path_expfiles <- renderUI({
    # file input as renderUI to allow a reset in case that the upload method is changed
    message("output$ic_par_path_expfiles")
    fileInput(inputId = "ic_par_path_expfiles_inner", label = "Select Files", multiple = TRUE)
  })
  
  ### setup reactive Values ##############################################----
  # the editable peak table
  ic_table_peaks_edit <- shiny::reactiveVal()
  # setup initial plot range (min, max)
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
  # initial mass bias method
  current_mb_method <- reactiveVal("none")
  # return current screen height to adjust table height
  screen_height <- reactiveVal(960)
  
  ### show/hide section ##################################################----
  # modify UI depending on workflow (IR-Delta or IDMS)
  observeEvent(input$ic_par_app_method, {
    shinyjs::toggleElement(id = "IDMS_par_section", condition = input$ic_par_app_method=="IDMS")
    shinyjs::toggleElement(id = "Processing_par_section", condition = input$ic_par_app_method!="IDMS")
    if (input$ic_par_app_method=="IDMS") {
      updateCheckboxGroupInput(inputId = "ic_par_specplot", selected = c("overlay_pb", "overlay_mi"))
      disable(selector = "#ic_par_specplot input[value='overlay_si']")
      disable(selector = "#ic_par_specplot input[value='overlay_drift']")
      disable(selector = "#ic_par_specplot input[value='correct_drift']")
      shiny::hideTab(inputId = "ic_tabPanel_tables", target = "Ratio table")
      shiny::hideTab(inputId = "ic_tabPanel_tables", target = "Delta table")
    } else {
      enable(selector = "#ic_par_specplot input[value='overlay_si']")
      enable(selector = "#ic_par_specplot input[value='overlay_drift']")
      enable(selector = "#ic_par_specplot input[value='correct_drift']")
      shiny::showTab(inputId = "ic_tabPanel_tables", target = "Ratio table")
      shiny::showTab(inputId = "ic_tabPanel_tables", target = "Delta table")
    }
  })
  
  ### reactives ########################################################### ----
  # get input data as list of tables
  file_in <- reactive({
    req(input$ic_par_libsource)
    message("file_in")
    out <- NULL
    if (input$ic_par_libsource=="Upload files") {
      if (!is.null(input$ic_par_path_expfiles_inner)) {
        out <- try(lapply(input$ic_par_path_expfiles_inner$datapath, function(x) {
          read_raw_data(path=x, format=input$ic_par_inputformat)
        }))
        if (inherits(x = out, what = "try-error")) {
          out <- NULL
        } else {
          names(out) <- input$ic_par_path_expfiles_inner$name
        }
      } else {
        out <- NULL
      }
    } else {
      if (input$ic_par_libsource=="Testdata") {
        if (input$ic_par_app_method == "IR-Delta") {
          updateSelectInput(inputId = "ic_par_inputformat", selected="exp")
          out <- testdata
        }
        if (input$ic_par_app_method == "IDMS") {
          updateSelectInput(inputId = "ic_par_inputformat", selected="generic")
          out <- testdata_IDMS
        }
      }
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
  
  # register the file_in reactive for app testing
  shiny::exportTestValues(
    file_in = file_in
  )
  
  observeEvent(input$ic_par_specplot, {
    toggle(id = "ic_par_focus_sample", condition = !("overlay_mi" %in% input$ic_par_specplot))
  }, ignoreNULL = FALSE)
  
  # check table headers for consistency and to get colnames to allow user column selection
  file_in_cols <- reactive({
    # [JL] we need input$ic_par_inputformat here to ensure to trigger updates below in observeEvent(file_in_cols())
    req(file_in(), input$ic_par_inputformat)
    #req(file_in())
    headers <- sapply(lapply(file_in(), colnames), paste, collapse="")
    validate(need(length(unique(headers))==1, message = "Files contain different headers"))
    message("[file_in_cols] set for input format ", input$ic_par_inputformat)
    return(colnames(file_in()[[1]]))
  })
  
  # IDMS reactive objects ----
  IDMS_data <- reactive({
    req(file_in(), input$ic_par_IDMS_f, input$ic_par_IR_sample, input$ic_par_Abund_MI, input$ic_par_Inj_Amount, input$ic_par_IR_spike, input$ic_par_Abund_SI, input$ic_par_MF_Spike, input$ic_par_mi_amu, input$ic_par_si_amu, current_mb_method())
    validate(need(input$ic_par_app_method=="IDMS", "Method IDMS not selected"))
    validate(need(all(sapply(file_in(), function(x) { all(c(input$ic_par_mi_col, input$ic_par_si_col) %in% colnames(x)) })), "Selected columns not found in input data"))
    validate(need(all(!duplicated(c(input$ic_par_mi_col, input$ic_par_si_col))), "Identical columns selected for SI and MI"))
    validate(need(!identical(input$ic_par_mi_amu, input$ic_par_si_amu), "Identical amu specified for SI and MI"))
    # R_observe/R_true
    #f_value <- log(x = input$ic_par_IDMS_f, base = input$ic_par_mi_amu/input$ic_par_si_amu)
    k <- IsoCor::mass_bias(
      mi_amu = input$ic_par_mi_amu, 
      si_amu = input$ic_par_si_amu, 
      method = current_mb_method(), 
      #f_value = f_value
      f_value = input$ic_par_IDMS_f
    )
    coef <- input$ic_par_MF_Spike * (input$ic_par_mi_amu / input$ic_par_si_amu) * (input$ic_par_Abund_SI / input$ic_par_Abund_MI)
    validate(need(is.finite(coef), "Can not compute valid coef with these parameters. Check 'MI amu' and 'SI amu'"))
    message("IDMS_data")
    idms <- lapply(file_in(), function(x) { 
      x$IR<- x[,input$ic_par_mi_col]/x[,input$ic_par_si_col]
      x$IR_cor <- x$IR * k
      # previous version of Dariya (from 03/2023)
      #x$MF <- coef * (x$IR_cor - input$ic_par_IR_spike) / (1 - x$IR_cor * input$ic_par_IR_sample)
      # Dariya's version from 06/2023
      x$MF <- input$ic_par_MF_Spike * (input$ic_par_IR_spike - x$IR_cor * input$ic_par_Abund_SI) / (input$ic_par_IR_sample * x$IR_cor - input$ic_par_Abund_MI)
      return(x)
    })
    return(idms)
  })
  
  
  # convert input tables into MALDIquant spectra format for selected MI trace and RT column ----
  ic_mi_spectra_raw <- reactive({
    req(file_in(), input$ic_par_rt_col, input$ic_par_mi_col, cut_range$min, cut_range$max, rt_shift(), input$ic_par_app_method)
    if (input$ic_par_app_method=="IDMS") req(IDMS_data())
    message("ic_mi_spectra_raw")
    get_spectrum(
      data = if (input$ic_par_app_method=="IDMS") IDMS_data() else file_in(), 
      rt_col = input$ic_par_rt_col, 
      int_col = ifelse(input$ic_par_app_method=="IDMS", "MF", input$ic_par_mi_col), 
      cut_range = shiny::reactiveValuesToList(cut_range), 
      rt_shift = rt_shift()
    )
  })
  
  # convert input tables into MALDIquant spectra format for selected SI trace and RT column ----
  ic_si_spectra_raw <- reactive({
    req(file_in(), input$ic_par_rt_col, input$ic_par_si_col, cut_range$min, rt_shift())
    req(input$ic_par_app_method=="IR-Delta")
    message("ic_si_spectra_raw")
    get_spectrum(
      data = file_in(), 
      rt_col = input$ic_par_rt_col, 
      int_col = input$ic_par_si_col, 
      cut_range = shiny::reactiveValuesToList(cut_range), 
      rt_shift = rt_shift()
    )
  })
  
  # provide spectra based on processed raw data ----
  ic_mi_spectra <- reactive({
    req(ic_mi_spectra_raw(), input$ic_par_halfWindowSize, input$ic_par_baseline_method, input$ic_par_peakpicking_SNR, input$ic_par_app_method)
    message("ic_mi_spectra")
    # wrap processing in try to account for extreme parameter selections
    out <- try(spec_pre_process(data = ic_mi_spectra_raw(), hWS = isolate(input$ic_par_halfWindowSize), BLmethod = input$ic_par_baseline_method, wf = input$ic_par_app_method))
    validate(need(!inherits(out, "try-error"), "Could not preprocess ic_mi_spectra_raw()"))
    return(out)
  })

  # provide spectra based on processed raw data ----
  ic_si_spectra <- reactive({
    req(ic_si_spectra_raw(), input$ic_par_baseline_method)
    message("ic_si_spectra")
    spec_pre_process(data = ic_si_spectra_raw(), hWS = isolate(input$ic_par_halfWindowSize), BLmethod = input$ic_par_baseline_method, wf = input$ic_par_app_method)
  })
  
  # identify peaks in processed mi spectra ----
  ic_mi_peaks <- reactive({
    req(ic_mi_spectra())
    message("ic_mi_peaks")
    # disable button here, will be enabled potentially if consistent number of peaks is found
    disable(id = "ic_par_align_rt")
    # wrap peak detection in try to account for extreme parameter selections
    pks <- try(lapply(ic_mi_spectra(), function(x) {
      get_peaks(x=x, hWS = isolate(input$ic_par_halfWindowSize), SNR = isolate(input$ic_par_peakpicking_SNR), wf = input$ic_par_app_method, hWS_IDMS = input$ic_par_IDMS_halfWindowSize, set_noise = input$ic_par_peakpicking_noise, k = input$ic_par_peakpicking_k)
    }), silent = TRUE)
    validate(need(!(inherits(pks, "try-error")), "Can't obtain peaks from MI spectra"))
    return(pks)
  })
  
  # IDMS table ----
  ic_table_idms_pre <- reactive({
    req(ic_mi_spectra(), ic_mi_peaks(), current_mb_method())
    message("ic_table_idms_pre")
    spc <- ic_mi_spectra()
    pks <- ic_mi_peaks()
    out <- prep_tab_peaks(p = pks, s = spc, mb = current_mb_method())
    #f_value <- log(x = input$ic_par_IDMS_f, base = input$ic_par_mi_amu/input$ic_par_si_amu)
    f_value <- input$ic_par_IDMS_f
    validate(need(is.finite(f_value), "Can't calculate a finite f_value for IDMS peaks"))
    out[,"f_value"] <- f_value
    out[,"k"] <- IsoCor::mass_bias(mi_amu = input$ic_par_mi_amu, si_amu = input$ic_par_si_amu, method = current_mb_method(), f_value = f_value)
    idms <- lapply(1:length(spc), function(i) {
      data.frame("MF_dt" = spc[[i]]@intensity * c(diff(spc[[i]]@mass), 0))
    })
    out$Abs_q <- sapply(1:nrow(out), function(i) {
      sum(idms[[out[i,"Sample"]]][out[i,"Scan start"]:out[i,"Scan end"],"MF_dt"], na.rm=TRUE)
    })
    out$Conc <- out$Abs_q / input$ic_par_Inj_Amount
    out$f_value <- round(out$f_value, 6)
    out$Abs_q <- round(out$Abs_q, 3)
    out$Conc <- round(out$Conc, 3)
    return(out)
  })
  
  # mi peak table ----
  ic_table_peaks_pre <- reactive({
    if (input$ic_par_app_method=="IDMS") req(ic_table_idms_pre()) else req(ic_mi_peaks())
    message("ic_table_peaks_pre")
    if (input$ic_par_app_method=="IDMS") {
      out <- ic_table_idms_pre()
    } else {
      out <- prep_tab_peaks(p = ic_mi_peaks(), s = ic_mi_spectra(), mb = isolate(current_mb_method()))
    }
    # enable ic_par_align_rt only if consistent number of peaks are found in all samples and more than 2 samples are available
    if (length(ic_mi_peaks())>=2 & length(unique(table(out[,"Peak ID"])))==1) { enable(id = "ic_par_align_rt") }
    return(out)
  })
  
  # mi/si ratio calculation ----
  ic_table_ratios_pre <- reactive({
    req(ic_table_peaks_edit(), ic_si_spectra(), ic_mi_spectra(), zones())
    message("ic_table_ratios_pre")
    prep_tab_ratios(
      pks = ic_table_peaks_edit(), 
      mi_pks = ic_mi_peaks(), 
      mi_spc = ic_mi_spectra(), 
      si_spc = ic_si_spectra(), 
      isos = paste(input$ic_par_si_col_name, input$ic_par_mi_col_name, sep="/"),
      bl_method = input$ic_par_baseline_method,
      zones = zones(),
      current_coef = current_coef()
    )
  })
  
  # IDMS observer
  observeEvent(input$ic_par_IDMS_mb_method, {
    current_mb_method(input$ic_par_IDMS_mb_method)
  }, ignoreInit = FALSE) 
  
  # add or remove zone levels ----
  observeEvent(input$ic_btn_add_zone, {
    shinyalert::shinyalert(
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
    shinyalert::shinyalert(
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
    shinyalert::shinyalert(
      html = TRUE,
      text = tagList(
        numericInput(inputId = "ic_par_coef", label = "Add 'coef' to normalize delta", value = current_coef(), min=0.9, max=1.1, step=0.0001) |> bslib::tooltip("Define coef parameter for delta calculation.")
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
    df <- ic_table_ratios_pre()
    validate(need(length(unique(df[,"Sample"]))>=3, message = "This view is only available if you uploaded at least 3 replicate measurements."))
    validate(need(any(grep("Delta", colnames(df))), "The ratio table does not contain a column of Delta values to be evaluated."))
    prep_tab_deltas(df = df, prec = 3)
  })
  
  # table of peaks of 'new sample' ----
  shiny::observeEvent(ic_table_peaks_pre(), {
    tmp <- ic_table_peaks_pre()
    if (input$ic_par_app_method!="IDMS" && nrow(tmp)>=1 && all(table(tmp[,"Peak ID"])==max(tmp[,"Sample"]))) {
      np <- max(tmp[,"Peak ID"])
      if (length(np)==1 && np>=2) {
        type <- c("standard", rep("sample", np-2), "standard")
        if (length(type)==2) type[2] <- "sample"
        tmp[,"Type"] <- sapply(tmp[,"Peak ID"], function(i) {type[i]})
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
    rng <- range(sapply(ic_mi_spectra(), function(x) { range(MALDIquant::mass(x), na.rm=TRUE) }))
    spec_plots_xmin(rng[1])
    spec_plots_xmax(rng[2])
  })
  
  # show fileUpload only when data source is set to 'upload files' ----
  observeEvent(input$ic_par_libsource, {
    toggle(id = "ic_par_path_expfiles", condition = input$ic_par_libsource=="Upload files")
    toggle(id = "ic_par_inputformat", condition = input$ic_par_libsource=="Upload files")
  })
  
  # reset time windows (upon new data or new RT column)
  reset_times <- function() {
    req(file_in(), file_in_cols(), input$ic_par_rt_col)
    if (input$ic_par_rt_col %in% file_in_cols()) {
      # reset range cut
      rng <- sapply(file_in(), function(x) { range(x[,input$ic_par_rt_col], na.rm=TRUE) })
      cut_range$min <- min(rng[1,])
      cut_range$max <- max(rng[2,])
      status_range_cut("off")
      updateActionButton(inputId = "ic_par_cut_range", label = "cut range")
      # reset alignment
      rt_shift(rep(0, length(file_in())))
      status_align("off")
      updateActionButton(inputId = "ic_par_align_rt", label = "align rt")
      # ...reset display range
      spec_plots_xmin(cut_range$min)
      spec_plots_xmax(cut_range$max)
    }
  }
  
  # update column selectors when input columns change  
  observeEvent(file_in_cols(), {
    fic <- file_in_cols()
    n <- length(fic)
    mi_selected <- switch(
      input$ic_par_inputformat, 
      "exp"=fic[min(c(7,n))], 
      "icp"=fic[min(c(2,n))],
      "generic"=fic[min(c(2,n))],
      "data"=fic[min(c(2,n))])
    si_selected <- switch(
      input$ic_par_inputformat, 
      "exp"=fic[min(c(9,n))], 
      "icp"=fic[min(c(4,n))],
      "generic"=fic[min(c(3,n))],
      "data"=fic[min(c(3,n))])
    rt_selected <- ifelse("Minutes" %in% fic, "Minutes", fic[1])
    updateSelectInput(inputId = "ic_par_rt_col", choices = I(fic), selected = rt_selected)
    updateSelectInput(inputId = "ic_par_mi_col", choices = I(fic), selected = mi_selected)
    updateSelectInput(inputId = "ic_par_si_col", choices = I(fic), selected = si_selected)
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
      status_range_cut("on")
    } else {
      rng <- sapply(file_in(), function(x) { range(x[,input$ic_par_rt_col], na.rm=TRUE) })
      cut_range$min <- min(rng[1,])
      cut_range$max <- max(rng[2,])
      updateActionButton(inputId = "ic_par_cut_range", label = "cut range")
      status_range_cut("off")
    }
  })
  observeEvent(status_range_cut(), {
    btn_col <- if (status_range_cut()=="on") {
      shinyjs::runjs('document.getElementById("ic_par_cut_range").style.backgroundColor = "#FFA500";')
    } else {
      shinyjs::runjs('document.getElementById("ic_par_cut_range").style.backgroundColor = "#FFFFFF";')
    }
  })
  observeEvent(status_align(), {
    btn_col <- if (status_align()=="on") {
      shinyjs::runjs('document.getElementById("ic_par_align_rt").style.backgroundColor = "#FFA500";')
    } else {
      shinyjs::runjs('document.getElementById("ic_par_align_rt").style.backgroundColor = "#FFFFFF";')
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
      status_align("on")
    } else {
      rt_shift(rep(0, length(file_in())))
      updateActionButton(inputId = "ic_par_align_rt", label = "align rt")
      status_align("off")
    }
  })
  
  # open a modal to allow the user to specify quantiles for drift filtering
  observeEvent(input$ic_par_set_drift, {
    shinyalert::shinyalert(
      html = TRUE,
      text = tagList(
        fluidRow(
          div(style = "margin-bottom: 5px", fluidRow(column(12, strong("Points filter (lower/upper quantile)")))),
          fluidRow(
            column(width = 6, align = "center", numericInput(inputId = "ic_par_quant_low", label = NULL, value = current_drift_filter()[1], min=0, max=0.25, step=0.01, width="90%") |> bslib::tooltip("Define lower quantile parameter to limit depicted drift value distribution.")),
            column(width = 6, align = "center", numericInput(inputId = "ic_par_quant_high", label = NULL, value = current_drift_filter()[2], min=0.75, max=1, step=0.01, width="90%") |> bslib::tooltip("Define upper quantile parameter to limit depicted drift value distribution."))
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
  output$ic_table_peaks <- DT::renderDT({
    req(ic_table_peaks_edit())
    message("output$ic_table_peaks")
    style_tab_peaks(data = ic_table_peaks_edit(), IDMS = input$ic_par_app_method=="IDMS", sh = screen_height())
  })
  
  # apply mass bias correction using table action button
  shiny::observeEvent(input$ic_btn_mass_bias, {
    shinyalert::shinyalert(
      html = TRUE,
      text = tagList(
        fluidRow(
          column(width = 6, selectInput(inputId = "ic_par_mb_method", label = "Mass bias method", choices = c("none","Linear","Russel","Exponential"), selected = current_mb_method()) |> bslib::tooltip("Select mass bias method.")),
          column(
            width = 6, 
            shiny::textAreaInput(
              inputId = "txt_textAreaInput",
              label = "Copy/Paste or Enter",
              placeholder = paste("You can copy/paste a numeric Excel range of exactly 1 columns and", nrow(ic_table_peaks_edit()), "rows to fill column 'f_value' automatically. Alternatively you can enter values manually. Entering only a single value will lead to applying this value to all rows."),
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
          if (prod(dim(tmp))==nrow(tab) && all(is.finite(tmp))) {
            tab[, "f_value"] <- as.vector(unlist(tmp))
          } else {
            if (prod(dim(tmp))==1 && all(is.finite(tmp)) & is.matrix(tmp)) {
              tab[, "f_value"] <- rep(tmp[1,1], nrow(tab))
            } else {
              tab[, "f_value"] <- rep(1, nrow(tab))
            }
          }
          tab[,"Mass bias method"] <- rep(input$ic_par_mb_method, nrow(tab))
          current_mb_method(input$ic_par_mb_method)
          updateNumericInput(inputId = "ic_par_IDMS_f", value = tab[1, "f_value"])
          ic_table_peaks_edit(tab)
        }
      }
    )
  })
  
  # opens a modal upon button click to enable the user to change the peak type
  shiny::observeEvent(input$ic_btn_peak_type, {
    if (is.null(input$ic_table_peaks_rows_selected)) {
      shinyalert::shinyalert(text = "Please select a table row first", type = "info")
    } else {
      if (!("Type" %in% colnames(ic_table_peaks_edit()))) {
        shinyalert::shinyalert(text = "Peak types can only be assigned once a similar number of peaks are found in all sampels", type = "info")
      } else {
        i <- input$ic_table_peaks_rows_selected
        shinyalert::shinyalert(
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
  
  # enables manual editing of the f_value column in the peak table
  shiny::observeEvent(input$ic_table_peaks_cell_edit, {
    # convert column values to numeric
    x <- as.numeric(gsub("[^[[:digit:]-].]", "", input$ic_table_peaks_cell_edit$value))
    # replace in correct column and update 'ic_table_peaks_edit'
    tmp <- ic_table_peaks_edit()
    tmp[, input$ic_table_peaks_cell_edit$col[1] + 1] <- x
    ic_table_peaks_edit(tmp)
  })
  
  # update function for automatic calculation of k and several observers
  # [ToDo] Check if the observers should be combined in one observe statement
  update_k <- function() {
    #message("update k in peak table")
    shiny::isolate({
      tmp <- ic_table_peaks_edit()
      if (nrow(tmp)>=1) { 
        if (current_mb_method()=="none") {
          tmp[,"k"] <- rep(0, nrow(tmp))
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
    # [ToDo] maybe switch off for IDMS
    #input$ic_par_app_method=="IDMS"
    req(input$ic_par_app_method!="IDMS")
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
  
  # collapse the options side bar to make space for figure and tables output
  # observeEvent(input$sidebar_button, {
  #   shinyjs::toggle(id = "options_panel")
  #   shinyjs::toggleClass("main_panel", "col-sm-9")
  #   shinyjs::toggleClass("main_panel", "col-sm-12")
  #   if (input$sidebar_button%%2 == 1) {
  #     shiny::updateActionLink(inputId = "sidebar_button", icon = shiny::icon("angle-right"))
  #   } else {
  #     shiny::updateActionLink(inputId = "sidebar_button", icon = shiny::icon("angle-left"))
  #   }
  # })
  
  # adjust UI to current device height in pixel
  observe({
    invalidateLater(3000)
    if (!identical(input$CurrentScreenHeight, screen_height())) {
      screen_height(input$CurrentScreenHeight)
    }
  })
  
  # ratio(s) table ----
  output$ic_table_ratios <- DT::renderDT({
    message("output$ic_table_ratios")
    dt <- DT::datatable(
      data = ic_table_ratios_pre(),
      "extensions" = "Buttons", 
      "options" = list(
        "server" = FALSE, 
        "dom"="Bft", 
        "autoWidth" = TRUE,
        "paging" = FALSE,
        #"scrollY" = screen_height()-570,
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
            extend = "pdf",
            text = 'add new zone',
            action = DT::JS(
              "function ( e, dt, node, config ) {
              Shiny.setInputValue('ic_btn_add_zone', 1, {priority: 'event'});
              }")
          ),
          list(
            extend = "pdf",
            text = 'rem selected zone',
            action = DT::JS(
              "function ( e, dt, node, config ) {
              Shiny.setInputValue('ic_btn_rem_zone', 1, {priority: 'event'});
              }")
          ),
          list(
            extend = "pdf",
            text = 'set coef',
            action = DT::JS(
              "function ( e, dt, node, config ) {
              Shiny.setInputValue('ic_btn_set_coef', 1, {priority: 'event'});
              }")
          ),
          list(
            extend = "pdf",
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
    dt <- DT::formatCurrency(table = dt, columns = grep("Delta P", colnames(ic_table_ratios_pre())), digits = 3, currency="")
    dt <- DT::formatCurrency(table = dt, columns = grep("Ratio P", colnames(ic_table_ratios_pre())), digits = 6, currency="")
    return(dt)
  })
  
  # delta table ----
  output$ic_table_deltas <- DT::renderDT({
    message("output$ic_table_deltas")
    dt <- DT::datatable(
      data = ic_table_deltas_pre(),
      "extensions" = "Buttons", 
      "options" = list(
        "server" = FALSE, 
        "dom"="Bft", 
        "autoWidth" = TRUE,
        "paging" = FALSE,
        #"scrollY" = screen_height()-570,
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
            extend = 'pdf',
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
  output$ic_specplot <- shiny::renderPlot({
    req(ic_mi_spectra(), input$ic_par_si_col_name, input$ic_par_mi_col_name, ic_table_peaks_edit(), input$ic_par_app_method)
    message("output$ic_specplot")
    #browser()
    chk <- input$ic_par_app_method=="IDMS"
    if (!chk) req(ic_si_spectra())
    opt <- input$ic_par_specplot
    if (chk) {
      opt[!(opt %in% c("overlay_si"))]
      si_spec <- NULL
      ylab <- "MF"
    } else {
      si_spec <- ic_si_spectra()
      ylab <- "Intensity [V]"
    }
    ic_specplot(
      opt = opt, 
      xrng = c(spec_plots_xmin(), spec_plots_xmax()),
      mi_spec = ic_mi_spectra(),
      si_spec = si_spec,
      xlab = paste0("Time [", input$ic_par_mi_rt_unit, "]"),
      ylab = ylab,
      ylab2 = paste0(input$ic_par_si_col_name, "/", input$ic_par_mi_col_name),
      s_focus = input$ic_par_focus_sample,
      pks = ic_table_peaks_edit(),
      mi_pks = ic_mi_peaks(),
      cdf = current_drift_filter(),
      sel_pk = input$ic_table_peaks_rows_selected
    )
  })
  
  # delta plot ----
  output$ic_deltaplot2 <- shiny::renderPlot({
    shiny::req(ic_table_deltas_pre())
    message("output$ic_deltaplot2")
    ic_deltaplot(df = ic_table_deltas_pre())
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
  shiny::observeEvent(input$ic_help09, { help_the_user(filename = "09_IDMS_table") })
  shiny::observeEvent(input$ic_help10, { help_the_user(filename = "10_processing_params_IDMS") })

}
# ================================