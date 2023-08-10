testthat::test_that(
	desc = "ic_app works as expected", 
	code = {
	  # Don't run these tests on the CRAN build servers
	  testthat::skip_on_cran()
	  
	  # remove resource path 'www' to get consistent snapshots
	  if ("www" %in% names(shiny::resourcePaths())) shiny::removeResourcePath("www")
	  
	  # run this test app in a headless browser using shinytest2
	  app <- shinytest2::AppDriver$new(IsoCor::ic_app(), name = "ic_app", load_timeout = 25*1000, timeout = 10*1000, seed = 1234, height = 1080, width = 1920)
	  #app$view()
	  #app$expect_values()
	  
	  # get initial app values
	  init_vals <- app$get_values()

	  # check if inputs are consistent
	  previous <- c("CurrentScreenHeight", "ic_help01", "ic_help02", "ic_help03", 
	                "ic_help04", "ic_help05", "ic_help10", "ic_par_Abund_MI", "ic_par_Abund_SI", 
	                "ic_par_IDMS_f", "ic_par_IDMS_halfWindowSize", "ic_par_IDMS_mb_method", 
	                "ic_par_IR_sample", "ic_par_IR_spike", "ic_par_Inj_Amount", "ic_par_MF_Spike", 
	                "ic_par_align_rt", "ic_par_app_method", "ic_par_baseline_method", 
	                "ic_par_cut_range", "ic_par_focus_sample", "ic_par_halfWindowSize", 
	                "ic_par_inputformat", "ic_par_libsource", "ic_par_mi_amu", "ic_par_mi_col", 
	                "ic_par_mi_col_name", "ic_par_mi_rt_unit", "ic_par_path_expfiles_inner", 
	                "ic_par_peakpicking_SNR", "ic_par_peakpicking_k", "ic_par_peakpicking_noise", 
	                "ic_par_rt_col", "ic_par_set_drift", "ic_par_si_amu", "ic_par_si_col", 
	                "ic_par_si_col_name", "ic_par_specplot", "ic_tabPanel_tables", 
	                "ic_table_peaks_cell_clicked", "ic_table_peaks_cells_selected", 
	                "ic_table_peaks_columns_selected", "ic_table_peaks_rows_all", 
	                "ic_table_peaks_rows_current", "ic_table_peaks_rows_selected", 
	                "ic_table_peaks_search", "ic_table_peaks_state", "sidebar_button"
	  )
	  previous <- c("ic_help01", "ic_help02", "ic_help03")
	  current <- names(init_vals$input)
	  testthat::expect_true(all(previous %in% current))
	  # test <- current[!(current %in% previous)]
	  # if (length(test)>=1) message(test)
	  # testthat::expect_equal(current, previous)
	  # 
	  # # check if IDMS test data were correctly initialized as 'file_in'
	  # testthat::expect_equal(shiny::isolate(init_vals$export$`file_in`()), IsoCor::testdata_IDMS)
	  
	  # check if IR test data are loaded upon click
	  app$set_inputs(ic_par_libsource = "testdata")
	  file_in <- shiny::isolate(app$get_values(export = "file_in")$export$file_in())
	  testthat::expect_equal(file_in, IsoCor::testdata)

	}
)
