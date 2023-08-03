testthat::test_that(
  desc = "read_raw_data can handle all inpt types",
  code = {
    
    fmt_data <- system.file(package = "IsoCor", "extdata", 'fmt_data.txt')
    testthat::expect_true(is.data.frame(IsoCor::read_raw_data(path = fmt_data, format = "data")))
    testthat::expect_null(IsoCor::read_raw_data(path = fmt_data, format = "exp"))
    testthat::expect_null(IsoCor::read_raw_data(path = fmt_data, format = "icp"))
    testthat::expect_null(IsoCor::read_raw_data(path = fmt_data, format = "generic"))
    
    fmt_exp <- system.file(package = "IsoCor", "extdata", 'fmt_exp.exp')
    testthat::expect_null(IsoCor::read_raw_data(path = fmt_exp, format = "data"))
    testthat::expect_true(is.data.frame(IsoCor::read_raw_data(path = fmt_exp, format = "exp")))
    testthat::expect_null(IsoCor::read_raw_data(path = fmt_exp, format = "icp"))
    testthat::expect_null(IsoCor::read_raw_data(path = fmt_exp, format = "generic"))
    
    fmt_icp <- system.file(package = "IsoCor", "extdata", 'fmt_icp.txt')
    testthat::expect_null(IsoCor::read_raw_data(path = fmt_icp, format = "data"))
    testthat::expect_null(IsoCor::read_raw_data(path = fmt_icp, format = "exp"))
    testthat::expect_true(is.data.frame(IsoCor::read_raw_data(path = fmt_icp, format = "icp")))
    testthat::expect_null(IsoCor::read_raw_data(path = fmt_icp, format = "generic"))
    
    fmt_generic <- system.file(package = "IsoCor", "extdata", 'fmt_generic.txt')
    testthat::expect_null(IsoCor::read_raw_data(path = fmt_generic, format = "data"))
    testthat::expect_null(IsoCor::read_raw_data(path = fmt_generic, format = "exp"))
    #testthat::expect_null(IsoCor::read_raw_data(path = fmt_generic, format = "icp"))
    testthat::expect_true(is.data.frame(IsoCor::read_raw_data(path = fmt_generic, format = "icp")))
    testthat::expect_true(is.data.frame(IsoCor::read_raw_data(path = fmt_generic, format = "generic")))
    
  }
)

testthat::test_that(
	desc = "iso_ratio is calculated properly", 
	code = {
	  peak1 <- 1 + cos(x = seq(-pi, pi, length.out = 100))
	  set.seed(1234)
	  peak2 <- 0.05 * peak1 * runif(n = 100, min = 1, max = 1.01)
	  inp <- cbind(peak1, peak2)
	  testthat::expect_equal(iso_ratio(data = inp), 0.05019344)
	  exp_out <- list("PBP"=0.05019344, "PAI"=0.05022314, "LRS"=0.05023559)
    for (i in 1:length(exp_out)) {
      testthat::expect_equal(
        IsoCor::iso_ratio(data = inp, method = names(exp_out)[i]), 
        exp_out[[i]], 
        tolerance = 10^-5)
    }
	}
)

testthat::test_that(
  desc = "mass_bias is calculated properly", 
  code = {
    exp_out <- list("Linear"=1.2, "Russel"=1.006081, "Exponential"=1.221403)
    for (i in 1:length(exp_out)) {
      testthat::expect_equal(
        IsoCor::mass_bias(mi_amu = 32, si_amu = 34, method = names(exp_out)[i], f_value = 0.1), 
        exp_out[[i]], 
        tolerance = 10^-5
      )
    }
  }
)

testthat::test_that(
  desc = "find_peak_boundaries works as expected", 
  code = {
    x <- sin(seq(-pi, 2*pi, 0.01))+1
    testthat::expect_equal(IsoCor::find_peak_boundaries(x), c(157, 787))
  }
)
