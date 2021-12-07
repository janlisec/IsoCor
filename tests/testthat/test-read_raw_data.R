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