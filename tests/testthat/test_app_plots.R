testthat::test_that("ic_specplot produces an expected figure", {
  # prepare input data
  set.seed(0)
  utils::data("testdata", package = "IsoCor")
  suppressWarnings({
    mi_spec <- lapply(testdata, function(x) {
      MALDIquant::createMassSpectrum(mass = x[,"Time"]/60, intensity = x[,"32S"])
    })
    si_spec <- lapply(testdata, function(x) {
      MALDIquant::createMassSpectrum(mass = x[,"Time"]/60, intensity = x[,"34S"])
    })
  })
  # avoid creating a Rplots.pdf in testthat folder
  pdf(NULL)
  vdiffr::expect_doppelganger(
    title = "ic_specplot Standard",
    fig = function() IsoCor:::ic_specplot(mi_spec=mi_spec)
  )
  vdiffr::expect_doppelganger(
    title = "ic_specplot Annotated",
    fig = function() IsoCor:::ic_specplot(
      opt = c("overlay_mi", "overlay_legend", "overlay_si", "overlay_drift", "correct_drift"),
      mi_spec=mi_spec, si_spec=si_spec
    )
  )
})

testthat::test_that("ic_deltaplot produces an expected figure", {
  # prepare input data
  set.seed(0)
  df <- data.frame(
    "Ratio method" = gl(n = 3, k = 4, labels = c("PBP","PAI","LRS")),
    "Zone [%]" = rep(c(50,80,95,100), 3),
    "Mean Delta" = rnorm(12),
    "SD Delta" = rnorm(12),
    check.names = FALSE
  )
  # avoid creating a Rplots.pdf in testthat folder
  pdf(NULL)
  vdiffr::expect_doppelganger(
    title = "ic_deltaplot Standard",
    fig = function() IsoCor:::ic_deltaplot(df = df)
  )
})
  
