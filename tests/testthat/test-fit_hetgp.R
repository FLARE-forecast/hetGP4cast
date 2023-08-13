context("fit_hetgp")
data("sample_lake_data_1mdepth")
testthat::test_that("errors", {

  testthat::expect_error(
    model1 = fit_hetgp(X = "D", Y = "temperature", site_id = "FCR", df = sample_lake_data_1mdepth),
    "Inputs, X, must be equal to : DOY"
  )

  testthat::expect_error(
    model1 = fit_hetgp(X = "DOY", Y = "temp", site_id = "FCR", df = sample_lake_data_1mdepth),
    "The only supported names for the response variable, Y, are : temperature"
  )
})



