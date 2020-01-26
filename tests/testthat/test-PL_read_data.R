context("Download data errors")

test_that("Check that wrong date format throws error",{

  expect_error(suppressWarnings(PL_read_data("date1", "date2")))

  # expect_error(PL_read_data("10-0817", "10-0818"))
  #expect_error(PL_read_data("2017-07-01", "2018-05-15"))  # shouldn't throw error
})


test_that("Can read from website",{

  if (is.character(RCurl::getURL("www.google.com"))) {
    have_connection <- TRUE
  } else {
    have_connection  <- FALSE
  }

  if (!have_connection) skip("No internet connection")

  sample_data <- PL_read_data("2017-07-01", "2018-05-15")
  expect_equal(dim(sample_data), c(760, 65))

})
