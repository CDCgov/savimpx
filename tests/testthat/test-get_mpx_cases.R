test_that("Endemic Countries are filtered correctly", {
  # Read in test data
  test_data_path <- file.path("20220712-mpx-test-data.csv")
  # -- Including Endemic Countries
  endemic <- get_mpx_cases(test_data_path, include_endemic = TRUE)

  any_endemic <- any(endemic[["iso3code"]] %in% endemic_countries[["iso3code"]])

  # Expect that Endemic countries are included if requested
  expect_true(any_endemic)

  # -- Excluding Endemic Countries
  non_endemic <- get_mpx_cases(test_data_path, include_endemic = FALSE)

  any_endemic <- any(non_endemic[["iso3code"]] %in% endemic_countries[["iso3code"]])

  # Expect that endemic countries are excluded if requested
  expect_false(any_endemic)
})