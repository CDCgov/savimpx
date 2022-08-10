test_that("All country names parse to ISO 3166-1 alpha-3 country codes", {
  test_data_path <- file.path("20220712-mpx-test-data.csv")
  # --- Cases ----------------------
  cases <- get_mpx_cases(test_data_path)

  expect_s3_class(cases, "data.frame")

  # Column and row length
  expect_true(nrow(cases) > 0L)
  expect_true(ncol(cases) == 4)

  # Column names and types
  expect_mapequal(
    vapply(cases, class, character(1)),
    c(Country = "character", date = "Date", cases = "integer", iso3code = "character")
  )

  # All ISO codes present
  expect_true(all(!is.na(cases[["iso3code"]])))
})