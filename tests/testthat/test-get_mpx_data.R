test_that("Local File I/O works", {
  test_data_path <- file.path("20220712-mpx-test-data.csv")
  test_data_deaths_path <- file.path("20220809-mpx-test-data-deaths.csv")
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

  # --- Deaths --------------------------

  deaths <- get_mpx_deaths(test_data_deaths_path)

  expect_s3_class(deaths, "data.frame")

  # Column and row length
  expect_true(nrow(deaths) > 0L)
  expect_true(ncol(deaths) == 4)

  # Column names and types
  expect_mapequal(
    vapply(deaths, class, character(1)),
    c(Country = "character", date = "Date", deaths = "integer", iso3code = "character")
  )
})

test_that("Local File I/O from Excel works", {
  test_data_path <- file.path("20220712-mpx-test-data.xlsx")
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
})

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

test_that("Sharepoint / MS Teams I/O works", {
  env_vars <- list(
    tenant = Sys.getenv("AZURE_TENANT_ID"),
    client_id = Sys.getenv("ITF_SAVI_PROD_ID"),
    client_secret = Sys.getenv("ITF_SAVI_PROD_SECRET"),
    teams_name = Sys.getenv("ITF_SAVI_TEAMS_NAME"),
    case_file_path = Sys.getenv("SPO_CASE_PATH"),
    deaths_file_path = Sys.getenv("SPO_DEATH_PATH")
  )

  # Skip if we don't have these assigned
  skip_if(any(env_vars == ""), "Some SPO Environment Vars Missing, skipping.")

  # Test writeout path
  spo_con <- spoConnection$new(
    tenant = Sys.getenv("AZURE_TENANT_ID"),
    client_id = Sys.getenv("ITF_SAVI_PROD_ID"),
    client_secret = Sys.getenv("ITF_SAVI_PROD_SECRET"),
    teams_name = Sys.getenv("ITF_SAVI_TEAMS_NAME")
  )

  # --- Cases ----------------------
  cases <- get_mpx_cases(Sys.getenv("SPO-CASE-PATH"), connection = spo_con)

  expect_s3_class(cases, "data.frame")

  # Column and row length
  expect_true(nrow(cases) > 0L)
  expect_true(ncol(cases) == 4)

  # Column names and types
  expect_mapequal(
    vapply(cases, class, character(1)),
    c(Country = "character", date = "Date", cases = "integer", iso3code = "character")
  )

  # --- Deaths --------------------------
  deaths <- get_mpx_deaths(Sys.getenv("SPO-DEATH-PATH"), connection = spo_con)

  expect_s3_class(deaths, "data.frame")

  # Column and row length
  expect_true(nrow(deaths) > 0L)
  expect_true(ncol(deaths) == 4)

  # Column names and types
  expect_mapequal(
    vapply(deaths, class, character(1)),
    c(Country = "character", date = "Date", deaths = "integer", iso3code = "character")
  )
})

test_that("Azure Data Lake I/O works", {
  env_vars <- list(
    tenant = Sys.getenv("AZURE_TENANT_ID"),
    client_id = Sys.getenv("AZURE_APP_ID"),
    client_secret = Sys.getenv("AZURE_APP_SECRET"),
    case_file_path = Sys.getenv("AZDL_CASE_PATH"),
    deaths_file_path = Sys.getenv("AZDL_DEATH_PATH"),
    container = Sys.getenv("AZDL_CONTAINER")
  )

  # Skip if we don't have these assigned
  skip_if(any(env_vars == ""), "Some AZDL Environment Vars Missing, skipping.")
  skip_if(!requireNamespace("AzureRMR", quietly = TRUE), "AzureRMR required for AZDL tests, Skipping.")

  # Test writeout path
  data_lake_path <- Sys.getenv("AZDL-CONTAINER")

  azure_token <- AzureRMR::get_azure_token(
    "https://storage.azure.com",
    tenant = Sys.getenv("AZURE_TENANT_ID"),
    app = Sys.getenv("AZURE_APP_ID"),
    password = Sys.getenv("AZURE_APP_SECRET")
  )

  azure_container <- AzureStor::storage_container(data_lake_path, token = azure_token)

  # --- Cases ----------------------
  cases <- get_mpx_cases(Sys.getenv("AZDL_CASE_PATH"), connection = azure_container)

  expect_s3_class(cases, "data.frame")

  # Column and row length
  expect_true(nrow(cases) > 0L)
  expect_true(ncol(cases) == 4)

  # Column names and types
  expect_mapequal(
    vapply(cases, class, character(1)),
    c(Country = "character", date = "Date", cases = "integer", iso3code = "character")
  )

  # --- Deaths --------------------------
  deaths <- get_mpx_deaths(Sys.getenv("AZDL_DEATH_PATH"), connection = azure_container)

  expect_s3_class(deaths, "data.frame")

  # Column and row length
  expect_true(nrow(deaths) > 0L)
  expect_true(ncol(deaths) == 4)

  # Column names and types
  expect_mapequal(
    vapply(deaths, class, character(1)),
    c(Country = "character", date = "Date", deaths = "integer", iso3code = "character")
  )
})