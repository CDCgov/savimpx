#' @title Pull MPOX Case and Death Data
#' @description
#' This function provides a standardized interface to read in CDC Monkeypox line-list data
#' from local path, Sharepoint/Teams, or Azure DataLake. The interface is liable to change
#' as longer-term data storage changes.
#'
#' @param path character string path to MPX data
#' @param connection (optional) an `spoConnection` or `AzureStor::storage_container` object
#' @param include_endemic (boolean, default: `TRUE`) should data from MPX endemic countries be included?
#' @return A data frame with n rows and 4 variables:
#' \itemize{
#'   \item{\code{Country}}{ character English name for administrative region}
#'   \item{\code{date}}{ date reporting date}
#'   \item{\code{cases}}{ integer cumulative monkeypox cases}
#'   \item{\code{iso3code}}{ character ISO 3166-1 alpha-3 country code}
#' }
#'
#' @details
#' In the past, all MPOX data was "wide", where the first column is `Country`, and the following columns are
#' dates. This was pivoted internally to "long", and ISO code inferred from Country column.
#'
#' The default is now a "long" dataset, which doesn't require this processing (includes ISO code, country name, and date columns).
#'
#' @examples
#' \dontrun{
#' # Using local path (on your PC or from shared drive)
#' path <- "<path_to_my_dir>/mpx_data.csv"
#'
#' get_mpx_cases(path)
#'
#' # From Sharepoint / MS Teams (remotely)
#' # In this case, path should be relative to the Shared Documents folder
#' spo_path <- "<My Sharepoint Folder>/mpx_data.csv"
#' spo_con <- spoConnection$new(
#'   tenant = "Tennant-Id",
#'   client_id = Sys.getenv("CLIENT-ID"),
#'   client_secret = Sys.getenv("CLIENT-SECRET"),
#'   teams_name = "My-Team-Name"
#' )
#'
#' get_mpx_cases(spo_path, connection = spo_con)
#'
#' # From Azure Data Lake
#'
#' azdl_path <- "<path_to_azdl_folder>/mpx_data.csv"
#'
#' # Retrieve token using app registration
#' token <- AzureRMR::get_azure_token(
#'   "https://storage.azure.com",
#'   tenant = Sys.getenv("AZURE_TENANT_ID"),
#'   app = Sys.getenv("AZURE_APP_ID"),
#'   password = Sys.getenv("AZURE_APP_SECRET")
#' )
#'
#' # We use the Blob URL, but DFS should work too
#' azdl_con <- AzureStor::storage_container("https://<my_azdl_site>.blob.core.windows.net/<my_azdl_container>/", token = token)
#'
#' get_mpx_cases(azdl_path, connection = azdl_con)
#' }
#' @import dplyr
#' @import tidyr
#' @importFrom countrycode countrycode
#' @importFrom AzureStor storage_download
#' @importFrom janitor convert_to_date
#' @export
get_mpx_cases <- function(path, connection = NULL, include_endemic = TRUE) {

  # Pull data using whatever method required
  raw_data <- fetch_cdc_mpx_data(path, connection)
  df <- raw_data %>%
    rename(cases_cum = cases) %>%
    arrange(iso3code, date) %>%
    group_by(iso3code) %>%
    mutate(cases_new = cases_cum - lag(cases_cum)) %>%
    mutate(cases_new = if_else(is.na(cases_new), 0L, cases_new)) %>%
    ungroup()
  
  df_date_first <- df %>%
    filter(cases_cum > 0) %>%
    group_by(iso3code) %>%
    summarise(date = min(date)) %>%
    ungroup() %>%
    mutate(new_country = 1)
  
  df_date_last <- df %>%
    filter(cases_new > 0) %>%
    group_by(iso3code) %>%
    summarise(date = max(date)) %>%
    ungroup() %>%
    mutate(last_update = 1)
  
  raw_data <- df %>%
    left_join(df_date_first) %>%
    left_join(df_date_last)
  # Filter out endemic countries, if indicated
  if (!include_endemic) {
    out <- raw_data %>%
      filter(!iso3code %in% endemic_countries[["iso3code"]])
  } else {
    out <- raw_data
  }

  return(out)
}


#' @import dplyr
#' @import tidyr
#' @importFrom countrycode countrycode
#' @importFrom AzureStor storage_download
#' @importFrom janitor convert_to_date
#' @rdname get_mpx_cases
#' @export
get_mpx_deaths <- function(path, connection = NULL, include_endemic = TRUE) {

  # Pull data using whatever method required
  raw_data <- fetch_cdc_mpx_data(path, connection)
  df <- raw_data %>%
    rename(deaths_cum = deaths) %>%
    arrange(iso3code, date) %>%
    group_by(iso3code) %>%
    mutate(deaths_new = deaths_cum - lag(deaths_cum)) %>%
    mutate(deaths_new = if_else(is.na(deaths_new), 0L, deaths_new)) %>%
    ungroup()
  
  df_date_first <- df %>%
    filter(deaths_cum > 0) %>%
    group_by(iso3code) %>%
    summarise(date = min(date)) %>%
    ungroup() %>%
    mutate(new_country = 1)
  
  df_date_last <- df %>%
    filter(deaths_new > 0) %>%
    group_by(iso3code) %>%
    summarise(date = max(date)) %>%
    ungroup() %>%
    mutate(last_update = 1)
  
  raw_data <- df %>%
    left_join(df_date_first) %>%
    left_join(df_date_last)
  # Filter out endemic countries, if indicated
  if (!include_endemic) {
    out <- out %>%
      filter(!iso3code %in% endemic_countries[["iso3code"]])
  } else {
    out <- raw_data
  }

  return(out)
}


# Helper function to handle pulling line-list data
# (only used internally)
fetch_cdc_mpx_data <- function(path, connection = NULL, ...) {
  # Dispatch based on whether we need to pull from a sharepoint site
  # or locally
  UseMethod("fetch_cdc_mpx_data", connection)
}


# Pulling MPX data when an anonymous Sharepoint Connection is passed
fetch_cdc_mpx_data.spoConnection <- function(path, connection, ...) {
  data_raw <- connection$read_file(path)

  return(as_tibble(data_raw))
}

# Pulling MPX data when an AzureStor storage_container connection is passed
fetch_cdc_mpx_data.storage_container <- function(path, connection, ...) {
  tmp <- tempfile(fileext = paste0(".", tools::file_ext(path)))

  AzureStor::storage_download(connection, src = path, dest = tmp)

  read_fn <- get_read_fn(tmp)
  data_raw <- read_fn(tmp)

  return(as_tibble(data_raw))
}

# Pulling MPX data when passed a standard path
fetch_cdc_mpx_data.default <- function(path, ...) {
  read_fn <- get_read_fn(path)
  data_raw <- read_fn(path)

  return(as_tibble(data_raw))
}

