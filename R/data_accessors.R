#' @title Pull MPX Case Data
#' @description 
#' This function provides a standardized interface to read in CDC Monkeypox line-list data 
#' from local path, Sharepoint/Teams, or Azure DataLake. The interface is liable to change
#' as longer-term data storage changes.
#' 
#' @param path character string path to MPX data
#' @param connection (optional) an `spoConnection` or `AzureStor::storage_container` object
#' @param include_endemic (boolean, default: `TRUE`) should data from MPX endemic countries be included?
#' 
#' @return A data frame with n rows and 4 variables: 
#' \itemize{
#'   \item{\code{Country}}{ character English name for administrative region}
#'   \item{\code{date}}{ date reporting date}      
#'   \item{\code{cases}}{ integer cumulative monkeypox cases}    
#'   \item{\code{iso3code}}{ character ISO 3166-1 alpha-3 country code}
#'}
#' 
#' @details 
#' Note that this assumes that `path` links to a "wide" CSV 
#' where the first column is `Country`, and the following columns are 
#' dates. This is pivoted internally to "long", and ISO code inferred from
#' Country column.
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
#'  tenant = "Tennant-Id",
#'  client_id = Sys.getenv("CLIENT-ID"),
#'  client_secret = Sys.getenv("CLIENT-SECRET"),
#'  teams_name = "My-Team-Name"
#' )
#' 
#' get_mpx_cases(spo_path, connection = spo_con)
#' 
#' # From Azure Data Lake
#'  
#' azdl_path <- "<path_to_azdl_folder>/mpx_data.csv"
#' 
#'  # Retrieve token using app registration
#'  token <- AzureRMR::get_azure_token(
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

#'  
#' }
#' @import dplyr
#' @import tidyr
#' @importFrom passport parse_country
#' @importFrom AzureStor storage_download
#' @export
get_mpx_cases <- function(path, connection = NULL, include_endemic = TRUE) {

  # Pull data using whatever method required
  raw_data <- fetch_mpx_cases(path, connection)

  out <- raw_data %>%
    tidyr::pivot_longer(-Country, names_to = "date", values_to = "cases") %>%
    mutate(date = convert_to_date(date, character_fun = ymd)) %>%
    mutate(
      iso3code = parse_country(Country, to = "iso3c"),
      date = as.Date(date, "%m/%d/%Y")
    )
  
  # Filter out endemic countries, if indicated
  if (!include_endemic) {
    out <- out %>%
      filter(!iso3code %in% endemic_countries[["iso3code"]])
  }
  
  return(out)
}



# Helper function to handle pulling line-list data
# (only used internally)
fetch_mpx_cases <- function(path, connection = NULL, ...) {
  # Dispatch based on whether we need to pull from a sharepoint site
  # or locally
  UseMethod("fetch_mpx_cases", connection)
}


# Pulling MPX data when an anonymous Sharepoint Connection is passed
fetch_mpx_cases.spoConnection <- function(path, connection, ...) {
  data_raw <- connection$read_file(path)

  return(as_tibble(data_raw))
}

# Pulling MPX data when an AzureStor storage_container connection is passed
fetch_mpx_cases.storage_container <- function(path, connection, ...) {
  tmp <- tempfile(fileext = ".csv")
  
  AzureStor::storage_download(connection, src=path, dest=tmp)
  
  data_raw <- data.table::fread(tmp)

  return(as_tibble(data_raw))
}

# Pulling MPX data when passed a standard path
fetch_mpx_cases.default <- function(path, ...) {
  data_raw <- data.table::fread(path)

  return(as_tibble(data_raw))
}
