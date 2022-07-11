#' @title Pull MPX Case Data
#'
#' @param path character string path to MPX data
#' @param connection (optional) an `spoConnection` object
#' @param ... additional arguments handled by other methods (not yet implemented)
#' 
#' @import dplyr
#' @import tidyr
#' @importFrom passport parse_country
#' 
get_mpx_cases <- function(path, connection = NULL) {
  raw_data <- fetch_mpx_cases(path, connection)

  out <- raw_data %>%
    tidyr::pivot_longer(-Country, names_to = "date", values_to = "cases") %>%
    mutate(
      iso3code = parse_country(Country, to = "iso3c"),
      date = as.Date(date, "%m/%d/%Y")
    )
  
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

# Pulling MPX data when passed a standard path
fetch_mpx_cases.default <- function(path, ...) {
  data_raw <- data.table::fread(path)

  return(as_tibble(data_raw))
}
