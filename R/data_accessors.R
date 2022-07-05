
#' @title Pull MPX Case Data
#'
#' @param .path Either a spoConnection or character string path to MPX data
#' @param ... additional arguments handled by other methods (not yet implemented)
#' 
#' @importFrom readr read_csv
#' @import dplyr
#' @import tidyr
#' @importFrom passport parse_country
#' 
#' @export
get_mpx_cases <- function(.path, ...) {
  UseMethod("get_mpx_cases")
}


#' Pulling MPX data when an anonymous Sharepoint Connection is passed
#' @rdname get_mpx_cases
#' @export
get_mpx_cases.spoConnection <- function(.path, ...) {
  warning("Not yet implemented")

  return(NULL)
}

#' Pulling MPX data when passed a standard path
#' @rdname get_mpx_cases
#' @export
get_mpx_cases.default <- function(.path, ...) {
  data_raw <- readr::read_csv(.path, show_col_types = FALSE)

  out <- data_raw %>%
    tidyr::pivot_longer(-Country, names_to = "date", values_to = "cases") %>%
    mutate(
      iso3code = parse_country(Country, to = "iso3c"),
      date = as.Date(date, "%m/%d/%Y")
    )
}
