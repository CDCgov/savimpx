#' @title Global Map Shapefile
#' @description A standard shapefile we use for mapping tasks
#' @format A Simple Feature Collection with 174 rows and 4 variables:
#' \describe{
#'   \item{\code{TYPE}}{character one of ("Sovereign country", "Country", "Dependency", "Disputed", NA)}
#'   \item{\code{ADMIN}}{character English name for administrative region}
#'   \item{\code{iso3code}}{character ISO 3166-1 alpha-3 country code}
#'   \item{\code{geometry}}{list sf geometry column}
#' }
#' @details Liable to update, possibly (likely) incorrect spherical geometry.
"global_map"

#' @title Monkeypox Endemic Countries
#' @description A data frame of countries that are known to be endemic for Monkeypox.
#' @format A data frame with 10 rows and 3 variables:     
#' \describe{
#'   \item{\code{Country}}{character English name for administrative region}  
#'   \item{\code{Endemic}}{character "Y"}  
#'   \item{\code{iso3code}}{character ISO 3166-1 alpha-3 country code} 
#'}
#' @details 
#' This is used internally for filtering for some tables and viz.
"endemic_countries"
