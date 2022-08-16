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

#' @title Global Population Denominators
#' @description A centralized metadata table containing all country ids, categories, and population counts
#' @format A data frame with 237 rows and 9 variables:
#' \itemize{
#'   \item{\code{iso3code}}{character ISO 3166-1 alpha-3 country code}
#'   \item{\code{iso2code}}{character ISO 3166-1 alpha-2 country code}
#'   \item{\code{state_region}}{character US Department of State Region}
#'   \item{\code{who_region}}{character WHO Region acronym}
#'   \item{\code{who_region_desc}}{character WHO Region english name}
#'   \item{\code{who_country}}{character WHO English country text name}
#'   \item{\code{incomelevel_value}}{character World Bank Income level}
#'   \item{\code{population}}{double UN Total population estimates for 2020}
#'   \item{\code{eighteenplus}}{double UN 18+ population estimates for 2020}
#'   \item{\code{geometry}}{list List of simple features for mapping}
#' }
#' @section Notes:
#' Population figures for Pitcairn Islands augmented from CIA World Factbook.
#'
#' @section Sources:
#'   * https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_General/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx
#'   * https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_TotalPopulationBySex.csv
#'   * https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/2_Population/WPP2022_POP_F03_1_POPULATION_SELECT_AGE_GROUPS_BOTH_SEXES.xlsx
#'   * https://www.cia.gov/the-world-factbook/field/population/country-comparison
#'   * https://worldbank.org
#' @md
"global_denoms"

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

#' Additional entries not acknowledged by WHO
#' but required to create global_denoms
#' @keywords internal
onetable_addn_countries <- data.frame(
  iso2code = c("HK", "MO", "TW"),
  country = c("Hong Kong", "Macau", "Taiwan"),
  who_region = c("WPRO", "WPRO", "WPRO")
)

#' Countries with no current UNWPP data
#' that are manually updated here from CIA World Factbook
#' https://www.cia.gov/the-world-factbook/field/population/country-comparison
#' Currently 2022 estimates are being used
#' @keywords internal
cia_wfb_addn_countries <- data.frame(
  country = c("Pitcairn Islands"),
  iso3code = c("PCN"),
  total = c(50)
)