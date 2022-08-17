#' Manual ISO 3166-1 alpha-3 country code entries that don't get parsed by countrycode
manual_iso3_lk <- list(
  Micronesia = "FSM",
  `Saint Martin` = "MAF",
  `Eswatini` = "SWZ"
)

#' @title WHO "pretty" english region names
#' @description A helper lookup table to map WHO region acronyms to a "pretty" english name.
#' @format A character vector of length 6:
#' \describe{
#'   \item{\code{AMRO}}{character Americas}
#'   \item{\code{EURO}}{character Europe}
#'   \item{\code{SEARO}}{character Southeast Asia}
#'   \item{\code{EMRO}}{character Eastern Mediterranean}
#'   \item{\code{AFRO}}{character Africa}
#'   \item{\code{WPRO}}{character Western Pacific}
#' }
#' @details For internal use in generating [global_denoms] via [get_global_denoms()]
#' @keywords internal
who_region_lk <- c(
  AMRO = "Americas",
  EURO = "Europe",
  SEARO = "Southeast Asia",
  EMRO = "Eastern Mediterranean",
  AFRO = "Africa",
  WPRO = "Western Pacific"
)

#' A list of all data sources used in the package
#' to be updated as needed.
#' @keywords internal
datasource_lk <- list(
  # World Bank country income classification
  wb_income = "http://api.worldbank.org/v2/country?format=json&per_page=300",
  # UN World Population Projections (UNWPP)
  # Location metadata
  un_location_meta = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_General/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx",
  # Total country population projections
  un_overall_projections = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_TotalPopulationBySex.csv",
  # Country population projections by age group
  un_age_projections = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/2_Population/WPP2022_POP_F03_1_POPULATION_SELECT_AGE_GROUPS_BOTH_SEXES.xlsx"
)