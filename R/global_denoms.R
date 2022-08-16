# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Update Global Population Denominators
#' @description
#' Output is available through the package as "global_denoms," but this function can be used to recreate this dataset.
#'
#' Note: World Health Organization (WHO) region, Department of State (DoS) region, and english country names are handled externally in a CSV file.

#' @param vintage (numeric, default: 2021) The year of population projections to use from UN data
#' @param country_metadata (data.frame, optional) A data.frame containing country-specific metadata with at least four columns: ["iso3code", "iso3code", "state_region", "who_region"]

#' @return Returns a df of 238 rows and 10 columns
#' @section Note:
#' * `country_metadata` overrides metadata contained in package. Your milage may vary.  
#' * Population updates for Pitcairn Islands are hardcoded and must be pulled manually via CIA factbook unless another source is found.
#'
#' @seealso [global_denoms] for more complete data documentation
#' @examples
#' \dontrun{
#' # UPDATING global_denoms
#' global_denoms <- get_global_denoms()
#' usethis::use_data(global_denoms, overwrite = TRUE)
#' write_csv(global_denoms, "inst/extdata/global_denoms.csv")
#' }
#' @importFrom readxl read_xlsx
#' @importFrom httr GET content
#' @export
#'
get_global_denoms <- function(vintage = 2021, country_metadata = NULL) {

  # === Country Metadata =========================================================
  # including the complete list of countries and their DoS and WHO region designation
  # If no file was passed, use the one saved in the package files
  if (is.null(country_metadata)) {
    country_metadata_path <- system.file("inst/extdata/region_meta.csv", package = "savimpx")
    country_metadata <- fread(country_metadata_path, stringsAsFactors = FALSE, encoding = "UTF-8")
  }

  # === World Bank Income Indicators =============================================
  # Make the API call to the World Bank's API for income classification metadata.
  res <- httr::GET(datasource_lk$wb_income)

  df_wb <- httr::content(res, as = "parsed", simplifyDataFrame = TRUE, flatten = TRUE)[[2]] %>%
    rename_all(tolower) %>%
    # Remove aggregates, and "Channel Islands", which is not a country
    filter(region.value != "Aggregates" | is.na(region.value), iso2code != "JG") %>%
    select(iso3code = id, incomelevel_value = incomelevel.value) %>%
    as_tibble()

  # === UN World Population ===================================================
  # Getting the population numbers from UNWPP, and Pitcairn Islands from CIA WFB

  # --- Download xlsx files for processing
  unwpp_metadata_path <- tempfile(fileext = ".xlsx")
  unwpp_18plus_path <- tempfile(fileext = ".xlsx")

  download.file(datasource_lk$un_location_meta, unwpp_metadata_path, mode = "wb")
  download.file(datasource_lk$un_age_projections, unwpp_18plus_path, mode = "wb")

  # --- Location / Country metadata ------------
  df_un_location_meta <- readxl::read_xlsx(unwpp_metadata_path, sheet = 1, skip = 16, guess_max = 1e5) %>%
    select(country = 3, LocID = 5, iso3code = 6, type = 9) %>%
    filter(type == "Country/Area") %>%
    distinct() %>%
    as_tibble()
  # --- Total population estimates ---------------------------
  df_un_medium_pop_est <- data.table::fread(datasource_lk$un_overall_projections) %>%
    filter(Variant == "Medium", Time == vintage) %>%
    mutate(total = 1000 * as.numeric(PopTotal)) %>%
    distinct(LocID, Time, total)

  # --- 18+ Estimates -------------------------------------------
  df_un_medium_pop_est_single_year <- readxl::read_xlsx(unwpp_18plus_path, sheet = 1, skip = 16, guess_max = 1e5) %>%
    filter(Year == vintage) %>%
    semi_join(df_un_location_meta, by = c("Location code" = "LocID")) %>% # Filter to only countries, to speed up summarize step
    select(LocID = `Location code`, Time = Year, `18+`) %>%
    mutate(Time = as.integer(Time), `18+` = 1000 * as.numeric(`18+`))

  # --- Join all UN pop estimates together and add the manual CIA ones -------------------------
  df_all_un_pop_est <- df_un_location_meta %>%
    left_join(df_un_medium_pop_est, by = "LocID") %>%
    left_join(df_un_medium_pop_est_single_year, by = c("LocID", "Time")) %>%
    select(country, iso3code, total, `18+`) %>%
    # Add in data from CIA world factbook
    bind_rows(cia_wfb_addn_countries)

  # === Join Country metadata, UNWPP, and WB data together ===================
  df_meta <- country_metadata %>%
    left_join(df_wb, by = "iso3code") %>%
    left_join(df_all_un_pop_est, by = "iso3code") %>%
    select(-country) # Debugging to see if it aligns with who_country

  # === Add "pretty" WHO region names ========================================
  df_meta <- df_meta %>%
    mutate(who_region_desc = who_region_lk[who_region])

  df_meta <- df_meta %>%
    select(
      iso3code, iso2code, state_region, who_region, who_region_desc,
      who_country, incomelevel_value, population = total, eighteenplus = `18+`
    ) %>%
    arrange(iso3code) %>%
    as_tibble()

  return(df_meta)
}