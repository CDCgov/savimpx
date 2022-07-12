## Code to prepare endemic country lookup
library(data.table)
library(passport)
library(dplyr)

endemic_countries <- fread("inst/extdata/Endemic Countries.csv") %>%
  mutate(iso3code = parse_country(Country, to = "iso3c")) %>%
  as_tibble()

usethis::use_data(endemic_countries, overwrite = TRUE)
