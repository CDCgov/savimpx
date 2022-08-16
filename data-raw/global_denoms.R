# Code to prepare global population denominators
library(savimpx)

global_denoms <- get_global_denoms()

usethis::use_data(global_denoms, overwrite = TRUE)
readr::write_csv(global_denoms, "inst/extdata/global_denoms.csv")
