## Code to prepare global shapefile
library(sf)

global_map <- st_read("inst/extdata/global_map.geojson")

usethis::use_data(global_map, overwrite = TRUE)
