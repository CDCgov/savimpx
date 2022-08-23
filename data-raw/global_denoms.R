# Code to prepare global population denominators
library(savimpx)

global_denoms <- get_global_denoms()

usethis::use_data(global_denoms, overwrite = TRUE)

# Write out to folder
readr::write_csv(global_denoms, "inst/extdata/global_denoms.csv")

# Save to Data Lake (If packages are available)
if (requireNamespace("AzureRMR") && requireNamespace("AzureStor")) {
  token <- AzureRMR::get_azure_token(
    "https://storage.azure.com",
    tenant = Sys.getenv("AZURE_TENANT_ID"),
    app = Sys.getenv("AZURE_APP_ID"),
    password = Sys.getenv("AZURE_APP_SECRET")
  )

  con <- AzureStor::storage_container(Sys.getenv("AZDL_CONTAINER"), token = token)

  AzureStor::storage_write_csv(global_denoms, con, "DGHT/ITF-SAVI/MPX/global_denoms.csv")
}
