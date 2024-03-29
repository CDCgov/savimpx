% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_accessors.R
\name{get_mpx_cases}
\alias{get_mpx_cases}
\alias{get_mpx_deaths}
\title{Pull MPOX Case and Death Data}
\usage{
get_mpx_cases(path, connection = NULL, include_endemic = TRUE)

get_mpx_deaths(path, connection = NULL, include_endemic = TRUE)
}
\arguments{
\item{path}{character string path to MPX data}

\item{connection}{(optional) an \code{spoConnection} or \code{AzureStor::storage_container} object}

\item{include_endemic}{(boolean, default: \code{TRUE}) should data from MPX endemic countries be included?}
}
\value{
A data frame with n rows and 4 variables:
\itemize{
\item{\code{Country}}{ character English name for administrative region}
\item{\code{date}}{ date reporting date}
\item{\code{cases}}{ integer cumulative monkeypox cases}
\item{\code{iso3code}}{ character ISO 3166-1 alpha-3 country code}
}
}
\description{
This function provides a standardized interface to read in CDC Monkeypox line-list data
from local path, Sharepoint/Teams, or Azure DataLake. The interface is liable to change
as longer-term data storage changes.
}
\details{
In the past, all MPOX data was "wide", where the first column is \code{Country}, and the following columns are
dates. This was pivoted internally to "long", and ISO code inferred from Country column.

The default is now a "long" dataset, which doesn't require this processing (includes ISO code, country name, and date columns).
}
\examples{
\dontrun{
# Using local path (on your PC or from shared drive)
path <- "<path_to_my_dir>/mpx_data.csv"

get_mpx_cases(path)

# From Sharepoint / MS Teams (remotely)
# In this case, path should be relative to the Shared Documents folder
spo_path <- "<My Sharepoint Folder>/mpx_data.csv"
spo_con <- spoConnection$new(
  tenant = "Tennant-Id",
  client_id = Sys.getenv("CLIENT-ID"),
  client_secret = Sys.getenv("CLIENT-SECRET"),
  teams_name = "My-Team-Name"
)

get_mpx_cases(spo_path, connection = spo_con)

# From Azure Data Lake

azdl_path <- "<path_to_azdl_folder>/mpx_data.csv"

# Retrieve token using app registration
token <- AzureRMR::get_azure_token(
  "https://storage.azure.com",
  tenant = Sys.getenv("AZURE_TENANT_ID"),
  app = Sys.getenv("AZURE_APP_ID"),
  password = Sys.getenv("AZURE_APP_SECRET")
)

# We use the Blob URL, but DFS should work too
azdl_con <- AzureStor::storage_container("https://<my_azdl_site>.blob.core.windows.net/<my_azdl_container>/", token = token)

get_mpx_cases(azdl_path, connection = azdl_con)
}
}
