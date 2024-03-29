% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maps.R
\name{mpx_case_choro}
\alias{mpx_case_choro}
\title{Standard MPX Global Choropleth}
\usage{
mpx_case_choro(x, breaks = c(1, 2, 6, 20, 99), latest_date = Sys.Date())
}
\arguments{
\item{x}{A Monkey pox data.frame with at least \code{iso3code} and \code{cases} columns}

\item{breaks}{A numeric vector containing breaks for the choropleth bins}

\item{latest_date}{A date value for the subtitle, latest updated date (default: \code{Sys.Date()})}
}
\value{
A styled choropleth map for use in various data products
}
\description{
Standard MPX Global Choropleth
}
