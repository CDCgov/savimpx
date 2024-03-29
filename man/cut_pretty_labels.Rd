% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{cut_pretty_labels}
\alias{cut_pretty_labels}
\title{A thin wrapper around Hmisc::cut2 which
relabels the output labels using a format string}
\usage{
cut_pretty_labels(
  x,
  fmt = "\%s-\%s",
  last_value_gte = TRUE,
  big_mark = ",",
  ...
)
}
\arguments{
\item{x}{A numeric vector to bin using cut2}

\item{fmt}{A string interpretable by sprintf formatting the breaks}

\item{last_value_gte}{(boolean, default: TRUE) If \code{TRUE}, will output last level with ">=number" notation}

\item{big_mark}{(default: ,) A big mark separator for number formatting or NULL for none}

\item{...}{additional parameters passed thru to cut2}
}
\description{
A thin wrapper around Hmisc::cut2 which
relabels the output labels using a format string
}
