#' Oneliner that splits a vector (x) into n chunks
#' helpful for string formatting when we have discrete text we want to avoid splitting
#' @param x a vector (not-necessarily atomic) to split
#' @param n numeric number of bins to split x into
chunk <- function(x, n) split(x, cut(seq_along(x), n, labels = FALSE))

#' A thin wrapper around Hmisc::cut2 which
#' relabels the output labels using a format string
#' @param x A numeric vector to bin using cut2
#' @param fmt A string interpretable by sprintf formatting the breaks
#' @param last_val_plus (default: FALSE) Should the final break be an interval or "{max(x)}+"
#' @param ... additional parameters passed thru to cut2
#'
#' @importFrom Hmisc cut2
#' @importFrom forcats fct_relabel
cut_pretty_labels <- function(x, fmt = "%s-%s", last_val_plus = FALSE, ...) {
  binned_vals <- Hmisc::cut2(x, oneval = FALSE, ...)

  out <- forcats::fct_relabel(binned_vals, ~ cut_relabel(., fmt, last_val_plus))

  return(out)
}

#' Relabel bins from cut2 nicely for plotting
#' NOTE: Assumes Hmisc::cut2(., oneval=FALSE)
#'
#' @param str a character vector of labels from cut2 to re-label
#' @param fmt a format string interpretable by sprintf to format the labels with
#' @param last_val_plus (boolean) If `TRUE`, will output last level with "number+" notation
#'
#' @importFrom stringr str_match
cut_relabel <- function(str, fmt = "%s - %s", last_val_plus = FALSE) {
  split_str <- stringr::str_match(str, "\\[\\s*(\\d*)\\,\\s*(\\d*)[\\]\\)]")

  # cut2 provides left-sided cuts, so we have to subtract one
  # from all the bins that have a ")"
  left_sided <- grepl("\\)", split_str[, 1])
  split_str[left_sided, 3] <- as.character(as.integer(split_str[left_sided, 3]) - 1L)

  # Paste back together using format string provided
  out <- sprintf(fmt, split_str[, 2], split_str[, 3])

  # Correct for any levels that have just one value
  single_val <- split_str[, 2] == split_str[, 3]
  out[single_val] <- split_str[single_val, 2]

  # Fix up the final value if indicated
  if (last_val_plus) {
    # Find max last value
    max_val_str <- as.character(max(as.integer(split_str[, 2])))
    which_max <- split_str[, 2] == max_val_str

    out[which_max] <- paste0(max_val_str, "+")
  }


  # Pass thru NAs
  out[is.na(split_str[, 1])] <- NA_character_

  return(out)
}
