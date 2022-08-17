#' Oneliner that splits a vector (x) into n chunks
#' helpful for string formatting when we have discrete text we want to avoid splitting
#' @param x a vector (not-necessarily atomic) to split
#' @param n numeric number of bins to split x into
chunk <- function(x, n) split(x, cut(seq_along(x), n, labels = FALSE))

#' A thin wrapper around Hmisc::cut2 which
#' relabels the output labels using a format string
#' @param x A numeric vector to bin using cut2
#' @param fmt A string interpretable by sprintf formatting the breaks
#' @param last_value_gte (boolean, default: TRUE) If `TRUE`, will output last level with ">=number" notation
#' @param big_mark (default: ,) A big mark separator for number formatting or NULL for none
#' @param ... additional parameters passed thru to cut2
#'
#' @importFrom Hmisc cut2
#' @importFrom forcats fct_relabel
cut_pretty_labels <- function(x, fmt = "%s-%s", last_value_gte = TRUE, big_mark = ",", ...) {
  # If we ask for big mark, supply a function for number formatting with that big mark
  if (!is.null(big_mark)) {
    fmt_fun <- ~format(., big.mark=big_mark)
  }

  # Bin our values with cut2
  binned_vals <- Hmisc::cut2(x, oneval = TRUE, formatfun = fmt_fun, ...)

  # Relabeling function for pretty formatting
  out <- forcats::fct_relabel(binned_vals, ~ cut_relabel(., fmt, last_value_gte=last_value_gte, big_mark = big_mark))

  return(out)
}

#' Relabel bins from cut2 nicely for plotting
#' NOTE: Assumes Hmisc::cut2(., oneval=FALSE)
#'
#' @param str a character vector of labels from cut2 to re-label
#' @param fmt a format string interpretable by sprintf to format the labels with
#' @param last_value_gte (boolean) If `TRUE`, will output last level with ">=number" notation
#'
#' @importFrom stringr str_match
cut_relabel <- function(str, fmt = "%s - %s", big_mark = NULL, last_value_gte = FALSE) {
  split_str <- stringr::str_match(str, "\\[\\s*(\\d+\\,*\\d*)\\,\\s*(\\d*\\,*\\d*)[\\]\\)]")

  # cut2 provides left-sided cuts, so we have to subtract one
  # from all the bins that have a ")"
  left_sided <- grepl("\\)", split_str[, 1])
  split_str[left_sided, 3] <- format(as.integer(sub(big_mark, "", split_str[left_sided, 3], fixed=TRUE)) - 1L, big.mark = big_mark, trim = TRUE)

  # Paste back together using format string provided
  out <- sprintf(fmt, split_str[, 2], split_str[, 3])

  # Correct for any levels that have just one value
  single_val <- split_str[, 2] == split_str[, 3]
  out[single_val] <- split_str[single_val, 2]

  # Fix up the final value if indicated
  if (last_value_gte) {
    # Find max last value
    max_val_str <- format(max(as.integer(sub(big_mark, "", split_str[, 2], fixed = TRUE)), na.rm = TRUE), big.mark = big_mark, trim = TRUE)
    which_max <- split_str[, 2] == max_val_str

    out[which_max] <- paste0(">=", max_val_str)
  }


  # Ignore NAs (pass thru input value)
  # HACK: Trim ws to avoid justified look
  out[is.na(split_str[, 1])] <- trimws(str[is.na(split_str[, 1])])

  return(out)
}

# A helper function to return a reading function based on file extension
# (or return error)
get_read_fn <- function(file) {
  ext <- tolower(tools::file_ext(file))
  fn <- switch(ext,
    csv = data.table::fread,
    tsv = data.table::fread,
    xlsx = readxl::read_xlsx,
    xls = readxl::read_xls,
    stop(sprintf("No read function available for extension %s", ext))
  )

  return(fn)
}