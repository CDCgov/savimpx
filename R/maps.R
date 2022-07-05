#' A sort-of standard map theme
#'
#' @import ggplot2
gddoc_map_theme <- function() {
  theme_classic() +
    theme(
      # Default text size for all others not specified
      text = element_text(size = 30),
      # Title, subtitle, caption
      plot.title = element_text(size = 44, face = "bold"),
      plot.subtitle = element_text(size = 38),
      plot.caption = element_text(hjust = 0),
      plot.caption.position = "panel",
      # Panel grid and backing color
      panel.grid.major = element_line(
        color = gray(.5), linetype = "dashed", size = 0.5
      ),
      panel.background = element_rect(fill = "#b8cee5"),
      # Legend
      legend.background = element_rect(color = "black"),
      legend.position = c(0.1, 0.25),
      legend.text = element_text(size = 28),
      legend.title = element_text(size = 30),
      # Size of the fill boxes
      legend.key.size = unit(2, "cm")
    )
}

#' @title Standard MPX Global Choropleth
#'
#' @param x A Monkey pox data.frame with at least `iso3code` and `cases` columns
#' @param breaks A numeric vector containing breaks for the choropleth bins
#' @param latest_date A date value for the subtitle, latest updated date (default: `Sys.Date()`)
#' @return A styled choropleth map for use in various data products
#'
#' @import sf
mpx_case_choro <- function(x, breaks = c(1, 2, 6, 20, 99), latest_date = Sys.Date()) {
  global_map %>%
    left_join(x, by = "iso3code") %>%
    mutate(cases_bin = cut_pretty_labels(cases, breaks = case_breaks)) %>%
    ggplot() +
    geom_sf(fill = "white") +
    geom_sf(aes(fill = cases_bin)) +
    scale_fill_brewer(type = "seq", palette = "Oranges", na.translate = FALSE) +
    labs(
      title = "Global Monkeypox Cases",
      subtitle = sprintf("Confirmed Cases in Non-endemic Countries \U2013 %s 5:00 PM EDT", format(latest_date, "%d %B %Y")),
      fill = "Confirmed Monkeypox Cases"
    ) +
    gddoc_map_theme()
}
