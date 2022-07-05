#' A sort-of standard map theme
#'
#' @import ggplot2
gddoc_map_theme <- function() {
  theme_void() +
    theme(
      # Title, subtitle, caption
      plot.title = ggplot2::element_text(size = 15, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 8, margin = margin(0, 0, 5, 0)),
      plot.title.position = "panel",
      plot.caption = ggplot2::element_text(size = 6, hjust = 0, vjust = 2),
      plot.caption.position = "plot",

      panel.background = element_rect(fill = "#b8cee5", color = "white"),
      panel.border = element_blank(),
      # Legend
      legend.position = c(0.01, 0.01),
      legend.justification = c("left", "bottom"),
      legend.box.just = "left",
      legend.key.size = unit(0.5, "cm"),
      legend.margin = ggplot2::margin(2, 2, 2, 2),
      legend.title = ggplot2::element_text(size = 8),
      legend.text = ggplot2::element_text(size = 6),
      legend.background = element_rect(fill = scales::alpha("white", 0.5), colour = "black")
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
#' @export
mpx_case_choro <- function(x, breaks = c(1, 2, 6, 20, 99), latest_date = Sys.Date()) {
  global_map %>%
    left_join(x, by = "iso3code") %>%
    mutate(cases_bin = cut_pretty_labels(cases, cuts = breaks)) %>%
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
