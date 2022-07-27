#' Top 10 Table Summaries
#'
#' @import gt

mpx_table_cases <- function(){
  df <- mpx_case_refresh(mpx_linelist_path, spo_con, include_endemic = FALSE)
  
  df_table <- df %>%
    group_by(Country) %>%
    filter(date == max(date)) %>%
    summarise(cum_cases = max(cases, na.rm = TRUE)) %>%
    arrange(desc(cum_cases)) %>%
    head(10) %>%
    mutate("rank" = 1:10)

  mpx_top10_cases <- gt(df_table) %>%
    tab_header(title = "Top 10 Countries") %>%
    cols_move_to_start(rank) %>%
    cols_label(cum_cases = "Cumulative Cases",
               rank = "") %>%
    fmt_number(columns = cum_cases, use_seps = TRUE, decimals = 0) %>%
    tab_options(table.font.size = 20,
                column_labels.font.weight = "bold",
                column_labels.font.size = 20,
                row_group.padding = 0,
                data_row.padding = 0)
}
  

#' Top 10 Table Summaries
#' New cases calculated using "date_since"
#' Enter "date_since" using date format YYYY-MM-DD
#' 
#' 
  mpx_table_newcases <- function(date_since){
    df <- mpx_case_refresh(mpx_linelist_path, spo_con, include_endemic = FALSE)
    
    df_table_newcases <- df %>%
      filter(date >= date_since) %>%
      group_by(Country) %>%
      mutate(cases_new = cases - lag(cases)) %>%
      summarise(cases_2week = sum(cases_new, na.rm = TRUE)) %>%
      arrange(desc(cases_2week)) %>%
      head(10) %>%
      mutate("rank" = 1:10)
    
  mpx_top10_newcases <- gt(df_table_newcases) %>%
    tab_header(title = "Top 10 Countries") %>%
    tab_footnote(footnote = paste("*New cases since", date_since)) %>%
    cols_move_to_start(rank) %>%
    cols_label(cases_2week = "New Cases*",
               rank = "") %>%
    fmt_number(columns = cases_2week, use_seps = TRUE, decimals = 0) %>%
    tab_options(table.font.size = 20,
                column_labels.font.weight = "bold",
                column_labels.font.size = 20,
                row_group.padding = 0,
                data_row.padding = 0)

}
