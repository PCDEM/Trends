library(readxl)
library(tidyverse)
library(gt)
library(purrr)


trends <- read_excel('2024/PC_trends_2015_2024.xlsx', sheet = 'edited') |>
  mutate(Magnitude = ifelse(Trend == 'Not', NA, Magnitude),
         Trend = ifelse(Trend == 'Not','No Trend',Trend))

df <- trends |>
  filter(Param %in% c('TN','TP','DO%','Chl_a','TSS','Turbidity'),
         Level == 'Surface') |>
  mutate(Segment = str_to_title(Segment)) |>
  pivot_wider(names_from = 'Param', values_from = c('Trend','Magnitude')) |>
  select(-c(WBID, Latitude, Longitude, Level))

params <- df |>
  select(starts_with("Trend_")) |>
  names() %>%
  gsub("Trend_", "", .)

tbl <- df |>
  gt() |>
  cols_hide(columns = starts_with("Magnitude_")) |>
  cols_label(
    Trend_TN = "TN",
    Trend_TP = "TP",
    `Trend_DO%` = 'DO%',
    Trend_Chl_a = 'Chl-a',
    Trend_TSS = 'TSS',
    Trend_Turbidity = 'Turbidity') 

for (p in params) {
  trend_col <- paste0("Trend_", p)
  mag_col <- paste0("Magnitude_", p)
  
  if (p == "DO%") {
    tbl <- tbl %>%
      tab_style(
        style = cell_fill(color = "#c6e0b4"), # light green
        locations = cells_body(
          columns = trend_col,
          rows = !!sym(trend_col) == "Increasing" & !!sym(mag_col) == "Small"
        )
      ) %>%
      tab_style(
        style = cell_fill(color = "#70ad47"), # darker green
        locations = cells_body(
          columns = trend_col,
          rows = !!sym(trend_col) == "Increasing" & !!sym(mag_col) == "Large"
        )
      ) %>%
      tab_style(
        style = cell_fill(color = "#f4b084"),
        locations = cells_body(
          columns = trend_col,
          rows = !!sym(trend_col) == "Decreasing" & !!sym(mag_col) == "Small"
        )
      ) %>%
      tab_style(
        style = cell_fill(color = "red"),
        locations = cells_body(
          columns = trend_col,
          rows = !!sym(trend_col) == "Decreasing" & !!sym(mag_col) == "Large"
        )
      )
  } else {
    tbl <- tbl %>%
      tab_style(
        style = cell_fill(color = "#f4b084"),
        locations = cells_body(
          columns = trend_col,
          rows = !!sym(trend_col) == "Increasing" & !!sym(mag_col) == "Small"
        )
      ) %>%
      tab_style(
        style = cell_fill(color = "red"),
        locations = cells_body(
          columns = trend_col,
          rows = !!sym(trend_col) == "Increasing" & !!sym(mag_col) == "Large"
        )
      ) %>%
      tab_style(
        style = cell_fill(color = "#c6e0b4"),
        locations = cells_body(
          columns = trend_col,
          rows = !!sym(trend_col) == "Decreasing"
        )
      )
  }
}

tbl
  


