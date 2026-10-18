library(haven)
library(readxl)
library(tidyverse)

# sas <- read_sas('pinellas_trend_input_2003_2022.sas7bdat')

r <- data.frame(
  read_excel('data/Pinellas_County_Water_Quality_Data_2003_2024.xlsx', guess_max = 50000)
) |>
  filter(year(Date) %in% seq(2014,2023))

# # Remove data if it has certain qualifiers associated with it:
# for(i in seq(19, ncol(r)-1, 2)){
#   r[, i][which(grepl('Y|\\?|\\*\\*', r[, i + 1]) == TRUE)] <- NA
# }

# declare within year sampling frequency for autocorrelation test
tsfreq<-8

#define end year
endyr<-2023 #list.files


r2 <- r |>
  dplyr::mutate(
    Year = lubridate::year(Date), 
    TN = TKN + NOX, 
    Period = as.numeric(Period),
    Date = as.Date(Date), 
    Site = ifelse(substr(Site,1,1) %in% c('E','W','S','L','R'), substr(Site,1,2),Site)
  ) |>
  dplyr::select(
    station = Site, 
    Level, 
    Year, 
    sample_period = Period, 
    Date, 
    Conductivity = 'Sp.Conductivity',
    DO, 
    'DO%' = DO., 
    Salinity, 
    pH, 
    BOD5, 
    Chl_a = 'Chl.a', 
    E_coli, 
    Color, 
    Enterococci,
    NH3, NOX, OP, TKN, TN, TP, TSS, Turbidity
  ) |>
  tidyr::pivot_longer(
    cols = -c(station, Level, Year, sample_period, Date), 
    names_to='Param', 
    values_to='Value'
  ) |>
  group_by(station) |>
  arrange(station,Param)


r3 <- r2 |>
  group_by(station, Date) |>
  nest() |>
  mutate(data = map(data, ~ {
    # Extract surface and bottom data
    surface_data <- .x %>% filter(Level == "Surface")
    bottom_data <- .x %>% filter(Level == "Bottom")
    
    # If no bottom data, duplicate surface data and change Level to "Bottom"
    if (nrow(bottom_data) == 0) {
      bottom_data <- surface_data
      bottom_data$Level <- "Bottom"
    }
    
    # If fewer bottom data, duplicate rows to match the number of surface data
    if (nrow(bottom_data) < nrow(surface_data)){
      bottom_data <- bottom_data |>
        slice(rep(1:n(), length.out = nrow(surface_data)))
    }
    
    # Combine surface and (adjusted) bottom data
    bind_rows(surface_data, bottom_data)
  })) |>
  unnest(cols = c(data)) |>
  arrange(station, Date, Level)


# For each site and each param in r3, remove the earliest NA rows until the first
# data point is reached:
r4 <- r3 %>%
  group_by(station, Param) %>%
  mutate(first_data = which(!is.na(Value))[1]) %>%
  filter(row_number() >= first_data) %>%
  ungroup() %>%
  select(-first_data) |>
  filter(! (Level == 'Bottom' & Param %in% c('TN','OP','TP','TSS','Turbidity',
                                             'Chl_a','E_coli','Enterococci','Color',
                                             'NH3','NOX','TKN','TN','BOD5'))) |>
  filter(! station %in% c('35-04','53-05')) |>
  group_by(station,Date, Level, Year,sample_period, Param) |>
  summarise(Value = median(Value, na.rm = TRUE)) |>
  ungroup()




source('rkt_func.R')

output <- name(r4,location)
# output2 <- name(sas3,location)
# 



output



