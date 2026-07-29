library(tidyverse)
library(haven)

# Read in Location File
location <- read_sas("data/pinco_stations_watercourse2019.sas7bdat") |>
  mutate(final_stat = as.factor(STATION))

# Set water quality data path
dataPath <- 'data/pinellas_wq_2003_2026.xlsx'

# Define the end year to run the trends for. The trends will be calculated over
# the previous 10 years:
endyr <- 2025

# Source trends function
source("rkt_func.r")

# Run trends test
output <- rkt_func(data, location, tsfreq = 8, endyr)

# Save raw trends results to excel file
write.xlsx(output, paste0(endyr,'/PC_trends_', endyr - 9, '_', endyr, '.xlsx'))


# Get coordinates for each site for map arrows:
coords <- read_excel(dataPath, guess_max = 50000) |>
  filter(year(Date) >= endyr - 9) |>
  mutate(
    Site = ifelse(substr(Site,1,1) %in% c('E','W','S','L','R'), substr(Site,1,2),Site),
    Latitude = case_when(
      substr(Site,1,2) == 'W1' ~ 28.115382,
      substr(Site,1,2) == 'W2' ~ 28.019274,
      substr(Site,1,2) == 'W3' ~ 27.941201,
      substr(Site,1,2) == 'W4' ~ 27.822104,
      substr(Site,1,2) == 'W5' ~ 27.816364,
      substr(Site,1,2) == 'W6' ~ 27.789688,
      substr(Site,1,2) == 'W7' ~ 27.727523,
      substr(Site,1,2) == 'W8' ~ 27.663779,
      substr(Site,1,2) == 'SA' ~ 27.878414,
      substr(Site,1,2) == 'SB' ~ 27.848546,
      substr(Site,1,2) == 'LT' ~ 28.117508,
      substr(Site,1,2) == 'E1' ~ 27.982044,
      substr(Site,1,2) == 'E2' ~ 27.944683,
      substr(Site,1,2) == 'E3' ~ 27.937264,
      substr(Site,1,2) == 'E4' ~ 27.895923,
      substr(Site,1,2) == 'E5' ~ 27.854661,
      substr(Site,1,2) == 'RB' ~ 27.851203,
      TRUE ~ Latitude
    ),
    Longitude = case_when(
      substr(Site,1,2) == 'W1' ~ -82.805717,
      substr(Site,1,2) == 'W2' ~ -82.805357,
      substr(Site,1,2) == 'W3' ~ -82.824850,
      substr(Site,1,2) == 'W4' ~ -82.811026,
      substr(Site,1,2) == 'W5' ~ -82.762235,
      substr(Site,1,2) == 'W6' ~ -82.761709,
      substr(Site,1,2) == 'W7' ~ -82.713078,
      substr(Site,1,2) == 'W8' ~ -82.704781,
      substr(Site,1,2) == 'LT' ~ -82.728277,
      substr(Site,1,2) == 'SA' ~ -82.775218,
      substr(Site,1,2) == 'SB' ~ -82.781266,
      substr(Site,1,2) == 'E1' ~ -82.663591,
      substr(Site,1,2) == 'E2' ~ -82.711832,
      substr(Site,1,2) == 'E3' ~ -82.645195,
      substr(Site,1,2) == 'E4' ~ -82.604644,
      substr(Site,1,2) == 'E5' ~ -82.583360,
      substr(Site,1,2) == 'RB' ~ -82.616182,
      TRUE ~ Longitude
    )) |>
  select(Site, Segment, WBID, Latitude, Longitude) |>
  distinct()


# Cleaned up trends table
cleanTrends <- output |>
  select(Site = Station, Param, Level = lvl, Trend = final_td, Direction = final_dir,
         Magnitude = rel_mag, p_value = pval.corr) |>
  mutate(Direction = ifelse(Trend == 'No Trend', NA, Direction),
         Magnitude = ifelse(Trend == 'No Trend', NA, Magnitude),
         p_value = round(p_value,4)) |>
  filter(Trend == 'Trend',
         Direction != 'Not',
         Param %in% c('TN', 'TP','DO%','Chl_a','E_coli','Enterococci')) |>
  arrange(Site, Param) |>
  left_join(coords, by = 'Site') |>
  relocate(c('Segment','WBID','Latitude','Longitude'), .after = 'Site')



# Save cleaned trends:
write.xlsx(cleanTrends, paste0(endyr, '/PC_trends_', endyr-9,'_',endyr,'_cleaned.xlsx'))







