library(tidyverse)
library(readxl)
library(leaflet)
library(tigris)
library(sf)

# Load the cleaned trends data
trends <- read_excel('2025/PC_trends_2016_2025_cleaned.xlsx')

# Load the water quality data to get the WBIDS sampled during the trend period
wq <- read_excel('data/pinellas_wq_2003_2026.xlsx', guess_max = 50000) |>
  filter(year(Date) >= 2016 & year(Date) <= 2025)

# Load in the WBID shape file
wqShp <- sf::st_read('GIS/Waterbody_IDs_(WBIDs).shp') |>
  dplyr::select(HUC, WBID, ID = WATERBODY_, REGION = GROUP_NAME, CLASS) |>
  sf::st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") |>
  filter(WBID %in% unique(wq$WBID))

# Get the county outline from the tigris package
pc <- counties(
  state = 'FL',
  cb = TRUE,
  year = 2025
) |>
  filter(NAME == 'Pinellas')

# Merge the WBID shapefile with the trends and change the color based on trend
# and magnitude
merge_trends <- function(param){
  
  wqShp |>
    left_join(trends |> filter(Param == param), by = 'WBID') |>
    group_by(WBID) |>
    filter(if(n() > 1) row_number() != 1 else TRUE) |>
    ungroup() |>
    mutate(color = case_when(
      Direction == 'Decreasing' & Magnitude == 'Small' ~ '#C1DEC1',
      Direction == 'Decreasing' & Magnitude == 'Large' ~ '#29AB29',
      Direction == 'Increasing' & Magnitude == 'Small' ~ '#F0A1A1',
      Direction == 'Increasing' & Magnitude == 'Large' ~ '#DB0707',
      TRUE ~ NA
    )
  )
  
}

merge_trends_do <- function(param){
  
  wqShp |>
    left_join(trends |> filter(Param == param), by = 'WBID') |>
    group_by(WBID) |>
    filter(if(n() > 1) row_number() != 1 else TRUE) |>
    ungroup() |>
    mutate(color = case_when(
      Direction == 'Increasing' & Magnitude == 'Small' ~ '#C1DEC1',
      Direction == 'Increasing' & Magnitude == 'Large' ~ '#29AB29',
      Direction == 'Decreasing' & Magnitude == 'Small' ~ '#F0A1A1',
      Direction == 'Decreasing' & Magnitude == 'Large' ~ '#DB0707',
      TRUE ~ NA
    )
    )
  
}


# Function to make the trend map
trend_map <- function(x) {

  leaflet::leaflet(
    options = leaflet::leafletOptions(
      zoomControl = TRUE, 
      zoomSnap    = 0.1, 
      zoomDelta   = 1)
  ) |>
    leaflet::setView(
      lng  = -82.72, 
      lat  = 27.905, 
      zoom = 10.3
    ) |>
    leaflet::addPolygons(
      data        = pc,
      weight = 0.8,
      fillOpacity = 0.5,
      stroke      = TRUE,
      color       = 'black',
      fillColor = 'white',
      opacity     = 1,
      fill        = TRUE 
    ) |>
    leaflet::addPolygons(
      data        = x |> filter(! is.na(color)),  
      weight      = 1,
      fillOpacity = 1,
      stroke      = TRUE,
      color       = 'black',
      fillColor = ~color,
      opacity     = 1,
      fill        = TRUE  
      ) |>
    leaflet::addPolygons(
      data        = x |> filter(is.na(color)),  
      weight      = 1,
      fillOpacity = 1,
      stroke      = TRUE,
      color       = 'black',
      fillColor = 'gray',
      opacity     = 1,
      fill        = TRUE  
    )

}

trend_map(merge_trends('TP'))
trend_map(merge_trends('TN'))
trend_map(merge_trends('Chl_a'))
trend_map(merge_trends_do('DO%'))
trend_map(merge_trends('Enterococci'))
trend_map(merge_trends('E_coli'))


