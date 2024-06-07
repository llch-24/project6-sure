library(tidyverse)
library(ggplot2)
library(sf)
library(dplyr)
library(janitor)
covid_hospitalizations <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/covid_hospitalizations.csv")
clean_names(covid_hospitalizations)

#median of the available ICU beds per county####
# bc mean is skewed due to outliers
county_summary <- covid_hospitalizations |> 
  group_by(county) |> 
  summarize(median_icu_avail = median(icu_avail, na.rm = T)) |> 
  mutate(median_icu_avail = as.numeric(median_icu_avail))

#load shapefile, API broken 
counties <- st_read("PaCounty2024_05 (1).geojson")
counties <- clean_names(counties)

counties <- counties |> 
  rename(county = county_nam)

# make basic map to check it works
counties |> 
ggplot() +
  geom_sf(fill = "white", color = "black") +
  theme_minimal()

counties <- counties |> 
  left_join(county_summary, by = "county")

# trying to merge and it doesn't work!!!
county_summary <- left_join(county_summary, counties, by = "county")

# map also shows NA 
counties |>
  ggplot() +
  geom_sf(aes(fill = median_icu_avail), color = "white", lwd = 0.2) +
  labs(title = "Median ICU Beds Available by County",
       subtitle = "Pennsylvania, April - December 2020")

  
          








