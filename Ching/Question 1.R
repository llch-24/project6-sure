library(tidyverse)
library(ggplot2)
library(gganimate)
library(sf)

theme_set(theme_minimal())

# PLOT 1 - MEDIAN####
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
  
# only have to run once, made "county" the name variable for both and made format the same

counties <- counties |>
rename(county = county_nam) |>
mutate(county = str_to_title(county))

# merge dataset
counties <- left_join(counties, county_summary, by = "county")

# map
counties |>
  ggplot() +
  geom_sf(aes(fill = median_icu_avail), color = "black", lwd = 0.2) +
  scale_fill_gradient(low = "#F4E9FF", high = "darkorchid") + 
  labs(title = "Median ICU Beds Available by County",
       subtitle = "Pennsylvania, April - December 2020",
       fill = "Beds Available"
  ) +
  theme(
    legend.title = element_text(vjust = 3,
                                size = 12),
    plot.title = element_text(vjust = 0, size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5))

#PLOT 2 - ANIMATION####
# load dataset
counties <- st_read("PaCounty2024_05 (1).geojson")
counties <-janitor::clean_names(counties)

covid_hospitalizations <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/covid_hospitalizations.csv")

# Ensure the date column is properly formatted as Date objects
monthly_covid_hospitalizations <- covid_hospitalizations |> 
  mutate(date = as.Date(date), month = floor_date(date, "month"))

# how to change this to show the month? don't want it to be a date but a string for better wrangling. not really that important

#finding the mean icu available for every month and county
monthly_covid_hospitalizations <- monthly_covid_hospitalizations |>
  group_by(county, month) |> 
  mutate(mean_icu_avail = mean(icu_avail, na.rm = T)) |> 
  ungroup()

monthly_covid_hospitalizations <- monthly_covid_hospitalizations |>
  group_by(county) |>
  distinct(month, .keep_all = TRUE) |> 
  ungroup() |> 
  select(county, month, mean_icu_avail)

# clean data 
counties <- counties |>
  rename(county = county_nam) |>
  mutate(county = str_to_title(county))

# join together datasets
counties <- left_join(counties, monthly_covid_hospitalizations, by = "county")

# make the plot
map <- counties |> 
  ggplot() + 
  geom_sf(aes(fill = mean_icu_avail), color = "black", lwd = 0.2) +
  scale_fill_gradient(low = "#F4E9FF", high = "darkorchid") + 
  labs(title = "Median ICU Beds Available by County: {frame_time}",
       subtitle = "Pennsylvania, April - December 2020",
       fill = "Beds Available") +
  theme(
    legend.title = element_text(vjust = 3, size = 12),
    plot.title = element_text(vjust = 0, size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5)
  ) + 
  transition_time(month) + 
  ease_aes('linear')

# Render the animation
animate(map, duration = 15, fps = 10)
