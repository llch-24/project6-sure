library(tidyverse)
library(ggplot2)
library(sf)
library(janitor)
library(gganimate)

theme_set(theme_minimal())

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

# ventilators  
covid_hospitalizations |> 
  group_by(month = floor_date(date, "month")) |> 
  filter(vents_use == as.factor(vents_use), vents == as.factor(vents)) |> 
  summarize(mean_vents_use = mean(vents_use), mean_vents_total = mean(vents), na.rm = T) |> 
  ggplot(aes(x=month)) +
  geom_line(aes(y=mean_vents_use)) + 
  geom_line(aes(y=mean_vents_total))
