library(tidyverse)
covid_hospitalizations <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/covid_hospitalizations.csv")
#update

covid_hospitalizations |> 
  group_by(month = floor_date(date, "month")) |> 
  filter(vents_use == as.factor(vents_use), vents == as.factor(vents)) |> 
  summarize(mean_vents_use = mean(vents_use), mean_vents_total = mean(vents), na.rm = T) |> 
  ggplot(aes(x=month)) +
  geom_line(aes(y=mean_vents_use)) + 
  geom_line(aes(y=mean_vents_total))