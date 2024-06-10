install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
covid_hospitalizations <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/covid_hospitalizations.csv")

covid_hospitalizations
colnames(covid_hospitalizations)

covid_hospitalizations$month <- format(as.Date(covid_hospitalizations$date, format= "%d/%m/%Y"), "%m")


covid_hospitalizations |> group_by(date) |> 
  summarize(total_icu = sum(icu_total, na.rm = T),
            total_covid_icu = sum(covid_icu, na.rm = T)) |> 
  mutate(proportion_covid_icu = total_covid_icu / total_icu) |> 
  ggplot(aes(x = date, y = proportion_covid_icu)) +
  geom_line()



covid_hospitalizations |> 
  mutate(proportion_covid_icu = covid_icu / icu_total) |> 
 filter(county== c("Allegheny", "Jefferson", "Fayette", "Delaware", "Philadelphia", "Westmoreland")) |> 
  ggplot(aes(x = date, y = proportion_covid_icu, color=county, color=c("purple", "tomato", "thistle", "orange", "seagreen"))) +
  geom_line()+
  facet_wrap(~county)

 

covid_hospitalizations |> 
  mutate(proportion_covid_icu = covid_icu / icu_total) |> 
  ggplot(aes(x = date, y = proportion_covid_icu)) +
  geom_line()+
  facet_wrap(~county)


