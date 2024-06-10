install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
covid_hospitalizations <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/covid_hospitalizations.csv")

covid_hospitalizations
colnames(covid_hospitalizations)

covid_hospitalizations$month <- format(as.Date(covid_hospitalizations$date, format= "%d/%m/%Y"), "%m")


covid_hospitalizations|> 
  select(month, icu_total, covid_icu) |> 
  group_by(month) |> 
  summarise(icu_total_month= sum(icu_total, na.rm=T),    covid_icu_month=sum(covid_icu, na.rm=T)) |> 
  mutate(prop_total= covid_icu_month/icu_total_month)
  
 
  


