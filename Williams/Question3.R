#How do the dynamics of total ventilators and ventilator use change over time?
#(Continuous) Line plot with multiple lines for total ventilators and ventilator use (Line plot series)
#two lines, first ventilators in use and second total ventilators)


#loading the library and dataset
library(tidyverse)
covid_hospitalizations <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/covid_hospitalizations.csv")

#view(covid_hospitalizations)

#Summarize ventilator data by date
ventilator_dynamics <- covid_hospitalizations %>%
  group_by(month = floor_date(date, "month")) |> 
  filter(vents_use == as.factor(vents_use), vents == as.factor(vents)) |> 
  summarize(mean_vents_use = mean(vents_use), mean_vents_total = mean(vents), na.rm = T)

#updated
# Line plot for ventilator dynamics over time
ggplot(ventilator_dynamics, aes(x = month)) +
  geom_line(aes(y=mean_vents_use, color = "Ventilators in Use")) + 
  geom_line(aes(y=mean_vents_total, color = "Total Ventilators")) +
  labs(title = "Ventilator Use Dynamics Over Time",
       x = "Date",
       y = "Number of Ventilators",
       color = "Ventilator Use") +
  theme_light()