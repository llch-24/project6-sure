#How do the dynamics of total ventilators and ventilator use change over time?
#(Continuous) Line plot with multiple lines for total ventilators and ventilator use (Line plot series)
#two lines, first ventilators in use and second total ventilators)


#loading the library and dataset
library(tidyverse)
covid_hospitalizations <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/covid_hospitalizations.csv")

#view(covid_hospitalizations)

#Summarize ventilator data by date
#Change dates
ventilator_dynamics <- covid_hospitalizations %>%
  group_by() %>%
  summarise(total_vents = mean(vents, na.rm = TRUE),
            vents_in_use = mean(vents_use, na.rm = TRUE))

# Line plot for ventilator dynamics over time
ggplot(ventilator_dynamics, aes(x = date)) +
  geom_line(aes(y = total_vents, color = "Total Ventilators")) +
  geom_line(aes(y = vents_in_use, color = "Ventilators in Use")) +
  labs(title = "Ventilator Use Dynamics Over Time",
       x = "Date",
       y = "Number of Ventilators",
       color = "Ventilator Use") +
  theme_light() 

