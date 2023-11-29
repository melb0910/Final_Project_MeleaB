library(ggplot2)
library(tidyverse)
homicides <-read.csv("/Users/meleabarahona/Library/CloudStorage/OneDrive-Colostate/Second Year 2023-2024/ERHS535/Final_Project_MeleaB/data/homicide-data.csv")

library(lubridate)
homicides$reported_date <- ymd(homicides$reported_date)

homicides_MD <- homicides %>%
  filter(city == "Baltimore")

homicides_MD <- homicides_MD %>%
  mutate(Season = ifelse(month(reported_date) %in% c(5:10, 11:4), "Summer", "Winter"))

library(stringr)

homicides_MD <- homicides_MD %>%
  separate(reported_date, c("Year", "Month", "Day"), sep = "-") 
    

Freddie_grey <- homicides_MD %>%
  filter(victim_first == "FREDDIE CARLOS")

homicides_counts <- homicides_MD %>%
  group_by(Month, Year, Season) %>%
  count() %>%
  ungroup()

homicides_counts <- homicides_counts %>%
  mutate(month_year = paste(Month, Year, 01, sep = "-")) 

homicides_counts$month_year <- myd(homicides_counts$month_year)

colors <- c("gray", "cadetblue1")
names(colors) <- c("Summer", "Winter")

colors


homicides_counts %>%
  ggplot() +
  geom_bar(aes(x = month_year, y = n, fill = Season), stat = "identity") +
  scale_fill_manual(values = colors) +
  geom_smooth(span = 0.28, se = FALSE, (aes(month_year, n))) +
  geom_vline(xintercept = ymd("2015-04-12"), linetype = "dashed", color = "red") +
  geom_text(x = ymd("2015-04-12"), y = 40, label = "Death of \n Freddie Grey", hjust = 1.08) +
  ggtitle("Homicides in Baltimore, MD") +
  labs(x = "Dates",
       y = "Monthly Homicides") +
  theme_dark()

