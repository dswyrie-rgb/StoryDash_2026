library(readxl)
dengue <- read_excel("C:/Users/dswyr/Downloads/dengue_data_2010_2020.xlsx")
View(dengue)

unique(dengue$City)
str(dengue)
nrow(dengue)
colSums(is.na(dengue))

library(tidyverse)

dengue %>%
  mutate(Year = as.numeric(format(as.Date(Date), "%Y"))) %>%
  group_by(City, Year) %>%
  summarise(Total_Cases = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Year, y = Total_Cases, color = City, group = City)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_continuous(breaks = 2010:2020) +
  theme_minimal() +
  labs(title = "Yearly Dengue Trends by District", x = "Year", y = "Total Cases")

library(tidyverse)
library(tsibble)
library(feasts)

# 1. Prepare tsibble
dengue_ts <- dengue %>%
  mutate(Date = yearmonth(as.Date(Date))) %>%
  as_tsibble(index = Date, key = City)

# 2. Time plot for ALL cities (Faceted for clarity)
dengue_ts %>%
  autoplot(Value) +
  facet_wrap(~ City, scales = "free_y") +
  theme_minimal() +
  labs(title = "Monthly Dengue Time Series by District", x = "Date", y = "Cases")

# 3. Time plot for SELECTED cities on a single chart
dengue_ts %>%
  filter(City %in% c("Colombo", "Gampaha", "Kandy", "Galle")) %>%
  autoplot(Value) +
  theme_minimal() +
  labs(title = "Monthly Dengue Cases Comparison", x = "Date", y = "Cases")