# Load required libraries
library(tidyverse)
library(lubridate)

setwd("C:\\Users\\dswyr\\Downloads")

# 1. Download/Read the dataset directly from HDX
data <- read_csv("C:\\Users\\dswyr\\Downloads\\lka-rainfall-subnat-full.csv")
colSums(is.na(data))

# 2. Lookup table for Sri Lanka District P-Codes (adm_level = 2)
pcode_to_district <- tibble(
  PCODE = c("LK11", "LK12", "LK13", "LK21", "LK22", "LK23", "LK31", "LK32", "LK33", 
            "LK41", "LK42", "LK43", "LK44", "LK45", "LK51", "LK52", "LK53", "LK61", 
            "LK62", "LK71", "LK72", "LK81", "LK82", "LK91", "LK92"),
  district_name = c("Colombo", "Gampaha", "Kalutara", "Kandy", "Matale", "Nuwara Eliya", 
                    "Galle", "Matara", "Hambantota", "Jaffna", "Kilinochchi", "Mannar", 
                    "Vavuniya", "Mullaitivu", "Batticaloa", "Ampara", "Trincomalee", 
                    "Kurunegala", "Puttalam", "Anuradhapura", "Polonnaruwa", "Badulla", 
                    "Moneragala", "Ratnapura", "Kegalle")
)

# 3. Process the data
rainfall_monthly_2010_2020 <- data %>%
  # Filter for District level (adm_level == 2)
  filter(adm_level == 2) %>%
  # Convert date column to Date format
  mutate(date = as.Date(date)) %>%
  # Filter date range: 2010 to 2020
  filter(year(date) >= 2010 & year(date) <= 2020) %>%
  # Join district names using PCODE
  left_join(pcode_to_district, by = "PCODE") %>%
  # Extract Year and Month
  mutate(
    year = year(date),
    month = month(date, label = TRUE, abbr = F)
  ) %>%
  # Group by District, Year, and Month to aggregate dekadal rainfall into monthly total
  group_by(district_name, PCODE, adm_id, year, month) %>%
  summarise(
    total_monthly_rainfall_mm = sum(rfh, na.rm = TRUE),
    avg_dekad_rainfall_mm = mean(rfh, na.rm = TRUE),
    .groups = "drop"
  )

# View the formatted output
head(rainfall_monthly_2010_2020)

# 4. (Optional) Save to a CSV file
write_csv(rainfall_monthly_2010_2020, "sri_lanka_monthly_rainfall_2010_2020.csv")