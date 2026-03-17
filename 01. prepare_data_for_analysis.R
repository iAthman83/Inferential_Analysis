# -----------------------------
# load packages
# -----------------------------
source("./resources/init.R")

# -----------------------------
# read data for all months
# -----------------------------
fuel_prices <- read.csv("./data/fuel_prices_ssd.csv") %>%
  mutate(,
    temp_date = dmy(Price.Date),
    join_date = format(temp_date, "%m/%Y"),
    join_county = tolower(str_replace_all(Admin.2, fixed(" "), "")),
    Price = as.numeric(Price)
  ) %>%
  select(-temp_date) %>%
  filter(Data.Type == "Aggregated") %>%
  filter(Commodity == "Fuel (petrol-gasoline)") %>%
  group_by(join_county, join_date) %>%
  summarise(
    # Get the average price for that county in that month
    mean_fuel_price = mean(Price, na.rm = TRUE),
    .groups = "drop" # This replaces ungroup() automatically
  )
# -----------------------------
# read data for all months
# -----------------------------

raw_data_jan25 <- read.xlsx(
  "./data/JMMI_HISTORICAL_Jan_2025_Feb_2026.xlsx",
  sheet = 1
) %>%
  mutate(
    date = "01/01/2025",
    across(everything(), as.character),
    temp_date = dmy(date),
    join_date = format(temp_date, "%m/%Y"),
    join_county = tolower(County)
  ) %>%
  select(-temp_date)
# -----------------------------
raw_data_feb25 <- read.xlsx(
  "./data/JMMI_HISTORICAL_Jan_2025_Feb_2026.xlsx",
  sheet = 2
) %>%
  mutate(
    date = "01/02/2025",
    across(everything(), as.character),
    temp_date = dmy(date),
    join_date = format(temp_date, "%m/%Y"),
    join_county = tolower(County)
  ) %>%
  select(-temp_date)
# -----------------------------
raw_data_mar25 <- read.xlsx(
  "./data/JMMI_HISTORICAL_Jan_2025_Feb_2026.xlsx",
  sheet = 3
) %>%
  mutate(
    date = "01/03/2025",
    across(everything(), as.character),
    temp_date = dmy(date),
    join_date = format(temp_date, "%m/%Y"),
    join_county = tolower(County)
  ) %>%
  select(-temp_date)
# -----------------------------
raw_data_apr25 <- read.xlsx(
  "./data/JMMI_HISTORICAL_Jan_2025_Feb_2026.xlsx",
  sheet = 4
) %>%
  mutate(
    date = "01/04/2025",
    across(everything(), as.character),
    temp_date = dmy(date),
    join_date = format(temp_date, "%m/%Y"),
    join_county = tolower(County)
  ) %>%
  select(-temp_date)
# -----------------------------
raw_data_may25 <- read.xlsx(
  "./data/JMMI_HISTORICAL_Jan_2025_Feb_2026.xlsx",
  sheet = 5
) %>%
  mutate(
    date = "01/05/2025",
    across(everything(), as.character),
    temp_date = dmy(date),
    join_date = format(temp_date, "%m/%Y"),
    join_county = tolower(County)
  ) %>%
  select(-temp_date)
# -----------------------------
raw_data_jun25 <- read.xlsx(
  "./data/JMMI_HISTORICAL_Jan_2025_Feb_2026.xlsx",
  sheet = 6
) %>%
  mutate(
    date = "01/06/2025",
    across(everything(), as.character),
    temp_date = dmy(date),
    join_date = format(temp_date, "%m/%Y"),
    join_county = tolower(County)
  ) %>%
  select(-temp_date)
# -----------------------------
raw_data_jul25 <- read.xlsx(
  "./data/JMMI_HISTORICAL_Jan_2025_Feb_2026.xlsx",
  sheet = 7
) %>%
  mutate(
    date = "01/07/2025",
    across(everything(), as.character),
    temp_date = dmy(date),
    join_date = format(temp_date, "%m/%Y"),
    join_county = tolower(County)
  ) %>%
  select(-temp_date)
# -----------------------------
raw_data_aug25 <- read.xlsx(
  "./data/JMMI_HISTORICAL_Jan_2025_Feb_2026.xlsx",
  sheet = 8
) %>%
  mutate(
    date = "01/08/2025",
    across(everything(), as.character),
    temp_date = dmy(date),
    join_date = format(temp_date, "%m/%Y"),
    join_county = tolower(County)
  ) %>%
  select(-temp_date)
# -----------------------------
raw_data_sep25 <- read.xlsx(
  "./data/JMMI_HISTORICAL_Jan_2025_Feb_2026.xlsx",
  sheet = 9
) %>%
  mutate(
    date = "01/09/2025",
    across(everything(), as.character),
    temp_date = dmy(date),
    join_date = format(temp_date, "%m/%Y"),
    join_county = tolower(County)
  ) %>%
  select(-temp_date)
# -----------------------------
raw_data_oct25 <- read.xlsx(
  "./data/JMMI_HISTORICAL_Jan_2025_Feb_2026.xlsx",
  sheet = 10
) %>%
  mutate(
    date = "01/10/2025",
    across(everything(), as.character),
    temp_date = dmy(date),
    join_date = format(temp_date, "%m/%Y"),
    join_county = tolower(County)
  ) %>%
  select(-temp_date)
# -----------------------------
raw_data_nov25 <- read.xlsx(
  "./data/JMMI_HISTORICAL_Jan_2025_Feb_2026.xlsx",
  sheet = 11
) %>%
  mutate(
    date = "01/11/2025",
    across(everything(), as.character),
    temp_date = dmy(date),
    join_date = format(temp_date, "%m/%Y"),
    join_county = tolower(County)
  ) %>%
  select(-temp_date)
# -----------------------------
raw_data_dec25 <- read.xlsx(
  "./data/JMMI_HISTORICAL_Jan_2025_Feb_2026.xlsx",
  sheet = 12
) %>%
  mutate(
    date = "01/12/2025",
    across(everything(), as.character),
    temp_date = dmy(date),
    join_date = format(temp_date, "%m/%Y"),
    join_county = tolower(County)
  ) %>%
  select(-temp_date)
# -----------------------------
raw_data_jan26 <- read.xlsx(
  "./data/JMMI_HISTORICAL_Jan_2025_Feb_2026.xlsx",
  sheet = 13
) %>%
  mutate(
    date = "01/01/2026",
    across(everything(), as.character),
    temp_date = dmy(date),
    join_date = format(temp_date, "%m/%Y"),
    join_county = tolower(County)
  ) %>%
  select(-temp_date)
# -----------------------------
raw_data_feb26 <- read.xlsx(
  "./data/JMMI_HISTORICAL_Jan_2025_Feb_2026.xlsx",
  sheet = 14
) %>%
  mutate(
    date = "01/02/2026",
    across(everything(), as.character),
    temp_date = dmy(date),
    join_date = format(temp_date, "%m/%Y"),
    join_county = tolower(County)
  ) %>%
  select(-temp_date)


merged_data <- bind_rows(
  raw_data_jan25,
  raw_data_feb25,
  raw_data_mar25,
  raw_data_apr25,
  raw_data_may25,
  raw_data_jun25,
  raw_data_jul25,
  raw_data_aug25,
  raw_data_sep25,
  raw_data_oct25,
  raw_data_nov25,
  raw_data_dec25,
  raw_data_jan26,
  raw_data_feb26
)

final_dataset <- left_join(
  merged_data,
  fuel_prices,
  by = c("join_date", "join_county")
)

write.xlsx(
  final_dataset,
  "./output/JMMI_fuel_prices_gasoline.xlsx"
)
