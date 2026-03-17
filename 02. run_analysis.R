rm(list = ls())
# -----------------------------
# load packages
# -----------------------------
source("./resources/init.R")
# Install missing packages if needed
# install.packages(c("dplyr", "lubridate", "ggplot2", "tidyr", "ggcorrplot"))

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(ggcorrplot)

# ==========================================
# 1. Pre-process
# ==========================================
# Load the dataset
df <- read.xlsx("./output/JMMI_fuel_prices_gasoline.xlsx")

# Ensure date is a proper Date object, drop missing dates, aggregate by month
df_clean <- df %>%
  select(date, mean_fuel_price, MSSMEB, MSSMEB.food.basket) %>%
  # Assuming dates are formatted as dd/mm/yyyy based on the CSV
  mutate(
    date = dmy(date),
    MSSMEB = as.numeric(MSSMEB),
    MSSMEB.food.basket = as.numeric(MSSMEB.food.basket)
  ) %>%
  filter(!is.na(date)) %>%
  # Aggregate country/region level data to get a single monthly average
  group_by(date) %>%
  summarise(
    mean_fuel_price = mean(mean_fuel_price, na.rm = TRUE),
    MSSMEB = mean(MSSMEB, na.rm = TRUE),
    MSSMEB.food.basket = mean(MSSMEB.food.basket, na.rm = TRUE)
  ) %>%
  arrange(date)

# ==========================================
# 2. Analyze: Pearson Correlation Matrix
# ==========================================
cor_data <- df_clean %>% select(-date)
cor_matrix <- cor.test(cor_data, use = "complete.obs", method = "pearson")

print("Pearson Correlation Matrix:")
print(cor_matrix)

# ==========================================
# 3. Deep Dive: Cross-Correlation Function (CCF)
# ==========================================
# Convert columns to time series objects
fuel_ts <- ts(df_clean$mean_fuel_price)
meb_ts <- ts(df_clean$MSSMEB)

# Plot CCF to check if fuel prices lead MEB
# Positive lags indicate fuel price changes precede MEB changes
ccf_result <- ccf(
  fuel_ts,
  meb_ts,
  lag.max = 6,
  main = "CCF: Mean Fuel Price vs MSSMEB",
  ylab = "Cross-Correlation"
)
print(ccf_result)
# ==========================================
# 4. Visualize
# ==========================================
# 4A. Faceted Line Chart showing Trends over time
df_long <- df_clean %>%
  pivot_longer(
    cols = c(mean_fuel_price, MSSMEB, MSSMEB.food.basket),
    names_to = "Variable",
    values_to = "Value"
  )

p_trends <- ggplot(df_long, aes(x = date, y = Value, color = Variable)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~Variable, scales = "free_y", ncol = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Time Series Trends: Fuel Prices vs MEB (Jan 2025 - Feb 2026)",
    x = "Date",
    y = "Value"
  )

print(p_trends)

# 4B. Correlation Heatmap
p_heatmap <- ggcorrplot(
  cor_matrix,
  hc.order = TRUE,
  type = "lower",
  lab = TRUE,
  lab_size = 5,
  colors = c("#6D9EC1", "white", "#E46726"),
  title = "Pearson Correlation Heatmap"
)

print(p_heatmap)
