rm(list = ls())
# Install missing packages if needed
# install.packages(c("dplyr", "lubridate", "ggplot2", "tidyr", "purrr", "broom"))

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(purrr)
library(broom)

# ==========================================
# 1. Pre-process: Panel Data Structuring
# ==========================================
# Load the dataset
df <- read.xlsx(
  "./output/JMMI_fuel_prices_gasoline.xlsx"
)

# Format the dataset into a clean panel structure (Country-Date)
df_county <- df %>%
  select(date, county = County, mean_fuel_price, MSSMEB, MSSMEB.food.basket) %>%
  mutate(
    date = dmy(date),
    MSSMEB = as.numeric(MSSMEB),
    MSSMEB.food.basket = as.numeric(MSSMEB.food.basket)
  ) %>%
  filter(!is.na(date) & !is.na(county) & county != "") %>%
  # Average duplicate entries for the same county and month
  group_by(county, date) %>%
  summarise(
    mean_fuel_price = mean(mean_fuel_price, na.rm = TRUE),
    MSSMEB = mean(MSSMEB, na.rm = TRUE),
    MSSMEB.food.basket = mean(MSSMEB.food.basket, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(county, date)

# ==========================================
# 2. Analyze: County-Level Correlations (Using purrr & broom)
# ==========================================
# Filter out counties with too few observations (need at least 4 for basic correlation)
valid_counties <- df_county %>%
  group_by(county) %>%
  filter(sum(!is.na(mean_fuel_price) & !is.na(MSSMEB.food.basket)) >= 4) %>%
  ungroup()

invalid_counties <- df_county %>%
  group_by(county) %>%
  filter(sum(!is.na(mean_fuel_price) & !is.na(MSSMEB.food.basket)) < 4) %>%
  ungroup()

write.xlsx(invalid_counties, "./output/MSSMEB_fb_invalid_counties.xlsx")

# Calculate Pearson correlation for each valid county
county_correlations <- valid_counties %>%
  nest(data = c(date, mean_fuel_price, MSSMEB, MSSMEB.food.basket)) %>%
  mutate(
    # Concurrent correlation
    cor_test_mssmeb = map(
      data,
      ~ cor.test(
        .x$mean_fuel_price,
        .x$MSSMEB.food.basket,
        method = "pearson",
        exact = FALSE
      )
    ),
    cor_mssmeb_tidy = map(cor_test_mssmeb, tidy)
  ) %>%
  unnest(cor_mssmeb_tidy) %>%
  select(county, estimate, p.value) %>%
  rename(correlation = estimate) %>%
  arrange(desc(correlation))

# Classify the strength of correlation for visualization
county_correlations <- county_correlations %>%
  mutate(
    Strength = case_when(
      correlation >= 0.6 ~ "Strong Positive",
      correlation >= 0.3 ~ "Moderate Positive",
      correlation >= -0.3 ~ "Weak/None",
      TRUE ~ "Negative"
    )
  )

print("Top 5 Counties with highest Fuel/MEB correlation:")
print(head(county_correlations, 5))

# ==========================================
# 3. Visualize: County Divergence
# ==========================================
# Plot 1: Bar chart of correlation coefficients by county
p_corr_bar <- ggplot(
  county_correlations,
  aes(x = reorder(county, correlation), y = correlation, fill = Strength)
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(
    values = c(
      "Strong Positive" = "#d73027",
      "Moderate Positive" = "#fc8d59",
      "Weak/None" = "#e0f3f8",
      "Negative" = "#4575b4"
    )
  ) +
  labs(
    # title = "Fuel Pass-Through Effect by County",
    title = "Correlation by county between Petrol-Gasolin Prices and Total MSSMEB Food Basket",
    x = "County",
    y = "Correlation Coefficient (r)"
  ) +
  theme(
    # Increase Title size and make it bold
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),

    # Increase X and Y axis labels (the "County" and "Correlation" text)
    axis.title = element_text(size = 14),

    # Increase the actual names of the counties on the axis
    axis.text.x = element_text(
      size = 12,
      face = "bold",
      angle = 45,
      vjust = 1,
      hjust = 1
    ),
    axis.text.y = element_text(size = 12, face = "bold")
  )

print(p_corr_bar)

# Format the dataset into a clean panel structure (State-Date)
df_state <- df %>%
  select(date, state = State, mean_fuel_price, MSSMEB, MSSMEB.food.basket) %>%
  mutate(
    date = dmy(date),
    MSSMEB = as.numeric(MSSMEB),
    MSSMEB.food.basket = as.numeric(MSSMEB.food.basket)
  ) %>%
  filter(!is.na(date) & !is.na(state) & state != "") %>%
  # Average duplicate entries for the same state and month
  group_by(state, date) %>%
  summarise(
    mean_fuel_price = mean(mean_fuel_price, na.rm = TRUE),
    MSSMEB = mean(MSSMEB, na.rm = TRUE),
    MSSMEB.food.basket = mean(MSSMEB.food.basket, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(state, date)
# ==========================================
# 2. Analyze: State-Level Correlations (Using purrr & broom)
# ==========================================
# Filter out states with too few observations (need at least 4 for basic correlation)
valid_states <- df_state %>%
  group_by(state) %>%
  filter(sum(!is.na(mean_fuel_price) & !is.na(MSSMEB.food.basket)) >= 4) %>%
  ungroup()

invalid_states <- df_state %>%
  group_by(state) %>%
  filter(sum(!is.na(mean_fuel_price) & !is.na(MSSMEB.food.basket)) < 4) %>%
  ungroup()

write.xlsx(invalid_states, "./output/MSSMEB_fb_invalid_states.xlsx")

# Calculate Pearson correlation for each valid state
state_correlations <- valid_states %>%
  nest(data = c(date, mean_fuel_price, MSSMEB, MSSMEB.food.basket)) %>%
  mutate(
    # Concurrent correlation
    cor_test_mssmeb = map(
      data,
      ~ cor.test(
        .x$mean_fuel_price,
        .x$MSSMEB.food.basket,
        method = "pearson",
        exact = FALSE
      )
    ),
    cor_mssmeb_tidy = map(cor_test_mssmeb, tidy)
  ) %>%
  unnest(cor_mssmeb_tidy) %>%
  select(state, estimate, p.value) %>%
  rename(correlation = estimate) %>%
  arrange(desc(correlation))

# Classify the strength of correlation for visualization
state_correlations <- state_correlations %>%
  mutate(
    Strength = case_when(
      correlation >= 0.6 ~ "Strong Positive",
      correlation >= 0.3 ~ "Moderate Positive",
      correlation >= -0.3 ~ "Weak/None",
      TRUE ~ "Negative"
    )
  )

print("Top 5 Counties with highest Fuel/MEB correlation:")
print(head(state_correlations, 2))

# ==========================================
# 3. Visualize: County Divergence
# ==========================================
# Plot 1: Bar chart of correlation coefficients by state
p_corr_bar_state <- ggplot(
  state_correlations,
  aes(x = reorder(state, correlation), y = correlation, fill = Strength)
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(
    values = c(
      "Strong Positive" = "#d73027",
      "Moderate Positive" = "#fc8d59",
      "Weak/None" = "#e0f3f8",
      "Negative" = "#4575b4"
    )
  ) +
  labs(
    # title = "Fuel Pass-Through Effect by State",
    title = "Correlation by state between Petrol-Gasolin Prices and Total MSSMEB Food Basket",
    x = "State",
    y = "Correlation Coefficient (r)"
  ) +
  theme(
    # Increase Title size and make it bold
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),

    # Increase X and Y axis labels (the "County" and "Correlation" text)
    axis.title = element_text(size = 14),

    # Increase the actual names of the counties on the axis
    axis.text.x = element_text(
      size = 12,
      face = "bold",
      angle = 45,
      vjust = 1,
      hjust = 1
    ),
    axis.text.y = element_text(size = 12, face = "bold")
  )

print(p_corr_bar_state)

# ==========================================
# 4. Deep Dive: Trend Lines for Top & Bottom Counties
# ==========================================
# Extract top 2 and bottom 2 counties to visualize why the coefficients differ so vastly
extreme_counties <- c(
  head(county_correlations$county, 2),
  tail(county_correlations$county, 2)
)

df_extremes <- valid_counties %>%
  filter(county %in% extreme_counties) %>%
  # Normalize the data (Z-score) so we can plot Fuel and MEB on the same axis scale
  group_by(county) %>%
  mutate(
    Fuel_Index = scale(mean_fuel_price),
    MEB_Index = scale(MSSMEB.food.basket)
  ) %>%
  select(county, date, Fuel_Index, MEB_Index) %>%
  pivot_longer(
    cols = c(Fuel_Index, MEB_Index),
    names_to = "Indicator",
    values_to = "Z_Score"
  )

# Plot 2: Time series comparison of the extremes
p_extreme_trends <- ggplot(
  df_extremes,
  aes(x = date, y = Z_Score, color = Indicator)
) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~county, scales = "fixed", ncol = 2) +
  theme_bw() +
  scale_color_manual(
    values = c("Fuel_Index" = "#1f78b4", "MEB_Index" = "#e31a1c"),
    labels = c("Normalized Fuel Price", "Normalized MEB")
  ) +
  labs(
    title = "Why Does Correlation Vary? Normalized Trends for Selected Counties",
    x = "Date",
    y = "Standardized Value (Z-Score)"
  ) +
  theme(legend.position = "bottom", legend.title = element_blank())

print(p_extreme_trends)
