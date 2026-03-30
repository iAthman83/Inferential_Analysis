df <- read.xlsx(
  "./output/JMMI_fuel_prices_gasoline.xlsx"
)

county_vs_fuel <- df %>%
  select(County, MSSMEB, MSSMEB.food.basket, mean_fuel_price) %>%
  filter(County == "AweilCentre")
