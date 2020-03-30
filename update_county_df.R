library(dplyr)
library(purrr)
library(stringr)
library(readr)

county_by_pop <- read_csv("county_merged_df.csv") %>% 
  mutate(place_full = paste0(place, ", ", state)) %>% 
  select(place_full, population) %>% 
  group_by(place_full) %>% slice(1) %>% ungroup

county_data <- read_csv("us-counties.csv") %>% mutate(place_full = paste0(county, ", ", state))

county_df_fin <- left_join(county_data, county_by_pop, by = "place_full")

write_csv(county_df_fin, "county_df_fin.csv")
