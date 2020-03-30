library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(rvest)
library(tibble)
library(ggplot2)
library(gganimate)
library(transformr)
library(tweenr)

# NYT state data 
us_states <-   read_csv("us-states.csv")

# Wikipedia population data
state_populations <- read_html("https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population") %>% 
  html_table(., fill = T) %>% .[[1]] %>% .[, c("State", "Census population")]
state_populations$State[state_populations$State == "U.S. Virgin Islands"] <- "Virgin Islands"

# Merging data 
state_data_fin <- left_join(us_states %>% rename(State = state), state_populations, by = "State") %>% 
  rename(population = `Census population`, state = State)

state_data_fin <- state_data_fin %>% mutate(population = str_replace_all(population, ",", "") %>% as.numeric) %>% 
  mutate(cases_prop = 100000*(cases/population))

write_csv(state_data_fin, "state_df_fin.csv")
