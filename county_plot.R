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

county_df_fin <- read_csv("county_df_fin.csv")


####################################
county_merged_df <- county_merged_df %>% mutate(cases_prop = 100000*(cases/population)) 

ugh <- county_merged_df %>% mutate(place_full = paste0(place, ", ", state)) %>% 
  filter(population > 100000) %>% 
  filter(date == "2020-03-20" | date == "2020-03-27") %>%
  group_by(place_full) %>% 
  mutate(diff = cases_prop[2] - cases_prop[1]) %>% slice(1) %>% arrange(desc(diff)) %>% .[1:50,] %>% select(place_full) 



county_merged_df %>% mutate(place_full = paste0(place, ", ", state)) %>% 
  filter(population > 100000) %>% 
  filter(date == "2020-03-20" | date == "2020-03-27") %>%
  group_by(place_full) %>% 
  mutate(diff = cases_prop[2] - cases_prop[1]) %>% slice(1) %>% arrange(desc(diff)) %>% select(place_full) %>% 
  mutate(thing = place_full == "Durham, North Carolina") %>% ungroup %>% mutate(idx = 1:nrow(.)) %>% 
  filter(thing == T)


county_merged_df %>% mutate(place_full = paste0(place, ", ", state)) %>% 
  filter(population > 100000) %>% 
  filter(date == "2020-03-20" | date == "2020-03-27") %>%
  group_by(place_full) %>% 
  mutate(diff = cases_prop[2] - cases_prop[1]) %>% slice(1) %>%
  filter(place_full == "Durham, North Carolina")

