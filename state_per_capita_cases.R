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

# Preparing State Data 

us_states <-   read_csv("covid-19-data/us-states.csv")

state_populations <- read_html("https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population") %>% 
  html_table(., fill = T) %>% .[[1]] %>% .[, c("State", "Census population")]
state_populations$State[state_populations$State == "U.S. Virgin Islands"] <- "Virgin Islands"

state_data_fin <- left_join(us_states %>% rename(State = state), state_populations, by = "State") %>% 
  rename(population = `Census population`, state = State)

# 

state_data_fin <- state_data_fin %>% mutate(population = str_replace_all(population, ",", "") %>% as.numeric) %>% 
  mutate(population_prop = cases/population)

# Filtering to just the 50 states 
state_data_fin <- state_data_fin %>% filter(!state %in% c("Virgin Islands", "District of Columbia", "Guam", "Puerto Rico"))

# Filtering to starting in March
state_data_ab <- state_data_fin %>% filter(date >= "2020-03-01")

# Filtering to just the 10 states with the biggest  per capita differences since March 17
state_data_top_states <- state_data_ab %>% 
  filter(date == "2020-03-17" | date == "2020-03-27") %>%
  group_by(state) %>% 
  mutate(diff = population_prop[2] - population_prop[1]) %>% slice(1) %>% arrange(desc(diff)) %>% .[1:6,] %>% select(state) %>%  
  inner_join(., state_data_ab, by = "state")

state_data_top_states <- state_data_top_states %>% filter(date > "2020-03-09")





state_data_top_states <- state_data_top_states %>% group_by(date) %>% arrange(desc(population_prop)) %>% 
  mutate(rank = ifelse(population_prop == population_prop[1], .002,
             ifelse(population_prop == population_prop[2], .0019,
                    ifelse(population_prop == population_prop[3], .0018,
                           ifelse(population_prop == population_prop[4], .0017,
                                  ifelse(population_prop == population_prop[5], .0016, 
                                         ifelse(population_prop == population_prop[6], .0015, -1))))))) %>%
                                                                     ungroup

p <- ggplot(state_data_top_states, aes(x = date, y = population_prop, colour = state, group = state)) + 
  geom_line(size = .9) + 
  scale_color_manual(values=c("#DDA77B", "#A63A50", "#37123C", "#458548", "#4e46f0", "#A99F96")) +
  geom_text(aes(x = min(date) + 5, y = rank,
                label = state), hjust = 1, size = 4, family = 'Andale Mono') + 
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(family = "Optima"), axis.title = element_text(family = 'Andale Mono')) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylim(0, .0025) +
  ylab("Cases Per Capita") + 
  xlab("Date") +
  ggtitle("COVID-19 Population-Adjusted Case Rate by State") + 
  transition_reveal(date, keep_last = FALSE)



animate(p, fps=9, nframes = 200)
