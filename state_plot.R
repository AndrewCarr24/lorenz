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

state_data_fin <- read_csv("state_df_fin.csv")


# Filtering to just the 50 states 
state_data_fin <- state_data_fin %>% filter(!state %in% c("Virgin Islands", "District of Columbia", "Guam", "Puerto Rico"))

# Filtering to starting in March
state_data_ab <- state_data_fin %>% filter(date >= "2020-03-01")

# Filtering to just the 6 states with the biggest  per capita differences since March 23
state_data_top_states <- state_data_ab %>% 
  filter(date == "2020-03-23" | date == "2020-03-27") %>%
  group_by(state) %>% 
  mutate(diff = cases_prop[2] - cases_prop[1]) %>% slice(1) %>% arrange(desc(diff)) %>% .[1:6,] %>% select(state) %>%  
  inner_join(., state_data_ab, by = "state")

#  Filtering to cases since 03-09
state_data_top_states <- state_data_top_states %>% filter(date > "2020-03-09")

# Creating y position var for plot 
state_data_top_states <- state_data_top_states %>% group_by(date) %>% arrange(desc(cases_prop)) %>% 
  mutate(rank = ifelse(cases_prop == cases_prop[1], 210,
                       ifelse(cases_prop == cases_prop[2], 190,
                              ifelse(cases_prop == cases_prop[3], 170,
                                     ifelse(cases_prop == cases_prop[4], 150,
                                            ifelse(cases_prop == cases_prop[5], 130, 
                                                   ifelse(cases_prop == cases_prop[6], 110, -1))))))) %>% ungroup


## Plot 

p <- ggplot(state_data_top_states, aes(x = date, y = cases_prop, colour = state, group = state)) + 
  geom_line(size = .9) + 
  scale_color_manual(values=c("#DDA77B", "#A63A50", "#37123C", "#458548", "#4e46f0", "#A99F96")) +
  geom_text(aes(x = min(date) + 6, y = rank,
                label = state), hjust = 1, size = 5.5, family = 'Verdana', fontface = "bold") + 
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(size = 20, family = "Optima"),
        plot.subtitle = element_text(size = 14, family = "Optima"),
        axis.title = element_text(size = 14, family = 'Andale Mono')) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab("Cases Per 100,000 People") + 
  xlab("Date") +
  labs(subtitle = "For 6 states with highest rates") +
  ggtitle("COVID-19 Population-Adjusted Case Rate by State") + 
  transition_reveal(date, keep_last = FALSE)


# Saving animated plot as gif
anim <- animate(p, fps=9, nframes = 200)

anim_save("state_anim.gif", anim)
