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

# Cases per 100000 people 
county_merged_df <- county_merged_df %>% mutate(cases_prop = 100000*(cases/population)) 

# Filtering to just the 50 states 
county_merged_df <- county_merged_df %>% filter(!state %in% c("Virgin Islands", "District of Columbia", "Guam", "Puerto Rico"))

# Filtering to starting in March
county_merged_df <- county_merged_df %>% filter(date >= "2020-03-01")

# Creating places_full var
county_merged_df <- county_merged_df %>% mutate(places_full = ifelse(state == "Alaska", paste0(place, " Borough, ", state), 
                                                 ifelse(state == "Louisiana", paste0(place, " Parish, ", state),
                                                        paste0(place, " County, ", state)))) 


# Filtering to just the 6 counties with the biggest per capita differences since March 23 (among counties with > 100000 people)
county_merged_df_top_places <- county_merged_df %>% 
  filter(population > 100000) %>% 
  filter(date == "2020-03-23" | date == "2020-03-27") %>%
  group_by(places_full) %>% 
  mutate(diff = cases_prop[2] - cases_prop[1]) %>% slice(1) %>% arrange(desc(diff)) %>% .[1:6,] %>% select(places_full) %>%  
  inner_join(., county_merged_df, by = "places_full")


#  Filtering to cases since 03-12
county_merged_df_top_places <- county_merged_df_top_places %>% filter(date > "2020-03-12")


# Creating y position var for plot 
county_merged_df_top_places <- county_merged_df_top_places %>% group_by(date) %>% arrange(desc(cases_prop)) %>% 
  mutate(rank = ifelse(cases_prop == cases_prop[1], 640,
                       ifelse(cases_prop == cases_prop[2], 600,
                              ifelse(cases_prop == cases_prop[3], 560,
                                     ifelse(cases_prop == cases_prop[4], 520,
                                            ifelse(cases_prop == cases_prop[5], 480, 
                                                   ifelse(cases_prop == cases_prop[6], 440, -1))))))) %>% ungroup


p <- ggplot(county_merged_df_top_places, aes(x = date, y = cases_prop, colour = places_full, group = places_full)) + 
  geom_line(size = .9) + 
  scale_color_manual(values=c("#DDA77B", "#A63A50", "#37123C", "#458548", "#4e46f0", "#A99F96")) +
  geom_text(aes(x = min(date) + 9, y = rank,
                label = places_full), hjust = 1, size = 4, family = 'Verdana', fontface = "bold") + 
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(size = 16, family = "Optima"),
        plot.subtitle = element_text(size = 14, family = "Optima"),
        axis.title = element_text(size = 14, family = 'Andale Mono')) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab("Cases Per 100,000 People") + 
  xlab("Date") +
  labs(subtitle = "For 6 counties with highest rates") +
  ggtitle("COVID-19 Population-Adjusted Case Rate by County") + 
  transition_reveal(date, keep_last = FALSE)

anim <- animate(p, fps=9, nframes = 200)

anim_save("county_anim.gif", anim)

# county_merged_df %>% 
#   filter(population > 10000) %>% 
#   filter(date == "2020-03-23" | date == "2020-03-27") %>%
#   group_by(places_full) %>% 
#   mutate(diff = cases_prop[2] - cases_prop[1]) %>% slice(1) %>% arrange(desc(diff)) %>% .[1:10,]


