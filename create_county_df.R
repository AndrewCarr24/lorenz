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

# Creating County-Population Data # 

# NYT County Data
us_counties <- read_csv("us-counties.csv")

# NYT State Data 
us_states <-   read_csv("us-states.csv")
state_populations <- read_html("https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population") %>% 
  html_table(., fill = T) %>% .[[1]] %>% .[, c("State", "Census population")]
state_populations$State[state_populations$State == "U.S. Virgin Islands"] <- "Virgin Islands"

state_data_fin <- left_join(us_states %>% rename(State = state), state_populations, by = "State") %>% 
  rename(population = `Census population`, state = State)


state_data_fin <- state_data_fin %>% mutate(population = str_replace_all(population, ",", "") %>% as.numeric) %>% 
  mutate(population_prop = cases/population)

# Filtering to just the 50 states 
state_data_fin <- state_data_fin %>% filter(!state %in% c("Virgin Islands", "District of Columbia", "Guam", "Puerto Rico"))

# Vector of state names
states <- state_data_fin %>% distinct(state) %>% unlist %>% unname

# Getting County Population Information From Wikipedia 
count <- 1
county_data <- list()

for(i in str_replace_all(states, " ", "_")[-c(6, 12, 14, 25, 27, 32, 35, 36, 39, 42, 45)]){
  
county_data[[count]] <- read_html(paste0("https://en.wikipedia.org/wiki/List_of_counties_in_", i)) %>% 
    html_table(., fill = T) %>% .[[2]] %>% .[,grepl("Population|County", colnames(.))] 

attr(county_data[[count]], "state") <- i

print(count)
count <- count + 1

Sys.sleep(1)
}

for(i in str_replace_all(states, " ", "_")[c(6, 25, 27, 36)]){
  
  county_data[[count]] <- read_html(paste0("https://en.wikipedia.org/wiki/List_of_counties_in_", i)) %>% 
    html_table(., fill = T) %>% .[[1]] %>% .[,grepl("Population|County", colnames(.))] 
  
  attr(county_data[[count]], "state") <- i
  
  print(count)
  count <- count + 1
  Sys.sleep(1)  
}

for(i in str_replace_all(states, " ", "_")[c(12, 14,  32, 39, 42)]){
  
county_data[[count]] <- read_html(paste0("https://en.wikipedia.org/wiki/List_of_counties_in_", i)) %>% 
  html_table(., fill = T) %>% .[[3]] %>% .[,grepl("Pop|County", colnames(.))] 
attr(county_data[[count]], "state") <- i

print(count)
count <- count + 1 
Sys.sleep(1)  
}

county_data[[count]] <- read_html(paste0("https://en.wikipedia.org/wiki/List_of_counties_in_", str_replace_all(states[35], " ", "_"))) %>% 
  html_table(., fill = T) %>% .[[3]] %>% .[,grepl("Pop|Parish", colnames(.))] 
attr(county_data[[count]], "state") <- "Louisiana"
count <- count + 1

county_data[[count]] <- read_html(paste0("https://en.wikipedia.org/wiki/List_of_counties_in_", str_replace_all(states[45], " ", "_"))) %>% 
  html_table(., fill = T) %>% .[[2]] %>% .[,grepl("Pop|Borough", colnames(.))] 
attr(county_data[[count]], "state") <- "Alaska"


## Fixing NJ 
county_data[[14]] <- read_html(paste0("https://en.wikipedia.org/wiki/List_of_counties_in_", "New_Jersey")) %>% 
  html_table(., fill = T) %>% .[[2]]  %>% .[,grepl("Pop|County", colnames(.))] 
attr(county_data[[14]] , 'state') <- "New_Jersey"

## Fixing NC 
county_data[[13]] <- read_html(paste0("https://en.wikipedia.org/wiki/List_of_counties_in_", "North_Carolina")) %>% 
  html_table(., fill = T) %>% .[[2]]  %>% .[,grepl("Pop|County", colnames(.))] 
attr(county_data[[13]] , 'state') <- "North_Carolina"


# Turning into a df 
county_df <- map(county_data, function(x){
  x <- cbind(x[1], rep(attr(x, "state"), nrow(x)), x[length(x)])
  colnames(x) <- c("place", "state",  "population")
  return(x)
}) %>% do.call("rbind", .) %>% 
  mutate(population = str_replace_all(population, ",", "") %>% as.numeric())


### Merging county population data to nyt data 
county_df <- county_df %>% mutate(state = str_replace_all(state, "_", " "))


county_merged <- list()

for(i in 1:50){
  
county_merged[[i]] <- county_df %>% mutate(place = str_replace(place, "[^ ]*$", "") %>% trimws) %>% filter(state == states[i]) %>%
  right_join(., us_counties %>% filter(state == states[i]), by = c("place" = "county"))

}
  
county_merged_df <- do.call("rbind", county_merged) %>% select(-`state.y`) %>% rename(state = `state.x`) %>% 
rbind(., us_counties %>% filter(!state %in% states) %>% rename(place = county) %>% mutate(population = NA))

county_merged_df <- as_tibble(county_merged_df)

######
county_merged_df <- county_merged_df %>% filter(place != "Unknown")



## Manually Adding data for 42 places - NYT data has cities instead of counties for some places  
missing_places <- county_merged_df %>% filter(is.na(population)) %>% distinct(place) %>% unname %>% unlist

county_merged_df[county_merged_df$place == missing_places[1], "population"] <- 883305
county_merged_df[county_merged_df$place == missing_places[1], "state"] <- "California"

county_merged_df[county_merged_df$place == missing_places[2], "population"] <- 8398748 
county_merged_df[county_merged_df$place == missing_places[2], "state"] <- "New York"

county_merged_df[county_merged_df$place == missing_places[3], "population"] <- 716492 
county_merged_df[county_merged_df$place == missing_places[3], "state"] <- "Colorado"

county_merged_df[county_merged_df$place == missing_places[4], "population"] <- 69267 
county_merged_df[county_merged_df$place == missing_places[4], "state"] <- "Colorado"

county_merged_df[county_merged_df$place == missing_places[5], "population"] <- 602495 
county_merged_df[county_merged_df$place == missing_places[5], "state"] <- "Maryland"

county_merged_df[county_merged_df$place == missing_places[6], "population"] <- 55414 
county_merged_df[county_merged_df$place == missing_places[6], "state"] <- "Nevada"

county_merged_df[county_merged_df$place == missing_places[7], "population"] <- 14307 
county_merged_df[county_merged_df$place == missing_places[7], "state"] <- "Kentucky"

county_merged_df[county_merged_df$place == missing_places[8], "population"] <- 302838 
county_merged_df[county_merged_df$place == missing_places[8], "state"] <- "Missouri"

county_merged_df[county_merged_df$place == missing_places[9] & is.na(county_merged_df$state), "population"] <- 70764 
county_merged_df[county_merged_df$place == missing_places[9], "state"] <- "Missouri"

county_merged_df[county_merged_df$place == missing_places[11], "population"] <- 2159159 
county_merged_df[county_merged_df$place == missing_places[11], "state"] <- "Missouri"

county_merged_df[county_merged_df$place == missing_places[13], "population"] <- 450189
county_merged_df[county_merged_df$place == missing_places[13], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[14], "population"] <- 160530
county_merged_df[county_merged_df$place == missing_places[14], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[15], "population"] <- 54033
county_merged_df[county_merged_df$place == missing_places[15], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[16], "population"] <- 48117
county_merged_df[county_merged_df$place == missing_places[16], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[17], "population"] <- 14896
county_merged_df[county_merged_df$place == missing_places[17], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[18], "population"] <- 228783
county_merged_df[county_merged_df$place == missing_places[18], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[19], "population"] <- 178626
county_merged_df[county_merged_df$place == missing_places[19], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[20], "population"] <- 244076
county_merged_df[county_merged_df$place == missing_places[20], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[21], "population"] <- 94632
county_merged_df[county_merged_df$place == missing_places[21], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[22], "population"] <- 91185
county_merged_df[county_merged_df$place == missing_places[22], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[23], "population"] <- 40693
county_merged_df[county_merged_df$place == missing_places[23], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[24], "population"] <- 242634
county_merged_df[county_merged_df$place == missing_places[24], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[25], "population"] <- 24574
county_merged_df[county_merged_df$place == missing_places[25], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[26], "population"] <- 29144
county_merged_df[county_merged_df$place == missing_places[26], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[27], "population"] <- 41641
county_merged_df[county_merged_df$place == missing_places[27], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[28], "population"] <- 134313
county_merged_df[county_merged_df$place == missing_places[28], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[29], "population"] <- 82126
county_merged_df[county_merged_df$place == missing_places[29], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[30], "population"] <- 12190
county_merged_df[county_merged_df$place == missing_places[30], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[31], "population"] <- 18339
county_merged_df[county_merged_df$place == missing_places[31], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[32], "population"] <- 16482
county_merged_df[county_merged_df$place == missing_places[32], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[33], "population"] <- 	6423
county_merged_df[county_merged_df$place == missing_places[33], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[34], "population"] <- 	99920
county_merged_df[county_merged_df$place == missing_places[34], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[35], "population"] <- 	22596
county_merged_df[county_merged_df$place == missing_places[35], "state"] <- "Virginia"

county_merged_df[county_merged_df$place == missing_places[36], "population"] <- 	27436
county_merged_df[county_merged_df$place == missing_places[36], "state"] <- "Louisiana"

county_merged_df[county_merged_df$place == missing_places[37], "population"] <- 	291538
county_merged_df[county_merged_df$place == missing_places[37], "state"] <- "Alaska"

county_merged_df[county_merged_df$place == missing_places[38], "population"] <- 	98971
county_merged_df[county_merged_df$place == missing_places[38], "state"] <- "Alaska"

county_merged_df[county_merged_df$place == missing_places[39], "population"] <- 	8289
county_merged_df[county_merged_df$place == missing_places[39], "state"] <- "Alaska"

county_merged_df[county_merged_df$place == missing_places[40], "population"] <- 	58533
county_merged_df[county_merged_df$place == missing_places[40], "state"] <- "Alaska"

county_merged_df[county_merged_df$place == missing_places[41], "population"] <- 	32113
county_merged_df[county_merged_df$place == missing_places[41], "state"] <- "Alaska"

county_merged_df[county_merged_df$place == missing_places[42], "population"] <- 	107610
county_merged_df[county_merged_df$place == missing_places[42], "state"] <- "Alaska"

county_merged_df[county_merged_df$place == missing_places[43], "population"] <- 	705749
county_merged_df[county_merged_df$place == missing_places[43], "state"] <- "District of Columbia"



write_csv(county_merged_df, "county_merged_df.csv")
















