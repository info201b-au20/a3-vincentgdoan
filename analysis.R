# ASSIGNMENT 3

# Load necessary packages.
library(tidyverse)
library(ggplot2)
library(styler)
library(lintr)
options(dplyr.summarise.inform = FALSE)

# Load data in question.
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Section: Summary Information

# What is the average value of Black populations in jail across all the counties
# (in the most recent year)?
avg_value_black <- incarceration %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  summarize(mean_jail_pop = mean(black_jail_pop, na.rm = TRUE))
  

# What is the highest value of Black populations in jail across all counties (in
# the most recent year)?
highest_value_black <- incarceration %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  summarize(max_jail_pop = max(black_jail_pop, na.rm = TRUE))

# What county and state is the Black populations in jail the highest (in the 
# most recent year)?
county_state_highest_black <- incarceration %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>%
  mutate(location = str_c(county_name, state, sep = ", ")) %>% 
  pull(location)

# What state is the Black populations in jail the lowest (in the most recent 
# year)?
state_lowest_black <- incarceration %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  group_by(state) %>% 
  summarize(total_state_pop = sum(black_jail_pop, na.rm = TRUE)) %>% 
  filter(total_state_pop == min(total_state_pop, na.rm = TRUE)) %>%
  pull(state)

# How much has the Black populations in jail across all counties and states 
# changed over the last 5 years?
over_time_value <- incarceration %>% 
  group_by(year) %>% 
  summarize(total_pop_year = sum(black_jail_pop, na.rm = TRUE))

difference_five_year <- 
  pull(filter(over_time_value, year == max(year, na.rm = TRUE)), total_pop_year) - pull(filter(over_time_value, year == (max(year, na.rm = TRUE)) - 5), total_pop_year)

change_five_year <- abs(difference_five_year)

# Trends Over Time Chart(s)

# Creates variable which calculates the sum of jail populations by year, and
# calculates the proportion by race.
trend_chart_data <- incarceration %>% 
  group_by(year) %>% 
  summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE),
            total_black_pop = sum(black_jail_pop, na.rm = TRUE),
            total_aapi_pop = sum(aapi_jail_pop, na.rm = TRUE),
            total_latinx_pop = sum(latinx_jail_pop, na.rm = TRUE),
            total_native_pop = sum(native_jail_pop, na.rm = TRUE),
            total_white_pop = sum(white_jail_pop, na.rm = TRUE),
            ) %>% 
  mutate(proportion_black = total_black_pop / total_jail_pop,
         proportion_aapi = total_aapi_pop / total_jail_pop,
         proportion_latinx = total_latinx_pop / total_jail_pop,
         proportion_native = total_native_pop / total_jail_pop,
         proportion_white = total_white_pop / total_jail_pop)

# Creates line graph for proportion of jail populations by race.
trend_chart <- ggplot(data = trend_chart_data) +
  geom_line(mapping = aes(x = year,
                          y = proportion_black,
                          color = "Black")) +
  geom_line(mapping = aes(x = year,
                          y = proportion_aapi,
                          color = "Asian/Pacific Islander")) +
  geom_line(mapping = aes(x = year,
                          y = proportion_latinx,
                          color = "LatinX")) +
  geom_line(mapping = aes(x = year,
                          y = proportion_native,
                          color = "Native")) +
  geom_line(mapping = aes(x = year,
                          y = proportion_white,
                          color = "White")) +
  ggtitle("Proportion of U.S Jail Population over Time by Race") +
  xlab("Time (Years)") +
  ylab("Proportion") +
  labs(colour = "Race/Ethnicity") +
  ylim(0, 1)

# Creates variable which calculates the sum of U.S. populations by year, and
# calculates the proportion by race.

trend_chart_data_part_two <- incarceration %>% 
  group_by(year) %>% 
  summarize(total_pop = sum(total_pop_15to64, na.rm = TRUE),
            total_black_pop = sum(black_pop_15to64, na.rm = TRUE),
            total_aapi_pop = sum(aapi_pop_15to64, na.rm = TRUE),
            total_latinx_pop = sum(latinx_pop_15to64, na.rm = TRUE),
            total_native_pop = sum(native_pop_15to64, na.rm = TRUE),
            total_white_pop = sum(white_pop_15to64, na.rm = TRUE),
  ) %>% 
  mutate(proportion_black = total_black_pop / total_pop,
         proportion_aapi = total_aapi_pop / total_pop,
         proportion_latinx = total_latinx_pop / total_pop,
         proportion_native = total_native_pop / total_pop,
         proportion_white = total_white_pop / total_pop)

# Creates line graph for proportion of U.S. populations by race.
trend_chart_part_two <- ggplot(data = trend_chart_data_part_two) +
  geom_line(mapping = aes(x = year,
                          y = proportion_black,
                          color = "Black")) +
  geom_line(mapping = aes(x = year,
                          y = proportion_aapi,
                          color = "Asian/Pacific Islander")) +
  geom_line(mapping = aes(x = year,
                          y = proportion_latinx,
                          color = "LatinX")) +
  geom_line(mapping = aes(x = year,
                          y = proportion_native,
                          color = "Native")) +
  geom_line(mapping = aes(x = year,
                          y = proportion_white,
                          color = "White")) +
  ggtitle("Proportion of U.S Population over Time by Race") +
  xlab("Time (Years)") +
  ylab("Proportion") +
  labs(colour = "Race/Ethnicity") +
  ylim(0, 1)

# Variable Comparison Chart

# Create variable which holds a data frame that has the prison admission rate 
# over time for Black individuals, and the prison population rate of Black 
# individuals.
variable_comparison <- incarceration %>% 
  group_by(year) %>% 
  summarize(prison_admission_rate = sum(black_prison_adm_rate, na.rm = TRUE),
            prison_pop_rate = sum(black_prison_pop_rate, na.rm = TRUE)) %>% 
  filter(prison_admission_rate != 0)

# Create a scatterplot which compares the prison admission rate for Blacks to
# the prison population rate for Blacks.
variable_comparison_scatterplot <- ggplot(data = variable_comparison) +
  geom_point(mapping = aes(x = prison_admission_rate,
                          y = prison_pop_rate)) +
  ggtitle("Association of Prison Admission Rates and Prison Population Rates for Blacks") +
  xlab("Admission Rate") +
  ylab("Population Rate")

# Map "Chart"

# Create dataframe for map.

# Created dataframe to translate state name to abbreviation.
state_abb <- data.frame(state.abb, state.name) %>%
  rename(stateabb = state.abb, state = state.name) %>%
  mutate(state = tolower(state))

# Created dataframe for map data.
map_data_jail <- incarceration %>%
  filter(year == max(year, na.rm = TRUE)) %>% 
  group_by(state) %>% 
  summarize(total_black_pop = sum(black_jail_pop, na.rm = TRUE),
            total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>% 
  mutate(proportion_black_jail = total_black_pop / total_jail_pop) %>%
  left_join(state_abb, by = c("state" = "stateabb"))


# Created shapefile for U.S states and proportion of Jail population.
state_shape <- map_data("state") %>%
  rename(state = region) %>%
  left_join(map_data_jail, by = c("state" = "state.y"))

# Created map.
map <- ggplot(state_shape) + 
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = proportion_black_jail),
    color = "white",
    size = .1
  ) +
  scale_fill_continuous(low = "#132B43", high = "Blue") +
  labs(fill = "Proportion") +
  ggtitle("Proportion of Black Individuals in U.S. Jail Populations") 
            