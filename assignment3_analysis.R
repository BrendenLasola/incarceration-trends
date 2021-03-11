library(dplyr)
library(ggplot2)
library(tidyverse)
library(styler)
library(maps)

incar_trends <- read.csv("incarceration_trends.csv")

#black jail pop per year in Alabama
al_data <- incar_trends %>%
  filter(state == "AL") %>%
  filter(year >= 1995) %>%
  group_by(year) %>%
  summarize(black_jail_pop = sum(black_jail_pop,na.rm = TRUE),
            total_jail_pop = sum(total_jail_pop, na.rm = TRUE),
            white_jail_pop = sum(white_jail_pop, na.rm = TRUE),
            latinx_jail_pop = sum(latinx_jail_pop, na.rm = TRUE),
            other_race_jail_pop = sum(other_race_jail_pop,na.rm = TRUE))


#average black jail pop in latest year in the mid-west
midwest_jail_pop <- incar_trends %>%
  filter(region == "Midwest") %>%
  filter(year == max(year)) %>%
  group_by(state) %>%
  summarize (black_jail_pop = mean(black_jail_pop, na.rm = TRUE))

#midwest
midwest_pop <- incar_trends %>%
  filter(year == max(year)) %>%
  filter(region == "Midwest") %>%
  group_by(state) %>%
  summarize( black_ratio = sum(black_jail_pop, na.rm = TRUE)/sum(black_pop_15to64, na.rm = TRUE),
             white_ratio = sum(white_jail_pop, na.rm = TRUE)/sum(white_pop_15to64, na.rm = TRUE))

#total white pop 15to64 in midwest
white_pop <- incar_trends %>%
  filter(year == max(year)) %>%
  filter(region == "Midwest") %>%
  group_by(state) %>%
  summarize(white_pop_15to64 = sum(white_pop_15to64, na.rm = TRUE))


#total black pop 15to64 in midwest
black_pop <- incar_trends %>%
  filter(year == max(year)) %>%
  filter(region == "Midwest") %>%
  group_by(state) %>%
  summarize(black_pop_15to64 = sum(black_pop_15to64, na.rm = TRUE))


#scatter chart
colors <- c("Black Ratio" = "red", "White Ratio" = "blue")
mw_chart <- ggplot(data = midwest_pop, aes(x = state)) +
  geom_point(aes (y = black_ratio, color = "Black Ratio"), size = 2) +
  geom_point(aes (y = white_ratio, color = "White Ratio"), size = 2) + 
  labs (title = "Comparing the percentage of people in jail vs. not in jail for black and white people in the midwest",
        x = "States ", 
        y = "Percentage of people in jail vs not in jail ",
        color = "Legend") +
  scale_color_manual(values = colors)


#Line Chart
colours <- c("Black in Jail" = "magenta", "White in Jail" = "green", "Latinx in Jail" = "black",
             "Other in Jail" = "yellow", "Total in Jail" = "orange")

al_chart <- ggplot(data = al_data, aes(x = year)) + 
  geom_line(aes (y = black_jail_pop, color = "Black in Jail"), size = 1.5) +
  geom_line(aes (y = white_jail_pop, color = "White in Jail"), size = 1.5) +
  geom_line(aes (y = latinx_jail_pop, color = "Latinx in Jail"), size = 1.5) +
  geom_line(aes (y = other_race_jail_pop, color = "Other in Jail"), size = 1.5) +
  geom_line(aes (y = total_jail_pop, color = "Total in Jail"), size = 1.5) + 
  labs (title = "Line chart of jailed individuals in alabama by their race",
        x = "Year", 
        y = "# of individuals in jail ",
        color = "Legend") +
  scale_color_manual(values = colours)


