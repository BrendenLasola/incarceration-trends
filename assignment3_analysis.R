incar_trends <- read.csv("incarceration_trends.csv")
library(dplyr)
library(ggplot)


#black jail pop per year in Atlanta
al_data <- incar_trends %>%
  filter(state == "AL") %>%
  filter(year >= 1995) %>%
  group_by(year) %>%
  summarize(black_jail_pop = sum(black_jail_pop,na.rm = TRUE),
            total_jail_pop = sum(total_jail_pop, na.rm = TRUE),
            white_jail_pop = sum(white_jail_pop, na.rm = TRUE),
            latinx_jail_pop = sum(latinx_jail_pop, na.rm = TRUE),
            other_race_jail_pop = sum(other_race_jail_pop,na.rm = TRUE))


#average black jail pop in latest year in the south 
south_jail_pop <- incar_trends %>%
  filter(region == "South") %>%
  filter(year == max(year)) %>%
  group_by(state) %>%
  summarize (black_jail_pop = mean(black_jail_pop, na.rm = TRUE))


#average black jail pop in the latest year in the west
west_jail_pop <- incar_trends %>%
  filter(region == "West") %>%
  filter(year == max(year)) %>%
  group_by(state) %>%
  summarize (black_jail_pop = mean(black_jail_pop, na.rm = TRUE))



#Line Chart
al_chart <- ggplot(data = al_data, aes( x = year)) + 
  geom_line(aes (y = black_jail_pop), color = "blue") +
  geom_line(aes (y = white_jail_pop), color = "red") +
  geom_line(aes (y = latinx_jail_pop), color = "green") +
  geom_line(aes (y = other_race_jail_pop), color = "magenta") +
  geom_line(aes (y = total_jail_pop), color = "black")

#Bar Chart
south_west <- ggplot(data = west_jail_pop) + geom_col(mapping = aes(x = black_jail_pop)) + geom_bar(data = south_jail_pop, mapping = aes(x = black_jail_pop))

south_west


