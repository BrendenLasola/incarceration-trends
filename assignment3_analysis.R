incar_trends <- read.csv("incarceration_trends.csv")

#black jail pop per year in Atlanta
blck_jail_pop_AL <- incar_trends %>%
  filter(state == "AL") %>%
  select(year, black_jail_pop) %>%
  group_by(year) %>%
  summarize(black_jail_pop = sum(black_jail_pop,na.rm = TRUE))

View(blck_jail_pop_AL)

#total Jail pop per year in Atlanta
total_jail_pop_AL <- incar_trends %>%
  filter(state == "AL") %>%
  select(year,total_jail_pop) %>%
  group_by(year) %>%
  summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  
View(total_jail_pop_AL)


#average black jail pop in latest year in the south 
south_jail_pop <- incar_trends %>%
  filter(region == "South") %>%
  filter(year == max(year)) %>%
  group_by(state) %>%
  summarize (black_jail_pop = mean(black_jail_pop, na.rm = TRUE))

View(south_jail_pop)

#average black jail pop in the latest year in the west
west_jail_pop <- incar_trends %>%
  filter(region == "West") %>%
  filter(year == max(year)) %>%
  group_by(state) %>%
  summarize (black_jail_pop = mean(black_jail_pop, na.rm = TRUE))

View(west_jail_pop)

#ratio of black jail pop increasing vs total jail pop in North America
black_total_ratio <- incar_trends %>%
  filter(year >= 1985) %>%
  group_by(year) %>%
  summarize (black_ratio = sum(black_jail_pop,na.rm = TRUE)/ sum(total_jail_pop, na.rm = TRUE))

View(black_total_ratio)

