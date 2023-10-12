library(tidyverse)

results <- read.csv("data/results.csv")
races <- read.csv("data/races.csv")
lap_time <- rad.csv("data/lap_times.csv")

lap_time_and_race <- lap_times %>% 
  inner_join(races, by = "raceId") %>% 
  select(raceId, circuitId, year, driverId, lap, position, milliseconds)

monza_lap_time_and_race <- lap_time_and_race %>% filter(circuitId == 14) %>% arrange(year)
length(unique(monza_lap_time_and_race$raceId))

year_quickest_lap <- monza_lap_time_and_race %>% group_by(year) %>% summarise(quickest_lap = min(milliseconds))

year_quickest_lap <- year_quickest_lap %>% mutate(quickest_lap = quickest_lap/1000)

# 92-2005 v10 3-3.5
# 2006 and 2007 v6 with 07 frozen 2.4l
# in 06 and 07 teams were allowed to use v10 with rev limitter
# 09 kers came in
# 14 1.6 v6

ggplot(year_quickest_lap, aes(x = as.factor(year)))+
  geom_line(aes(y = quickest_lap), group = 1)+
  geom_point(aes(y = quickest_lap))+
  labs(x = "Year", y = "Quickest lap in Monza")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
