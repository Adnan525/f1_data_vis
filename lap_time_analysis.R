library(tidyverse)

results <- read.csv("data/results.csv")
races <- read.csv("data/races.csv")
lap_times <- read.csv("data/lap_times.csv")

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


# differnt colour got it from chatgpt
# Create a data frame to define the color groups
color_data <- data.frame(
  engine_configuration = c("1996-2005", "2005-2014", "2014-2022"),
  color = c("red", "blue", "green")
)

# Add a 'year_group' column to your existing data frame
year_quickest_lap$engine_configuration <- cut(
  year_quickest_lap$year,
  breaks = c(1996, 2005, 2013, 2022),
  labels = c("V10 3.5L", "V6 2.4L", "V6 1.6L Turbo Hybrid"),
  include.lowest = TRUE
)
ggplot(year_quickest_lap, aes(x = as.factor(year), y = quickest_lap, color = engine_configuration)) +
  geom_line(group = 1) +
  geom_point() +
  labs(x = "Year", y = "Quickest lap in Monza") +
  scale_color_manual(values = color_data$color) +  # Assign colors manually
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

