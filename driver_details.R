drivers <- read.csv("data/drivers.csv")
drivers <- drivers %>% mutate(driver_name = paste(forename, surname))
driver_standings <- read.csv("data/driver_standings.csv")
races <- read.csv("data/races.csv")
races <- races %>% rename(race_name = name)
results <- read.csv("data/results.csv")
constructors <- read.csv("data/constructors.csv")
constructors <- constructors %>% rename(constructor_name = name)

# driver name
# constructor
# still driving?
# total wins
# total podiums
# total points

target <- driver_standings %>% 
  # select(-driverStandingsId) %>% 
  inner_join(drivers, by = "driverId") %>% 
  select(driver_name, names(driver_standings))
# DRIVER STANDINGS POSITION IS NOT SAME AS RESULTS POSITION
target <- target %>% 
  inner_join(results, by = c("raceId", "driverId")) %>% 
  rename(points = points.x, position = positionOrder, positionText = positionText.y)%>% 
  select(constructorId, names(target))

target <- target %>% 
  inner_join(constructors, by = "constructorId") %>% 
  select(constructor_name, names(target))

target <- target %>% 
  inner_join(races, by = "raceId") %>% 
  select(year, race_name, names(target))


# test
# we want to know about Valtteri Bottas
target_driver <- "Valtteri Bottas"
current_year <- 2023

target_driver_df <- target %>% filter(driver_name == target_driver) %>% arrange(year)
is_currently_driving <- ifelse(current_year %in% unique(target_driver_df$year), TRUE, FALSE)
current_team <- ifelse(is_currently_driving,
                       unique(target_driver_df$constructor_name)[length(unique(target_driver_df$constructor_name))],
                       NA)
all_teams <- unique(target_driver_df$constructor_name)
total_win <- target_driver_df %>% 
  group_by(year) %>% 
  summarise(win_by_year = max(wins)) %>% 
  summarise(total_win = sum(win_by_year)) %>% 
  pull(total_win)
team_wise_win <- target_driver_df %>% 
  group_by(constructor_name, year) %>% 
  summarise(total_win = max(wins)) %>% 
  group_by(constructor_name) %>% 
  summarise(win_by_team = sum(total_win))

# DRIVER STANDINGS POSITION IS NOT SAME AS RESULTS POSITION
total_podium <- target_driver_df %>% 
  mutate(podium_bin = ifelse(position <= 3, 1, 0)) %>% 
  # filter(podium_bin == 1)
  summarise(total_podium = sum(podium_bin)) %>%
  pull(total_podium)
team_wise_podium <- target_driver_df %>% 
  mutate(podium_bin = ifelse(position <= 3, 1, 0)) %>% 
  group_by(constructor_name) %>% 
  summarise(podium_by_team = sum(podium_bin))
