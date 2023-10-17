source("convert_time_string.R")

circuits <- read.csv("data/circuits.csv")
results <- read.csv("data/results.csv")
races <- read.csv("data/races.csv")

race_result <- results %>% 
  inner_join(races, by = "raceId") %>% 
  rename(time = time.x) %>% 
  select(circuitId, names(results))
race_result <- race_result %>% mutate(fastestLapTime = ifelse(fastestLapTime == "//N", "9:9.9", "text"))
race_result %>% mutate(fastestLap_seconds = apply(fastestLapTime, 1, convert_time_string))
