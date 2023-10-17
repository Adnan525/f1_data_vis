# WE HAVE FASTEST LAP DATA FOR 37 CIRCUITS ONLY
# TOTAL NUMBER OF CIRCUITS IS 77

source("convert_time_string.R")

circuits <- read.csv("data/circuits.csv")
results <- read.csv("data/results.csv")
races <- read.csv("data/races.csv")
drivers <- read.csv("data/drivers.csv")
constructors <- read.csv("data/constructors.csv")

race_result <- results %>% 
  inner_join(races, by = "raceId") %>% 
  rename(time = time.x) %>% 
  select(year, circuitId, names(results))
# temp <- race_result %>% 
#   mutate(fastestLapTime = ifelse(fastestLapTime == "\\N", "9:9.9", fastestLapTime))
temp <- race_result %>% 
  mutate(fastestLapTime_inSeconds = lapply(fastestLapTime, convert_time_string))
temp$fastestLapTime_inSeconds <- as.double(temp$fastestLapTime_inSeconds)
temp <- na.omit(temp)

temp <- temp %>% select(year, circuitId, driverId, constructorId, fastestLapTime_inSeconds)

fastest_lap_time_37_circuits <- temp %>% 
  group_by(circuitId) %>% 
  filter(fastestLapTime_inSeconds == min(fastestLapTime_inSeconds))

circuits_updated <- circuits %>% 
  left_join(fastest_lap_time_37_circuits, by = "circuitId") %>% 
  rename(circuit_name = name) %>% 
  select(circuitRef, circuit_name, location, lat, lng, names(fastest_lap_time_37_circuits))
circuits_updated <- circuits_updated %>% 
  left_join(drivers, by = "driverId") %>% 
  select(driverRef, names(circuits_updated)) %>% 
  select(-driverId)

circuits_updated <- circuits_updated %>% 
  left_join(constructors, by = "constructorId") %>% 
  select(constructorRef, names(circuits_updated)) %>% 
  select(-constructorId)
circuits_updated <- circuits_updated %>% 
  select(circuit_name, circuitRef, location, lat, lng, fastestLapTime_inSeconds, driverRef, constructorRef, year)
write.csv(circuits_updated, file = "data/circuits_updated.csv",row.names = FALSE)
