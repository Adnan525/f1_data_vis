
pit_stops <- read.csv("data/pit_stops.csv")
races <- read.csv("data/races.csv")

pit_with_year <- inner_join(races, pit_stops, by = "raceId") %>% 
  select(year, raceId, driverId, stop, lap, milliseconds)

# max(pit_with_year$milliseconds)/1000
# pit_with_year %>% filter(milliseconds > 30*1000)

temp <- pit_with_year %>%
  group_by(year) %>%
  summarize(fastest_pit = min(milliseconds))

# punish
temp <- temp %>% 
  mutate(adj_time = ifelse(year<2014, fastest_pit*1, ifelse(year>=2014 & year<=2021, fastest_pit * 0.8, fastest_pit*0.6)))
# 2014-2021 20% reduce
# temp <- target %>%
#   filter(year >= 2014 & year <= 2021) %>%
#   mutate(fastest_lap = fastest_lap * 0.8)
# 
# # 2022 and 2023 40%
# temp2 <- target %>%
#   filter(year >= 2022 & year <= 2023 ) %>%
#   mutate(fastest_lap = fastest_lap * 0.6)
# 
# target <- bind_rows(
#   target %>% filter(year < 2014), # data before 2014
#   temp,
#   temp2
# )


# ggplot(temp, aes(x = as.factor(year), y = fastest_pit, group = 1)) +
#   geom_line() +
#   labs(x = "Year", y = "Pit Time")
library(ggplot2)
ggplot(temp, aes(x = as.factor(year), y = fastest_pit, group = 1)) +
  geom_line(aes(color = "Fastest Pit Time")) +
  geom_line(data = temp, aes(x = as.factor(year), y = adj_time, color = "Adjusted Time")) +
  labs(x = "Year", y = "Pit-time") +
  scale_color_manual(values = c("Fastest Pit Time" = "blue", "Adjusted Time" = "red"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
