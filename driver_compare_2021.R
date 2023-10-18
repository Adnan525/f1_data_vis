library(tidyverse)
options(warn = -1)
results <- read.csv("data/results.csv")
races <- read.csv("data/races.csv")
names(races)[5] <- "race_name"
status <- read.csv("data/status.csv")



races_2021_2022 <- races %>% filter(year %in% c(2021, 2022)) %>% arrange(year)
# race 1053-1073 1074-1096

results_2021 <- results %>% filter(raceId >= 1052 & raceId <= 1073)
results_2022 <- results %>% filter(raceId >= 1074 & raceId <= 1096)


# CHECKING TOTAL POINT AND HIGHEST POSITION OF YUKI AND MICK IN 2021
# mick shchumacher
results_2021 %>% 
  filter(driverId == 854) %>% 
  summarise(total_points = sum(points), highest_position = min(positionOrder))

# yuki tsunoda
results_2021 %>% 
  filter(driverId == 852) %>% 
  summarise(total_points = sum(points), highest_position = min(positionOrder))



# DIRECT COMPARE BETWEEN MICK AND YUKI'S POSITION

# apt install libxtst6 libxt6
yuki_positions <- results_2021 %>%
  filter(driverId == 852) %>%
  pull(positionOrder)
mick_positions <- results_2021 %>%
  filter(driverId == 854) %>%
  pull(positionOrder)

# CREATING NEW DATAFRAME WITH MICK AND YUKI POSITIONS
raceId <- unique(results_2021$raceId)
target <- data.frame(raceId, yuki_positions, mick_positions)
target <- target %>% 
  inner_join(races, by = "raceId") %>% 
  select(raceId, race_name, yuki_positions, mick_positions)

ggplot(target, aes(x = as.factor(race_name))) +
  geom_line(aes(y = target$yuki_positions, color = "Yuki", group = 1), show.legend = TRUE) +
  geom_line(aes(y = target$mick_positions, color = "Mick", group = 1), show.legend = TRUE) +
  geom_point(aes(y = target$yuki_position, color = "Yuki"), shape = 15, size = 3) +
  geom_point(aes(y = target$mick_positions, color = "Mick"), shape = 15, size = 3) +
  scale_color_manual(values = c("Yuki" = "blue", "Mick" = "red")) +
  labs(x = "Races", y = "Position Order", title = "Position comparison between Yuki and Mick in 2021") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# dnfs
yuki_status <- results_2021 %>%
  filter(driverId == 852) %>%
  pull(statusId)
mick_status <- results_2021 %>%
  filter(driverId == 854) %>%
  pull(statusId)
mick_yuki_status_df <- data.frame(yuki_status, mick_status)

mick_yuki_status_df <- merge(mick_yuki_status_df, status, by.x = "yuki_status", by.y = "statusId", all.x = TRUE)
mick_yuki_status_df <- merge(mick_yuki_status_df, status, by.x = "mick_status", by.y = "statusId", all.x = TRUE)
mick_yuki_status_df <- mick_yuki_status_df %>% 
  rename(mick_status_text = status.y, yuki_status_text = status.x) %>% 
  select(-c(mick_status, yuki_status))
table(mick_yuki_status_df$yuki_status_text)
table(mick_yuki_status_df$mick_status_text)
