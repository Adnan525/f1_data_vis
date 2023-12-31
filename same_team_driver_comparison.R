# mick schumacher seasons 2021, 2022
results <- read.csv("data/results.csv")
results_2021 <- results %>% filter(raceId >= 1052 & raceId <= 1073)
results_2022 <- results %>% filter(raceId >= 1074 & raceId <= 1096)
races <- read.csv("data/races.csv")
names(races)[5] <- "race_name"

yuki_id <- 852
mick_id <- 854
gasly_id <- 842
nikita_id <- 853

alphatauri <- results_2021 %>% filter(driverId == 852 | driverId == 842)
haas <- results_2021 %>% filter(driverId == mick_id | driverId == nikita_id)

alphatauri_point_df_2021 <- alphatauri %>% 
  group_by(driverId) %>% 
  summarise(total_points = sum(points)) %>% 
  mutate(driverId = ifelse(driverId == gasly_id, "Pierre Gasly", "Yuki Tsunoda"))

haas_point_df_2021 <- haas %>% 
  group_by(driverId) %>% 
  summarise(total_points = sum(points)) %>% 
  mutate(driverId = ifelse(driverId == nikita_id, "Nikita Mazepin", "Mick Schumacher"))

# position data
yuki_positions <- results_2021 %>% filter(driverId == yuki_id) %>% pull(positionOrder)
gasly_positions <- results_2021 %>% filter(driverId == gasly_id) %>% pull(positionOrder)
mick_positions <- results_2021 %>% filter(driverId == mick_id) %>% pull(positionOrder)
nikita_positions <- results_2021 %>% filter(driverId == nikita_id) %>% pull(positionOrder)
raceId <- unique(results_2021$raceId)
position_df_2021 <- data.frame(raceId, yuki_positions, gasly_positions, mick_positions, nikita_positions)

position_df_2021 <- position_df_2021 %>% 
  inner_join(races, by = "raceId") %>% 
  select(raceId, race_name, yuki_positions, gasly_positions, mick_positions, nikita_positions)

# domination

# alphatauri <- alphatauri %>% 
#   left_join(races, by = "raceId", keep = FALSE) %>% 
#   rename(time = time.x) %>% 
#   select(race_name, names(alphatauri))
alphatauri_dominate_driver_df_2021 <- alphatauri %>% 
  inner_join(races, by = "raceId") %>% 
  mutate(driver_name = ifelse(driverId == gasly_id, "Pierre Gasly", "Yuki Tsunoda")) %>% 
  group_by(raceId) %>% 
  filter(positionOrder == min(positionOrder)) %>% 
  select(raceId, driver_name)
alphatauri_dominate_driver_df_2021 <- data.frame(table(alphatauri_dominate_driver_df_2021$driver_name))

haas_dominate_driver_df_2021 <- haas %>% 
  inner_join(races, by = "raceId") %>% 
  mutate(driver_name = ifelse(driverId == mick_id, "Mick Schumacher", "Nikita Mazepin")) %>% 
  group_by(raceId) %>% 
  filter(positionOrder == min(positionOrder)) %>% 
  select(raceId, driver_name)
haas_dominate_driver_df_2021 <- data.frame(table(haas_dominate_driver_df_2021$driver_name))




# ALPHATAURI

# position
ggplot(position_df_2021, aes(x = as.factor(race_name))) +
  geom_line(aes(y = position_df_2021$yuki_positions, color = "Yuki", group = 1), show.legend = TRUE) +
  geom_line(aes(y = position_df_2021$gasly_positions, color = "Gasly", group = 1), show.legend = TRUE) +
  geom_point(aes(y = position_df_2021$yuki_position, color = "Yuki"), shape = 15, size = 3) +
  geom_point(aes(y = position_df_2021$gasly_positions, color = "Gasly"), shape = 15, size = 3) +
  scale_color_manual(values = c("Yuki" = "blue", "Gasly" = "red"), name = "Alphatauri Drivers") +
  labs(x = "Races", y = "Position Order", title = "Position comparison between Scuderia Alphatauri drivers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# points
ggplot(alphatauri_point_df_2021, 
       aes(x = total_points, 
           y = driverId, 
           fill = "Red")) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Alphatauri Driver Standings", x = "Points", y = "Driver Name")+
  scale_fill_manual(values = c("Red" = "red"))+
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"))

# points pie chart

# Calculate the total points for the two drivers
total_points <- sum(alphatauri_point_df_2021$total_points)

# Create a data frame for the pie chart
pie_data <- data.frame(
  Driver = c("Pierre Gasly", "Yuki Tsunoda"),
  Points = c(
    alphatauri_point_df_2021$total_points[1],
    alphatauri_point_df_2021$total_points[2])
)

ggplot(pie_data, aes(x = "", y = Points, fill = Driver)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Alphatauri Drivers' Points Contribution 2021", fill = "Driver") +
  scale_fill_manual(values = c("Pierre Gasly" = "red", "Yuki Tsunoda" = "blue", "Other" = "gray")) +
  theme_void()


# dominate
ggplot(alphatauri_dominate_driver_df_2021, aes(x = Var1, y = Freq, fill = "Red"))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  labs(title = "Alphatauri dominate driver 2021", x = "Driver Name", y = "Frequency")+
  scale_fill_manual(values = c("Red" = "red"))+
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"))



#HAAS

#position
ggplot(position_df_2021, aes(x = as.factor(race_name))) +
  geom_line(aes(y = position_df_2021$mick_positions, color = "Mick", group = 1), show.legend = TRUE) +
  geom_line(aes(y = position_df_2021$nikita_positions, color = "Nikita", group = 1), show.legend = TRUE) +
  geom_point(aes(y = position_df_2021$mick_positions, color = "Mick"), shape = 15, size = 3) +
  geom_point(aes(y = position_df_2021$nikita_positions, color = "Nikita"), shape = 15, size = 3) +
  scale_color_manual(values = c("Mick" = "blue", "Nikita" = "red"), name = "Haas Drivers") +
  labs(x = "Races", y = "Position Order", title = "Position comparison between Haas drivers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# points
ggplot(haas_point_df_2021, 
       aes(x = total_points, 
           y = driverId, 
           fill = "Red")) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Haas Driver Standings 2021", x = "Points", y = "Driver Name")+
  scale_fill_manual(values = c("Red" = "red"))+
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"))

# dominate
ggplot(haas_dominate_driver_df_2021, aes(x = Var1, y = Freq, fill = "Red"))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  labs(title = "Haas dominate driver 2021", x = "Driver Name", y = "Frequency")+
  scale_fill_manual(values = c("Red" = "red"))+
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"))
