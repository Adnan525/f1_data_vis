library(tidyverse)

results <- read.csv("data/results.csv")

results_2022 <- results %>% filter(raceId >= 1074 & raceId <= 1096)

# zhou 855
# mick 854
# yuki 852

zhou <- results_2022 %>% filter(driverId == 855) %>% select(raceId, driverId, positionOrder, points, fastestLapTime, statusId)
mick <- results_2022 %>% filter(driverId == 854) %>% select(raceId, driverId, positionOrder, points, fastestLapTime, statusId)
yuki <- results_2022 %>% filter(driverId == 852) %>% select(raceId, driverId, positionOrder, points, fastestLapTime, statusId)


ggplot(zhou, aes(x = as.factor(raceId)))+
  #zhou
  geom_line(aes(y = positionOrder, group = 1, color = "zhou"))+
  geom_point(aes(y = positionOrder, color = "zhou"), shape = 15, size = 2)+
  #mick
  geom_line(data = mick, aes(y = positionOrder, group = 1, color = "mick"))+
  geom_point(data = mick, aes(y = positionOrder, color = "mick"), shape = 15, size = 2)+
  #yuki
  geom_line(data = yuki, aes(y = positionOrder, group = 1, color = "yuki"))+
  geom_point(data = yuki, aes(y = positionOrder, color = "yuki"), shape = 15, size = 2)+
  scale_color_manual(values = c("yuki" = "blue", "mick" = "red", "zhou" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# point comparison
