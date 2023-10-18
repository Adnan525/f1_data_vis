get_driver_details <- function(target_driver, current_year){
  # load data
  driver_standings <- read.csv("data/driver_standings_updated.csv")
  championship_df <- read.csv("data/championship_df.csv")
  
  # get filtered df for target driver
  target_driver_df <- driver_standings %>% filter(driver_name == target_driver) %>% arrange(year)
  
  
  target_driver_appearance <- dim(target_driver_df)[1]
  
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
  
  total_points <- target_driver_df %>% 
    group_by(year) %>% 
    summarise(points_by_year = max(points)) %>% 
    summarise(total_points = sum(points_by_year)) %>% 
    pull(total_points)
  
  team_wise_point <- target_driver_df %>% 
    group_by(constructor_name, year) %>% 
    summarise(point_by_year = max(points)) %>% 
    group_by(constructor_name) %>% 
    summarise(point_by_team = sum(point_by_year))
  
  # championship_count
  temp <- table(championship_df$driver_name)
  target_driver_championship <- ifelse(target_driver %in% championship_df$driver_name, temp[target_driver], 0)
  
  # adding values
  driver_details <-list(
    name = target_driver,
    appearance = target_driver_appearance,
    currently_driving = is_currently_driving,
    current_team = current_team,
    all_teams = all_teams,
    wins = total_win,
    win_by_team = team_wise_win,
    podiums = total_podium,
    podium_by_team = team_wise_podium,
    career_points = total_points,
    points_by_team = team_wise_point,
    championship = target_driver_championship
  )
  
  return(driver_details)
}