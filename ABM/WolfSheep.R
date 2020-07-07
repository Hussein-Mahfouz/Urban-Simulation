library(tidyverse)

# read in the data
#data <- read_csv('Data/wolf-sheep-grass-vary-initial.csv', skip = 6, col_names = TRUE) # skip because we don't want top 6 rows
data <- read_csv('Data/wolf-sheep-grass-no-steps.csv', skip = 6, col_names = TRUE) # skip because we don't want top 6 rows
# with steps
data2 <- read_csv('Data/wolf-sheep-grass-with-200-steps.csv', skip = 6, col_names = TRUE) 

# max sheep value at which run terminates
max_sheep <- 80000
no_repetitions <- 20    # how many times the model is run for each combination

# add group labels 
#groups <- data %>%  
#  mutate(., group = group_indices(., `wolf-gain-from-food`, `wolf-reproduce`, `initial-number-wolves`, `initial-number-sheep`,
#                                  `sheep-gain-from-food`, `grass-regrowth-time`, `sheep-reproduce`))
groups <- data %>%  
  mutate(., group = group_indices(.,`initial-number-wolves`, `initial-number-sheep`, `grass-regrowth-time`))

# for data without steps
stats <- groups %>% 
  group_by(`group`) %>%
  # get some stats
  summarise(
    sheep_mean       = mean(`count sheep`), sheep_std   = sd(`count sheep`),
    sheep_min        = min(`count sheep`), sheep_max   = max(`count sheep`),
    sheep_dead_perc  = (length(`count sheep`[`count sheep` == 0]) / no_repetitions) * 100,  # how many times all sheep died
    wolves_mean      = mean(`count wolves`), wolves_std   = sd(`count wolves`),
    wolves_min       = min(`count wolves`), wolves_max   = max(`count wolves`),
    wolves_dead_perc = (length(`count wolves`[`count wolves` == 0]) / no_repetitions) * 100, # how many times all wolves died
    steps_mean       = mean(`[step]`), steps_std   = sd(`[step]`), 
    steps_min        = min(`[step]`), steps_max   = max(`[step]`),
    initial_ratio    = round(mean(`initial-number-wolves` / `initial-number-sheep`), digits = 2)) 

# filter out all groups where all sheep and all wolves die

alive <- stats %>% filter(sheep_dead_perc != 100 & wolves_dead_perc <= 50 
                          & sheep_mean < max_sheep & wolves_mean > wolves_std
                          & steps_mean > 300)  

# join this filtered table to the groups table (for plotting)

merged <- left_join(alive, groups, by = 'group')
          

# All 2 df are for heatmap which is based on csv with steps

# for data with steps
groups2 <- data2 %>%  
  mutate(., group = group_indices(.,`initial-number-wolves`, `initial-number-sheep`, `grass-regrowth-time`))

stats2 <- groups2 %>% 
   group_by(`group`, `[step]`) %>%
#   # get some stats
   summarise(
     sheep_mean       = mean(`count sheep`), sheep_std   = sd(`count sheep`),
     sheep_min        = min(`count sheep`), sheep_max   = max(`count sheep`),
     sheep_dead_perc  = (length(`count sheep`[`count sheep` == 0]) / no_repetitions) * 100,  # how many times all sheep died
     wolves_mean      = mean(`count wolves`), wolves_std   = sd(`count wolves`),
     wolves_min       = min(`count wolves`), wolves_max   = max(`count wolves`),
     wolves_dead_perc = (length(`count wolves`[`count wolves` == 0]) / no_repetitions) * 100, # how many times all wolves died
     steps_mean       = mean(`[step]`), steps_std   = sd(`[step]`), 
     steps_min        = min(`[step]`), steps_max   = max(`[step]`))  

# filter out all groups where all sheep and all wolves die

#alive2 <- stats2 %>% filter(sheep_dead_perc != 100 & wolves_dead_perc <= 50 
#                          & sheep_mean < max_sheep & wolves_mean > wolves_std
#                          & steps_mean > 100)  

# join this filtered table to the groups table (for plotting)

merged2 <- left_join(alive2, groups2, by = 'group')

# group by initial counts of wolf and sheep + step (for heat map)
forHeatMap <- 
  groups2 %>% group_by(group, `[step]`) %>%
  #   # get some stats
  summarise(
    sheep                = mean(`count sheep`),     
    wolves               = mean(`count wolves`),
    #wolf_variation_perc  = ((mean(`count wolves`) / mean(`initial-number-wolves`)) /  mean(`initial-number-wolves`)) * 100,
    initial_wolves       = mean(`initial-number-wolves`),
    initial_sheep        = mean(`initial-number-sheep`),
    ratio                = mean(`initial-number-sheep`) / mean(`initial-number-wolves`),
    grass_regrowth_time  = mean(`grass-regrowth-time`))     






