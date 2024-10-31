'03
Determing the peak timepoint and velocity and duration of the peak in velocity after activation'

# Calculate the time where the velocity returns to the mean of preactivation after the peak -----

# Import the thresholded data set (containing all time series of embryos that have been thresholded by zscore>2 of peak velocity response )

thresholded_data <- read.xlsx('results/spreadsheets/2024-06-20_thresholded_data_ungrouped.xlsx') %>% 
  mutate(position = factor(position, levels = c('C', 'N', 'S', 'E', 'W')))

# determine the peak velocity
peak_vel_during_act <- thresholded_data %>%
  filter(measure == 'vel', position != 'C', timepoint >= 0, timepoint <= 120) %>%
  group_by(embryo.id) %>%
  slice_max(order_by = mean) %>% 
  rename(peak_vel = mean,
         peak_vel_tp = timepoint,
         peak_vel_position = position) %>% 
  select(embryo.id, peak_vel, peak_vel_tp, peak_vel_position) %>% 
  ungroup()

filename <- paste(Sys.Date(), 'peak_vel', sep = "_")

write_xlsx(peak_vel_during_act, path = sprintf('results/spreadsheets/%s.xlsx', filename))


# make a dataframe containing the velocity data from ant & N and post & S
# add a column containing the difference between the mean and the mean before - doesnt work????
vel_diff <- thresholded_data %>% 
  group_by(embryo.id, timepoint, direction) %>% 
  filter(measure == 'vel', (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S')) %>% 
  select(embryo.id, timepoint, direction, mean, std) %>% 
  mutate(diff = mean - lag(mean)) 

# find the preactivation mean velocity for each embryo 
preact_stats = thresholded_data %>% 
  filter(measure == 'vel', timepoint < 0, (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S')) %>% 
  group_by(embryo.id) %>% 
  summarise(std_preact = sqrt(sum(std^2) / n()), 
            mean_preact = mean(mean))

# combine the data frames
vel_diff <- vel_diff %>% 
  left_join(preact_stats, by = 'embryo.id')

# When does velocity go below the preactivation mean velocity (after the peak value)
peak_duration <- vel_diff %>% 
  filter(timepoint >= 0, timepoint <= 120) %>% 
  group_by(embryo.id) %>% 
  filter(timepoint > timepoint[which.max(mean)]) %>% 
  filter(mean < mean_preact) %>% 
  slice_head(n = 1) %>% 
  ungroup(embryo.id) # %>% select(embryo.id, timepoint)

print(peak_duration)

# # A tibble: 9 × 2
# embryo.id        timepoint
# <chr>                <dbl>
# 1 20221018_E1_15ss        50
# 2 20221108_E8_15ss        35
# 3 20221214_E2_13ss        35
# 4 20221214_E3_14ss        50
# 5 20221214_E4_15ss        35
# 6 20221214_E5_12ss        70
# 7 20230804_E1_21ss        70
# 8 20231026_E1_16ss        70
# 9 20231026_E3_NA          55

deact_peak_duration <- vel_diff %>% 
  filter(timepoint >= 120) %>% 
  group_by(embryo.id) %>% 
  filter(timepoint > timepoint[which.max(mean)]) %>% 
  filter(mean < mean_preact) %>% 
  slice_head(n = 1) %>% 
  mutate(timepoint = timepoint - 120) %>% 
  ungroup() # %>% select(embryo.id, timepoint)

print(deact_peak_duration)

deact_peak_duration %>% 
  summarise(duration = mean(timepoint),
            duration_std = sd(timepoint))

# plot the velocity and the threshold
(
  peak_width <- vel_diff %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    geom_hline(aes(yintercept = mean_preact), colour = 'red', alpha = 0.5) +
    geom_line() +
    geom_point(data = peak_duration, aes(x = timepoint, y = mean), colour = 'red') +
    facet_wrap(~direction ~embryo.id)
)

# find the mean and std of the peak duration
peak_duration_all <- peak_duration %>% 
  summarise(duration = mean(timepoint),
            duration_std = stats::sd(timepoint))


peak_duration_dir <- peak_duration %>% 
  group_by(direction) %>% 
  summarise(duration = mean(timepoint),
            duration_std = stats::sd(timepoint))

print(peak_duration_all) # this is to the nearest 5secs though - no interpolation to calc. this value
print(peak_duration_dir) 

# # A tibble: 1 × 2
# duration duration_std
# <dbl>        <dbl>
#   1     52.2         15.2
# > print(peak_duration_dir) 
# # A tibble: 2 × 3
# direction duration duration_std
# <chr>        <dbl>        <dbl>
#   1 anterior      58.3        14.4 
# 2 posterior     40           8.66


# Doing the above but on the mean velocity of all N or S embryos ----
# find the preactivation mean velocity for each set of embryo 
preact_stats_N = thresholded_data %>% 
  filter(measure == 'vel', timepoint < 0, (direction == 'anterior' & position == 'N')) %>% 
  summarise(std_preact = sqrt(sum(std^2) / n()), 
            mean_preact = mean(mean))

preact_stats_S = thresholded_data %>% 
  filter(measure == 'vel', timepoint < 0, (direction == 'posterior' & position == 'S')) %>% 
  summarise(std_preact = sqrt(sum(std^2) / n()), 
            mean_preact = mean(mean))

preact_stats_N_S = thresholded_data %>% 
  filter(measure == 'vel', timepoint < 0, (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S')) %>% 
  summarise(std_preact = sqrt(sum(std^2) / n()), 
            mean_preact = mean(mean))

# make a dataframe containing the velocity data from ant & N and post & S

vel_N <- thresholded_data %>% 
  group_by(timepoint) %>% 
  filter(measure == 'vel', (direction == 'anterior' & position == 'N')) %>% 
  select(timepoint, mean, std) %>% 
  summarise(mean = mean(mean), 
            std = sqrt(sum(std^2) / n())) %>% 
  mutate(diff = mean - lag(mean),
         mean_preact = preact_stats_N$mean_preact,
         std_preact = preact_stats_N$std_preact) 

vel_S <- thresholded_data %>% 
  group_by(timepoint) %>% 
  filter(measure == 'vel', (direction == 'posterior' & position == 'S')) %>% 
  select(timepoint, mean, std) %>% 
  summarise(mean = mean(mean), 
            std = sqrt(sum(std^2) / n())) %>% 
  mutate(diff = mean - lag(mean),
         mean_preact = preact_stats_S$mean_preact,
         std_preact = preact_stats_S$std_preact) 

vel_ave_N_S <- thresholded_data %>% 
  group_by(timepoint) %>% 
  filter(measure == 'vel', (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S')) %>% 
  select(timepoint, mean, std) %>% 
  summarise(mean = mean(mean), 
            std = sqrt(sum(std^2) / n())) %>% 
  mutate(diff = mean - lag(mean),
         mean_preact = preact_stats_N_S$mean_preact,
         std_preact = preact_stats_N_S$std_preact) 

# When does velocity go below the preactivation mean velocity (after the peak value)
peak_duration_N <- vel_N %>% 
  filter(timepoint >= 0, timepoint <= 120) %>% 
  filter(timepoint > timepoint[which.max(mean)]) %>% 
  filter(mean < mean_preact) %>% 
  slice_head(n = 1) %>% 
  ungroup()

print(peak_duration_N$timepoint) # 2024_01_24 - duration is 70s - this is to the nearest 5secs - no interpolation to calc. this value

peak_duration_S <- vel_S %>% 
  filter(timepoint >= 0, timepoint <= 120) %>% 
  filter(timepoint > timepoint[which.max(mean)]) %>% 
  filter(mean < mean_preact) %>% 
  slice_head(n = 1) %>% 
  ungroup()

print(peak_duration_S$timepoint) # 2024_01_24 - duration is 50s - this is to the nearest 5secs - no interpolation to calc. this value

peak_duration_N_S <- vel_ave_N_S %>% 
  filter(timepoint >= 0, timepoint <= 120) %>% 
  filter(timepoint > timepoint[which.max(mean)]) %>% 
  filter(mean < mean_preact) %>% 
  slice_head(n = 1) %>% 
  ungroup()

print(peak_duration_N_S$timepoint) # 2024_01_24 - duration is 70s - this is to the nearest 5secs - no interpolation to calc. this value


# plot the velocity and the threshold
(
  peak_width_N <- vel_N %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    geom_hline(aes(yintercept = mean_preact), colour = 'red', alpha = 0.5) +
    geom_line() +
    geom_point(data = peak_duration_N, aes(x = timepoint, y = mean), colour = 'red') 
)

(
  peak_width_S <- vel_S %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    geom_hline(aes(yintercept = mean_preact), colour = 'red', alpha = 0.5) +
    geom_line() +
    geom_point(data = peak_duration_S, aes(x = timepoint, y = mean), colour = 'red') 
)

(
  peak_width_N_S <- vel_ave_N_S %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    geom_hline(aes(yintercept = mean_preact), colour = 'red', alpha = 0.5) +
    geom_line() +
    geom_point(data = peak_duration_N_S, aes(x = timepoint, y = mean), colour = 'red') 
)

peak_width_N + peak_width_S + peak_width_N_S



