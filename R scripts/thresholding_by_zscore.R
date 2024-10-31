# Helena's code to generate plots in different cardinal directions for measurements from PIV analysis 
# Oct 2023 - Dec 2023
# # = normal comments, '##' = notes to myself

# Load libraries
library(tidyverse)
library(openxlsx)
library(circular)
library(pdf2pptx)
library(ggquiver)
library(viridis)  

#-------------------

## is this not needed when using git or when there is rproj?
# setwd('~/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis')

wd <- getwd()


# Read in data annotations / metadata
metadata <- read.xlsx('/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/light-patterning-experiment-metadata.xlsx')

# Create a data frame containing the PIV data (mean and std of different measures in different cardinal direction around the ROI)
file1 <- 'data/2022-10-18/E1_new_15ss_470nm_TL_region1.tif/xlsx_data/2022-10-18_E1_new_15ss_470nm_TL_region1.xlsx'
file2 <- 'data/2022-11-08/E8_15ss_470nm_TL_.tif/xlsx_data/2022-11-08_E8_15ss_470nm_TL_.xlsx'
file3 <- 'data/2022-12-14/E4_15ss_470nm_TL_.tif/xlsx_data/2022-12-14_E4_15ss_470nm_TL_.xlsx'
file4 <- 'data/2023-08-04/E1_approx21ss_470nm_TL_.tif/xlsx_data/2023-08-04_E1_approx21ss_470nm_TL_.xlsx'
file5 <- 'data/2023-09-01/E2_470nm_6m-TL_.tif/xlsx_data/2023-09-01_E2_470nm_6m-TL_.xlsx'
file6 <- 'data/2023-09-01/E5_470nm_6m-TL_.tif/xlsx_data/E5_470nm_6m-TL_.tif.xlsx' # should remove as dmd was sent a bmp file with 1 as grey value
file7 <- 'data/2023-09-01/E6_470nm_6m-TL_.tif/xlsx_data/E6_470nm_6m-TL_.tif.xlsx'
file8 <- 'data/2023-10-26/E1_16ss_6m_470nm_TL_.tif/xlsx_data/2023-10-26_E1_16ss_6m_470nm_TL_.xlsx'
file9 <- 'data/2023-10-26/E3_6m_470nm_TL_.tif/xlsx_data/2023-10-26_E3_6m_470nm_TL_.xlsx'
file10 <- 'data/20221214_E2_13ss/xlsx_data/data_20221214_E2_13ss.xlsx'
file11 <- 'data/20221214_E3_14ss/xlsx_data/data_20221214_E3_14ss.xlsx'
file12 <- 'data/20221214_E5_12ss/xlsx_data/data_20221214_E5_12ss.xlsx' 

file_names <- c(file1, file2, file3, file4, file5, file6, file7, file8, file9, file10, file11, file12)  
data_list <- list()
i <- 1

# 
# for (file in file_names){
#   file <- file.path(wd, file)
#   print(file)
#   exists <- file.exists(file)
#   print(exists)
#   }

for (file in file_names){
  temp_data <- read.xlsx(file)
  to_keep <- rowSums(temp_data, na.rm = T) != 0
  date_id <- str_extract(file, '\\d{8}') # should this just be \d{8}
  date_id_2 <- str_extract(file, '\\b\\d{4}-\\d{2}-\\d{2}\\b')
  embryo_id <- str_extract(file, '[Ee]\\d')
  somite_id <- str_extract(file, '\\d+ss')
  ## direction_id <- str_split(file, '/', simplify = TRUE)[1,8] # add the label from the 8th position in the file name # obsolete
  temp_data <- as.data.frame(temp_data) %>% 
    mutate(date = date_id,
           date_2 = date_id_2,
           embryo = embryo_id,
           somite = somite_id,
           timepoint = (1:n()) - 200L)
  data_list[[i]] <- temp_data[to_keep,]
  i <- i+1
}
i <- 1

data <- bind_rows(data_list) %>% as_tibble()

# Put the data into a tidy format
data <- data %>% 
  mutate(date_2 = format(as.Date(date_2), "%Y%m%d")) %>% 
  mutate(date = coalesce(date, date_2)) %>% 
  select(-'date_2') %>% 
  pivot_longer(cols = c(-'date', -'embryo', -'somite', -'timepoint'), 
               names_to = 'to_split', 
               values_to = 'value') %>% 
  separate(to_split, c('position', 'measure', 'statistic'), sep = '_') %>% 
  pivot_wider(names_from = 'statistic', values_from = 'value') %>% 
  mutate(position = factor(position, levels = c('C', 'N', 'S', 'E', 'W'))) %>% 
  mutate(timepoint = timepoint * 5) %>% 
  unite(col = 'embryo.id', c('date', 'embryo', 'somite'), remove = FALSE) 


## data$direction %>% unique() # checking what values are in direction column

# Make the embryo.id column in metadata the same as data
metadata <- metadata %>%
  mutate(embryo.id = gsub('-', '', embryo.id)) %>% 
  unite(col = 'embryo.id', c('embryo.id', 'embryo.age'), remove = FALSE)

# Add timelapse specific metadata to the data
timelapse_metadata <- metadata %>% 
  filter(file.type == 'timelapse', total.timepoints == '73') %>% 
  select(embryo.id, timelapse.length.sec, timestep.sec, total.timepoints,	activation.start.tp,	activation.end.tp)

data <- data %>% 
  left_join(timelapse_metadata, by = 'embryo.id', keep = FALSE)  %>% 
  mutate(align.activation.start =  0,  # Guillermo aligned the PIV data so activation start = 0
         align.activation.end = 5 * (as.numeric(activation.end.tp) - as.numeric(activation.start.tp) + 1) )

## double check this is aligned properly in fiji by looking at the timelapse e.g. should i add the 1??
## unique(data[,c('embryo.id', 'align.activation.end')]) 

## #obsolete # data <- merge(data, metadata[, c('embryo.id', 'timelapse.length.sec', 'timestep.sec', 'timepoints',	'activation.start.tp',	'activation.end.tp')], by = 'embryo.id', all.x = TRUE)


# Finding axial direction of movement by mean vang ----------------------------------------------

angles_wm_by_position_during <- data %>% 
  filter(!position %in% c('C', 'W'), timepoint >= 0, timepoint <= 45) %>% # exclude C/ROI due to optogenetic recruitment, W as not neural tube, include t=0-45 as peak velocity occurs then 
  filter(measure %in% c("vel", "vang")) %>% 
  pivot_wider(id_cols = -std, names_from = measure, values_from = mean) %>% # make cols called vel and vang containing the mean 
  #drop_na(vang) %>% ## check for na, if so need to drop
  mutate(vang = as.circular(vang, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  group_by(embryo.id, position) %>% 
  summarise(weighted_mean = as.double(weighted.mean.circular(x = vang, w = vel, na.rm = T)),
            mean_vel = mean(vel)) %>% # how does weighting work
  mutate(weighted_mean = weighted_mean * (180 / pi),
         period = 'during')  # convert to degrees


angles_wm_by_position_before <- data %>% 
  filter(!position %in% c('C', 'W'), timepoint >= -50, timepoint <= -5) %>% # exclude C/ROI due to optogenetic recruitment, W as not neural tube, include t=0-45 as peak velocity occurs then 
  filter(measure %in% c("vel", "vang")) %>% 
  pivot_wider(id_cols = -std, names_from = measure, values_from = mean) %>% # make cols called vel and vang containing the mean 
  #drop_na(vang) %>% ## check for na, if so need to drop
  mutate(vang = as.circular(vang, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  group_by(embryo.id, position) %>% 
  summarise(weighted_mean = as.double(weighted.mean.circular(x = vang, w = vel, na.rm = T)),
            mean_vel = mean(vel)) %>% # how does weighting work
  mutate(weighted_mean = weighted_mean * (180 / pi),
         period = 'before')  # convert to degrees

angles_wm_by_position_after <- data %>% 
  filter(!position %in% c('C', 'W'), timepoint >= 125, timepoint <= 170) %>% # exclude C/ROI due to optogenetic recruitment, W as not neural tube, include t=0-45 as peak velocity occurs then 
  filter(measure %in% c("vel", "vang")) %>% 
  pivot_wider(id_cols = -std, names_from = measure, values_from = mean) %>% # make cols called vel and vang containing the mean 
  #drop_na(vang) %>% ## check for na, if so need to drop
  mutate(vang = as.circular(vang, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  group_by(embryo.id, position) %>% 
  summarise(weighted_mean = as.double(weighted.mean.circular(x = vang, w = vel, na.rm = T)),
            mean_vel = mean(vel)) %>% # how does weighting work
  mutate(weighted_mean = weighted_mean * (180 / pi),
         period = 'after')  # convert to degrees



angles_wm_all_before <- data %>% 
  filter(!position %in% c('C', 'W'), timepoint >= -45, timepoint <= 0) %>%
  filter(measure %in% c("vel", "vang")) %>% 
  pivot_wider(id_cols = -std, names_from = measure, values_from = mean) %>% 
  # drop_na(vang) %>% 
  mutate(vang = as.circular(vang, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  group_by(embryo.id) %>% 
  summarise(wm_vang_before = as.double(weighted.mean.circular(x = vang, w = vel, na.rm = T)),
            mean_vel_before = mean(vel)) %>% 
  mutate(wm_vang_before = wm_vang_before * (180 / pi)) %>% 
  ungroup() 

angles_wm_all_first45 <- data %>% 
  filter(!position %in% c('C', 'W'), timepoint >= 0, timepoint <= 45) %>%
  filter(measure %in% c("vel", "vang")) %>% 
  pivot_wider(id_cols = -std, names_from = measure, values_from = mean) %>% 
  # drop_na(vang) %>% 
  mutate(vang = as.circular(vang, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  group_by(embryo.id) %>% 
  summarise(wm_vang_first45 = as.double(weighted.mean.circular(x = vang, w = vel, na.rm = T)),
            mean_vel_first45 = mean(vel)) %>% 
  mutate(wm_vang_first45 = wm_vang_first45 * (180 / pi)) %>% 
  ungroup() %>% 
  mutate(direction = case_when(wm_vang_first45 < 0 ~ 'anterior',
                               wm_vang_first45 > 0 ~ 'posterior'))

angles_wm_all_after <- data %>% 
  filter(!position %in% c('C', 'W'), timepoint >= 125, timepoint <= 170) %>%
  filter(measure %in% c("vel", "vang")) %>% 
  pivot_wider(id_cols = -std, names_from = measure, values_from = mean) %>% 
  # drop_na(vang) %>% 
  mutate(vang = as.circular(vang, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  group_by(embryo.id) %>% 
  summarise(wm_vang_after = as.double(weighted.mean.circular(x = vang, w = vel, na.rm = T)),
            mean_vel_after = mean(vel)) %>% 
  mutate(wm_vang_after = wm_vang_after * (180 / pi)) %>% 
  ungroup()

angles_wm_all <- left_join(angles_wm_all_before, angles_wm_all_first45)
angles_wm_all <- left_join(angles_wm_all, angles_wm_all_after)

angles_wm_by_position <- bind_rows(angles_wm_by_position_before, angles_wm_by_position_during, angles_wm_by_position_after)
angles_wm_by_position <- angles_wm_by_position %>% 
  left_join(angles_wm_all_first45, by = 'embryo.id', keep = FALSE) %>% 
  select(-wm_vang_first45, -mean_vel_first45)


AP_grouped_data <- data %>%
  left_join(angles_wm_all_first45, by = 'embryo.id', keep = FALSE) %>% 
  relocate(direction, .before=position)

#--------




# Create column with true/false for preactivation

AP_grouped_data = AP_grouped_data %>% 
  mutate(preactivation = case_when(timepoint < 0 ~ TRUE,
                                   timepoint >= 0 ~ FALSE))

# Find the preactivation mean and std for each preprocessing method
# Calculate z-score transform (value - mean / std) and add as column to data frame

preact = AP_grouped_data %>% 
  filter(preactivation)

preact_sum = preact %>% 
  filter(measure == 'vel') %>% 
  group_by(embryo.id, position) %>% 
  summarise(std_pop = sqrt(sum(std^2) / n()), 
            std_pop_bio = sd(mean),
            mean_pop = mean(mean)) %>% 
  select(embryo.id, position, std_pop, std_pop_bio, mean_pop)

z_score_df = AP_grouped_data %>% 
  left_join(preact_sum, by = c('embryo.id', 'position'), keep = FALSE)

z_score_vel = z_score_df %>% 
  filter(measure == 'vel') %>% 
  mutate(z_score_vel = (mean - mean_pop)/ std_pop) %>%        #z_score uses mean of the std before activation = variation due to preprocessing and effect on PIV analysis
  mutate(z_score_bio_vel = (mean - mean_pop )/ std_pop_bio) %>%  #z_score_bio uses std of the velocity before activation = biological variation in the velocity in unactivated state
  select(embryo.id, position, timepoint, z_score_vel, z_score_bio_vel)

z_score_df = z_score_df %>% 
  left_join(z_score_vel, by = c('embryo.id', 'position', 'timepoint'), keep = FALSE)



# Plot z_score
# z-score shows the difference of the velocity from the preactivation mean
# if over 2 equivalent to p=0.05 significant difference from the mean before activation


(
  z_score_df %>% 
    filter(measure == 'vel') %>% 
    ggplot(aes(x = timepoint, y = z_score_vel)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "brown") +
    annotate("text", x = 60, y = 0.02, label = "ON", size = 2) +
    geom_line() +
    theme_bw() +
    facet_grid(rows = vars(direction, embryo.id),
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity z-score")))
)

# Plot velocity normalised by std during preactivation (SNR)
# Here looking at the signal is relevant e.g. peak value

(
SNR = z_score_df %>% 
  filter(measure == 'vel') %>% 
  ggplot(aes(x = timepoint, y = mean / std_pop)) +
  annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "brown") +
  annotate("text", x = 60, y = 0.02, label = "ON", size = 2) +
  geom_ribbon(aes(ymin = ((mean - std) / std_pop), ymax = ((mean + std) / std_pop)), alpha = 0.3) +
  geom_line() +
  theme_bw() +
  facet_grid(rows = vars(direction, embryo.id),
             cols = vars(position)) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Time [s]') +
  ylab(expression(paste("Velocity / preactivation std")))
)


# threshold the peak z-score in the activation period >2

peak_zscore_during_act <- z_score_df %>%
  filter(measure == 'vel', 
         (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S'),
         timepoint >= 0, timepoint <= 45) %>%
  group_by(embryo.id) %>%
  slice_max(order_by = z_score_vel) %>% 
  rename(peak_z_score = z_score_vel,
         peak_tp = timepoint) %>% 
  select(embryo.id, peak_tp, peak_z_score)

z_score_df <- z_score_df %>% 
  left_join(peak_zscore_during_act, by = 'embryo.id')

thresholded_data <- z_score_df %>% 
  filter(peak_z_score >= 2) # this removed 2 embryos

## here is a good place to write the data frame to a new file

# Combining thresholded embryos
# Averaged into anterior and posterior by average vang --------------

averaged_data_1 = thresholded_data %>% 
  group_by(timepoint, measure, position, direction) %>% 
  filter(!measure %in% c('vang', 'aniang')) %>% 
  summarize(mean = mean(mean),
            std = sqrt(sum(std^2) / n()))

averaged_data_2 = thresholded_data %>% 
  group_by(timepoint, measure, position, direction) %>% 
  filter(measure %in% c('vang', 'aniang')) %>% 
  mutate(mean = as.circular(mean, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  summarize(mean = as.double(mean.circular(mean))) %>% 
  mutate(mean = as.double(mean * (180 / pi)))

averaged_data <- as.data.frame(full_join(averaged_data_1, averaged_data_2))        

# Plotting averaged data ------

plot_names <- as_labeller(c(`anterior` = 'Anterior & N', `posterior` = 'Posterior & S'))

(
  vel <- averaged_data %>% 
    filter(measure == 'vel', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 0.011, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    # geom_line(data = thresholded_data %>%
    #             filter(measure == 'vel') %>%
    #           aes(x = timepoint, y = mean, colour = embryo.id),
    #           alpha = 0.5) +
    facet_grid(rows = vars(direction), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)

(
  vel_N_S <- averaged_data %>% 
    filter(measure == 'vel', (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S')) %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 0.011, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_wrap(~direction, labeller = plot_names) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)

(
  vang <- averaged_data %>% 
    filter(measure == 'vang', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(direction), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity Direction [º]")))
)

(
  vang_N_S <- averaged_data %>% 
    filter(measure == 'vang', (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S')) %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    facet_wrap(~direction, labeller = plot_names) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity Direction [º]")))
)

(
  iso <- averaged_data %>% 
    filter(measure == 'iso', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 2e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(direction), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Isotropic strain rate [", s^-1, "]")))
)

(
  iso_N_S <- averaged_data %>% 
    filter(measure == 'iso', (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S')) %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 2e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_wrap(~direction, labeller = plot_names) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Isotropic strain rate [", s^-1, "]")))
)

(
  ani <- averaged_data %>% 
    filter(measure == 'ani', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 3.5e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(direction), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic strain rate [", s^-1, "]")))
)

(
  ani_N_S <- averaged_data %>% 
    filter(measure == 'ani', (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S')) %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 3.5e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_wrap(~direction, labeller = plot_names) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic strain rate [", s^-1, "]")))
)


(
  aniang <- averaged_data %>% 
    filter(measure == 'aniang', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(direction), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic Strain Direction [º]")))
  
)

(
  aniang_N_S <- averaged_data %>% 
    filter(measure == 'aniang', (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S')) %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    facet_wrap(~direction, labeller = plot_names) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic Strain Direction [º]")))
  
)

filename <- paste(Sys.Date(), 'threshold_averaged_plots', sep = "_")

pdf(sprintf('results/%s.pdf', filename), height = 4, width = 7) #number is inches 4 and 7 convert to 16:9 ratio
vel
vang
iso
ani
aniang
vel_N_S
vang_N_S
iso_N_S
ani_N_S
aniang_N_S
dev.off()

pdf2pptx(sprintf('results/%s.pdf', filename), sprintf('results/%s.pptx', filename), ratio = 169)





