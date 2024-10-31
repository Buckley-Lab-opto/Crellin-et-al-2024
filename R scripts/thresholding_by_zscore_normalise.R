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
library(patchwork)
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



# Plot z_score -----
# z-score shows the difference of the velocity from the preactivation mean
# if over 2 equivalent to p=0.05 significant difference from the mean before activation


(
  z_score_ant <- z_score_df %>% 
    filter(measure == 'vel', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = z_score_vel)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "brown") +
    annotate("text", x = 60, y = 6, label = "ON", size = 2) +
    geom_line() +
    geom_hline(yintercept = 2, linetype = 'dashed', alpha = 0.5, size = 0.2) +
    ylim(-2, 7.5) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id),
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity z-score")))
)

(
  z_score_post <- z_score_df %>% 
    filter(measure == 'vel', direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = z_score_vel)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "brown") +
    annotate("text", x = 60, y = 0.02, label = "ON", size = 2) +
    geom_line() +
    geom_hline(yintercept = 2, linetype = 'dashed', alpha = 0.5, size = 0.2) +
    theme_bw() +
    ylim(-2, 7.5) +
    facet_grid(rows = vars(embryo.id),
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity z-score")))
)


filename <- paste(Sys.Date(), 'z-score_velocity', sep = "_")

pdf(sprintf('results/%s.pdf', filename), height = 9, width = 16) #number is inches 4 and 7 convert to 16:9 ratio
z_score_ant + (z_score_post + plot_layout(nrow = 2))
dev.off()

pdf2pptx(sprintf('results/%s.pdf', filename), sprintf('results/%s.pptx', filename), ratio = 169)


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


# Combining thresholded embryos ------
# Averaged into anterior and posterior by average vang --------------

averaged_data_1 = thresholded_data %>% 
  group_by(timepoint, measure, position, direction) %>% 
  filter(!measure %in% c('vang', 'aniang')) %>% 
  summarize(mean = mean(mean),
            std = sqrt(sum(std^2) / n()),
            SEM = std / sqrt(length(unique(embryo.id))))

averaged_data_2 = thresholded_data %>% 
  group_by(timepoint, measure, position, direction) %>% 
  filter(measure %in% c('vang', 'aniang')) %>% 
  mutate(mean = as.circular(mean, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  summarize(mean = as.double(mean.circular(mean)),
            std = mean(std)) %>%  # need to work out if this is valid way to do this with circ data
  mutate(mean = as.double(mean * (180 / pi)),
         std = std * (180 / pi))

averaged_data <- as.data.frame(full_join(averaged_data_1, averaged_data_2))        

# Plotting averaged data ------

plot_names <- as_labeller(c(`anterior` = 'Anterior & N', `posterior` = 'Posterior & S'))

(
  vel <- averaged_data %>% 
    filter(measure == 'vel', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 0.011, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - SEM), ymax = (mean + SEM)), fill = "black", alpha = 0.3) +
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
    geom_ribbon(aes(ymin = (mean - SEM), ymax = (mean + SEM)), fill = "black", alpha = 0.3) +
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
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    
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
  str <- averaged_data %>% 
    filter(measure == 'str', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 2e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - SEM), ymax = (mean + SEM)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(direction), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Total strain rate [", s^-1, "]")))
)

(
  str_N_S <- averaged_data %>% 
    filter(measure == 'str', (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S')) %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 2e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - SEM), ymax = (mean + SEM)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_wrap(~direction, labeller = plot_names) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Total strain rate [", s^-1, "]")))
)

(
  iso <- averaged_data %>% 
    filter(measure == 'iso', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 2e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - SEM), ymax = (mean + SEM)), fill = "black", alpha = 0.3) +
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
    geom_ribbon(aes(ymin = (mean - SEM), ymax = (mean + SEM)), fill = "black", alpha = 0.3) +
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
    geom_ribbon(aes(ymin = (mean - SEM), ymax = (mean + SEM)), fill = "black", alpha = 0.3) +
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
    geom_ribbon(aes(ymin = (mean - SEM), ymax = (mean + SEM)), fill = "black", alpha = 0.3) +
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
str
iso
ani
aniang
vel_N_S
vang_N_S
str_N_S
iso_N_S
ani_N_S
aniang_N_S
dev.off()

pdf2pptx(sprintf('results/%s.pdf', filename), sprintf('results/%s.pptx', filename), ratio = 169)


# Plotting individual embryos  ------


(
  vel_ant <- thresholded_data %>% 
    filter(measure == 'vel', direction == 'anterior', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 0.011, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    ylim(-0, 0.02) +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)

(
  vel_post <- thresholded_data %>% 
    filter(measure == 'vel', direction == 'posterior', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 0.011, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    ylim(-0, 0.02) +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)



(
  vang_ant <- thresholded_data %>% 
    filter(measure == 'vang', direction == 'anterior', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = (mean * (180 / pi)))) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity Direction [º]")))
)

(
  vang_post <- thresholded_data %>% 
    filter(measure == 'vang', direction == 'posterior', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = (mean * (180 / pi)))) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity Direction [º]")))
)

(
  str_ant <- thresholded_data %>% 
    filter(measure == 'str', direction == 'anterior', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 2e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    ylim(0e+00, 1.5e-03) +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Total strain rate [", s^-1, "]")))
)
(
  str_post <- thresholded_data %>% 
    filter(measure == 'str', direction == 'posterior', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 2e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    ylim(0e+00, 1.5e-03) +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Total strain rate [", s^-1, "]")))
)

(
  iso_ant <- thresholded_data %>% 
    filter(measure == 'iso', direction == 'anterior', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 2e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    ylim(-0.0005, 0.0005) +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Isotropic strain rate [", s^-1, "]")))
)
(
  iso_post <- thresholded_data %>% 
    filter(measure == 'iso', direction == 'posterior', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 2e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    ylim(-0.0005, 0.0005) +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Isotropic strain rate [", s^-1, "]")))
)

(
  ani_ant <- thresholded_data %>% 
    filter(measure == 'ani', direction == 'anterior', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 3.5e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    ylim(0, 6e-04) +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic strain rate [", s^-1, "]")))
)


(
  ani_post <- thresholded_data %>% 
    filter(measure == 'ani', direction == 'posterior', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 3.5e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    ylim(0, 6e-04) +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic strain rate [", s^-1, "]")))
)



(
  aniang_ant <- thresholded_data %>% 
    filter(measure == 'aniang', direction == 'anterior', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = (mean * (180 / pi)))) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic Strain Direction [º]")))
  
)

(
  aniang_post <- thresholded_data %>% 
    filter(measure == 'aniang', direction == 'posterior', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = (mean * (180 / pi)))) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic Strain Direction [º]")))
  
)


filename <- paste(Sys.Date(), 'threshold_individual_plots', sep = "_")

pdf(sprintf('results/%s.pdf', filename), height = 9, width = 16) #number is inches 4 and 7 convert to 16:9 ratio
vel_ant + (vel_post + plot_layout(nrow = 2))
vang_ant + (vang_post + plot_layout(nrow = 2))
str_ant + (str_post + plot_layout(nrow = 2))
iso_ant + (iso_post + plot_layout(nrow = 2))
ani_ant + (ani_post + plot_layout(nrow = 2))
aniang_ant + (aniang_post + plot_layout(nrow = 2))
dev.off()

pdf2pptx(sprintf('results/%s.pdf', filename), sprintf('results/%s.pptx', filename), ratio = 169)


# Individual embryos to show clare - high responders -----
# anterior "20221018_E1_15ss" "20221108_E8_15ss" "20221214_E5_12ss" 
# posterior "20221214_E3_14ss"

# anterior "20221018_E1_15ss" ------

(
  vel_1 <- thresholded_data %>% 
    filter(measure == 'vel', embryo.id == '20221018_E1_15ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 0.011, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)

(
  vang_1 <- thresholded_data %>% 
    filter(measure == 'vang', embryo.id == '20221018_E1_15ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = (mean * (180 / pi)))) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity Direction [º]")))
)

(
  str_1 <- thresholded_data %>% 
    filter(measure == 'str', embryo.id == '20221018_E1_15ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 2e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("total strain rate [", s^-1, "]")))
)

(
  iso_1 <- thresholded_data %>% 
    filter(measure == 'iso', embryo.id == '20221018_E1_15ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 2e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Isotropic strain rate [", s^-1, "]")))
)

(
  ani_1 <- thresholded_data %>% 
    filter(measure == 'ani', embryo.id == '20221018_E1_15ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 3.5e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic strain rate [", s^-1, "]")))
)

(
  aniang_1 <- thresholded_data %>% 
    filter(measure == 'aniang', embryo.id == '20221018_E1_15ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = (mean * (180 / pi)))) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic Strain Direction [º]")))
  
)
# anterior  "20221108_E8_15ss" -----
(
  vel_2 <- thresholded_data %>% 
    filter(measure == 'vel', embryo.id == '20221108_E8_15ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 0.011, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)

(
  vang_2 <- thresholded_data %>% 
    filter(measure == 'vang', embryo.id == '20221108_E8_15ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = (mean * (180 / pi)))) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity Direction [º]")))
)

(
  str_2 <- thresholded_data %>% 
    filter(measure == 'str', embryo.id == '20221108_E8_15ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 2e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("total strain rate [", s^-1, "]")))
)

(
  iso_2 <- thresholded_data %>% 
    filter(measure == 'iso', embryo.id == '20221108_E8_15ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 2e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Isotropic strain rate [", s^-1, "]")))
)

(
  ani_2 <- thresholded_data %>% 
    filter(measure == 'ani', embryo.id == '20221108_E8_15ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 3.5e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic strain rate [", s^-1, "]")))
)

(
  aniang_2 <- thresholded_data %>% 
    filter(measure == 'aniang', embryo.id == '20221108_E8_15ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = (mean * (180 / pi)))) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic Strain Direction [º]")))
  
)

# anterior "20221214_E5_12ss" -----
(
  vel_3 <- thresholded_data %>% 
    filter(measure == 'vel', embryo.id == '20221214_E5_12ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 0.011, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)

(
  vang_3 <- thresholded_data %>% 
    filter(measure == 'vang', embryo.id == '20221214_E5_12ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = (mean * (180 / pi)))) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity Direction [º]")))
)

(
  str_3 <- thresholded_data %>% 
    filter(measure == 'str', embryo.id == '20221214_E5_12ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 2e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("total strain rate [", s^-1, "]")))
)



(
  iso_3 <- thresholded_data %>% 
    filter(measure == 'iso', embryo.id == '20221214_E5_12ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 2e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Isotropic strain rate [", s^-1, "]")))
)

(
  ani_3 <- thresholded_data %>% 
    filter(measure == 'ani', embryo.id == '20221214_E5_12ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 3.5e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic strain rate [", s^-1, "]")))
)

(
  aniang_3 <- thresholded_data %>% 
    filter(measure == 'aniang', embryo.id == '20221214_E5_12ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = (mean * (180 / pi)))) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic Strain Direction [º]")))
  
)

# posterior "20221214_E3_14ss"----------

(
  vel_4  <- thresholded_data %>% 
    filter(measure == 'vel', embryo.id == '20221214_E3_14ss', position == 'S') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 0.011, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)

(
  vang_4  <- thresholded_data %>% 
    filter(measure == 'vang', embryo.id == '20221214_E3_14ss', position == 'S') %>% 
    ggplot(aes(x = timepoint, y = (mean * (180 / pi)))) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity Direction [º]")))
)

(
  str_4 <- thresholded_data %>% 
    filter(measure == 'str', embryo.id == '20221214_E3_14ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 2e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("total strain rate [", s^-1, "]")))
)

(
  iso_4  <- thresholded_data %>% 
    filter(measure == 'iso', embryo.id == '20221214_E3_14ss', position == 'S') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 2e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Isotropic strain rate [", s^-1, "]")))
)

(
  ani_4  <- thresholded_data %>% 
    filter(measure == 'ani', embryo.id == '20221214_E3_14ss', position == 'S') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 3.5e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic strain rate [", s^-1, "]")))
)

(
  aniang_4  <- thresholded_data %>% 
    filter(measure == 'aniang', embryo.id == '20221214_E3_14ss', position == 'S') %>% 
    ggplot(aes(x = timepoint, y = (mean * (180 / pi)))) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic Strain Direction [º]")))
  
)

# threshold_individual_embryos plots -----

filename <- paste(Sys.Date(), 'threshold_individual_embryos', sep = "_")

pdf(sprintf('results/%s.pdf', filename), height = 4, width = 7) #number is inches 4 and 7 convert to 16:9 ratio
vel_1
vang_1
str_1
iso_1
ani_1
aniang_1
vel_2
vang_2
str_2
iso_2
ani_2
aniang_2
vel_3
vang_3
str_3
iso_3
ani_3
aniang_3
vel_4
vang_4
str_4
iso_4
ani_4
aniang_4
dev.off()

pdf2pptx(sprintf('results/%s.pdf', filename), sprintf('results/%s.pptx', filename), ratio = 169)


# Normalised velocity ------


# Normalised by peak in each position

# peak_velocity <- thresholded_data %>%
#   filter(measure == 'vel', timepoint >= 0, timepoint <= 125) %>%
#   group_by(embryo.id, position) %>%
#   slice_max(order_by = mean) %>%
#   rename(peak_vel = mean,
#          peak_vel_tp = timepoint) %>% 
#   select(embryo.id, peak_vel_tp, peak_vel, position)
# 
# normalised_data = thresholded_data %>% 
#   left_join(peak_velocity, by = c('embryo.id', 'position'), keep = FALSE)
# 
# normalised_data = normalised_data %>% 
#   group_by(timepoint, measure, position, direction) %>% 
#   filter(measure == 'vel') %>% 
#   mutate(normalised_mean = (mean / peak_vel) )

# Normalised by peak in N or S

peak_velocity_all_vel <- thresholded_data %>%
  filter(measure == 'vel', position %in% c('N', 'S'), timepoint >= 0, timepoint <= 125) %>%
  group_by(embryo.id) %>%
  slice_max(order_by = mean) %>%
  rename(peak_vel = mean,
         peak_vel_tp = timepoint,
         position_peak = position) %>% 
  select(embryo.id, peak_vel_tp, peak_vel, position_peak)

normalised_data_vel = thresholded_data %>% 
  left_join(peak_velocity_all, by = c('embryo.id'), keep = FALSE)

normalised_data_vel = normalised_data_1 %>% 
  group_by(timepoint, measure, position, direction) %>% 
  filter(measure == 'vel') %>% 
  mutate(normalised_mean = (mean / peak_vel) * 100)

peak_str_all <- thresholded_data %>%
  filter(measure == 'str', position %in% c('N', 'S'), timepoint >= 0, timepoint <= 125) %>%
  group_by(embryo.id) %>%
  slice_max(order_by = mean) %>%
  rename(peak_str = mean,
         peak_str_tp = timepoint,
         position_peak_str = position) %>% 
  select(embryo.id, peak_str_tp, peak_str, position_peak_str)

normalised_data_str = thresholded_data %>% 
  left_join(peak_str_all, by = c('embryo.id'), keep = FALSE)

normalised_data_str = normalised_data_str %>% 
  group_by(timepoint, measure, position, direction) %>% 
  filter(measure == 'str') %>% 
  mutate(normalised_str = (mean / peak_str) * 100)

(
  vel_ant <- normalised_data_vel %>% 
    filter(measure == 'vel',  !position == 'W', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = normalised_mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 150, label = "ON", size = 4) +
   # geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_hline(yintercept = 100, linetype = 'dashed') +
    geom_line(size = 1) +
    theme_bw() +
    ylim(0, 160) +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity (% of peak activation velocity)")))
)

(
  vel_post <- normalised_data_vel %>% 
    filter(measure == 'vel',  !position == 'W', direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = normalised_mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 150, label = "ON", size = 4) +
    # geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_hline(yintercept = 100, linetype = 'dashed') +
    geom_line(size = 1) +
    theme_bw() +
    ylim(0, 160) +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity (% of peak activation velocity)")))
)

(
  str_ant <- normalised_data_str %>% 
    filter(measure == 'str',  !position == 'W', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = normalised_str)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 150, label = "ON", size = 4) +
    # geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_hline(yintercept = 100, linetype = 'dashed') +
    geom_line(size = 1) +
    theme_bw() +
    ylim(0, 170) +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("% total strain rate of peak total strain")))
)

(
  str_post <- normalised_data_str %>% 
    filter(measure == 'str',  !position == 'W', direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = normalised_str)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 150, label = "ON", size = 4) +
    geom_hline(yintercept = 100, linetype = 'dashed') +
    geom_line(size = 1) +
    theme_bw() +
    ylim(0, 170) +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("% total strain rate of peak total strain")))
  )


(
  vel_ant_N <- normalised_data_vel %>% 
    filter(measure == 'vel',  position == 'N', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = normalised_mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 150, label = "ON", size = 4) +
    # geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_hline(yintercept = 100, linetype = 'dashed') +
    geom_line(size = 1) +
    theme_bw() +
    ylim(0, 160) +
    facet_wrap(~embryo.id) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity (% of peak activation velocity)")))
)

(
  vel_post_S <- normalised_data_vel %>% 
    filter(measure == 'vel',  position == 'S', direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = normalised_mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 150, label = "ON", size = 4) +
    # geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_hline(yintercept = 100, linetype = 'dashed') +
    geom_line(size = 1) +
    theme_bw() +
    ylim(0, 160) +
    facet_wrap(~embryo.id) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity (% of peak activation velocity)")))
)

(
  str_ant_N <- normalised_data_str %>% 
    filter(measure == 'str',  position == 'N', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = normalised_str)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 150, label = "ON", size = 4) +
    # geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_hline(yintercept = 100, linetype = 'dashed') +
    geom_line(size = 1) +
    theme_bw() +
    ylim(0, 170) +
    facet_wrap(~embryo.id) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("% total strain rate of peak total strain")))
)

(
  str_post_S <- normalised_data_str %>% 
    filter(measure == 'str',  position == 'S', direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = normalised_str)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 150, label = "ON", size = 4) +
    geom_hline(yintercept = 100, linetype = 'dashed') +
    geom_line(size = 1) +
    theme_bw() +
    ylim(0, 170) +
    facet_wrap(~embryo.id) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("% total strain rate of peak total strain")))
)

filename <- paste(Sys.Date(), 'normalised_plots', sep = "_")

pdf(sprintf('results/%s.pdf', filename), height = 9, width = 16) #number is inches 4 and 7 convert to 16:9 ratio
vel_ant + (vel_post + plot_layout(nrow = 2))
str_ant + (str_post + plot_layout(nrow = 2))
vel_ant_N + (vel_post_S + plot_layout(nrow = 2))
str_ant_N + (str_post_S + plot_layout(nrow = 2))
dev.off()

pdf2pptx(sprintf('results/%s.pdf', filename), sprintf('results/%s.pptx', filename), ratio = 169)


# width of velocity peak ------

diff <- averaged_data %>% 
  filter(measure == 'vel', timepoint >= 0, timepoint <=120, (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S')) %>% 
  select(timepoint, mean) %>% 
  group_by(timepoint) %>%
  summarise(mean = mean(mean)) %>% 
  mutate(diff_mean = mean - lag(mean))

peaks <- which(diff$diff_mean < 0)
troughs <- which(diff$diff_mean > 0)
  
