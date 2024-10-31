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


# Finding peak velocity by position and adding the axial direction of peak velocity ----------------------------------------------
## is this right - should i actually use vang?? - as I'm making the assumption that the position is the same as direction 

peak_velocity_during_act <- data %>%
  filter(measure == 'vel', !(position %in% c('C')), timepoint >= 0, timepoint <= 125) %>%
  group_by(embryo.id) %>%
  slice_max(order_by = mean)

peak_velocity_axial_direction <- peak_velocity_during_act %>% 
  mutate(direction = case_when(
    position == 'N' ~ 'anterior',
    position == 'S' ~ 'posterior',
    position == 'E' ~ 'medial',
    position == 'W' ~ 'lateral')) %>% 
  select(embryo.id, direction)

directed_data <- data %>%
  left_join(peak_velocity_axial_direction, by = 'embryo.id', keep = FALSE) %>% 
  relocate(direction, .before=position)


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


angles <- seq(-180, 180, by = 45)
angles <- as.data.frame(angles)

bars <- data.frame(angle = c(90, -90),
                   direction = c('posterior', 'anterior'))

ggplot() +
  geom_col(data = bars, aes(x = angle, y = 1, fill = direction), width = 90) +
  geom_point(data = angles, aes(x = angles, y = 1), size = 0.1) +
  geom_col(data = angles_wm_by_position, aes(x = weighted_mean, y = 0.75), width = 1) +
  coord_polar(theta = "x", start = 1.57, direction = -1) + 
  scale_x_reverse(breaks = seq(180, -180, -45)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank()) +
  labs(title = "Reference for vector direction by degrees") +
  facet_wrap(~position)

ggplot() +
  geom_col(data = bars, aes(x = angle, y = 1, fill = direction), width = 90) +
  geom_point(data = angles, aes(x = angles, y = 1), size = 0.1) +
  geom_col(data = angles_wm_all, aes(x = weighted_mean_vang, y = 0.75), width = 1) +
  coord_polar(theta = "x", start = 1.57, direction = -1) + 
  scale_x_reverse(breaks = seq(180, -180, -45)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank()) +
  labs(title = "Reference for vector direction by degrees") 

angles_wm_all %>% 
  ggplot() +
  annotate("text", x = -90, y = 0.5, label = "Anterior", size = 5) +
  annotate("text", x = 90, y = 0.5, label = "Posterior", size = 5) +
  geom_segment(aes(x = wm_vang_first45, y = 0, xend = wm_vang_first45, yend = mean_vel_first45*60), size = 1, arrow = arrow(length = unit(0.4, "cm"))) +
  geom_point(data = angles, aes(x = angles, y = 0.5), size = 0.01, alpha = 0.01) +
  coord_polar(theta = "x", start = 1.57, direction = -1) +  
  scale_x_reverse(breaks = seq(180, -180, -45)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank()) +
  labs(title = "Average vector direction in the first 45s after activation
(average of all regions)") 


angles_wm_all %>% 
  filter(direction == 'anterior') %>% 
  ggplot() +
  annotate("text", x = -90, y = 0.4, label = "Anterior", size = 5) +
  annotate("text", x = 90, y = 0.4, label = "Posterior", size = 5) +
  geom_segment(aes(x = wm_vang_before, y = 0, xend = wm_vang_before, yend = mean_vel_before*60, colour = embryo.id), size = 1, arrow = arrow(length = unit(0.4, "cm"))) +
  geom_point(data = angles, aes(x = angles, y = 0.4), alpha = 0.01) +
  coord_polar(theta = "x", start = 1.57, direction = -1) +  
  scale_x_reverse(breaks = seq(180, -180, -45)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank()) +
  labs(title = "Average vector direction in the 45s before activation
(filtered by anterior moving, average of all regions)") 

angles_wm_by_position %>% 
  filter(position == 'N', direction == 'anterior', period == 'before') %>% 
  ggplot() +
  annotate("text", x = -90, y = 0.6, label = "Anterior", size = 5) +
  annotate("text", x = 90, y = 0.6, label = "Posterior", size = 5) +
  geom_segment(aes(x = weighted_mean, y = 0, xend = weighted_mean, yend = mean_vel*60, colour = embryo.id), size = 1, arrow = arrow(length = unit(0.4, "cm"))) +
  geom_point(data = angles, aes(x = angles, y = 0.6), alpha = 0.01) +
  coord_polar(theta = "x", start = 1.57, direction = -1) +  
  scale_x_reverse(breaks = seq(180, -180, -45)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank()) +
  labs(title = "Anterior group: Average vector direction in the north region, 45s before activation") 



angles_wm_by_position %>% 
  filter(position == 'S', direction == 'posterior', period == 'before') %>% 
  ggplot() +
  annotate("text", x = -90, y = 0.6, label = "Anterior", size = 5) +
  annotate("text", x = 90, y = 0.6, label = "Posterior", size = 5) +
  geom_segment(aes(x = weighted_mean, y = 0, xend = weighted_mean, yend = mean_vel*60, colour = embryo.id), size = 1, arrow = arrow(length = unit(0.4, "cm"))) +
  geom_point(data = angles, aes(x = angles, y = 0.6), alpha = 0.01) +
  coord_polar(theta = "x", start = 1.57, direction = -1) +  
  scale_x_reverse(breaks = seq(180, -180, -45)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank()) +
  labs(title = "Posterior group: Average vector direction in the south region, 45s before activation") 



angles_wm_all %>% 
  filter(direction == 'anterior') %>% 
  ggplot() +
  annotate("text", x = -90, y = 0.4, label = "Anterior", size = 5) +
  annotate("text", x = 90, y = 0.4, label = "Posterior", size = 5) +
  geom_segment(aes(x = wm_vang_first45, y = 0, xend = wm_vang_first45, yend = mean_vel_first45*60, colour = embryo.id), size = 1, arrow = arrow(length = unit(0.4, "cm"))) +
  geom_point(data = angles, aes(x = angles, y = 0.4), alpha = 0.01) +
  coord_polar(theta = "x", start = 1.57, direction = -1) +  
  scale_x_reverse(breaks = seq(180, -180, -45)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank()) +
  labs(title = "Average vector direction in the first 45s after activation") 

angles_wm_by_position %>% 
  filter(position == 'N', direction == 'anterior', period == 'during') %>% 
  ggplot() +
  annotate("text", x = -90, y = 0.6, label = "Anterior", size = 5) +
  annotate("text", x = 90, y = 0.6, label = "Posterior", size = 5) +
  geom_segment(aes(x = weighted_mean, y = 0, xend = weighted_mean, yend = mean_vel*60, colour = embryo.id), size = 1, arrow = arrow(length = unit(0.4, "cm"))) +
  geom_point(data = angles, aes(x = angles, y = 0.6), alpha = 0.01) +
  coord_polar(theta = "x", start = 1.57, direction = -1) +  
  scale_x_reverse(breaks = seq(180, -180, -45)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank()) +
  labs(title = "Average vector direction in the north region, in the first 45s of activation") 

angles_wm_by_position %>% 
  filter(position == 'S', direction == 'posterior', period == 'during') %>% 
  ggplot() +
  annotate("text", x = -90, y = 0.6, label = "Anterior", size = 5) +
  annotate("text", x = 90, y = 0.6, label = "Posterior", size = 5) +
  geom_segment(aes(x = weighted_mean, y = 0, xend = weighted_mean, yend = mean_vel*60, colour = embryo.id), size = 1, arrow = arrow(length = unit(0.4, "cm"))) +
  geom_point(data = angles, aes(x = angles, y = 0.6), alpha = 0.01) +
  coord_polar(theta = "x", start = 1.57, direction = -1) +  
  scale_x_reverse(breaks = seq(180, -180, -45)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank()) +
  labs(title = "Posterior: Average vector direction in the south region, in the first 45s of activation") 


angles_wm_all %>% 
  filter(direction == 'posterior') %>% 
  ggplot() +
  annotate("text", x = -90, y = 0.4, label = "Anterior", size = 5) +
  annotate("text", x = 90, y = 0.4, label = "Posterior", size = 5) +
  geom_segment(aes(x = wm_vang_after, y = 0, xend = wm_vang_after, yend = mean_vel_after*60, colour = embryo.id), size = 1, arrow = arrow(length = unit(0.4, "cm"))) +
  geom_point(data = angles, aes(x = angles, y = 0.4), alpha = 0.01) +
  coord_polar(theta = "x", start = 1.57, direction = -1) +  
  scale_x_reverse(breaks = seq(180, -180, -45)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank()) +
  labs(title = "Average vector direction in the first 45s after deactivation") 

angles_wm_by_position %>% 
  filter(position == 'N', direction == 'anterior', period == 'after') %>% 
  ggplot() +
  annotate("text", x = -90, y = 0.6, label = "Anterior", size = 5) +
  annotate("text", x = 90, y = 0.6, label = "Posterior", size = 5) +
  geom_segment(aes(x = weighted_mean, y = 0, xend = weighted_mean, yend = mean_vel*60, colour = embryo.id), size = 1, arrow = arrow(length = unit(0.4, "cm"))) +
  geom_point(data = angles, aes(x = angles, y = 0.6), alpha = 0.01) +
  coord_polar(theta = "x", start = 1.57, direction = -1) +  
  scale_x_reverse(breaks = seq(180, -180, -45)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank()) +
  labs(title = "Anterior: Average vector direction in the north region, 45s after deactivation") 

angles_wm_by_position %>% 
  filter(position == 'S', direction == 'posterior', period == 'after') %>% 
  ggplot() +
  annotate("text", x = -90, y = 0.6, label = "Anterior", size = 5) +
  annotate("text", x = 90, y = 0.6, label = "Posterior", size = 5) +
  geom_segment(aes(x = weighted_mean, y = 0, xend = weighted_mean, yend = mean_vel*60, colour = embryo.id), size = 1, arrow = arrow(length = unit(0.4, "cm"))) +
  geom_point(data = angles, aes(x = angles, y = 0.6), alpha = 0.01) +
  coord_polar(theta = "x", start = 1.57, direction = -1) +  
  scale_x_reverse(breaks = seq(180, -180, -45)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank()) +
  labs(title = "Posterior: Average vector direction in the south region, 45s after deactivation") 


(ave_vang_direction <- data %>%
  filter(measure == 'vang', position != 'C', timepoint >= 0, timepoint <= 125) %>%
  group_by(embryo.id) %>% 
  summarise(mean = circular_mean(mean)))

ave_vang_direction_45s <- data %>%
  filter(measure == 'vang', position != 'C', timepoint >= 0, timepoint <= 45) %>%
  group_by(embryo.id) %>% 
  summarise(mean = circular_mean(mean))


(ave_vang_direction_in_peak <- directed_data %>%
  filter(direction == 'medial') %>% 
  filter(measure == 'vang', position == 'N', timepoint >= 0, timepoint <= 45) %>%
  group_by(embryo.id) %>% 
  summarise(mean = circular_mean(mean)))


# Plotting individual embryos ---------------------------------------

# Plot velocity against time with the y-axis scaled individually
# filtered by direction of movement e.g. anterior or posterior

(
  anterior_vel <-  directed_data %>% 
    filter(measure == 'vel',
           direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "brown") +
    annotate("text", x = 60, y = 0.02, label = "ON", size = 2) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line() +
    theme_bw() +
    facet_grid(rows = vars(direction, embryo.id),
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)


(
  posterior_vel <-  directed_data %>% 
    filter(measure == 'vel',
           direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "brown") +
    annotate("text", x = 60, y = 0.02, label = "ON", size = 2) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line() +
    theme_bw() +
    facet_grid(rows =vars(direction, embryo.id),
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)

(
  medial_vel <-  directed_data %>% 
    filter(measure == 'vel',
           direction == 'medial') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "brown") +
    annotate("text", x = 60, y = 0.02, label = "ON", size = 2) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line() +
    theme_bw() +
    facet_grid(rows = vars(direction, embryo.id),
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)

# plot velocity angle

radian_to_degree <- 180 / pi
anterior_degrees <- c(-135, -45)
posterior_degrees <- c(45, 135)

(
  anterior_vang <-  directed_data %>% 
    filter(measure == 'vang',
           direction == 'anterior') %>% 
    mutate(mean = mean * radian_to_degree,
           std = std * radian_to_degree) %>%
    ggplot(aes(x = timepoint, y = mean)) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate('rect', xmin = -Inf, xmax = Inf, ymin = anterior_degrees[1], ymax = anterior_degrees[2], alpha = 0.2, fill = 'black') +
    annotate('rect', xmin = -Inf, xmax = Inf, ymin = posterior_degrees[1], ymax = posterior_degrees[2], alpha = 0.2, fill = 'black') +
    geom_vline(aes(xintercept = align.activation.start), size = 0.3, colour = 'red') +
    geom_vline(aes(xintercept = 125), size = 0.3, colour = 'red') +
    geom_line() +
    # geom_ribbon(aes(x = timepoint, ymin = (mean - std), ymax=(mean + std)), fill="blue", alpha=0.2) +
    facet_grid(rows = vars(embryo.id),
               cols = vars(position)) +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab('Velocity Angle [º]')
)

(
  posterior_vang <-  directed_data %>% 
    filter(measure == 'vang',
           direction == 'posterior') %>% 
    mutate(mean = mean * radian_to_degree,
           std = std * radian_to_degree) %>%
    ggplot(aes(x = timepoint, y = mean)) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate('rect', xmin = -Inf, xmax = Inf, ymin = anterior_degrees[1], ymax = anterior_degrees[2], alpha = 0.2, fill = 'black') +
    annotate('rect', xmin = -Inf, xmax = Inf, ymin = posterior_degrees[1], ymax = posterior_degrees[2], alpha = 0.2, fill = 'black') +
    geom_vline(aes(xintercept = align.activation.start), size = 0.3, colour = 'red') +
    geom_vline(aes(xintercept = 125), size = 0.3, colour = 'red') +
    geom_line() +
    # geom_ribbon(aes(x = timepoint, ymin = (mean - std), ymax=(mean + std)), fill="blue", alpha=0.2) +
    facet_grid(rows = vars(embryo.id),
               cols = vars(position)) +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab('Velocity Angle [º]')
)

(
  medial_vang <-  directed_data %>% 
    filter(measure == 'vang',
           direction == 'medial') %>% 
    mutate(mean = mean * radian_to_degree,
           std = std * radian_to_degree) %>%
    ggplot(aes(x = timepoint, y = mean)) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate('rect', xmin = -Inf, xmax = Inf, ymin = anterior_degrees[1], ymax = anterior_degrees[2], alpha = 0.2, fill = 'black') +
    annotate('rect', xmin = -Inf, xmax = Inf, ymin = posterior_degrees[1], ymax = posterior_degrees[2], alpha = 0.2, fill = 'black') +
    geom_vline(aes(xintercept = align.activation.start), size = 0.3, colour = 'red') +
    geom_vline(aes(xintercept = 125), size = 0.3, colour = 'red') +
    geom_line() +
    # geom_ribbon(aes(x = timepoint, ymin = (mean - std), ymax=(mean + std)), fill="blue", alpha=0.2) +
    facet_grid(rows = vars(embryo.id),
               cols = vars(position)) +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab('Velocity Angle [º]')
)



# plot isotropic strain

(anterior_iso <-  directed_data %>% 
  filter(measure == 'iso',
         direction == 'anterior') %>% 
  ggplot(aes(x = timepoint, y = mean)) +
  annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "brown") +
  #annotate("text", x = 60, y = 0.001, label = "ON", size = 2) +
  geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
  geom_line() +
  theme_bw() +
  facet_grid(rows = vars(direction, embryo.id),
             cols = vars(position)) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Time [s]') +
  ylab('Isotropic strain rate')
)


# plot anisotropic strain

(anterior_ani <-  directed_data %>% 
    filter(measure == 'ani',
           direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "brown") +
    #annotate("text", x = 60, y = 0.001, label = "ON", size = 2) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line() +
    theme_bw() +
    facet_grid(rows = vars(direction, embryo.id),
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab('Anisotropic strain rate')
)

# plot anisotropic strain angle

(
  anterior_aniang <-  directed_data %>% 
  filter(measure == 'aniang',
         direction == 'anterior') %>% 
  mutate(mean = mean * radian_to_degree,
         std = std * radian_to_degree) %>%
  ggplot(aes(x = timepoint, y = mean)) +
  expand_limits(y = c(190, -190)) +
  scale_y_reverse(breaks = seq(180, -180, by = -45)) +
  annotate('rect', xmin = -Inf, xmax = Inf, ymin = anterior_degrees[1], ymax = anterior_degrees[2], alpha = 0.2, fill = 'black') +
  annotate('rect', xmin = -Inf, xmax = Inf, ymin = posterior_degrees[1], ymax = posterior_degrees[2], alpha = 0.2, fill = 'black') +
  geom_vline(aes(xintercept = align.activation.start), size = 0.3, colour = 'red') +
  geom_vline(aes(xintercept = 125), size = 0.3, colour = 'red') +
  geom_line() +
  # geom_ribbon(aes(x = timepoint, ymin = (mean - std), ymax=(mean + std)), fill="blue", alpha=0.2) +
  facet_grid(rows = vars(embryo.id),
             cols = vars(position)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Time [s]') +
  ylab('Anisotropic strain angle [º]')
)




pdf('direction_plots.pdf', height = 5, width = 5) #number is inches

dev.off()


# Averaged into anterior and posterior by average vang --------------

anterior_data_1 = AP_grouped_data %>% 
  filter(direction == 'anterior') %>%
  group_by(timepoint, measure, position, direction) %>% 
  filter(!measure %in% c('vang', 'aniang')) %>% 
  summarize(mean = mean(mean),
            std = sqrt(sum(std^2) / n()))

anterior_data_2 = AP_grouped_data %>% 
  filter(direction == 'anterior') %>%
  group_by(timepoint, measure, position, direction) %>% 
  filter(measure %in% c('vang', 'aniang')) %>% 
  mutate(mean = as.circular(mean, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  summarize(mean = as.double(mean.circular(mean))) %>% 
  mutate(mean = as.double(mean * (180 / pi)))

anterior_data <- full_join(anterior_data_1, anterior_data_2)         

posterior_data_1 = AP_grouped_data %>% 
  filter(direction == 'posterior') %>%
  group_by(timepoint, measure, position, direction) %>% 
  filter(!measure %in% c('vang', 'aniang')) %>% 
  summarize(mean = mean(mean),
            std = sqrt(sum(std^2) / n()))

posterior_data_2 = AP_grouped_data %>% 
  filter(direction == 'posterior') %>%
  group_by(timepoint, measure, position, direction) %>% 
  filter(measure %in% c('vang', 'aniang')) %>% 
  mutate(mean = as.circular(mean, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  summarize(mean = as.double(mean.circular(mean))) %>% 
  mutate(mean = as.double(mean * (180 / pi)))

posterior_data <- full_join(posterior_data_1, posterior_data_2)  

AP_averaged_data <- rbind(anterior_data, posterior_data)

# Averaged by direction ----------------------------------------------

averaged_ant = directed_data %>% 
  filter(direction == 'anterior') %>%
  group_by(timepoint, measure, position, direction) %>% 
  summarize(mean = mean(mean),
            std = sqrt(sum(std^2) / n()))

averaged_post = directed_data %>% 
  filter(direction == 'posterior') %>%
  group_by(timepoint, measure, position, direction) %>%
  summarize(mean = mean(mean),
            std = sqrt(sum(std^2) / n()))


# could have done groupby direction instead of filter and then wouldnt have need to make a new df

averaged_direction <- as.data.frame(rbind(averaged_ant, averaged_post))


# Plotting averaged data -------

measure_names <- list(
  'vel'='Velocity Magnitude',
  'vang'= 'Velocity Direction',
  'iso'= 'Isotropic Strain Rate',
  'ani'= 'Anisotropic Strain Rate',
  'aniang'= 'Anisotropic Strain Direction'
)

measure_labeller <- function(variable,value){
    return(measure_names[value])
  }

(
vel <- AP_averaged_data %>% 
  filter(measure == 'vel') %>% 
  ggplot(aes(x = timepoint, y = mean)) +
  annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
  annotate("text", x = 60, y = 0.011, label = "ON", size = 4) +
  geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
  geom_line() +
  theme_bw() +
  # geom_line(data = data %>% 
  #             filter(measure == 'vel') %>% 
  #           aes(x = timepoint, y = mean, colour = embryo),
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




# plot velocity angle
vang <-  averaged_direction %>% 
  filter(measure == 'vang') %>% 
  ggplot(aes(x = timepoint, y = mean)) +
  geom_vline(aes(xintercept = align.activation.start), size = 0.3) +   
  geom_vline(aes(xintercept = align.activation.end), size = 0.3) +
  geom_line() +
  theme_bw() +
  geom_line(aes(x = timepoint, y = ((mean - std))), linetype = 'dashed') + 
  geom_line(aes(x = timepoint, y = ((mean + std))), linetype = 'dashed') +
  facet_wrap(~direction + position, ncol=5) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(1, 'lines')) +
  xlab('Time') +
  ylab('Velocity angle')

# plot isotropic strain
iso <- averaged_direction %>% 
  filter(measure == 'iso') %>% 
  ggplot(aes(x = timepoint, y = mean)) +
  geom_vline(aes(xintercept = align.activation.start), size = 0.3) +   
  geom_vline(aes(xintercept = align.activation.end), size = 0.3) +
  geom_line() +
  theme_bw() +
  geom_line(aes(x = timepoint, y = ((mean - std))), linetype = 'dashed') + 
  geom_line(aes(x = timepoint, y = ((mean + std))), linetype = 'dashed') +
  facet_wrap(~direction + position, ncol=5) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(1, 'lines')) +
  #facet_wrap(~position + date + embryo) +
  xlab('Time') +
  ylab('Isotropic strain rate')

# plot anisotropic strain
ani <- averaged_direction %>% 
  filter(measure == 'ani') %>% 
  ggplot(aes(x = timepoint, y = mean)) +
  geom_vline(aes(xintercept = align.activation.start), size = 0.3) +   
  geom_vline(aes(xintercept = align.activation.end), size = 0.3) +
  geom_line() +
  theme_bw() +
  geom_line(aes(x = timepoint, y = ((mean - std))), linetype = 'dashed') + 
  geom_line(aes(x = timepoint, y = ((mean + std))), linetype = 'dashed') +
  facet_wrap(~direction + position, ncol=5) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(1, 'lines')) +
  xlab('Time') +
  ylab('Anisotropic strain rate')

# plot anisotropic strain angle
aniang <- averaged_direction %>%   
  filter(measure == 'aniang') %>% 
  ggplot(aes(x = timepoint, y = mean)) +
  geom_vline(aes(xintercept = align.activation.start), size = 0.3) +   
  geom_vline(aes(xintercept = align.activation.end), size = 0.3) +
  geom_line() +
  theme_bw() +
  geom_line(aes(x = timepoint, y = ((mean - std))), linetype = 'dashed') + 
  geom_line(aes(x = timepoint, y = ((mean + std))), linetype = 'dashed') +
  facet_wrap(~direction + position, ncol=5) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(1, 'lines')) +
  xlab('Time') +
  ylab('Anisotropic strain angle')



pdf('averaged_direction_plots_1.pdf', height = 4, width = 7) #number is inches
vel
vang
iso
ani
aniang
dev.off()

pdf2pptx('averaged_direction_plots_1.pdf', 'averaged_direction_plots_1.pptx')

# Plots for Clare ---------

(
  vel_ant_N <- AP_averaged_data %>% 
    filter(measure == 'vel', position == 'N', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 0.011, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)

(
  vang_ant_N <- AP_averaged_data %>% 
    filter(measure == 'vang', position == 'N', direction == 'anterior') %>% 
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
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity Direction [º]")))
)

(
  iso_ant_N <- AP_averaged_data %>% 
    filter(measure == 'iso', position == 'N', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 2e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Isotropic strain rate [", s^-1, "]")))
)

(
  ani_ant_N <- AP_averaged_data %>% 
    filter(measure == 'ani', position == 'N', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 3.5e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic strain rate [", s^-1, "]")))
)

(
  aniang_ant_N <- AP_averaged_data %>% 
    filter(measure == 'aniang', position == 'N', direction == 'anterior') %>% 
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
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic Strain Direction [º]")))
)

(
  vel_ant_N_ind <- AP_averaged_data %>% 
    filter(measure == 'vel', position == 'N', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 0.015, label = "ON", size = 4) +
    geom_line(data = AP_grouped_data %>%
                filter(measure == 'vel', position == 'N', direction == 'anterior'),
              aes(x = timepoint, y = mean, group = embryo.id),
              color = "grey",
              size = 0.5) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)

(
  vang_ant_N_ind <- AP_averaged_data %>% 
    filter(measure == 'vang', position == 'N', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_line(data = AP_grouped_data %>%
                filter(measure == 'vang', position == 'N', direction == 'anterior'),
              aes(x = timepoint, y = mean * (180/pi), group = embryo.id),
              color = "grey",
              size = 0.5) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity Direction [º]")))
)

(
  iso_ant_N_ind <- AP_averaged_data %>% 
    filter(measure == 'iso', position == 'N', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 4e-04, label = "ON", size = 4) +
    geom_line(data = AP_grouped_data %>%
                filter(measure == 'iso', position == 'N', direction == 'anterior'),
              aes(x = timepoint, y = mean, group = embryo.id),
              color = "grey",
              size = 0.5) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Isotropic strain rate [", s^-1, "]")))
)

(
  ani_ant_N_ind <- AP_averaged_data %>% 
    filter(measure == 'ani', position == 'N', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 4e-04, label = "ON", size = 4) +
    geom_line(data = AP_grouped_data %>%
                filter(measure == 'ani', position == 'N', direction == 'anterior'),
              aes(x = timepoint, y = mean, group = embryo.id),
              color = "grey",
              size = 0.5) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic strain rate [", s^-1, "]")))
)

(
  aniang_ant_N_ind <- AP_averaged_data %>% 
    filter(measure == 'aniang', position == 'N', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_line(data = AP_grouped_data %>%
                filter(measure == 'aniang', position == 'N', direction == 'anterior'),
              aes(x = timepoint, y = mean * (180/pi), group = embryo.id),
              color = "grey",
              size = 0.5) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic Strain Direction [º]")))
)


pdf('results/for-clare/anterior_average_N_plots.pdf', height = 4, width = 5) #number is inches
vel_ant_N
vang_ant_N
iso_ant_N
ani_ant_N
aniang_ant_N
vel_ant_N_ind
vang_ant_N_ind
iso_ant_N_ind
ani_ant_N_ind
aniang_ant_N_ind
dev.off()

pdf2pptx('results/for-clare/anterior_average_N_plots.pdf', 'results/for-clare/anterior_average_N_plots.pptx')


(
  vel_post_S <- AP_averaged_data %>% 
    filter(measure == 'vel', position == 'S', direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 0.009, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)

(
  vang_post_S <- AP_averaged_data %>% 
    filter(measure == 'vang', position == 'S', direction == 'posterior') %>% 
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
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity Direction [º]")))
)

(
  iso_post_S <- AP_averaged_data %>% 
    filter(measure == 'iso', position == 'S', direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 2e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Isotropic strain rate [", s^-1, "]")))
)

(
  ani_post_S <- AP_averaged_data %>% 
    filter(measure == 'ani', position == 'S', direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 3e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic strain rate [", s^-1, "]")))
)


(
  aniang_post_S <- AP_averaged_data %>% 
    filter(measure == 'aniang', position == 'S', direction == 'posterior') %>% 
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
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic Strain Direction [º]")))
)

(
  vel_post_S_ind <- AP_averaged_data %>% 
    filter(measure == 'vel', position == 'S', direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 0.014, label = "ON", size = 4) +
    geom_line(data = AP_grouped_data %>%
                filter(measure == 'vel', position == 'S', direction == 'posterior'),
              aes(x = timepoint, y = mean, group = embryo.id),
              color = "grey",
              size = 0.5) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)

(
  vang_post_S_ind <- AP_averaged_data %>% 
    filter(measure == 'vang', position == 'S', direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_line(data = AP_grouped_data %>%
                filter(measure == 'vang', position == 'S', direction == 'posterior'),
              aes(x = timepoint, y = mean * (180/pi), group = embryo.id),
              color = "grey",
              size = 0.5) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity Direction [º]")))
)

(
  iso_post_S_ind <- AP_averaged_data %>% 
    filter(measure == 'iso', position == 'S', direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y =2e-04, label = "ON", size = 4) +
    geom_line(data = AP_grouped_data %>%
                filter(measure == 'iso', position == 'S', direction == 'posterior'),
              aes(x = timepoint, y = mean, group = embryo.id),
              color = "grey",
              size = 0.5) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Isotropic strain rate [", s^-1, "]")))
)

(
  ani_post_S_ind <- AP_averaged_data %>% 
    filter(measure == 'ani', position == 'S', direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 3e-04, label = "ON", size = 4) +
    geom_line(data = AP_grouped_data %>%
                filter(measure == 'ani', position == 'S', direction == 'posterior'),
              aes(x = timepoint, y = mean, group = embryo.id),
              color = "grey",
              size = 0.5) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic strain rate [", s^-1, "]")))
)

(
  aniang_post_S_ind <- AP_averaged_data %>% 
    filter(measure == 'aniang', position == 'S', direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_line(data = AP_grouped_data %>%
                filter(measure == 'aniang', position == 'S', direction == 'posterior'),
              aes(x = timepoint, y = mean * (180/pi), group = embryo.id),
              color = "grey",
              size = 0.5) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic Strain Direction [º]")))
)


pdf('results/for-clare/posterior_average_S_plots.pdf', height = 4, width = 5) #number is inches
vel_post_S
vang_post_S
iso_post_S
ani_post_S
aniang_post_S
vel_post_S_ind
vang_post_S_ind
iso_post_S_ind
ani_post_S_ind
aniang_post_S_ind
dev.off()

pdf2pptx('results/for-clare/posterior_average_S_plots.pdf', 'results/for-clare/posterior_average_S_plots.pptx')

dev.off()

(
  vel_20221214_E3_14ss <- AP_grouped_data %>% 
    filter(embryo.id == '20221214_E3_14ss', measure == 'vel', position == 'S', direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 0.013, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)

(
  vang_20221214_E3_14ss <- AP_grouped_data %>% 
    filter(embryo.id == '20221214_E3_14ss', measure == 'vang', position == 'S', direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = (mean * (180/pi)))) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity Direction [º]")))
)

(
  iso_20221214_E3_14ss <- AP_grouped_data %>% 
    filter(embryo.id == '20221214_E3_14ss', measure == 'iso', position == 'S', direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 2e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Isotropic strain rate [", s^-1, "]")))
)

(
  ani_20221214_E3_14ss <- AP_grouped_data %>% 
    filter(embryo.id == '20221214_E3_14ss', measure == 'ani', position == 'S', direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 2.5e-04, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic strain rate [", s^-1, "]")))
)


(
  aniang_20221214_E3_14ss <- AP_grouped_data %>% 
    filter(embryo.id == '20221214_E3_14ss', measure == 'aniang', position == 'S', direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = mean * (180/pi))) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic Strain Direction [º]")))
)

pdf('results/for-clare/20221214_E3_14ss_S_plots.pdf', height = 4, width = 5) #number is inches
vel_20221214_E3_14ss 
vang_20221214_E3_14ss 
iso_20221214_E3_14ss 
ani_20221214_E3_14ss 
aniang_20221214_E3_14ss 
dev.off()

pdf2pptx('results/for-clare/20221214_E3_14ss_S_plots.pdf', 'results/for-clare/20221214_E3_14ss_S_plots.pptx')

dev.off()

# Filtered by Somite stage  ----------------------------------------------

# Plot velocity against time with the y-axis scaled individually

vel <-  data %>% 
  filter(date !='20220114',
         date != '20220715') %>% 
  filter(measure == 'vel') %>% 
  ggplot(aes(x = timepoint, y = mean)) +
  geom_vline(aes(xintercept = align.activation.start), size = 0.3) +   
  geom_vline(aes(xintercept = align.activation.end), size = 0.3) +
  geom_line() +
  theme_bw() +
  geom_line(aes(x = timepoint, y = ((mean - std))), linetype = 'dashed') + 
  geom_line(aes(x = timepoint, y = ((mean + std))), linetype = 'dashed') +
  #  facet_wrap(~position + date + embryo, ncol = 3) +
  facet_grid(rows = vars(somite, id),
             cols = vars(position)) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Time') +
  ylab('Veolcity')

# plot velocity angle
vang <-  data %>% 
  filter(date !='20220114',
         date != '20220715') %>% 
  filter(measure == 'vang') %>% 
  ggplot(aes(x = timepoint, y = mean)) +
  geom_vline(aes(xintercept = align.activation.start), size = 0.3) +   
  geom_vline(aes(xintercept = align.activation.end), size = 0.3) +
  geom_line() +
  theme_bw() +
  geom_line(aes(x = timepoint, y = ((mean - std))), linetype = 'dashed') + 
  geom_line(aes(x = timepoint, y = ((mean + std))), linetype = 'dashed') +
  facet_grid(rows = vars(somite, id),
             cols = vars(position)) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(1, 'lines')) +
  xlab('Time') +
  ylab('Velocity angle')

# plot isotropic strain
iso <- data %>% 
  filter(date !='20220114',
         date != '20220715') %>% 
  filter(measure == 'iso') %>% 
  ggplot(aes(x = timepoint, y = mean)) +
  geom_vline(aes(xintercept = align.activation.start), size = 0.3) +   
  geom_vline(aes(xintercept = align.activation.end), size = 0.3) +
  geom_line() +
  theme_bw() +
  geom_line(aes(x = timepoint, y = ((mean - std))), linetype = 'dashed') + 
  geom_line(aes(x = timepoint, y = ((mean + std))), linetype = 'dashed') +
  facet_grid(rows = vars(somite, id),
             cols = vars(position)) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(1, 'lines')) +
  #facet_wrap(~position + date + embryo) +
  xlab('Time') +
  ylab('Isotropic strain rate')

# plot anisotropic strain
ani <- data %>% 
  filter(date !='20220114',
         date != '20220715') %>% 
  filter(measure == 'ani') %>% 
  ggplot(aes(x = timepoint, y = mean)) +
  geom_vline(aes(xintercept = align.activation.start), size = 0.3) +   
  geom_vline(aes(xintercept = align.activation.end), size = 0.3) +
  geom_line() +
  theme_bw() +
  geom_line(aes(x = timepoint, y = ((mean - std))), linetype = 'dashed') + 
  geom_line(aes(x = timepoint, y = ((mean + std))), linetype = 'dashed') +
  facet_grid(rows = vars(somite, id),
             cols = vars(position)) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(1, 'lines')) +
  xlab('Time') +
  ylab('Anisotropic strain rate')

# plot anisotropic strain angle
aniang <- data %>%   
  filter(date !='20220114',
         date != '20220715') %>% 
  filter(measure == 'aniang') %>% 
  ggplot(aes(x = timepoint, y = mean)) +
  geom_vline(aes(xintercept = align.activation.start), size = 0.3) +   
  geom_vline(aes(xintercept = align.activation.end), size = 0.3) +
  geom_line() +
  theme_bw() +
  geom_line(aes(x = timepoint, y = ((mean - std))), linetype = 'dashed') + 
  geom_line(aes(x = timepoint, y = ((mean + std))), linetype = 'dashed') +
  facet_grid(rows = vars(somite, id),
             cols = vars(position)) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(1, 'lines')) +
  xlab('Time') +
  ylab('Anisotropic strain angle')

pdf('somite_plots.pdf', height = 15, width = 15) #number is inches
vel
vang
iso
ani
aniang
dev.off()

