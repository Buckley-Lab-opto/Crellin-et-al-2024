'01
Making a dataframe of embryo PIV data, with the direction of movement in the AP axis annotated'
# # = normal comments, '##' = notes to myself

# Load libraries
library(tidyverse)
library(openxlsx)
library(circular)
#library(pdf2pptx)
#library(ggquiver)
#library(viridis)  
#library(patchwork)
library(writexl)
#-------------------

## is this not needed when using git or when there is rproj?
# setwd('~/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis')

wd <- getwd()


# Read in data annotations / metadata
metadata <- read.xlsx('/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/light-patterning-experiment-metadata.xlsx')

# Create a data frame containing the PIV data (mean and std of different measures in different cardinal direction around the ROI)
file1 <- 'data/2022-10-18/E1_new_15ss_470nm_TL_region1.tif/xlsx_data/2024_Jun_17/2022-10-18_E1_new_15ss_470nm_TL_region1.xlsx'
file2 <- 'data/2022-11-08/E8_15ss_470nm_TL_.tif/xlsx_data/2024_Jun_17/2022-11-08_E8_15ss_470nm_TL_.xlsx'
file3 <- 'data/2022-12-14/E4_15ss_470nm_TL_/xlsx_data/2024_Jun_17/2022-12-14_E4_15ss_470nm_TL_.xlsx'
file4 <- 'data/2023-08-04/E1_approx21ss_470nm_TL_rotated/xlsx_data/2024_Jun_17/2023-08-04_E1_approx21ss_470nm_TL_rotated.xlsx'
file5 <- 'data/2023-09-01/E2_470nm_6m-TL_rotated/xlsx_data/2024_Jun_17/2023-09-01_E2_470nm_6m-TL_rotated.xlsx'
file6 <- 'data/2023-09-01/E5_470nm_6m-TL_.tif/xlsx_data/2024_Jun_17/2023-09-01_E5_470nm_6m-TL_.xlsx' # should remove as dmd was sent a bmp file with 1 as grey value
file7 <- 'data/2023-09-01/E6_470nm_6m-TL_.tif/xlsx_data/2024_Jun_17/2023-09-01_E6_470nm_6m-TL_.xlsx'
file8 <- 'data/2023-10-26/E1_16ss_6m_470nm_TL_.tif/xlsx_data/2024_Jun_17/2023-10-26_E1_16ss_6m_470nm_TL_.xlsx'
file9 <- 'data/2023-10-26/E3_6m_470nm_TL_.tif/xlsx_data/2024_Jun_17/2023-10-26_E3_6m_470nm_TL_.xlsx'
file10 <- 'data/20221214_E2_13ss/xlsx_data/2024_Jun_17/data_20221214_E2_13ss.xlsx'
file11 <- 'data/20221214_E3_14ss/xlsx_data/2024_Jun_17/data_20221214_E3_14ss.xlsx'
file12 <- 'data/20221214_E5_12ss/xlsx_data/2024_Jun_17/data_20221214_E5_12ss.xlsx' 
file_names <- c(file1, file2, file3, file4, file5, file6, file7, file8, file9, file10, file11, file12)  
data_list <- list()
i <- 1



for (file in file_names){
  file <- file.path(wd, file)
  print(file)
  exists <- file.exists(file)
  print(exists)
  }

for (file in file_names){
  temp_data <- read.xlsx(file)
  to_keep <- rowSums(temp_data, na.rm = T) != 0
  file_name <- str_extract(file, '[^/]+$')
  date_id <- str_extract(file_name, '\\d{8}')
  date_id_2 <- str_extract(file_name, '\\d{4}-\\d{2}-\\d{2}')
  embryo_id <- str_extract(file_name, '[Ee]\\d')
  somite_id <- str_extract(file_name, '\\d+ss')
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
  mutate(to_split = str_replace(to_split, 'iso_x', 'isox'),
         to_split = str_replace(to_split, 'iso_y', 'isoy')) %>% 
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
         align.activation.end = 5 * (as.numeric(activation.end.tp) - as.numeric(activation.start.tp)) ) # this should be 120 for the 6min timelapses

## unique(data[,c('embryo.id', 'align.activation.end')]) # to check the data


# Finding axial direction of movement by mean vang ----------------------------------------------

# This section is for data exploration purposes - looking at the mean vang in each position ------

# find the weighted mean angle in each position (NSE) during activation (first 55s - as peak velocity occurs then )
angles_wm_by_position_act <- data %>% 
  filter(!position %in% c('C', 'W'), timepoint >= 0, timepoint <= 55) %>% # exclude C/ROI due to optogenetic recruitment, W as not neural tube
  filter(measure %in% c("vel", "vang")) %>% 
  pivot_wider(id_cols = -std, names_from = measure, values_from = mean) %>% # make cols called vel and vang containing the mean 
  #drop_na(vang) %>% ## check for na, if so need to drop
  mutate(vang = as.circular(vang, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  group_by(embryo.id, position) %>% 
  summarise(weighted_mean = as.double(weighted.mean.circular(x = vang, w = vel, na.rm = T)),
            mean_vel = mean(vel)) %>% # how does weighting work
  mutate(weighted_mean = weighted_mean * (180 / pi),
         period = 'during')   %>%   # convert to degrees
  ungroup()

# find the weighted mean angle in each position (NSE) during preactivation (55s before activation, excluding the timepoint (-5) which would be included in the moving time average )
angles_wm_by_position_preact <- data %>% 
  filter(!position %in% c('C', 'W'), timepoint >= -65, timepoint < -5) %>% # exclude C/ROI due to optogenetic recruitment, W as not neural tube
  filter(measure %in% c("vel", "vang")) %>% 
  pivot_wider(id_cols = -std, names_from = measure, values_from = mean) %>% # make cols called vel and vang containing the mean 
  #drop_na(vang) %>% ## check for na, if so need to drop
  mutate(vang = as.circular(vang, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  group_by(embryo.id, position) %>% 
  summarise(weighted_mean = as.double(weighted.mean.circular(x = vang, w = vel, na.rm = T)),
            mean_vel = mean(vel)) %>% # how does weighting work
  mutate(weighted_mean = weighted_mean * (180 / pi),
         period = 'before')   %>%   # convert to degrees
  ungroup()

# find the weighted mean angle in each position (NSE) during postactivation (55s post acitvation)
angles_wm_by_position_postact <- data %>% 
  filter(!position %in% c('C', 'W'), timepoint >= 125, timepoint <= 180) %>% # exclude C/ROI due to optogenetic recruitment, W as not neural tube
  filter(measure %in% c("vel", "vang")) %>% 
  pivot_wider(id_cols = -std, names_from = measure, values_from = mean) %>% # make cols called vel and vang containing the mean 
  #drop_na(vang) %>% ## check for na, if so need to drop
  mutate(vang = as.circular(vang, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  group_by(embryo.id, position) %>% 
  summarise(weighted_mean = as.double(weighted.mean.circular(x = vang, w = vel, na.rm = T)),
            mean_vel = mean(vel)) %>% # how does weighting work
  mutate(weighted_mean = weighted_mean * (180 / pi),
         period = 'after') %>%   # convert to degrees
  ungroup()

# Defining the embryo as anterior or posterior moving by the overall mean vang direction -----
# find the weighted mean angle, averaging the NSE positions, during preactivation 
angles_wm_all_preact <- data %>% 
  filter(!position %in% c('C', 'W'), timepoint >= -65, timepoint < -5) %>%
  filter(measure %in% c("vel", "vang")) %>% 
  pivot_wider(id_cols = -std, names_from = measure, values_from = mean) %>% 
  # drop_na(vang) %>% 
  mutate(vang = as.circular(vang, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  group_by(embryo.id) %>% 
  summarise(wm_vang_preact = as.double(weighted.mean.circular(x = vang, w = vel, na.rm = T)),
            mean_vel_preact = mean(vel)) %>% 
  mutate(wm_vang_preact = wm_vang_preact * (180 / pi)) %>% 
  ungroup() 

# find the weighted mean angle, averaging the NSE positions, during activation 
# use this to determine whether the embryo moves anteriorly or posteriorly
angles_wm_all_act <- data %>% 
  filter(!position %in% c('C', 'W'), timepoint >= 0, timepoint <= 55) %>%
  filter(measure %in% c("vel", "vang")) %>% 
  pivot_wider(id_cols = -std, names_from = measure, values_from = mean) %>% 
  # drop_na(vang) %>% 
  mutate(vang = as.circular(vang, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  group_by(embryo.id) %>% 
  summarise(wm_vang_act = as.double(weighted.mean.circular(x = vang, w = vel, na.rm = T)),
            mean_vel_act = mean(vel)) %>% 
  mutate(wm_vang_act = wm_vang_act * (180 / pi)) %>% 
  ungroup() %>% 
  mutate(direction = case_when(wm_vang_act < 0 ~ 'anterior',
                               wm_vang_act > 0 ~ 'posterior')) #anterior/posterior encompasses the entire 180º in the A/P direction

# find the weighted mean angle, averaging the NSE positions, during postactivation 
angles_wm_all_postact <- data %>% 
  filter(!position %in% c('C', 'W'), timepoint >= 125, timepoint <= 180) %>%
  filter(measure %in% c("vel", "vang")) %>% 
  pivot_wider(id_cols = -std, names_from = measure, values_from = mean) %>% 
  # drop_na(vang) %>% 
  mutate(vang = as.circular(vang, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  group_by(embryo.id) %>% 
  summarise(wm_vang_postact = as.double(weighted.mean.circular(x = vang, w = vel, na.rm = T)),
            mean_vel_postact = mean(vel)) %>% 
  mutate(wm_vang_postact = wm_vang_postact * (180 / pi)) %>% 
  ungroup()

# combine the data
angles_wm_all <- left_join(angles_wm_all_preact, angles_wm_all_act, by = join_by(embryo.id))
angles_wm_all <- left_join(angles_wm_all, angles_wm_all_postact, by = join_by(embryo.id))

angles_wm_by_position <- bind_rows(angles_wm_by_position_preact, angles_wm_by_position_act, angles_wm_by_position_postact)
angles_wm_by_position <- angles_wm_by_position %>% # cant remember what the purpose of this was - to add the directions in?
  left_join(angles_wm_all_act, by = 'embryo.id', keep = FALSE) %>% 
  select(-wm_vang_act, -mean_vel_act)

AP_grouped_data <- data %>%
  left_join(angles_wm_all_act, by = 'embryo.id', keep = FALSE) %>% 
  relocate(direction, .before=position)

# Print AP_grouped_data to excel ------

filename <- paste(Sys.Date(), 'AP_grouped_data', sep = "_")

write_xlsx(AP_grouped_data, path = sprintf('results/spreadsheets/%s.xlsx', filename))

