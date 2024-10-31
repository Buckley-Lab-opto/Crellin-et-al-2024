# Helena's code to generate plots in different cardinal directions for measurements from PIV analysis 
# Oct 2023 - Nov 2023
# # = normal comments, '##' = notes to myself

# Load libraries
library(tidyverse)
library(openxlsx)

# Function to calculate circular mean (mean of angles)--------------
# Based on python code from Circular Mean, Wikipedia

circular_mean <- function(angle) {

# If needing to convert degrees to radians, uncomment code below and comment out next line 
# radians <- sapply(angle, function(angle) angle * (pi / 180))
 
radians <- angle

# Calculate the sum of sin and cos values
sin_sum <- sum(sin(radians))
cos_sum <- sum(cos(radians))

# Calculate the circular mean using arctan2
mean_rad <- atan2(sin_sum, cos_sum)

# Convert the mean back to degrees
mean_degree <- (mean_rad * (180 / pi)) %% 360

# If wanting to return radians, comment out code above and uncomment code below:
# mean_degree <- mean_rad

return(mean_degree)
}

#-------------------

setwd('/Users/helena/Desktop/PIV-Analysis')


# Read in data annotations / metadata
metadata <- read.xlsx('/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/light-patterning-experiment-metadata.xlsx')

# Create a data frame containing the PIV data (mean and std of different measures in different cardinal direction around the ROI)
#file1 <- 'data/2022-11-08/E8_15ss_470nm_TL_posterior-region_.tif/results_sp_5_tf_1/PIV/xlsx_data/E8_15ss_470nm_TL_posterior-region_.tif.xlsx'
file2 <- '/Users/helena/Desktop/PIV-Analysis/data/2022-11-08/E8_15ss_470nm_TL_.tif/results_sp_5_tf_1/PIV/xlsx_data/E8_15ss_470nm_TL_.tif.xlsx'
file3 <- '/Users/helena/Desktop/PIV-Analysis/data/2022-12-14/E4_15ss_470nm_TL_.tif/results_sp_5_tf_1/PIV/xlsx_data/E4_15ss_470nm_TL_.tif.xlsx'
file4 <- '/Users/helena/Desktop/PIV-Analysis/data/2023-08-04/E1_approx21ss_470nm_TL_.tif/results_sp_5_tf_1/PIV/xlsx_data/E1_approx21ss_470nm_TL_.tif.xlsx'
file5 <- '/Users/helena/Desktop/PIV-Analysis/data/2023-09-01/E2_470nm_6m-TL_.tif/results_sp_5_tf_1/PIV/xlsx_data/E2_470nm_6m-TL_.tif.xlsx'
file6 <- '/Users/helena/Desktop/PIV-Analysis/data/2023-09-01/E5_470nm_6m-TL_.tif/results_sp_5_tf_1/PIV/xlsx_data/E5_470nm_6m-TL_.tif.xlsx'
file7 <- '/Users/helena/Desktop/PIV-Analysis/data/2023-09-01/E6_470nm_6m-TL_.tif/results_sp_5_tf_1/PIV/xlsx_data/E6_470nm_6m-TL_.tif.xlsx'
file8 <- '/Users/helena/Desktop/PIV-Analysis/data/2023-10-26/E1_16ss_6m_470nm_TL_.tif/results_sp_5_tf_1/PIV/xlsx_data/E1_16ss_6m_470nm_TL_.tif.xlsx'
file9 <- '/Users/helena/Desktop/PIV-Analysis/data/2023-10-26/E3_6m_470nm_TL_.tif/results_sp_5_tf_1/PIV/xlsx_data/E3_6m_470nm_TL_.tif.xlsx'
file10 <- 'data/20221214_E2_13ss/xlsx_data_new/20221214_E2_13ss.xlsx'
file11 <- 'data/20221214_E3_14ss/xlsx_data_new/20221214_E3_14ss.xlsx'
file12 <- 'data/20221018_E1_15ss/xlsx_data/20221018_E1_15ss.xlsx'
file13 <- 'data/20221214_E5_12ss/xlsx_data_new/20221214_E5_12ss.xlsx'

file_names <- c( file2, file3, file4, file5, file6, file7, file8, file9, file10, file11, file12, file13)  
data_list <- list()
i <- 1

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
  pivot_longer(cols = c(-'date', -'date_2', -'embryo', -'somite', -'timepoint'), 
               names_to = 'to_split', 
               values_to = 'value') %>% 
  separate(to_split, c('position', 'measure', 'statistic'), sep = '_') %>% 
  pivot_wider(names_from = 'statistic', values_from = 'value') %>% 
  mutate(position = factor(position, levels = c('C', 'N', 'S', 'E', 'W'))) %>% 
  mutate(timepoint = timepoint * 5) %>% 
  mutate(date = coalesce(date, date_2)) %>% 
  unite(col = 'embryo.id', c('date', 'embryo', 'somite'), remove = FALSE) %>% 
  select(-'date_2')

## data$direction %>% unique() # checking what values are in direction column

# Make the embryo.id column in metadata the same as data
metadata <- metadata %>%
  mutate(embryo.id = gsub('-', '', embryo.id)) %>% 
  unite(col = 'embryo.id', c('embryo.id', 'embryo.age'), remove = FALSE)

# Add timelapse specific metadata to the data
timelapse_metadata <- metadata %>% 
  filter(file.type == 'timelapse') %>% 
  select(embryo.id, timelapse.length.sec, timestep.sec, total.timepoints,	activation.start.tp,	activation.end.tp)

data <- data %>% 
  left_join(timelapse_metadata, by = 'embryo.id', keep = FALSE) %>% 
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

peak_AP_velocity_result <- paste('The peak velocity in the AP axis during activation of ', peak_AP_velocity_during_act$embryo.id, ' is in the', peak_AP_velocity_during_act$position, ' position at t=', peak_AP_velocity_during_act$timepoint)

print(peak_AP_velocity_result)

directed_data <- data %>%
  left_join(peak_velocity_axial_direction, by = 'embryo.id', keep = FALSE) %>% 
  relocate(direction, .before=position)


# Finding axial direction of movement by mean vang ----------------------------------------------

ave_vang_direction <- data %>%
  filter(measure == 'vang', position != 'C', timepoint >= 0, timepoint <= 125) %>%
  group_by(embryo.id) %>% 
  summarise(mean = circular_mean(mean))

ave_vang_direction_45s <- data %>%
  filter(measure == 'vang', position != 'C', timepoint >= 0, timepoint <= 45) %>%
  group_by(embryo.id) %>% 
  summarise(mean = circular_mean(mean))



# Plotting individual embryos ---------------------------------------

# Plot velocity against time with the y-axis scaled individually
# filtered by direction of movement e.g. anterior or posterior

(
  anterior_vel <-  directed_data %>% 
    filter(measure == 'vel',
           direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    geom_vline(aes(xintercept = align.activation.start), size = 0.3, colour = 'red') +
    geom_vline(aes(xintercept = 125), size = 0.3, colour = 'red') +
    geom_line() +
    theme_bw() +
    geom_line(aes(x = timepoint, y = ((mean - std))), linetype = 'dashed') + 
    geom_line(aes(x = timepoint, y = ((mean + std))), linetype = 'dashed') +
  #  facet_wrap(~position + date + embryo, ncol = 3) +
    facet_grid(rows = vars(direction, embryo.id),
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time') +
    ylab('Veolcity')
)


(
  posterior_vel <-  directed_data %>% 
    filter(measure == 'vel',
           direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    geom_vline(aes(xintercept = align.activation.start), size = 0.3, colour = 'red') +
    geom_vline(aes(xintercept = 125), size = 0.3, colour = 'red') +
    geom_line() +
    theme_bw() +
    geom_line(aes(x = timepoint, y = ((mean - std))), linetype = 'dashed') + 
    geom_line(aes(x = timepoint, y = ((mean + std))), linetype = 'dashed') +
    #  facet_wrap(~position + date + embryo, ncol = 3) +
    facet_grid(rows = vars(direction, embryo.id),
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time') +
    ylab('Veolcity')
)

(
  medial_vel <-  directed_data %>% 
    filter(measure == 'vel',
           direction == 'medial') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    geom_vline(aes(xintercept = align.activation.start), size = 0.3, colour = 'red') +
    geom_vline(aes(xintercept = 125), size = 0.3, colour = 'red') +
    geom_line() +
    theme_bw() +
    geom_line(aes(x = timepoint, y = ((mean - std))), linetype = 'dashed') + 
    geom_line(aes(x = timepoint, y = ((mean + std))), linetype = 'dashed') +
    #  facet_wrap(~position + date + embryo, ncol = 3) +
    facet_grid(rows = vars(direction, embryo.id),
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time') +
    ylab('Veolcity')
)

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
  facet_grid(rows = vars(direction, embryo.id),
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
  facet_grid(rows = vars(direction, embryo.id),
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
  facet_grid(rows = vars(direction, embryo.id),
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
  facet_grid(rows = vars(direction, embryo.id),
             cols = vars(position)) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(1, 'lines')) +
  xlab('Time') +
  ylab('Anisotropic strain angle')



pdf('direction_plots.pdf', height = 15, width = 15) #number is inches
vel
vang
iso
ani
aniang
dev.off()


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

averaged_direction <- rbind(averaged_ant, averaged_post)

vel <- averaged_direction %>% 
  filter(measure == 'vel') %>% 
  filter(position == 'C') %>% 
  ggplot(aes(x = timepoint, y = mean)) +
  geom_vline(aes(xintercept = align.activation.start), size = 0.3) +  
  geom_vline(aes(xintercept = align.activation.end), size = 0.3) +
  geom_line() +
  theme_bw() +
  geom_line(aes(x = timepoint, y = ((mean - std))), linetype = 'dashed') + 
  geom_line(aes(x = timepoint, y = ((mean + std))), linetype = 'dashed') +
  geom_line(data = data %>% 
              filter(measure == 'vel') %>% 
            aes(x = timepoint, y = mean, colour = embryo),
            alpha = 0.5) +
  facet_wrap(~direction + position, ncol=5) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Time') +
  ylab('Veolcity')

vel

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

