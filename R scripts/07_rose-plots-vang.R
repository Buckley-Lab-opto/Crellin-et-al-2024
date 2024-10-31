'07 
Rose plots for velocity angles
WIP'

library(tidyverse)
library(openxlsx)
library(circular)
#library(pdf2pptx)
#library(ggquiver)
#library(viridis)  
library(patchwork)
library(writexl)
library(svglite)

thresholded_data <- read.xlsx('results/spreadsheets/2024-06-20_thresholded_data.xlsx') %>% 
  mutate(position = factor(position, levels = c('C', 'N', 'S', 'E', 'W')))

# find the weighted mean angle in each position (NSE) during preactivation (55s before activation, excluding the timepoint (-5) which would be included in the moving time average )
angles_wm_by_position_preact <- thresholded_data %>% 
  filter(measure %in% c("vel", "vang"), !position %in% c('C', 'W'), timepoint >= -65, timepoint < -5) %>% # exclude C/ROI due to optogenetic recruitment, W as not neural tube
  pivot_wider(id_cols = c(embryo.id, timepoint, position), names_from = measure, values_from = mean) %>% # make cols called vel and vang containing the mean 
  #drop_na(vang) %>% ## check for na, if so need to drop
  mutate(vang = as.circular(vang, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  group_by(embryo.id, position) %>% 
  summarise(weighted_mean = as.double(weighted.mean.circular(x = vang, w = vel, na.rm = T)),
            mean_vel = mean(vel)) %>% # how does weighting work
  mutate(weighted_mean = weighted_mean * (180 / pi),
         period = 'before')   %>%   # convert to degrees
  ungroup()

# find the weighted mean angle in each position (NSE) during activation (first 55s - as peak velocity occurs then )
angles_wm_by_position_act <- thresholded_data %>% 
  filter(measure %in% c("vel", "vang"), !position %in% c('C', 'W'), timepoint >= 0, timepoint < 55) %>% # exclude C/ROI due to optogenetic recruitment, W as not neural tube
  pivot_wider(id_cols = c(embryo.id, timepoint, position), names_from = measure, values_from = mean) %>% # make cols called vel and vang containing the mean 
  # drop_na(vang) %>% ## check for na, if so need to drop
  mutate(vang = as.circular(vang, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  group_by(embryo.id, position) %>% 
  summarise(weighted_mean = as.double(weighted.mean.circular(x = vang, w = vel, na.rm = T)),
            mean_vel = mean(vel)) %>% # how does weighting work
  mutate(weighted_mean = weighted_mean * (180 / pi),
         period = 'during')   %>%   # convert to degrees
  ungroup()

# find the weighted mean angle in each position (NSE) during postactivation (55s post acitvation)
angles_wm_by_position_postact <- thresholded_data %>% 
  filter(measure %in% c("vel", "vang"), !position %in% c('C', 'W'), timepoint >= 120, timepoint < 175) %>% # exclude C/ROI due to optogenetic recruitment, W as not neural tube
  pivot_wider(id_cols = c(embryo.id, timepoint, position), names_from = measure, values_from = mean) %>% # make cols called vel and vang containing the mean 
  #drop_na(vang) %>% ## check for na, if so need to drop
  mutate(vang = as.circular(vang, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  group_by(embryo.id, position) %>% 
  summarise(weighted_mean = as.double(weighted.mean.circular(x = vang, w = vel, na.rm = T)),
            mean_vel = mean(vel)) %>% # how does weighting work
  mutate(weighted_mean = weighted_mean * (180 / pi),
         period = 'after') %>%   # convert to degrees
  ungroup()

angles_wm_by_position <- bind_rows(angles_wm_by_position_preact, angles_wm_by_position_act, angles_wm_by_position_postact)


# Defining the embryo as anterior or posterior moving by the overall mean vang direction -----
# find the weighted mean angle, averaging the NSE positions, during preactivation 
angles_wm_all_preact <- thresholded_data %>% 
  filter(measure %in% c("vel", "vang"), !position %in% c('C', 'W'), timepoint >= -65, timepoint < -5) %>% # exclude C/ROI due to optogenetic recruitment, W as not neural tube
  pivot_wider(id_cols = c(embryo.id, timepoint, position), names_from = measure, values_from = mean) %>% # make cols called vel and vang containing the mean 
  # drop_na(vang) %>% 
  mutate(vang = as.circular(vang, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  group_by(embryo.id) %>% 
  summarise(wm_vang_preact = as.double(weighted.mean.circular(x = vang, w = vel, na.rm = T)),
            mean_vel_preact = mean(vel)) %>% 
  mutate(wm_vang_preact = wm_vang_preact * (180 / pi)) %>% 
  ungroup() 

# find the weighted mean angle, averaging the NSE positions, during activation 
# use this to determine whether the embryo moves anteriorly or posteriorly
angles_wm_all_act <- thresholded_data %>% 
  filter(measure %in% c("vel", "vang"), !position %in% c('C', 'W'), timepoint >= 0, timepoint < 55) %>% # exclude C/ROI due to optogenetic recruitment, W as not neural tube
  pivot_wider(id_cols = c(embryo.id, timepoint, position), names_from = measure, values_from = mean) %>% # make cols called vel and vang containing the mean 
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
angles_wm_all_postact <- thresholded_data %>% 
  filter(measure %in% c("vel", "vang"), !position %in% c('C', 'W'),  timepoint >= 120, timepoint < 175) %>% # exclude C/ROI due to optogenetic recruitment, W as not neural tube
  pivot_wider(id_cols = c(embryo.id, timepoint, position), names_from = measure, values_from = mean) %>% # make cols called vel and vang containing the mean 
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

angles_wm_by_position <- angles_wm_by_position %>% # cant remember what the purpose of this was - to add the directions in?
  left_join(angles_wm_all_act, by = 'embryo.id', keep = FALSE) %>% 
  select(-wm_vang_act, -mean_vel_act)



# Priming the plot

angles <- seq(-180, 180, by = 45)
angles <- as.data.frame(angles)

bars <- data.frame(angle = c(90, -90),
                   direction = c('posterior', 'anterior'))

# 

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
  geom_segment(aes(x = wm_vang_act, y = 0, xend = wm_vang_act, yend = mean_vel_act*60), size = 1, arrow = arrow(length = unit(0.4, "cm"))) +
  geom_point(data = angles, aes(x = angles, y = 0.5), size = 0.01, alpha = 0.01) +
  coord_polar(theta = "x", start = 1.57, direction = -1) +  
  scale_x_reverse(breaks = seq(180, -180, -45)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank()) +
  labs(title = "Average vector direction in the first 55s of activation") 



angles_wm_all %>% 
  filter(direction == 'anterior') %>% 
  ggplot() +
  annotate("text", x = -90, y = 0.4, label = "Anterior", size = 5) +
  annotate("text", x = 90, y = 0.4, label = "Posterior", size = 5) +
  geom_segment(aes(x = wm_vang_preact, y = 0, xend = wm_vang_preact, yend = mean_vel_preact*60, colour = embryo.id), size = 1, arrow = arrow(length = unit(0.4, "cm"))) +
  geom_point(data = angles, aes(x = angles, y = 0.4), alpha = 0.01) +
  coord_polar(theta = "x", start = 1.57, direction = -1) +  
  scale_x_reverse(breaks = seq(180, -180, -45)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank()) +
  labs(title = "Average vector direction in the 55s before activation") 

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
  labs(title = "Average vector direction in the north region, 55s before activation") 



angles_wm_all %>% 
  filter(direction == 'anterior') %>% 
  ggplot() +
  annotate("text", x = -90, y = 0.4, label = "Anterior", size = 5) +
  annotate("text", x = 90, y = 0.4, label = "Posterior", size = 5) +
  geom_segment(aes(x = wm_vang_act, y = 0, xend = wm_vang_act, yend = mean_vel_act*60, colour = embryo.id), size = 1, arrow = arrow(length = unit(0.4, "cm"))) +
  geom_point(data = angles, aes(x = angles, y = 0.4), alpha = 0.01) +
  coord_polar(theta = "x", start = 1.57, direction = -1) +  
  scale_x_reverse(breaks = seq(180, -180, -45)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank()) +
  labs(title = "Average vector direction in the first 55s after activation") 

# plot for presentation / paper -----

# categorising direction 

(vang_during <- angles_wm_all %>% 
  ggplot() +
  annotate("text", x = -90, y = 0.4, label = "Anterior", size = 5, family = 'Avenir') +
  annotate("text", x = 90, y = 0.4, label = "Posterior", size = 5, family = 'Avenir') +
  annotate('text', x = 0, y = c(0.1, 0.2, 0.3, 0.4), label = c('0.1', '0.2', '0.3', '0.4'), family = 'Avenir', size = 4) +
  geom_segment(aes(x = wm_vang_act, y = 0, xend = wm_vang_act, yend = mean_vel_act*60, colour = direction), size = 1, arrow = arrow(length = unit(0.25, "cm"),  type = "closed")) +
  geom_point(data = angles, aes(x = angles, y = 0.4), size = 0.01, alpha = 0.01) +
  coord_polar(theta = "x", start = 1.57, direction = -1) +  
  scale_x_reverse(breaks = seq(180, -179, -45)) +
   theme_minimal() +
   theme(text = element_text(size = 22, family = 'Avenir'),
         aspect.ratio = 1,
         legend.position = "none",
         axis.title = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(), 
         panel.grid.major = element_line(size = 1.3, colour = 'grey85'),
         panel.grid.minor = element_blank()
   ) +
   labs(title = 
          "Average vector direction in 
the first 55s of activation") 
)

filename <- paste0(Sys.Date(), 'polar_NSE_during.png')
ggsave(filename, plot = vang_during, path = 'results/plots/paper/polar', width = 5, height = 5.19)
filename <- paste0('results/plots/paper/polar/', Sys.Date(), '_polar_NSE_during.svg')
svglite(filename, width = 4, height = 4.1)
vang_during
dev.off()


# direction in region of peak velocity


(vang_N_S_before <- 
  angles_wm_by_position %>% 
  filter( (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S'), period == 'before') %>% 
  ggplot() +
  annotate("text", x = -90, y = 0.6, label = "Anterior", size = 5, family = 'Avenir') +
  annotate("text", x = 90, y = 0.6, label = "Posterior", size = 5, family = 'Avenir') +
  annotate('text', x = 0, y = c(0.1, 0.2, 0.3, 0.4, 0.5), label = c('0.1', '0.2', '0.3', '0.4', '0.5'), family = 'Avenir', size = 4) +
  geom_segment(aes(x = weighted_mean, y = 0, xend = weighted_mean, yend = mean_vel*60, colour = direction), size = 1, arrow = arrow(length = unit(0.25, "cm"),  type = "closed")) +
  geom_point(data = angles, aes(x = angles, y = 0.6), alpha = 0.01) +
  coord_polar(theta = "x", start = 1.57, direction = -1) +  
  scale_x_reverse(breaks = seq(180, -179, -45)) +
  theme_minimal() +
  theme(text = element_text(size = 22, family = 'Avenir'),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.grid.major = element_line(size = 1.5, colour = 'grey85'),
        panel.grid.minor = element_blank()
  ) +
  labs(title = 
         "Average vector direction
in the peak region,
55s before activation") )

vang_N_S_during <- 
angles_wm_by_position %>% 
  filter( (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S'), period == 'during') %>% 
  ggplot() +
  annotate("text", x = -90, y = 0.6, label = "Anterior", size = 5, family = 'Avenir') +
  annotate("text", x = 90, y = 0.6, label = "Posterior", size = 5, family = 'Avenir') +
  annotate('text', x = 0, y = c(0.1, 0.2, 0.3, 0.4, 0.5), label = c('0.1', '0.2', '0.3', '0.4', '0.5'), family = 'Avenir', size = 4) +
  geom_segment(aes(x = weighted_mean, y = 0, xend = weighted_mean, yend = mean_vel*60, colour = direction), size = 1, arrow = arrow(length = unit(0.25, "cm"),  type = "closed")) +
  geom_point(data = angles, aes(x = angles, y = 0.6), alpha = 0.01) +
  coord_polar(theta = "x", start = 1.57, direction = -1) +  
  scale_x_reverse(breaks = seq(180, -179, -45)) +
  theme_minimal() +
  theme(text = element_text(size = 22, family = 'Avenir'),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.grid.major = element_line(size = 1.5, colour = 'grey85'),
        panel.grid.minor = element_blank()
  ) +
  labs(title = 
"Average vector direction
in the peak region,
first 55s of activation") 

(vang_N_S_after <- 
  angles_wm_by_position %>% 
  filter( (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S'), period == 'after') %>% 
  ggplot() +
  annotate("text", x = -90, y = 0.6, label = "Anterior", size = 5, family = 'Avenir') +
  annotate("text", x = 90, y = 0.6, label = "Posterior", size = 5, family = 'Avenir') +
  annotate('text', x = 0, y = c(0.1, 0.2, 0.3, 0.4, 0.5), label = c('0.1', '0.2', '0.3', '0.4', '0.5'), family = 'Avenir', size = 4) +
  geom_segment(aes(x = weighted_mean, y = 0, xend = weighted_mean, yend = mean_vel*60, colour = direction), size = 1, arrow = arrow(length = unit(0.25, "cm"),  type = "closed")) +
  geom_point(data = angles, aes(x = angles, y = 0.6), alpha = 0.01) +
  coord_polar(theta = "x", start = 1.57, direction = -1) +  
  scale_x_reverse(breaks = seq(180, -179, -45)) +
  theme_minimal() +
  theme(text = element_text(size = 22, family = 'Avenir'),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.grid.major = element_line(size = 1.5, colour = 'grey85'),
        panel.grid.minor = element_blank()
  ) +
  labs(title = 
         "Average vector direction
in the peak region,
first 55s of deactivation") 
)

filename <- paste0(Sys.Date(), 'polar_before.png')
ggsave(filename, plot = vang_N_S_before, path = 'results/plots/paper/polar', width = 5, height = 5.19)


filename <- paste0(Sys.Date(), 'polar_during.png')
ggsave(filename, plot = vang_N_S_during, path = 'results/plots/paper/polar',width = 5, height = 5.19)

filename <- paste0(Sys.Date(), 'polar_after.png')
ggsave(filename, plot = vang_N_S_after, path = 'results/plots/paper/polar', width = 5, height = 5.19)

# -----


angles_wm_all %>% 
  filter(direction == 'posterior') %>% 
  ggplot() +
  annotate("text", x = -90, y = 0.4, label = "Anterior", size = 5) +
  annotate("text", x = 90, y = 0.4, label = "Posterior", size = 5) +
  geom_segment(aes(x = wm_vang_postact, y = 0, xend = wm_vang_postact, yend = mean_vel_postact*60, colour = embryo.id), size = 1, arrow = arrow(length = unit(0.4, "cm"))) +
  geom_point(data = angles, aes(x = angles, y = 0.4), alpha = 0.01) +
  coord_polar(theta = "x", start = 1.57, direction = -1) +  
  scale_x_reverse(breaks = seq(180, -180, -45)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank()) +
  labs(title = "Average vector direction in the first 55s after deactivation") 

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
  labs(title = "Average vector direction in the south region, 55s after deactivation") 



(ave_vang_direction <- data %>%
    filter(measure == 'vang', position != 'C', timepoint >= 0, timepoint <= 125) %>%
    group_by(embryo.id) %>% 
    summarise(mean = circular_mean(mean)))

ave_vang_direction_55s <- data %>%
  filter(measure == 'vang', position != 'C', timepoint >= 0, timepoint <= 45) %>%
  group_by(embryo.id) %>% 
  summarise(mean = circular_mean(mean))


(ave_vang_direction_in_peak <- directed_data %>%
    filter(direction == 'medial') %>% 
    filter(measure == 'vang', position == 'N', timepoint >= 0, timepoint <= 45) %>%
    group_by(embryo.id) %>% 
    summarise(mean = circular_mean(mean)))

