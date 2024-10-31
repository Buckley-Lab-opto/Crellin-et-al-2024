'09
Somite stage analysis'
# # = normal comments, '##' = notes to myself

# Load libraries
library(tidyverse)
library(openxlsx)
library(pdf2pptx)
#library(ggquiver)
#library(viridis)  
library(patchwork)
library(writexl)
library(rstatix)
library(ggpubr)


# Add in missing somite stage data and group into MET stage

AP_grouped_data <- read.xlsx('results/spreadsheets/2024-01-24_AP_grouped_data.xlsx')

thresholded_data <- read.xlsx('results/spreadsheets/2024-06-20_thresholded_data_ungrouped.xlsx') %>% 
  mutate(position = factor(position, levels = c('C', 'N', 'S', 'E', 'W')),
         rhombomere = 'R5')

post_data <- read.xlsx('results/spreadsheets/2024-09-09_post_thresholded_data_ungrouped.xlsx') %>% 
  mutate(position = factor(position, levels = c('C', 'N', 'S', 'E', 'W')))

thresholded_data <- thresholded_data %>% 
  bind_rows(post_data) %>% 
  as.data.frame()

somite_info <- data.frame(
  embryo.id = c('20231026_E3_NA', '20230901_E2_NA', '20230901_E5_NA', '20230901_E6_NA'),
  somite = c('18ss', '21ss', '23ss', '25ss'))

AP_grouped_data <- thresholded_data %>% 
  left_join(somite_info, by = c('embryo.id')) %>% 
  mutate(somite = coalesce(somite.x, somite.y)) %>% 
  mutate(embryo.id = case_when(str_detect(embryo.id, '20221214_E5_12ss') ~ '20221214_E5_18ss',
                               .default = embryo.id),
         somite = case_when(str_detect(embryo.id, '20221214_E5_18ss') ~ '18',
                            .default = somite)) %>% 
  mutate(embryo.id = case_when(str_detect(embryo.id, 'NA') ~ paste(date, embryo, somite, sep = '_'),
                               str_detect(embryo.id, 'NA', negate = TRUE) ~ embryo.id),
         somite = as.numeric(str_remove(somite, 'ss'))) %>% 
  select(-somite.x, -somite.y) %>% 
  # mutate(dev_stage = case_when(somite <= 14 ~ 'preEpithelial', #### get clarity on these groups
  #                              between(somite, 15, 18) ~ 'Polarising',
  #                              somite >= 19 ~ 'Polarised')) %>% 
  # mutate(dev_stage = factor(dev_stage, levels = c('preEpithelial', 'Polarising', 'Polarised')))
  mutate(dev_stage = case_when(somite <= 15 ~ 'Neural Keel',
                               between(somite, 16, 21) ~ 'Neural Rod')) 

info <- AP_grouped_data %>% filter(dev_stage == 'Neural Rod')
length(unique(info$embryo.id))
AP_grouped_data %>% distinct(embryo.id, somite, direction) %>% arrange(somite)
       

# finding peak region and velocity

peak_vel_during_act <- AP_grouped_data %>%
  filter(measure == 'vel', position != 'C', timepoint >= 0, timepoint <= 100) %>%
  group_by(embryo.id) %>%
  slice_max(order_by = mean) %>% 
  rename(peak_vel_tp = timepoint,
         peak_vel_position = position,
         peak_vel = mean) %>% 
  select(embryo.id, peak_vel_tp, peak_vel_position, peak_vel, dev_stage, somite) %>% 
  ungroup()

peak_vel_during_act_to_merge <- AP_grouped_data %>%
  filter(measure == 'vel', position != 'C', timepoint >= 0, timepoint <= 100) %>%
  group_by(embryo.id) %>%
  slice_max(order_by = mean) %>% 
  rename(peak_vel_tp = timepoint,
         peak_vel_position = position,
         peak_vel = mean) %>% 
  select(embryo.id, peak_vel_tp, peak_vel_position, peak_vel) %>% 
  ungroup()

AP_grouped_data <- AP_grouped_data %>% left_join(peak_vel_during_act_to_merge, by = 'embryo.id')


# average dev_stage by peak

averaged_dev_stage <- AP_grouped_data %>% 
  filter(!measure %in% c('vang', 'aniang'), position == peak_vel_position) %>%  #filtering for non-circular data
  group_by(timepoint, measure, dev_stage) %>% 
  summarise(std = sd(`mean`),
            SEM = std / sqrt(length(unique(embryo.id))),
            mean = mean(`mean`),
            std_zscore = sd(z_score_vel),
            SEM_zscore = std_zscore / sqrt(length(unique(embryo.id))),
            mean_zscore = mean(z_score_vel)) %>% 
            #std = sqrt(sum(std^2) / n()),
            #SEM = std / sqrt(length(unique(embryo.id)))) %>% 
  ungroup()

# plot the stages to compare neural keel and rod

(dev_stage_plot <- averaged_dev_stage %>% 
  filter(measure == 'vel', dev_stage != 'NA') %>% 
  mutate(mean = mean * 60,
         SEM = SEM * 60) %>% 
  ggplot(aes(x = timepoint, y = mean, group = dev_stage, colour = dev_stage)) +
  geom_line() +
  annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
  annotate("text", x = 60, y = 0.6, label = "ON", size = 6, family ='Avenir') +
  geom_ribbon(aes(ymin = (mean - SEM), ymax = (mean + SEM), fill = dev_stage), alpha = 0.3, linewidth = 0) +
  # geom_line(data = AP_grouped_data %>% 
  #             filter(measure == 'vel', position == peak_vel_position, dev_stage == 'preEpithelial'),
  #           aes(x = timepoint, y = mean, group = embryo.id), linewidth = 0.3, alpha = 0.5) +
  # geom_line(data = AP_grouped_data %>% 
  #             filter(measure == 'vel', position == peak_vel_position, dev_stage == 'polarised'),
  #           aes(x = timepoint, y = mean, group = embryo.id), linewidth = 0.3, alpha = 0.5) +
  geom_line(linewidth = 1) +
  theme_classic(base_size = 18) +
  theme(text = element_text(size = 20, family = 'Avenir'),
        strip.background = element_blank(),
        legend.title= element_blank(),
        legend.text = element_text(size = 14, family = 'Avenir'),
        panel.spacing = unit(0.5, 'lines'),
        aspect.ratio = 1) +
  xlab('Time (s)') +
  ylab(expression(paste("Velocity (µm ", min^-1, ")"))) 
)

filename <- paste0('results/plots/paper/', Sys.Date(), '_Fig5_devstage_velocity.svg')
svglite(filename, width = 8, height = 5)
dev_stage_plot
dev.off()


# t test and plot to compare the peak velocity
peak_vel_during_act <- peak_vel_during_act %>% 
  filter(!is.na(dev_stage)) %>% 
  mutate(peak_vel = peak_vel * 60)

qqnorm(peak_vel_during_act %>% 
         filter(dev_stage == 'Neural Keel') %>% #'Neural Rod' 'Neural Keel'
         pull(peak_vel))
         
qqline(peak_vel_during_act %>% 
         filter(dev_stage == 'Neural Keel') %>% #'Neural Rod' 'Neural Keel'
         pull(peak_vel))

shapiro.test(peak_vel_during_act %>% 
               filter(dev_stage == 'Neural Keel') %>% #'Neural Rod' 'Neural Keel'
               pull(peak_vel))

bartlett.test(peak_vel ~ dev_stage, data = peak_vel_during_act)

stat <- peak_vel_during_act %>% 
  t_test(peak_vel ~ dev_stage, var.equal = TRUE) %>% 
  add_xy_position() %>% 
  add_significance()

stat_pvalue <- stat %>% pull(p) %>% as.character()


(
  boxplot <- peak_vel_during_act  %>% 
  ggplot(aes(x = dev_stage, y = peak_vel, colour = dev_stage)) +
    stat_boxplot(geom ='errorbar', width = 0.25, size = 1) +
  geom_boxplot(size = 1) +
  geom_point(aes(group = embryo.id), size = 5) +
  stat_pvalue_manual(stat, bracket.nudge.y	= 0.05, family="Avenir") +
  annotate('text', x = 1.5, y = 0.83, label = paste('p =', stat_pvalue ), family="Avenir" ) +
  theme_classic(base_size = 18) +
  theme(text = element_text(size = 20, family = 'Avenir'),
        strip.background = element_blank(),
        legend.position = 'none',
        panel.spacing = unit(0.5, 'lines'),
        aspect.ratio = 1) +
  xlab('') +
  ylab(expression(paste("Peak Velocity (µm mi", n^-1, ")"))) 
)

filename <- paste0('results/plots/paper/', Sys.Date(), '_Fig5_devstage_peak_velocity.svg')
svglite(filename, width = 8, height = 5)
boxplot
dev.off()


# plotting the dev_stage groups individually ------

(
  preEpithelial <- averaged_dev_stage %>%
    filter(measure == 'vel', dev_stage == 'Neural Keel') %>%
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    annotate("text", x = 60, y = 0.011, label = "ON", size = 4, family = 'Avenir') +
    geom_line(linewidth = 1) +
    geom_line(aes(x = timepoint, y = mean), linewidth = 1) +
    geom_line(data = AP_grouped_data %>%
                filter(measure == 'vel', position == peak_vel_position, dev_stage == 'Neural Keel'),
              aes(x = timepoint, y = mean, group = embryo.id), linewidth = 0.3) +
    theme_classic(base_size = 18) +
    theme(text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          legend.position = 'none',
          panel.spacing = unit(0.5, 'lines')) +
    # theme(strip.background = element_blank(),
    #       axis.title = element_text(size = 20),
    #       axis.text = element_text(size = 15),
    #       legend.title = element_text(size = 12),
    #       legend.text = element_text(size = 12),
    #       panel.spacing = unit(0.5, 'lines')) +
    xlab('Time (s)') +
    ylab(expression(paste("Velocity (µm ", min^-1, ")")))
  
)
#   
#   
#   

# 
(
  Polarised <- averaged_dev_stage %>%
    filter(measure == 'vel', dev_stage == 'Neural Rod') %>%
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    annotate("text", x = 60, y = 0.011, label = "ON", size = 4, family = 'Avenir') +
    geom_line(linewidth = 1) +
    geom_line(aes(x = timepoint, y = mean), linewidth = 1) +
    geom_line(data = AP_grouped_data %>%
                filter(measure == 'vel', position == peak_vel_position, dev_stage == 'Neural Rod'),
              aes(x = timepoint, y = mean, group = embryo.id), linewidth = 0.3) +
    theme_classic(base_size = 18) +
    theme(text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          legend.position = 'none',
          panel.spacing = unit(0.5, 'lines')) +

    # theme(strip.background = element_blank(),
    #       axis.title = element_text(size = 20),
    #       axis.text = element_text(size = 15),
    #       legend.title = element_text(size = 12),
    #       legend.text = element_text(size = 12),
    #       panel.spacing = unit(0.5, 'lines')) +
    xlab('Time (s)') +
    ylab(expression(paste("Velocity (µm ", min^-1, ")")))


)

