'1.01
Plots for forces across scales presentation'
# # = normal comments, '##' = notes to myself

# Load libraries
library(tidyverse)
library(openxlsx)
library(circular)
library(pdf2pptx)
#library(ggquiver)
#library(viridis)  
library(patchwork)
library(writexl)
library(gganimate)
library(magick)

# Read in files from code 02_thresholding-and-averaging.R - the data where embryos have been thresholded by the zscore of velocity

thresholded_data <- read.xlsx('results/spreadsheets/2024-02-20_thresholded_data.xlsx') %>% 
  mutate(position = factor(position, levels = c('C', 'N', 'S', 'E', 'W')))


# Individual embryos  - high responders -----
# anterior "20221214_E5_12ss" 
# posterior "20221214_E3_14ss"


# anterior "20221214_E5_12ss" -----

vel_3_df <- thresholded_data %>% 
  filter(measure == c('vel'), embryo.id == '20221214_E5_12ss', position == 'N') %>% 
  mutate(mean = mean / mean_pop,
         std = std / mean_pop,
         timepoint = timepoint + 5)

vel_3_df <- thresholded_data %>% 
  filter(measure == c('vel', 'vang'), embryo.id == '20221214_E5_12ss', position == 'N') %>% 
  select(timepoint, measure, mean, std) %>% 
  pivot_wider(names_from = measure, values_from = c(mean, std))

vel_3_df %>% 
  mutate(anterior_vel = mean_vel * sin(-mean_vang)) %>% 
  ggplot(aes(x = timepoint, y = anterior_vel)) +
  geom_line() +
  annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
  annotate("text", x = 60, y = 0.011, label = "ON", size = 4) +
  #geom_ribbon(aes(ymin = (mean - std_vel), ymax = (mean + std_vel)), fill = "black", alpha = 0.3) +
  geom_line(linewidth = 1) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Time [s]') +
  ylab(expression(paste("Velocity [µm ", s^-1, "]"))) 

(
  vel_3 <- vel_3_df %>% 
    filter(measure == 'vel', embryo.id == '20221214_E5_12ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 0.011, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(linewidth = 1) +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]"))) 
)

(
  animated_plot <- vel_3_df %>% 
    filter(measure == 'vel', embryo.id == '20221214_E5_12ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = mean), group = 1) +
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 0.3, label = "ON", size = 10) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(linewidth = 1) +
    theme_bw() +
    theme(axis.text=element_text(size = 20),
          axis.title=element_text(size = 20)) +
    scale_y_continuous(breaks = c(0,1,2,3,4,5,6)) +
    xlab('Time [s]') +
    ylab(expression(paste("Normalised Velocity"))) +
    geom_point(size = 2) +
    transition_reveal(timepoint)
)

# ------

(
  vang_3 <- thresholded_data %>% 
    filter(measure == 'vang', embryo.id == '20221214_E5_12ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = (mean * (180 / pi)))) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45)) +
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
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
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
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
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
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
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
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
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
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
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
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
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
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
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
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
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
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
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
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
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
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


animation <- animate(animated_plot, nframes=74, renderer=magick_renderer())
image_write_gif(animation, 'results/plots/forces conference/20221214_E5_12ss_vel_animation.gif') # then need to export as tif in preview before making as movie in fiji

unique(thresholded_data$somite)

# Plots for Figure 4 of paper - Velocity - representative anterior and posterior embryos -------
# anterior "20221018_E1_15ss"  ------

(
  vel_20221018_E1_15ss <- thresholded_data %>% 
    filter(measure == 'vel', embryo.id == '20221018_E1_15ss', position == 'N') %>% 
    mutate(mean = mean * 60,
           std = std * 60) %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 0.015, label = "ON", size = 4) +
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
    ylab(expression(paste("Velocity [µm ", min^-1, "]")))
)

(
  vang_20221018_E1_15ss <- thresholded_data %>% 
    filter(measure == 'vang', embryo.id == '20221018_E1_15ss', position == 'N') %>% 
    mutate(mean = (mean * (180 / pi)),
           std = (std * (180 / pi))) %>% 
    ggplot(aes(x = timepoint, y = mean )) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45),
                    labels = c(180, 135, 'Posterior  90', 45, 0, -45, 'Anterior  -90', -135, -180)) +
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    #annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    #annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
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
    ylab(expression(paste("Velocity Direction [º]")))
)


# posterior "20221214_E3_14ss"----------

(
  vel_20221214_E3_14ss  <- thresholded_data %>% 
    filter(measure == 'vel', embryo.id == '20221214_E3_14ss', position == 'S') %>% 
    mutate(mean = mean * 60,
           std = std * 60) %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 0.015, label = "ON", size = 4) +
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
    ylab(expression(paste("Velocity [µm ", min^-1, "]")))
)

(
  vang_20221214_E3_14ss  <- thresholded_data %>% 
    filter(measure == 'vang', embryo.id == '20221214_E3_14ss', position == 'S') %>% 
    mutate(mean = (mean * (180 / pi)),
           std = (std * (180 / pi))) %>% 
    ggplot(aes(x = timepoint, y = mean )) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45),
                    labels = c(180, 135, 'Posterior  90', 45, 0, -45, 'Anterior  -90', -135, -180)) +
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    #annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    #annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
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
    ylab(expression(paste("Velocity Direction [º]")))
)

filename <- 'Fig4_representative_anterior_embryo_velocity.png'
ggsave(filename, plot = vel_20221018_E1_15ss, path = 'results/plots/paper/', width = 4, height = 4)
filename <- 'Fig4_representative_anterior_embryo_vang.png'
ggsave(filename, plot = vang_20221018_E1_15ss, path = 'results/plots/paper/', width = 4.5, height = 4)


filename <- 'Fig4_representative_posterior_embryo_velocity.png'
ggsave(filename, plot = vel_20221214_E3_14ss, path = 'results/plots/paper/', width = 4, height = 4)
filename <- 'Fig4_representative_posterior_embryo_vang.png'
ggsave(filename, plot = vang_20221214_E3_14ss, path = 'results/plots/paper/', width = 4.5, height = 4)




# test plot below----

(
  vel_20231026_E1_16ss <- thresholded_data %>% 
    filter(measure == 'vel', embryo.id == '20231026_E1_16ss', position != c('W', "E")) %>% 
    mutate(position = factor(position, levels = c('N', 'C', 'S', 'E', 'W'))) %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 0.012, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    geom_vline(xintercept = 30, linetype = 'dashed') +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    ylim(-0.0002, 0.012) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)

filename <- '20231026_E1_16ss_CNS_vel.png'
ggsave(filename, plot = vel_20231026_E1_16ss , path = 'results/plots/', width = 15, height = 5)

thresholded_data %>% 
  filter(measure == 'vel', embryo.id == '20231026_E1_16ss', position == 'N') %>%
  slice_max(mean) %>% 
  select(timepoint)
