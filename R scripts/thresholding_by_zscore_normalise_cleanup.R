'03
Averaging thresholded embryos'
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

# Read in file from code 02_thresholding-by-zscore.R - the data where embryos have been thresholded by the zscore of velocity

thresholded_data <- read.xlsx('results/spreadsheets/2024-01-24_thresholded_data.xlsx')

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


