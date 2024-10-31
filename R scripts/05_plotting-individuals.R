'05
Plotting Individual embryos'
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

# Read in files from code 02_thresholding-and-averaging.R - the data where embryos have been thresholded by the zscore of velocity

thresholded_data <- read.xlsx('results/spreadsheets/2024-01-24_thresholded_data.xlsx') %>% 
  mutate(position = factor(position, levels = c('C', 'N', 'S', 'E', 'W')))

averaged_data <- read.xlsx('results/spreadsheets/2024-01-25_averaged_data.xlsx') %>% 
  mutate(position = factor(position, levels = c('C', 'N', 'S', 'E', 'W')))

# Plotting individual embryos  ------
# Plots of CNSE of every anterior / posterior embryo



(
  vel_ant <- thresholded_data %>% 
    filter(measure == 'vel', direction == 'anterior', position != c('W', 'E')) %>% 
    mutate(position = factor(position, levels = c('N', 'C', 'S'))) %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    annotate("text", x = 60, y = 0.011, label = "ON", size = 4) +
    #geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 0.8) +
    geom_vline(xintercept = 150 - 120, linetype = 'dashed') +
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
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
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
  vang_post <- thresholded_data %>% 
    filter(measure == 'vang', direction == 'posterior', !position == 'W') %>% 
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
  str_ant <- thresholded_data %>% 
    filter(measure == 'str', direction == 'anterior', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
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
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
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
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
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
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
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
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
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
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
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

(
  aniang_post <- thresholded_data %>% 
    filter(measure == 'aniang', direction == 'posterior', !position == 'W') %>% 
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


filename <- paste(Sys.Date(), 'thresholded-individuals_plots', sep = "_")

pdf(sprintf('results/plots/%s.pdf', filename), height = 9, width = 16) #number is inches 4 and 7 convert to 16:9 ratio
vel_ant + (vel_post + plot_layout(nrow = 2))
vang_ant + (vang_post + plot_layout(nrow = 2))
str_ant + (str_post + plot_layout(nrow = 2))
iso_ant + (iso_post + plot_layout(nrow = 2))
ani_ant + (ani_post + plot_layout(nrow = 2))
aniang_ant + (aniang_post + plot_layout(nrow = 2))
dev.off()

pdf2pptx(sprintf('results/plots/%s.pdf', filename), sprintf('results/plots/%s.pptx', filename), ratio = 169)