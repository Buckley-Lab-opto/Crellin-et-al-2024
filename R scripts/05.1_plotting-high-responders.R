'05.1
Plotting Individual high responding embryos'
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


# Individual embryos to show clare - high responders -----
# anterior "20221018_E1_15ss" "20221108_E8_15ss" "20221214_E5_12ss" 
# posterior "20221214_E3_14ss"

# anterior "20221018_E1_15ss" ------

(
  vel_1 <- thresholded_data %>% 
    filter(measure == 'vel', embryo.id == '20221018_E1_15ss', position == 'N') %>% 
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
  vang_1 <- thresholded_data %>% 
    filter(measure == 'vang', embryo.id == '20221018_E1_15ss', position == 'N') %>% 
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
  str_1 <- thresholded_data %>% 
    filter(measure == 'str', embryo.id == '20221018_E1_15ss', position == 'N') %>% 
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
  iso_1 <- thresholded_data %>% 
    filter(measure == 'iso', embryo.id == '20221018_E1_15ss', position == 'N') %>% 
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
  ani_1 <- thresholded_data %>% 
    filter(measure == 'ani', embryo.id == '20221018_E1_15ss', position == 'N') %>% 
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
  aniang_1 <- thresholded_data %>% 
    filter(measure == 'aniang', embryo.id == '20221018_E1_15ss', position == 'N') %>% 
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
# anterior  "20221108_E8_15ss" -----
(
  vel_2 <- thresholded_data %>% 
    filter(measure == 'vel', embryo.id == '20221108_E8_15ss', position == 'N') %>% 
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
  vang_2 <- thresholded_data %>% 
    filter(measure == 'vang', embryo.id == '20221108_E8_15ss', position == 'N') %>% 
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
  str_2 <- thresholded_data %>% 
    filter(measure == 'str', embryo.id == '20221108_E8_15ss', position == 'N') %>% 
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
  iso_2 <- thresholded_data %>% 
    filter(measure == 'iso', embryo.id == '20221108_E8_15ss', position == 'N') %>% 
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
  ani_2 <- thresholded_data %>% 
    filter(measure == 'ani', embryo.id == '20221108_E8_15ss', position == 'N') %>% 
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
  aniang_2 <- thresholded_data %>% 
    filter(measure == 'aniang', embryo.id == '20221108_E8_15ss', position == 'N') %>% 
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

# anterior "20221214_E5_12ss" -----
(
  vel_3 <- thresholded_data %>% 
    filter(measure == 'vel', embryo.id == '20221214_E5_12ss', position == 'N') %>% 
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

filename <- paste(Sys.Date(), 'high-responding-individuals_plots', sep = "_")

pdf(sprintf('results/plots/%s.pdf', filename), height = 4, width = 7) #number is inches 4 and 7 convert to 16:9 ratio
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

pdf2pptx(sprintf('results/plots/%s.pdf', filename), sprintf('results/plots/%s.pptx', filename), ratio = 169)

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
