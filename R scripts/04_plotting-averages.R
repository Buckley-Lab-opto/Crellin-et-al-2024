'04
Plotting Averaged embryos'
# # = normal comments, '##' = notes to myself

# Load libraries
library(tidyverse)
library(openxlsx)
library(circular) # remember to do this if not circular - stats::sd(mean)
library(pdf2pptx)
#library(ggquiver)
#library(viridis)  
library(patchwork)
library(writexl)

# Read in files from code 02_thresholding-and-averaging.R - the data where embryos have been thresholded by the zscore of velocity

thresholded_data <- read.xlsx('results/spreadsheets/2024-06-20_thresholded_data.xlsx') %>% 
  mutate(position = factor(position, levels = c('C', 'N', 'S', 'E', 'W')))


averaged_data <- read.xlsx('results/spreadsheets/2024-06-20_averaged_data.xlsx') %>% 
  mutate(position = factor(position, levels = c('C', 'N', 'S', 'E', 'W')))

averaged_data <- read.xlsx('results/spreadsheets/2024-06-20_averaged_data_ungrouped.xlsx') %>% 
  mutate(position = factor(position, levels = c('C', 'N', 'S', 'E', 'W'))) %>% 
  mutate(time = time + 5)


# Plotting averaged data ------
# plot 1 of the pair e.g. vel = the average of CSNE regions for anterior / posterior embryos
# plot 2 of the pair e.g. vel_N_S = the average of N regions for anterior and S region for posterior embryos

plot_names <- as_labeller(c(`anterior` = 'Anterior & N', `posterior` = 'Posterior & S'))

(
  vel <- averaged_data %>% 
    filter(measure == 'vel', !position == 'W') %>% 
    mutate(mean = mean * 60, #making the units um/min instead of um/s
           std = std * 60,
           SEM = SEM * 60) %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    annotate("text", x = 60, y = 0.6, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - SEM), ymax = (mean + SEM)), fill = "black", alpha = 0.3) +
    # geom_line(data = thresholded_data %>%
    #             filter(measure == 'vel') %>% 
    #             mutate(mean = mean * 60,
    #                    std = std * 60),
    #           aes(x = timepoint, y = mean, colour = embryo.id),
    #           alpha = 0.5) +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(direction), 
               cols = vars(position)) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 10),
      strip.text.y = element_text(size = 10),
      panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity [µm ", min^-1, "]")))
)

(
  vel_N_S <- averaged_data %>% 
    mutate(mean = mean * 60,
           std = std * 60,
           SEM = SEM * 60) %>% 
    filter(measure == 'vel', (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S')) %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    annotate("text", x = 60, y = 0.5, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (mean - SEM), ymax = (mean + SEM)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_classic(base_size = 18) +
    facet_wrap(~direction, labeller = plot_names) +
    theme(text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 18),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity [µm ", min^-1, "]")))
)

(
  vang <- averaged_data %>% 
    filter(measure == 'vang', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -90),
                    labels = c(180, 'Posterior  90', 0, 'Anterior  -90', -180)) +
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    annotate("text", x = 60, y = -190, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    #annotate("text", x = -90, y = -90, label = "Anterior", size = 4) +
    #annotate("text", x = -90, y = 90, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = (mean - var), ymax = (mean + var)), fill = "black", alpha = 0.3) +
    theme_classic(base_size = 18) +
    facet_grid(rows = vars(direction), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity Direction (º)")))
)

(
  vang_N_S <- averaged_data %>% 
    filter(measure == 'vang', (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S')) %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -90),
                    labels = c(180, 'Posterior  90', 0, 'Anterior  -90', -180)) +
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    annotate("text", x = 60, y = -250, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'black', linetype = 'longdash') +
    geom_hline(aes(yintercept = 90), size = 0.3, colour = 'red', linetype = 'longdash') +
    geom_hline(aes(yintercept = -90), size = 0.3, colour = 'red', linetype = 'longdash') +
    #annotate("text", x = -90, y = -90, label = "Anterior", size = 4) +
    #annotate("text", x = -90, y = 90, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    theme_classic(base_size = 18) +
    facet_wrap(~direction, labeller = plot_names) +
    theme(aspect.ratio = 1,
          text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 18),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity Direction (º)")))
)

(
  str <- averaged_data %>% 
    filter(measure == 'str', !position == 'W') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    annotate("text", x = 60, y = 8e-04, label = "ON", size = 4) +
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
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    annotate("text", x = 60, y = 6.5e-04, label = "ON", size = 4) +
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
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    annotate("text", x = 60, y = 1.8e-04, label = "ON", size = 4) +
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
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    annotate("text", x = 60, y = 1.2e-04, label = "ON", size = 4) +
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
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
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
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    annotate("text", x = 60, y = 3e-04, label = "ON", size = 4) +
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
    scale_y_reverse(breaks = seq(180, -180, by = -45),
                    labels = c(180, 135, 'Posterior  90', 45, 0, -45, 'Anterior  -90', -135, -180)) +
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    annotate("text", x = 60, y = -180, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    #annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    #annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
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
    expand_limits(y = c(50, -50)) +
    scale_y_reverse(breaks = seq(45, -45, by = -45)) +
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    annotate("text", x = 60, y = -55, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'red', linetype = 'longdash') +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    #annotate("text", x = -100, y = -135, label = "Anterior", size = 4) +
    #annotate("text", x = -100, y = 135, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    theme_bw() +
    facet_wrap(~direction, labeller = plot_names) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Anisotropic Strain Direction [º]")))
  
)

(
  vang_N <- averaged_data %>% 
    filter(measure == 'vang', (direction == 'anterior' & position == 'N')) %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    scale_y_reverse(breaks = seq(180, -180, by = -45),
                    labels = c(180, 135, 'Posterior  90', 45, 0, -45, 'Anterior  -90', -135, -180)) +
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    annotate("text", x = 60, y = -210, label = "ON", size = 6) +
    geom_hline(aes(yintercept = 90), size = 0.3, colour = 'red', linetype = 'longdash') +
    geom_hline(aes(yintercept = -90), size = 0.3, colour = 'red', linetype = 'longdash') +
    #annotate("text", x = -90, y = -90, label = "Anterior", size = 4) +
    #annotate("text", x = -90, y = 90, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = (mean - var), ymax = (mean + var)), fill = "black", alpha = 0.3) +
    theme_classic(base_size = 18) +
    theme(strip.background = element_blank(),
          axis.text=element_text(size = 18),
          axis.title=element_text(size = 18)) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity Direction [º]")))
)

(
  vang_N_S_opposite <- averaged_data %>% 
    filter(measure == 'vang', (direction == 'anterior' & position == 'S') | (direction == 'posterior' & position == 'N')) %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    expand_limits(y = c(190, -190)) +
    scale_y_reverse(breaks = seq(180, -180, by = -90),
                    labels = c(180, 'Posterior  90', 0, 'Anterior  -90', -180)) +
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    annotate("text", x = 60, y = -250, label = "ON", size = 4) +
    geom_hline(aes(yintercept = 0), size = 0.3, colour = 'black', linetype = 'longdash') +
    geom_hline(aes(yintercept = 90), size = 0.3, colour = 'red', linetype = 'longdash') +
    geom_hline(aes(yintercept = -90), size = 0.3, colour = 'red', linetype = 'longdash') +
    #annotate("text", x = -90, y = -90, label = "Anterior", size = 4) +
    #annotate("text", x = -90, y = 90, label = "Posterior", size = 4) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    theme_classic(base_size = 18) +
    facet_wrap(~direction, labeller = plot_names) +
    theme(aspect.ratio = 1,
          text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 18),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity Direction (º)")))
)

#Saving the plots as pdf and pptx -----

filename <- 'vang.png'
ggsave(filename, plot = vang_N, path = 'results/plots/forces conference/', width = 5.5, height = 4)


filename <- paste(Sys.Date(), 'averaged_CNSE_plots', sep = "_")

pdf(sprintf('results/plots/%s.pdf', filename), height = 4, width = 7) #number is inches 4 and 7 convert to 16:9 ratio
vel
vang
str
iso
ani
aniang
dev.off()

pdf2pptx(sprintf('results/plots/%s.pdf', filename), sprintf('results/plots/%s.pptx', filename), ratio = 169)

filename <- paste(Sys.Date(), 'averaged_AN-PS_plots', sep = "_")

pdf(sprintf('results/plots/%s.pdf', filename), height = 4, width = 7) #number is inches 4 and 7 convert to 16:9 ratio
vel_N_S
vang_N_S
str_N_S
iso_N_S
ani_N_S
aniang_N_S
dev.off()

pdf2pptx(sprintf('results/plots/%s.pdf', filename), sprintf('results/plots/%s.pptx', filename), ratio = 169)

# Plots for paper --------
# filename <- 'Fig4_average_anterior_posterior_velocity.png'
# filename <- paste(Sys.Date(), filename, sep = "_")
# ggsave(filename, plot = vel_N_S, path = 'results/plots/paper/', width = 8, height = 5)
# filename <- 'Fig4_average_anterior_posterior_vang.png'
# filename <- paste(Sys.Date(), filename, sep = "_")
# ggsave(filename, plot = vang_N_S, path = 'results/plots/paper/', width = 9, height = 5)

filename <- paste0('results/plots/paper/', Sys.Date(), '_Fig4_average_anterior_posterior_velocity.svg')
svglite(filename, width = 8, height = 5)
vel_N_S
dev.off()

filename <- paste0('results/plots/paper/', Sys.Date(), '_Fig4_average_anterior_posterior_vang.svg')
svglite(filename, width = 9, height = 5)
vang_N_S
dev.off()

filename <- paste0('results/plots/paper/', Sys.Date(), '_Fig4_average_anterior_posterior_vang_opposite.svg')
svglite(filename, width = 9, height = 5)
vang_N_S_opposite
dev.off()
# 


