'06
Normalising data 
Exploratory analysis / visualisation to see if it reveals trends eg. second peaks'
# # = normal comments, '##' = notes to myself

# Load libraries
library(tidyverse)
library(openxlsx)
library(pdf2pptx)
library(patchwork)
library(writexl)
library(ggpubr)
library(svglite)

# Read in files from code 02_thresholding-and-averaging.R - the data where embryos have been thresholded by the zscore of velocity
# Use ungrouped data

#thresholded_data <- read.xlsx('results/spreadsheets/2024-05-13_thresholded_data.xlsx') %>% 
thresholded_data <- read.xlsx('results/spreadsheets/2024-06-20_thresholded_data.xlsx') %>% 
  mutate(position = factor(position, levels = c('C', 'N', 'S', 'E', 'W'))) 


#thresholded_data_ungrouped <- read.xlsx('results/spreadsheets/2024-05-13_thresholded_data_ungrouped.xlsx') %>% 
thresholded_data_ungrouped <- read.xlsx('results/spreadsheets/2024-06-20_thresholded_data_ungrouped.xlsx') %>% 
  mutate(position = factor(position, levels = c('C', 'N', 'S', 'E', 'W'))) 
#%>%   mutate(time = time + 5)



# Normalised velocity by peak in each position ------

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

# Normalised velocity by peak in N or S ---------


# peak_velocity_all_vel <- thresholded_data %>%
#   filter(measure == 'vel', position %in% c('N', 'S'), timepoint >= 0, timepoint <= 120) %>%
#   group_by(embryo.id) %>%
#   slice_max(order_by = mean) %>%
#   rename(peak_vel = mean,
#          peak_vel_tp = timepoint,
#          position_peak = position) %>% 
#   select(embryo.id, peak_vel_tp, peak_vel, position_peak) %>% 
#   ungroup()
# 
# mean_peak_vel <- peak_velocity_all_vel %>% 
#   summarise(time = mean(peak_vel_tp),
#             time_sd = sd(peak_vel_tp),
#             vel = mean(peak_vel*60),
#             sd = sd(peak_vel*60))
# 
# normalised_data_vel <- thresholded_data %>% 
#   left_join(peak_velocity_all_vel, by = c('embryo.id'), keep = FALSE)
# 
# normalised_data_vel <- normalised_data_vel %>% 
#   group_by(timepoint, measure, position, direction) %>% 
#   filter(measure == 'vel') %>% 
#   mutate(normalised_mean = (mean / peak_vel)) %>% 
#   ungroup()

# Normalised measures by average in preact ---------

## mean_pop is grouped by position, here i group be positon, but should the threshold not be grouped by positon - eg more representative
# norm_to_preact <- thresholded_data %>%
#   group_by(timepoint, measure, position, direction) %>%
#   mutate(normalised_mean = (mean / mean_pop),
#          normalised_std = (std / mean_pop)) %>%
#   ungroup()
# 
# peak_velocity_all_vel <- norm_to_preact %>%
#   filter(measure == 'vel', position %in% c('N', 'S'), timepoint >= 0, timepoint <= 120) %>%
#   group_by(embryo.id) %>%
#   slice_max(order_by = normalised_mean) %>%
#   rename(peak_vel = normalised_mean,
#          peak_vel_tp = timepoint,
#          position_peak = position) %>% 
#   select(embryo.id, peak_vel_tp, peak_vel, position_peak) %>% 
#   ungroup()
# 
# deact_peak <- norm_to_preact %>%
#   filter(measure == 'vel', (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S'), timepoint >= 120) %>%
#   group_by(embryo.id) %>%
#   slice_max(order_by = normalised_mean) %>%
#   rename(peak_vel = normalised_mean,
#          peak_vel_tp = timepoint,
#          position_peak = position) %>% 
#   select(embryo.id, peak_vel_tp, peak_vel, position_peak) %>% 
#   ungroup()
# 
# mean_peak_vel <- peak_velocity_all_vel %>% 
#   summarise(time = mean(peak_vel_tp),
#             time_sd = sd(peak_vel_tp),
#             vel = mean(peak_vel),
#             sd = sd(peak_vel))
# 
# mean_peak_vel <- deact_peak %>% 
#   summarise(time = mean(peak_vel_tp),
#             time_sd = sd(peak_vel_tp),
#             vel = mean(peak_vel),
#             sd = sd(peak_vel))

norm_to_preact_ungrouped <- thresholded_data_ungrouped %>% 
  group_by(timepoint, measure, position, direction) %>% 
  mutate(normalised_mean = (mean / mean_pop),
         normalised_std = (std / mean_pop)) %>% 
  ungroup()

peak_velocity_all_vel <- norm_to_preact_ungrouped %>%
  filter(measure == 'vel', position %in% c('N', 'S'), timepoint >= 0, timepoint <= 120) %>%
  group_by(embryo.id) %>%
  slice_max(order_by = normalised_mean) %>%
  rename(peak_vel = normalised_mean,
         peak_vel_tp = timepoint,
         position_peak = position) %>% 
  select(embryo.id, peak_vel_tp, peak_vel, position_peak) %>% 
  ungroup()

peak_velocity_all_vel %>% 
              summarise(time = mean(peak_vel_tp),
              time_sd = sd(peak_vel_tp),
              vel = mean(peak_vel),
              sd = sd(peak_vel))

deact_peak <- norm_to_preact_ungrouped %>%
  filter(measure == 'vel', (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S'), timepoint >= 120) %>%
  group_by(embryo.id) %>%
  slice_max(order_by = normalised_mean) %>%
  rename(peak_vel = normalised_mean,
         peak_vel_tp = timepoint,
         position_peak = position) %>%
  select(embryo.id, peak_vel_tp, peak_vel, position_peak) %>%
  ungroup()

deact_peak %>%
  summarise(time = mean(peak_vel_tp),
            time_sd = sd(peak_vel_tp),
            vel = mean(peak_vel),
            sd = sd(peak_vel))


# aniang and vang preactivation comparision for figure 7 ------
library(circular)


preact_aniang <- thresholded_data_ungrouped %>% 
    filter(position %in% c('C','N','S'), measure == 'aniang') %>% 
    dplyr::filter(timepoint >= -120 & timepoint <= -5) %>% 
    mutate(aniang = as.circular(mean, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
    group_by(direction, position, embryo.id) %>% 
    summarise(aniang_preact = as.double(mean.circular(x = aniang, na.rm = T))) %>% 
  ungroup()

ant_aniang <- preact_aniang %>% 
  filter(direction == 'anterior', position == 'C') %>% 
  pull(aniang_preact) %>% 
  circular(units = "radians")

post_aniang <- preact_aniang %>% 
  filter(direction == 'posterior', position == 'C') %>% 
  pull(aniang_preact) %>% 
  circular(units = "radians")

watson_test <- watson.two.test(ant_aniang, post_aniang,  alpha = 0.05)
print(watson_test)

(plot <- preact_aniang %>% 
  mutate(aniang_preact = aniang_preact * (180 / pi)) %>% 
 # filter(position == 'C') %>% 
  ggplot(aes(x = direction, y = aniang_preact)) +
  geom_boxplot(aes(fill = direction), alpha = 0.2) +
  geom_jitter(aes(colour = direction)) +
  labs(title = '', #measure_lab,
       x = '',
       y = 'Anisotropic strain 
rate angle of contraction (º)') +
  # geom_bracket(
  #   xmin = 1,
  #   xmax = 2,
  #   label = 'ns',
  #   y.position = 37
  # ) +
  facet_wrap(~position)+
  theme_classic(base_size = 18) +
  theme(text = element_text(size = 20, family = 'Avenir'),
        aspect.ratio = 2,
        legend.position = 'none',
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.background = element_blank(),
        panel.spacing = unit(0.5, 'lines')) 
)

filename <- paste0('results/plots/paper/', Sys.Date(), '_aniang_ROI_before_CNS.svg')
svglite(filename, width = 8, height = 5)
plot
dev.off()


preact_vang <- thresholded_data_ungrouped %>% 
  filter(position %in% c('C','N','S'), measure == 'vang') %>% 
  dplyr::filter(timepoint >= -120 & timepoint <= -5) %>% 
  mutate(vang = as.circular(mean, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  group_by(direction, position, embryo.id) %>% 
  summarise(vang_preact = as.double(mean.circular(x = vang, na.rm = T))) %>% 
  ungroup()

ant_vang <- preact_vang %>% 
  filter(direction == 'anterior', position == 'C') %>% 
  pull(vang_preact) %>% 
  circular(units = "radians")

post_vang <- preact_vang %>% 
  filter(direction == 'posterior', position == 'C') %>% 
  pull(vang_preact) %>% 
  circular(units = "radians")

watson_test <- watson.two.test(ant_vang, post_vang) #critical value for n = 9 is 0.204 - https://pdf.sciencedirectassets.com/311282/3-s2.0-C20130074257/3-s2.0-B9780124711501500170/main.pdf?X-Amz-Security-Token=IQoJb3JpZ2luX2VjEKf%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJGMEQCIGmte369pSZ2jn5WheywAR%2BQ2i6Er%2Bn1Q4vUSBQkVg%2BrAiAF5O9cov5qLQKmrpExCvydvmk5FaR%2FdriAaB3DMG0Fdiq8BQi%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F8BEAUaDDA1OTAwMzU0Njg2NSIM8ht07ezGkgLPm%2F7TKpAF1uo%2BgtQMAMmhe60oGbEql0jzDkG5PB%2Bz5Rd3B3hxwY4fUwM%2Fd4F0BpfuH8HzYPlGdvzmWAy3ivrBQGAaG7FlWPUKTs5RZu1iN9cZa4%2BxgzlIhW3MH7va4vy9Jt4t5yZnUmkoeQ5r9sNoaMcMaSU3tbBJToVdoDyxdoOEKGH8rldc5wBkfouW5nvoVBaRenGdxcm1YCXtv0KwIOVeXHVQ7fpgC9m7D3vdb3STMH73wbmnYRlbChItLzMgPDR7ChjJAGnRHwv8OmM9HuOjKydXsZXG3AhrStsEJqqZW2xo0cjybEFibRXDSp4S85RIAnVfepG49sbehcRBEOhcj68COeyOFHorcDsfwL2ho9uB%2BG9AGUm7BXyM5WtK58q%2BSOycrEvFpQJvbjuju3ufeCRjoOzyQ05Jkd7fCI5k%2FNyC54VuwdFwevEFzI1T4JSZuKiQny9lYwX4QkG75JTuYiANRkg6ZZFls0HvcOXPqVBfkPAz4AGWFjfAYtsvt4ixCzQsaSRVLrLiiLPEfgHq653kAkYw%2FF%2BDRKj6erXNKoL9L9PEE5zc6f3nFD2wl1gqXJzC%2B0502nD3ytYxCaYcOAcWL2h0QLnRRaDPCcdv79feyjlufeUq%2FPCSqhs881f3UMxmXYHJboNfOGHyRbIPGsbfybjoWtQja2jj1P9iK452r%2F%2FtcIcu9rkw1wbY7GrQDpNagkUJev2YSQ9ndDR4PdtOFMiiTZQRGo5Fb9ZHqjYdA3jXwZF4iwMdZNZjrR%2BeHHDmiS8J9233WJNPeboDFhifu8xvrQUqQqdjaeLK%2FJ0C%2FrE4R5yj43hIPkM%2BUPh4qj6HuX2ZSDbdE2eIzQtgmIPxiq0E4uU1p%2F7TVDITrHG8dGIw0bPctgY6sgE7zhrsL23sqEuvFuak7fWMDOVmU8AP%2BZsUKHSfKI0XzsxZI9h1LrrGEFGgA55qRH5vsjKEuy1UsZvvZZUNIdbIj%2FxJcelPu4ECOGHrgKij2pNkeYv%2FM2OWu0xhLvSFHeb0P%2BBD2%2B%2FFjqrSqs89g%2FLrHQk7l8NLBtDnIAspvexhnwQVEQ0cwdsp5ZnBTpoI8R6FkwHi53%2B48kh3J1qqIdCuaiv9RzEK84pDKkHVVwH9FHeV&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20240903T150002Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAQ3PHCVTYSR62CDQ4%2F20240903%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=134bf4153e6bf606864dfe27b78fa0ed7b24e49db2f9233238997557d5f6bda6&hash=613a526f4039e7c4be2e079490fc8244333a5993284e81f16aeeefa9ccce2dba&host=68042c943591013ac2b2430a89b270f6af2c76d8dfd086a07176afe7c76c2c61&pii=B9780124711501500170&tid=spdf-15889b88-8a01-4c0a-a049-16593803dc0e&sid=53f646df7ede914b8c8a44b0b330e7731809gxrqb&type=client&tsoh=d3d3LnNjaWVuY2VkaXJlY3QuY29t&ua=1d06560307515a055456&rr=8bd69b69e99cbed8&cc=gb
p_value <- 'p > 0.1'

(plot <- preact_vang %>% 
  mutate(vang_preact = vang_preact * (180 / pi)) %>% 
   filter(position == 'C') %>% 
  ggplot(aes(x = direction, y = vang_preact)) +
  geom_boxplot(aes(fill = direction), alpha = 0.2) +
  geom_jitter(aes(colour = direction), width = 0.2, height = 0, size = 2) +
  labs(title = '', #measure_lab,
       x = '',
       y = 'Velocity 
direction (º)') +
  geom_bracket(
    xmin = 1,
    xmax = 2,
    label = 'ns',
    y.position = 180,
    family = 'avenir') +
    annotate('text', x = 1.5, y = -170, label = 'p > 0.1', family  = 'avenir') +
  scale_y_reverse()+
  facet_wrap(~position)+
  theme_classic(base_size = 18) +
  theme(text = element_text(size = 20, family = 'Avenir'),
        aspect.ratio = 2,
        legend.position = 'none',
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(0.5, 'lines')) 
)
filename <- paste0('results/plots/paper/', Sys.Date(), '_vang_ROI_before_C.svg')
svglite(filename, width = 5, height = 5.5)
plot
dev.off()
    
# Averaging normalised velocity ------
# averaged_norm <- norm_to_preact %>%  
#   group_by(timepoint, measure, position, direction) %>% 
#   summarise(normalised_std = stats::sd(normalised_mean),
#             SEM = normalised_std / sqrt(length(unique(embryo.id))),
#             normalised_mean = mean(normalised_mean)
#             #normalised_std = sqrt(sum(normalised_std^2) / n()),
#             #SEM = normalised_std / sqrt(length(unique(embryo.id)))
#             ) %>% 
#   ungroup()

averaged_norm_ungrouped <- norm_to_preact_ungrouped %>%  
  group_by(timepoint, measure, position, direction) %>% 
  summarise(normalised_std = stats::sd(normalised_mean),
             SEM = normalised_std / sqrt(length(unique(embryo.id))),
             normalised_mean = mean(normalised_mean)
             #normalised_std = sqrt(sum(normalised_std^2) / n()),
             #SEM = normalised_std / sqrt(length(unique(embryo.id)))
  ) %>% 
  ungroup()

averaged_norm_ungrouped %>% 
  filter(measure == 'vel', (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S'), timepoint >= 120, timepoint <= 240) %>%
  group_by(direction) %>% 
  slice_max(order_by = normalised_mean) %>%
  rename(peak_vel = normalised_mean,
         peak_vel_tp = timepoint,
         position_peak = position) %>% 
  ungroup() %>% 
  summarise(mean(peak_vel),
            mean(peak_vel_tp))

# Min-max feature scaling = Normalised between 0 and 1 -------
# 0 = preactivation mean, 1 = peak activation

# normalised_0_1 <- normalised_data_vel %>% 
#   filter(measure == 'vel') %>% 
#   group_by(timepoint, measure, position, direction) %>% 
#   mutate(normalised_mean = ( (mean - mean_pop) / (peak_vel - mean_pop))) %>% 
#   ungroup()

# Normalised total strain by peak in N or S ---------
# this doesnt make sense if not a peak in these

# peak_str_all <- thresholded_data %>%
#   filter(measure == 'str', position %in% c('N', 'S'), timepoint >= 0, timepoint <= 120) %>%
#   group_by(embryo.id) %>%
#   slice_max(order_by = mean) %>%
#   rename(peak_str = mean,
#          peak_str_tp = timepoint,
#          position_peak_str = position) %>% 
#   select(embryo.id, peak_str_tp, peak_str, position_peak_str) %>% 
#   ungroup()
# 
# normalised_data_str <- thresholded_data %>% 
#   left_join(peak_str_all, by = c('embryo.id'), keep = FALSE)
# 
# normalised_data_str <- normalised_data_str %>% 
#   group_by(timepoint, measure, position, direction) %>% 
#   filter(measure == 'str') %>% 
#   mutate(normalised_str = (mean / peak_str) * 100) %>% 
#   ungroup()




# Plotting normalised velocity -------

plot_names <- as_labeller(c(`anterior` = 'Anterior & N', `posterior` = 'Posterior & S'))

(
  vel_raw <- normalised_0_1 %>% 
    filter(measure == 'vel',  !position == 'W', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = mean)) +
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    #annotate("text", x = 60, y = 1, label = "ON", size = 4) +
    # geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
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
  vel_norm_preacttopeak <- normalised_0_1 %>% 
    filter(measure == 'vel',  !position == 'W', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = normalised_mean)) +
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    #annotate("text", x = 60, y = 1, label = "ON", size = 4) +
    # geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    theme_bw() +
    ylim(-0.5, 1.5) +
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
  vel_norm_peak <- normalised_data_vel %>% 
    filter(measure == 'vel',  !position == 'W', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = normalised_mean)) +
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    #annotate("text", x = 60, y = 1, label = "ON", size = 4) +
   # geom_ribbon(aes(ymin = (mean - std), ymax = (mean + std)), fill = "black", alpha = 0.3) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    geom_line(size = 1) +
    theme_bw() +
    #ylim(0, 160) +
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
  vel_norm_preact <- norm_to_preact %>% 
    filter(measure == 'vel',  !position == 'W', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = normalised_mean)) +
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    #annotate("text", x = 60, y = 1, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (normalised_mean - normalised_std), ymax = (normalised_mean + normalised_std)), fill = "black", alpha = 0.3) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    geom_line(size = 1) +
    theme_bw() +
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
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
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
  str_norm_preact_ant <- norm_to_preact_ungrouped %>% 
    filter(measure == 'str',  !position == 'W', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = normalised_mean)) +
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    #annotate("text", x = 60, y = 1, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (normalised_mean - normalised_std), ymax = (normalised_mean + normalised_std)), fill = "black", alpha = 0.3) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Relative Total Strain Rate (to preactivation mean)")))
)

(
  str_norm_preact_post <- norm_to_preact %>% 
    filter(measure == 'str',  !position == 'W', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = normalised_mean)) +
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    #annotate("text", x = 60, y = 1, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (normalised_mean - normalised_std), ymax = (normalised_mean + normalised_std)), fill = "black", alpha = 0.3) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Relative Total Strain Rate (to preactivation mean)")))
)

(
  vel_ant_N <- normalised_data_vel %>% 
    filter(measure == 'vel',  position == 'N', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = normalised_mean)) +
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
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
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
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
  ani_norm_preact_ant <- norm_to_preact %>% 
    filter(measure == 'ani',  !position == 'W', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = normalised_mean)) +
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    #annotate("text", x = 60, y = 1, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (normalised_mean - normalised_std), ymax = (normalised_mean + normalised_std)), fill = "black", alpha = 0.3) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Relative Anisotropic Strain Rate (to preactivation mean)")))
)

(
  ani_norm_preact_post <- norm_to_preact %>% 
    filter(measure == 'ani',  !position == 'W', direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = normalised_mean)) +
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    #annotate("text", x = 60, y = 1, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (normalised_mean - normalised_std), ymax = (normalised_mean + normalised_std)), fill = "black", alpha = 0.3) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    geom_line(size = 1) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Relative Anisotropic Strain Rate (to preactivation mean)")))
)


filename <- paste(Sys.Date(), 'normalised_plots', sep = "_")

pdf(sprintf('results/plots/%s.pdf', filename), height = 9, width = 16) #number is inches 4 and 7 convert to 16:9 ratio
vel_ant + (vel_post + plot_layout(nrow = 2))
str_ant + (str_post + plot_layout(nrow = 2))
vel_ant_N + (vel_post_S + plot_layout(nrow = 2))
str_ant_N + (str_post_S + plot_layout(nrow = 2))
dev.off()

pdf2pptx(sprintf('results/plots/%s.pdf', filename), sprintf('results/plots/%s.pptx', filename), ratio = 169)



str_norm_preact <- str_norm_preact_ant + (str_norm_preact_post + plot_layout(nrow = 2))
ani_norm_preact <- ani_norm_preact_ant + (ani_norm_preact_post + plot_layout(nrow = 2))

filename <- paste(Sys.Date(), 'norm_str.png', sep = "_")
ggsave(filename, plot = str_norm_preact, path = 'results/plots/', width = 16, height = 9)

filename <- paste(Sys.Date(), 'norm_ani.png', sep = "_")
ggsave(filename, plot = ani_norm_preact, path = 'results/plots/', width = 16, height = 9)



# Plots for Figure 4 of paper - Normalised Velocity  -------
# Including averaged, representative anterior and posterior embryos 
(
  vel_norm_preact <- averaged_norm_ungrouped %>% 
    filter(measure == 'vel', (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S')) %>% 
    ggplot(aes(x = timepoint, y = normalised_mean)) +
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    annotate("text", x = 60, y = 3, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (normalised_mean - SEM), ymax = (normalised_mean + SEM)), fill = "black", alpha = 0.3) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    geom_line(linewidth = 1) +
    theme_classic(base_size = 18) +
    facet_wrap(~direction, labeller = plot_names) +
    theme(aspect.ratio = 1,
          text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 18),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Relative Velocity")))
)

(
  vel_norm_preact_opposite <- averaged_norm_ungrouped %>% 
    filter(measure == 'vel', (direction == 'anterior' & position == 'S') | (direction == 'posterior' & position == 'N')) %>% 
    ggplot(aes(x = timepoint, y = normalised_mean)) +
    annotate("rect", xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    annotate("text", x = 60, y = 3, label = "ON", size = 4) +
    geom_ribbon(aes(ymin = (normalised_mean - SEM), ymax = (normalised_mean + SEM)), fill = "black", alpha = 0.3) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    geom_line(linewidth = 1) +
    theme_classic(base_size = 18) +
    facet_wrap(~direction, labeller = plot_names) +
    theme(aspect.ratio = 1,
          text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 18),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Relative Velocity")))
)



# (
#   vel_norm_preact_ant <- averaged_norm_ungrouped %>% 
#     filter(measure == 'vel', (direction == 'anterior' & position == 'N') ) %>% 
#     ggplot(aes(x = timepoint, y = normalised_mean)) +
#     annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
#     annotate("text", x = 60, y = 3, label = "ON", size = 6) +
#     geom_ribbon(aes(ymin = (normalised_mean - SEM), ymax = (normalised_mean + SEM)), fill = "black", alpha = 0.3) +
#     geom_hline(yintercept = 1, linetype = 'dashed') +
#     geom_line(linewidth = 1) +
#     ylim(0.55, 3) +
#     theme_classic(base_size = 18) +
#     facet_wrap(~direction, labeller = plot_names) +
#     theme(text = element_text(size = 28, family = 'Avenir'),
#           strip.background = element_blank(),
#           strip.text.x = element_text(size = 18),
#           strip.text.y = element_text(size = 18),
#           panel.spacing = unit(0.5, 'lines')) +
#     xlab('Time [s]') +
#     ylab(expression(paste("Normalised Velocity")))
# )


# (
#   str_norm_preact <- averaged_norm %>% 
#     filter(measure == 'str', (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S')) %>% 
#     ggplot(aes(x = timepoint, y = normalised_mean)) +
#     annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
#     annotate("text", x = 60, y = 2, label = "ON", size = 4) +
#     geom_ribbon(aes(ymin = (normalised_mean - SEM), ymax = (normalised_mean + SEM)), fill = "black", alpha = 0.3) +
#     geom_hline(yintercept = 1, linetype = 'dashed') +
#     geom_line(linewidth = 1) +
#     theme_classic(base_size = 18) +
#     facet_wrap(~direction, labeller = plot_names) +
#     theme(text = element_text(size = 28, family = 'Avenir', face = 'bold'),
#           strip.background = element_blank(),
#           strip.text.x = element_text(size = 18),
#           strip.text.y = element_text(size = 18),
#           panel.spacing = unit(0.5, 'lines')) +
#     xlab('Time [s]') +
#     ylab(expression(paste("Relative Total Strain Rate (to preactivation mean)")))
# )

# (
#   ani_norm_preact <- averaged_norm_ungrouped %>% 
#     filter(measure == 'ani', (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S')) %>% 
#     ggplot(aes(x = timepoint, y = normalised_mean)) +
#     annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
#     annotate("text", x = 60, y = 3, label = "ON", size = 10) +
#     geom_ribbon(aes(ymin = (normalised_mean - SEM), ymax = (normalised_mean + SEM)), fill = "black", alpha = 0.3) +
#     geom_hline(yintercept = 1, linetype = 'dashed') +
#     geom_line(linewidth = 1) +
#     theme_bw() +
#     facet_wrap(~direction, labeller = plot_names) +
#     theme(strip.background = element_blank(),
#           axis.text=element_text(size = 20),
#           axis.title=element_text(size = 20),
#           panel.spacing = unit(0.5, 'lines')) +
#     xlab('Time [s]') +
#     ylab(expression(paste("Relative Anisotropic Strain Rate (to preactivation mean)")))
# )
# 
# (
#   ani_norm_preact_ant <- averaged_norm_ungrouped %>% 
#     filter(measure == 'ani', (direction == 'anterior' & position == 'N')) %>% 
#     ggplot(aes(x = timepoint, y = normalised_mean)) +
#     annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
#     annotate("text", x = 60, y = 3, label = "ON", size = 6) +
#     geom_ribbon(aes(ymin = (normalised_mean - SEM), ymax = (normalised_mean + SEM)), fill = "black", alpha = 0.3) +
#     geom_hline(yintercept = 1, linetype = 'dashed') +
#     geom_line(linewidth = 1) +
#     theme_bw() +
#     ylim(0.55, 3) +
#     theme(strip.background = element_blank(),
#           axis.text=element_text(size = 18),
#           axis.title=element_text(size = 18)) +
#     xlab('Time [s]') +
#     ylab(expression(paste("Normalised Anisotropic Strain Rate")))
# )

filename <- paste0('results/plots/paper/', Sys.Date(), '_Fig4_norm_average_anterior_posterior_velocity_ungrouped.svg')
#ggsave(filename, plot = vel_norm_preact, path = 'results/plots/paper/', width = 8, height = 5)
svglite(filename, width = 8, height = 5)
vel_norm_preact
dev.off()

filename <- paste0('results/plots/paper/', Sys.Date(), '_Fig4_norm_average_anterior_posterior_velocity_ungrouped_opposite.svg')
#ggsave(filename, plot = vel_norm_preact, path = 'results/plots/paper/', width = 8, height = 5)
svglite(filename, width = 8, height = 5)
vel_norm_preact_opposite
dev.off()
# 
# filename <- 'norm_average_anterior_posterior_tot-strain.png'
# ggsave(filename, plot = str_norm_preact, path = 'results/plots/paper/', width = 8, height = 4)
# 
# filename <- 'norm_average_anterior_posterior_ani-strain_ungrouped_scaled.png'
# ggsave(filename, plot = ani_norm_preact, path = 'results/plots/paper/', width = 8, height = 4)
# 
# filename <- 'norm_average_anterior_posterior_ani-strain_ungrouped_scaled.png'
# ggsave(filename, plot = ani_norm_preact_ant, path = 'results/plots/forces conference/', width = 4, height = 4)
# 
# filename <- 'norm_average_anterior_posterior_vel_ungrouped_scaled.png'
# ggsave(filename, plot = vel_norm_preact_ant, path = 'results/plots/forces conference/', width = 4, height = 4)
# 


# anterior "20221214_E5_12ss"  ------
  
(
  vel_20221214_E5_12ss <- norm_to_preact_ungrouped %>% 
    filter(measure == 'vel', embryo.id == '20221214_E5_12ss', position == 'N') %>% 
    ggplot(aes(x = timepoint, y = normalised_mean)) +
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    annotate("text", x = 60, y = 6, label = "ON", size = 8) +
    geom_ribbon(aes(ymin = (normalised_mean - normalised_std), ymax = (normalised_mean + normalised_std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme_classic(base_size = 18) +
    theme(text = element_text(size = 28, family = 'Avenir'),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 18),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Relative Velocity")))
)


# posterior "20221214_E3_14ss"----------

(
  vel_20221214_E3_14ss  <- norm_to_preact_ungrouped %>% 
    filter(measure == 'vel', embryo.id == '20221214_E3_14ss', position == 'S') %>% 
    ggplot(aes(x = timepoint, y = normalised_mean)) +
    annotate("rect", xmin = -5, xmax = 115, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    annotate("text", x = 60, y = 3.5, label = "ON", size = 8) +
    geom_ribbon(aes(ymin = (normalised_mean - normalised_std), ymax = (normalised_mean + normalised_std)), fill = "black", alpha = 0.3) +
    geom_line(size = 1) +
    facet_grid(rows = vars(embryo.id), 
               cols = vars(position)) +
    theme_classic(base_size = 18) +
    theme(text = element_text(size = 28, family = 'Avenir'),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 18),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Relative Velocity")))
)



filename <- 'Fig4_representative_anterior_embryo_norm_velocity.png'
ggsave(filename, plot = vel_20221214_E5_12ss, path = 'results/plots/paper/', width = 8, height = 9)


filename <- 'Fig4_representative_posterior_embryo_norm_velocity.png'
ggsave(filename, plot = vel_20221214_E3_14ss, path = 'results/plots/paper/', width = 8, height = 9)


# preactiovation BASELINE-----
(
  baseline_AP <- averaged_norm_ungrouped %>% 
    filter(measure == 'vel', (direction == 'anterior' & position %in% c('N','S')) | (direction == 'posterior' & position %in% c('N','S')), timepoint < 0) %>% 
    ggplot(aes(x = timepoint, y = normalised_mean, colour = direction)) +
    geom_ribbon(aes(ymin = (normalised_mean - SEM), ymax = (normalised_mean + SEM)), fill = "black", alpha = 0.3) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    geom_line(linewidth = 1) +
    theme_bw() +
    facet_wrap(~position~direction) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          panel.spacing = unit(0.5, 'lines'),
          legend.position = "none") +
    xlab('Time [s]') +
    ylab(expression(paste("Relative to preactivation mean")))
  )

average_before <- averaged_norm_ungrouped %>% 
  filter(timepoint < 0) %>% 
  group_by(measure, position, direction) %>% 
  summarise(mean = mean(normalised_mean),
            sd = stats::sd(mean))

( average_before %>% 
    filter(position %in% c('N', 'S'), measure %in% c('vel', 'ani', 'iso')) %>% 
    ggplot(aes(x = position, y = mean, colour = direction)) +   
    geom_point() +
    facet_wrap(~measure)
  
)

averaged_norm_ungrouped %>%  
  filter(position %in% c('N', 'S'), measure %in% c('vel', 'ani', 'iso')) %>% 
  filter(timepoint > -30, timepoint < 0) %>% 
  mutate(measure = factor(measure, levels = c("vel", "ani", "iso"))) %>% 
  ggplot(aes(x = position, y = normalised_mean, colour = direction)) +   
  geom_point(position = position_jitterdodge()) +
  stat_summary(mapping = aes(group = direction), position = position_dodge(width = 1), fun.data="mean_sdl", fun.args = list(mult=1),geom="errorbar", color="black", width=0.3,  linewidth = 1) +  
  stat_mean(size = 4, position = position_dodge(width = 1)) +
  facet_wrap(~measure) 
 




