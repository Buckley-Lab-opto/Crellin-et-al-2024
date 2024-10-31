library(gsignal)
library(openxlsx)
library(pdf2pptx)
library(officer)
library(rstatix)
library(tidyverse)
library(svglite)


zscore_column <- read.xlsx('/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/results/spreadsheets/column/2024-10-01_zscore_column.xlsx') 
#averaged_zscore <- read.xlsx('/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/results/spreadsheets/column/2024-07-04_averaged_zscore_column.xlsx')  


# fft testing ----
fft_test_during <- averaged_zscore %>% 
  dplyr::filter(timepoint == 55, measure == 'iso', direction == 'anterior') %>% 
  select(distance_aligned, mean) %>% 
  mutate(id = row_number())
fft_test_before <- averaged_zscore %>% 
  dplyr::filter(timepoint == -50, measure == 'iso', direction == 'anterior') %>% 
  select(distance_aligned, mean)

fft_test_during_ <- data.frame(fft(fft_test_during$mean)) %>% 
  mutate(id = row_number())
fft_test_during <- left_join(fft_test_during, fft_test_during_)

fft_test_before_ <- data.frame(fft(fft_test_before$mean) )
fft_test_before <- merge(fft_test_before, fft_test_before_)

res <- as_data_frame(ifft(fft_test_during$fft.fft_test_during.mean.)) %>% 
  mutate(id = row_number())

res %>% 
  ggplot(aes(y = value, x = id)) +
  geom_line() 

fft_test_during %>% 
  ggplot(aes(y = abs(fft.fft_test_during.mean.), x =  id)) +
  geom_line() +
  xlim(0,15) +
  ylim(0,50)

fft_test_before %>% 
  ggplot(aes(y = mean, x =  distance_aligned)) +
  geom_line() 

fft_test_before %>% 
  ggplot(aes(x = abs(fft.fft_test_before.mean.))) +
  geom_histogram(binwidth = 1) +
  xlim(0,15) +
  ylim(0,1300)




# ----


wave_analysis <- zscore_column %>% 
  dplyr::filter(measure == 'iso') %>% 
  group_by(embryo.id, direction, timepoint) %>% 
  arrange(distance_aligned) %>% 
  mutate(id = row_number()) %>% 
  ungroup() %>% 
  select(embryo.id, direction, timepoint, id, distance_aligned, z_score, value, ROI_start_aligned, ROI_end_aligned)

# butterworth filter  ----
fs <- 1 / 4.4 # spatial sample frequency is 1 / 4.4µm
nyquist_frequency <- fs / 2  
low <- 0.01 / nyquist_frequency  # 0.01 is 1/100µm chosen by looking at waveforms and filtered result
high <- 0.05 / nyquist_frequency  # 0.05 is 1/20µm chosen by looking at waveforms and filtered result
bf <- butter(10, c(low, high), type = 'pass')
# see below - sgolayfilt(value, p = 3, n = 9), # p is the polynomial order and n is the filter length - chosen by visualising fit

wave_analysis <- wave_analysis %>%
  group_by(timepoint, embryo.id) %>%
  mutate(sgf_value = sgolayfilt(value, p = 3, n = 9), # p is the polynomial order and n is the filter length
         bf_value = filtfilt(bf, value),
         sgf_zscore = sgolayfilt(z_score, p = 3, n = 9), # p is the polynomial order and n is the filter length
         bf_zscore = filtfilt(bf, z_score)) %>%
  ungroup()

wave_analysis_tidy <- wave_analysis |>
  select(-c('z_score', 'sgf_zscore', 'bf_zscore')) |>
  rename('unadjusted' = value, 'sgf' = sgf_value, 'bf' = bf_value) |>
  pivot_longer(cols = c(unadjusted, sgf, bf), values_to = 'value', names_to = 'type')

wave_analysis_tidy <- wave_analysis_tidy |> left_join(
wave_analysis |>
  select(-c('value', 'sgf_value', 'bf_value')) |>
  rename('unadjusted' = z_score, 'sgf' = sgf_zscore, 'bf' = bf_zscore) |>
  pivot_longer(cols = c(unadjusted, sgf, bf), values_to = 'z_score', names_to = 'type')
)

peak_analysis_tidy <- wave_analysis_tidy |>
  group_by(timepoint, embryo.id, type) |>
  mutate(value_peak = list(distance_aligned[gsignal::findpeaks(value, DoubleSided = TRUE, MinPeakDistance  = 4)[[2]]]),
         z_score_peak = list(distance_aligned[gsignal::findpeaks(z_score, DoubleSided = TRUE, MinPeakDistance = 4)[[2]]])) |>
  mutate(type = factor(type, levels = c('unadjusted', 'sgf', 'bf'))) %>% 
  ungroup()

peak_analysis_tidy |>
  dplyr::filter(timepoint == 30) |>
  unnest(cols = value_peak) |>
  ggplot(aes(x = distance_aligned, y = value, group = type, colour = type)) +
  geom_line() +
  geom_vline(aes(xintercept = value_peak)) +
  facet_grid(type~embryo.id) +
  theme(aspect.ratio = 1)

amp_summary <- peak_analysis_tidy |>
  group_by(embryo.id, timepoint, id) |>
  mutate(value = value[type == 'unadjusted'],
         z_score = z_score[type == 'unadjusted']) |>
  unnest(cols = value_peak) |>
  select(-z_score_peak) %>% 
  dplyr::filter(round(distance_aligned, digits = 3) == round(value_peak, digits = 3)) |>
  group_by(embryo.id, timepoint, type) |>
  arrange(distance_aligned, .by_group = TRUE) |>
  mutate(abs_amplitude_value = abs((value - lag(value) / 2)),
         amplitude_value =(value - lag(value) / 2),
         abs_amplitude_zscore = abs((z_score - lag(z_score) / 2)),
         amplitude_zscore = (z_score - lag(z_score) / 2)) |>
  drop_na() %>% 
  ungroup()

wl_summary <- peak_analysis_tidy |>
  group_by(embryo.id, timepoint, id) |>
  mutate(value = value[type == 'unadjusted'],
         z_score = z_score[type == 'unadjusted']) |>
  unnest(cols = value_peak) |>
  select(-z_score_peak) %>% 
  dplyr::filter(round(distance_aligned, digits = 3) == round(value_peak, digits = 3)) |>
  group_by(embryo.id, timepoint, type) |>
  arrange(distance_aligned, .by_group = TRUE) |>
  mutate(wavelength = distance_aligned - lag(distance_aligned, n = 2)) |>
  drop_na() %>% 
  ungroup

wave_summary <- amp_summary %>% 
  left_join(wl_summary)

# calculated phase wrong
# phase <- peak_analysis_tidy %>% 
#   group_by(embryo.id) %>% 
#   dplyr::filter(between(distance_aligned, ROI_start_aligned+1, ROI_end_aligned-1)) %>% 
#   group_by(embryo.id, timepoint, id) |>
#   mutate(value = value[type == 'unadjusted'],
#          z_score = z_score[type == 'unadjusted']) |>
#   unnest(cols = value_peak) |>
#   select(-z_score_peak) %>% 
#   dplyr::filter(round(distance_aligned, digits = 3) == round(value_peak, digits = 3)) |>
#   group_by(embryo.id, timepoint, type) |> 
#   arrange(distance_aligned, .by_group = TRUE) %>% 
#   mutate(phase = tan(z_score / (id/10)) * 180/pi,
#          phase = circular::as.circular(phase, type = 'angles', units = 'degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')) |>
#   mutate(period.subset = case_when(timepoint >= -55 & timepoint <= 0 ~ 'before',
#                                    timepoint >= 0 & timepoint <= 55 ~ 'during',
#                                    timepoint >= 125 & timepoint <= 180 ~ 'after')) %>% 
#   mutate(period.subset = factor(period.subset, levels = c('before', 'during', 'after'))) %>% 
#   drop_na() %>% 
#   ungroup()
  
phase_difference <- peak_analysis_tidy %>% 
  group_by(embryo.id) %>% 
  dplyr::filter(between(distance_aligned, ROI_start_aligned+1, ROI_end_aligned-1)) %>% 
  group_by(embryo.id, timepoint, id) |>
  mutate(value = value[type == 'unadjusted'],
         z_score = z_score[type == 'unadjusted']) |>
  unnest(cols = value_peak) |>
  select(-z_score_peak) %>% 
  dplyr::filter(round(distance_aligned, digits = 3) == round(value_peak, digits = 3)) |>
  group_by(embryo.id, timepoint, type) |> 
  arrange(distance_aligned, .by_group = TRUE) %>% 
  mutate(wavelength = distance_aligned - lag(distance_aligned, n = 2)) |>
  slice_max(z_score) %>% 
  ungroup(timepoint) %>% 
  mutate(phase_difference = (value_peak[timepoint == 0] - value_peak) / wavelength,
         amp_difference = z_score[timepoint == 0] - z_score) %>% 
  mutate(period.subset = case_when(timepoint >= -85 & timepoint < 30 ~ 'before',
                                   timepoint > 0 & timepoint <= 55 ~ 'during',
                                   timepoint >= 125 & timepoint < 180 ~ 'after')) %>% 
  mutate(period.subset = factor(period.subset, levels = c('before', 'during', 'after'))) %>% 
  drop_na() %>% 
  ungroup()

phase_difference <- peak_analysis_tidy %>% 
  group_by(embryo.id, timepoint, id) |>
  mutate(value = value[type == 'unadjusted'],
         z_score = z_score[type == 'unadjusted']) |>
  unnest(cols = value_peak) |>
  select(-z_score_peak) %>% 
  dplyr::filter(round(distance_aligned, digits = 3) == round(value_peak, digits = 3)) |>
  group_by(embryo.id, timepoint, type) |> 
  arrange(distance_aligned, .by_group = TRUE) %>% 
  mutate(wavelength = distance_aligned - lag(distance_aligned, n = 2)) |>
  group_by(embryo.id) %>% 
  dplyr::filter(between(distance_aligned, ROI_start_aligned+1, ROI_end_aligned-1)) %>% 
  group_by(embryo.id, timepoint, type) |> 
  slice_max(z_score) %>% 
  ungroup(timepoint) %>% 
  mutate(phase_difference = ((value_peak[timepoint == 0] - value_peak)), # / wavelength),
         phase_lag = (value_peak - lag(value_peak)), # / wavelength,
         amp_difference = z_score[timepoint == 0] - z_score) %>% 
  mutate(period.subset = case_when(timepoint >= -55 & timepoint < 0 ~ 'before',
                                   timepoint > 0 & timepoint <= 55 ~ 'during',
                                   timepoint >= 125 & timepoint < 180 ~ 'after')) %>% 
  mutate(period.subset = factor(period.subset, levels = c('before', 'during', 'after'))) %>% 
  drop_na() %>% 
  ungroup()

phase_difference |>
 # mutate(phase_difference = abs(phase_difference)) %>% 
  dplyr::filter(type == 'sgf') %>% 
  ggplot(aes(x = period.subset, y = phase_lag, colour = period.subset)) +
geom_point() +
  facet_wrap(~embryo.id) +
  theme(aspect.ratio = 1)


  
  

# wave_summary |>
#   filter(timepoint == -20) |>
#   ggplot(aes(x = distance_aligned, y = wavelength, group = type, colour = type)) +
#   geom_point() +
#   facet_grid(type~embryo.id) +
#   theme(aspect.ratio = 1)
# 
# wave_summary |>
#   group_by(type, embryo.id, timepoint) |>
#   summarise(mu = mean(amplitude),
#             std = sd(amplitude)) |>
#   group_by(type) |>
#   summarise(average_std = median(std))
# 
# wave_summary |>
#   group_by(type, embryo.id, timepoint) |>
#   summarise(mu = mean(wavelength),
#             std = sd(wavelength)) |>
#   group_by(type) |>
#   summarise(average_std = median(std))

region_summary <- wave_summary |>
  mutate(period.subset = case_when(timepoint >= -85 & timepoint <= -30 ~ 'before',
                                   timepoint >= 0 & timepoint <= 55 ~ 'during',
                                   timepoint >= 125 & timepoint <= 180 ~ 'after')) %>% 
  mutate(period.subset = factor(period.subset, levels = c('before', 'during', 'after'))) %>% 
  drop_na() |>
  group_by(type, period.subset, embryo.id, direction) |>
  summarise(amplitude_value = mean(amplitude_value),
            abs_amplitude_value = mean(abs_amplitude_value),
            amplitude_zscore = mean(amplitude_zscore),
            abs_amplitude_zscore = mean(abs_amplitude_zscore),
            wavelength = mean(wavelength, trim = 0.1)) |>
  pivot_longer(cols = c('amplitude_value', 'abs_amplitude_value', 'amplitude_zscore', 'abs_amplitude_zscore', 'wavelength'), names_to = 'stat', values_to = 'value') |>
  ungroup()
 

# this is wrong
# phase_summary <- phase %>% 
#   drop_na() |>
#   group_by(type, period.subset, embryo.id, direction) |>
#   summarise(std = circular::sd.circular(phase),
#             phase = circular::mean.circular(phase)) %>% 
#   ungroup() %>% 
#   group_by(type)
# 
# phase_AP_summary <- phase %>% 
#   drop_na() |>
#   group_by(type, period.subset, direction) |>
#   summarise(std = circular::sd.circular(phase),
#             phase = circular::mean.circular(phase)
#             ) %>% 
#   ungroup() %>% 
#   group_by(type)

phase_difference_summary <- phase_difference %>% 
  group_by(type, period.subset, embryo.id, direction) |>
  summarise(std = sd(phase_difference),
            phase_difference = mean(phase_difference),
            amp_std = sd(amp_difference),
            amp_difference = mean(amp_difference),
            std_lag = sd(phase_lag),
            phase_lag = mean(phase_lag),
  ) %>% 
  ungroup() %>% 
  group_by(type)
  

# region_summary |>
#   ggplot(aes(x = period.subset, y = value, fill = period.subset)) +
#   geom_boxplot() +
#   theme_bw() +
#   theme(aspect.ratio = 1, legend.position = 'none') +
#   facet_grid(stat~type, scales = 'free_y') 
# 
# region_summary |>
#   ggplot(aes(x = period.subset, y = value, group = embryo.id, colour = embryo.id)) +
#   geom_line() +
#   theme_bw() +
#   theme(aspect.ratio = 1) +
#   facet_grid(stat~type, scales = 'free_y') 
  

# normally distributed = yes
region_summary %>% 
  group_by(type, stat, period.subset) %>% 
  shapiro_test(value) %>% 
  add_significance() 

# anova ----

aov_data_ampv <- region_summary %>%
  group_by(type) %>%   
  dplyr::filter(stat == 'abs_amplitude_value') # %>% mutate(value = value * 1000 ) %>% get_summary_stats()

aov_data_ampz <- region_summary %>%
  group_by(type) %>%   
  dplyr::filter(stat == 'abs_amplitude_zscore') # %>% get_summary_stats()

aov_data_wl <- region_summary %>%
  group_by(type) %>%   
  dplyr::filter(stat == 'wavelength') # %>% get_summary_stats()

aov_data_wl %>% 
  dplyr::filter(type == 'sgf') %>%
  #mutate(value = value * 60) %>% 
  pivot_wider(names_from = period.subset, values_from = value) %>% 
  summary()

aov_ampv <- aov_data_ampv %>% 
  anova_test(dv = value, wid = embryo.id, within = period.subset)
get_anova_table(aov_ampv)
aov_ampz <- aov_data_ampz %>% 
  anova_test(dv = value, wid = embryo.id, within = period.subset)
get_anova_table(aov_ampz)
aov_wl <- aov_data_wl %>% 
  anova_test(dv = value, wid = embryo.id, within = period.subset)
get_anova_table(aov_wl) 

# aov_data_ampv %>%
#   dplyr::filter(type == 'sgf') %>%
#   pairwise_t_test(
#     value ~ period.subset,
#     paired = TRUE,
#     p.adjust.method = "bonferroni"
#   )

region_summary |>
  dplyr::filter(type == 'unadjusted', period.subset == 'before') %>% 
  dplyr::filter(stat == 'amplitude_zscore') %>%
  #mutate(value = value * 60 * 1000 )  %>% 
 # group_by(direction) %>% get_summary_stats()
  t_test(value ~ direction)

wave_summary |>
  mutate(period.subset = case_when(timepoint >= -85 & timepoint <= -30 ~ 'before',
                                   timepoint >= 0 & timepoint <= 55 ~ 'during',
                                   timepoint >= 125 & timepoint <= 180 ~ 'after')) %>% 
  mutate(period.subset = factor(period.subset, levels = c('before', 'during', 'after'))) %>% 
  drop_na() |>
  pivot_longer(cols = c('amplitude_value', 'abs_amplitude_value', 'amplitude_zscore', 'abs_amplitude_zscore', 'wavelength'), names_to = 'stat', values_to = 'value') |>
  dplyr::filter(type == 'unadjusted', period.subset == 'before') %>% 
  dplyr::filter(stat == 'amplitude_value') %>%
  #mutate(value = value * 60 * 1000 )  %>% 
  # group_by(direction) %>% get_summary_stats()
  t_test(value ~ direction)

# plot ----

region_summary |>
  ggplot(aes(x = period.subset, y = value, fill = period.subset)) +
  geom_boxplot() +
  geom_point() +
  theme_bw() +
  theme(aspect.ratio = 1, legend.position = 'none') +
  facet_grid(stat~type, scales = 'free_y') 

region_summary |>
  dplyr::filter(type == 'unadjusted', period.subset == 'before') %>% 
  ggplot(aes(x = direction, y = value)) +
  geom_boxplot() +
  geom_point() +
  theme_bw() +
  theme(aspect.ratio = 1, legend.position = 'none') +
  facet_grid(stat~type, scales = 'free_y') 

plotting_angles <- seq(180, -180, by = -45) %>% 
  as.data.frame()

#phase plots -----

# phase_summary %>% 
#   dplyr::filter(type == 'sgf') %>% 
#   ggplot(aes(x = period.subset, y = phase, fill = period.subset)) +
#   geom_boxplot() +
#   geom_jitter(height = 0, width = 0.2) +
#   scale_fill_manual(values = c("before" = "grey50",
#                                  "during" = "#fbb03b",
#                                  "after" = "#4296DE")) +
#   coord_flip() +
#   
#   theme_classic() +
#   theme(aspect.ratio = 1, legend.position = 'none') +
#   facet_wrap(~direction, scales = 'free_y')

phase_difference %>% 
  dplyr::filter(type == 'sgf') %>% 
  ggplot(aes(x = period.subset, y = phase_difference, fill = period.subset)) +
  geom_boxplot() +
  geom_jitter(height = 0, width = 0.2, aes(colour = embryo.id)) +
  scale_fill_manual(values = c("before" = "grey50",
                               "during" = "#fbb03b",
                               "after" = "#4296DE")) +
  coord_flip() +
  theme_classic() +
  theme(aspect.ratio = 1, legend.position = 'none') +
  facet_wrap(~direction, scales = 'free_y')

(phase_plot <- phase_difference_summary %>% 
  dplyr::filter(type == 'sgf') %>% 
  ggplot(aes(x = period.subset, y = phase_difference, fill = period.subset)) +
  geom_boxplot() +
  geom_jitter(height = 0, width = 0.2) +
  scale_fill_manual(values = c("before" = "grey50",
                               "during" = "#fbb03b",
                               "after" = "#4296DE")) +
  theme_classic() +
  theme(aspect.ratio = 2,
        legend.position="none", 
        text = element_text(size = 18, family = 'Avenir'),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18),
        panel.spacing = unit(1, 'lines')) +
  facet_wrap(~direction, scales = 'free_y') +
  ylab('Phase difference') +
  xlab(""))

filename <- paste0('results/plots/paper/', Sys.Date(), '_phase_plot.svg')
svglite(filename, width = 6, height = 5)
phase_plot
dev.off()

# 
# (iso_wave_phase <- 
#     phase_summary %>% 
#     dplyr::filter(type == 'sgf') %>% 
#     ggplot() +
#     geom_segment(data = phase_summary %>% 
#                    dplyr::filter(type == 'sgf'),
#                  aes(x = phase, y = 0, xend = phase, yend = 0.8, colour = period.subset), alpha = 0.6, size = 0.5, arrow = arrow(length = unit(0.25, "cm"),  type = "open")) +
#     geom_segment(data = phase_AP_summary %>% 
#                    dplyr::filter(type == 'sgf'),
#                  aes(x = phase, y = 0, xend = phase, yend = 1, colour = period.subset), size = 1, arrow = arrow(length = unit(0.25, "cm"),  type = "closed")) +
#     geom_point(data = plotting_angles, aes(x = ., y = 0.9), alpha = 0) +
#     coord_polar(theta = "x", start = 1.57, direction = -1) +  
#     scale_x_continuous(breaks = seq(180, -179, -45)) +
#     scale_colour_manual(values = c("before" = "grey30",
#                                    "during" = "#fbb03b",
#                                    "after" = "#4296DE")) +
#     theme_minimal() +
#     facet_grid(direction~type) +
#     theme(text = element_text(size = 22, family = 'Avenir'),
#           axis.title = element_blank(),
#           axis.text.y = element_blank(),
#           axis.ticks = element_blank(), 
#           panel.grid.major = element_line(size = 1.5, colour = 'grey85'),
#           panel.grid.minor = element_blank()
#     ) 
# )


# amp and wavelength plots ------


(iso_wave_ampv <- region_summary %>% 
    dplyr::filter(stat == 'abs_amplitude_value', type == 'sgf') %>% 
    #mutate(value = value * 60) %>% 
    ggplot(aes(x = period.subset, y = value)) +
   stat_boxplot(geom ='errorbar', width = 0.25) +
   geom_boxplot(outlier.shape=NA, size = 0.5, position = position_dodge(0.5), width = 0.5) +
   geom_jitter(aes(colour = factor(period.subset)), size = 2,  alpha = 0.6, width = 0.25, show.legend = F, height = 0) +
   scale_colour_manual(values = c("before" = "grey30",
                                  "during" = "#fbb03b",
                                  "after" = "#4296DE")) +
   # scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)))+
   theme_classic(base_size = 18) + 
    labs(x = '', y = expression(paste("Amplitude (", s^{-1}, ')'))) +
   theme(aspect.ratio = 1.5,
         text = element_text(size = 18, family = 'Avenir'),
         strip.background = element_blank(),
         strip.text.x = element_text(size = 18),
         strip.text.y = element_text(size = 18),
         panel.spacing = unit(0.5, 'lines')) 
)

(iso_wave_ampz <- region_summary %>% 
    dplyr::filter(stat == 'abs_amplitude_zscore', type == 'sgf') %>% 
    ggplot(aes(x = period.subset, y = value)) +
    stat_boxplot(geom ='errorbar', width = 0.25) +
    geom_boxplot(outlier.shape=NA, size = 0.5, position = position_dodge(0.5), width = 0.5) +
    geom_jitter(aes(colour = factor(period.subset)), size = 2,  alpha = 0.7, width = 0.25, show.legend = F, height = 0) +
    scale_colour_manual(values = c("before" = "grey30",
                                   "during" = "#fbb03b",
                                   "after" = "#4296DE")) +
    scale_y_continuous(expand = expansion(mult = c(0.2, 0.2)))+
    theme_classic(base_size = 18) + 
    labs(x = '', y = "Amplitude z-score") +
    theme(aspect.ratio = 1.5,
          text = element_text(size = 18, family = 'Avenir'),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 18),
          panel.spacing = unit(0.5, 'lines')) 
)

(iso_wave_wavelength <- region_summary %>% 
    dplyr::filter(stat == 'wavelength', type == 'sgf') %>% 
    ggplot(aes(x = period.subset, y = value)) +
    scale_y_continuous(limits = c(30, 70)) +
    stat_boxplot(geom ='errorbar', width = 0.25) +
    annotate("text", x = 3, y = 60.6, label = "Rhombomere", size = 5, family = 'Avenir', colour = 'darkgreen') +
    annotate("text", x = 3, y = 33, label = "ROI", size = 5, family = 'Avenir') +
    annotate("rect", xmin = 0.5, xmax = 3.5, ymin = 53.5, ymax = 70, alpha = .2, fill = 'green') +
    annotate("rect", xmin = 0.5, xmax = 3.5, ymin = 30.8, ymax = 44, alpha = .2) +
    geom_boxplot(size = 0.5, position = position_dodge(0.5), width = 0.5) +
    geom_jitter(aes(colour = factor(period.subset)), size = 2,  alpha = 0.7, width = 0.25, show.legend = F, height = 0) +
    scale_colour_manual(values = c("before" = "grey30",
                                   "during" = "#fbb03b",
                                   "after" = "#4296DE")) +
    theme_classic(base_size = 18) + 
    labs(x = '', y = "Wavelength (µm)") +
    theme(aspect.ratio = 1.5,
          text = element_text(size = 18, family = 'Avenir'),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 18),
          panel.spacing = unit(0.5, 'lines')) 
)





filename <- paste0('results/plots/paper/', Sys.Date(), '_iso_wave_amp.svg')
svglite(filename, width = 4, height = 5)
iso_wave_ampv
dev.off()

filename <- paste0('results/plots/paper/', Sys.Date(), '_iso_wave_amp_zscore.svg')
svglite(filename, width = 4, height = 5)
iso_wave_ampz
dev.off()

filename <- paste0('results/plots/paper/', Sys.Date(), '_iso_wave_wavelength.svg')
svglite(filename, width = 4, height = 5)
iso_wave_wavelength
dev.off()

# old code -------


wave_analysis_ <- wave_analysis %>%
  left_join(sgf_peaks, by = c("id", "timepoint", "embryo.id")) %>%
  left_join(bf_peaks, by = c("id", "timepoint", "embryo.id")) %>% 
  mutate(sgf_peaks = if_else(is.na(sgf_peaks), FALSE, TRUE),
         bf_peaks = if_else(is.na(bf_peaks), FALSE, TRUE))

# comparing filtered result
sgf_peaks_full <- wave_analysis %>%
  left_join(sgf_peaks) |>
  pivot_longer(cols = c(value, sgf_value, bf_value), values_to = 'value', names_to = 'value_type')

sgf_peaks_full |>
  dplyr::filter(timepoint == -5) %>%
  ggplot(aes(x = distance_aligned, y = value, group = value_type, colour = value_type)) +
  geom_line() +
  # geom_line(aes(x = distance_aligned, y = sgf_value), colour = 'blue') +
  # geom_line(aes(x = distance_aligned, y = bf_value), colour = 'red') +
  geom_vline(data = sgf_peaks_full |> drop_na(sgf_peaks) |> dplyr::filter(timepoint == -5) , mapping = aes(xintercept = distance_aligned)) +
  facet_grid(value_type~embryo.id, scales = "free_y") +
  theme(aspect.ratio = 1)


wave_analysis_ <- wave_analysis_ %>%
  group_by(timepoint, embryo.id) %>%
  arrange(distance_aligned) %>%
  dplyr::filter(sgf_peaks == T | bf_peaks == T) %>% 
  mutate(sgf_amplitude = if_else(sgf_peaks == T & id > 1, (abs(sgf_value) - lag(abs(sgf_value)))/2, NA_real_),
         bf_amplitude = if_else(bf_peaks == T & id > 1, (abs(bf_value) - lag(abs(bf_value)))/2, NA_real_),
         sgf_zscore_amplitude = if_else(sgf_peaks == T & id > 1, (abs(sgf_zscore) - lag(abs(sgf_zscore)))/2, NA_real_),
         bf_zscore_amplitude = if_else(bf_peaks == T & id > 1, (abs(bf_zscore) - lag(abs(bf_zscore)))/2, NA_real_)) %>%
  ungroup()

mean_amplitude <- wave_analysis_ %>%
  mutate(period.subset = case_when(timepoint >= -85 & timepoint <= -30 ~ 'before',
                                   timepoint >= 0 & timepoint <= 55 ~ 'during',
                                   timepoint >= 125 & timepoint <= 180 ~ 'after')) %>% 
  mutate(period.subset = factor(period.subset, levels = c('before', 'during', 'after'))) %>% 
  group_by(period.subset) %>%
  summarize(mean_sgf_amplitude = mean(sgf_amplitude, na.rm = TRUE),
            mean_bf_amplitude = mean(bf_amplitude, na.rm = TRUE),
            mean_sgf_zscore_amplitude = mean(sgf_zscore_amplitude, na.rm = TRUE),
            mean_bf_zscore_amplitude = mean(bf_zscore_amplitude, na.rm = TRUE)
  ) %>%
  ungroup()

mean_amplitude %>% 
  ggplot(aes(y = mean_sgf_amplitude, x = period.subset)) +
  geom_boxplot() +
  geom_boxplot(aes(y = mean_bf_amplitude, x = period.subset), colour = 'blue')

mean_amplitude %>% 
  ggplot() +
  geom_boxplot(aes(y = mean_sgf_zscore_amplitude, x = period.subset), colour = 'red') +
  geom_boxplot(aes(y = mean_bf_zscore_amplitude, x = period.subset), colour = 'purple') 


wave_analysis_ <- wave_analysis_ %>%
  group_by(timepoint, embryo.id) %>%
  arrange(distance_aligned) %>%
  dplyr::filter(sgf_peaks == T | bf_peaks == T) %>% 
  dplyr::filter(value > 0) %>% 
  mutate(
    sgf_wavelength = ifelse(sgf_peaks == T & id > 1, (distance_aligned-lead(distance_aligned)), NA),
    bf_wavelength = ifelse(bf_peaks == T & id > 1, (distance_aligned-lead(distance_aligned)), NA)) %>%
  ungroup()

mean_wavelength <- wave_analysis_ %>%
  mutate(period.subset = case_when(timepoint >= -85 & timepoint <= -30 ~ 'before',
                                   timepoint >= 0 & timepoint <= 55 ~ 'during',
                                   timepoint >= 125 & timepoint <= 180 ~ 'after')) %>% 
  mutate(period.subset = factor(period.subset, levels = c('before', 'during', 'after'))) %>% 
  group_by(period.subset) %>%
  summarize(
    mean_sgf_wavelength = mean(sgf_wavelength, na.rm = TRUE),
    mean_bf_wavelength = mean(bf_wavelength, na.rm = TRUE)
  ) %>%
  ungroup()

mean_wavelength <- wave_analysis_ %>%
  mutate(period.subset = case_when(timepoint >= -85 & timepoint <= -30 ~ 'before',
                                   timepoint >= 0 & timepoint <= 55 ~ 'during',
                                   timepoint >= 125 & timepoint <= 180 ~ 'after')) %>% 
  mutate(period.subset = factor(period.subset, levels = c('before', 'during', 'after'))) %>% 
  group_by(period.subset)

mean_wavelength %>% 
  ggplot(aes(y = sgf_wavelength, x = period.subset)) +
  
  geom_boxplot(aes(y = bf_wavelength, x = period.subset), colour = 'blue')+
geom_boxplot() 

