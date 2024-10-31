'02
Thresholding embryos based on the z-score of peak velocity and averaging the anterior and posterior groups'
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
library(stats)

# Read in file from code 01 - the data where embryos have been grouped by A/P direction

AP_grouped_data <- read.xlsx('results/spreadsheets/2024-06-20_AP_grouped_data.xlsx')

   
# Z score calculation --------

# Create column with true/false for preactivation

AP_grouped_data = AP_grouped_data %>% 
  mutate(preactivation = case_when(timepoint < 0 ~ TRUE,
                                   timepoint >= 0 ~ FALSE))

# Find the preactivation mean and std for each preprocessing method in each CNSEW position

preact <- AP_grouped_data %>% 
  filter(preactivation, timepoint > -110, timepoint < -10 ) # trim the preactivation time for discrepancies at start or end of movie due to time averaging

preact_sum <- preact %>% 
  filter(measure != c('vang', 'aniang')) %>% 
  group_by(embryo.id, measure, position) %>% ## should position be grouped - eg maybe the mean of pop should be averaged by all position / CNSE - yes i think so
  summarise(std_pop = sqrt(sum(std^2) / n()), 
            std_pop_bio = stats::sd(mean),
            mean_pop = mean(mean)) %>% 
  ungroup() %>% 
  select(embryo.id, measure, position, std_pop, std_pop_bio, mean_pop)

preact_sum_ungrouped <- preact %>% 
  filter(position != 'W', measure != c('vang', 'aniang')) %>% #removing W position from population mean as it is not in tissue
  group_by(embryo.id, measure) %>% 
  summarise(std_pop = sqrt(sum(std^2) / n()), 
            std_pop_bio = stats::sd(mean),
            mean_pop = mean(mean)) %>% 
  ungroup() %>% 
  select(embryo.id, measure, std_pop, std_pop_bio, mean_pop)

# Calculate z-score transform (value - mean / std) and add as column to data frame
# Have only done this for velocity but may be interesting to look at other measures

z_score_df <- AP_grouped_data %>% 
  left_join(preact_sum, by = c('embryo.id', 'measure', 'position'), keep = FALSE)

z_score_df_ungrouped <- AP_grouped_data %>% 
  left_join(preact_sum_ungrouped, by = c('embryo.id', 'measure'), keep = FALSE)


z_score_vel <- z_score_df %>% 
  filter(measure == 'vel') %>% 
  mutate(z_score_vel = (mean - mean_pop)/ std_pop) %>%        #z_score uses average std before activation = variation due to preprocessing and effect of PIV analysis
  mutate(z_score_bio_vel = (mean - mean_pop )/ std_pop_bio) %>%  #z_score_bio uses std of the velocity before activation = biological variation in the velocity in unactivated state
  select(embryo.id, position, timepoint, z_score_vel, z_score_bio_vel)

z_score_vel_ungrouped <- z_score_df_ungrouped %>% 
  filter(measure == 'vel') %>% 
  mutate(z_score_vel = (mean - mean_pop)/ std_pop) %>%        #z_score uses average std before activation = variation due to preprocessing and effect of PIV analysis
  mutate(z_score_bio_vel = (mean - mean_pop )/ std_pop_bio) %>%  #z_score_bio uses std of the velocity before activation = biological variation in the velocity in unactivated state
  select(embryo.id, position, timepoint, z_score_vel, z_score_bio_vel)

z_score_df <- z_score_df %>% 
  left_join(z_score_vel, by = c('embryo.id', 'position', 'timepoint'), keep = FALSE)
z_score_df_ungrouped <- z_score_df_ungrouped %>% 
  left_join(z_score_vel_ungrouped, by = c('embryo.id', 'position', 'timepoint'), keep = FALSE)

# Plot z_score -----
# this part is only necessary to run if wanting to visualise the data - and see which embryos are low / high responders
# z-score shows the difference of the velocity from the preactivation mean
# if over 2 equivalent to p=0.05 significant difference from the mean before activation
# did not use z-score-bio - but maybe should have?
## z score should be calculated by the population mean / sd not the sample mean / sd which i have done here?

(
  z_score_ant <- z_score_df %>% 
    filter(measure == 'vel', direction == 'anterior') %>% 
    ggplot(aes(x = timepoint, y = z_score_vel)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    annotate("text", x = 60, y = 6, label = "ON", size = 2) +
    geom_line() +
    geom_hline(yintercept = 2, linetype = 'dashed', alpha = 0.5, size = 0.2) +
    ylim(-2, 7.5) +
    theme_bw() +
    facet_grid(rows = vars(embryo.id),
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity z-score")))
)

(
  z_score_post <- z_score_df %>% 
    filter(measure == 'vel', direction == 'posterior') %>% 
    ggplot(aes(x = timepoint, y = z_score_vel)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#fbb03b") +
    annotate("text", x = 60, y = 0.02, label = "ON", size = 2) +
    geom_line() +
    geom_hline(yintercept = 2, linetype = 'dashed', alpha = 0.5, size = 0.2) +
    theme_bw() +
    ylim(-2, 7.5) +
    facet_grid(rows = vars(embryo.id),
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity z-score")))
)


filename <- paste(Sys.Date(), 'z-score_velocity', sep = "_")

pdf(sprintf('results/plots/%s.pdf', filename), height = 9, width = 16) #number is inches 4 and 7 convert to 16:9 ratio
z_score_ant + (z_score_post + plot_layout(nrow = 2))
dev.off()

pdf2pptx(sprintf('results/plots/%s.pdf', filename), sprintf('results/%s.pptx', filename), ratio = 169)


# Plot velocity normalised by std during preactivation (SNR)
# this part is only necessary to run if wanting to visualise the data - and see which embryos are low / high responders
# Here looking at the signal is relevant e.g. peak value

(
  SNR <- z_score_df %>% 
    filter(measure == 'vel') %>% 
    ggplot(aes(x = timepoint, y = mean / std_pop)) +
    annotate("rect", xmin = 0, xmax = 125, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "brown") +
    annotate("text", x = 60, y = 0.02, label = "ON", size = 2) +
    geom_ribbon(aes(ymin = ((mean - std) / std_pop), ymax = ((mean + std) / std_pop)), alpha = 0.3) +
    geom_line() +
    theme_bw() +
    facet_grid(rows = vars(direction, embryo.id),
               cols = vars(position)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Time [s]') +
    ylab(expression(paste("Velocity / preactivation std")))
)


# threshold the peak z-score in the activation period >2
# uses z-score not z-score-bio
# uses t=0-55 as duration of the peak velocity

peak_zscore_during_act <- z_score_df %>%
  filter(measure == 'vel', 
         (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S'),
         timepoint >= 0, timepoint <= 55) %>%
  group_by(embryo.id) %>%
  slice_max(order_by = z_score_vel) %>% 
  rename(peak_z_score = z_score_vel,
         peak_tp = timepoint) %>% 
  select(embryo.id, peak_tp, peak_z_score) %>% 
  ungroup()

peak_zscore_during_act_ungrouped <- z_score_df_ungrouped %>%
  filter(measure == 'vel', 
         (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S'),
         timepoint >= 0, timepoint <= 55) %>%
  group_by(embryo.id) %>%
  slice_max(order_by = z_score_vel) %>% 
  rename(peak_z_score = z_score_vel,
         peak_tp = timepoint) %>% 
  select(embryo.id, peak_tp, peak_z_score) %>% 
  ungroup()

z_score_df <- z_score_df %>% 
  left_join(peak_zscore_during_act, by = 'embryo.id')

z_score_df_ungrouped <- z_score_df_ungrouped %>% 
  left_join(peak_zscore_during_act_ungrouped, by = 'embryo.id')

thresholded_data <- z_score_df %>% 
  filter(peak_z_score >= 2) 

thresholded_data_ungrouped <- z_score_df_ungrouped %>% 
  filter(peak_z_score >= 2) 

# to check thresholding 
# this removed 2 embryos
length(unique(z_score_df$embryo.id)) - length(unique(thresholded_data$embryo.id))

# this removed 3 embryos
length(unique(z_score_df_ungrouped$embryo.id)) - length(unique(thresholded_data_ungrouped$embryo.id))

# Print thresholded_data to excel ------

filename <- paste(Sys.Date(), 'thresholded_data', sep = "_")

write_xlsx(thresholded_data, path = sprintf('results/spreadsheets/%s.xlsx', filename))

filename <- paste(Sys.Date(), 'thresholded_data_ungrouped', sep = "_")

write_xlsx(thresholded_data_ungrouped, path = sprintf('results/spreadsheets/%s.xlsx', filename))

# Averaging thresholded embryos --------------------
# Averaged into anterior and posterior by average vang 

averaged_data_1 <- thresholded_data %>% 
  group_by(timepoint, measure, position, direction) %>% 
  filter(!measure %in% c('vang', 'aniang')) %>%  #filtering for non-circular data
  group_by(timepoint, measure, position, direction) %>% 
  summarise(std = stats::sd(mean),
            mean = mean(mean),
            # std = sqrt(sum(std^2) / n()),
            SEM = std / sqrt(length(unique(embryo.id)))) %>% 
  ungroup()


averaged_data_2 <- thresholded_data %>% 
  group_by(timepoint, measure, position, direction) %>% 
  filter(measure %in% c('vang', 'aniang')) %>% #filtering for circular data
  mutate(mean = as.circular(mean, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  summarise(std = sd.circular(mean),
            var = var.circular(mean),
            mean = as.double(mean.circular(mean))) %>%  
  mutate(mean = as.double(mean * (180 / pi)),
         std = as.double(std * (180 / pi)),
         var = as.double(var * (180 / pi))) %>%  # is this also valid to convert the std into degrees?
  ungroup()

averaged_data <- as.data.frame(full_join(averaged_data_1, averaged_data_2)) %>% 
  relocate(SEM, .after=std)

# find the peak vel value ----
peak_vel <- averaged_data %>% 
  filter(measure == 'vel', (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S') ) %>% 
  group_by(direction) %>% 
  slice_max(mean) %>% 
  mutate(mean = mean * 60) %>% 
  select(measure, timepoint, position, direction, mean) %>% 
  ungroup()

print(peak_vel)

# # A tibble: 2 × 5
# measure timepoint position direction  mean
# <chr>       <dbl> <chr>    <chr>     <dbl>
#   1 vel            30 N        anterior  0.471
# 2 vel            20 S        posterior 0.461

# Print averaged_data to excel ------

filename <- paste(Sys.Date(), 'averaged_data', sep = "_")

write_xlsx(averaged_data, path = sprintf('results/spreadsheets/%s.xlsx', filename))

# ungrouped - Averaged into anterior and posterior by average vang - 

averaged_data_1 <- thresholded_data_ungrouped %>% 
  group_by(timepoint, measure, position, direction) %>% 
  filter(!measure %in% c('vang', 'aniang')) %>%  #filtering for non-circular data
  summarise(std = stats::sd(mean),
            mean = mean(mean),
            # std = sqrt(sum(std^2) / n()),
            SEM = std / sqrt(length(unique(embryo.id)))) %>% 
  ungroup()

averaged_data_2 <- thresholded_data_ungrouped %>% 
  group_by(timepoint, measure, position, direction) %>% 
  filter(measure %in% c('vang', 'aniang')) %>% #filtering for circular data
  mutate(mean = as.circular(mean, type = "angles", units = "radians", template = "none", modulo = "asis", zero = 0, rotation = "clock")) %>% 
  summarise(std = sd.circular(mean),
            var = var.circular(mean),
            mean = as.double(mean.circular(mean))) %>%  
  mutate(mean = as.double(mean * (180 / pi)),
         std = as.double(std * (180 / pi)),
         var = as.double(var * (180 / pi))) %>%  # is this also valid to convert the std into degrees?
  ungroup()


averaged_data_ungrouped <- as.data.frame(full_join(averaged_data_1, averaged_data_2)) %>% 
  relocate(SEM, .after=std)

# find the peak vel value ----
peak_vel <- averaged_data_ungrouped %>% 
  filter(measure == 'vel', (direction == 'anterior' & position == 'N') | (direction == 'posterior' & position == 'S') ) %>% 
  group_by(direction) %>% 
  slice_max(mean) %>% 
  mutate(mean = mean * 60) %>% 
  select(measure, timepoint, position, direction, mean) %>% 
  ungroup()

print(peak_vel)

# # A tibble: 2 × 5
# measure timepoint position direction  mean
# <chr>       <dbl> <chr>    <chr>     <dbl>
#   1 vel            25 N        anterior  0.531
# 2 vel            20 S        posterior 0.461

# Print averaged_data to excel ------

filename <- paste(Sys.Date(), 'averaged_data_ungrouped', sep = "_")

write_xlsx(averaged_data_ungrouped, path = sprintf('results/spreadsheets/%s.xlsx', filename))

