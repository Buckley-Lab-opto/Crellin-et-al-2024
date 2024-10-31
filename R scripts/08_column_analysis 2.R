library(tidyverse)
library(openxlsx)
library(viridis)
library(patchwork)
library(pdf2pptx)
library(officer)

# Set thpdf2pptx# Set the directory where your files are located
file_directory <- "/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/data/column analysis/2024_Feb_15"

# Get a list of file names in the directory
file_list <- list.files(file_directory, pattern = ".xlsx", full.names = TRUE)
data_list <- list()
i <- 1
# Create a function to read and process each file
for (file in file_list){
  data <- read.xlsx(file)
  file_name <- str_extract(file, '[^/]+$')
  date_id <- str_extract(file_name, '\\d{8}')
  date_id_2 <- str_extract(file_name, '\\d{4}-\\d{2}-\\d{2}')
  embryo_id <- str_extract(file_name, '[Ee]\\d')
  somite_id <- str_extract(file_name, '\\d+ss')
  measure <- strsplit(file_name, '_') %>% 
    unlist() %>% 
    tail(1) %>% 
    str_remove('\\.xlsx$') 
  data <- as.data.frame(data) %>%
    mutate(date = date_id,
           date_2 = date_id_2,
           embryo = embryo_id,
           somite = somite_id,
           embryo.id = date,
           measure = measure,
           row.id = (1:n()))
  data_list[[i]] <- data
  i <- i+1
}
i <- 1

data <- bind_rows(data_list) %>% as_tibble()

column_data <- data %>% 
  mutate(date_2 = format(as.Date(date_2), "%Y%m%d")) %>% 
  mutate(date = coalesce(date, date_2)) %>% 
  select(-'date_2') %>% 
  pivot_longer(cols = c(-'date', -'embryo', -'somite', -'embryo.id', -'measure', -'row.id', -'ROI', -'POSITION'), 
               names_to = 'timepoint', 
               values_to = 'value') %>% 
  rename(distance = POSITION) %>% 
  mutate(timepoint = as.numeric(str_remove(timepoint, 't'))) %>% 
  mutate(timepoint = (timepoint - 26 ) * 5) %>% # Preact = -125 to -5 , Act = 0 - 120 , Deact = 125 - 240??
  unite(col = 'embryo.id', c('date', 'embryo', 'somite'), remove = FALSE) 

# Annotate with anterior or posterior from thresholded_data and remove embryos below threshold

thresholded_data <- read.xlsx('results/spreadsheets/2024-02-20_thresholded_data.xlsx') %>% 
  select(embryo.id, direction) %>% 
  distinct(embryo.id, direction)

peak_position <- read.xlsx('results/spreadsheets/2024-02-24_peak_vel.xlsx') 

column_data <- column_data %>% 
  left_join(thresholded_data, by = 'embryo.id') %>% 
  filter(!is.na(direction)) %>% 
  left_join(peak_position, by = 'embryo.id')

# Aligning the data by ROI --------


ROI_position <- column_data %>%
  group_by(embryo.id) %>%
  filter(ROI == 1) %>%
  summarise(ROI_start = first(distance),
            ROI_end = last(distance)) %>%
  ungroup()

aligned_column <- column_data %>%
  left_join(ROI_position, by = "embryo.id") %>%
  mutate(distance_aligned = case_when(direction == "anterior" ~ distance - ROI_start,
                                      direction == "posterior" ~ distance - ROI_end),
         ROI_start_aligned = case_when(direction == "anterior" ~ ROI_start - ROI_start,
                              direction == "posterior" ~ ROI_end - ROI_start),
         ROI_end_aligned = case_when(direction == "anterior" ~ ROI_end - ROI_start,
                             direction == "posterior" ~ ROI_end - ROI_end)) %>% 
  mutate(distance_aligned = round(distance_aligned, digits = 2))


# Create a threshold during preactivation to measure total force propagation -------

preact_threshold <- aligned_column %>% # average measure of all t=before and all y
  filter(timepoint < 0, distance > 3, distance < (max(distance) - 3)) %>% # trim edges of distance
  group_by(embryo.id, measure) %>% 
  summarise(threshold = mean(value),
            threshold_std = sd(value)) %>% 
  ungroup() 


peak_value <- aligned_column %>% # average measure of all t=before and all y
  filter(timepoint >= 0, timepoint <=120, distance > 3, distance < (max(distance) - 3)) %>% # trim edges of distance
  select(embryo.id, measure, distance, timepoint, value) %>% 
  group_by(embryo.id, measure) %>% 
  slice_max(order_by = value) %>% 
  mutate(peak_value = value,
         peak_distance = distance,
         peak_timepoint = timepoint) %>% 
  select(-value, -distance, -timepoint) %>% 
  ungroup() 

normalised_column_1 <- aligned_column %>% # normalisation to preact mean (norm to min)
  left_join(preact_threshold, by = join_by(embryo.id, measure)) %>% 
  left_join(peak_value, by = join_by(embryo.id, measure)) %>% 
  group_by(embryo.id) %>% 
  mutate(norm_value = (value / threshold)) %>% 
  drop_na(norm_value) %>% 
  ungroup()

normalised_column_2 <- aligned_column %>% # min/max normalisation 0 = preactivation, 1 = max
  left_join(preact_threshold, by = join_by(embryo.id, measure)) %>% 
  left_join(peak_value, by = join_by(embryo.id, measure)) %>% 
  group_by(embryo.id) %>% 
  mutate(norm_value = (value - threshold) / (peak_value - threshold)) %>% 
  drop_na(norm_value) %>% 
  ungroup()

zscore_column <- aligned_column %>% # normalisation to preact mean (norm to min)
  left_join(preact_threshold, by = join_by(embryo.id, measure)) %>% 
  left_join(peak_value, by = join_by(embryo.id, measure)) %>% 
  group_by(embryo.id) %>%
  mutate(z_score = (value - threshold) / threshold_std ) %>% 
  drop_na(z_score) %>% 
  ungroup()

normalised_column_1 %>% 
  filter(direction == 'anterior', measure == 'iso', timepoint == peak_timepoint[measure == 'vel']) %>% #, timepoint >= 120, timepoint <= 175
  ggplot(aes(x = distance_aligned, y = norm_value, colour = embryo.id)) +
  geom_rect(aes(xmin = 0, xmax = (ROI_end), ymin = -Inf, ymax = Inf), alpha = 0.5, fill = 'pink', colour = 'pink') +
  geom_line(linewidth = 1) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Distance') +
  ylab(expression(paste("Normalised Velocity")))

normalised_column_2 %>% 
  filter(direction == 'anterior', measure == 'iso', timepoint == peak_timepoint[measure == 'vel']) %>% #, timepoint >= 120, timepoint <= 175
  ggplot(aes(x = distance_aligned, y = norm_value, colour = embryo.id)) +
  geom_rect(aes(xmin = 0, xmax = (ROI_end), ymin = -Inf, ymax = Inf), alpha = 0.5, fill = 'pink', colour = 'pink') +
  geom_line(linewidth = 1) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Distance') +
  ylab(expression(paste("Normalised Velocity")))

zscore_column %>% 
  filter(direction == 'anterior', measure == 'iso', timepoint > 10, timepoint <=30) %>% #, timepoint >= 120, timepoint <= 175
  ggplot(aes(x = distance_aligned, y = z_score, colour = embryo.id, group = timepoint)) +
  geom_rect(aes(xmin = 0, xmax = (ROI_end), ymin = -Inf, ymax = Inf), alpha = 0.5, fill = 'pink', colour = 'pink') +
  geom_line(linewidth = 1) +
  theme_bw() + facet_wrap(~embryo.id)+ 
  
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Distance') +
  ylab(expression(paste("Normalised Velocity")))

aligned_column %>% 
  filter(direction == 'anterior', measure == 'iso', timepoint > 10, timepoint <=30) %>% #, timepoint >= 120, timepoint <= 175
  ggplot(aes(x = distance_aligned, y = value, colour = embryo.id, group = timepoint)) +
  geom_rect(aes(xmin = 0, xmax = (ROI_end), ymin = -Inf, ymax = Inf), alpha = 0.5, fill = 'pink', colour = 'pink') +
  geom_line(linewidth = 1) +
  theme_bw() +
  facet_wrap(~embryo.id)+
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Distance') +
  ylab(expression(paste("Normalised Velocity")))


# plotting individual embryos mar/apr 2024 -----

zscore_column <- zscore_column %>% 
  mutate(period.subset = case_when(timepoint >= -60 & timepoint < 0 ~ 'before',
                            timepoint >= 0 & timepoint <= 60 ~ 'during',
                            timepoint > 120 & timepoint <= 180 ~ 'after')) %>% 
  mutate(period.subset = factor(period.subset, levels = c('before', 'during', 'after')))


measures <- c('vel', 'iso', 'ani')
periods <- c('before', 'during', 'after')

y_limits <- list(
  vel = c(-2, 12),  
  iso = c(-6, 6),  
  ani = c(-2, 6)   
)

for (i in measures) {
  y_lim <- y_limits[[i]]
  
  for (j in periods) {
   
      filtered_data <- zscore_column %>% 
        filter(direction == 'anterior', measure == i, period.subset == j)
  
      plot <- filtered_data %>% 
        ggplot(aes(x = distance_aligned, y = z_score, colour = timepoint, group = timepoint)) +
        geom_line(linewidth = 1, alpha = 0.5) +
        geom_vline(xintercept = 0) +
        scale_colour_gradient(low = "red", high = "blue", na.value = NA) +
        theme_bw() + 
        facet_wrap(~embryo.id) +
        theme(strip.background = element_blank(),
              strip.text.x = element_text(size = 8),
              panel.spacing = unit(0.5, 'lines')) +
        xlab('Distance') +
        ylab(expression(paste("zscore"))) +
        ylim(y_lim[1], y_lim[2]) 

      filename <- paste0(i, "_col_zscore_ant_", j, ".png")
      ggsave(filename, plot = plot, path = 'results/plots/colanal/', width = 12, height = 8)
    
  }
}

for (i in measures) {
  y_lim <- y_limits[[i]]
  
  for (j in periods) {
    
    filtered_data <- zscore_column %>% 
      filter(direction == 'posterior', measure == i, period.subset == j)
    
    plot <- filtered_data %>% 
      ggplot(aes(x = distance_aligned, y = z_score, colour = timepoint, group = timepoint)) +
      geom_line(linewidth = 1, alpha = 0.5) +
      geom_vline(xintercept = 0) +
      scale_colour_gradient(low = "red", high = "blue", na.value = NA) +
      theme_bw() + 
      facet_wrap(~embryo.id) +
      theme(strip.background = element_blank(),
            strip.text.x = element_text(size = 8),
            panel.spacing = unit(0.5, 'lines')) +
      xlab('Distance') +
      ylab(expression(paste("zscore"))) +
      ylim(y_lim[1], y_lim[2]) 
    
    filename <- paste0(i, "_col_zscore_post_", j, ".png")
    ggsave(filename, plot = plot, path = 'results/plots/colanal/', width = 12, height = 8)
    
  }
}




pdfname <- paste(Sys.Date(), 'col_zscore_iso', sep = "_")

pdf(sprintf('results/plots/colanal/%s.pdf', pdfname), height = 4, width = 12) 

mylist <- list()
x <- 0

for (id in unique(zscore_column$embryo.id)) {
      filtered_data <- zscore_column %>% 
        filter(measure == 'iso', embryo.id == id, !is.na(period.subset)) %>% 
        group_by(period.subset) %>%
        mutate(timepoint = timepoint - min(timepoint)) %>%
        ungroup()
      
      plot <- filtered_data %>% 
        ggplot(aes(x = distance_aligned, y = z_score, colour = timepoint, group = timepoint)) +
        geom_line(linewidth = 1, alpha = 0.5) +
        geom_vline(xintercept = 0) +
        scale_colour_gradient(low = "red", high = "blue", na.value = NA) +
        theme_bw() + 
        facet_wrap(~period.subset, scales = "free") +
        theme(strip.background = element_blank(),
              strip.text.x = element_text(size = 8),
              panel.spacing = unit(0.5, 'lines')) +
        xlab('Distance') +
        ylab(expression(paste("zscore"))) +
        ylim(-6, 6) +
        labs(title = paste("Embryo ID:", id, ", Measure: iso", "Direction:", unique(filtered_data$direction))) 
      
      print(plot)
      
      x <- x + 1
      
      mylist[[x]] <- plot
      
      #filename <- paste0(id, "_col_zscore_", i, ".png")
      #ggsave(filename, plot = plot, path = 'results/plots/colanal/', width = 12, height = 4)
      
}


dev.off()  


pdfname <- paste(Sys.Date(), 'col_zscore_iso_all', sep = "_")

myplot <- patchwork::wrap_plots(mylist, ncol = 1, nrow = 10)
ggsave(sprintf('results/plots/colanal/%s.png', pdfname),
       myplot,
       width = 12,
       height = 40,
       units = 'in')

pdfname <- paste(Sys.Date(), 'col_zscore_ani', sep = "_")

pdf(sprintf('results/plots/colanal/%s.pdf', pdfname), height = 4, width = 12) 
mylist <- list()
x <- 0

for (id in unique(zscore_column$embryo.id)) {
  filtered_data <- zscore_column %>% 
    filter(measure == 'ani', embryo.id == id, !is.na(period.subset)) %>% 
    group_by(period.subset) %>%
    mutate(timepoint = timepoint - min(timepoint)) %>%
    ungroup()
  
  plot <- filtered_data %>% 
    ggplot(aes(x = distance_aligned, y = z_score, colour = timepoint, group = timepoint)) +
    geom_line(linewidth = 1, alpha = 0.5) +
    geom_vline(xintercept = 0) +
    scale_colour_gradient(low = "red", high = "blue", na.value = NA) +
    theme_bw() + 
    facet_wrap(~period.subset, scales = "free") +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance') +
    ylab(expression(paste("zscore"))) +
    ylim(-2.5, 6) +
    labs(title = paste("Embryo ID:", id, ", Measure: ani", "Direction:", unique(filtered_data$direction))) 
  
  print(plot)
  
  x <- x + 1
  
  mylist[[x]] <- plot
  
  #filename <- paste0(id, "_col_zscore_", i, ".png")
  #ggsave(filename, plot = plot, path = 'results/plots/colanal/', width = 12, height = 4)
  
}


dev.off()  

pdfname <- paste(Sys.Date(), 'col_zscore_ani_all', sep = "_")

myplot <- patchwork::wrap_plots(mylist, ncol = 1, nrow = 10)
ggsave(sprintf('results/plots/colanal/%s.png', pdfname),
       myplot,
       width = 12,
       height = 40,
       units = 'in')

pdfname <- paste(Sys.Date(), 'col_zscore_vel', sep = "_")

pdf(sprintf('results/plots/colanal/%s.pdf', pdfname), height = 4, width = 12) 
mylist <- list()
x <- 0


for (id in unique(zscore_column$embryo.id)) {
  filtered_data <- zscore_column %>% 
    filter(measure == 'vel', embryo.id == id, !is.na(period.subset)) %>% 
    group_by(period.subset) %>%
    mutate(timepoint = timepoint - min(timepoint)) %>%
    ungroup()
  
  plot <- filtered_data %>% 
    ggplot(aes(x = distance_aligned, y = z_score, colour = timepoint, group = timepoint)) +
    geom_line(linewidth = 1, alpha = 0.5) +
    geom_vline(xintercept = 0) +
    scale_colour_gradient(low = "red", high = "blue", na.value = NA) +
    theme_bw() + 
    facet_wrap(~period.subset, scales = "free") +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance') +
    ylab(expression(paste("zscore"))) +
    ylim(-2, 12) +
    labs(title = paste("Embryo ID:", id, ", Measure: vel", "Direction:", unique(filtered_data$direction))) 
  
  print(plot)
  
  x <- x + 1
  
  mylist[[x]] <- plot
  
  #filename <- paste0(id, "_col_zscore_", i, ".png")
  #ggsave(filename, plot = plot, path = 'results/plots/colanal/', width = 12, height = 4)
  
}


dev.off()  

pdfname <- paste(Sys.Date(), 'col_zscore_vel_all', sep = "_")

myplot <- wrap_plots(mylist, ncol = 1, nrow = 10)
ggsave(sprintf('results/plots/colanal/%s.png', pdfname),
       myplot,
       width = 12,
       height = 40,
       units = 'in')




# averaging before----


#------


averaged_normalised_1 <- normalised_column_1 %>% 
  group_by(measure, direction, timepoint, distance_aligned) %>% 
  summarise(mean = mean(norm_value),
            ROI_start_aligned = median(ROI_start_aligned),
            ROI_end_aligned = median(ROI_end_aligned)) %>% 
  ungroup()


averaged_normalised_2 <- normalised_column_2 %>% 
  group_by(measure, direction, timepoint, distance_aligned) %>% 
  summarise(mean = mean(norm_value),
            ROI_start_aligned = median(ROI_start_aligned),
            ROI_end_aligned = median(ROI_end_aligned)) %>% 
  ungroup()

averaged_zscore <- zscore_column %>% 
  group_by(measure, direction, timepoint, distance_aligned) %>% 
  summarise(mean = mean(z_score),
            std = sd(z_score),
            ROI_start_aligned = median(ROI_start_aligned),
            ROI_end_aligned = median(ROI_end_aligned)) %>% 
  ungroup()

averaged_unnorm <- aligned_column %>% 
  group_by(measure, direction, timepoint, distance_aligned) %>% 
  summarise(mean = mean(value),
            ROI_start_aligned = median(ROI_start_aligned),
            ROI_end_aligned = median(ROI_end_aligned)) %>% 
  ungroup()


averaged_normalised_1 %>% 
  filter(timepoint > 0, timepoint <=50, measure == 'iso', direction == 'anterior') %>% 
  ggplot(aes(x = distance_aligned, y = mean, color = timepoint, group = timepoint)) +
  # geom_rect(aes( xmin = 0, xmax = (ROI_end - ROI_start), ymin = -Inf, ymax = Inf), alpha = 0.5, fill = 'pink', colour = 'pink') +
  geom_line() +
  geom_vline(xintercept =  0) +
  scale_color_viridis(option = 'turbo') + 
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Distance') +
  ylab(expression(paste("Normalised Velocity")))

averaged_normalised_2 %>% 
  filter(timepoint > 0, timepoint <=50, measure == 'iso', direction == 'anterior') %>% 
  ggplot(aes(x = distance_aligned, y = mean, color = timepoint, group = timepoint)) +
  # geom_rect(aes( xmin = 0, xmax = (ROI_end - ROI_start), ymin = -Inf, ymax = Inf), alpha = 0.5, fill = 'pink', colour = 'pink') +
  geom_line() +
  geom_vline(xintercept =  0) +
  scale_color_viridis(option = 'turbo') + 
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Distance') +
  ylab(expression(paste("Normalised Velocity")))



(iso_col_during <- averaged_zscore %>% 
  filter(timepoint >= 0, timepoint <=50, measure == 'iso', direction == 'anterior') %>% 
  ggplot(aes(x = distance_aligned, y = mean, color = timepoint, group = timepoint)) +
  geom_rect(aes(xmin = ROI_start_aligned, xmax = (ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.2, fill = 'grey', colour = 'grey') +
  annotate("text", x = 20, y = -1.5, label = "ROI", size = 6) +
  geom_hline(yintercept = 0, linetype = 'dashed', linewidth = 0.5) +
  geom_line() +
  scale_color_viridis(option = 'turbo') + 
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        strip.text.y = element_text(size = 8),
        axis.text=element_text(size = 15),
        axis.title=element_text(size = 15),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Distance [µm]') +
  ylab(expression(paste("Average Isotropic Strain z-score"))))

iso_col_before <- averaged_zscore %>% 
  filter(timepoint >= -50, timepoint <=0, measure == 'iso', direction == 'anterior') %>% 
  ggplot(aes(x = distance_aligned, y = mean, color = timepoint, group = timepoint)) +
  geom_rect(aes(xmin = ROI_start_aligned, xmax = (ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.2, fill = 'grey', colour = 'grey') +
  annotate("text", x = 20, y = -1.5, label = "ROI", size = 6) +
  geom_hline(yintercept = 0, linetype = 'dashed', linewidth = 0.5) +
  geom_line() +
  scale_color_viridis(option = 'turbo') + 
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        strip.text.y = element_text(size = 8),
        axis.text=element_text(size = 15),
        axis.title=element_text(size = 15),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Distance [µm]') +
  ylab(expression(paste("Average Isotropic Strain z-score")))


#filename <- 'Average_iso_col_zscore_before.png'
#ggsave(filename, plot = iso_col_before , path = 'results/plots/', width = 10, height = 4)
#filename <- 'Average_iso_col_zscore_during.png'
#ggsave(filename, plot = iso_col_during , path = 'results/plots/', width = 10, height = 4)


averaged_unnorm %>% 
  filter(timepoint > 0, timepoint <=50, measure == 'iso', direction == 'anterior') %>% 
  ggplot(aes(x = distance_aligned, y = mean, color = timepoint, group = timepoint)) +
  # geom_rect(aes( xmin = 0, xmax = (ROI_end - ROI_start), ymin = -Inf, ymax = Inf), alpha = 0.5, fill = 'pink', colour = 'pink') +
  geom_line() +
  geom_vline(xintercept =  0) +
  scale_color_viridis(option = 'turbo') + 
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Distance') +
  ylab(expression(paste("Normalised Velocity")))

normalised_column_1 %>% 
  filter(measure == 'vel', timepoint >=0, timepoint <= 120) %>% 
  group_by(direction, distance_aligned, embryo.id) %>% 
  slice_max(value, n = 1) %>% 
  select(timepoint, peak_timepoint) %>% 
  group_by(direction, distance_aligned) %>% 
  summarise(mu = mean(value)) %>% 
  ggplot(aes(x = distance_aligned, y = mu, colour = direction)) +
  geom_line()

averaged_normalised_peak <- normalised_column_2 %>% 
  group_by(embryo.id) %>%
  filter(timepoint == peak_timepoint[measure == 'vel']) %>%  
  group_by(measure, direction, distance_aligned) %>% 
  summarise(mean = mean(norm_value),
            ROI_start = median(ROI_start),
            ROI_end = median(ROI_end)) %>% 
  ungroup()

averaged_zscore_peak <- zscore_column %>% 
  group_by(embryo.id) %>%
  filter(timepoint == peak_timepoint[measure == 'vel']) %>%  
  group_by(measure, direction, distance_aligned) %>% 
  summarise(mean = mean(z_score),
            sem = sd(z_score) / sqrt(n()),
            ROI_start_aligned = median(ROI_start_aligned),
            ROI_end_aligned = median(ROI_end_aligned)) %>% 
  ungroup()


ani_col_peak <- averaged_zscore_peak %>% 
  filter(measure == 'ani', direction == 'anterior', distance_aligned > -115 , distance_aligned < 150) %>% 
  ggplot(aes(x = distance_aligned, y = mean)) +
  geom_rect(aes(xmin = ROI_start_aligned, xmax = (ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.2, fill = 'grey', colour = 'grey') +
  geom_ribbon(aes(ymin = (mean - sem), ymax = (mean + sem)), fill = "black", alpha = 0.3) +
  annotate("text", x = 20, y = -1.5, label = "ROI", size = 6) +
  geom_line(linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = 'dashed', linewidth = 0.5) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        strip.text.y = element_text(size = 8),
        axis.text=element_text(size = 18),
        axis.title=element_text(size = 18),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Distance [µm]') +
  ylab(expression(paste("Peak anisotropic strain rate z-score")))

filename <- 'Average_ani_col_zscore_peak.png'
ggsave(filename, plot = ani_col_peak , path = 'results/plots/', width = 10, height = 4)


# Binning to see any preact trends -----


average_before <- zscore_column %>% 
  filter(timepoint < -15, distance > 3, distance < (max(distance) - 3)) %>%
  group_by(embryo.id, measure, distance_aligned, ROI_start, ROI_end, direction, somite) %>% 
  summarise(mean = mean(z_score)) %>% 
  ungroup() 

pdfname <- paste(Sys.Date(), 'before_zscore_ant', sep = "_")

#pdf(sprintf('results/plots/colanal/%s.pdf', pdfname), height = 6, width = 12) 

pptx <- read_pptx() %>%
  add_slide(layout='Title Slide', master='Office Theme')

for (i in measures) {
  d = 'anterior'
  
  plot <- average_before %>% 
    filter(measure == i, direction == d) %>% 
    ggplot(aes(x = distance_aligned, y = mean)) +
    geom_line(linewidth = 1, alpha = 0.5) +
    geom_vline(xintercept = 0) +
    scale_colour_gradient(low = "red", high = "blue", na.value = NA) +
    theme_bw() + 
    facet_wrap(~embryo.id, nrow = 3, ncol = 3) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance') +
    ylab(expression(paste("zscore"))) 
  
  title = paste("Average", i ,"preactivation z-score of", d ,"embryos")
  
  pptx <- pptx %>% 
    add_slide(layout='Title and Content',master='Office Theme') %>% 
    ph_with(title, location = ph_location_type(type="title")) %>%
    ph_with(plot, location = ph_location("body", left = 1, top = 2, width = 8, height = 5))
  
  #print(plot)
  
}



# dev.off()

print(pptx, sprintf('results/plots/colanal/%s.pptx', pdfname))

#

pdfname <- paste(Sys.Date(), 'before_zscore_post', sep = "_")

# pdf(sprintf('results/plots/colanal/%s.pdf', pdfname), height = 3, width = 12)

pptx <- read_pptx() %>%
  add_slide(layout='Title Slide', master='Office Theme')

for (i in measures) {
  d = 'posterior'
  plot <- average_before %>% 
    filter(measure == i, direction == d) %>% 
    ggplot(aes(x = distance_aligned, y = mean)) +
    geom_line(linewidth = 1, alpha = 0.5) +
    geom_vline(xintercept = 0) +
    scale_colour_gradient(low = "red", high = "blue", na.value = NA) +
    theme_bw() + 
    facet_wrap(~embryo.id, nrow = 3, ncol = 3) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance') +
    ylab(expression(paste("zscore"))) 
  
  title = paste("Average", i ,"preactication z-score of", d ,"embryos")
  
  pptx <- pptx %>% 
    add_slide(layout='Title and Content',master='Office Theme') %>% 
    ph_with(title, location = ph_location_type(type="title")) %>%
    ph_with(plot, location = ph_location("body", left = 1, top = 3, width = 8, height = 2))
    
  
}

# dev.off()

print(pptx, sprintf('results/plots/colanal/%s.pptx', pdfname))

# Binning data into 55secs of each period --------



  
binned_before <- aligned_column %>% 
  filter(timepoint >= -70, timepoint <= -20) %>%
  group_by(embryo.id, measure, distance_aligned, ROI_start, ROI_end, direction, somite) %>% 
  summarise(mean = mean(value)) %>% 
  mutate(period = 'during') %>% 
  ungroup() 


binned_during <- aligned_column %>% 
  filter(timepoint >= 0, timepoint <= 50) %>%
  group_by(embryo.id, measure, distance_aligned, ROI_start, ROI_end, direction, somite) %>% 
  summarise(mean = mean(value)) %>% 
  mutate(period = 'during') %>% 
  ungroup() 

binned_after <- aligned_column %>% 
  filter(timepoint >= 125, timepoint <= 175) %>%
  group_by(embryo.id, measure, distance_aligned, ROI_start, ROI_end, direction, somite) %>% 
  summarise(mean = mean(value)) %>% 
  mutate(period = 'after') %>% 
  ungroup() 

# binned_during <- aligned_column %>% 
#   filter(timepoint >= 120, timepoint <= 175) %>%
#   group_by(embryo.id, distance) %>% 
#   summarise(mean = mean(value)) %>%  
#   mutate(period = 'during') %>% 
#   ungroup() 
# 
# binned_after <- aligned_column %>% 
#   filter(timepoint >= 240, timepoint <= 315) %>%
#   group_by(embryo.id, distance) %>% 
#   summarise(mean = mean(value)) %>%  
#   mutate(period = 'after') %>% 
#   ungroup() 

binned_column <- bind_rows(binned_before, binned_during, binned_after) %>% 
  left_join(preact_threshold, by = join_by(embryo.id, measure)) %>% 
  mutate(period = factor(period, levels = c('before', 'during', 'after'))) %>% 
  group_by(embryo.id) %>% 
  mutate(above_threshold = mean > threshold,
         norm_vel = mean / threshold) %>% 
  ungroup()

# Averaging the anterior and posterior embryos ------

# need to sort this into anterior/posterior first and make sure the rois are aligned
averaged_column <- binned_column %>% 
  group_by(measure, direction, period, distance_aligned) %>% 
  summarise(mean = mean(mean),
            norm_mean = mean(norm_vel)) %>% 
  ungroup()

# Plotting singular timepoints from each embryo ------
  
(
  ant_embryos_col_vel <- aligned_column %>% 
    filter(direction == 'anterior', measure == 'vel', timepoint >= 120, timepoint <= 170) %>% #, timepoint >= 120, timepoint <= 175
    ggplot(aes(x = distance_aligned, y = value, group = timepoint, colour = timepoint)) +
    geom_rect(aes(xmin = 0, xmax = (ROI_end - ROI_start), ymin = -Inf, ymax = Inf), alpha = 0.5, fill = 'pink', colour = 'pink') +
    geom_line() +
    scale_color_viridis(option = 'turbo') + 
    theme_bw() +
    facet_grid(rows = vars(embryo.id)) +
    ylim(-0, 0.016) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)

plot <- vel_ant + ant_embryos_col_vel + plot_layout(widths = c(2,1)) 
plot
filename <- 'ant_t=3_col_vel.png'
ggsave(filename, plot = plot  , path = 'results/plots/', width = 8, height = 5)


(
  vel_col_20231026_E1_16ss <- aligned_column %>% 
    filter(direction == 'anterior', measure == 'vel', timepoint == 150, embryo.id == '20231026_E1_16ss') %>% 
    ggplot(aes(x = distance_aligned, y = value)) +
    geom_rect(aes(xmin = 0, xmax = (ROI_end - ROI_start), ymin = -Inf, ymax = Inf), alpha = 0.5, fill = 'pink', colour = 'pink') +
    geom_line() +
    scale_color_viridis() + 
    theme_bw() +
    facet_wrap(~embryo.id) +
    ylim(-0.0002, 0.012) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)

filename <- 'ant_embryos_col_vel.png'
ggsave(filename, plot = ant_embryos_col_vel  , path = 'results/plots/', width = 5, height = 5)



# Plotting thresholded binned timepoints ------

(
  binned_column %>% 
    filter(direction == 'anterior', measure == 'vel', period == 'before') %>% 
    ggplot(aes(x = distance_aligned, y = norm_vel, colour = embryo.id)) +
   # annotate('rect', xmin = 'ROI_start', xmax = 'ROI_end', ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    geom_rect(aes(xmin = 0, xmax = (ROI_end - ROI_start), ymin = -Inf, ymax = Inf), alpha = 0.03, fill = 'pink', colour = NA ) +
    geom_hline(aes(yintercept = 1)) +  
    geom_line() +
    theme_bw() +
    facet_grid(~period) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)

(
  binned_column %>% 
    filter(direction == 'anterior', measure == 'vel') %>% 
    ggplot(aes(x = distance_aligned, y = mean, colour = above_threshold, group = embryo.id)) +
    # annotate('rect', xmin = 'ROI_start', xmax = 'ROI_end', ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    geom_rect(aes(xmin = 0, xmax = (ROI_end - ROI_start), ymin = -Inf, ymax = Inf), alpha = 0.03, fill = 'pink', colour = NA ) +
    geom_hline(aes(yintercept = threshold)) +  
    geom_line() +
    theme_bw() +
    facet_grid(embryo.id~period) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)

(
  binned_column %>% 
    filter(direction == 'posterior', measure == 'vel') %>% 
    ggplot(aes(x = distance_aligned, y = mean, colour = above_threshold, group = embryo.id)) +
    # annotate('rect', xmin = 'ROI_start', xmax = 'ROI_end', ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "pink") +
    geom_rect(aes(xmin = 0, xmax = (ROI_start - ROI_end), ymin = -Inf, ymax = Inf), fill = "pink", alpha = 0.5) +
    geom_hline(aes(yintercept = threshold)) +  
    geom_line() +
    theme_bw() +
    facet_wrap(~period) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
)

# Plotting ant/post averaged embryos -------

(averaged_column %>%
   filter(measure == 'vel') %>% 
    ggplot(aes(x = distance_aligned, y = norm_mean)) +
    geom_line() +
   geom_vline(xintercept = 0) +
   geom_hline(yintercept = 1) +
    theme_bw() +
    facet_grid(direction~period) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance') +
    ylab(expression(paste("Velocity [µm ", s^-1, "]")))
  )


library(seewave)

wave <- aligned_column %>% 
  filter(embryo.id == '20231026_E3_NA', measure == 'vel', timepoint == 10) %>% 
  pull(value) 

spectrum(wave,log="no")

 del<-5
 x.spec <- spectrum(wave,log="no",span=10,plot=FALSE)
 spx <- x.spec$freq/del
 spy <- 2*x.spec$spec
 plot(spy~spx,xlab="frequency",ylab="spectral density",type="l")
