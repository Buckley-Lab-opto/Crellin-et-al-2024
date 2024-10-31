library(tidyverse)
library(openxlsx)
# library(plotly)
library(cmocean)
library(viridis)
library(ggpubr)
library(svglite)
library(rstatix)
# library(pdf2pptx)
# library(officer)
# library(patchwork)


# Read in data and make data frame - with compatible identifiers to other data ----
file_directory <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/data/column analysis/2024_Jun_17'

file_list <- list.files(file_directory, pattern = '.xlsx', full.names = TRUE)
data_list <- list()
i <- 1

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
  mutate(date_2 = format(as.Date(date_2), '%Y%m%d')) %>% 
  mutate(date = coalesce(date, date_2)) %>% 
  select(-'date_2') %>% 
  pivot_longer(cols = c(-'date', -'embryo', -'somite', -'embryo.id', -'measure', -'row.id', -'ROI', -'POSITION'), 
               names_to = 'timepoint', 
               values_to = 'value') %>% 
  rename(distance = POSITION) %>% 
  mutate(timepoint = as.numeric(str_remove(timepoint, 't'))) %>% 
  mutate(timepoint = (timepoint - 26 ) * 5) %>% # Preact = -125 to -5 , Act = 0 - 120 , Deact = 125 - 240??
  unite(col = 'embryo.id', c('date', 'embryo', 'somite'), remove = FALSE) 

# Annotate with anterior or posterior from thresholded_data and remove embryos below threshold ----

thresholded_data <- read.xlsx('results/spreadsheets/2024-06-20_thresholded_data_ungrouped.xlsx') %>% 
  select(embryo.id, direction) %>% 
  distinct(embryo.id, direction)

peak_position <- read.xlsx('results/spreadsheets/2024-06-20_peak_vel.xlsx') 

column_data <- column_data %>% 
  left_join(thresholded_data, by = 'embryo.id') %>% 
  filter(!is.na(direction)) %>% 
  left_join(peak_position, by = 'embryo.id')

column_data %>% 
  select(embryo.id, direction) %>% 
  distinct(embryo.id, direction)
# Aligning the data by ROI --------


ROI_position <- column_data %>%
  group_by(embryo.id) %>%
  filter(ROI == 1) %>%
  summarise(ROI_start = first(distance),
            ROI_end = last(distance),
            ROI_size = ROI_end - ROI_start) %>%
  ungroup()

ROI_position %>% get_summary_stats(ROI_size)

aligned_column <- column_data %>%
  left_join(ROI_position, by = 'embryo.id') %>%
  mutate(distance_aligned = case_when(direction == 'anterior' ~ round((distance - ROI_start), digits = 2)  ,
                                      direction == 'posterior' ~ round((distance - ROI_start), digits = 2) ),
         ROI_start_aligned = case_when(direction == 'anterior' ~ round((ROI_start - ROI_start), digits = 2)   ,
                              direction == 'posterior' ~ round((ROI_start - ROI_start), digits = 2) ),
         ROI_end_aligned = case_when(direction == 'anterior' ~ round((ROI_end - ROI_start), digits = 2)   ,
                             direction == 'posterior' ~ round((ROI_end - ROI_start), digits = 2) )) 
## float issues only solved with below
 # mutate(distance_aligned = round(distance_aligned, digits = 2),
 #        ROI_start_aligned = round(ROI_start_aligned, digits = 2),
 #        ROI_end_aligned = round(ROI_end_aligned, digits = 2)  )


# Create a threshold during preactivation to measure total force propagation -------

preact_threshold <- aligned_column %>% # average measure of all t=before and all y
  group_by(distance_aligned) %>% 
  filter(n_distinct(embryo.id) > 5) %>% 
  #ungroup() %>% 
  filter(timepoint < 0) %>% 
  #filter(timepoint < 0, distance > 3, distance < (max(distance) - 3)) %>% # trim edges of distance
  group_by(embryo.id, measure) %>% 
  #drop_na(value) %>% 
  summarise(threshold = mean(value),
            threshold_std = sd(value)) %>% 
  ungroup() 


peak_value <- aligned_column %>% # average measure of all t=before and all y
  group_by(distance_aligned) %>% 
  filter(n_distinct(embryo.id) > 5) %>% 
  ungroup() %>% 
  filter(timepoint >= 0, timepoint <=120) %>% 
  #filter(timepoint >= 0, timepoint <=120, distance > 3, distance < (max(distance) - 3)) %>% # trim edges of distance
  select(embryo.id, measure, distance, timepoint, value) %>% 
  group_by(embryo.id, measure) %>% 
  slice_max(order_by = value) %>% 
  mutate(peak_value = value,
         peak_distance = distance,
         peak_timepoint = timepoint) %>% 
  select(-value, -distance, -timepoint) %>% 
  ungroup() 

# normalisation testing -----

# normalised_column_1 <- aligned_column %>% # normalisation to preact mean (norm to min)
#   left_join(preact_threshold, by = join_by(embryo.id, measure)) %>% 
#   left_join(peak_value, by = join_by(embryo.id, measure)) %>% 
#   group_by(embryo.id) %>% 
#   mutate(norm_value = (value / threshold)) %>% 
#   drop_na(norm_value) %>% 
#   ungroup()
# 
# normalised_column_2 <- aligned_column %>% # min/max normalisation 0 = preactivation, 1 = max
#   left_join(preact_threshold, by = join_by(embryo.id, measure)) %>% 
#   left_join(peak_value, by = join_by(embryo.id, measure)) %>% 
#   group_by(embryo.id) %>% 
#   mutate(norm_value = (value - threshold) / (peak_value - threshold)) %>% 
#   drop_na(norm_value) %>% 
#   ungroup()

zscore_column <- aligned_column %>% # normalisation to preact mean (norm to min)
  left_join(preact_threshold, by = join_by(embryo.id, measure)) %>% 
  left_join(peak_value, by = join_by(embryo.id, measure)) %>% 
  group_by(embryo.id) %>%
  mutate(z_score = (value - threshold) / threshold_std ) %>% 
  #drop_na(z_score) %>% 
  ungroup() %>% 
  mutate(period.subset = case_when(timepoint >= -85 & timepoint <= -30 ~ 'before',
                                   timepoint >= 0 & timepoint <= 55 ~ 'during',
                                   timepoint >= 125 & timepoint <= 180 ~ 'after')) %>% 
  mutate(period.subset = factor(period.subset, levels = c('before', 'during', 'after')))

filename <- paste(Sys.Date(), 'zscore_column', sep = "_")
write.xlsx(zscore_column, file = sprintf('results/spreadsheets/column/%s.xlsx', filename))



# normalised_column_1 %>% 
#   filter(direction == 'anterior', measure == 'iso', timepoint == peak_timepoint[measure == 'vel']) %>% #, timepoint >= 120, timepoint <= 175
#   ggplot(aes(x = distance_aligned, y = norm_value, colour = embryo.id)) +
#   geom_rect(aes(xmin = 0, xmax = (ROI_end), ymin = -Inf, ymax = Inf), alpha = 0.5, fill = '#fbb03b', colour = '#fbb03b') +
#   geom_line(linewidth = 1) +
#   theme_bw() +
#   theme(strip.background = element_blank(),
#         strip.text.x = element_text(size = 8),
#         panel.spacing = unit(0.5, 'lines')) +
#   xlab('Distance') +
#   ylab(expression(paste('Normalised Velocity')))
# 
# normalised_column_2 %>% 
#   filter(direction == 'anterior', measure == 'iso', timepoint == peak_timepoint[measure == 'vel']) %>% #, timepoint >= 120, timepoint <= 175
#   ggplot(aes(x = distance_aligned, y = norm_value, colour = embryo.id)) +
#   geom_rect(aes(xmin = 0, xmax = (ROI_end), ymin = -Inf, ymax = Inf), alpha = 0.5, fill = '#fbb03b', colour = '#fbb03b') +
#   geom_line(linewidth = 1) +
#   theme_bw() +
#   theme(strip.background = element_blank(),
#         strip.text.x = element_text(size = 8),
#         panel.spacing = unit(0.5, 'lines')) +
#   xlab('Distance') +
#   ylab(expression(paste('Normalised Velocity')))

#------
zscore_column %>% 
  filter(direction == 'anterior', measure == 'ani', timepoint > 10, timepoint <=30) %>% #, timepoint >= 120, timepoint <= 175
  ggplot(aes(x = distance_aligned, y = z_score, colour = embryo.id, group = timepoint)) +
  geom_rect(aes(xmin = 0, xmax = ROI_end_aligned, ymin = -Inf, ymax = Inf), alpha = 0.5, fill = '#fbb03b', colour = '#fbb03b') +
  geom_line(linewidth = 1) +
  theme_bw() + facet_wrap(~embryo.id) + 
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Distance') +
  ylab(expression(paste('Normalised Aniso Strain Rate')))

zscore_column %>% 
  filter(direction == 'anterior', measure == 'str', timepoint > 10, timepoint <=30) %>% #, timepoint >= 120, timepoint <= 175
  ggplot(aes(x = distance_aligned, y = z_score, colour = embryo.id, group = timepoint)) +
  geom_rect(aes(xmin = 0, xmax = ROI_end_aligned, ymin = -Inf, ymax = Inf), alpha = 0.5, fill = '#fbb03b', colour = '#fbb03b') +
  geom_line(linewidth = 1) +
  theme_bw() + facet_wrap(~embryo.id) + 
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Distance') +
  ylab(expression(paste('Normalised Strain Rate')))

zscore_column %>% 
  filter(direction == 'anterior', measure == 'apstr', timepoint > 10, timepoint <=30) %>% #, timepoint >= 120, timepoint <= 175
  ggplot(aes(x = distance_aligned, y = z_score, colour = embryo.id, group = timepoint)) +
  geom_rect(aes(xmin = 0, xmax = ROI_end_aligned, ymin = -Inf, ymax = Inf), alpha = 0.5, fill = '#fbb03b', colour = '#fbb03b') +
  geom_line(linewidth = 1) +
  theme_bw() + facet_wrap(~embryo.id) + 
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Distance') +
  ylab(expression(paste('Normalised AP Strain rate')))

zscore_column %>% 
  filter(direction == 'anterior', measure == 'mlstr', timepoint > 10, timepoint <=30) %>% #, timepoint >= 120, timepoint <= 175
  ggplot(aes(x = distance_aligned, y = z_score, colour = embryo.id, group = timepoint)) +
  geom_rect(aes(xmin = 0, xmax = ROI_end_aligned, ymin = -Inf, ymax = Inf), alpha = 0.5, fill = '#fbb03b', colour = '#fbb03b') +
  geom_line(linewidth = 1) +
  theme_bw() + facet_wrap(~embryo.id) + 
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Distance') +
  ylab(expression(paste('Normalised ML Strain Rate')))

aligned_column %>% 
  filter(measure == 'iso', timepoint >= 0, timepoint <=55) %>% #, timepoint >= 120, timepoint <= 175
  ggplot(aes(x = distance_aligned, y = value, colour = embryo.id, group = timepoint)) +
  geom_rect(aes(xmin = ROI_start_aligned, xmax = (ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.5, fill = '#fbb03b', colour = '#fbb03b') +
  geom_line(linewidth = 0.5) +
  theme_bw() +
  facet_wrap(~embryo.id)+
  theme(aspect.ratio = 1,
        strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Distance') +
  ylab(expression(paste('Normalised Velocity')))

aligned_column %>% 
  filter(measure == 'iso', timepoint >= -55, timepoint <=0) %>% #, timepoint >= 120, timepoint <= 175
  ggplot(aes(x = distance_aligned, y = value, colour = embryo.id, group = timepoint)) +
  geom_rect(aes(xmin = ROI_start_aligned, xmax = (ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.5, fill = '#fbb03b', colour = '#fbb03b') +
  geom_line(linewidth = 0.5) +
  theme_bw() +
  facet_wrap(~embryo.id)+
  theme(aspect.ratio = 1,
        strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Distance') +
  ylab(expression(paste('Normalised Velocity')))

aligned_column %>% 
  filter(measure == 'iso', timepoint >= 125, timepoint <=180) %>% #, timepoint >= 120, timepoint <= 175
  ggplot(aes(x = distance_aligned, y = value, colour = embryo.id, group = timepoint)) +
  geom_rect(aes(xmin = ROI_start_aligned, xmax = (ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.5, fill = '#fbb03b', colour = '#fbb03b') +
  geom_line(linewidth = 0.5) +
  theme_bw() +
  facet_wrap(~embryo.id)+
  theme(aspect.ratio = 1,
        strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Distance') +
  ylab(expression(paste('Normalised Velocity')))


# plotting individual embryos mar/apr 2024 -----


measures <- c('vel', 'iso', 'ani', 'str', 'apstr', 'mlstr', 'x', 'y')
periods <- c('before', 'during', 'after')

y_limits <- list(
  vel = c(-2, 12),  
  iso = c(-6, 6),  
  ani = c(-2, 6),
  str = c(-2, 8),
  apstr = c(-2, 8),
  mlstr = c(-2, 8),
  x = c(-6, 6),  
  y = c(-6, 6)
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
        scale_colour_gradient(low = 'red', high = 'blue', na.value = NA) +
        theme_bw() + 
        facet_wrap(~embryo.id) +
        theme(strip.background = element_blank(),
              strip.text.x = element_text(size = 8),
              panel.spacing = unit(0.5, 'lines')) +
        xlab('Distance') +
        ylab(expression(paste('zscore'))) +
        ylim(y_lim[1], y_lim[2]) 

      print(plot)
      #filename <- paste0(i, '_col_zscore_ant_', j, '.png')
      #ggsave(filename, plot = plot, path = 'results/plots/colanal/', width = 12, height = 8)
    
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
      scale_colour_gradient(low = 'red', high = 'blue', na.value = NA) +
      theme_bw() + 
      facet_wrap(~embryo.id) +
      theme(strip.background = element_blank(),
            strip.text.x = element_text(size = 8),
            panel.spacing = unit(0.5, 'lines')) +
      xlab('Distance') +
      ylab(expression(paste('zscore'))) +
      ylim(y_lim[1], y_lim[2]) 
    
    filename <- paste0(i, '_col_zscore_post_', j, '.png')
    ggsave(filename, plot = plot, path = 'results/plots/colanal/', width = 12, height = 8)
    
  }
}

#heatmaps


limits <- list(
  vel = c(-1.5, 3.5),
  iso = c(-1.7, 1.7),
  ani = c(-1.5, 3.1),
  str = c(-1.5, 5),
  apstr = c(-1.5, 3.1),
  mlstr = c(-1.5, 2.2),
  x = c(-2.6, 3),
  y = c(-2.6, 3)
)



measure_labs <- list(
  vel = 'Velocity',  
  iso = 'Isotropic Strain Rate',  
  ani = 'Anisotropic Strain Rate',
  str = ' Strain Rate' ,
  apstr = 'AP Strain Rate' ,
  mlstr = 'ML Strain Rate',
  x = 'ML Iso Strain Rate',
  y = 'AP Iso Strain Rate'
)
for (i in measures) {
  limit <- y_limits[[i]]
  measure_lab <- measure_labs[[i]]
  
  if (i == 'iso') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
  } else if (i == 'ani') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'amp')
  } else if (i == 'vel') {
    colour_map = scale_fill_viridis(limits = c(limit[1], limit[2]))
  } else if (i == 'x') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
  } else if (i == 'y') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
  } 
  
  # for (j in periods) {
  #   
  #   filtered_data <- zscore_column %>% 
  #     filter(measure == i, period.subset == j)
  filtered_data <- zscore_column %>% 
         filter(measure == i)
    
    (
      plot <- filtered_data %>% 
        ggplot(aes(y = distance_aligned, z = z_score, x = timepoint, group = timepoint)) +
        geom_raster(aes(fill = z_score)) +
        geom_hline(yintercept = 0, linewidth = 1.5, colour = 'black', alpha = 0.6) +
        geom_hline(aes(yintercept = max(ROI_end_aligned)), linewidth = 1.5, colour = 'black', alpha = 0.6) +
        colour_map +
        facet_wrap(direction~embryo.id) +
        scale_x_continuous(expand = c(0,0), n.breaks = 6) +
        scale_y_reverse(expand = expansion(add = c(0, 1)),  n.breaks = 6)  +
        labs(title = measure_lab, #measure_lab,
             x = 'Time (s)', 
             y = 'Distance relative to ROI (µm)', 
             fill = 'Z-Score',
             hjust = 0.5 ) +
        theme_bw() +
        theme(text = element_text(size = 10, family = 'Avenir'),
              axis.title = element_blank(),
              axis.line = element_blank(),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              plot.title = element_text(hjust = 0.5),
              legend.key.size = unit(3.5 , 'line')) 
    )
    
    print(plot)  
   # filename <- paste0(i, '_col_zscore_', j, '.png')
  #  filename <- paste(Sys.Date(), filename, sep = "_")
   # ggsave(filename, plot = plot, path = 'results/plots/colanal/individuals/', width = 6, height = 14)
    
    
  }





pdfname <- paste(Sys.Date(), 'col_zscore_iso', sep = '_')

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
        scale_colour_gradient(low = 'red', high = 'blue', na.value = NA) +
        theme_bw() + 
        facet_wrap(~period.subset, scales = 'free') +
        theme(strip.background = element_blank(),
              strip.text.x = element_text(size = 8),
              panel.spacing = unit(0.5, 'lines')) +
        xlab('Distance') +
        ylab(expression(paste('zscore'))) +
        ylim(-6, 6) +
        labs(title = paste('Embryo ID:', id, ', Measure: iso', 'Direction:', unique(filtered_data$direction))) 
      
      print(plot)
      
      x <- x + 1
      
      mylist[[x]] <- plot
      
      #filename <- paste0(id, '_col_zscore_', i, '.png')
      #ggsave(filename, plot = plot, path = 'results/plots/colanal/', width = 12, height = 4)
      
}


dev.off()  


pdfname <- paste(Sys.Date(), 'col_zscore_iso_all', sep = '_')

myplot <- patchwork::wrap_plots(mylist, ncol = 1, nrow = 10)
ggsave(sprintf('results/plots/colanal/%s.png', pdfname),
       myplot,
       width = 12,
       height = 40,
       units = 'in')

pdfname <- paste(Sys.Date(), 'col_zscore_ani', sep = '_')

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
    scale_colour_gradient(low = 'red', high = 'blue', na.value = NA) +
    theme_bw() + 
    facet_wrap(~period.subset, scales = 'free') +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance') +
    ylab(expression(paste('zscore'))) +
    ylim(-2.5, 6) +
    labs(title = paste('Embryo ID:', id, ', Measure: ani', 'Direction:', unique(filtered_data$direction))) 
  
  print(plot)
  
  x <- x + 1
  
  mylist[[x]] <- plot
  
  #filename <- paste0(id, '_col_zscore_', i, '.png')
  #ggsave(filename, plot = plot, path = 'results/plots/colanal/', width = 12, height = 4)
  
}


dev.off()  

pdfname <- paste(Sys.Date(), 'col_zscore_ani_all', sep = '_')

myplot <- patchwork::wrap_plots(mylist, ncol = 1, nrow = 10)
ggsave(sprintf('results/plots/colanal/%s.png', pdfname),
       myplot,
       width = 12,
       height = 40,
       units = 'in')

pdfname <- paste(Sys.Date(), 'col_zscore_vel', sep = '_')

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
    scale_colour_gradient(low = 'red', high = 'blue', na.value = NA) +
    theme_bw() + 
    facet_wrap(~period.subset, scales = 'free') +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance') +
    ylab(expression(paste('zscore'))) +
    ylim(-2, 12) +
    labs(title = paste('Embryo ID:', id, ', Measure: vel', 'Direction:', unique(filtered_data$direction))) 
  
  print(plot)
  
  x <- x + 1
  
  mylist[[x]] <- plot
  
  #filename <- paste0(id, '_col_zscore_', i, '.png')
  #ggsave(filename, plot = plot, path = 'results/plots/colanal/', width = 12, height = 4)
  
}


dev.off()  

pdfname <- paste(Sys.Date(), 'col_zscore_vel_all', sep = '_')

myplot <- wrap_plots(mylist, ncol = 1, nrow = 10)
ggsave(sprintf('results/plots/colanal/%s.png', pdfname),
       myplot,
       width = 12,
       height = 40,
       units = 'in')




# averaging together embryos-----


# averaged_normalised_1 <- normalised_column_1 %>% 
#   group_by(measure, direction, timepoint, distance_aligned) %>% 
#   summarise(mean = mean(norm_value),
#             ROI_start_aligned = median(ROI_start_aligned),
#             ROI_end_aligned = median(ROI_end_aligned)) %>% 
#   ungroup()
# 
# 
# averaged_normalised_2 <- normalised_column_2 %>% 
#   group_by(measure, direction, timepoint, distance_aligned) %>% 
#   summarise(mean = mean(norm_value),
#             ROI_start_aligned = median(ROI_start_aligned),
#             ROI_end_aligned = median(ROI_end_aligned)) %>% 
#   ungroup()



averaged_roi <-  zscore_column %>% 
  filter(ROI == 1) %>% 
  group_by(embryo.id) %>% 
  mutate(distance_aligned = (distance*1000 - ROI_start*1000) / 1000) %>% 
  mutate(distance_aligned = round(distance_aligned, digits = 2)) %>% 
  ungroup() %>% 
  group_by(measure, timepoint, distance_aligned) %>% 
  #drop_na(z_score) %>% 
  summarise(mean = mean(z_score),
            std = sd(z_score)) %>% 
  ungroup()

averaged_roi_dir <-  zscore_column %>% 
  filter(ROI == 1) %>% 
  group_by(embryo.id) %>% 
  mutate(distance_aligned = (distance*1000 - ROI_start*1000) / 1000) %>% 
  mutate(distance_aligned = round(distance_aligned, digits = 2)) %>% 
  ungroup() %>% 
  group_by(measure, timepoint, direction, distance_aligned) %>% 
  #drop_na(z_score) %>% 
  summarise(mean = mean(z_score),
            std = sd(z_score)) %>% 
  ungroup()
  

averaged_zscore <- zscore_column %>% 
  group_by(distance_aligned) %>% 
  filter(n_distinct(embryo.id) > 5) %>% 
  ungroup() %>% 
  group_by(measure, direction, timepoint, distance_aligned) %>% 
  #drop_na(z_score) %>% 
  summarise(mean = mean(z_score),
            std = sd(z_score),
            ROI_start_aligned = median(ROI_start_aligned),
            ROI_end_aligned = median(ROI_end_aligned)) %>% 
  ungroup()

filename <- paste(Sys.Date(), 'averaged_zscore_column', sep = "_")
write.xlsx(averaged_zscore, file = sprintf('results/spreadsheets/column/%s.xlsx', filename))





averaged_unnorm <- aligned_column %>% 
  group_by(measure, direction, timepoint, distance_aligned) %>% 
  summarise(mean = mean(value,  na.rm = TRUE),
            ROI_start_aligned = median(ROI_start_aligned, na.rm = TRUE),
            ROI_end_aligned = median(ROI_end_aligned, na.rm = TRUE),
            ROI_end_max = last(ROI_end_aligned)) %>% 
  ungroup()


averaged_zscore %>%
  filter(timepoint < 0, measure == 'iso', direction == 'anterior') %>%
  ggplot(aes(x = distance_aligned, y = mean, color = timepoint, group = timepoint)) +
  # geom_rect(aes( xmin = 0, xmax = (ROI_end - ROI_start), ymin = -Inf, ymax = Inf), alpha = 0.5, fill = '#fbb03b', colour = '#fbb03b') +
  geom_line() +
  geom_vline(xintercept =  0) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 8),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Distance') +
  ylab(expression(paste('Normalised Velocity')))
# 
# averaged_normalised_2 %>% 
#   filter(timepoint > 0, timepoint <=50, measure == 'iso', direction == 'anterior') %>% 
#   ggplot(aes(x = distance_aligned, y = mean, color = timepoint, group = timepoint)) +
#   # geom_rect(aes( xmin = 0, xmax = (ROI_end - ROI_start), ymin = -Inf, ymax = Inf), alpha = 0.5, fill = '#fbb03b', colour = '#fbb03b') +
#   geom_line() +
#   geom_vline(xintercept =  0) +
#   scale_color_viridis(option = 'turbo') + 
#   theme_bw() +
#   theme(strip.background = element_blank(),
#         strip.text.x = element_text(size = 8),
#         panel.spacing = unit(0.5, 'lines')) +
#   xlab('Distance') +
#   ylab(expression(paste('Normalised Velocity')))

# Heatmaps for paper -------

averaged_zscore <- averaged_zscore %>% 
  mutate(period.subset = case_when(timepoint >= -85 & timepoint <= -30 ~ 'before',
                                   timepoint >= 0 & timepoint <= 55 ~ 'during',
                                   timepoint >= 125 & timepoint <= 180 ~ 'after')) %>% 
  mutate(period.subset = factor(period.subset, levels = c('before', 'during', 'after')))


measures <- c('vel', 'iso', 'ani', 'x', 'y')
periods <- c('before', 'during', 'after')

# For checking limits
 averaged_zscore %>%
   filter(measure == 'vel', direction == 'anterior', timepoint <= 230) %>%
   summarise(max_mean = max(mean),
             min_mean = min(mean))

# limits <- list(
#   vel = c(-1.5, 3.5),  
#   iso = c(-1.7, 1.7),  
#   ani = c(-1.5, 3.1),
#   str = c(-1.5, 5),
#   apstr = c(-1.5, 3.1),
#   mlstr = c(-1.5, 2.2),
#   x = c(-2.6, 3),  
#   y = c(-2.6, 3)
# )

ant_limits <- list(
  vel = c(-1.6, 3.3),  
  iso = c(-1.7, 1.7),  
  ani = c(-1.8, 3.1),
  str = c(-1.5, 5),
  apstr = c(-1.5, 3.1),
  mlstr = c(-1.5, 2.2),
  x = c(-1.7, 1.7),  
  y = c(-1.9, 1.9)
)


measure_labs <- list(
  vel = 'Velocity',  
  iso = 'Isotropic Strain Rate',  
  ani = 'Anisotropic Strain Rate',
  str = ' Strain Rate' ,
  apstr = 'AP Strain Rate' ,
  mlstr = 'ML Strain Rate',
  x = 'ML Iso Strain Rate',
  y = 'AP Iso Strain Rate'
)

# anterior plots
for (i in measures) {
  limit <- ant_limits[[i]]
  measure_lab <- measure_labs[[i]]
  
    if (i == 'iso') {
      colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
    } else if (i == 'ani') {
      colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'amp')
    } else if (i == 'vel') {
      colour_map = scale_fill_viridis(limits = c(limit[1], limit[2]))
    } else if (i == 'x') {
      colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
    } else if (i == 'y') {
      colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
    } 

  for (j in periods) {
    
    filtered_data <- averaged_zscore %>% 
      filter(direction == 'anterior', measure == i, period.subset == j)
    
    (
      plot <- filtered_data %>% 
        ggplot(aes(x = timepoint, y = distance_aligned, z = mean)) +
        geom_raster(aes(fill = mean)) +
        geom_hline(yintercept = 0, linewidth = 1.5, colour = 'black', alpha = 0.6) +
        geom_hline(aes(yintercept = max(ROI_end_aligned)), linewidth = 1.5, colour = 'black', alpha = 0.6) +
        colour_map +
        scale_x_continuous(expand = c(0,0), n.breaks = 6) +
        scale_y_reverse(expand = expansion(add = c(0, 1)),  n.breaks = 6)  +
        labs(title = '', #measure_lab,
             x = 'Time (s)', 
             y = 'Distance relative to ROI (µm)', 
             fill = 'Z-Score',
             hjust = 0.5 ) +
        theme_bw() +
        theme(text = element_text(size = 28, family = 'Avenir'),
              axis.title = element_blank(),
              axis.line = element_blank(),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              plot.title = element_text(hjust = 0.5),
              legend.key.size = unit(3.5 , 'line')) 
    )
  
    print(plot)  
   filename <- paste0(i, '_col_zscore_ant_', j, '.png')
   filename <- paste(Sys.Date(), filename, sep = "_")
   ggsave(filename, plot = plot, path = 'results/plots/colanal/new/', width = 6, height = 14)

  }
}


# For checking limits
 averaged_zscore %>%
   filter(measure == 'y', direction == 'posterior', timepoint <= 230) %>% 
   summarise(max_mean = max(mean),
             min_mean = min(mean))

post_limits <- list(
  vel = c(-1.8, 4.3),  
  iso = c(-2.5, 3),  
  ani = c(-2.1, 4.7),
  str = c(-1.5, 5),
  apstr = c(-1.5, 3.1),
  mlstr = c(-1.5, 2.2),
  x = c(-3.31, 3.31),  
  y = c(-3.31, 3.31)
)


# posterior plots
for (i in measures) {
  limit <- post_limits[[i]]
  measure_lab <- measure_labs[[i]]
 
   if (i == 'iso') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
  } else if (i == 'ani') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'amp')
  } else if (i == 'vel') {
    colour_map = scale_fill_viridis(limits = c(limit[1], limit[2]))
  } else if (i == 'x') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
  } else if (i == 'y') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
  } 
  
  
  
  for (j in periods) {
    
    filtered_data <- averaged_zscore %>% 
      filter(direction == 'posterior', measure == i, period.subset == j)
    
    (
      plot <- filtered_data %>% 
        ggplot(aes(x = timepoint, y = distance_aligned, z = mean)) +
        geom_raster(aes(fill = mean)) +
        geom_hline(yintercept = 0, linewidth = 1.5, colour = 'black', alpha = 0.6) +
        geom_hline(aes(yintercept = min(ROI_start_aligned)), linewidth = 1.5, colour = 'black', alpha = 0.6) +
        colour_map +
        scale_x_continuous(expand = c(0,0), n.breaks = 6) +
        scale_y_reverse(expand = expansion(add = c(0, 1)),  n.breaks = 6)  +
        labs(title = '', #measure_lab,
             x = 'Timepoint', 
             y = 'Distance [µm]', 
             fill = 'Z-Score',
             hjust = 0.5 ) +
        theme_bw() +
        theme(text = element_text(size = 28, family = 'Avenir'),
              axis.title = element_blank(),
              axis.line = element_blank(),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              plot.title = element_text(hjust = 0.5),
              legend.key.size = unit(3.5 , 'line')) 
        
    )
    
    
    filename <- paste0(i, '_col_zscore_post_', j, '.png')
    filename <- paste(Sys.Date(), filename, sep = "_")
    ggsave(filename, plot = plot, path = 'results/plots/colanal/new/', width = 6, height = 14)
    
  }
}


# For checking ROI position
# averaged_zscore %>%
#   filter(direction == 'posterior', timepoint <= 230) %>%
#   summarize(max_ROI = max(ROI_start_aligned),
#             min_ROI = min(ROI_start_aligned),
#             median_roi = median(ROI_start_aligned))

# ROI plots ------

averaged_roi <- averaged_roi %>% 
  mutate(period.subset = case_when(timepoint >= -85 & timepoint <= -30 ~ 'before',
                                   timepoint >= 0 & timepoint <= 55 ~ 'during',
                                   timepoint >= 125 & timepoint <= 180 ~ 'after')) %>% 
  mutate(period.subset = factor(period.subset, levels = c('before', 'during', 'after')))

averaged_roi_dir <- averaged_roi_dir %>% 
  mutate(period.subset = case_when(timepoint >= -85 & timepoint <= -30 ~ 'before',
                                   timepoint >= 0 & timepoint <= 55 ~ 'during',
                                   timepoint >= 125 & timepoint <= 180 ~ 'after')) %>% 
  mutate(period.subset = factor(period.subset, levels = c('before', 'during', 'after')))

measures <- c('vel', 'iso', 'ani', 'x', 'y')
periods <- c('before', 'during', 'after')

zscore_column %>% 
  filter(ROI == 1) %>% 
  dplyr::filter(timepoint >= -55 & timepoint <= -5) %>% 
  group_by(embryo.id) %>% 
  mutate(distance_aligned = (distance*1000 - ROI_start*1000) / 1000) %>% 
  mutate(distance_aligned = round(distance_aligned, digits = 2)) %>% 
  ungroup() %>% 
  group_by(measure, direction, embryo.id) %>% 
  summarise(mean_zscore = mean(z_score),
            mean_value = mean(value)) %>% 
  pivot_longer(cols = c('mean_zscore', 'mean_value'), names_to = 'stat', values_to = 'value') |>
  ggplot(aes(x = direction, y = value)) +
  geom_boxplot() +
  geom_point()+
  facet_wrap(~measure~stat, scale = 'free_y')

ROI_stat <- zscore_column %>% 
  filter(ROI == 1) %>% 
  dplyr::filter(timepoint >= -55 & timepoint <= -5) %>% 
  group_by(embryo.id) %>% 
  mutate(distance_aligned = (distance*1000 - ROI_start*1000) / 1000) %>% 
  mutate(distance_aligned = round(distance_aligned, digits = 2)) %>% 
  ungroup() %>% 
  group_by(measure, direction, embryo.id) %>% 
  summarise(mean_zscore = mean(z_score),
            mean_value = mean(value)) %>% 
  pivot_longer(cols = c('mean_zscore', 'mean_value'), names_to = 'stat', values_to = 'value') |>
  # mutate(value = value * 60 * 1000 )  %>% 
  group_by(stat, measure) %>% #get_summary_stats() 
  #shapiro_test(value)
  levene_test(value ~ direction) 
  t_test(value ~ direction, var.equal = T) %>% add_significance() %>% add_y_position()

# (plot <- zscore_column %>% 
#   filter(ROI == 1) %>% 
#   dplyr::filter(timepoint >= -55 & timepoint <= -5) %>% 
#   group_by(embryo.id) %>% 
#   mutate(distance_aligned = (distance*1000 - ROI_start*1000) / 1000) %>% 
#   mutate(distance_aligned = round(distance_aligned, digits = 2)) %>% 
#   ungroup() %>% 
#   group_by(measure, direction, embryo.id) %>% 
#   summarise(mean_zscore = mean(z_score),
#             mean_value = mean(value)) %>% 
#   pivot_longer(cols = c('mean_zscore', 'mean_value'), names_to = 'stat', values_to = 'value') |>
#   dplyr::filter(measure %in% c('ani', 'iso', 'vel'), stat == 'mean_zscore') %>% 
#   ggplot(aes(x = direction, y = value)) +
#   geom_boxplot(aes(fill = direction), alpha = 0.2) +
#   geom_point(aes(colour = direction)) +
#   stat_pvalue_manual(ROI_stat %>% dplyr::filter(measure %in% c('ani', 'iso', 'vel'), stat == 'mean_zscore'), hide.ns = F,
#                      label = "{p.signif}") +
#   facet_wrap(~measure, scale = 'free_y') +
#   labs(title = '', #measure_lab,
#        x = '',
#        y = 'Z-score (A.U.)') +
#   theme_classic(base_size = 18) +
#   theme(text = element_text(size = 20, family = 'Avenir'),
#         aspect.ratio = 2,
#         legend.position = 'none',
#         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
#         strip.background = element_blank(),
#         panel.spacing = unit(0.5, 'lines')) 
# )

(plot <- zscore_column %>% 
    filter(ROI == 1) %>% 
    dplyr::filter(timepoint >= -55 & timepoint <= -5) %>% 
    group_by(embryo.id) %>% 
    mutate(distance_aligned = (distance*1000 - ROI_start*1000) / 1000) %>% 
    mutate(distance_aligned = round(distance_aligned, digits = 2)) %>% 
    ungroup() %>% 
    group_by(measure, direction, embryo.id) %>% 
    summarise(mean_zscore = mean(z_score),
              mean_value = mean(value)) %>% 
    pivot_longer(cols = c('mean_zscore', 'mean_value'), names_to = 'stat', values_to = 'value') |>
    dplyr::filter(measure == 'iso', stat == 'mean_zscore') %>% 
    ggplot(aes(x = direction, y = value)) +
    geom_boxplot(aes(fill = direction), alpha = 0.2) +
    geom_point(aes(colour = direction)) +
    stat_pvalue_manual(ROI_stat %>% dplyr::filter(measure == 'iso', stat == 'mean_zscore'), hide.ns = F,
                       label = "{p.signif}") +
    labs(title = '', #measure_lab,
         x = '',
         y = 'Isotropic strain rate
z-score (A.U.)') +
    theme_classic(base_size = 18) +
    theme(text = element_text(size = 20, family = 'Avenir'),
          aspect.ratio = 2,
          legend.position = 'none',
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) 
)

filename <- paste0('results/plots/paper/', Sys.Date(), '_ROI_before_iso_stats.svg')
svglite(filename, width = 6, height = 5)
plot
dev.off()

(plot <- zscore_column %>% 
    filter(ROI == 1) %>% 
    dplyr::filter(timepoint >= -55 & timepoint <= -5) %>% 
    group_by(embryo.id) %>% 
    mutate(distance_aligned = (distance*1000 - ROI_start*1000) / 1000) %>% 
    mutate(distance_aligned = round(distance_aligned, digits = 2)) %>% 
    ungroup() %>% 
    group_by(measure, direction, embryo.id) %>% 
    summarise(mean_zscore = mean(z_score),
              mean_value = mean(value)) %>% 
    pivot_longer(cols = c('mean_zscore', 'mean_value'), names_to = 'stat', values_to = 'value') |>
    dplyr::filter(measure %in% c('ani', 'iso'), stat == 'mean_zscore') %>% 
    ggplot(aes(x = direction, y = value)) +
    geom_boxplot(aes(fill = direction), alpha = 0.2) +
    geom_point(aes(colour = direction)) +
    stat_pvalue_manual(ROI_stat %>% dplyr::filter(measure %in% c('ani', 'iso'), stat == 'mean_zscore'), hide.ns = F,
                       label = "{p.signif}") +
    facet_wrap(~measure, scale = 'free_y') +
    labs(title = '', #measure_lab,
         x = '',
         y = 'Z-score (A.U.)') +
    theme_classic(base_size = 18) +
    theme(text = element_text(size = 20, family = 'Avenir'),
          aspect.ratio = 2,
          legend.position = 'none',
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) 
)



filename <- paste0('results/plots/paper/', Sys.Date(), '_ROI_before_stats_novel.svg')
svglite(filename, width = 5, height = 7)
plot
dev.off()

# for checking limits
averaged_roi %>% 
  filter(measure == 'y', timepoint <= 230, distance_aligned < 33) %>%
  summarise(max_mean = max(mean),
            min_mean = min(mean))

roi_limits <- list(
  vel = c(-0.8, 3.6),  
  iso = c(-1.4, 1.4),  
  ani = c(-0.6, 3.1),
  str = c(-0.6, 2.7),
  apstr = c(-0.5, 3.2),
  mlstr = c(-0.7, 1.9),
  x = c(-2.75, 2.75),  
  y = c(-2.75, 2.75)
)


# ROI plots
# anterior
for (i in measures) {
  limit <- roi_limits[[i]]
  measure_lab <- measure_labs[[i]]
  
  if (i == 'iso') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
  } else if (i == 'ani') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'amp')
  } else if (i == 'vel') {
    colour_map = scale_fill_viridis(limits = c(limit[1], limit[2]))
  } else if (i == 'x') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
  } else if (i == 'y') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
  } 
  
  for (j in periods) {
    
    filtered_data <- averaged_roi_dir %>% 
      filter(direction == 'anterior', measure == i, period.subset == j, distance_aligned <= 33)
    
    (
      plot <- filtered_data %>% 
        ggplot(aes(x = timepoint, y = distance_aligned, z = mean)) +
        geom_raster(aes(fill = mean)) +
        colour_map +
        scale_x_continuous(expand = c(0,0), n.breaks = 6) +
        scale_y_reverse(expand = expansion(add = c(0, 0)),  n.breaks = 6)  +
        labs(title = '', #measure_lab,
             x = 'Timepoint', 
             y = 'Distance [µm]', 
             fill = 'Z-Score',
             hjust = 0.5 ) +
        theme_bw() +
        theme(text = element_text(size = 28, family = 'Avenir'),
              axis.title = element_blank(),
              axis.line = element_blank(),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              plot.title = element_text(hjust = 0.5),
              legend.key.size = unit(3.5 , 'line')) 
    )
    
    # print(plot)
    filename <- paste0(i, '_col_zscore_ant_', j, '_ROI.png')
    filename <- paste(Sys.Date(), filename, sep = "_")
    ggsave(filename, plot = plot, path = 'results/plots/colanal/roi/', width = 6, height = 5)

  }
}

# posterior
for (i in measures) {
  limit <- roi_limits[[i]]
  measure_lab <- measure_labs[[i]]
  
  if (i == 'iso') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
  } else if (i == 'ani') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'amp')
  } else if (i == 'vel') {
    colour_map = scale_fill_viridis(limits = c(limit[1], limit[2]))
  } else if (i == 'x') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
  } else if (i == 'y') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
  } 
  
  for (j in periods) {
    
    filtered_data <- averaged_roi_dir %>% 
      filter(direction == 'posterior', measure == i, period.subset == j, distance_aligned <= 33)
    
    (
      plot <- filtered_data %>% 
        ggplot(aes(x = timepoint, y = distance_aligned, z = mean)) +
        geom_raster(aes(fill = mean)) +
        colour_map +
        scale_x_continuous(expand = c(0,0), n.breaks = 6) +
        scale_y_reverse(expand = expansion(add = c(0, 0)),  n.breaks = 6)  +
        labs(title = '', #measure_lab,
             x = 'Timepoint', 
             y = 'Distance [µm]', 
             fill = 'Z-Score',
             hjust = 0.5 ) +
        theme_bw() +
        theme(text = element_text(size = 28, family = 'Avenir'),
              axis.title = element_blank(),
              axis.line = element_blank(),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              plot.title = element_text(hjust = 0.5),
              legend.key.size = unit(3.5 , 'line')) 
    )
    
    # print(plot)
    filename <- paste0(i, '_col_zscore_post_', j, '_ROI.png')
    filename <- paste(Sys.Date(), filename, sep = "_")
    ggsave(filename, plot = plot, path = 'results/plots/colanal/roi/', width = 6, height = 5)

  }
}

# combined

roi_limits <- list(
  vel = c(-0.8, 3.6),  
  iso = c(-1.4, 1.4),  
  ani = c(-0.6, 3.1),
  str = c(-0.6, 2.7),
  apstr = c(-0.5, 3.2),
  mlstr = c(-0.7, 1.9),
  x = c(-1.75, 1.75),  
  y = c(-1.75, 1.75)
)


for (i in measures) {
  limit <- roi_limits[[i]]
  measure_lab <- measure_labs[[i]]
  
  if (i == 'iso') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
  } else if (i == 'ani') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'amp')
  } else if (i == 'vel') {
    colour_map = scale_fill_viridis(limits = c(limit[1], limit[2]))
  } else if (i == 'x') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
  } else if (i == 'y') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
  } 
  
  for (j in periods) {
    
    filtered_data <- averaged_roi %>% 
      filter(measure == i, period.subset == j, distance_aligned <= 33)
    
    (
      plot <- filtered_data %>% 
        ggplot(aes(x = timepoint, y = distance_aligned, z = mean)) +
        geom_raster(aes(fill = mean)) +
        colour_map +
        scale_x_continuous(expand = c(0,0), n.breaks = 6) +
        scale_y_reverse(expand = expansion(add = c(0, 0)),  n.breaks = 6)  +
        labs(title = '', #measure_lab,
             x = 'Timepoint', 
             y = 'Distance [µm]', 
             fill = 'Z-Score',
             hjust = 0.5 ) +
        theme_bw() +
        theme(text = element_text(size = 28, family = 'Avenir'),
              axis.title = element_blank(),
              axis.line = element_blank(),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              plot.title = element_text(hjust = 0.5),
              legend.key.size = unit(3.5 , 'line')) 
    )
    
    filename <- paste0(i, '_col_zscore_all_', j, '_ROI-1.png')
    filename <- paste(Sys.Date(), filename, sep = "_")
    ggsave(filename, plot = plot, path = 'results/plots/colanal/roi/', width = 6, height = 5)
    
  }
}

# histogram of values -----

(histo <- zscore_column %>%
  #filter(ROI == 1) %>%
  mutate(period.subset = case_when(timepoint >= -120 & timepoint <= -5 ~ 'before',
                                   timepoint >= 0 & timepoint <= 120 ~ 'during',
                                   timepoint >= 125 & timepoint <= 240 ~ 'after')) %>% 
  mutate(period.subset = factor(period.subset, levels = c('before', 'during', 'after'))) %>% 
  group_by(embryo.id) %>%
  # mutate(distance_aligned = (distance*1000 - ROI_start*1000) / 1000) %>%
  # mutate(distance_aligned = round(distance_aligned, digits = 2)) %>%
  ungroup() %>%
  pivot_longer(cols = c('z_score', 'value'), names_to = 'stat', values_to = 'value') %>%
  filter(stat == 'z_score', measure %in% c('ani', 'iso', 'vel')) %>% 
  ggplot(aes(x = value, colour = direction,  fill = direction)) +
  geom_histogram(aes(y = after_stat(c(
    count[group==1]/sum(count[group==1]),
    count[group==2]/sum(count[group==2]))*100)),
    binwidth = 0.5, position = "identity", alpha = 0.4) +
  #geom_density(aes(y = after_stat(density)), alpha = 0.5) +
  facet_grid(rows = vars(period.subset), cols = vars(measure), scale = 'free_y') +
  labs(x = "Value", y = "%") +
  theme_minimal()
)


temp <- zscore_column %>%
  #filter(ROI == 1) %>%
  mutate(period.subset = case_when(timepoint >= -120 & timepoint <= -5 ~ 'before',
                                   timepoint >= 0 & timepoint <= 120 ~ 'during',
                                   timepoint >= 125 & timepoint <= 240 ~ 'after')) %>% 
  mutate(period.subset = factor(period.subset, levels = c('before', 'during', 'after'))) %>% 
  group_by(embryo.id) %>%
  # mutate(distance_aligned = (distance*1000 - ROI_start*1000) / 1000) %>%
  # mutate(distance_aligned = round(distance_aligned, digits = 2)) %>%
  ungroup() %>%
  pivot_longer(cols = c('z_score', 'value'), names_to = 'stat', values_to = 'value') %>%
  filter(stat == 'z_score', measure %in% c('ani', 'iso', 'vel')) |>
  select(value, direction, period.subset, measure)

temp |>
  mutate(value = round(value, digits = 0)) |> # Discretising by rounding to nearest whole number
  group_by(direction, period.subset, measure, value) |>
  summarise(N = n()) |>
  group_by(direction, period.subset, measure) |>
  mutate(percent = 100 * N / sum(N)) |>
  ggplot(aes(x = value, y = percent, colour = direction,  fill = direction)) +
  geom_bar(stat = 'identity', position = 'identity', alpha = 0.1) +
  #geom_density(aes(y = after_stat(density)), alpha = 0.5) +
  facet_grid(rows = vars(period.subset), cols = vars(measure), scale = 'free_y') +
  labs(x = "Value", y = "%") +
  theme_minimal()

# infotheo::discretize


zscore_column %>%
  filter(ROI == 1) %>%
  mutate(period.subset = case_when(timepoint >= -85 & timepoint <= -30 ~ 'before',
                                   timepoint >= 0 & timepoint <= 55 ~ 'during',
                                   timepoint >= 125 & timepoint <= 180 ~ 'after')) %>%
  mutate(period.subset = factor(period.subset, levels = c('before', 'during', 'after'))) %>%
  drop_na() %>% 
  group_by(embryo.id) %>%
  mutate(distance_aligned = (distance*1000 - ROI_start*1000) / 1000) %>%
  mutate(distance_aligned = round(distance_aligned, digits = 2)) %>%
  ungroup() %>%
  pivot_longer(cols = c('z_score', 'value'), names_to = 'stat', values_to = 'value') %>%
  filter(stat == 'z_score', measure %in% c('ani', 'iso', 'vel')) %>%
  ggplot(aes(x = value, fill = period.subset)) +
  #geom_histogram(aes(y = after_stat(count) / sum(after_stat(count)) * 100), binwidth = 0.5, position = "identity", alpha = 0.4) +
  geom_density(aes(y=after_stat(density)), alpha = 0.4)+
  facet_wrap(~measure~direction) +
  #facet_grid(rows = vars(period.subset), cols = vars(measure), scale = 'free_y') +
  labs(x = "Value", y = "Percentage (%)") +
  theme_minimal()

# ROI line plot ----

averaged_roi_line <-  averaged_roi %>% 
  group_by(measure, timepoint) %>% 
  filter(n_distinct(distance_aligned) > 3) %>% 
  summarise(mean = mean(mean)) %>% 
  ungroup()

averaged_roi_dir_line <-   averaged_roi_dir %>% 
  group_by(measure, timepoint, direction) %>% 
  filter(n_distinct(distance_aligned) > 3) %>% 
  summarise(mean = mean(mean)) %>% 
  ungroup()

averaged_roi_dir_line %>% 
  filter(measure == 'x', direction == "posterior") %>% 
  ggplot(aes(x = timepoint, y = mean)) +
  geom_line() +
  colour_map +
  scale_x_continuous(expand = c(0,0), n.breaks = 6) +
  scale_y_reverse(expand = expansion(add = c(0, 0)),  n.breaks = 6)  +
  labs(title = '', #measure_lab,
       x = 'Timepoint', 
       y = 'Distance [µm]', 
       fill = 'Z-Score',
       hjust = 0.5 ) +
  theme_bw() +
  theme(text = element_text(size = 28, family = 'Avenir'),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(3.5 , 'line')) 


# Full TL heatmap -----

# Anterior
for (i in measures) {
  limit <- limits[[i]]
  measure_lab <- measure_labs[[i]]
  
  if (i %in% c('iso', 'x', 'y')) {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
  } else if (i == 'ani') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'amp')
  } else if (i == 'vel') {
    colour_map = scale_fill_viridis(limits = c(limit[1], limit[2]))
  }
  
  filtered_data <- averaged_zscore %>% 
    filter(direction == 'anterior', measure == i)

  (
    plot <- filtered_data %>% 
      filter(timepoint <= 230) %>% 
      ggplot(aes(x = timepoint, y = distance_aligned, z = mean)) +
      geom_raster(aes(fill = mean)) +
      # geom_linerange(aes(y = 102, xmin = -121, xmax = 0),
      #                color = '#009245', linewidth = 5, alpha = 0.02) +
      # geom_linerange(aes(y = -102, xmin = 0, xmax = 120),
      #                color = '#d4145a', linewidth = 5, alpha = 0.02) +
      # geom_linerange(aes(y = -102, xmin = 120, xmax = 231),
      #                color = '#009245', linewidth = 5, alpha = 0.02) +
      geom_vline(xintercept = 0, linewidth = 1, linetype = 'dotted', colour = 'white') +
      geom_vline(xintercept = 120, linewidth = 1, linetype = 'dotted', colour = 'white') +
      geom_hline(yintercept = 0, linewidth = 1.5, colour = 'black', alpha = 0.6) +
      geom_hline(aes(yintercept = max(ROI_end_aligned)), linewidth = 1.5, colour = 'black', alpha = 0.6) +
      colour_map +
      scale_x_continuous(expand = c(0,0), n.breaks = 6) +
      scale_y_reverse(expand = expansion(add = c(0, 1)),  n.breaks = 6) +
      labs(title = measure_lab,
           x = 'Timepoint', 
           y = 'Distance [µm]', 
           fill = 'Z-Score',
           hjust = 0.5 ) +
      theme_bw() +
      theme(text = element_text(size = 28, family = 'Avenir'),
            axis.title = element_blank(),
            axis.line = element_blank(),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            plot.title = element_text(hjust = 0.5),
            legend.key.size = unit(3.5 , 'line')) 
  )
  
  print(plot)
  #filename <- paste0(i, '_col_zscore_ant_.png')
  #ggsave(filename, plot = plot, path = 'results/plots/colanal/heatmap/full/', width = 15, height = 12)
}

# Posterior
for (i in measures) {
  limit <- post_limits[[i]]
  measure_lab <- measure_labs[[i]]
  
  if (i == 'iso') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
  } else if (i == 'ani') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'amp')
  } else if (i == 'vel') {
    colour_map = scale_fill_viridis(limits = c(limit[1], limit[2]))
  }
  
  filtered_data <- averaged_zscore %>% 
    filter(direction == 'posterior', measure == i)

  (
    plot <- filtered_data %>% 
      filter(timepoint <= 230) %>% 
      ggplot(aes(x = timepoint, y = distance_aligned, z = mean)) +
      geom_raster(aes(fill = mean)) +
      # geom_linerange(aes(y = 102, xmin = -121, xmax = 0),
      #                color = '#009245', linewidth = 5, alpha = 0.02) +
      # geom_linerange(aes(y = -102, xmin = 0, xmax = 120),
      #                color = '#d4145a', linewidth = 5, alpha = 0.02) +
      # geom_linerange(aes(y = -102, xmin = 120, xmax = 231),
      #                color = '#009245', linewidth = 5, alpha = 0.02) +
      geom_vline(xintercept = 0, linewidth = 1, linetype = 'dotted', colour = 'white') +
      geom_vline(xintercept = 120, linewidth = 1, linetype = 'dotted', colour = 'white') +
      geom_hline(yintercept = 0, linewidth = 1.5, colour = 'black', alpha = 0.6) +
      geom_hline(aes(yintercept = min(ROI_start_aligned)), linewidth = 1.5, colour = 'black', alpha = 0.6) +
      colour_map +
      scale_x_continuous(expand = c(0,0), n.breaks = 6) +
      scale_y_reverse(expand = expansion(add = c(0, 1)),  n.breaks = 6) +
      labs(title = measure_lab,
           x = 'Timepoint', 
           y = 'Distance [µm]', 
           fill = 'Z-Score',
           hjust = 0.5 ) +
      theme_bw() +
      theme(text = element_text(size = 28, family = 'Avenir'),
            axis.title = element_blank(),
            axis.line = element_blank(),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            plot.title = element_text(hjust = 0.5),
            legend.key.size = unit(3.5 , 'line')) 
  )
  
  print(plot)
  # filename <- paste0(i, '_col_zscore_post_.png')
  # ggsave(filename, plot = plot, path = 'results/plots/colanal/heatmap/full/', width = 15, height = 12)
}

# ROI plots ------

 # for checking limits
 # averaged_roi %>%
 #   filter(measure == 'mlstr', timepoint <= 230) %>% 
 #   summarise(max_mean = max(mean),
 #             min_mean = min(mean))
 
 roi_limits <- list(
   vel = c(-0.8, 3.6),  
   iso = c(-1.4, 1.4),  
   ani = c(-0.6, 3.1),
   str = c(-0.6, 2.7),
   apstr = c(-0.5, 3.2),
   mlstr = c(-0.7, 1.9),
   x = c(-1.5, 1.6),  
   y = c(-1.5, 1.6)
 )
 
  
for (i in measures) {
  limit <- roi_limits[[i]]
  measure_lab <- measure_labs[[i]]
  
  if (i == 'iso') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
  } else if (i == 'ani') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'amp')
  } else if (i == 'vel') {
    colour_map = scale_fill_viridis(limits = c(limit[1], limit[2]))
  } else if (i == 'x') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
  } else if (i == 'y') {
    colour_map = cmocean::scale_fill_cmocean(limits = c(limit[1], limit[2]), name = 'balance')
  } 
  
    
    filtered_data <- averaged_roi %>% 
      filter(measure == i, distance_aligned <= 33)
    
    (
      plot <- filtered_data %>% 
        ggplot(aes(x = timepoint, y = distance_aligned, z = mean)) +
        geom_raster(aes(fill = mean)) +
       # geom_hline(yintercept = 0, linewidth = 1.5, colour = 'black', alpha = 0.6) +
      #  geom_hline(aes(yintercept = max(ROI_end_aligned)), linewidth = 1.5, colour = 'black', alpha = 0.6) +
        colour_map +
        scale_x_continuous(expand = c(0,0), n.breaks = 6) +
        scale_y_reverse(expand = expansion(add = c(0, 0)),  n.breaks = 6)  +
        labs(title = '', #measure_lab,
             x = 'Timepoint', 
             y = 'Distance [µm]', 
             fill = 'Z-Score',
             hjust = 0.5 ) +
        theme_bw() +
        theme(text = element_text(size = 28, family = 'Avenir'),
              axis.title = element_blank(),
              axis.line = element_blank(),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              plot.title = element_text(hjust = 0.5),
              legend.key.size = unit(3.5 , 'line')) 
    )
    
    print(plot)
    
    filename <- paste0(i, '_col_zscore_ROI_ave.png')
    filename <- paste(Sys.Date(), filename, sep = "_")
    ggsave(filename, plot = plot, path = 'results/plots/colanal/new/', width = 18, height = 5)
    
}



# Supplementary figure of lineplots of all timepoints -------



(iso_col_during <- averaged_zscore %>% 
  filter(timepoint >= 0, timepoint <= 120, measure == 'iso', direction == 'anterior') %>% 
  ggplot(aes(x = distance_aligned, y = mean, color = timepoint, group = timepoint)) +
  geom_rect(aes(xmin = ROI_start_aligned, xmax = (ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.2, fill = 'grey', colour = 'grey') +
  annotate('text', x = 20, y = -1.5, label = 'ROI', size = 6) +
  geom_hline(yintercept = 0, linetype = 'dashed', linewidth = 0.5) +
  geom_line(linewidth = 0.8) +
  #scale_color_gradient() + 
   scale_colour_cmocean(name = 'thermal')+
   theme_classic(base_size = 18) +
   theme(aspect.ratio = 0.4,
         text = element_text(size = 20, family = 'Avenir'),
         strip.background = element_blank(),
         panel.spacing = unit(0.5, 'lines')) +
  xlab('Distance (µm)') +
  ylab(expression(paste('Isotropic Strain Rate
          z-score'))))



(iso_col_before <- averaged_zscore %>% 
  filter(timepoint<=0, measure == 'iso', direction == 'anterior') %>% 
  ggplot(aes(x = distance_aligned, y = mean, color = timepoint, group = timepoint)) +
  geom_rect(aes(xmin = ROI_start_aligned, xmax = (ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.2, fill = 'grey', colour = 'grey') +
  annotate('text', x = 20, y = -1.5, label = 'ROI', size = 6) +
  geom_hline(yintercept = 0, linetype = 'dashed', linewidth = 0.5) +
  geom_line(linewidth=0.8) +
    scale_colour_cmocean(name = 'thermal')+
    theme_classic(base_size = 18) +
    theme(aspect.ratio = 0.4,
          text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance (µm)') +
  ylab(expression(paste('Isotropic Strain Rate
            z-score'))))

(iso_col_after <- averaged_zscore %>% 
    filter(timepoint >=120, measure == 'iso', direction == 'anterior') %>% 
    ggplot(aes(x = distance_aligned, y = mean, color = timepoint, group = timepoint)) +
    geom_rect(aes(xmin = ROI_start_aligned, xmax = (ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.2, fill = 'grey', colour = 'grey') +
    annotate('text', x = 20, y = -1.5, label = 'ROI', size = 6) +
    geom_hline(yintercept = 0, linetype = 'dashed', linewidth = 0.5) +
    geom_line(linewidth=0.8) +
    scale_colour_cmocean(name = 'thermal')+
    theme_classic(base_size = 18) +
    theme(aspect.ratio = 0.4,
          text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance (µm)') +
    ylab(expression(paste('Isotropic Strain Rate
            z-score'))))



#ggsave(filename, plot = iso_col_before , path = 'results/plots/', width = 10, height = 4)
filename <- paste0('results/plots/paper/', Sys.Date(), '_Average_iso_col_zscore_before.svg')
svglite(filename, width = 9, height = 6)
iso_col_before
dev.off()

#ggsave(filename, plot = iso_col_during , path = 'results/plots/', width = 10, height = 4)
filename <- paste0('results/plots/paper/', Sys.Date(), '_Average_iso_col_zscore_during.svg')
svglite(filename, width = 9, height = 6)
iso_col_during
dev.off()

filename <- paste0('results/plots/paper/', Sys.Date(), '_Average_iso_col_zscore_after.svg')
svglite(filename, width = 9, height = 6)
iso_col_after
dev.off()

(ani_col_during <- averaged_zscore %>% 
    filter(timepoint >= 0, timepoint <= 50, measure == 'ani', direction == 'anterior') %>% 
    ggplot(aes(x = distance_aligned, y = mean, color = timepoint, group = timepoint)) +
    geom_rect(aes(xmin = ROI_start_aligned, xmax = (ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.2, fill = 'grey', colour = 'grey') +
    annotate('text', x = 20, y = -1.5, label = 'ROI', size = 6) +
    geom_hline(yintercept = 0, linetype = 'dashed', linewidth = 0.5) +
    geom_line(linewidth = 0.8) +
    scale_color_viridis(option = 'turbo') + 
    theme_classic(base_size = 18) +
    theme(text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance (µm)') +
    ylab(expression(paste('Average Anisotropic Strain z-score'))))

(ani_col_before <- averaged_zscore %>% 
    filter(timepoint >= -80, timepoint <=-30, measure == 'ani', direction == 'anterior') %>% 
    ggplot(aes(x = distance_aligned, y = mean, color = timepoint, group = timepoint)) +
    geom_rect(aes(xmin = ROI_start_aligned, xmax = (ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.2, fill = 'grey', colour = 'grey') +
    annotate('text', x = 20, y = -1.5, label = 'ROI', size = 6) +
    geom_hline(yintercept = 0, linetype = 'dashed', linewidth = 0.5) +
    geom_line(linewidth=0.8) +
    scale_color_viridis(option = 'turbo') + 
    theme_classic(base_size = 18) +
    theme(text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance (µm)') +
    ylab(expression(paste('Average Anisotropic Strain z-score'))))


filename <- paste(Sys.Date(), 'Average_ani_col_zscore_before.png', sep = '_')
ggsave(filename, plot = ani_col_before , path = 'results/plots/', width = 10, height = 4)
filename <-  paste(Sys.Date(), 'Average_ani_col_zscore_during.png', sep = '_')
ggsave(filename, plot = ani_col_during , path = 'results/plots/', width = 10, height = 4)



averaged_unnorm %>% 
  filter(timepoint > 0, timepoint <=50, measure == 'iso', direction == 'anterior') %>% 
  ggplot(aes(x = distance_aligned, y = mean, color = timepoint, group = timepoint)) +
  # geom_rect(aes( xmin = 0, xmax = (ROI_end - ROI_start), ymin = -Inf, ymax = Inf), alpha = 0.5, fill = '#fbb03b', colour = '#fbb03b') +
  geom_line() +
  geom_vline(xintercept =  0) +
  scale_color_viridis(option = 'turbo') + 
  theme_classic(base_size = 18) +
  theme(text = element_text(size = 20, family = 'Avenir'),
        strip.background = element_blank(),
        panel.spacing = unit(0.5, 'lines')) +
  xlab('Distance (µm)') +
  ylab(expression(paste('Normalised Velocity')))

# normalised_column_1 %>% 
#   filter(measure == 'vel', timepoint >=0, timepoint <= 120) %>% 
#   group_by(direction, distance_aligned, embryo.id) %>% 
#   slice_max(value, n = 1) %>% 
#   select(timepoint, peak_timepoint) %>% 
#   group_by(direction, distance_aligned) %>% 
#   summarise(mu = mean(value)) %>% 
#   ggplot(aes(x = distance_aligned, y = mu, colour = direction)) +
#   geom_line()
# 
# averaged_normalised_peak <- normalised_column_2 %>% 
#   group_by(embryo.id) %>%
#   filter(timepoint == peak_timepoint[measure == 'vel']) %>%  
#   group_by(measure, direction, distance_aligned) %>% 
#   summarise(mean = mean(norm_value),
#             ROI_start = median(ROI_start),
#             ROI_end = median(ROI_end)) %>% 
#   ungroup()

# plotting during the peak -----
averaged_zscore_peak <- zscore_column %>% 
  group_by(embryo.id) %>%
  filter(timepoint == peak_timepoint[measure == 'vel']) %>% 
  ungroup() %>%  
  filter(n_distinct(embryo.id) > 7) %>% 
  group_by(direction) %>% 
  mutate(ROI_end_aligned = mean(ROI_end_aligned)) %>% 
  group_by(measure, direction, distance_aligned) %>% 
  summarise(mean = mean(z_score),
            sem = sd(z_score) / sqrt(n()),
            ROI_start_aligned = mean(ROI_start_aligned),
            ROI_end_aligned = mean(ROI_end_aligned)) %>% 
  drop_na() %>% 
  ungroup() 

averaged_zscore_peak %>%
  group_by(measure) %>% 
  filter(mean < 0) %>% 
  slice_head(n = 1)

averaged_zscore_peak %>%
  group_by(measure) %>% 
  filter(mean > 0) %>% 
  slice_tail(n = 1)

averaged_unnorm_peak <- aligned_column %>% 
  left_join(peak_value, by = join_by(embryo.id, measure)) %>% 
  group_by(embryo.id) %>%
  filter(timepoint == peak_timepoint[measure == 'vel']) %>% 
  ungroup() %>%  
  filter(n_distinct(embryo.id) > 7) %>% 
  group_by(measure, direction, distance_aligned) %>% 
  summarise(mean = mean(value),
            sem = sd(value) / sqrt(n()),
            ROI_start_aligned = median(ROI_start_aligned),
            ROI_end_aligned = median(ROI_end_aligned)) %>% 
  drop_na() %>% 
  ungroup() 

(col_peak <- averaged_unnorm_peak %>% 
    filter(measure %in% c('ani', 'iso')) %>% 
    filter(direction == 'anterior', distance_aligned > -115 , distance_aligned < 150) %>% 
    ggplot(aes(x = distance_aligned, y = mean)) +
    geom_rect(aes(xmin = ROI_start_aligned, xmax = (ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.2, fill = 'grey', colour = 'grey') +
    geom_hline(yintercept = 0, linetype = 'dashed', linewidth = 0.5) +
    geom_ribbon(aes(ymin = (mean - sem), ymax = (mean + sem), fill = measure), alpha = 0.3) +
    annotate('text', x = 20, y = -0.0001, label = 'ROI', size = 6, family = 'Avenir') +
    geom_line(aes(colour = measure), linewidth = 1.5) +
    theme_classic(base_size = 18) +
    theme(text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines'),
          aspect.ratio = 0.4) +
    xlab('Distance (µm)') +
    ylab(
      expression(paste('Peak strain rate (', s^-1, ')')))
)


filename <- paste0('results/plots/colanal/paper/', Sys.Date(), '_Average_col_peak_ant.svg')
svglite(filename, width = 10, height = 5)
col_peak
dev.off()


(correlation <- averaged_unnorm_peak %>% 
    filter(direction == 'anterior', distance_aligned > -115 , distance_aligned < 150) %>% 
    pivot_wider(names_from = measure, values_from = c(mean, sem)) %>% 
    ggplot(aes(x = mean_ani, y = mean_iso)) +
    #geom_rect(aes(xmin = ROI_start_aligned, xmax = (ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.2, fill = 'grey', colour = 'grey') +
    #geom_ribbon(aes(ymin = (mean - sem), ymax = (mean + sem), fill = measure), alpha = 0.3) +
    #annotate('text', x = 20, y = -0.0001, label = 'ROI', size = 6) +
    geom_path() +
    geom_point(aes(colour = distance_aligned)) +
    scale_colour_viridis(option = 'turbo')+
    theme_classic(base_size = 18) +
    theme(text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Ani strain') +
    ylab(
      'Iso strain rate (s-1)'))

(correlation <- zscore_column %>% 
    filter(direction == 'anterior', distance_aligned > -115 , distance_aligned < 150) %>% 
    select(embryo.id, timepoint, distance_aligned, z_score, value, measure) %>% 
    pivot_wider(names_from = measure, values_from = c(z_score, value)) %>% 
    ggplot(aes(x = z_score_ani, y = abs(z_score_iso))) +
    #geom_rect(aes(xmin = ROI_start_aligned, xmax = (ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.2, fill = 'grey', colour = 'grey') +
    #geom_ribbon(aes(ymin = (mean - sem), ymax = (mean + sem), fill = measure), alpha = 0.3) +
    #annotate('text', x = 20, y = -0.0001, label = 'ROI', size = 6) +
    geom_path() +
    facet_wrap(~embryo.id)+
    geom_point(aes(colour = timepoint)) +
    scale_colour_viridis(option = 'turbo')+
    theme_classic(base_size = 18) +
    theme(text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Ani strain') +
    ylab(
      'Iso strain rate (s-1)'))



(col_peak <- averaged_unnorm_peak %>% 
    filter(measure %in% c('ani', 'iso')) %>% 
    filter(direction == 'posterior', distance_aligned > -115 , distance_aligned < 150) %>% 
    ggplot(aes(x = distance_aligned, y = mean)) +
    geom_rect(aes(xmin = ROI_start_aligned, xmax = (ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.2, fill = 'grey', colour = 'grey') +
    geom_ribbon(aes(ymin = (mean - sem), ymax = (mean + sem), fill = measure), alpha = 0.3) +
    annotate('text', x = 18, y = -0.0001, label = 'ROI', size = 6) +
    geom_line(aes(colour = measure), linewidth = 1.5) +
    theme_classic(base_size = 18) +
    theme(aspect.ratio = 0.4,
          text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          legend.position = 'none',
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance (µm)') +
    ylab(
      'Peak strain rate (s-1)'))


filename <- paste0('results/plots/colanal/paper/', Sys.Date(), '_Average_col_peak_post.svg')
svglite(filename, width = 8, height = 5)
col_peak
dev.off()


(iso_col_peak <- averaged_zscore_peak %>% 
  filter(measure == 'iso', direction == 'anterior', distance_aligned > -115 , distance_aligned < 150) %>% 
  ggplot(aes(x = distance_aligned, y = mean)) +
  geom_rect(aes(xmin = ROI_start_aligned, xmax = (ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.2, fill = 'grey', colour = 'grey') +
  geom_ribbon(aes(ymin = (mean - sem), ymax = (mean + sem)), fill = 'black', alpha = 0.3) +
  annotate('text', x = 20, y = -1.5, label = 'ROI', size = 6, family = 'avenir') +
  geom_line(linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = 'dashed', linewidth = 0.5) +
    theme_classic(base_size = 18) +
    theme(aspect.ratio = 0.4,
          text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance (µm)') +
  ylab(
'Peak isotropic strain rate 
z-score'))


(ani_col_peak <- averaged_zscore_peak %>% 
    filter(measure == 'ani', direction == 'anterior', distance_aligned > -115 , distance_aligned < 150) %>% 
    ggplot(aes(x = distance_aligned, y = mean)) +
    geom_rect(aes(xmin = ROI_start_aligned, xmax = (ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.2, fill = 'grey', colour = 'grey') +
    geom_ribbon(aes(ymin = (mean - sem), ymax = (mean + sem)), fill = 'black', alpha = 0.3) +
    annotate('text', x = 20, y = -1.5, label = 'ROI', size = 6) +
    geom_line(linewidth = 1.5) +
    geom_hline(yintercept = 0, linetype = 'dashed', linewidth = 0.5) +
    theme_classic(base_size = 18) +
    theme(aspect.ratio = 0.4,
          text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance (µm)') +
    ylab(
'Peak anisotropic strain rate
z-score'))

(vel_col_peak <- averaged_zscore_peak %>% 
    filter(measure == 'vel',  distance_aligned > -115 , distance_aligned < 150) %>% 
    mutate(direction = case_match(direction, 
    'anterior' ~ 'anteriorly directed',
    'posterior' ~ 'posteriorly directed')) %>% 
    ggplot(aes(x = distance_aligned, y = mean)) +
    geom_rect(aes(xmin = ROI_start_aligned, xmax = (ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.2, fill = 'grey', colour = 'grey') +
    geom_ribbon(aes(ymin = (mean - sem), ymax = (mean + sem)), fill = 'black', alpha = 0.3) +
    annotate('text', x = 18, y = -1.25, label = 'ROI', size = 6) +
    geom_line(linewidth = 1.5) +
    geom_hline(yintercept = 0, linetype = 'dashed', linewidth = 0.5) +
    theme_classic(base_size = 18) +
    facet_grid(vars(direction)) +
    theme(aspect.ratio = 0.4,
          text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance (µm)') +
    ylab(
'Peak velocity 
z-score'))


(vel_col_peak_post <- averaged_zscore_peak %>% 
    filter(measure == 'vel', direction == 'posterior', distance_aligned > -115 , distance_aligned < 150) %>% 
    ggplot(aes(x = distance_aligned, y = mean)) +
    geom_rect(aes(xmin = ROI_start_aligned, xmax = (ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.2, fill = 'grey', colour = 'grey') +
    geom_ribbon(aes(ymin = (mean - sem), ymax = (mean + sem)), fill = 'black', alpha = 0.3) +
    annotate('text', x = 18, y = -1.5, label = 'ROI', size = 6) +
    geom_line(linewidth = 1.5) +
    geom_hline(yintercept = 0, linetype = 'dashed', linewidth = 0.5) +
    theme_classic(base_size = 18) +
    ylim(-2, 7) +
    theme(aspect.ratio = 0.4,
          text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance (µm)') +
    ylab(
      'Peak velocity 
z-score'))

# filename <- paste(Sys.Date(), 'Average_ant_ani_col_zscore_peak.png', sep = '_')
# ggsave(filename, plot = ani_col_peak , path = 'results/plots/colanal/paper/', width = 10, height = 4)
# filename <- paste(Sys.Date(), 'Average_ant_iso_col_zscore_peak.png', sep = '_')
# ggsave(filename, plot = iso_col_peak , path = 'results/plots/colanal/paper/', width = 10, height = 4)
# filename <- paste(Sys.Date(), 'Average_ant_vel_col_zscore_peak.png', sep = '_')
# ggsave(filename, plot = vel_col_peak , path = 'results/plots/colanal/paper/', width = 10, height = 4)

filename <- paste0('results/plots/paper/', Sys.Date(), '_Average_vel_col_zscore_peak.svg')
svglite(filename, width = 8, height = 5)
vel_col_peak
dev.off()

filename <- paste0('results/plots/paper/', Sys.Date(), '_Average_post_vel_col_zscore_peak.svg')
svglite(filename, width = 8, height = 5)
vel_col_peak_post
dev.off()

filename <- paste0('results/plots/paper/', Sys.Date(), '_Average_ant_ani_col_zscore_peak.svg')
svglite(filename, width = 8, height = 5)
ani_col_peak
dev.off()

filename <- paste0('results/plots/paper/', Sys.Date(), '_Average_ant_iso_col_zscore_peak.svg')
svglite(filename, width = 8, height = 5)
iso_col_peak
dev.off()
# 


(iso_col_peak <- averaged_zscore_peak %>% 
    filter(measure == 'iso', direction == 'posterior', distance_aligned > -115 , distance_aligned < 150) %>% 
    ggplot(aes(x = distance_aligned, y = mean)) +
    geom_rect(aes(xmax = ROI_start_aligned, xmin = max(ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.2, fill = 'grey', colour = 'grey') +
    geom_ribbon(aes(ymin = (mean - sem), ymax = (mean + sem)), fill = 'black', alpha = 0.3) +
    annotate('text', x = 18, y = -1.5, label = 'ROI', size = 6) +
    geom_line(linewidth = 1.5) +
    geom_hline(yintercept = 0, linetype = 'dashed', linewidth = 0.5) +
    theme_classic(base_size = 18) +
    theme(aspect.ratio = 0.4,
          text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance (µm)') +
    ylab(
      'Peak isotropic strain rate 
z-score'))


(ani_col_peak <- averaged_zscore_peak %>% 
    filter(measure == 'ani', direction == 'posterior', distance_aligned > -115 , distance_aligned < 150) %>% 
    ggplot(aes(x = distance_aligned, y = mean)) +
    geom_rect(aes(xmin = ROI_start_aligned, xmax = max(ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.2, fill = 'grey', colour = 'grey') +
    geom_ribbon(aes(ymin = (mean - sem), ymax = (mean + sem)), fill = 'black', alpha = 0.3) +
    annotate('text', x = 18, y = -1, label = 'ROI', size = 6) +
    geom_line(linewidth = 1.5) +
    geom_hline(yintercept = 0, linetype = 'dashed', linewidth = 0.5) +
    theme_classic(base_size = 18) +
    theme(aspect.ratio = 0.4,
          text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance (µm)') +
    ylab(
      'Peak anisotropic strain rate
z-score'))

(vel_col_peak <- averaged_zscore_peak %>% 
    filter(measure == 'vel', direction == 'posterior', distance_aligned > -115 , distance_aligned < 150) %>% 
    ggplot(aes(x = distance_aligned, y = mean)) +
    geom_rect(aes(xmin = ROI_start_aligned, xmax = max(ROI_end_aligned), ymin = -Inf, ymax = Inf), alpha = 0.2, fill = 'grey', colour = 'grey') +
    geom_ribbon(aes(ymin = (mean - sem), ymax = (mean + sem)), fill = 'black', alpha = 0.3) +
    annotate('text', x = 18, y = -1, label = 'ROI', size = 6) +
    geom_line(linewidth = 1.5) +
    geom_hline(yintercept = 0, linetype = 'dashed', linewidth = 0.5) +
    theme_classic(base_size = 18) +
    theme(aspect.ratio = 0.4,
          text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance (µm)') +
    ylab(
      'Peak velocity 
z-score'))


filename <- paste0('results/plots/paper/', Sys.Date(), '_Average_posterior_ani_col_zscore_peak.svg')
svglite(filename, width = 8, height = 5)
ani_col_peak
dev.off()
filename <- paste0('results/plots/paper/', Sys.Date(), '_Average_posterior_iso_col_zscore_peak.svg')
svglite(filename, width = 8, height = 5)
iso_col_peak
dev.off()
filename <- paste0('results/plots/paper/', Sys.Date(), '_Average_posterior_vel_col_zscore_peak.svg')
svglite(filename, width = 8, height = 5)
vel_col_peak
dev.off()

# Binning to see any preact trends -----


average_before <- zscore_column %>% 
  filter(timepoint < -15, distance > 3, distance < (max(distance) - 3)) %>%
  group_by(embryo.id, measure, distance_aligned, ROI_start, ROI_end, direction, somite) %>% 
  summarise(mean = mean(z_score)) %>% 
  ungroup() 

pdfname <- paste(Sys.Date(), 'before_zscore_ant', sep = '_')

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
    scale_colour_gradient(low = 'red', high = 'blue', na.value = NA) +
    theme_bw() + 
    facet_wrap(~embryo.id, nrow = 3, ncol = 3) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance') +
    ylab(expression(paste('zscore'))) 
  
  title = paste('Average', i ,'preactivation z-score of', d ,'embryos')
  
  pptx <- pptx %>% 
    add_slide(layout='Title and Content',master='Office Theme') %>% 
    ph_with(title, location = ph_location_type(type='title')) %>%
    ph_with(plot, location = ph_location('body', left = 1, top = 2, width = 8, height = 5))
  
  #print(plot)
  
}



# dev.off()

print(pptx, sprintf('results/plots/colanal/%s.pptx', pdfname))

#

pdfname <- paste(Sys.Date(), 'before_zscore_post', sep = '_')

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
    scale_colour_gradient(low = 'red', high = 'blue', na.value = NA) +
    theme_bw() + 
    facet_wrap(~embryo.id, nrow = 3, ncol = 3) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance') +
    ylab(expression(paste('zscore'))) 
  
  title = paste('Average', i ,'preactication z-score of', d ,'embryos')
  
  pptx <- pptx %>% 
    add_slide(layout='Title and Content',master='Office Theme') %>% 
    ph_with(title, location = ph_location_type(type='title')) %>%
    ph_with(plot, location = ph_location('body', left = 1, top = 3, width = 8, height = 2))
    
  
}

# dev.off()

print(pptx, sprintf('results/plots/colanal/%s.pptx', pdfname))

# Binning data into 55secs of each period --------



  
binned_before <- aligned_column %>% 
  filter(timepoint >= -115, timepoint <= -5) %>%
  group_by(embryo.id, measure, distance_aligned, ROI_start, ROI_end, direction, somite) %>% 
  summarise(mean = mean(value)) %>% 
  mutate(period = 'during') %>% 
  ungroup() 


binned_during <- aligned_column %>% 
  filter(timepoint >= 0, timepoint <= 120) %>%
  group_by(embryo.id, measure, distance_aligned, ROI_start, ROI_end, direction, somite) %>% 
  summarise(mean = mean(value)) %>% 
  mutate(period = 'during') %>% 
  ungroup() 

binned_after <- aligned_column %>% 
  filter(timepoint >= 125, timepoint <= 235) %>%
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
    geom_rect(aes(xmin = 0, xmax = (ROI_end - ROI_start), ymin = -Inf, ymax = Inf), alpha = 0.5, fill = '#fbb03b', colour = '#fbb03b') +
    geom_line() +
    scale_color_viridis(option = 'turbo') + 
    theme_bw() +
    facet_grid(rows = vars(embryo.id)) +
    ylim(-0, 0.016) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance') +
    ylab(expression(paste('Velocity [µm ', s^-1, ']')))
)

plot <- vel_ant + ant_embryos_col_vel + plot_layout(widths = c(2,1)) 
plot
filename <- 'ant_t=3_col_vel.png'
ggsave(filename, plot = plot  , path = 'results/plots/', width = 8, height = 5)


(
  vel_col_20231026_E1_16ss <- aligned_column %>% 
    filter(direction == 'anterior', measure == 'vel', timepoint == 150, embryo.id == '20231026_E1_16ss') %>% 
    ggplot(aes(x = distance_aligned, y = value)) +
    geom_rect(aes(xmin = 0, xmax = (ROI_end - ROI_start), ymin = -Inf, ymax = Inf), alpha = 0.5, fill = '#fbb03b', colour = '#fbb03b') +
    geom_line() +
    scale_color_viridis() + 
    theme_bw() +
    facet_wrap(~embryo.id) +
    ylim(-0.0002, 0.012) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance') +
    ylab(expression(paste('Velocity [µm ', s^-1, ']')))
)

filename <- 'ant_embryos_col_vel.png'
ggsave(filename, plot = ant_embryos_col_vel  , path = 'results/plots/', width = 5, height = 5)



# Plotting thresholded binned timepoints ------

(
  binned_column %>% 
    filter(direction == 'anterior', measure == 'vel', period == 'before') %>% 
    ggplot(aes(x = distance_aligned, y = norm_vel, colour = embryo.id)) +
   # annotate('rect', xmin = 'ROI_start', xmax = 'ROI_end', ymin = -Inf, ymax = Inf, alpha = 0.5, fill = '#fbb03b') +
    geom_rect(aes(xmin = 0, xmax = (ROI_end - ROI_start), ymin = -Inf, ymax = Inf), alpha = 0.03, fill = '#fbb03b', colour = NA ) +
    geom_hline(aes(yintercept = 1)) +  
    geom_line() +
    theme_bw() +
   # facet_grid(~period) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance') +
    ylab(expression(paste('Velocity [µm ', s^-1, ']')))
)

(
  binned_column %>% 
    filter(direction == 'anterior', measure == 'vel') %>% 
    ggplot(aes(x = distance_aligned, y = mean, colour = above_threshold, group = embryo.id)) +
    # annotate('rect', xmin = 'ROI_start', xmax = 'ROI_end', ymin = -Inf, ymax = Inf, alpha = 0.5, fill = '#fbb03b') +
    geom_rect(aes(xmin = 0, xmax = (ROI_end - ROI_start), ymin = -Inf, ymax = Inf), alpha = 0.03, fill = '#fbb03b', colour = NA ) +
    geom_hline(aes(yintercept = threshold)) +  
    geom_line() +
    theme_bw() +
    facet_grid(embryo.id~period) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance') +
    ylab(expression(paste('Velocity [µm ', s^-1, ']')))
)

(
  binned_column %>% 
    filter(direction == 'posterior', measure == 'vel') %>% 
    ggplot(aes(x = distance_aligned, y = mean, colour = above_threshold, group = embryo.id)) +
    # annotate('rect', xmin = 'ROI_start', xmax = 'ROI_end', ymin = -Inf, ymax = Inf, alpha = 0.5, fill = '#fbb03b') +
    geom_rect(aes(xmin = 0, xmax = (ROI_start - ROI_end), ymin = -Inf, ymax = Inf), fill = '#fbb03b', alpha = 0.5) +
    geom_hline(aes(yintercept = threshold)) +  
    geom_line() +
    theme_bw() +
    facet_wrap(~period) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(size = 8),
          panel.spacing = unit(0.5, 'lines')) +
    xlab('Distance') +
    ylab(expression(paste('Velocity [µm ', s^-1, ']')))
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
    ylab(expression(paste('Velocity [µm ', s^-1, ']')))
  )


library(seewave)

wave <- aligned_column %>% 
  filter(embryo.id == '20231026_E3_NA', measure == 'vel', timepoint == 10) %>% 
  pull(value) 

spectrum(wave,log='no')

 del<-5
 x.spec <- spectrum(wave,log='no',span=10,plot=FALSE)
 spx <- x.spec$freq/del
 spy <- 2*x.spec$spec
 plot(spy~spx,xlab='frequency',ylab='spectral density',type='l')

 
 # 3d example ------
 
 
 (iso_3d_during <- averaged_zscore %>% 
    filter(timepoint >= 0, timepoint <= 50, measure == 'iso', direction == 'anterior') %>%
    plot_ly(x = ~distance_aligned, y = ~timepoint, z = ~mean, split = ~timepoint,
            type = 'scatter3d', mode = 'lines', line = list(width = 4)) %>%
    add_trace(data = iso_col_during, type = 'mesh3d',
              x = ~c(-Inf, -Inf, Inf, Inf, -Inf),
              y = ~c(-Inf, -Inf, Inf, Inf, -Inf),
              z = ~c(0, 0, 0, 0, 0),
              color = I('grey'), opacity = 0.2) %>%
    layout(scene = list(xaxis = list(title = 'Distance [µm]'),
                        yaxis = list(title = 'Timepoint'),
                        zaxis = list(title = 'Average Isotropic Strain z-score')),
           margin = list(l = 50, r = 50, b = 50, t = 50),
           title = 'Average Isotropic Strain z-score over Time'))
 
 
 (iso_3d_before <- averaged_zscore %>% 
     filter(timepoint >= -80, timepoint <=-30, measure == 'iso', direction == 'anterior') %>% 
     plot_ly(x = ~distance_aligned, y = ~timepoint, z = ~mean, split = ~timepoint,
             type = 'scatter3d', mode = 'lines', line = list(width = 4)) %>%
     add_trace(data = iso_col_during, type = 'mesh3d',
               x = ~c(-Inf, -Inf, Inf, Inf, -Inf),
               y = ~c(-Inf, -Inf, Inf, Inf, -Inf),
               z = ~c(0, 0, 0, 0, 0),
               color = I('grey'), opacity = 0.2) %>%
     layout(scene = list(xaxis = list(title = 'Distance [µm]'),
                         yaxis = list(title = 'Timepoint'),
                         zaxis = list(title = 'Average Isotropic Strain z-score')),
            margin = list(l = 50, r = 50, b = 50, t = 50),
            title = 'Average Isotropic Strain z-score over Time'))
 
 
# rhombomere positions ------
 
 rhombomeres <- read.xlsx('other analysis/rhombomere-y-positions.xlsx') %>% 
   pivot_longer(cols = everything(),
                names_to = 'embryo.id', 
                values_to = 'rhombomere') %>% 
   mutate(rhombomere = rhombomere * 0.275,
          rhombomere = round(rhombomere)) %>% 
   drop_na()
 
 
 measures <- c('vel', 'iso', 'ani', 'str', 'apstr', 'mlstr', 'x', 'y')
 periods <- c('before', 'during', 'after')
 
 y_limits <- list(
   vel = c(-2, 12),  
   iso = c(-6, 6),  
   ani = c(-2, 6),
   str = c(-2, 8),
   apstr = c(-2, 8),
   mlstr = c(-2, 8),
   x = c(-6, 6),  
   y = c(-6, 6)
 )
 
 measure_labs <- list(
   vel = 'Velocity',  
   iso = 'Isotropic Strain Rate',  
   ani = 'Anisotropic Strain Rate',
   str = ' Strain Rate' ,
   apstr = 'AP Strain Rate' ,
   mlstr = 'ML Strain Rate',
   x = 'ML Iso Strain Rate',
   y = 'AP Iso Strain Rate'
 )

 
 for (i in measures) {
   y_lim <- y_limits[[i]]
   measure_lab <- measure_labs[[i]]
     
   for (j in periods) {
     
     filtered_data <- zscore_column %>% 
       filter(measure == i, timepoint < 0, timepoint >-20)
     
     plot <- filtered_data %>% 
       ggplot(aes(x = distance, y = z_score, colour = timepoint, group = timepoint)) +
       geom_line(linewidth = 1, alpha = 0.5) +
       geom_vline(data = rhombomeres, aes(xintercept = rhombomere), linetype = 'dotted') +
       geom_vline(aes(xintercept = ROI_start)) +
       geom_vline(aes(xintercept = ROI_end)) +
       scale_colour_gradient(low = 'red', high = 'blue', na.value = NA) +
       theme_bw() + 
       facet_wrap(~embryo.id) +
       theme(strip.background = element_blank(),
             strip.text.x = element_text(size = 8),
             panel.spacing = unit(0.5, 'lines')) +
       labs(title = measure_lab) +
       xlab('Distance') +
       ylab(expression(paste('zscore'))) +
       ylim(y_lim[1], y_lim[2]) 
     
     print(plot)
     
     filename <- paste0('before2_', i, '_col_zscore.png')
     ggsave(filename, plot = plot, path = 'results/plots/colanal/rhombo/', width = 8, height = 4)
     
  }
 }
 

 
 
 
# fft test ----
 fft_test_during <- averaged_zscore %>% 
   filter(timepoint == 55, measure == 'iso', direction == 'anterior') %>% 
   select(distance_aligned, mean)
 fft_test_before <- averaged_zscore %>% 
   filter(timepoint == -50, measure == 'iso', direction == 'anterior') %>% 
   select(distance_aligned, mean)

  fft_test_during_ <- data.frame(fft(fft_test_during$mean) )
  fft_test_during <- merge(fft_test_during, fft_test_during_)
  
  fft_test_before_ <- data.frame(fft(fft_test_before$mean) )
  fft_test_before <- merge(fft_test_before, fft_test_before_)

  fft_test_during %>% 
    ggplot(aes(x = abs(fft.fft_test_during.mean.))) +
    geom_histogram(binwidth = 1) +
    xlim(0,15) +
    ylim(0,1300)
  
  fft_test_before %>% 
    ggplot(aes(x = abs(fft.fft_test_before.mean.))) +
    geom_histogram(binwidth = 1) +
    xlim(0,15) +
    ylim(0,1300)
  