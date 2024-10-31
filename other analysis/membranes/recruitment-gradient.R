library(tidyverse)
library(openxlsx)
library(pdf2pptx)
library(viridis)  
library(rstatix)
library(svglite)
library(writexl)

membrane_files <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/membranes/results/membrane-grad' 
membrane_files <- list.files(membrane_files, pattern="*.csv", full.names=TRUE)
data_list <- list()
i <- 1

for (file in membrane_files){
  temp_data <- read_csv(file)
  date_id <- str_extract(file, '\\d{8}')
  embryo_id <- str_extract(file, '\\d{8}_[Ee]\\d_\\d{2}ss')
  embryo_id2 <- str_extract(file, '\\d{8}_[Ee]\\d_NA')
  embryo <- str_extract(file, '[Ee]\\d')
  somite_id <- str_extract(file, '\\d+ss')
  temp_data <- as.data.frame(temp_data) %>% 
    mutate(embryo.id = embryo_id,
           date = date_id,
           embryo = embryo,
           somite = somite_id,
           location = 'membrane',
           embryo.id2 = embryo_id2) 
  data_list[[i]] <- temp_data
  i <- i+1
}
i <- 1

membrane_data <- bind_rows(data_list) %>% as_tibble() 

cyto_files <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/membranes/results/cyto-grad' 
cyto_files <- list.files(cyto_files, pattern="*.csv", full.names=TRUE)
data_list <- list()
i <- 1

for (file in cyto_files){
  temp_data <- read_csv(file)
  date_id <- str_extract(file, '\\d{8}')
  embryo_id <- str_extract(file, '\\d{8}_[Ee]\\d_\\d{2}ss')
  embryo_id2 <- str_extract(file, '\\d{8}_[Ee]\\d_NA')
  embryo <- str_extract(file, '[Ee]\\d')
  somite_id <- str_extract(file, '\\d+ss')
  temp_data <- as.data.frame(temp_data) %>% 
    mutate(embryo.id = embryo_id,
           date = date_id,
           embryo = embryo,
           somite = somite_id,
           location = 'cyto',
           embryo.id2 = embryo_id2) 
  data_list[[i]] <- temp_data
  i <- i+1
}
i <- 1

cyto_data <- bind_rows(data_list) %>% as_tibble() 

thresholded_data <- read.xlsx('results/spreadsheets/2024-06-20_thresholded_data_ungrouped.xlsx') %>% 
  select(embryo.id, direction) %>% 
  distinct(embryo.id, direction)
post_data <- read.xlsx('results/spreadsheets/2024-09-09_post_thresholded_data_ungrouped.xlsx') %>% 
  select(embryo.id, direction) %>% 
  distinct(embryo.id, direction)

thresholded_data <- thresholded_data %>%  bind_rows(post_data)

data <- membrane_data %>% 
  bind_rows(cyto_data) %>% 
  mutate(embryo.id = ifelse(is.na(embryo.id2), embryo.id, embryo.id2)) %>%
  group_by(embryo.id, location) %>% 
  arrange(...1) %>% 
  mutate(cell = row_number(),
         cell = paste0(embryo.id, cell)) %>% 
  select(-embryo.id2) %>% 
  ungroup()

data <- data %>% 
  left_join(thresholded_data, by = 'embryo.id') %>% 
  dplyr::filter(!is.na(direction))

label <- data %>% 
  dplyr::filter(location == 'membrane') %>% 
  mutate(y = str_extract(Label, ':\\d{4}'),
         y = as.numeric(str_remove(y, ':')),
         y = y * 0.275,
         y = case_when(location == 'cyto' ~ NA,
                       location == 'membrane' ~ y),
         time = '36') %>% 
  select(cell, y, time)

data <- data %>% 
  left_join(label, by = 'cell') %>% 
  # unite(col = 'embryo_id', c('date', 'embryo', 'somite'), remove = FALSE) %>% 
  group_by(embryo.id) %>% 
  arrange(...1) %>% 
  select(-Label, -Area, -...1, -Length, -StdDev, -Min, -Max, -Median) %>% 
  ungroup() %>% 
  group_by(embryo.id, cell) %>% 
  pivot_wider(names_from = 'location', 
              values_from = c('Mean')) %>% #, 'StdDev', 'Min', 'Max', 'Median')) %>% 
  drop_na(membrane, cyto) %>%
  mutate(ratio = membrane / cyto) %>% 
  ungroup()

grad <- data %>% 
  group_by(embryo.id, direction) %>% 
  summarise(EGFP = coef(lm(ratio ~ y))[2],
            #EGFP1 = coef(lm(membrane ~ y))[2]
            ) %>% 
  ungroup() 

filename <- paste(Sys.Date(), 'recruitment_gradient', sep = "_")
write_xlsx(grad, path = sprintf('results/spreadsheets/%s.xlsx', filename))

mean_grad <- grad %>% 
  group_by(direction) %>% 
  summarise(mean = mean(EGFP),
            std = sd(EGFP))

ant_grad <- data %>% 
  group_by(embryo.id, direction) %>% 
  summarise(EGFP = coef(lm(ratio ~ y))[2],
            EGFP1 = coef(lm(membrane ~ y))[2]) %>% 
  filter(direction == 'anterior')

post_grad <- data %>% 
  group_by(embryo.id, direction) %>% 
  summarise(EGFP = coef(lm(ratio ~ y))[2],
            EGFP1 = coef(lm(membrane ~ y))[2]) %>% 
  filter(direction == 'posterior')

qqnorm(grad %>% 
         filter(direction == 'anterior') %>% 
         pull(EGFP))

qqline(grad %>% 
         filter(direction == 'anterior') %>% 
         pull(EGFP))

shapiro.test(grad %>% 
               filter(direction == 'anterior') %>% 
               pull(EGFP))

bartlett.test(EGFP ~ direction, data = grad)

grad_stat <- grad %>% 
  wilcox_test(EGFP ~ direction) %>% 
  add_xy_position() %>% 
  add_significance()

grad_pvalue <- grad_stat %>% pull(p) %>% as.character()

t.test(grad %>% 
  filter(direction == 'anterior') %>% 
  pull(EGFP))



embryo_colors <- c(
  "20221018_E1_15ss" = "#AAF9A9",  
  "20221108_E8_15ss" = "#3357FF",  
  "20221214_E2_13ss" = "#33FF57",  
  "20221214_E3_14ss" = "#FF33A1",  
  "20221214_E4_15ss" = "#A133FF",  
  "20221214_E5_12ss" = "#33FFA1",  
  "20230315_E5_23ss" = "#FFC300",  
  "20230315_E5_24ss" = "#FF5733",  
  "20230804_E1_21ss" = "#900C3F",  
  "20231026_E1_16ss" = "#581845",  
  "20231026_E3_NA"   = "#00FFFF", 
  "20240426_E6_NA"   = "#FF00FF",  
  "20240503_E7_21ss" = "#FFFF33",  
  "20240503_E7_22ss" = "#FF3333"   
)


( gradient_lm <- data %>%
    ggplot(aes(y = ratio, x = y)) +
    #geom_line() +
     geom_point(aes(colour = embryo.id), alpha = 0.5) +
    geom_smooth(aes(colour = embryo.id), method = 'lm', se = FALSE)  +
     geom_text(data = mean_grad, family="Avenir", aes(x = 20, y = 1.32,
              label = paste('slope =',round(mean, digits = 5))))+
   # geom_smooth(aes(group = direction), method = 'lm', se = FALSE, colour = 'black')  +
  facet_wrap(~direction) +
  scale_colour_manual(values = embryo_colors) +
  theme_classic(base_size = 18) + 
  theme(aspect.ratio = 2,
       legend.position="none", 
        text = element_text(size = 20, family = 'Avenir'),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18),
        panel.spacing = unit(1, 'lines')) +
     ylab('Membrane/Cytoplasm Ratio') +
     xlab("AP distance in ROI (µm)")
)

(gradient_bp <- grad %>% 
    ggplot(aes(y = EGFP, x = direction)) +
    geom_boxplot(aes(fill = direction), alpha = 0.2)+
    #stat_boxplot(geom = "errorbar", width = 0.2) +
    geom_point(aes(colour = embryo.id), size = 2) +
    scale_colour_manual(values = embryo_colors) +
    stat_pvalue_manual(grad_stat, bracket.nudge.y	= 0.0005, family="Avenir") +
    annotate('text', x = 1.5, y = 0.0025, label = paste('p =', grad_pvalue ), family="Avenir" ) +
    theme_classic(base_size = 18) + 
    theme(aspect.ratio = 2,
          legend.position="none", 
          text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 18),
          panel.spacing = unit(1, 'lines')) +
  ylab('AP recruitment gradient') +
  xlab('')
)


( gradient_raw <- data %>%
    ggplot(aes(y = ratio, x = y, colour = embryo.id)) +
    geom_line() +
    geom_point() +
    #geom_smooth(method = 'lm')  +
    facet_wrap(~direction) +
    scale_colour_manual(values =embryo_colors) +
    theme_classic(base_size = 18) + 
    theme(aspect.ratio = 1,
          legend.position="none", 
          text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 18),
          panel.spacing = unit(1, 'lines')) +
    ylab('Membrane/Cytoplasm Ratio') +
    xlab("AP distance in ROI (µm)")
)

filename <- paste0('results/plots/paper/', Sys.Date(), '_gradient_lm.svg')
svglite(filename, width = 5, height = 5)
gradient_lm
dev.off()
filename <- paste0('results/plots/paper/', Sys.Date(), '_gradient_bp.svg')
svglite(filename, width = 5, height = 5)
gradient_bp
dev.off()
filename <- paste0('results/plots/paper/', Sys.Date(), '_gradient_raw.svg')
svglite(filename, width = 5, height = 5)
gradient_raw
dev.off()

# mcherry levels -----

mcherry_files <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/membranes/results/mcherry' 
mcherry_files <- list.files(mcherry_files, pattern="*.csv", full.names=TRUE)
data_list <- list()
i <- 1

for (file in mcherry_files){
  temp_data <- read_csv(file)
  date_id <- str_extract(file, '\\d{8}')
  embryo_id <- str_extract(file, '\\d{8}_[Ee]\\d_\\d{2}ss')
  embryo_id2 <- str_extract(file, '\\d{8}_[Ee]\\d_NA')
  embryo <- str_extract(file, '[Ee]\\d')
  somite_id <- str_extract(file, '\\d+ss')
  temp_data <- as.data.frame(temp_data) %>% 
    mutate(embryo.id = embryo_id,
           date = date_id,
           embryo = embryo,
           somite = somite_id,
           location = 'mcherry',
           embryo.id2 = embryo_id2) 
  data_list[[i]] <- temp_data
  i <- i+1
}
i <- 1

mcherry_data <- bind_rows(data_list) %>% as_tibble() 

baseline_data <- read.xlsx('/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/membranes/mcherry-total-fluor.xlsx') %>% 
  select(embryo.id, direction, Mean) %>% 
  rename(baseline = Mean)

mcherry_data <- mcherry_data %>% 
  mutate(embryo.id = ifelse(is.na(embryo.id2), embryo.id, embryo.id2)) %>%
  group_by(embryo.id, location) %>% 
  arrange(...1) %>% 
  mutate(cell = row_number(),
         cell = paste0(embryo.id, cell)) %>% 
  select(-embryo.id2) %>% 
  ungroup()


mcherry_data <- baseline_data %>% 
  right_join(mcherry_data, by = 'embryo.id') 

label <- mcherry_data %>% 
  mutate(y = str_extract(Label, ':\\d{4}'),
         y = as.numeric(str_remove(y, ':')),
         y = y * 0.275,
         y = case_when(location == 'mcherry' ~ y)) %>% 
  select(cell, y)

mcherry_data <- mcherry_data %>% 
  left_join(label, by = 'cell') %>% 
  # unite(col = 'embryo_id', c('date', 'embryo', 'somite'), remove = FALSE) %>% 
  group_by(embryo.id) %>% 
  arrange(...1) %>% 
  select(-Label, -Area, -...1, -Length, -StdDev, -Min, -Max, -Median) %>% 
  ungroup() %>% 
  group_by(embryo.id, cell) %>% 
  mutate(ratio = Mean / baseline) %>% 
  rename(mcherry = Mean) %>% 
  ungroup()

grad1 <- mcherry_data  %>% 
  group_by(embryo.id, direction)  %>% 
  summarise(mch = coef(lm(ratio ~ y))[2],
            mch1 = coef(lm(mcherry ~ y))[2])


grad_all <- grad %>% 
  full_join(grad1) %>% 
  drop_na()

cor.test(grad_all %>% pull(EGFP), grad_all %>% pull(mch1), method = 'spearman')
cor <- grad_all %>% cor_test(EGFP, mch1, method = 'spearman')

(correlation <- grad_all %>% 
  drop_na() %>% 
  ggplot(aes(y=EGFP, x = mch1, colour = embryo.id)) +
  geom_point(size = 6) +
  annotate('text', x = -0.95, y = -0.0027, hjust = 0, label = paste('rho =', cor$cor ), family="Avenir", size = 5 ) +
  annotate('text', x = -0.95, y = -0.003, hjust = 0, label = paste('p =', cor$p ), family="Avenir", size = 5 ) +
  scale_colour_manual(values = embryo_colors) +
  scale_x_continuous(expand = expansion(add = 0.08)) +
  scale_y_continuous(expand = expansion(add = 0.0005)) +
  theme_classic(base_size = 18) + 
  theme(aspect.ratio = 1,
        text = element_text(size = 20, family = 'Avenir'),
        strip.background = element_blank(),
        legend.position = 'none',
        strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18),
        panel.spacing = unit(1, 'lines')) +
  ylab('EGFP gradient') +
  xlab("mCherry gradient")
)

filename <- paste0('results/plots/paper/', Sys.Date(), '_gradient_cor.svg')
svglite(filename, width = 5, height = 5)
correlation
dev.off()




