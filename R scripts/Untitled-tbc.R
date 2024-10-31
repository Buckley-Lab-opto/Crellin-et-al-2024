# Load libraries
library(tidyverse)
library(openxlsx)
library(pdf2pptx)
library(viridis)  
library(rstatix)

membrane_files <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/membranes/results/membrane' 
membrane_files <- list.files(membrane_files, pattern="*.csv", full.names=TRUE)
data_list <- list()
i <- 1

for (file in membrane_files){
  temp_data <- read_csv(file)
  date_id <- str_extract(file, '\\d{8}')
  embryo_id <- str_extract(file, '\\d{8}_[Ee]\\d_\\d{2}ss')
  embryo <- str_extract(file, '[Ee]\\d')
  somite_id <- str_extract(file, '\\d+ss')
  temp_data <- as.data.frame(temp_data) %>% 
    mutate(embryo.id = embryo_id,
           date = date_id,
           embryo = embryo,
           somite = somite_id,
           location = 'membrane',
           bg = 'og')
  data_list[[i]] <- temp_data
  i <- i+1
}
i <- 1

membrane_data <- bind_rows(data_list) %>% as_tibble() 

act_cyto_files <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/membranes/results/act_cyto' 
act_cyto_files <- list.files(act_cyto_files, pattern="*.csv", full.names=TRUE)
data_list <- list()
i <- 1

for (file in act_cyto_files){
  temp_data <- read_csv(file)
  date_id <- str_extract(file, '\\d{8}')
  embryo_id <- str_extract(file, '\\d{8}_[Ee]\\d_\\d{2}ss')
  embryo <- str_extract(file, '[Ee]\\d')
  somite_id <- str_extract(file, '\\d+ss')
  temp_data <- as.data.frame(temp_data) %>% 
    mutate(embryo.id = embryo_id,
           date = date_id,
           embryo = embryo,
           somite = somite_id,
           location = 'cyto',
           bg = 'og')
  data_list[[i]] <- temp_data
  i <- i+1
}
i <- 1

act_cyto_data <- bind_rows(data_list) %>% as_tibble() 

deact_cyto_files <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/membranes/results/deact_cyto' 
deact_cyto_files <- list.files(deact_cyto_files, pattern="*.csv", full.names=TRUE)
data_list <- list()
i <- 1

for (file in deact_cyto_files){
  temp_data <- read_csv(file)
  date_id <- str_extract(file, '\\d{8}')
  embryo_id <- str_extract(file, '\\d{8}_[Ee]\\d_\\d{2}ss')
  embryo <- str_extract(file, '[Ee]\\d')
  somite_id <- str_extract(file, '\\d+ss')
  temp_data <- as.data.frame(temp_data) %>% 
    mutate(embryo.id = embryo_id,
           date = date_id,
           embryo = embryo,
           somite = somite_id,
           location = 'cyto',
           bg = 'og')
  data_list[[i]] <- temp_data
  i <- i+1
}
i <- 1

deact_cyto_data <- bind_rows(data_list) %>% as_tibble() 

membrane_files <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/membranes/results/bg_subtracted/membrane' 
membrane_files <- list.files(membrane_files, pattern="*.csv", full.names=TRUE)
data_list <- list()
i <- 1

for (file in membrane_files){
  temp_data <- read_csv(file)
  date_id <- str_extract(file, '\\d{8}')
  embryo_id <- str_extract(file, '\\d{8}_[Ee]\\d_\\d{2}ss')
  embryo <- str_extract(file, '[Ee]\\d')
  somite_id <- str_extract(file, '\\d+ss')
  temp_data <- as.data.frame(temp_data) %>% 
    mutate(embryo.id = embryo_id,
           date = date_id,
           embryo = embryo,
           somite = somite_id,
           location = 'membrane',
           bg = 'sub')
  data_list[[i]] <- temp_data
  i <- i+1
}
i <- 1

membranebg_data <- bind_rows(data_list) %>% as_tibble() 

act_cyto_files <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/membranes/results/bg_subtracted/act_cyto' 
act_cyto_files <- list.files(act_cyto_files, pattern="*.csv", full.names=TRUE)
data_list <- list()
i <- 1

for (file in act_cyto_files){
  temp_data <- read_csv(file)
  date_id <- str_extract(file, '\\d{8}')
  embryo_id <- str_extract(file, '\\d{8}_[Ee]\\d_\\d{2}ss')
  embryo <- str_extract(file, '[Ee]\\d')
  somite_id <- str_extract(file, '\\d+ss')
  temp_data <- as.data.frame(temp_data) %>% 
    mutate(embryo.id = embryo_id,
           date = date_id,
           embryo = embryo,
           somite = somite_id,
           location = 'cyto',
           bg = 'sub')
  data_list[[i]] <- temp_data
  i <- i+1
}
i <- 1

act_cytobg_data <- bind_rows(data_list) %>% as_tibble() 

deact_cytobg_files <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/membranes/results/bg_subtracted/deact_cyto' 
deact_cytobg_files <- list.files(deact_cytobg_files, pattern="*.csv", full.names=TRUE)
data_list <- list()
i <- 1

for (file in deact_cytobg_files){
  temp_data <- read_csv(file)
  date_id <- str_extract(file, '\\d{8}')
  embryo_id <- str_extract(file, '\\d{8}_[Ee]\\d_\\d{2}ss')
  embryo <- str_extract(file, '[Ee]\\d')
  somite_id <- str_extract(file, '\\d+ss')
  temp_data <- as.data.frame(temp_data) %>% 
    mutate(embryo.id = embryo_id,
           date = date_id,
           embryo = embryo,
           somite = somite_id,
           location = 'cyto',
           bg = 'sub')
  data_list[[i]] <- temp_data
  i <- i+1
}
i <- 1

deact_cytobg_data <- bind_rows(data_list) %>% as_tibble() 


data <- membrane_data %>% 
  bind_rows(act_cyto_data) %>% 
  bind_rows(deact_cyto_data) %>% 
  bind_rows(membranebg_data) %>% 
  bind_rows(act_cytobg_data) %>% 
  bind_rows(deact_cytobg_data) %>% 
#  unite(col = 'embryo.id', c('date', 'embryo', 'somite'), remove = FALSE) %>% 
  mutate(time = str_extract(Label, 't=\\d{2}'),
         time = as.numeric(str_remove(time, 't='))) %>% 
  group_by(embryo.id, location, bg, time) %>% 
  arrange(...1) %>% 
  mutate(cell = row_number()) %>% 
  unite(col = 'id', c('embryo.id', 'cell'), remove = FALSE) %>% 
  select(-Label, -Area, -...1, -Length, -StdDev, -Min, -Max, -Median, -X, -Y) %>% 
  ungroup() %>%
  group_by(embryo.id, bg, time, cell) %>% 
  pivot_wider(names_from = 'location', 
              values_from = c('Mean')) %>% #, 'StdDev', 'Min', 'Max', 'Median')) %>% 
  mutate(ratio = membrane / cyto,
         activation = case_when(time < 50 ~ 'act',
                                time > 49 ~ 'deact')) %>% 
  drop_na() %>% 
  group_by(activation) %>% 
  mutate(time = (time - min(time)) * 5) %>% 
  ungroup()



averaged_data <- data %>% 
  group_by(time, bg, activation) %>% 
  summarise(ratio = mean(ratio))
  


# plots -----

(plot <- data %>%
   filter(bg == 'sub', activation == 'act', embryo.id == '20221214_E2_13ss') %>% 
   ggplot(aes(x = time, y = ratio, colour = id)) +
   annotate("rect", xmin = 5, xmax = 60, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "#fbb03b") +
   geom_line() +
   labs(x = 'Time (s)', y = "Membrane / cytoplasm ratio") +
   theme_classic(base_size = 18) + 
   theme(text = element_text(size = 20, family = 'Avenir'),
         strip.background = element_blank(),
         strip.text.x = element_text(size = 18),
         strip.text.y = element_text(size = 18),
         panel.spacing = unit(0.5, 'lines')) 
)

(plot <- averaged_data %>%
   filter(bg == 'sub') %>% 
   ggplot(aes(x = time, y = ratio, group = activation)) +
   annotate("rect", xmin = 5, xmax = 60, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "#fbb03b") +
   #geom_line() +
   geom_line(data = data %>% filter(bg == 'sub', activation == 'act'), aes(x = time, y = ratio, group = embryo.id), colour = 'grey')+
   geom_line(data = data %>% filter(bg == 'sub', activation == 'deact'), aes(x = time, y = ratio, group = embryo.id), colour = 'grey')+
   geom_smooth() +
   labs(x = 'Time (s)', y = "Membrane / cytoplasm ratio") +
   theme_classic(base_size = 18) + 
   theme(text = element_text(size = 20, family = 'Avenir'),
         strip.background = element_blank(),
         strip.text.x = element_text(size = 18),
         strip.text.y = element_text(size = 18),
         panel.spacing = unit(0.5, 'lines')) 
)

# -----


# Test for normality 

displacement %>% 
  group_by(period) %>% 
  shapiro_test(X, Y, D, abs_X, abs_Y)


# only X before and after are normal

# Testing for equal variance (for non normal data =leveneTest, for normal data = bartlett.test)

displacement %>%
  levene_test(Y ~ period) 

displacement %>% 
  group_by(period) %>% 
  summarise(IQR(Y),
            IQR(abs_Y))

displacement %>%
  levene_test(X ~ period)

displacement %>%
  levene_test(abs_X ~ period)

displacement %>%
  levene_test(abs_Y ~ period)

displacement %>% 
  qqnorm()
# Y and abs_Y has difference between variances

# outcome is do non-parametric anova - Kruksal wallis

Xstats <- displacement %>% 
  kruskal_test(X ~ period) %>% 
  add_significance() %>% 
  print()
# not significant

Ystats <- displacement %>% 
  kruskal_test(Y ~ period) %>% 
  add_significance() %>% 
  print()

Ystats <- displacement %>% 
  dunn_test(Y ~ period, p.adjust.method = 'bonferroni') %>% 
  add_significance() %>% 
  add_xy_position() %>% 
  print()

Dstats <- displacement %>% 
  kruskal_test(D ~ period) %>% 
  add_significance() %>% 
  print()

Dstats <- displacement %>% 
  dunn_test(D ~ period, p.adjust.method = 'bonferroni') %>% 
  add_significance() %>% 
  add_xy_position()

absXstats <- displacement %>% 
  kruskal_test(abs_X ~ period) %>% 
  add_significance() %>% 
  print()

absYstats <- displacement %>% 
  kruskal_test(abs_Y ~ period) %>% 
  add_significance() %>% 
  print()

absYstats <- displacement %>% 
  dunn_test(abs_Y ~ period, p.adjust.method = 'bonferroni') %>% 
  add_significance() %>% 
  add_xy_position() %>% 
  print()


# double checking the dunn test with a t test
# displacement %>% 
#   group_by(period) %>% 
#   t_test(Y ~ embryo.id)


# check for contribution of different embryos
# library(lme4)
# displacement
# lmer(Y ~ period + (1|embryo.id), data = displacement)
# 


Yplot <-  Yplot + stat_pvalue_manual(Ystats, label = "{p.adj.signif}", size = 8, hide.ns = TRUE)
filename <- paste0(Sys.Date(), 'nuclei_APdisp_ant.png')
ggsave(filename, plot = Yplot, path = 'results/plots/paper/', width = 4, height = 6)

Dplot <-  Dplot + stat_pvalue_manual(Dstats, label = "{p.adj.signif}", size = 8, hide.ns = TRUE)
filename <- paste0(Sys.Date(), 'nuclei_disp_ant.png')
ggsave(filename, plot = Dplot, path = 'results/plots/paper/', width = 4, height = 6)

