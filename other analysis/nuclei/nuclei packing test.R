# Load libraries
library(tidyverse)
library(openxlsx)
library(pdf2pptx)
library(ggquiver)
library(viridis)  

# Create a data frame containing the PIV data (mean and std of different measures in different cardinal direction around the ROI)
file1 <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/nuclei/anterior/20221018_E1_15ss/Results.csv'
file2 <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/nuclei/anterior/20221108_E8_15ss/Results.csv'
file3 <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/nuclei/anterior/20220901_E2_/Results.csv'
file4 <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/nuclei/anterior/20221214_E5_18ss/Results.csv'
file5 <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/nuclei/anterior/20230804_E1_21ss/Results.csv'
file6 <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/nuclei/anterior/20231026_E1_16ss/Results.csv' 
file7 <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/nuclei/anterior/20231026_E3_NA/Results.csv'
 
file_names <- c(file1, file2, file3, file4, file5, file6, file7)  
data_list <- list()
i <- 1

for (file in file_names){
  temp_data <- read_csv(file)
  date_id <- str_extract(file, '\\d{8}')
  embryo_id <- str_extract(file, '[Ee]\\d')
  somite_id <- str_extract(file, '\\d+ss')
  temp_data <- as.data.frame(temp_data) %>% 
    mutate(date = date_id,
           embryo = embryo_id,
           somite = somite_id)
  data_list[[i]] <- temp_data
  i <- i+1
}
i <- 1

data <- bind_rows(data_list) %>% as_tibble() 

data <- data %>% 
  unite(col = 'embryo.id', c('date', 'embryo', 'somite'), remove = FALSE) %>% 
  mutate(time = case_when(str_detect(Label, 'img_0013') == T ~ 13,
                          str_detect(Label, 'img_0024') == T ~ 24,
                          str_detect(Label, 'img_0036') == T ~ 36,
                          str_detect(Label, 'img_0061') == T ~ 61,
                          str_detect(Label, 'img_0051') == T ~ 51,
                          str_detect(Label, 'img_0050') == T ~ 50,
                          str_detect(Label, 'img_0035') == T ~ 35),
         X = round(X),
         Y = round(Y)) %>% 
  group_by(embryo.id, time) %>% 
  arrange(...1) %>% 
  mutate(nuclei = row_number()) %>% 
  unite(col = 'id', c('embryo.id', 'nuclei'), remove = FALSE) %>% 
  ungroup() %>%
  select(-Label, -Area, -...1) %>% 
  pivot_wider(names_from = 'time', 
               values_from = c('X','Y')) 


displacement <- data %>% 
  group_by(id) %>% 
  mutate(X_before = X_24 - X_13,
         X_during1 = X_35 - X_24,
         X_during = X_36 - X_24,
         X_during = coalesce(X_during, X_during1),
         X_after1 = X_61 - X_50,
         X_after = X_61 - X_51,
         X_after = coalesce(X_after, X_after1),
         Y_before = Y_24 - Y_13,
         Y_during1 = Y_35 - Y_24,
         Y_during = Y_36 - Y_24,
         Y_during = coalesce(Y_during, Y_during1),
         Y_after1 = Y_61 - Y_50,
         Y_after = Y_61 - Y_51,
         Y_after = coalesce(Y_after, Y_after1)) %>% 
  select(-X_during1, -X_after1, -Y_during1, -Y_after1) %>% 
  select('embryo.id','id', 'X_before', 'X_during', 'X_after', 'Y_before', 'Y_during', 'Y_after') %>% 
  pivot_longer(cols = -c(id, embryo.id), 
               names_to = 'period',
               values_to = 'value') %>% 
  separate(period, into = c('XY', 'period')) %>% 
  pivot_wider(names_from = XY, values_from = value) %>% 
  ungroup() %>% 
  mutate(X = X * 0.275,
         Y = Y * 0.275,
         period = factor(period, levels = c('before', 'during', 'after'))) 


average <- displacement %>% 
  summarise(X_before = mean(X_before),
            X_during = mean(X_during),
            X_after = mean(X_after),
            Y_before = mean(Y_before),
            Y_during = mean(Y_during),
            Y_after = mean(Y_after))

  
plot_names <- as_labeller(c(`before` = 'Anterior & N', `during` = 'Posterior & S', `after` = 'Posterior & S' ))


 (Yplot <- displacement %>%
    ggplot(aes(x = period, y = Y)) +
    geom_boxplot(outlier.shape=NA, size = 0.5, position = position_dodge(0.5), width = 0.5) +
    geom_jitter(size = 2,  alpha = 0.7, width = 0.25, aes(colour = factor(period)), show.legend = F) +
    scale_colour_manual(values = c("before" = "grey30",
                                 "during" = "#fbb03b",
                                 "after" = "grey30")) +
    scale_y_reverse() +
    labs(x = '', y = "Displacement (µm)") +
    theme_classic(base_size = 18) + 
    theme(text = element_text(size = 20, family = 'Avenir'),
         strip.background = element_blank(),
         strip.text.x = element_text(size = 18),
         strip.text.y = element_text(size = 18),
         panel.spacing = unit(0.5, 'lines')) 
 )
 
 # Test for normality 
 
 displacement %>% 
   group_by(period) %>% 
   shapiro_test(X, Y)
 
 # only X before and after are normal

 # Testing for equal variance (for non normal data =leveneTest, for normal data = bartlett.test)

 displacement %>%
   levene_test(Y ~ period)
 
 displacement %>%
   levene_test(X ~ period)
 
 # no difference between variances
 
 # outcome is do non-parametric anova - Kruksal wallis
 
 Xstats <- displacement %>% 
   kruskal_test(X ~ period) %>% 
   add_significance() 
 # not significant
 
 Ystats <- displacement %>% 
   kruskal_test(Y ~ period) %>% 
   add_significance() 
 
 Ystats <- displacement %>% 
   dunn_test(Y ~ period, p.adjust.method = 'bonferroni') %>% 
   add_significance() %>% 
   add_xy_position()
 
 displacement %>% 
   group_by(period) %>% 
   t_test(Y ~ embryo.id)
 
 Yplot + stat_pvalue_manual(Ystats, label = "{p.adj.signif}", size = 8, hide.ns = TRUE)

 library(lme4)
 displacement
 lmer(Y ~ period + (1|embryo.id), data = displacement)
 
 #-----
 
 
 

# nuclear packing test ------

data <- read_csv('/Users/helena/Desktop/Results.csv')

data <- as.tibble(data)

ggplot(data, aes(x=X, y=Y) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white", h = 50) +
  coord_equal()
