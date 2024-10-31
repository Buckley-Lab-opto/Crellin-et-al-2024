# Load libraries
library(tidyverse)
library(openxlsx)
library(pdf2pptx)
library(viridis)  
library(rstatix)
library(ggpubr)
library(svglite)


# Create a data frame containing the PIV data (mean and std of different measures in different cardinal direction around the ROI)
file1 <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/nuclei/anterior/20221018_E1_15ss/Results.csv'
file2 <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/nuclei/anterior/20221108_E8_15ss/Results.csv'
file3 <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/nuclei/anterior/20230901_E2_/Results.csv'
file4 <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/nuclei/anterior/20221214_E5_18ss/Results.csv'
file5 <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/nuclei/anterior/20230804_E1_21ss/Results.csv'
file6 <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/nuclei/anterior/20231026_E1_16ss/Results.csv' 
file7 <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/nuclei/anterior/20231026_E3_NA/Results.csv'
file8 <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/nuclei/posterior/20221214_E2_13ss/Results.csv'
file9 <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/nuclei/posterior/20221214_E3_14ss/Results.csv'
file10 <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/nuclei/posterior/20221214_E4_15ss/Results.csv'
 
file_names <- c(file1, file2, file3, file4, file5, file6, file7, file8, file9, file10)  
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

thresholded_data <- read.xlsx('results/spreadsheets/2024-06-20_thresholded_data.xlsx') %>% 
  select(embryo.id, direction) %>% 
  distinct(embryo.id, direction) %>% 
  mutate(embryo.id = case_match(embryo.id, '20221214_E5_12ss' ~ '20221214_E5_18ss', .default = embryo.id))

data <- data %>% 
  unite(col = 'embryo.id', c('date', 'embryo', 'somite'), remove = FALSE) %>% 
  mutate(time = case_when(str_detect(Label, 'img_0013') == T ~ 13,
                          str_detect(Label, 'img_0024') == T ~ 24,
                          str_detect(Label, 'img_0036') == T ~ 36,
                          str_detect(Label, 'img_0061') == T ~ 61,
                          str_detect(Label, 'img_0051') == T ~ 51,
                          str_detect(Label, 'img_0050') == T ~ 50,
                          str_detect(Label, 'img_0035') == T ~ 35),
         # X = round(X),
         # Y = round(Y)
         ) %>% 
  group_by(embryo.id, time) %>% 
  arrange(...1) %>% 
  mutate(nuclei = row_number()) %>% 
  unite(col = 'id', c('embryo.id', 'nuclei'), remove = FALSE) %>% 
  ungroup() %>%
  select(-Label, -Area, -...1) %>% 
  pivot_wider(names_from = 'time', 
               values_from = c('X','Y')) %>% 
  left_join(thresholded_data, by = 'embryo.id')


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
  select('embryo.id','id', 'X_before', 'X_during', 'X_after', 'Y_before', 'Y_during', 'Y_after', 'direction') %>% 
  mutate(D_before = sqrt((X_before)^2 + (Y_before)^2),
         D_during = sqrt((X_during)^2 + (Y_during)^2),
         D_after = sqrt((X_after)^2 + (Y_after)^2)) %>% 
  pivot_longer(cols = -c(id, embryo.id, direction), 
               names_to = 'period',
               values_to = 'value') %>% 
  separate(period, into = c('XY', 'period')) %>% 
  pivot_wider(names_from = XY, values_from = value) %>% 
  ungroup() %>% 
  mutate(X = X * 0.275,
         Y = Y * 0.275,
         D = D * 0.275,
         abs_X = abs(X),
         abs_Y = abs(Y),
         period = factor(period, levels = c('before', 'during', 'after'))) 

length(unique(displacement$id))
  
#labels <- as_labeller(c(`before` = 'Before activation', `during` = 'RhoA activation', `after` = 'RhoA deactivation' ))

# split direction plots -----

(Xplot <- displacement %>%
    ggplot(aes(x = period, y = X)) +
    geom_boxplot(outlier.shape=NA, size = 0.5, position = position_dodge(0.5), width = 0.5) +
    geom_jitter(aes(colour = factor(period), shape = direction), size = 2,  alpha = 0.7, width = 0.25, show.legend = F) +
    scale_colour_manual(values = c("before" = "grey30",
                                   "during" = "#fbb03b",
                                   "after" = "grey30")) +
    #scale_x_discrete(labels = labels) + 
    coord_flip() +
    labs(x = '', y = "ML Nuclear Displacement (µm)") +
   facet_wrap(~direction) +
    theme_classic(base_size = 18) + 
    theme(text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 18),
          panel.spacing = unit(0.5, 'lines')) 
)


 
 (Yplot <- displacement %>%
    ggplot(aes(x = period, y = Y)) +
    geom_boxplot(outlier.shape=NA, size = 0.5, position = position_dodge(0.5), width = 0.5) +
    geom_jitter(aes(colour = factor(period), shape = direction), size = 2,  alpha = 0.7, width = 0.25, show.legend = F) +
    scale_colour_manual(values = c("before" = "grey30",
                                 "during" = "#fbb03b",
                                 "after" = "grey30")) +
    #scale_x_discrete(labels = labels) + 
    scale_y_reverse() +
    labs(x = '', y = "AP Nuclear Displacement (µm)") +
    facet_wrap(~direction) +
    theme_classic(base_size = 18) + 
    theme(text = element_text(size = 20, family = 'Avenir'),
         strip.background = element_blank(),
         strip.text.x = element_text(size = 18),
         strip.text.y = element_text(size = 18),
         panel.spacing = unit(0.5, 'lines')) 
 )
 
(Dplot <- displacement %>%
    ggplot(aes(x = period, y = D)) +
    geom_boxplot(outlier.shape=NA, size = 0.5, position = position_dodge(0.5), width = 0.5) +
    geom_jitter(aes(colour = factor(period), shape = direction), size = 2,  alpha = 0.7, width = 0.25, show.legend = F) +
    scale_colour_manual(values = c("before" = "grey30",
                                   "during" = "#fbb03b",
                                   "after" = "grey30")) +
    labs(x = '', y = "Nuclear Displacement (µm)") +
    facet_wrap(~direction) +
    theme_classic(base_size = 18) + 
    theme(text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 18),
          panel.spacing = unit(0.5, 'lines')) 
)

# absolute displacement plots

(absXplot <- displacement %>%
    ggplot(aes(x = period, y = abs_X)) +
    geom_boxplot(outlier.shape=NA, size = 0.5, position = position_dodge(0.5), width = 0.5) +
    geom_jitter(size = 2,  alpha = 0.7, width = 0.25, aes(colour = factor(period)), show.legend = F) +
    scale_colour_manual(values = c("before" = "grey30",
                                   "during" = "#fbb03b",
                                   "after" = "grey30")) +
    #scale_x_discrete(labels = labels) + 
    coord_flip() +
    labs(x = '', y = "ML Nuclear Displacement (µm)") +
    theme_classic(base_size = 18) + 
    theme(text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 18),
          panel.spacing = unit(0.5, 'lines')) 
)



(absYplot <- displacement %>%
    ggplot(aes(x = period, y = abs_Y)) +
    stat_boxplot(geom ='errorbar', width = 0.25) +
    geom_boxplot(outlier.shape=NA, size = 0.5, position = position_dodge(0.5), width = 0.5) +
    geom_jitter(aes(colour = factor(period)), size = 2,  alpha = 0.7, width = 0.25, show.legend = F, height = 0) +
    scale_colour_manual(values = c("before" = "grey30",
                                   "during" = "#fbb03b",
                                   "after" = "#4296DE")) +
    #scale_x_discrete(labels = labels) + 
    labs(x = '', y = "Absolute AP Nuclear Displacement (µm)") +
    theme_classic(base_size = 18) + 
    theme(aspect.ratio = 1.5,
          text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 18),
          panel.spacing = unit(0.5, 'lines')) 
)




 # Test for normality 
 
 displacement %>% 
   group_by(period) %>% 
   shapiro_test(X, Y, D, abs_X, abs_Y) %>% 
   add_significance()
 

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
 
 qqnorm(displacement %>% 
        filter(period == 'after') %>% 
          pull(abs_Y))

 
 # Y and abs_Y has difference between variances
 
 # outcome is do repeated measures non-parametric anova - Friedman test + wilcoxon with bonferronni correction
 #(kruskal wallis and dunns for unpaired data)
 
 Xstats <- displacement %>% 
   friedman_test(X ~ period | id) %>% 
   add_significance() %>% 
   print()
 # not significant
 
 Ystats <- displacement %>% 
   friedman_test(Y ~ period | id) %>% 
   add_significance() %>% 
   print()
 
 Ystats <- displacement %>% 
   wilcox_test(Y ~ period, paired = TRUE, p.adjust.method = "bonferroni") %>% 
   add_significance() %>% 
   add_xy_position() %>% 
   print()

 Dstats <- displacement %>% 
   friedman_test(D ~ period | id) %>% 
   add_significance() %>% 
   print()
 
 Dstats <- displacement %>% 
   wilcox_test(D ~ period, paired = TRUE, p.adjust.method = "bonferroni") %>% 
   add_significance() %>% 
   add_xy_position()
 
 absXstats <- displacement %>% 
   friedman_test(abs_X ~ period | id) %>% 
   add_significance() %>% 
   print()
 
 absYstats <- displacement %>% 
   friedman_test(abs_Y ~ period | id) %>% 
   add_significance() %>% 
   print()
 
 absYstats <- displacement %>% 
   wilcox_test(abs_Y ~ period, paired = TRUE, p.adjust.method = "bonferroni") %>% 
   add_significance() %>% 
   add_xy_position() %>% 
   print()
 
 displacement %>% 
   group_by(period) %>% 
   get_summary_stats(abs_Y)
 
 
 # double checking the wilcox test with a t test
 # displacement %>% 
 #   group_by(period) %>% 
 #   t_test(Y ~ embryo.id)
 

# check for contribution of different embryos
 # library(lme4)
 # displacement
 # lmer(Y ~ period + (1|embryo.id), data = displacement)
 # 


Yplot <-  Yplot + stat_pvalue_manual(Ystats, label = "{p.adj.signif}", size = 8, hide.ns = TRUE, family = 'Avenir')
filename <- paste0(Sys.Date(), '_nuclei_APdisp_ant.png')
ggsave(filename, plot = Yplot, path = 'results/plots/paper/', width = 4, height = 6)

Dplot <-  Dplot + stat_pvalue_manual(Dstats, label = "{p.adj.signif}", size = 8, hide.ns = TRUE, family = 'Avenir')
filename <- paste0(Sys.Date(), '_nuclei_disp_ant.png')
ggsave(filename, plot = Dplot, path = 'results/plots/paper/', width = 4, height = 6)

absYplot <-  absYplot + stat_pvalue_manual(absYstats, label = "{p.adj.signif}", size = 8, hide.ns = TRUE, family = 'Avenir')
filename <- paste0(Sys.Date(), '_nuclei_AP-abs-disp.png')
ggsave(filename, plot = absYplot, path = 'results/plots/paper/', width = 3, height = 5)
filename <- paste0('results/plots/paper/', Sys.Date(), '_nuclei_AP-abs-disp.svg')
svglite(filename, width = 4, height = 6)
absYplot
dev.off()
