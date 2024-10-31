# Load libraries
library(tidyverse)
library(openxlsx)
library(viridis)  
library(svglite)
library(rstatix)
library(ggpubr)

nuclei_density_data <- read.xlsx('/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/nuclei/nuclei-density.xlsx')

nuclei_density <- nuclei_density_data %>% 
  filter(is.na(note) == TRUE) %>% 
  select(-note) %>% 
  pivot_longer(cols = c(anterior, posterior, roi), names_to = 'region') %>% 
  mutate(region = factor(region, levels = c('anterior', 'roi', 'posterior'), labels = c('north', 'roi', 'south')),
         roi.area = round(roi.area, digits = 3),
         density = value / roi.area * 1000) # density. = nuclei per 1000µm^2

# Quick look at the data ----
ggplot(nuclei_density, aes(x=region, y=density, colour = direction)) +
  geom_boxplot() +
  geom_point(position =   position_dodge(width=0.75)) 
  #geom_path(aes(group = embryo.id),  position =   position_dodge(width=0.75) )


nuclei_density %>%
  group_by(region) %>% 
  get_summary_stats()

# Filter data to exclude unwanted embryos ----

nuclei_density <- nuclei_density %>% 
 # filter(region != 'roi', !(embryo.id %in% c('20221018_E1_15ss', '20240503_E7_21ss', '20230315_E5_23ss')))
  filter(region != 'roi', !(embryo.id %in% c('20221018_E1_15ss'))) # exclude as one region did not fully cover tissue


 # Test for normality 
 
nuclei_density %>% 
   group_by(region) %>% 
  group_by(direction) %>%
   shapiro_test(value, density)

# has normal distribution
 

 # Testing for equal variance (for non normal data =leveneTest, for normal data = bartlett.test)

nuclei_density %>%
  group_by(direction) %>%
   levene_test(value ~ region)
 
nuclei_density %>%
  group_by(direction) %>%
  levene_test(density ~ region)
 
 # no difference between variances
 
 # outcome is do parametric rm anova 
 
df <- nuclei_density %>% 
  filter(region != 'roi', embryo.id != '20221018_E1_15ss') %>% 
  group_by(direction) %>% 
   anova_test(dv = density, wid = embryo.id, within = region) %>% 
   add_significance() 

stats <- nuclei_density %>% 
  filter(region != 'roi', embryo.id != '20221018_E1_15ss') %>% 
  group_by(direction) %>% 
  t_test(density ~ region, paired = TRUE, var.equal = T) %>% 
  add_significance() %>% 
  add_y_position()

stats_pvalue <- stats %>% select(direction, p, y.position)

# Plot for Figure 7 ----

(plot <- ggplot(nuclei_density, aes(x=region, y=density)) +
    geom_boxplot(aes(fill = direction), alpha = 0.2) +
    geom_point(aes(colour = direction)) +
    geom_line(aes(group = embryo.id, colour = direction)) +
    geom_text(data = stats_pvalue, family="Avenir", 
              aes(x = 1.5, y = y.position, label = paste0('p = ', p))) +
    facet_wrap(~direction) +
    theme_classic(base_size = 18) +
    theme(aspect.ratio = 2,
          legend.position="none",
          text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          panel.spacing = unit(0.5, 'lines')) +
    ylab('Nuclear density
(per 1000µm2)') +
    xlab("") +
    stat_pvalue_manual(stats, label = "p.signif", size = 5, family = 'Avenir', bracket.nudge.y = 0.4)
  )

filename <- paste0('results/plots/paper/', Sys.Date(), '_nuclear_density.svg')
svglite(filename, width = 6, height = 5)
plot
dev.off()



