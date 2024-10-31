# Load libraries
library(tidyverse)
library(openxlsx)
library(pdf2pptx)
library(viridis)  
library(rstatix)
library(svglite)

files <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/membranes/results/orientj' 
files <- list.files(files, pattern="*.csv", full.names=TRUE)
data_list <- list()
i <- 1

for (file in files){
  temp_data <- read_csv(file)
  date_id <- str_extract(file, '\\d{8}')
  embryo_id <- str_extract(file, '\\d{8}_[Ee]\\d_\\d{2}ss')
  embryo_id2 <- str_extract(file, '\\d{8}_[Ee]\\d_NA')
  embryo <- str_extract(file, '[Ee]\\d')
  somite_id <- str_extract(file, '\\d+ss')
  time <- str_extract(file, 't+\\d{2}')
  temp_data <- as.data.frame(temp_data) %>% 
    mutate(embryo.id = embryo_id,
           date = date_id,
           embryo = embryo,
           somite = somite_id,
           time = time,
           embryo.id2 = embryo_id2) 
  data_list[[i]] <- temp_data
  i <- i+1
}
i <- 1

orientation_data <- bind_rows(data_list) %>% as_tibble() %>% 
  mutate(embryo.id = ifelse(is.na(embryo.id2), embryo.id, embryo.id2)) %>%
  select(-embryo.id2) %>% 
  mutate(time = as.numeric(str_remove(time, 't')))


orientation_data %>%
  ggplot(aes(x = Orientation, y = Slice1, group = time, colour = time)) +
  geom_line() +
  facet_wrap(~embryo.id) +
  #theme_classic(base_size = 18) + 
  theme(aspect.ratio = 1,
        text = element_text(size = 20, family = 'Avenir'),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18),
        panel.spacing = unit(1, 'lines')) 

orientation_data %>% 
  select(embryo.id, time, Orientation, Slice1) %>% 
  mutate(Slice1 = as.integer(Slice1)) %>% 
  pivot_wider(id_cols = c(embryo.id, Orientation), names_from = time, values_from = Slice1) %>% 
  group_by(embryo.id) %>% 
  drop_na() %>% 
  summarise(test = ks.test(`29`,`36`)$p.value)

