# Load libraries
library(tidyverse)
library(openxlsx)
library(pdf2pptx)
library(viridis)  
library(rstatix)
library(svglite)

membrane_files <- '/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/membranes/results/membrane' 
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
           bg = 'og',
           embryo.id2 = embryo_id2) 
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
  embryo_id2 <- str_extract(file, '\\d{8}_[Ee]\\d_NA')
  embryo <- str_extract(file, '[Ee]\\d')
  somite_id <- str_extract(file, '\\d+ss')
  temp_data <- as.data.frame(temp_data) %>% 
    mutate(embryo.id = embryo_id,
           date = date_id,
           embryo = embryo,
           somite = somite_id,
           location = 'cyto',
           bg = 'og',
           embryo.id2 = embryo_id2) 
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
  embryo_id2 <- str_extract(file, '\\d{8}_[Ee]\\d_NA')
  embryo <- str_extract(file, '[Ee]\\d')
  somite_id <- str_extract(file, '\\d+ss')
  temp_data <- as.data.frame(temp_data) %>% 
    mutate(embryo.id = embryo_id,
           date = date_id,
           embryo = embryo,
           somite = somite_id,
           location = 'cyto',
           bg = 'og',
           embryo.id2 = embryo_id2) 
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
  embryo_id2 <- str_extract(file, '\\d{8}_[Ee]\\d_NA')
  embryo <- str_extract(file, '[Ee]\\d')
  somite_id <- str_extract(file, '\\d+ss')
  temp_data <- as.data.frame(temp_data) %>% 
    mutate(embryo.id = embryo_id,
           date = date_id,
           embryo = embryo,
           somite = somite_id,
           location = 'membrane',
           bg = 'sub',
           embryo.id2 = embryo_id2) 
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
  embryo_id2 <- str_extract(file, '\\d{8}_[Ee]\\d_NA')
  embryo <- str_extract(file, '[Ee]\\d')
  somite_id <- str_extract(file, '\\d+ss')
  temp_data <- as.data.frame(temp_data) %>% 
    mutate(embryo.id = embryo_id,
           date = date_id,
           embryo = embryo,
           somite = somite_id,
           location = 'cyto',
           bg = 'sub',
           embryo.id2 = embryo_id2) 
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
  embryo_id2 <- str_extract(file, '\\d{8}_[Ee]\\d_NA')
  embryo <- str_extract(file, '[Ee]\\d')
  somite_id <- str_extract(file, '\\d+ss')
  temp_data <- as.data.frame(temp_data) %>% 
  mutate(embryo.id = embryo_id,
           date = date_id,
           embryo = embryo,
           somite = somite_id,
           location = 'cyto',
           bg = 'sub',
           embryo.id2 = embryo_id2)
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
 # unite(col = 'embryo_id', c('date', 'embryo', 'somite'), remove = FALSE) %>% 
  mutate(embryo.id = ifelse(is.na(embryo.id2), embryo.id, embryo.id2)) %>%
  mutate(time = str_extract(Label, 't=\\d{2}'),
         time = as.numeric(str_remove(time, 't='))) %>% 
  group_by(embryo.id, location, bg, time) %>% 
  arrange(...1) %>% 
  mutate(cell = row_number()) %>% 
  unite(col = 'id', c('embryo.id', 'cell'), remove = FALSE) %>% 
  select(-Label, -Area, -...1, -Length, -StdDev, -Min, -Max, -Median, -X, -Y, -embryo.id2) %>% 
  ungroup() %>%
  group_by(embryo.id, bg, time, cell) %>% 
  pivot_wider(names_from = 'location', 
              values_from = c('Mean')) %>% #, 'StdDev', 'Min', 'Max', 'Median')) %>% 
  drop_na(membrane, cyto) %>%
  mutate(ratio = membrane / cyto,
         activation = case_when(time < 50 ~ 'act',
                                time > 49 ~ 'deact')) %>% 
  #drop_na() %>% 
  ungroup(time) %>% 
  mutate(norm_ratio_by_act = case_when(activation == 'act' ~ ratio / ratio[time == 26],
                                activation == 'deact' ~ ratio / ratio[time == 62]),
         norm_ratio = ratio / ratio[time == 26]) %>% 
  group_by(activation) %>% 
  mutate(time = (time - min(time) - 1) * 5) %>% 
  ungroup()

averaged_data <- data %>% 
  group_by(embryo.id, bg, cell) %>% 
  mutate(membrane = membrane / membrane[time == 0],
         cyto = cyto / cyto[time == 0]) %>% 
  group_by(time, bg, activation, embryo.id) %>% 
  summarise(ratio = mean(ratio),
            membrane = mean(membrane),
            cyto = mean(cyto),
            norm_ratio = mean(norm_ratio))

averaged_all_data <- averaged_data %>% 
  group_by(time, bg, activation) %>% 
  summarise(sem = sd(ratio) / n(),
            n = n(),
            ratio = mean(ratio),
            norm_sem = sd(norm_ratio) / n(),
            norm_ratio = mean(norm_ratio),
            membrane = mean(membrane),
            cyto = mean(cyto)
            )
  
lmmodel <- data %>% group_by(time, embryo.id) %>% do(model = lm(membrane ~ cell,  data=.)) %>% rowwise(lmmodel) %>% tidy(model)
lmmodel$model

  

# plots -----

# Checking individual cells
(data %>%
   filter(bg == 'sub', activation == 'act') %>% 
   ggplot(aes(x = time, y = norm_ratio, colour = id)) +
   geom_line() +
   labs(x = 'Time (s)', y = "Membrane / cytoplasm ratio") +
   theme_classic(base_size = 18) + 
   theme(aspect.ratio = 1,
         text = element_text(size = 20, family = 'Avenir'),
         strip.background = element_blank(),
         strip.text.x = element_text(size = 18),
         strip.text.y = element_text(size = 18),
         panel.spacing = unit(0.5, 'lines')) 
)

# Checking embryos
(averaged_data %>%
    filter(bg == 'sub', activation == 'act') %>% 
    ggplot(aes(x = time, y = norm_ratio, colour = embryo.id)) +
    geom_line() +
    labs(x = 'Time (s)', y = "Membrane / cytoplasm ratio") +
    theme_classic(base_size = 18) + 
    theme(text = element_text(size = 20, family = 'Avenir'),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 18),
          strip.text.y = element_text(size = 18),
          panel.spacing = unit(0.5, 'lines')) 
)

#deact.model <- (nls( norm_ratio ~ (a - b) * exp( r * time) + b, data = averaged_all_data %>% filter(bg == 'sub', activation == 'deact'), start = c(a = 1, b = 0, r = -0.1))) # original model but as we want time_constant we will fit that directly
deact.model <- (nls( norm_ratio ~ b + (a - b) * exp( -(time_constant^(-1)) * time), data = averaged_all_data %>% filter(bg == 'sub', activation == 'deact', time >=0), start = c(a = 1, b = 0, time_constant = 10)))
summary(deact.model) 
deact.model.label <- paste0('y = ', round(coef(deact.model)['b'], digit = 3), ' + (', round(coef(deact.model)['a'], digit = 3),' - ', round(coef(deact.model)['b'], digit = 3),') * exp(', round(-(coef(deact.model)['time_constant'])^(-1), digits = 3),' * x)')



act.model <- (nls( norm_ratio ~ b + (a - b) * (1 - exp( -(time_constant^(-1)) * time)), averaged_all_data %>% filter(bg == 'sub', activation == 'act', time >=0), start = c(a = 1, b = 0, time_constant = 10)))
summary(act.model)
act.model.label <- paste0('y = ', round(coef(act.model)['b'], digit = 3), ' + (', round(coef(act.model)['a'], digit = 3),' - ', round(coef(act.model)['b'], digit = 3),') * (1 - exp(', round(-(coef(act.model)['time_constant'])^(-1), digits = 3),' * x)')


#deact.time.constant <-  -1/coef(deact.coeff)['r'] #https://www.graphpad.com/guides/prism/latest/curve-fitting/reg_exponential_decay_1phase.htm
#act.time.constant <-  -1/coef(act.coeff)['r'] # https://www.graphpad.com/guides/prism/latest/curve-fitting/reg_exponential_association.htm

new.data <- data.frame(time = seq(min(averaged_all_data$time), max(averaged_all_data$time), by = 0.1))
deact.interval <- as_tibble(investr::predFit(deact.model, newdata = new.data, interval = "confidence")) %>% 
  mutate(time = new.data$time)

confint(deact.model)
act.interval <- as_tibble(investr::predFit(act.model, newdata = new.data, interval = "confidence")) %>% 
  mutate(time = new.data$time)

(plot <- averaged_all_data %>%
   filter(bg == 'sub', time >= 0) %>% 
   ggplot(aes(x = time, y = norm_ratio, group = activation)) +
   # geom_line(data = averaged_data %>% filter(activation == 'act'),
   #           aes(x = time, y = norm_ratio, group = embryo.id), colour = 'grey') +
   # geom_line(data = averaged_data %>% filter(activation == 'deact'),
   #           aes(x = time, y = norm_ratio, group = embryo.id), colour = 'grey') +
   # geom_line(data = averaged_all_data %>%
   #              filter(bg == 'sub'),
   #            aes(x = time, y = norm_ratio, group = activation)) +
    geom_ribbon(data = deact.interval %>% filter(time >= 0), aes(x=time, ymin=lwr, ymax=upr), alpha=0.5, inherit.aes=F, fill="grey") +
    geom_smooth(data = . %>% filter(activation == 'deact', time >= 0),
               aes(x = time, y = norm_ratio),
               method = "nls", se = FALSE,
                formula = y ~ (a-b) * exp(r * x) + b,
                method.args = list(start = c(a = 1, b = 0, r = -0.1)),
                color = "blue") +
    geom_ribbon(data = act.interval %>% filter(time >= 0), aes(x=time, ymin=lwr, ymax=upr), alpha=0.5, inherit.aes=F, fill="grey") +
    geom_smooth(data = . %>% filter(activation == 'act', time >= 0),
                aes(x = time, y = norm_ratio),
                method = "nls", se = FALSE,
                formula = y ~ b + (a-b) * (1-exp(-r * x)),
                method.args = list(start = c(a = 1, b = 0, r = 0.1)),
                colour = "#fbb03b") +
    #annotate("text", x = 50, y = 1.75, label = ) +
    geom_errorbar(aes(ymin=norm_ratio-norm_sem, ymax=norm_ratio+norm_sem), width = 1, alpha = 0.8) +
    geom_point(aes(colour = activation), size = 2) +
    scale_colour_manual(values = c('#fbb03b', 'blue')) +
   labs(x = 'Time (s)', y = "Membrane / cytoplasm ratio") +
   theme_classic(base_size = 18) + 
   theme(aspect.ratio = 1,
         legend.position="none", 
         text = element_text(size = 20, family = 'Avenir'),
         strip.background = element_blank(),
         strip.text.x = element_text(size = 18),
         strip.text.y = element_text(size = 18),
         panel.spacing = unit(0.5, 'lines')) 
)

filename <- paste0('results/plots/paper/', Sys.Date(), '_activation_curves.svg')
svglite(filename, width = 4, height = 6)
plot
dev.off()

# doing the above but for cytoplasm intensity not mem/cyto ratio

# deact.model <- (nls( cyto ~ (a - b) * exp( -(time_constant^(-1)) * time) + b, data = averaged_all_data %>% filter(bg == 'sub', activation == 'deact'), start = c(a = 1, b = 0, time_constant = 10)))
# summary(deact.model)
# 
# act.model <- (nls( cyto ~ b + (a - b) * (1 - exp( -(time_constant^(-1)) * time)), averaged_all_data %>% filter(bg == 'sub', activation == 'act'), start = c(a = 1, b = 0, time_constant = 10)))
# summary(act.model)

averaged_all_data %>%
  filter(bg == 'sub') %>% 
  ggplot(aes(x = time, y = cyto, group = activation)) +
  # geom_line(data = averaged_data %>% filter(activation == 'act'),
  #           aes(x = time, y = cyto, group = embryo.id), colour = 'grey') +
  # geom_line(data = averaged_data %>% filter(activation == 'deact'),
  #           aes(x = time, y = cyto, group = embryo.id), colour = 'grey') +
  # geom_line(data = averaged_all_data %>%
  #              filter(bg == 'sub'),
  #            aes(x = time, y = cyto, group = activation)) +
  geom_smooth(data = . %>% filter(activation == 'deact'),
              aes(x = time, y = cyto),
              method = "nls", se = FALSE,
              formula = y ~ (a-b) * exp(-r * x) + b,
              method.args = list(start = c(a = 1, b = 0, r = 0.1)),
              color = "black") +
  geom_smooth(data = . %>% filter(activation == 'act'),
              aes(x = time, y = cyto),
              method = "nls", se = FALSE,
              formula = y ~ b + (a-b) * (1-exp(-r * x)),
              method.args = list(start = c(a = 1, b = 0, r = 0.1)),
              color = "red") +
  geom_point(aes(colour = activation)) +
  scale_color_manual(values = c('red', 'black')) +
  #geom_errorbar(aes(ymin=cyto-norm_sem, ymax=cyto+norm_sem, colour = activation), width = 1) +
  labs(x = 'Time (s)', y = "Cytoplasm intensity") +
  theme_classic(base_size = 18) + 
  theme(aspect.ratio = 1,
        text = element_text(size = 20, family = 'Avenir'),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18),
        panel.spacing = unit(0.5, 'lines')) 



# gradient ---
(
  gradient <- data %>%
    group_by(time) %>% 
    filter(bg == 'sub', activation == 'act') %>% 
    ggplot(aes(y = membrane, x = cell, colour = embryo.id)) +
    #geom_line() +
    geom_smooth(method = 'lm', se = FALSE) +
    facet_wrap(~time)
)
