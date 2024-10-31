# 01_PIV_data_tidy
# Helena's code to generate tidy data from PIV CNSEW data - from Matlab function grabNSWECdataHC
# Dec 2023
# # = normal comments, '##' = notes to myself

# Remember to open PIV-Analysis.Rproj first to set the wd - not the script first

# Load libraries
library(tidyverse)
library(openxlsx)
library(circular)
library(pdf2pptx)

#-------------------

wd <- getwd()

# Read in timelapse data annotations / metadata ---------
metadata <- read.xlsx('/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/light-patterning-experiment-metadata.xlsx')

# Create a data frame containing the PIV data (output of grabNSWECdataHC - mean and std of different measures in different cardinal direction around the ROI)

# note: the vang and aniang from Dec2023 and earlier are currently inaccurate due to not using circular statistics to generate mean and std 

#file1 <- 'data/2022-10-18/E1_new_15ss_470nm_TL_region1.tif/xlsx_data/E1_new_15ss_470nm_TL_region1.tif.xlsx'
file2 <- 'data/2022-11-08/E8_15ss_470nm_TL_.tif/results_sp_5_tf_1/PIV/xlsx_data/E8_15ss_470nm_TL_.tif.xlsx'
file3 <- 'data/2022-12-14/E4_15ss_470nm_TL_.tif/results_sp_5_tf_1/PIV/xlsx_data/E4_15ss_470nm_TL_.tif.xlsx'
file4 <- 'data/2023-08-04/E1_approx21ss_470nm_TL_.tif/results_sp_5_tf_1/PIV/xlsx_data/E1_approx21ss_470nm_TL_.tif.xlsx'
file5 <- 'data/2023-09-01/E2_470nm_6m-TL_.tif/xlsx_data/E2_470nm_6m-TL_.tif.xlsx'
file6 <- 'data/2023-09-01/E5_470nm_6m-TL_.tif/xlsx_data/E5_470nm_6m-TL_.tif.xlsx'
file7 <- 'data/2023-09-01/E6_470nm_6m-TL_.tif/xlsx_data/E6_470nm_6m-TL_.tif.xlsx'
file8 <- 'data/2023-10-26/E1_16ss_6m_470nm_TL_.tif/xlsx_data/E1_16ss_6m_470nm_TL_.tif.xlsx'
file9 <- 'data/2023-10-26/E3_6m_470nm_TL_.tif/xlsx_data/E3_6m_470nm_TL_.tif.xlsx'
file10 <- 'data/20221214_E2_13ss/xlsx_data_new/20221214_E2_13ss.xlsx'
file11 <- 'data/20221214_E3_14ss/xlsx_data_new/20221214_E3_14ss.xlsx'
file12 <- 'data/20221018_E1_15ss/xlsx_data/20221018_E1_15ss.xlsx'
file13 <- 'data/20221214_E5_12ss/xlsx_data_new/20221214_E5_12ss.xlsx' # same as file1

file_names <- c(file2, file3, file4, file5, file6, file7, file8, file9, file10, file11, file12, file13)  
data_list <- list()
i <- 1


# Loop to read in data files, add identifying information and combine into tibble called data ------

for (file in file_names){
  temp_data <- read.xlsx(file)
  to_keep <- rowSums(temp_data, na.rm = T) != 0
  date_id <- str_extract(file, '\\d{8}') ## should/could this just be \d{8} ??
  date_id_2 <- str_extract(file, '\\b\\d{4}-\\d{2}-\\d{2}\\b')
  embryo_id <- str_extract(file, '[Ee]\\d')
  somite_id <- str_extract(file, '\\d+ss')
  temp_data <- as.data.frame(temp_data) %>% 
    mutate(date = date_id,
           date_2 = date_id_2,
           embryo = embryo_id,
           somite = somite_id,
           timepoint = (1:n()) - 200L)
  data_list[[i]] <- temp_data[to_keep,]
  i <- i+1
}
i <- 1

data <- bind_rows(data_list) %>% as_tibble()

# Troubleshooting loop to check if files exist - keep commented unless loop above fails
# for (file in file_names){
#   file <- file.path(wd, file)
#   print(file)
#   exists <- file.exists(file)
#   print(exists)
#   }


# Put the data into a tidy format and add metadata ---------
data <- data %>% 
  mutate(date_2 = format(as.Date(date_2), "%Y%m%d")) %>% 
  mutate(date = coalesce(date, date_2)) %>% 
  select(-'date_2') %>% 
  pivot_longer(cols = c(-'date', -'embryo', -'somite', -'timepoint'), 
               names_to = 'to_split', 
               values_to = 'value') %>% 
  separate(to_split, c('position', 'measure', 'statistic'), sep = '_') %>% 
  pivot_wider(names_from = 'statistic', values_from = 'value') %>% 
  mutate(position = factor(position, levels = c('C', 'N', 'S', 'E', 'W'))) %>% 
  unite(col = 'embryo.id', c('date', 'embryo', 'somite'), remove = FALSE) 

# Make the embryo.id column in metadata the same as data
metadata <- metadata %>%
  mutate(embryo.id = gsub('-', '', embryo.id)) %>% 
  unite(col = 'embryo.id', c('embryo.id', 'embryo.age'), remove = FALSE)

# Add timelapse specific metadata to the data
timelapse_metadata <- metadata %>% 
  # filter is specific for the 6min TL 
  filter(file.type == 'timelapse', timelapse.length.sec == '360', further.analysis %in% c('yes', 'maybe')) %>% 
  select(embryo.id, timelapse.length.sec, timestep.sec, total.timepoints,	activation.start.tp,	activation.end.tp)

data <- data %>% 
  left_join(timelapse_metadata, by = 'embryo.id', keep = FALSE)  %>% 
  mutate(timepoint = timepoint * timestep.sec) %>% # might need to change this for old data
  mutate(align.activation.start =  0,  # Guillermo aligned the PIV data so activation start = 0
         align.activation.end = 5 * (as.numeric(activation.end.tp) - as.numeric(activation.start.tp) + 1) )

## double check this is aligned properly in fiji by looking at the timelapse e.g. should i add the 1??
## unique(data[,c('embryo.id', 'align.activation.end')]) 

#



# Write to file -------

if (dir.exists('data/tidy_data') == FALSE) {
dir.create('data/tidy_data')
}

write.xlsx(data, 'data/tidy_data/tidy_data_by_embryo')
