

df_test <- directed_data %>% 
  filter(measure == "vel",
         position == "E",
         embryo.id == "20221214_E3_14ss") %>% 
  select(timepoint, mean) 

df_test %>% 
  ggplot(aes(x = timepoint, y = mean)) +
  geom_line()


for(time_step in 1:10){
  df_test$diff <- c(rep(NA, time_step),
                    df_test$mean[(1+time_step):nrow(df_test)] /
                      df_test$mean[1:(nrow(df_test) - time_step)])
  
  print({df_test %>% 
    drop_na() %>% 
    ggplot(aes(x = timepoint, y = diff)) +
    geom_line()})
}
time_step <- 1

