#clean manipulation
####################################################
#data import
df_clean <- head(df, 5000) %>%
  as.data.frame() %>%
  janitor::clean_names() %>%
  mutate(created_on_date = lubridate::as_datetime(created_on / 1000))
  
colnames(df_clean)
#####################################################

# total audio volume
total_audio_volume <- df_clean %>%
  sum(df_clean$x_audio_length_seconds, na.rm = TRUE)

summary(df_clean$x_audio_length_seconds)

total_audio_volume <- df_clean %>%
  summarise(total_audio = sum(x_audio_length_seconds/3600, na.rm = TRUE))
total_audio_volume

#######################################################
head(df_clean)
