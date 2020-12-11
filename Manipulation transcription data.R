library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(lubridate)
library(tidyr)

df <- read.csv('data/Transcript-data.csv')

df_clean <- head(df, 5000) %>%
  as.data.frame() %>%
  janitor::clean_names() %>%
  mutate(created_on_date = lubridate::as_datetime(created_on / 1000))
head(df_clean)



#total audio volume
total_audio_volume <- df_clean %>%
  summarise(total_volume =  round(sum(x_audio_length_seconds/3600, na.rm = TRUE)))
total_audio_volume  


#average daily audio
avg_daily_audio <- df_clean %>%
  summarise(avg_daily_audio_volume = sum(x_audio_length_seconds/5, na.rm = TRUE))
avg_daily_audio

#average Accuracy
avg_accuracy <- df_clean %>%
  summarise(avg_accuracy = mean(accuracy, na.rm = TRUE) * 100)
avg_accuracy


#average audio length
avg_audio_length <- df_clean %>%
  summarise(avg_audio_length = mean(x_audio_length_seconds/60, na.rm = TRUE))
avg_audio_length

#average word count
avg_word_count <- df_clean %>%
  summarise(avg_word_count = mean(word_count, na.rm = TRUE))
avg_word_count

#Machine and Manual Transcription
mac_tx <- df_clean %>%
  dplyr::mutate(used_machine_tx = replace_na(used_machine_tx, FALSE)) %>%
  janitor::tabyl(used_machine_tx) %>%
  mutate(percent = percent * 100) %>%
  mutate(used_machine_tx = replace(used_machine_tx, used_machine_tx == TRUE, "Machine Transcription")) %>%
  mutate(used_machine_tx = replace(used_machine_tx, used_machine_tx == FALSE, "Manual Transcription"))
  
plotly::plot_ly(mac_tx,labels = ~used_machine_tx, values = ~n, type = "pie") %>%
  layout(
    title = "Transcription Distribution",
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
  ) %>%
  config(displayModeBar = F)

#Accuracy tables insights(Time Taken and Accuracy)

#For Time taken
time_machine <- df_clean %>%
  dplyr::mutate(used_machine_tx = replace_na(used_machine_tx, FALSE)) %>%
  filter(used_machine_tx == TRUE) %>%
  summarise(sum(qf_end_time_in_seconds) / sum(x_audio_length_seconds))
time_machine

times_manual <- df_clean %>%
  dplyr::mutate(used_machine_tx = replace_na(used_machine_tx, FALSE)) %>%
  filter(used_machine_tx == FALSE) %>%
  summarise(sum(qf_end_time_in_seconds, na.rm = TRUE) / sum(x_audio_length_seconds, na.rm = TRUE))
times_manual
# Value might deviate due to outliers in data

overal_average <- df_clean %>%
  summarise(sum(qf_end_time_in_seconds, na.rm = TRUE)/sum(x_audio_length_seconds, na.rm = TRUE))
overal_average

avg_audio_review_time <- df_clean %>%
  summarise(sum(audio_review_end_time_in_seconds, na.rm = T) / sum(x_audio_length_seconds, na.rm = T))
avg_audio_review_time

#For Accuracy
machine_accuracy <- df_clean %>%
  filter(used_machine_tx == TRUE) %>%
  summarise(round(mean(accuracy, na.rm = TRUE) * 100, 2))
machine_accuracy  

manual_accuracy <- df_clean %>%
  filter(is.na(used_machine_tx)) %>%
  summarise(round(mean(accuracy, na.rm = T),2))
manual_accuracy

average_accuracy <- df_clean %>%
  summarise(round(mean(accuracy, na.rm = T) * 100,2))
average_accuracy


#spot award section

#qf
most_qf_volume <- df_clean %>%
  filter(accuracy > .85) %>%
  group_by(workflow_quickfire_user) %>%
  summarise(total_volume = sum(x_audio_length_seconds, na.rm = T) / 60) %>%
  arrange(desc(total_volume)) %>%
  head(10) %>%
  rename("Transcriber" = workflow_quickfire_user)

most_qf_volume
#ar
most_ar_vloume <- df_clean %>%
  filter(accuracy > 0.85) %>%
  group_by(workflow_peer_audio_review_user) %>%
  summarise(total_volume = sum(x_audio_length_seconds, na.rm = T)) %>%
  arrange(-total_volume) %>%
  head(10)
most_ar_vloume  

#fastest machine transcriber
fast_machine_transcriber <- df_clean %>%
  filter(qf_end_time_in_seconds < 7200, word_count >15, used_machine_tx == TRUE) %>%
  group_by(workflow_quickfire_user) %>%
  summarise(tx_time = round(sum(qf_end_time_in_seconds, na.rm = T) / sum(x_audio_length_seconds, na.rm = T), 2), audio_user_time = sum(x_audio_length_seconds, na.rm = T)) %>%
  filter(tx_time > 1800) %>%
  arrange(tx_time) %>%
  head(10)
fast_machine_transcriber

#best accuracy
best_accuracy <- df_clean %>%
  filter(accuracy > .85) %>%
  group_by(workflow_quickfire_user) %>%
  summarise(top_accuracy = mean(accuracy), audio_sum = sum(x_audio_length_seconds)) %>%
  arrange(-top_accuracy) %>%
  arrange(-audio_sum)
  head(10)
best_accuracy






