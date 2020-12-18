library(dplyr)
library(lubridate)
library(janitor)
library(highcharter)
library(data.table)


df <- data.table::fread("data/Transcript-Data.csv") %>%
  clean_names() %>%
  as.data.frame() %>%
  filter(workflow_completed_ended_on >= 1606780800000 & workflow_completed_ended_on <=1608246671000) %>%
  mutate(completed_date = lubridate::as_datetime(workflow_completed_ended_on / 1000)) %>%
  mutate(year_value = lubridate::year(completed_date)) %>%
  mutate(months_value = lubridate::month(completed_date)) %>%
  mutate(days_value = lubridate::wday(completed_date)) %>%
  mutate(days_ = lubridate::wday(completed_date, label = TRUE))s
