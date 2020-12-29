library(dplyr)
library(janitor)
library(lubridate)

df <- read.csv("data/Transcript-Data.csv") %>%
  clean_names() %>%
  filter(created_on >= 1604194922000 & created_on<= 1606480922000)
