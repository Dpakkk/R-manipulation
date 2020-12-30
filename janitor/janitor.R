library(janitor)
library(dplyr)
df <- mtcars
head(df)

clean <- clean_names(df)
colnames(clean) 

# count the numbers and write their percentage allocation in the dataset
ddf <- clean %>% tabyl(vs)

# count the numbers and view them as a percentage value with percentange sign

clean %>% 
  tabyl(vs) %>%
  adorn_pct_formatting(digits = 0, affix_sign = TRUE)

