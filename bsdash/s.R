# library loading
library(plyr)
library(dplyr)
library(hflights)


# Data reading
data(hflights)
head(hflights)


# converting dataframe to local dataframe
flights <- tbl_df(hflights)
# printing only the required dataframe according to the output screen
flights
 
print(flights, n=20)
data.frame(head(flights))
head(flights)

# These are are applied by dplyr function not BASE function
# adding a filter in the dataframe
filter(flights, Month == 1, DayofMonth == 1)

# Filter from dplyr function
flights %>% 
  filter(Month == 1, DayofMonth == 1) %>%
  select(Year:ArrTime, TailNum)
  
# selecting a required features from dataframe
select(flights,DepTime, ArrTime,FlightNum)
# selecting a specific featured
# from flight df select from year to dayofmonth and also keep rows which contains Taxi
select(flights, Year:DayofMonth, contains("Taxi"))


# Chaining method for using filter and select combined way
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  filter(DepDelay > 60)

# sorting using dplyr function
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(DepDelay)
# sorting in descending order
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(desc(DepDelay))

# Adding a new variable in a df -> Mutate
# Mutate based on dplyr library
# create new variable based on function of existing varibles
#mutate by selceting the specific variabel
data.frame(head(flights))


flights %>%
  select(Distance, AirTime) %>%
  mutate(speed = Distance/AirTime*60)
# mutate in a whole dataframe

flights <- flights %>% 
  mutate(speed = Distance/AirTime*60)

# checking the dataframe
data.frame(head(flights))


# summarize the dataframe group by function like in python
flights %>%
  group_by(Dest) %>%
  summarize(avg_delay = mean(ArrDelay, na.rm = TRUE))

flights %>%
  group_by(UniqueCarrier) %>%
  summarise_each(funs(mean),Cancelled, Diverted)


# for each carrier, calculate the minimum and maximum arrival and departure delays
flights %>% 
  group_by(UniqueCarrier) %>%
  summarise_each(funs(min(., na.rm = TRUE), max(., na.rm = TRUE)), matches("Delay"))


# its time to introduce the new function called helper function '''n()'''
# this helper function counts the number of rows in a group
# Helper function '''n_distinct()''' counts the number of unique items in a vector

# for each day of the year, count the total number of flights and sort in descending order
data.frame(head(flights))
  
flights %>%
    group_by(Month, DayofMonth) %>%
    summarise(flight_count = n()) %>%
    arrange(desc(flight_count))

flights %>%
  group_by(Month,UniqueCarrier) %>%
  summarise(Unique_Count=n()) %>%
  arrange(desc(Unique_Count))


# rewriting the above code using the '''Tally function'''
flights %>%
  group_by(Month, DayofMonth) %>%
  tally(sort = T)

# for each destination, count the total number of flights and the
#number of distinct planes that flew there
flights %>%
  group_by(Dest) %>%
  summarise(flight_count = n(), plane_count = n_distinct(TailNum))

data.frame(head(flights))  


# for each destination, show the number of cancelled and not cancelled flights
flights %>%
  group_by(Dest) %>%
  select(Cancelled)  %>%
  table() %>%
  head()
  

# for each carrier, calculate which two days of the year they 
# had their longest departure delays
# note: smallest (not largest) value is ranked as 1, so you have to use ' desc
# to rank by largest value

flights %>%
  group_by(UniqueCarrier) %>%
  select(Month, DayofMonth, DepDelay) %>%
  filter(min_rank(desc(DepDelay)) <=2) %>%
  arrange(UniqueCarrier, desc(DepDelay))

# rewriting above code with '''top_n''' function

flights %>%
  group_by(UniqueCarrier) %>%
  select(Month, DayofMonth, DepDelay) %>%
  top_n(2) %>%
  arrange(UniqueCarrier, desc(DepDelay))

# for each month, calculate the number of flights and the
# change from the previous month

flights %>%
  group_by(Month) %>%
  summarise(flight_count = n()) %>%
  mutate(change = flight_count - lag(flight_count))

# simplifying above function
flights %>%
  group_by(Month) %>%
  tally() %>%
  mutate(change = n - lag(n))

# other convinence functions
# sampling from flight data

flights %>% sample_n(5)

# randomly sample fraction of rows, without replacement
flights %>% sample_frac(0.25, replace = TRUE)

# view the structure of objects
# this is base function
str(flights)
#dplyr approach:
glimpse(flights)







