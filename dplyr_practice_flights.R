# import flight data super fast with readR
library(readr) # makes reading in csv files faster 
#setwd('/Users/hannahsmythe/Desktop/To_Publish_R/Replications/dplyr_flights/')

# dataset: US Airline delays, from Bureau of Transportation Stats
flights <- read_csv("737274986_T_T100D_MARKET_US_CARRIER_ONLY.csv")
flights # with readr, shows you just first 10 rows without needing head()

# how many unique carriers are there? 
flights %>% 
    dplyr::summarize(unique_carriers = n_distinct(UNIQUE_CARRIER))

# how many flights does each unique carrier have running late? 
flights %>%
    group_by(UNIQUE_CARRIER_NAME) %>%
    dplyr::summarize(counts = n())

# get column names
colnames(flights)

# remove cols you don't need 
flights %>% 
    select(-X8)
##########################################
# data from bureau of transport stats
# all flights that departed from NYC in 2013
#install.packages('nycflights13')
library(nycflights13)
dim(flights)
class(flights)
flights # tibbles, a modern reimagine of df, only print first 10 rows; dont need head()
# can convert df to tibble using as_tibble()

# filter down to flights on December 26
filter(flights, month == 12, day == 26)

# sort by year, month, day (year first, then month, then day)
arrange(flights, year, month, day)

# descending order (most recent first)
arrange(flights, desc(arr_delay))

# select only day, month, year cols
select(flights, year, month, day)

# select all cols between year and day (inclusive)
select(flights, year:day)

# select all cols except those from year to day (inclusive)
select(flights, -(dep_time:arr_time))
flights

# reaname some vars

# since select drops all cols except those named, not always useful
select(flights, YEAR=year) 
flights


# add new cols with mutate()
mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance/air_time * 60)

# if you oonly want to keep the new vars, use transmute()
transmute(flights,
          gain = arr_delay - dep_delay,
          gain_per_hour = gain/ (air_time / 60)) 

# sample fraction of dat
sample_frac(flights, .01)
?sample_frac()


flights %>%
    dplyr::arrange(year, month, day) %>%
    dplyr::sample_n(100)


by_tailnum <- group_by(flights, tailnum) # split data into individual planes
delay <- dplyr::summarize(by_tailnum,
                          count = n(),
                          dist = mean(distance, na.rm = TRUE),
                          delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 2, dist < 2000)

ggplot(delay, aes(dist, delay)) + 
    geom_point(aes(size=count), alpha=.5) + 
    geom_smooth() + 
    scale_size_area()

# summary peels off one level of the grouping 
daily <- group_by(flights, year, month, day)
(per_day <- dplyr::summarize(daily, flights=n()))
(per_month <- dplyr::summarize(per_day, flights=sum(flights)))
(per_year <- dplyr::summarize(per_month, flights=sum(flights)))


# select
vars <- c('year', 'month')
select(flights, vars, "day") # selects year, month, day

df <- select(flights, year:dep_time)
# add col for years 10 years in future
mutate(df, year + 10)
df