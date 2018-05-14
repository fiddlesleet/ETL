library(dplyr)
library(ggplot2)


df.car_torque <- read.csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_torque_DATA.txt"))
df.car_0_60_time <- read.csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_0-60-times_DATA.txt"))
df.car_engine_size <- read.csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_engine-size_DATA.txt"))
df.car_horsepower <- read.csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_horsepower_DATA.txt"))
df.car_top_speed <- read.csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_top-speed_DATA.txt"))
df.car_power_to_weight <- read.csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_power-to-weight_DATA.txt"))

# inspect data
head(df.car_torque)
head(df.car_0_60_time)
head(df.car_engine_size)
head(df.car_horsepower)
head(df.car_top_speed)
head(df.car_power_to_weight)

conflicts()
# filter out duplicate records

# find duplicates
find_duplicates <- function(df) {
    df %>%
        group_by(car_full_nm) %>%
        dplyr::summarize(count=n()) %>%
        filter(count!=1)
}

find_duplicates(df.car_torque)
find_duplicates(df.car_engine_size)
find_duplicates(df.car_0_60_time)
find_duplicates(df.car_horsepower)
find_duplicates(df.car_top_speed)
find_duplicates(df.car_power_to_weight)

# remove duplicates
remove_duplicates <- function(df) {
    df %>% 
        distinct(car_full_nm)
}

remove_duplicates(df.car_torque)
remove_duplicates(df.car_engine_size)
remove_duplicates(df.car_0_60_time)
remove_duplicates(df.car_horsepower)
remove_duplicates(df.car_top_speed)
remove_duplicates(df.car_power_to_weight)

join_to <- function(master, joiner) {
    left_join(master, joiner, by="car_full_nm")
}
# join data
df.car_specs <- join_to(df.car_horsepower, df.car_torque)
df.car_specs <- join_to(df.car_specs, df.car_0_60_time)
df.car_specs <- join_to(df.car_specs, df.car_engine_size)
df.car_specs <- join_to(df.car_specs, df.car_top_speed)
df.car_specs <- join_to(df.car_specs, df.car_power_to_weight)

# test for duplicates
find_duplicates(df.car_specs)
remove_duplicates(df.car_specs)

# add year var
df.car_specs <- mutate(df.car_specs, year=sub(".*\\[([0-9]{4})\\]","\\1",car_full_nm))
head(df.car_specs)

# new var: decade
df.car_specs <- mutate(df.car_specs,
                       decade = as.factor(
                           ifelse(substring(df.car_specs$year,1,3)=='193','1930s',
                            ifelse(substring(df.car_specs$year,1,3)=='194','1940s',
                            ifelse(substring(df.car_specs$year,1,3)=='195','1950s',
                            ifelse(substring(df.car_specs$year,1,3)=='196','1960s',
                            ifelse(substring(df.car_specs$year,1,3)=='197','1970s',
                            ifelse(substring(df.car_specs$year,1,3)=='198','1980s',
                           ifelse(substring(df.car_specs$year,1,3)=='199','1990s',
                           ifelse(substring(df.car_specs$year,1,3)=='200','2000s',
                           ifelse(substring(df.car_specs$year,1,3)=='201','2010s',"ERROR"
                          )))))))))
                       )
)

head(df.car_specs)
sample_n(df.car_specs, 50)

# make brand name of car col
# grabs first word from col (last var in function)
df.car_specs <- mutate(df.car_specs, make_nm = gsub(" .*$","", df.car_specs$car_full_nm))

# make car_weight in tons var
df.car_specs <- mutate(df.car_specs, car_weight_tons = horsepower_bhp / horsepower_per_ton_bhp)

# make torque_per_ton var
df.car_specs <- mutate(df.car_specs, torque_per_ton = torque_lb_ft / car_weight_tons)
head(df.car_specs)

# check decade var
df.car_specs %>% 
    group_by(decade) %>%
    dplyr::summarize(count=n())

# make frequency table 

df.car_specs %>%
    group_by(make_nm) %>%
    dplyr::summarize(make_count = length(make_nm)) %>%
    arrange(desc(make_count))




#################______________ Part 2 __________________

df.car_spec_data <- read.csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2015/01/auto-snout_car-specifications_COMBINED.txt"))
str(df.car_spec_data)

df.car_spec_data$year <- as.character(df.car_spec_data$year)

#--------------
# Create Theme
#--------------

# BASIC THEME
theme.car_chart <- 
    theme(legend.position = "none") +
    theme(plot.title = element_text(size=26, family="Trebuchet MS", face="bold", hjust=0, color="#666666")) +
    theme(axis.title = element_text(size=18, family="Trebuchet MS", face="bold", color="#666666")) +
    theme(axis.title.y = element_text(angle=0)) 


# SCATTERPLOT THEME
theme.car_chart_SCATTER <- theme.car_chart +
    theme(axis.title.x = element_text(hjust=0, vjust=-.5))

# HISTOGRAM THEME
theme.car_chart_HIST <- theme.car_chart +
    theme(axis.title.x = element_text(hjust=0, vjust=-.5))

# SMALL MULTIPLE THEME
theme.car_chart_SMALLM <- theme.car_chart +
    theme(panel.grid.minor = element_blank()) +
    theme(strip.text.x = element_text(size=16, family="Trebuchet MS", face="bold", color="#666666"))    

#-------------------------
# Horsepower vs. Top Speed
#-------------------------

ggplot(data=df.car_spec_data, aes(x=horsepower_bhp, y=top_speed_mph)) +
    geom_point(alpha=.4, size=4, color="#880011") +
    ggtitle("Horsepower vs. Top Speed") +
    labs(x="Horsepower, bhp", y="Top Speed,\n mph") +
    theme.car_chart_SCATTER

#------------------------
# Histogram of Top Speed
#------------------------

ggplot(data=df.car_spec_data, aes(x=top_speed_mph)) +
    geom_histogram(fill="#880011") +  
    ggtitle("Histogram of Top Speed") +
    labs(x="Top Speed, mph", y="Count\nof Records") +
    theme.car_chart_HIST

#----------------------------------
# ZOOM IN ON SPEED CONTROLLED CARS
#
# What is the 'limited' speed?
#  (create bar chart)
#----------------------------------

df.car_spec_data %>%
    filter(top_speed_mph >149 & top_speed_mph <159) %>%
    ggplot(aes(x= as.factor(top_speed_mph))) +
    geom_bar(fill="#880011") +
    labs(x="Top Speed, mph") +
    theme.car_chart

#------------------------
# Histogram of Top Speed
#  By DECADE
#------------------------

ggplot(data=df.car_spec_data, aes(x=top_speed_mph)) +
    geom_histogram(fill="#880011") +
    ggtitle("Histogram of Top Speed\nby decade") +
    labs(x="Top Speed, mph", y="Count\nof Records") +
    facet_wrap(~decade) +
    theme.car_chart_SMALLM

#-------------------------------
# TABLE OF CAR COMPANIES WITH 
#  CARS AT MAX SPEED = 155
#-------------------------------
df.car_spec_data %>%
    filter(top_speed_mph == 155 & year>=1990) %>%
    group_by(make_nm) %>% 
    summarize(count_speed_controlled = n()) %>%
    arrange(desc(count_speed_controlled))

#-----------------------------
# Top Speed vs Year (all cars)
#-----------------------------
ggplot(data=df.car_spec_data, aes(x=year, y=df.car_spec_data$top_speed_mph)) +
    geom_point(alpha=.35, size=4.5, color="#880011", position = position_jitter()) +
    scale_x_discrete(breaks = c("1950","1960","1970","1980","1990","2000","2010")) +
    ggtitle("Car Top Speeds by Year") +
    labs(x="Year" ,y="Top Speed\nmph") +
    theme.car_chart_SCATTER

#------------------------------------------
# PLOT: Maximum Speed (fastest car) by Year
#------------------------------------------

df.car_spec_data %>%
    group_by(year) %>%
    summarize(max_speed = max(top_speed_mph, na.rm=TRUE)) %>%
    ggplot(aes(x=year,y=max_speed,group=1)) + 
    geom_point(size=5, alpha=.8, color="#880011") +
    stat_smooth(method="auto",size=1.5) +
    scale_x_discrete(breaks = c("1950","1960","1970","1980","1990","2000","2010")) +
    ggtitle("Speed of Year's Fastest Car by Year") +
    labs(x="Year",y="Top Speed\n(fastest car)") +
    theme.car_chart_SCATTER

#----------------------
# Bar Chart
#  top 10 fastest cars
#----------------------
df.car_spec_data %>%
    select(car_full_nm,top_speed_mph) %>%
    filter(min_rank(desc(top_speed_mph)) <= 10) %>%
    arrange(desc(top_speed_mph)) %>%
    ggplot(aes(x=reorder(car_full_nm,top_speed_mph), y=top_speed_mph)) +
    geom_bar(stat="identity",fill="#880011") +
    coord_flip() +
    ggtitle("Top 10 Fastest Cars (through 2012)") +
    labs(x="",y="") +
    theme.car_chart +
    theme(axis.text.y = element_text(size=rel(1.5))) +
    theme(plot.title = element_text(hjust=1))

