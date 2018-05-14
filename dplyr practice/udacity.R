getwd()
setwd('~/Downloads')
#install.packages('ggthemes', dependencies = TRUE) 
reddit <- read.csv('reddit.csv')
install.packages('GGally') #scatterplot matrix
table(reddit$employment.status)
summary(reddit)
names(reddit)
library(plotly)
library(ggplot2)
library(ggthemes) 
library(gridExtra) 
library(dplyr)
theme_set(theme_minimal(24)) 


fb <- read.csv('pseudo_facebook.tsv', sep = '\t')
age_groups <- group_by(fb, age)
fb.fc_by_age <- summmarise(age_groups,
                           friend_count_mean = mean(friend_count),
                           friend_count_median = median(friend_count),
                           n = n())
fb.fc_by_age <- arrange(fb.fc_by_age, age_groups)
head(fb.fc_by_age)

#same as
fb.fc_by_age <- fb %>%
  group_by(age) %>% 
  summmarise(friend_count_mean = mean(friend_count),
             friend_count_median = median(friend_count),
             n = n()) %>% 
  arrange(age)
head(fb.fc_by_age)

ggplot(aes(x = dob_day), data = fb) + 
  geom_histogram(binwidth = 1) + 
  scale_x_continuous(breaks = 1:31)
names(fb)
qplot(x=dob_day, data=fb) +
  scale_x_continuous(breaks=1:31) + 
  facet_wrap(~dob_month, ncol=3)
# or, omit only gender NAs: data = subset(fb, !is.na(gender))
qplot(x=friend_count, data=na.omit(fb), binwidth = 10) + 
  scale_x_continuous(limits=c(0,1000), breaks = seq(0,1000,50)) +
  facet_wrap(~gender) +
  theme_solarized()

by(fb$friend_count, fb$gender, summary)
summary(log10(fb$friend_count + 1))  

p1 <- ggplot(aes(x = friend_count), data = fb) + geom_histogram()
p2 <- p1 + scale_x_log10()
p3 <- p1 + scale_x_sqrt()

qplot(x=friend_count, y = ..count../sum(..count..),
      data=na.omit(fb), binwidth = 10, geom='freqpoly', color=gender) + 
      scale_x_continuous(limits=c(0,1000), breaks = seq(0,1000,50)) +
      facet_wrap(~gender) +
      theme_solarized()

grid.arrange(p1,p2,p3,ncol=1)

# % checked in using mobile 
sum(fb$mobile_likes)

ggplot(aes(x=age, y=friend_count), data=fb) + 
  coord_cartesian(xlim = c(13, 90), ylim = c(0,1000)) +
  geom_point(alpha = .05,  
              position = position_jitter(h=0),
              color = 'orange') + 
  geom_line(stat = 'summary', fun.y = mean) + 
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1),
            linetype = 2, color = 'blue') + 
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .9),
            linetype = 2, color = 'blue') + 
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .5),
            linetype = 2, color = 'blue')
  
# pearson (default) correlation
with(subset(pf, age <= 70), cor.test(age, friend_count, method))

cor.test(fb$www_likes_received, fb$mobile_likes_received)

pf.fc_by_age_months <- pf %>%
  group_by(age_with_months) %>%
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count),
            n = n()) %>%
  arrange(age_with_months)

p1 <- ggplot(aes(x = age, y = friend_count_mean),
             data = subset(fb.fc_by_age, age < 72)) +
  geom_line()

p2 <- ggplot(aes(x = age_with_months, y = friend_count_mean),
                 data = subset(fb.fc_by_age_months, age_with_months < 72)) +
  geom_line()

p4 <- ggplot(aes(x = age_with_months, y = friend_count_mean),
             data = subset(fb.fc_by_age_months, age_with_months < 72)) +
  geom_line() + 
  geom_smooth()

p3 <- ggplot(aes(x = round(age/5) *5, y = friend_count),
                 data = subset(fb, age < 72)) +
  geom_line(stat='summary', fun.y = mean)

grid.arrange(p2,p1,p4,p3,ncol=2)

# install.packages("tidyr") # only need to run this once 
library(tidyr) 
p <- spread(subset(pf.fc_by_age_gender, select = c('gender', 'age', 'median_friend_count')), gender, median_friend_count)

head(p)

# ratio plot
ggplot(aes(x = age, y = female / male),
       data = p) + 
  geom_line() + 
  geom_hline(yintercept = 1, alpha = 0.3, linetype = 2)

fb$year_joined <- floor(2014 - fb$tenure/165)
# split your data into buckets based on cut year
fb.year_joined.bucket <- cut(fb$year_joined,
                             c(2004, 2009, 2011, 2012, 2014))

# plot the grand mean for each of these cohorts
ggplot(aes(x=age, y=friend_count),
       data = subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket), 
            stat = 'summmary',
            fun.y = mean) + 
  geom_line(stat = 'summary', fun.y = mean, linetype = 2)

# daily friending rate
with(subset(fb, tenure >= 1), summary(friend_count / tenure))

#ibc
ibc <- read.csv('ibc.csv')
length(unique(ibc$Location))
summary(ibc$Location)
table(ibc$Location)


# yo dtaset
yo <- read.csv('yogurt.csv')
# change id from an int to a factor
yo$id <- factor(yo$id)
str(yo)
# # unique prices in dataset
length(unique(yo$price))
table(yo$price)
yo <- transform(yo, all.purchases = strawberry + blueberry + pina.colada + plain + mixed.berry)
summary(yo$all.purchases)

# sampling obervations
set.seed(4230)
# sample from the levels since that's all the different households u have
sample.ids <- sample(levels(yo$id), 16)

ggplot(aes(time,price),
       data=subset(yo, id %in% sample.ids)) + 
  facet_wrap(~id) +
  geom_line() +
  geom_point(aes(size = all.purchases), pch=1)

# GGally scatterplot matrices
library(GGally)
theme_set(theme_minimal(20))

# set seed for reproducible results
set.seed(1836)
fb <- read.csv('pseudo_facebook.tsv', sep = '\t')
pf_subset <- fb[, c(2:15)]
names(pf_subset)
ggpairs(pf_subset[sample.int(nrow(pf_subset), 1000),])
