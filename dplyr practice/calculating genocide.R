library(gapminder)
library(tidyverse)

(my_gap <- gapminder)

# add GDP col
my_gap <- my_gap %>%
  mutate(pop * gdpPercap)

# select() can rename and reposition variables
# You’ve seen simple use of select(). There are two tricks you might enjoy:
?everything
my_gap %>%
  filter(country == "Afghanistan", year > 1995) %>%
  #   select() can rename the variables you request to keep.
  select(yr = year, lifeExp, gdpPercap) %>%
  # select() can be used with everything() to hoist a variable up to the front of the tibble.
  select(gdpPercap, everything())

# simple counting
my_gap %>%
  group_by(continent) %>%
  summarize(n = n())

# or
my_gap %>%
  group_by(continent) %>%
  tally()

# or
my_gap %>%
  count(continent)

my_gap %>%
  group_by(continent) %>%
  summarize(n = n(), n_countries = n_distinct(country))

# avg life expectency by continent
my_gap %>%
  group_by(continent) %>%
  summarize(avg_lifeExp = mean(lifeExp))

# mean PPP in 1952 and 2007
my_gap %>%
  filter(year %in% c(1952, 2007)) %>%
  group_by(continent, year) %>%
  summarise_each(funs(mean, median), lifeExp, gdpPercap)

# min and max life expectancy per yr Asia
my_gap %>%
  filter(continent == "Asia") %>%
  group_by(year) %>%
  summarize(min_lifeExp = min(lifeExp), max_lifeExp = max(lifeExp))

# min and max life expectancy per yr Asia, including which country
my_gap %>%
  filter(continent == "Asia") %>%
  select(year, country, lifeExp) %>%
  group_by(year) %>%
  filter(min_rank(desc(lifeExp)) < 2 | min_rank(lifeExp) < 2) %>% 
  arrange(year) %>%
  gather(year)

asia <- my_gap %>%
  filter(continent == "Asia") %>%
  select(year, country, lifeExp) %>%
  group_by(year)
asia

asia %>%
  mutate(le_rank = min_rank(lifeExp),
         le_desc_rank = min_rank(desc(lifeExp))) %>% 
  filter(country %in% c("Afghanistan", "Japan", "Thailand"), year > 1995)

# which country experienced the sharpest 5-year drop in life expectancy?
# Ponder that for a while. The subject matter and the code.
# Mostly you’re seeing what genocide looks like in 
# dry statistics on average life expectancy.
my_gap %>%
  select(country, year, continent, lifeExp) %>%
  group_by(continent, country) %>%
  ## within country, take (lifeExp in year i) - (lifeExp in year i - 1)
  ## positive means lifeExp went up, negative means it went down
  mutate(le_delta = lifeExp - lag(lifeExp)) %>% 
  ## within country, retain the worst lifeExp change = smallest or most negative
  summarize(worst_le_delta = min(le_delta, na.rm = TRUE)) %>% 
  ## within continent, retain the row with the lowest worst_le_delta
  top_n(-1, wt = worst_le_delta) %>% 
  arrange(worst_le_delta)



my_gap %>% 
  group_by(country) %>% 
  select(country, year, lifeExp) %>% 
  mutate(lifeExp_gain = lifeExp - first(lifeExp)) %>% 
  filter(year < 1963)
