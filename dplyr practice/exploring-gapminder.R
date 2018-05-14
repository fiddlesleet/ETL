library(gapminder)
library(tidyverse)

# inspect
(gap <- gapminder)
# count entries per continent
gap %>%
  count(continent)
# alternative to 
#gap %>%
#  group_by(continent) %>%
#  tally()
# alternative to
#gap %>%
#  group_by(continent) %>%
#  summarize(n = n())
# alternative to 
# table(gapminder$continent)
# get n countries per year
gap %>%
  group_by(year) %>%
  summarize(n = n(),
            n_countries = n_distinct(country))

# add gdp varible 
gap <- gap %>%
  mutate(gdp = pop * gdpPercap)

# Explore life expectancies
gap %>%
  group_by(continent) %>%
  summarize(avg_lifeExp = mean(lifeExp))

gap %>%
  filter(year %in% c(1952, 2007)) %>%
  group_by(continent, year) %>%
  summarize_at(vars(lifeExp, gdpPercap), funs(mean, median))
# inspect iraq data
gap %>%
  filter(country == "Iraq")

# arrange by life expectancy in 1952
gap %>%
  filter(year == 1952) %>%
  arrange(lifeExp)
# show highest life exp. first
gap %>%
  filter(year == 1952) %>%
  arrange(desc(lifeExp))

# explore changes in life expectancy
gap %>% 
  group_by(country) %>% 
  select(country, year, lifeExp) %>% 
  mutate(lifeExp_gain = lifeExp - first(lifeExp)) %>% 
  filter(year < 1963)

# get life expectancy ranks each year
asia <- gap %>%
  filter(continent == "Asia") %>%
  select(year, country, lifeExp) %>%
  group_by(year)
asia
asia %>%
  mutate(le_rank = min_rank(lifeExp),
         le_desc_rank = min_rank(desc(lifeExp))) %>% 
  filter(country %in% c("Afghanistan", "Japan", "Thailand"), year > 1995)

# which country had sharpest decrease in life expectancy?
gap %>%
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

gap %>%
  group_by(country) %>%
  do(tidy(lm(lifeExp ~ year, data = .))) # using broom



gap_asia_2007 <- gapminder %>% filter(year == 2007, continent == "Asia")
ggplot(gap_asia_2007, aes(x = lifeExp, y = country)) + geom_point()
ggplot(gap_asia_2007, aes(x = lifeExp, y = fct_reorder(country, lifeExp))) +
  geom_point()


# Use fct_reorder2() when you have a line chart of a quantitative x
# against another quantitative y and your factor provides the color.
# This way the legend appears in some order as the data! 

h_countries <- c("Egypt", "Haiti", "Romania", "Thailand", "Venezuela")
h_gap <- gapminder %>%
  filter(country %in% h_countries) %>% 
  droplevels()
ggplot(h_gap, aes(x = year, y = lifeExp, color = country)) +
  geom_line()
ggplot(h_gap, aes(x = year, y = lifeExp,
                  color = fct_reorder2(country, year, lifeExp))) +
  geom_line() +
  labs(color = "country")



japan_dat <- gapminder %>%
  filter(country == "Japan")
japan_tidy <- japan_dat %>%
  gather(key = var, value = value, pop, lifeExp, gdpPercap)
dim(japan_dat)
#> [1] 12  6
dim(japan_tidy)


p <- ggplot(japan_tidy, aes(x = year, y = value)) +
  facet_wrap(~ var, scales="free_y")
p + geom_point() + geom_line() +
  scale_x_continuous(breaks = seq(1950, 2011, 15))

japan_tidy <- gapminder %>%
  filter(country == "Japan") %>%
  gather(key = var, value = value, pop, lifeExp, gdpPercap)
ggplot(japan_tidy, aes(x = year, y = value)) +
  facet_wrap(~ var, scales="free_y") +
  geom_point() + geom_line() +
  scale_x_continuous(breaks = seq(1950, 2011, 15))

# get counts: this works because default stat for geom_bar() is "bin"
ggplot(gapminder, aes(x = continent)) + geom_bar()
#reorder the continents based on frequency
p <- ggplot(gapminder, aes(x = reorder(continent, continent, length)))
p + geom_bar()
# flip
p + geom_bar() + coord_flip()
# skinny
p + geom_bar(width = 0.05) + coord_flip()


# density hist: experiment with bin width; think in terms of the units of the x variable
ggplot(gapminder, aes(x = lifeExp)) +
  geom_histogram(binwidth = 1)

# compare frequency densities: don't stack, geom_freqpoly() is better in this case
ggplot(gapminder, aes(x = lifeExp, color = continent)) +
  geom_freqpoly()
# smoothed
ggplot(gapminder, aes(x = lifeExp, color = continent)) + geom_density()
#alpha transparency works here too
ggplot(gapminder, aes(x = lifeExp, fill = continent)) +
  geom_density(alpha = 0.2)
#with only two countries, maybe we should ignore Oceania?
ggplot(subset(gapminder, continent != "Oceania"),
       aes(x = lifeExp, fill = continent)) + geom_density(alpha = 0.2)
#facets work here too
ggplot(gapminder, aes(x = lifeExp)) + geom_density() + facet_wrap(~ continent)
# density
ggplot(subset(gapminder, continent != "Oceania"),
       aes(x = lifeExp, fill = continent)) + geom_histogram() +
  facet_grid(continent ~ .)

# since year is not of factor type, must explicitly specify as the grouping variable
ggplot(gapminder, aes(x = year, y = lifeExp)) + geom_boxplot(aes(group = year))
#violin
ggplot(gapminder, aes(x = year, y = lifeExp)) +
  geom_violin(aes(group = year)) +
  geom_jitter(alpha = 1/4) +
  geom_smooth(se = FALSE)


p <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) # just initializes
p + geom_point()
#log transformation ... quick and dirty
ggplot(gapminder, aes(x = log10(gdpPercap), y = lifeExp)) +
  geom_point()
#a better way to log transform
p + geom_point() + scale_x_log10()
p + geom_point(aes(color = continent))


# plot lifeExp against year
(y <- ggplot(gapminder, aes(x = year, y = lifeExp)) + geom_point())

# "connect the dots" for one country?
y <- ggplot(gapminder, aes(x = year, y = lifeExp)) + geom_point()
y + facet_wrap(~ continent) + geom_line(aes(group = country)) +
  geom_smooth(se = FALSE, lwd = 2) 
ggplot(gapminder %>% filter(country == "Zimbabwe"),
       aes(x = year, y = lifeExp)) + geom_line() + geom_point()

# lets just look at four countries
jCountries <- c("Canada", "Rwanda", "Cambodia", "Mexico")
ggplot(subset(gapminder, country %in% jCountries),
       aes(x = year, y = lifeExp, color = country)) + geom_line() + geom_point()

# when you really care, make your legend easy to navigate this means visual order = data order = factor level order
ggplot(subset(gapminder, country %in% jCountries),
       aes(x = year, y = lifeExp, color = reorder(country, -1 * lifeExp, max))) +
  geom_line() + geom_point()
