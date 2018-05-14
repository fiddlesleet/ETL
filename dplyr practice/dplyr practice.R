library(gapminder) 
library(dplyr)
gapminder
class(gapminder)

str(gapminder)
glimpse(gapminder) # compare with str()

# sample_frac(): sample a given % of rows
sample_frac(gapminder, 0.5)
## sample_n(): sample n rows
# useful instead of head since get a random selection
set.seed(2016)
tiny <- sample_n(gapminder, 3)
tiny
# rename & select cols
select(tiny, 
       starts_with("y"), #get year
       population = pop,
       matches("^co.*")
       )
# filter rows from df
filter(tiny, lifeExp > 60, year < 1980)
filter(tiny, lifeExp > 60 & year < 1980) # equivalent to above with ,

# slice(): select rows from df by index, making a subset
slice(gapminder, 300:303)
mutate(tiny, newVar = (lifeExp / gdpPercap), newcol=3:1)

# transmute(): add new col that is function of existing col, and drop existing col
tiny <- transmute(tiny, id = 1:3, country, continent, newVarSqrt = sqrt(newVar), pop)
tiny

# arrange(): reorder (sort) rows
arrange(tiny, pop)

gapminder %>%
    arrange(desc(year), lifeExp)

summarize(gapminder, avgLife = mean(lifeExp))

str(gapminder)

# filter for unique rows 
distinct(gapminder) # filter for duplicate rows 
tiny2 <- tiny[c(1,2,1,2),]
tiny2
distinct(tiny2)

# chaining
set.seed(2016)
gapminder %>%
    filter(continent == "Asia") %>%
    filter(lifeExp < 65) %>%
    sample_n(10)

# group_by(): convert df into grouped data, where ops performed by gorup
# count up number of entries for each continent 
gapminder %>% 
    group_by(continent) %>%
    tally


# join multiple dfs

superheroes <- c("name, alignment, gender, publisher",
                 "Magneto, bad, male, Marvel",
                 "Storm, good, female, Marvel",
                 "Mystique, bad, female, Marvel",
                 "Batman, good, male, DC",
                 "Joker, bad, male, DC",
                 "Catwoman, bad, female, DC",
                 "Hellboy, good, male, Dark Horse Comics")
# convert comma-sep'd data into df
superheroes <- read.csv(text = superheroes, strip.white = TRUE, as.is=TRUE)
publishers <- c("publisher, year_founded",
                "DC, 1934",
                "Marvel, 1939",
                "Image, 1992")
publishers <- read.csv(text=publishers, strip.white = TRUE, as.is = TRUE)

# inner join (right col of 1st df matches left col of 2nd)
inner_join(superheroes, publishers) # only joins rows with no missing data
left_join(superheroes, publishers) # right alignment of col text 
full_join(superheroes, publishers)

##### And again

filter(gapminder, lifeExp<30)
# filter down cols
select(gapminder, year, lifeExp)
gapminder %>%
    select(year, lifeExp) %>%
    head(4)

gap <- gapminder
gap %>%
    mutate(gdp = pop * gdpPercap)

# sort by year, and then by country
gap %>% 
    arrange(year, country)

gap %>% 
    group_by(continent) %>%
    dplyr::summarize(avg_life_expectency = mean(lifeExp))

gap %>%
    select(country, year, continent, lifeExp) %>%
    group_by(continent, country) %>%
    ## within country, take (lifeExp in year i) - (lifeExp in year i-1)
    ## positive means LifeExp inc'd, - means lifeExp dec'd
    mutate(d = lifeExp - lag(lifeExp)) %>% 
    ## within country, retain the worst lifeExp change (smallest or most -)
    dplyr::summarize(worst_d = min(d, na.rm=TRUE)) %>%
    top_n(-1, wt = worst_d) %>%
    arrange(worst_d)
    


