library(jsonlite)
library(stringr)
library(tidyr)


##########
# GET DATA
##########
df.yelp <- stream_in(file("yelp_academic_dataset_business.json")) 
# inspect
str(df.yelp)

# variable "hours" is a df containing 7 dfs, each of which is for a day of the week
# - Each weekday is also a df containing 2 cols

# flatten the JSON nested heirarchical structure, by assigning each nested var its own col
df.yelp <- flatten(df.yelp) %>% 
  tbl_df()
str(df.yelp)
# inspect
df.yelp

# get categories
df.yelp %>%
  mutate(categories = as.character(categories)) %>% 
  select(categories)

# remove all cols starting with "hours" or "attributes"
# select rows with "Restaurant" among their categorizations
# unnest() categorise: sample category vector: c("Books, Mags, Music & Video", "Shopping", "Bookstores")
df.yelp %>%
  select(-starts_with("hours"),
         -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>% # split category vector down category column
  filter(categories != "Restaurants") %>% 
  count(state, categories) %>% # most common categories by state
  arrange(-(n)) # descending order

# top restaurant category by state
df.yelp %>%
  select(-starts_with("hours"), -
           starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>% # split category vector down category column
  filter(categories != "Restaurants") %>% 
  count(state, categories) %>% # most common categories by state
  filter(n > 10) %>% # count must be > 10
  group_by(state) %>%
  top_n(1, n) # descending order
