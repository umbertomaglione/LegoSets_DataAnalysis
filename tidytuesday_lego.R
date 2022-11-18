library(tidyverse)
library(tidytuesdayR)
library(scales)

#doesn't work
tt <- tt_load("2022-09-06")
#so I will read it manually
inventories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
inventory_sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_sets.csv.gz', show_col_types = FALSE)
sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz', show_col_types = FALSE)


inventories
#counting the sets
inventories %>%
  count(set_num, sort=TRUE)

#this links sets to ids
inventory_sets

#downloading more data from https://rebrickable.com/downloads/
#reading these datasets...
setwd("C:/Users/umagl/Downloads/legos")
getwd()


lego_datasets <- tibble(file = dir("C:/Users/umagl/Downloads/legos", full.names= TRUE)) %>% #just "legos" as dir doesn't work!
  mutate(data = map(file, read_csv)) %>% #reading them
  extract(file,"name", "legos/(.*).csv.gz") %>% #pulling out the relevant part of the name
  deframe() #turns it into a list

#less sophisticated idea:
#lego_files <- dir("C:/Users/umagl/Downloads/legos", full.names= TRUE)
#lego_files %>%
#   map(read_csv) %>%
#   setNames(lego_files)

lego_datasets

lego_datasets$sets

#the lego sets start around the 40s
theme_set(theme_light())
lego_datasets$sets %>%
  count(year) %>%
  ggplot(aes(year,n)) + #n is the frequency from count(year)
  geom_col() + 
  labs(y = "# of LEGO sets")

#most produced sets, over time
lego_datasets$sets %>%
  group_by(name) %>%
  summarize(n_sets = n(),
            year_first = min(year),
            year_last = last(year)) %>%
  arrange(desc(n_sets)) %>%
  View()

lego_datasets$themes

#joining themes and sets
sets_themes <- lego_datasets$sets %>%
  left_join(lego_datasets$themes %>%
              select(id, theme_name = name),
            by = c(theme_id = "id"))

#plotting the themes by number of sets
sets_themes %>%
  count(theme_name, sort = TRUE) %>%
  head(20) %>%
  mutate(theme_name = fct_reorder(theme_name, n)) %>% #theme_name as factor
  ggplot(aes(n, theme_name)) +
  geom_col() +
  labs(x = '# of sets',
       y = 'Theme',
       title = "Most popular LEGO themes")
#to my surprise batman is not in the top 20


#something is wrong, the median n. of parts is 0 for some themes
  group_by(sets_themes, theme_name) %>%
  summarize(median(num_parts))

#there are more than 3000 sets with 0 parts
#probably the NAs were inserted as 0s
sets_themes %>%
  filter(num_parts == 0)

#0s to NAs
sets_themes <- sets_themes %>%
  mutate(num_parts = na_if(num_parts, 0))
 
#star wars sets are quite complex
by_theme <- sets_themes %>%
  group_by(theme_name) %>%
  summarize(n_sets = n(),
            median_parts = median(num_parts, na.rm = TRUE)) %>% #instead of mean, to avoid skewed results
  arrange(desc(n_sets)) 
by_theme

#visualized...
na.omit(by_theme) %>%
  head(25) %>%
  mutate(theme_name = fct_reorder(theme_name, median_parts)) %>%
  ggplot(aes(median_parts,theme_name)) +
  geom_col() +
  labs(title = "Most complex LEGO themes", 
       y= "",
       x= "# of parts")

#boxplot distributions
na.omit(sets_themes) %>% 
  filter(fct_lump(theme_name, 25) != "Other" ) %>%
  mutate(theme_name = fct_reorder(theme_name, num_parts))%>%
  ggplot(aes(num_parts, theme_name)) +
  geom_boxplot() +
  scale_x_log10() +
  labs(title = "Most complex LEGO themes", 
       y= "",
       x= "# of parts")
#I need to use sets_themes to catch the variability
#fct_lump cathegorizes the less common theme_name as Other -> 26 values


#are the most produced themes also the ones with more parts?
by_theme %>%
  filter(n_sets >= 90) %>%
  ggplot(aes(n_sets, median_parts)) +
  geom_point() + 
  geom_text(aes(label = theme_name), hjust = 1, vjust = 1,
            check_overlap = TRUE) + 
  scale_y_log10() + #rescaling y-axis to see more clearly the median parts
  scale_x_log10() +
  expand_limits(x = 50) #to fully see the labels
#there isn't a clear relation
#keychains are a 1-part product


#are the sets becoming more complicated over-time?
sets_themes %>%
  group_by(decade = 10 * floor(year/10)) %>%
  summarize(n_sets= n(),
            median_num_parts = median(num_parts, na.rm = TRUE))
#decade example: floor(1945/10)=194  *10= 1940  
#no univoque relation, hard to tell since the number of sets skyrocketed

  
lego_datasets$inventories
lego_datasets$inventory_parts #contains colors!

inventories_sub <- lego_datasets$inventories %>%
  distinct(set_num, .keep_all = TRUE) %>%
  select(inventory_id = id, set_num) #to match it with inventory_parts

sets_parts <- sets_themes %>%
  inner_join(inventories_sub, by = "set_num") %>%
  inner_join(lego_datasets$inventory_parts, by = "inventory_id",
             suffix = c("", "_inventory"))
  
