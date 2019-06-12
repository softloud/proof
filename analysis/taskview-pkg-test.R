library(ctv)
library(tidyverse)
library(lubridate)
conflicted::conflict_prefer("filter", "dplyr")

# get view metadata
# tv <- available.views()

# package list
dat <- tv %>% 
  transpose()  

# tag with taskview identifier and bind in one df
taskview_pkg_id <- dat %>% 
  as_tibble() %>% 
  mutate(pkg = map2(name, packagelist, .f = function(x, y){
    y %>% 
      mutate(taskview = x,
             no_pkgs_in_tv = nrow(y))
  })) %>% 
  pluck("pkg") %>% 
  bind_rows() %>% 
  rename(taskview_core = core)

# get pkg
pkg_test_size_desc <- read_rds("analysis/pkg_test_size_desc.rds")

# bind it to testing data
pkg_test_taskview <- pkg_test_size_desc %>% 
  rename(name = cran_pkg_names) %>% 
  full_join(taskview_pkg_id, by = "name") %>% 
  mutate(no_tests = test_size_ratio == 0)

# get dates
dates <- pkg_test_taskview %>% 
  filter(!is.na(date)) %>% # not sure if date is the correct field
  pluck("date") %>% 
  as_date() %>% 
  discard(is.na) # why are there nas?

# create plot data
plot_data <- seq.Date(min(dates), max(dates), by = "month") %>% 
  map_df(.f = function(x){
    pkg_test_taskview %>% 
      filter(date >= x) %>% 
      group_by(taskview) %>% 
      summarise(no_tests_prop = sum(no_tests)/length(no_tests)) %>% 
      mutate(date = as_date(x)) 
  }) 
  
# plot
plot_data %>%
  filter(!is.na(taskview )) %>% 
  ggplot(aes(x = date, y = no_tests_prop)) +
  geom_line(aes(group = taskview, colour = taskview)) +
  labs(x = "Month",
       y = str_wrap("Proportion of packages in taskview with no tests", width = 30))
ggsave("taskviews.png")


# zoom plot
plot_data %>%
  filter(!is.na(taskview),
         no_tests_prop > 0.2) %>% 
  ggplot(aes(x = date, y = no_tests_prop)) +
  geom_line(
    alpha = 0.8,
    aes(group = taskview, colour = taskview)) +
  labs(x = "Month",
       title = str_wrap("More than one fifth of the packages in these taskviews have no tests", width = 60),
       y = str_wrap("Proportion > 0.2 of packages with no tests, by taskview", width = 30)) +
  hrbrthemes::scale_colour_ipsum("Taskview")

ggsave("taskviews-zoom.png")


# 
pkg_test_taskview %>% 
  filter(!is.na(taskview), !is.na(no_tests)) %>% 
  group_by(taskview) %>% 
  ggplot(aes(x = taskview, fill = no_tests)) +
  geom_bar() +
  coord_flip() +
  scale_fill_grey()


