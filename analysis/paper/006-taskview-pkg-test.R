library(ctv)
library(tidyverse)
library(lubridate)
library(ggridges)
conflicted::conflict_prefer("filter", "dplyr")

# get view metadata
tv <- available.views() # this takes a while

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





# time, doesn't really work -----------------------------------------------


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
taskview_dat <- pkg_test_taskview %>%
  filter(!is.na(taskview), !is.na(no_tests)) %>%
  group_by(taskview)

# relevel
ts_prop <- taskview_dat %>%
  group_by(taskview) %>%
  select(name, no_tests) %>%
  summarise(prop = sum(no_tests)/length(no_tests)) %>%
  arrange(prop)

ts_levels <- ts_prop %>%
  pluck("taskview")

# barcharts
taskview_dat %>%
  ungroup() %>%
  mutate(taskview = fct_relevel(taskview, ts_levels)) %>%
  ggplot(aes(x = taskview, fill = no_tests)) +
  geom_bar() +
  coord_flip() +
  scale_fill_grey("No tests") +
  labs(
    title = "How many packages in a taskview have no tests?",
    x = "Number of packages in taskview",
    y = "Taskview",
    caption = "Taskviews ordered by proportion of packages with no tests."
  )




# ridges ------------------------------------------------------------------



ratio_levels <- taskview_dat %>%
  group_by(taskview) %>%
  summarise(median_ratio = median(test_size_ratio)) %>%
  arrange(median_ratio) %>% pluck("taskview")


props <- ts_prop %>%
  mutate(start = 0) %>%
  gather(key = terminal, value = ratio, start, prop)

# For the overall distribution
ratio_quantiles <- taskview_dat$test_size_ratio %>%
  quantile(c(0.25, 0.5, 0.75)) %>%
  log()

# ridges plot
ridges_dat <- taskview_dat %>%
  ungroup() %>%
  mutate(taskview = fct_relevel(taskview, ratio_levels),
         lr = log(test_size_ratio + 0.0000001))

ridges_dat %>%
  ggplot() +
  # geom_point()
  geom_vline(
    colour = "black",
    linetype = "dotted",
    xintercept = ratio_quantiles[[2]]
  ) +
  geom_vline(
    colour = "black",
    linetype = "dashed",
    xintercept = ratio_quantiles[c(1,3)]
  ) +
  # geom_rect(
  #   aes(xmin = ratio_quantiles[[1]],
  #           xmax = ratio_quantiles[[2]],
  #           ymin = -1000,
  #           ymax = 1000),
  #           colour = "lightgrey",
  #           alpha = 0.3) +
  geom_density_ridges(
  data = ridges_dat,
  aes(y = taskview, x = lr),
    alpha = 0.8,
    colour = "darkgrey"
    ) +
  labs(
    title = "Proportion of files associated with testing in taskview packages",
    x = "Log-ratio of size of files associated with tests with other files",
    y = "Taskview",
    caption = str_wrap("Taskviews ordered by median ratio of test files. Dashed lines indicate overall interquartile range, and dotted line, the overall median, of log-ratios of testing file sizes to other files.")
  )

ggsave("ridges.png")

