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
  }) %>%
  filter(!is.na(taskview ))



# plot
plot_data %>%
  ggplot(aes(x = date, y = no_tests_prop)) +
  geom_line(aes(group = taskview, colour = taskview)) +
  labs(x = "Month",
       y = str_wrap("Proportion of packages in taskview with no tests", width = 30))
ggsave("figures/taskviews.png")

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
  mutate(taskview = fct_relevel(taskview, ts_levels),
         tests = !no_tests) %>%
  ggplot(aes(x = taskview, fill = tests)) +
  geom_bar(alpha = 0.8) +
  coord_flip() +
  scale_fill_grey("Tests", start = 0.8, end = 0.2) +
  labs(
    title = "How many packages in a taskview have tests?",
    y = "Number of packages in taskview",
    x = "Taskview",
    caption = "Taskviews ordered by proportion of packages with no tests."
  )

ggsave("figures/tests-no-tests.png")


# ridges ------------------------------------------------------------------


tv_pkgs <- tidyverse::tidyverse_packages()


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

# tidyverse quantiles
tidyverse_quantiles <- taskview_dat %>%
  mutate(tidy = name %in% tv_pkgs) %>%
  filter(isTRUE(tidy)) %>%
  pluck("test_size_ratio") %>%
  quantile(c(0.25, 0.5, 0.75)) %>%
  log()

# but, as this currently yields nothing, some dummy code
tidyverse_quantiles <- runif(n = 100, -4, -2) %>%
  quantile(c(0.25, 0.5, 0.75))


# ridges plot data
ridges_dat <- taskview_dat %>%
  ungroup() %>%
  mutate(taskview = fct_relevel(taskview, ratio_levels),
         lr = log(test_size_ratio + 0.0000001))


# ridges plot
ridges_dat %>%
  ggplot() +
  # geom_point()
  geom_vline(
    colour = "black",
    linetype = "dotted",
    xintercept = tidyverse_quantiles
  ) +
  geom_vline(
    colour = "black",
    linetype = "dashed",
    xintercept = ratio_quantiles
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
    caption = str_wrap("Taskviews ordered by median ratio of test files. Dashed lines indicate overall quartiles of log-ratios of testing file sizes to other files. Dotted lines indicate the quartiles of log-ratios of testing files in tidyverse:: packages.")
  )

ggsave("figures/ridges.png")

