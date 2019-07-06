library(tidyverse)
library(ctv)

pkg_trees <- readRDS("analysis/data/raw_data/pkg_trees.rds") # only 1k
cran_pkg_names <- readRDS("analysis/data/raw_data/cran_pkg_names.rds")

# drop the errors
good_ones <- transpose(pkg_trees)$result
# how many?
length(good_ones)

# get list of dataframes of files for each pkg
x <- map(good_ones, 3, 'tree') %>%
  map(~map_df(.x, ~.x)) %>%
  set_names(cran_pkg_names)

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


# subset pkgs in task views
taskview_pkg_names <-  cran_pkg_names[(cran_pkg_names) %in% taskview_pkg_id$name]


# y/n tests
taskview_pkg_names_test_yn <-
  map_int(taskview_pkg_names,
          ~list.files(.x) %>%
            str_detect("tests") %>%
            sum(na.rm = TRUE)) %>%
  enframe() %>%
  mutate(name = basename(taskview_pkg_names))

taskview_pkg_names_test_yn_details <-
  taskview_pkg_id %>%
  left_join(taskview_pkg_names_test_yn)

# prop with tests
taskview_pkg_names_test_yn_details_props <-
  taskview_pkg_names_test_yn_details %>%
  group_by(taskview ) %>%
  summarise(count_with_tests = sum(value),
            prop_with_tests = count_with_tests / n(),
            n_pkgs_in_ctv = n()) %>%
  filter(!is.na(count_with_tests))

# plot it
density_plot <-
  ggplot(taskview_pkg_names_test_yn_details_props,
         aes(prop_with_tests)) +
  geom_density(fill = "grey80")  +
  labs(y = "",
       x = "Distribution of proportions") +
  theme_bw(base_size = 10)


ggplot(taskview_pkg_names_test_yn_details_props,
       aes( reorder(taskview  , prop_with_tests),
            prop_with_tests,
            size = count_with_tests )) +
  geom_point() +
  labs(x = "",
       y = "Proportion of packages in the Task View with tests",
       size = "Number of packages\nin the Task View") +
  coord_flip() +
  theme_minimal(base_size = 12) +
  annotation_custom(ggplotGrob(density_plot),
                    xmin = 2,
                    xmax = 8,
                    ymin = 0.5,
                    ymax = 0.8)



