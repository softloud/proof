library(tidyverse)
pkg_trees <- readRDS("analysis/data/raw_data/pkg_trees.rds")
cran_pkg_names <- readRDS("analysis/data/raw_data/cran_pkg_names.rds")

# drop the errors
good_ones <- transpose(pkg_trees)$result
# how many?
length(good_ones)

# get list of dataframes of files for each pkg
x <- map(good_ones, 3, 'tree') %>%
  map(~map_df(.x, ~.x)) %>%
  set_names(cran_pkg_names)

# covr uses tools::testInstalledPackage, which does three locations for tests
x_tests_dir <-
  map(x, ~.x %>% filter(str_detect(path, "tests|examples|vignettes"))) %>%
  discard( ~ nrow(.x) == 0)

# how many have tests?
n_pkgs_with_tests <- length(x_tests_dir)

# what proportion of our sample?
n <- length(good_ones)
prop_with_tests <- n_pkgs_with_tests / n
perc_with_tests <-  round(prop_with_tests * 100, 1)

# rough size of tests per pkg
test_file_sizes <-
x_tests_dir %>%
  map(~.x %>% filter(str_detect( path , "\\.R$|\\.r$")))

size_of_tests_per_pkg <-
  map_df(test_file_sizes, ~sum(.x$size, na.rm = TRUE)) %>%
  gather(cran_pkg_names, test_size) %>%
  left_join(
    tibble(cran_pkg_names = cran_pkg_names,
           pkg_size = map_int(x, ~sum(.x$size, na.rm = TRUE)))) %>%
  mutate(test_size_ratio =  test_size / pkg_size)

saveRDS(size_of_tests_per_pkg, "analysis/data/raw_data/size_of_tests_per_pkg.rds")

size_of_tests_per_pkg_hist_plot <-
  ggplot(size_of_tests_per_pkg,
         aes(test_size_ratio)) +
  geom_histogram() +
  labs(x = "Ratio of test files size to total pkg size",
       y = "Count") +
  ggtitle("Tests in R pkgs on CRAN: How much of the pkg are tests?",
          subtitle = str_glue('{n_pkgs_with_tests} pkgs that have tests, out of a sample of {n} pkg ({perc_with_tests}%)')) +
  theme_minimal(base_size = 14)


