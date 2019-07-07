library(tidyverse)

cran_pkg_names <-
  readRDS(here::here("analysis/data/raw-data/cran_pkg_names.rds"))
taskview_pkg_id <-
  readRDS(here::here("analysis/data/raw-data/taskview_pkg_id.rds"))
pkg_test_size_desc_birth_dates <-
  readRDS(here::here("analysis/data/raw-data/pkg_test_size_desc_birth_dates.rds"))


pkg_test_size_desc_birth_dates_ctv <-
  pkg_test_size_desc_birth_dates %>%
  distinct(cran_pkg_names, .keep_all = TRUE) %>%
  left_join(taskview_pkg_id,
            by = c('cran_pkg_names' = 'name'))


pkg_test_size_desc_birth_dates_ctv_cumsum <-
pkg_test_size_desc_birth_dates_ctv %>%
  group_by(birth_year, taskview) %>%
  tally() %>%
  ungroup() %>%
  group_by(taskview) %>%
  arrange(birth_year) %>%
  mutate(cumsum = cumsum(n)) %>%
  mutate(birth_year = parse_number(birth_year))

end_labels <-
  pkg_test_size_desc_birth_dates_ctv_cumsum %>%
  ungroup() %>%
  filter(birth_year == max(birth_year)) %>%
  top_n(10, cumsum) %>%
  bind_rows(  pkg_test_size_desc_birth_dates_ctv_cumsum %>%
                filter(birth_year == max(birth_year)) %>%
                 ungroup() %>%
                top_n(-10, cumsum))


library(ggplot2)
library(grid)
library(directlabels)

p0 <-  ggplot(pkg_test_size_desc_birth_dates_ctv_cumsum,
              aes(birth_year,
                  cumsum,
                  group = taskview,
                  colour = taskview)) +
  geom_line() +
  labs(y = "Number of pkgs in Task View",
       x = "Year") +
  xlim(1998,2025) +
  theme_minimal(base_size = 8)

p <-
 direct.label(p0, "last.bumpup") +
  theme(plot.margin = unit(c(1,3,1,1), "lines"))


pkg_test_size_desc_birth_dates_ctv_prop_plot <-
pkg_test_size_desc_birth_dates_ctv %>%
  group_by(taskview, test_yn) %>%
  tally() %>%
  spread(test_yn, n) %>%
  mutate(prop_y = y / sum(n, y, na.rm = TRUE)) %>%
  ggplot(aes(reorder(taskview, prop_y),
             prop_y)) +
  geom_col() +
  coord_flip() +
  labs(y = "Proportion of pkgs containing tests",
       x = "Task View") +
  theme_minimal(base_size = 8)

cowplot::plot_grid(p,
         pkg_test_size_desc_birth_dates_ctv_prop_plot,
         ncol = 1,
         labels="auto"
         )

ggsave(here::here('analysis/figures/000-ctv-prop-pkgs-with-tests.png'),
       h = 10, w = 10)







