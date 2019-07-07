library(tidyverse)
library(lubridate)

size_of_tests_per_pkg <-
  readRDS(here::here("analysis/data/raw-data/size_of_tests_per_pkg.rds"))
cran_pkg_metadata <-
  readRDS(here::here("analysis/data/raw-data/cran_pkg_metadata.rds"))

pkg_test_size_desc <-
  left_join(size_of_tests_per_pkg,
            cran_pkg_metadata, by = c("cran_pkg_names" = "Package")) %>%
  mutate(date_fmt = ymd(date)) %>%
  mutate(year = round_date(date_fmt, unit = "year")) %>%
  rename("Authors (n)" = "Aut",
         "Pkg size (bytes)" = "pkg_size")

saveRDS(pkg_test_size_desc,
        here::here("analysis/data/raw-data/pkg_test_size_desc.rds"))

# test ratio and Aut/RD/Date/pkg_size
test_ratio_vs_various_facet_plot1 <-
  pkg_test_size_desc %>%
  select(test_size_ratio,
         RDRI,
         `Authors (n)`,
         `Pkg size (bytes)`) %>%
  gather(variable,
         value,
         -test_size_ratio) %>%
  mutate(variable = ifelse(variable == "RDRI",
                           "Revdeps ∪ Revimps (n)",
                           variable)) %>%
  ggplot(aes(test_size_ratio,
             value)) +
  geom_point() +
  labs(x = "Test size ratio",
       y = "") +
  scale_y_log10(labels = scales::comma_format(accuracy = 1)) +
  facet_wrap( ~ variable,
              scales = "free") +
  theme_minimal(base_size = 14)

library(cowplot)
plot_grid(size_of_tests_per_pkg_hist_plot,
          test_ratio_vs_various_facet_plot1,
          labels="auto",
          ncol = 1)

ggsave(here::here('analysis/figures/000-how-much-of-th-pkg-is-tests.png'), h = 10, w = 10)

