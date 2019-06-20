library(tidyverse)
library(lubridate)

size_of_tests_per_pkg <- readRDS("analysis/data/raw_data/size_of_tests_per_pkg.rds")
c_dat <- readRDS("analysis/data/raw_data/c_dat.rds")

pkg_test_size_desc <-
  left_join(size_of_tests_per_pkg,
            c_dat, by = c("cran_pkg_names" = "Package")) %>%
  mutate(date_fmt = ymd(date)) %>%
  mutate(year = round_date(date_fmt, unit = "year"))

saveRDS(pkg_test_size_desc, "analysis/data/raw_data/pkg_test_size_desc.rds")

# test ratio and Aut/RD/Date/pkg_size
test_ratio_vs_various_facet_plot <-
  pkg_test_size_desc %>%
  select(test_size_ratio,
         Aut,
         RDRI,
         pkg_size) %>%
  gather(variable,
         value,
         -test_size_ratio) %>%
  ggplot(aes(test_size_ratio,
             value)) +
  geom_point() +
  scale_y_log10() +
  facet_wrap( ~ variable,
              scales = "free") +
  theme_minimal(base_size = 14)

library(cowplot)
plot_grid(size_of_tests_per_pkg_hist_plot,
          test_ratio_vs_various_facet_plot,
          ncol = 1)
