# get pkg dates from here
library(rvest)
library(tidyverse)
library(lubridate)
library(ggbeeswarm)

archive_tbls_lst_result <-
  readRDS(here::here("analysis/data/raw-data/archive_tbls_lst_result.rds"))

# work on it
pkg_birth_dates <-
  map(archive_tbls_lst_result,
      ~.x %>%
        as_tibble(.name_repair = "universal") %>%
        mutate(Last.modified = ymd_hm(Last.modified)) %>%
        filter(Last.modified == min(Last.modified, na.rm = TRUE)))

saveRDS(pkg_birth_dates,
        here::here("analysis/data/raw-data/pkg_birth_dates.rds"))

pkg_birth_dates_df <-
  bind_rows(pkg_birth_dates, .id = "cran_pkg_names")


pkg_birth_dates_df_with_tests <-
  left_join(pkg_birth_dates_df,
            size_of_tests_per_pkg,
  )

pkg_test_size_desc_birth_dates <-
  pkg_birth_dates_df_with_tests %>%
  mutate(birth_year = round_date(Last.modified , unit = "year"))  %>%
  mutate(birth_year = str_trunc(as.character(birth_year), 4,
                                side = "right",
                                ellipsis = "")) %>%
  mutate(test_yn = if_else(is.na(test_size), 'n', 'y'))

saveRDS(pkg_test_size_desc_birth_dates,
        here::here("analysis/data/raw-data/pkg_test_size_desc_birth_dates.rds" ))

pkg_test_size_desc_birth_dates_by_year <-
pkg_test_size_desc_birth_dates %>%
  group_by(birth_year, test_yn)  %>%
  tally() %>%
  spread(test_yn, n) %>%
  mutate(prop = y / (sum(n, y, na.rm = TRUE)))

pkg_test_size_desc_birth_dates_by_year_plot <-
ggplot(pkg_test_size_desc_birth_dates_by_year,
       aes(birth_year,
           prop )) +
  geom_col() +
  labs(y = "Proportion of CTV packages\nper year with tests",
       x = "") +
  theme_minimal(base_size = 8) +
  scale_fill_viridis_d()

pkg_test_size_desc_birth_dates_boxplot <-
ggplot(pkg_test_size_desc_birth_dates,
       aes(birth_year,
           test_size_ratio)) +
  geom_boxplot() +
  geom_quasirandom(alpha = 0.2) +
  labs(y = "Ratio of test file\nsize to package size",
       x = "Year of package origin") +
  scale_y_log10() +
  theme_minimal(base_size = 8)


# test ratio and Aut/RD/Date/pkg_size

c_dat <- readRDS(here::here("analysis/data/raw-data/cran_pkg_metadata.rds"))

test_ratio_vs_various_facet_plot <-
  pkg_birth_dates_df_with_tests %>%
  left_join(c_dat, by = c('cran_pkg_names' = 'Package')) %>%
  mutate(birth_year = round_date(Last.modified , unit = "year"))  %>%
  mutate(birth_year = str_trunc(as.character(birth_year), 4,
                                side = "right",
                                ellipsis = "")) %>%
  mutate(birth_year = parse_number(birth_year)) %>%
  mutate(test_yn = if_else(is.na(test_size), 0, 1)) %>%
  group_by(birth_year, test_yn)  %>%
  select(
    Aut,
    RDRI,
    test_yn,
    birth_year) %>%
  rename("Authors (n)" = "Aut",
         "Revdeps ∪ Revimps (n)" = "RDRI",
         "Year of package origin" = "birth_year") %>%
  gather(variable,
         value,
         -test_yn) %>%
  ggplot(aes(
    value,
    test_yn)) +
  geom_point() +
  labs(y = "Presence of test",
       x = "") +
  stat_smooth(method="glm",
              method.args=list(family="binomial"),
              se=TRUE) +
  facet_wrap( ~ variable,
              scales = "free") +
  theme_minimal(base_size = 14) +
  scale_y_continuous(breaks = c(0,1),
                     labels = c(0,1))

library(cowplot)
plot_grid(test_ratio_vs_various_facet_plot,
          pkg_test_size_desc_birth_dates_by_year_plot,
          pkg_test_size_desc_birth_dates_boxplot,
          ncol = 1)


