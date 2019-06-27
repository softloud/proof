# get pkg dates from here
library(rvest)
library(tidyverse)
library(lubridate)
library(ggbeeswarm)

# we need to look at the CRAN archive page for every pkg to get the first date

cran_pkg_names <- readRDS("analysis/data/raw_data/cran_pkg_names.rds")

archive_url <- "https://cran.r-project.org/src/contrib/Archive/"

archive_urls <- map_chr(cran_pkg_names, ~str_glue('{archive_url}{.x}'))

get_archive_tables_safely <-
  safely(function(x) read_html(x) %>%
            html_nodes("table") %>%
            html_table() %>%
           .[[1]])

# takes several hours
archive_tbls_lst <-
  map(archive_urls, ~get_archive_tables_safely(.x))

archive_tbls_lst_result <-
transpose(archive_tbls_lst)$result %>%
  set_names(cran_pkg_names) %>%
  compact()


saveRDS(archive_tbls_lst_result,
        "analysis/data/raw_data/archive_tbls_lst_result.rds")


#-----
# work on it
pkg_birth_dates <-
  map(archive_tbls_lst_result,
      ~.x %>%
        as_tibble(.name_repair = "universal") %>%
        mutate(Last.modified = ymd_hm(Last.modified)) %>%
        filter(Last.modified == min(Last.modified, na.rm = TRUE)))

saveRDS(pkg_birth_dates,
        "analysis/data/raw_data/pkg_birth_dates.rds")

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
  mutate(test_yn = if_else(is.na(test_size), 'n', 'y')) %>%
  group_by(birth_year, test_yn)  %>%
  tally() %>%
  spread(test_yn, n) %>%
  mutate(prop = y / (sum(n, y, na.rm = TRUE)))

ggplot(pkg_test_size_desc_birth_dates,
       aes(birth_year,
           prop )) +
  geom_col() +
  labs(y = "Proportion of packages per year with tests",
       x = "Year of pkg origin") +
  theme_minimal(base_size = 14) +
  scale_fill_viridis_d()

# test ratio and Aut/RD/Date/pkg_size
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
  gather(variable,
         value,
         -test_yn) %>%
  ggplot(aes(
             value,
             test_yn)) +
  geom_point() +
  stat_smooth(method="glm",
              method.args=list(family="binomial"),
              se=FALSE) +
  facet_wrap( ~ variable,
              scales = "free") +
  theme_minimal(base_size = 14) +
  ylim(0,1)


summary(pkg_test_size_desc_birth_dates)
