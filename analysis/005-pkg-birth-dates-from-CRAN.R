# get pkg dates from here
library(rvest)
library(tidyverse)
library(lubridate)
library(ggbeeswarm)

# we need to look at the CRAN archive page for every pkg to get the first date

cran_pkg_names <- readRDS("cran_pkg_names.rds")

archive_url <- "https://cran.r-project.org/src/contrib/Archive/"

archive_urls <- map_chr(cran_pkg_names, ~str_glue('{archive_url}{.x}'))

get_archive_tables_safely <- 
  safely(function(x) read_html(x) %>% 
            html_nodes("table") %>% 
            html_table() %>% 
           .[[1]])

archive_tbls_lst <- 
  map(archive_urls[1:1000], ~get_archive_tables_safely(.x))

archive_tbls_lst_result <- 
transpose(archive_tbls_lst)$result







#-------------------------------
the_url <- "https://cran.r-project.org/web/packages/available_packages_by_date.html"


cran_dates <- read_html(the_url) 

cran_dates_tbl <- 
  cran_dates %>% 
  html_nodes("table") %>% 
  html_table()

cran_dates_tbl <- cran_dates_tbl[[1]]

saveRDS(cran_dates_tbl, "cran_dates_tbl.rds")
cran_dates_tbl <- readRDS("cran_dates_tbl.rds")
pkg_test_size_desc <- readRDS("pkg_test_size_desc.rds")

pkg_test_size_desc_birth_dates <- 
left_join(cran_dates_tbl, 
          pkg_test_size_desc,
          by = c("Package" = "cran_pkg_names")) %>% 
  mutate(birth_date = ymd(Date)) %>% 
  mutate(birth_year = round_date(birth_date, unit = "year"))  %>% 
  mutate(birth_year = str_trunc(as.character(birth_year), 4, side = "right", ellipsis = ""))
  
ggplot(pkg_test_size_desc_birth_dates,
       aes(birth_year,
           test_size_ratio)) +
  geom_boxplot() +
  geom_quasirandom(alpha = 0.2) +
  labs(y = "Ratio of test file size to pkg size",
       x = "Year of pkg origin") +
  scale_y_log10() +
  theme_minimal(base_size = 14)

summary(pkg_test_size_desc_birth_dates)
