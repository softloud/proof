# get pkg dates from here
library(rvest)
library(tidyverse)
library(lubridate)
library(ggbeeswarm)

# we need to look at the CRAN archive page for every pkg to get the first date

cran_pkg_names <- readRDS(here::here("analysis/data/raw-data/cran_pkg_names.rds"))

archive_url <- "https://cran.r-project.org/src/contrib/Archive/"

archive_urls <- map_chr(cran_pkg_names, ~str_glue('{archive_url}{.x}'))

get_archive_tables_safely <-
  safely(function(x) read_html(x) %>%
           html_nodes("table") %>%
           html_table() %>%
           .[[1]])

# this will take a while
archive_tbls_lst <-
  map(archive_urls, ~get_archive_tables_safely(.x))

archive_tbls_lst_result <-
  transpose(archive_tbls_lst)$result %>%
  set_names(cran_pkg_names) %>%
  compact()


saveRDS(archive_tbls_lst_result,
        here::here("analysis/data/raw-data/archive_tbls_lst_result.rds"))

