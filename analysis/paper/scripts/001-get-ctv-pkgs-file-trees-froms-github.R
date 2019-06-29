
library(tidyverse)
library(gh)

.token = ""  # get one from https://github.com/settings/tokens

library(ctv)
library(tidyverse)
library(pak)
library(lubridate)
library(ggridges)

# windows needs Cygwin
options(Ncpus = 6)
options("install.packages.compile.from.source" = "never")

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

# how many do we have?
cran_pkg_names <- taskview_pkg_id$name
length(cran_pkg_names)

# get file tree for each pkg in all the CTVs

gh_safely <- safely(gh)

pkg_trees <-
  map(cran_pkg_names,
      ~gh_safely("GET /repos/:username/:repo/git/trees/master?recursive=1",
                 username = "cran",
                 repo = .x,
                 .token = .token))

saveRDS(taskview_pkg_id,  here::here("analysis/data/raw-data/taskview_pkg_id.rds"))
saveRDS(pkg_trees,  here::here("analysis/data/raw-data/pkg_trees.rds"))
saveRDS(cran_pkg_names,  here::here("analysis/data/raw-data/cran_pkg_names.rds"))
rm(pkg_trees)
