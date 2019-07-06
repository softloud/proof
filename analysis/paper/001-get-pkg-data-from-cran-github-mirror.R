
library(tidyverse)
library(gh)

.token = ""  # get one from https://github.com/settings/tokens

# get all pkgs on GitHub mirror of CRAN
n <- 16125 # 16125 # https://github.com/cran
cran_repos <- gh("/users/:username/repos",
                 username = "cran",
                 .limit = n,
                 .token = .token )

# extract pkg names
cran_pkg_names <- vapply(cran_repos, "[[", "", "name")
cran_pkg_created <- vapply(cran_repos, "[[", "", "created_at")

# how many do we have?
length(cran_pkg_names)

# get tree for each pkg

gh_safely <- safely(gh)

pkg_trees <-
  map(cran_pkg_names,
      ~gh_safely("GET /repos/:username/:repo/git/trees/master?recursive=1",
                 username = "cran",
                 repo = .x,
                 .token = .token))

saveRDS(pkg_trees, "proof/pkg_trees.rds")
saveRDS(cran_repos, "proof/cran_repos.rds")
saveRDS(cran_pkg_names, "proof/cran_pkg_names.rds")
rm(pkg_trees)
rm(cran_repos)


# how many pkgs have tests?
# how does test presence/absense/size relate to...
# pkg age/size/contributors/deps
# where do people put tests? toplevel tests/ or inst/ or what?
# what frameworks do they use?
# what testing fns do they use? expect_? are they just testing for correct class? budget!
# what is the frequency of testing taxa?

# related reading

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5500893/
# https://yihui.name/en/2013/09/testing-r-packages/
# http://www.win-vector.com/blog/2019/03/unit-tests-in-r/
# RUnit (released June 2004) itself collects test suites from directories and then runs them, recording user assertions in a JUnit-inspired report. The idea is that once you have a bunch of tests you really want to track them some way.
# testthat (released November 2009) self-describes as integrating into a workflow. It runs tests found in the tests/testthat sub-directory (directory found relative to the package source, not relative to an installed package) and tracks user assertions. The related devtools/usethis package both writes a canonical test controlling file into the tests directory (allowing testthat to be triggered by "R CMD check"), and can also directly run tests.
# unitizer (released April 2017) bases its tests on comparisons of objects, rather than comparing text or requiring user assertions. It also aids in producing and updating reference objects.
# tinytest (pre-release)
# tests can be in test/ or inst/
# https://www.mango-solutions.com/blog/analyzing-coverage-of-r-unit-tests-in-packages-the-testcoverage-package
# analysis using revdeps, "These numbers equate to around 8% of R packages on CRAN containing any kind of recognised test framework."
# http://r-pkgs.had.co.nz/tests.html
# https://journal.r-project.org/archive/2012/RJ-2012-018/RJ-2012-018.pdf

