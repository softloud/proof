
# pkg metadata from CRAN ------------------------------------
# https://rviews.rstudio.com/2018/03/08/cran-package-metadata/
# https://www.rstudio.com/resources/videos/what-makes-a-great-r-package/

pkg_DESC <- tools:::CRAN_package_db()
meta_data <- pkg_DESC[, c(1,4,5,17,37,60,61)]
names(meta_data) <- c("Package","Dep","Imp","Aut","Date","RD","RI")
library(stringr)
# A helper function to unlist and string split.
fcn <- function(x,y){
  x <- unlist(x) %>% strsplit(",")
  y <- unlist(y) %>% strsplit(",")
  z <- unlist(na.omit(union(x, y)))
}

meta_data <- mutate(meta_data,
                    DepImp = mapply(fcn,Dep,Imp),
                    RDRI = mapply(fcn,RD,RI))

# CLEAN THE AUTHOR'S FIELD
# Function to remove all text between two brackets
# http://bit.ly/2mE7TNJ
clean <- function(x){
  gsub("\\[[^]]*]", "",x)
}

# Function to remove line breaks
# http://bit.ly/2B0n4VS

clean2 <- function(x){
  gsub("[\r\n]", "", x)
}

# Clean Author's field
meta_data$Aut <- meta_data$Aut %>% map(clean) %>% map(clean2)

rm_na <- function(x){
  list(na.omit(unlist(x)))
}


# Process the fields Aut, Dep, Imp, RD, RI
c_dat1 <- seq_len(nrow(meta_data)) %>%
  map_df(~{
    meta_data[.x, ] %>%
      select(-Package,-DepImp,-RDRI) %>%
      map_df(~ifelse(is.na(.x), 0, length(str_split(.x, ",")[[1]]))) %>%
      mutate(Package = meta_data$Package[.x])
  }) %>%
  select(Package, Aut, Dep, Imp, RD, RI)
# head(c_dat1)

# Process the fields DepImp RDRI
c_dat2 <- seq_len(nrow(meta_data)) %>%
  map_df(~{
    meta_data[.x, ] %>%
      select(-Package, -Aut, -Dep, -Imp, -RD, -RI, -Date) %>%
      map_df(~ifelse(is.na(.x), 0, length(rm_na(.x)[[1]])))
  }) %>%
  select(DepImp, RDRI)

# head(c_dat2)
c_dat <- bind_cols(c_dat1, c_dat2, date = meta_data$Date)

head(c_dat)

saveRDS(c_dat, "analysis/data/raw_data/c_dat.rds")

ss <- function(x){
  avg <- round(mean(x),digits=2)
  std <- round(sd(x),digits=2)
  med <- median(x)
  res <- list(mean = avg, sd = std, median = med)
}

res <- cbind(names(c_dat[,-c(1,9)]), map_df(c_dat[,-c(1,9)],ss))
names(res) <- c("Features", "mean", "sd", "median")
res

quantile(c_dat$RDRI)
