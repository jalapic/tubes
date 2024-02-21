### Just rising data

source("~/tubes/functions.R", echo=TRUE)


data11 <- readRDS("data/cohort11_track_data.RDS")
data12 <- readRDS("data/cohort12_track_data.RDS")
data13 <- readRDS("data/cohort13_track_data.RDS")
data14 <- readRDS("data/cohort14_track_data.RDS")


## Cohort 11
l11 <- split(data11, data11$mouse)
l11 <- lapply(l11, make_df)
l11 <- lapply(l11, add_type)
unlist(lapply(l11, function(x) sum(is.na(x$type))))

## Cohort 13
l13 <- split(data13, data13$mouse)
l13 <- lapply(l13, make_df)
l13 <- lapply(l13, add_type)
unlist(lapply(l13, function(x) sum(is.na(x$type))))

## Need to get list of possible transitions for setup B.
