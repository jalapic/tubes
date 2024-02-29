### Just rising data

source("functions.R", echo=TRUE)


data11 <- readRDS("data/cohort11_track_data.RDS")
data12 <- readRDS("data/cohort12_track_data.RDS")
data13 <- readRDS("data/cohort13_track_data.RDS")
data14 <- readRDS("data/cohort14_track_data.RDS")


## add timestamps
data11$datetime <- as.POSIXct(data11$datetimestamp, format = "%d.%m.%Y %H:%M:%S:%OS")
data12$datetime <- as.POSIXct(data12$datetimestamp, format = "%d.%m.%Y %H:%M:%S:%OS")
data13$datetime <- as.POSIXct(data13$datetimestamp, format = "%d.%m.%Y %H:%M:%S:%OS")
data14$datetime <- as.POSIXct(data14$datetimestamp, format = "%d.%m.%Y %H:%M:%S:%OS")


### Removal Info
ri <- read_csv("removal_info.csv")
ri %>% filter(cohort==11) %>% arrange(date)
ri %>% filter(cohort==13) %>% arrange(date)
ri %>% filter(cohort==12) %>% arrange(date)
ri %>% filter(cohort==14) %>% arrange(date)



### Activity over Time - Controls vs Risers 
trntypes <- c("tt","tr","tra","trb","trab")



get_24hrs_activity <- function(df, dtime, mouseid){
  dtime <- as.POSIXct(dtime, format = "%Y-%m-%d %H:%M:%S")

  mstime <- df %>% filter(datetime < dtime) %>% ungroup() %>% arrange(ms) %>% filter(ms == max(ms)) %>% .$ms
  
  df.f <- df %>% 
    filter(datetime>dtime-hours(24)) %>% 
    filter(datetime<=dtime+hours(24)) %>%
    filter(mouse==mouseid)
  df.f.all <- add_type(make_df(df.f))
  df.f.sum <- df.f.all %>% 
    filter(type %in% trntypes) %>% 
    arrange(dtime)  %>%
    mutate(stepval = row_number()) %>%
    select(cohort,mouse,day,date,time,datetime,ms,stepval)
  df.f.sum$condition <- ifelse(df.f.sum$datetime<=dtime, "pre", "post")
  df.f.sum$mstime <- df.f.sum$ms - mstime
  df.f.sum$hours <- df.f.sum$mstime / (1000 * 60 * 60)
  
  return(df.f.sum)

}

#cohort 11
m1.11 <- get_24hrs_activity(df = data11, dtime = "2023-08-07 11:00:00 CDT", mouse = 1)
m6.11 <- get_24hrs_activity(df = data11, dtime = "2023-08-15 11:00:00 CDT", mouse = 6)
m12.11 <- get_24hrs_activity(df = data11, dtime = "2023-08-23 11:00:00 CDT", mouse = 12)
m7.11 <- get_24hrs_activity(df = data11, dtime = "2023-08-11 11:00:00 CDT", mouse = 7)
m4.11 <- get_24hrs_activity(df = data11, dtime = "2023-08-19 11:00:00 CDT", mouse = 4)

m1.11$group <- "control"
m6.11$group <- "control"
m12.11$group <- "control"
m7.11$group <- "rising"
m4.11$group <- "rising"


#cohort 12
m11.12 <- get_24hrs_activity(df = data12, dtime = "2023-08-07 11:00:00 CDT", mouse = 11)
m7.12 <- get_24hrs_activity(df = data12, dtime = "2023-08-15 11:00:00 CDT", mouse = 7)
m10.12 <- get_24hrs_activity(df = data12, dtime = "2023-08-23 11:00:00 CDT", mouse = 10)
m4.12 <- get_24hrs_activity(df = data12, dtime = "2023-08-11 11:00:00 CDT", mouse = 4)
m2.12 <- get_24hrs_activity(df = data12, dtime = "2023-08-19 11:00:00 CDT", mouse = 2)

m11.12$group <- "rising"
m7.12$group <- "rising"
m10.12$group <- "rising"
m4.12$group <- "control"
m2.12$group <- "control"



#cohort 13
m2.13 <- get_24hrs_activity(df = data13, dtime = "2023-09-08 11:00:00 CDT", mouse = 2)
m8.13 <- get_24hrs_activity(df = data13, dtime = "2023-09-16 11:00:00 CDT", mouse = 8)
m9.13 <- get_24hrs_activity(df = data13, dtime = "2023-09-04 11:00:00 CDT", mouse = 9)
m4.13 <- get_24hrs_activity(df = data13, dtime = "2023-09-12 11:00:00 CDT", mouse = 4)
m12.13 <- get_24hrs_activity(df = data13, dtime = "2023-09-20 11:00:00 CDT", mouse = 12)

m2.13$group <- "rising"
m8.13$group <- "rising"
m9.13$group <- "control"
m4.13$group <- "control"
m12.13$group <- "control"


#cohort 14
m5.14 <- get_24hrs_activity(df = data14, dtime = "2023-09-04 11:00:00 CDT", mouse = 5)
m8.14 <- get_24hrs_activity(df = data14, dtime = "2023-09-12 11:00:00 CDT", mouse = 8)
m7.14 <- get_24hrs_activity(df = data14, dtime = "2023-09-20 11:00:00 CDT", mouse = 7)
m6.14 <- get_24hrs_activity(df = data14, dtime = "2023-09-08 11:00:00 CDT", mouse = 6)
m11.14 <- get_24hrs_activity(df = data14, dtime = "2023-09-16 11:00:00 CDT", mouse = 11)

m5.14$group <- "rising"
m8.14$group <- "rising"
m7.14$group <- "rising"
m6.14$group <- "control"
m11.14$group <- "control"


## full dataframe

x<-rbind(m1.11,m6.11,m12.11,m7.11,m4.11,
         m11.12, m7.12, m10.12, m4.12, m2.12, 
         m2.13, m8.13, m9.13, m4.13, m12.13, 
         m5.14, m8.14, m7.14, m6.14, m11.14)


x$id <- paste(x$mouse, x$cohort, sep="-")

head(x)



### Graphing

ggplot(x, aes(x=hours, y=stepval, group=id, color=group)) +
  geom_step() +
  geom_vline(xintercept=0, lty=2, lwd=1) + 
  ylab("transitions")


# rising only
x1 <- x %>% filter(condition=='post') %>% 
  group_by(id)  %>%
  mutate(newstepval = stepval-min(stepval))


ggplot(x1, aes(x=hours, y=newstepval, group=id, color=group)) +
  geom_step(alpha=.8) +
  ylab("transitions") +
  theme_minimal() +
  scale_color_manual(values=c("firebrick", "dodgerblue"))

