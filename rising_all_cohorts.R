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



## Cohort 11
table(data11$mouse) #mouse 9 remove
data11 <- data11 %>% filter(mouse!=9)
l11 <- split(data11, data11$mouse)
l11 <- lapply(l11, make_df)
l11 <- lapply(l11, add_type)
unlist(lapply(l11, function(x) sum(is.na(x$type))))

## Cohort 13
table(data13$mouse)
l13 <- split(data13, data13$mouse)
l13 <- lapply(l13, make_df)
l13 <- lapply(l13, add_type)
unlist(lapply(l13, function(x) sum(is.na(x$type))))

## Cohort 12
table(data12$mouse)
l12 <- split(data12, data12$mouse)
l12 <- lapply(l12, make_df)
l12 <- lapply(l12, add_type)
unlist(lapply(l12, function(x) sum(is.na(x$type))))

## Cohort 14
table(data14$mouse)
l14 <- split(data14, data14$mouse)
l14 <- lapply(l14, make_df)
l14 <- lapply(l14, add_type)
unlist(lapply(l14, function(x) sum(is.na(x$type))))
lapply(l14,head)


### Removal Info
ri <- read_csv("removal_info.csv")
ri %>% filter(cohort==11) %>% arrange(date)
ri %>% filter(cohort==13) %>% arrange(date)
ri %>% filter(cohort==12) %>% arrange(date)
ri %>% filter(cohort==14) %>% arrange(date)

### Total Activity 

# 24 hrs post alpha removal; 24 hrs pre alpha removal
# tt + tr + tra + trb + trab
trntypes <- c("tt","tr","tra","trb","trab")


l11.7 <- l11[[5]] #mouse 7
l11.7 %>% as.data.frame()
l11.7post <- l11.7 %>% filter(datetime>"2023-08-11 11:00:00 CDT", datetime <= "2023-08-12 11:00:00 CDT")
l11.7pre <- l11.7 %>% filter(datetime>"2023-08-10 11:00:00 CDT", datetime <= "2023-08-11 11:00:00 CDT")
l11.7post %>% filter(type %in% trntypes) %>% tally() #2990
l11.7pre %>% filter(type %in% trntypes) %>% tally() #981

l11.4 <- l11[[3]] #mouse 4
l11.4 %>% as.data.frame()
l11.4post <- l11.4 %>% filter(datetime>"2023-08-19 11:00:00 CDT", datetime <= "2023-08-20 11:00:00 CDT")
l11.4pre <- l11.4 %>% filter(datetime>"2023-08-18 11:00:00 CDT", datetime <= "2023-08-19 11:00:00 CDT")
l11.4post %>% filter(type %in% trntypes) %>% tally() #5451
l11.4pre %>% filter(type %in% trntypes) %>% tally() #2319


l13.2 <- l13[[1]] #mouse 2
l13.2 %>% as.data.frame()
l13.2post <- l13.2 %>% filter(datetime>"2023-09-08 11:00:00 CDT", datetime <= "2023-09-09 11:00:00 CDT")
l13.2pre <- l13.2 %>% filter(datetime>"2023-09-07 11:00:00 CDT", datetime <= "2023-09-08 11:00:00 CDT")
l13.2post %>% filter(type %in% trntypes) %>% tally() #6377
l13.2pre %>% filter(type %in% trntypes) %>% tally() #1190

l13.8 <- l13[[7]] #mouse 8
l13.8 %>% as.data.frame()
l13.8post <- l13.8 %>% filter(datetime>"2023-09-16 11:00:00 CDT", datetime <= "2023-09-17 11:00:00 CDT")
l13.8pre <- l13.8 %>% filter(datetime>"2023-09-15 11:00:00 CDT", datetime <= "2023-09-16 11:00:00 CDT")
l13.8post %>% filter(type %in% trntypes) %>% tally() #4927
l13.8pre %>% filter(type %in% trntypes) %>% tally() #1889


l12.11 <- l12[[9]] #mouse 11
l12.11 %>% as.data.frame()
l12.11post <- l12.11 %>% filter(datetime>"2023-08-07 11:00:00 CDT", datetime <= "2023-08-08 11:00:00 CDT")
l12.11pre <- l12.11 %>% filter(datetime>"2023-08-06 11:00:00 CDT", datetime <= "2023-08-07 11:00:00 CDT")
l12.11post %>% filter(type %in% trntypes) %>% tally() #8664
l12.11pre %>% filter(type %in% trntypes) %>% tally() #3208

l12.7 <- l12[[5]] #mouse 7
l12.7 %>% as.data.frame()
l12.7post <- l12.7 %>% filter(datetime>"2023-08-15 11:00:00 CDT", datetime <= "2023-08-16 11:00:00 CDT")
l12.7pre <- l12.7 %>% filter(datetime>"2023-08-14 11:00:00 CDT", datetime <= "2023-08-15 11:00:00 CDT")
l12.7post %>% filter(type %in% trntypes) %>% tally() #7217
l12.7pre %>% filter(type %in% trntypes) %>% tally() #1997


l12.10 <- l12[[8]] #mouse 10
l12.10 %>% as.data.frame()
l12.10post <- l12.10 %>% filter(datetime>"2023-08-23 11:00:00 CDT", datetime <= "2023-08-24 11:00:00 CDT")
l12.10pre <- l12.10 %>% filter(datetime>"2023-08-22 11:00:00 CDT", datetime <= "2023-08-23 11:00:00 CDT")
l12.10post %>% filter(type %in% trntypes) %>% tally() #13137
l12.10pre %>% filter(type %in% trntypes) %>% tally() #2219


l14.5 <- l14[[4]] #mouse 5
l14.5 %>% as.data.frame()
l14.5post <- l14.5 %>% filter(datetime>"2023-09-04 11:00:00 CDT", datetime <= "2023-09-05 11:00:00 CDT")
l14.5pre <- l14.5 %>% filter(datetime>"2023-09-03 11:00:00 CDT", datetime <= "2023-09-04 11:00:00 CDT")
l14.5post %>% filter(type %in% trntypes) %>% tally() #6908
l14.5pre %>% filter(type %in% trntypes) %>% tally() #1432

l14.8 <- l14[[7]] #mouse 8
l14.8 %>% as.data.frame()
l14.8post <- l14.8 %>% filter(datetime>"2023-09-12 11:00:00 CDT", datetime <= "2023-09-13 11:00:00 CDT")
l14.8pre <- l14.8 %>% filter(datetime>"2023-09-11 11:00:00 CDT", datetime <= "2023-09-12 11:00:00 CDT")
l14.8post %>% filter(type %in% trntypes) %>% tally() #9212
l14.8pre %>% filter(type %in% trntypes) %>% tally() #3942

l14.7 <- l14[[6]] #mouse 7
l14.7 %>% as.data.frame()
l14.7post <- l14.7 %>% filter(datetime>"2023-09-20 11:00:00 CDT", datetime <= "2023-09-21 11:00:00 CDT")
l14.7pre <- l14.7 %>% filter(datetime>"2023-09-19 11:00:00 CDT", datetime <= "2023-09-20 11:00:00 CDT")
l14.7post %>% filter(type %in% trntypes) %>% tally() #5660
l14.7pre %>% filter(type %in% trntypes) %>% tally() #2506

act.df <- data.frame(
time=c('post','pre'),
cohort=c(11,11,11,11,13,13,13,13,12,12,12,12,12,12,14,14,14,14,14,14),
mouse=c(7,7,4,4,2,2,8,8,11,11,7,7,10,10,5,5,8,8,7,7),
act=c(2990, 981, 5451,2319,6377,1190,4927,1889,8664,3208,7217,1997,13137,2219,6908,1432,9212,3942,5660,2506)
)
act.df$mouse <- paste(act.df$cohort,act.df$mouse,sep="-")
act.df$time <- factor(act.df$time, levels = c("pre","post"))

ggplot(act.df, aes(x = time, y = act, group = mouse, color = factor(cohort))) +
  geom_line(lwd=1) +
  geom_point(fill = "white", size = 4, shape = 21, alpha=.8) +
  theme_classic() +
  ggtitle("Tube transitions of beta males 24hr \n before and after alpha removal") +
  scale_color_manual(values = c("blue", "dodgerblue", "#aabbee", "darkblue")) +
  labs(color = "Cohort") +
  ylab("Total Tube Transitions in 24 hours")



### Compare riser to other animals.

# data11
# mouse <- 7 #mouse 7
# dtime <- "2023-08-11 11:00:00 CDT"

graph_riser <- function(df, dtime, mouse){
dtime <- as.POSIXct(dtime, format = "%Y-%m-%d %H:%M:%S")
df.f <- df %>% filter(datetime>dtime-hours(24)) %>% filter(datetime<=dtime+hours(24))
df.f$condition <- ifelse(df.f$datetime<=dtime, "pre","post")
l <- split(df.f, df.f$mouse)
l <- lapply(l, make_df)
l <- lapply(l, add_type)
df.f.all <- data.table::rbindlist(l)
df.f.sum <- df.f.all %>% filter(type %in% trntypes) %>% group_by(mouse,condition) %>% tally()
df.f.sum$condition <- factor(df.f.sum$condition, levels = c("pre","post"))
df.f.sum$val <- ifelse(df.f.sum$mouse==mouse, "riser", "other")

ggplot(df.f.sum, aes(x = condition, y = n, group = factor(mouse), color = factor(val))) +
  geom_line(lwd=1) +
  geom_point(fill = "white", size = 4, shape = 21, alpha=.8) +
  theme_classic() +
  scale_color_manual(values = c("firebrick", "darkblue")) +
  labs(color = "") +
  theme(legend.position = 'none')+
  ylab("Transitions")

}

p1=graph_riser(df = data11, dtime = "2023-08-11 11:00:00 CDT", mouse = 7)
p2=graph_riser(df = data11, dtime = "2023-08-19 11:00:00 CDT", mouse = 4)
p3=graph_riser(df = data13, dtime = "2023-09-08 11:00:00 CDT", mouse = 2)
p4=graph_riser(df = data13, dtime = "2023-09-16 11:00:00 CDT", mouse = 8)
p5=graph_riser(df = data12, dtime = "2023-08-07 11:00:00 CDT", mouse = 11)
p6=graph_riser(df = data12, dtime = "2023-08-15 11:00:00 CDT", mouse = 7)
p7=graph_riser(df = data12, dtime = "2023-08-23 11:00:00 CDT", mouse = 10)
p8=graph_riser(df = data14, dtime = "2023-09-04 11:00:00 CDT", mouse = 5)
p9=graph_riser(df = data14, dtime = "2023-09-12 11:00:00 CDT", mouse = 8)
p10=graph_riser(df = data14, dtime = "2023-09-20 11:00:00 CDT", mouse = 7)

library(gridExtra)
grid.arrange(p1+ggtitle("m7c11"),
             p2+ggtitle("m4c11"),
             p3+ggtitle("m2c12"),
             p4+ggtitle("m8c12"),
             p5+ggtitle("m11c13"),
             p6+ggtitle("m7c13"),
             p7+ggtitle("m10c13"),
             p8+ggtitle("m5c14"),
             p9+ggtitle("m8c14"),
             p10+ggtitle("m7c14"),
             nrow=2)



### Looking at activity over 24 hours post-rising.

# dtime <- "2023-08-11 11:00:00 CDT"
# mouse <- 7
# df <- data11

rise_time <- function(df,mouse,dtime){
dtime <- as.POSIXct(dtime, format = "%Y-%m-%d %H:%M:%S")
df.f <- df %>% filter(datetime>dtime) %>% filter(datetime<=dtime+hours(24))
minms <- df.f %>% ungroup() %>% arrange(datetime) %>%head(1) %>% .$ms 
df.f$mstime <- df.f$ms - minms
l <- split(df.f, df.f$mouse)
l <- lapply(l, make_df)
l <- lapply(l, add_type)
df.f.all <- data.table::rbindlist(l)
df.f.time <- df.f.all %>% filter(type %in% trntypes) %>% group_by(mouse) %>% arrange(mstime) %>% mutate(stepval = row_number())
df.f.time$val <- ifelse(df.f.time$mouse==mouse, "riser", "other")
df.f.time$hours <- df.f.time$mstime / (1000 * 60 * 60)

ggplot(df.f.time, aes(x=hours, y=stepval, group=factor(mouse), color=factor(val))) + 
  geom_step() + 
  theme_minimal() +
  ylab("Transitions") +
  xlab("Hours") +
  theme(legend.position='none') +
  scale_color_manual(values = c("firebrick", "darkblue")) 
}

pp1=rise_time(df = data11, dtime = "2023-08-11 11:00:00 CDT", mouse = 7)
pp2=rise_time(df = data11, dtime = "2023-08-19 11:00:00 CDT", mouse = 4)
pp3=rise_time(df = data13, dtime = "2023-09-08 11:00:00 CDT", mouse = 2)
pp4=rise_time(df = data13, dtime = "2023-09-16 11:00:00 CDT", mouse = 8)
pp5=rise_time(df = data12, dtime = "2023-08-07 11:00:00 CDT", mouse = 11)
pp6=rise_time(df = data12, dtime = "2023-08-15 11:00:00 CDT", mouse = 7)
pp7=rise_time(df = data12, dtime = "2023-08-23 11:00:00 CDT", mouse = 10)
pp8=rise_time(df = data14, dtime = "2023-09-04 11:00:00 CDT", mouse = 5)
pp9=rise_time(df = data14, dtime = "2023-09-12 11:00:00 CDT", mouse = 8)
pp10=rise_time(df = data14, dtime = "2023-09-20 11:00:00 CDT", mouse = 7)


grid.arrange(pp1+ggtitle("m7c11"),
             pp2+ggtitle("m4c11"),
             pp3+ggtitle("m2c12"),
             pp4+ggtitle("m8c12"),
             pp5+ggtitle("m11c13"),
             pp6+ggtitle("m7c13"),
             pp7+ggtitle("m10c13"),
             pp8+ggtitle("m5c14"),
             pp9+ggtitle("m8c14"),
             pp10+ggtitle("m7c14"),
             nrow=2)



### Make a graph of the median values - looks terrible, no need for it.

rise_time_df <- function(df,mouse,dtime){
  dtime <- as.POSIXct(dtime, format = "%Y-%m-%d %H:%M:%S")
  df.f <- df %>% filter(datetime>dtime) %>% filter(datetime<=dtime+hours(24))
  minms <- df.f %>% ungroup() %>% arrange(datetime) %>%head(1) %>% .$ms 
  df.f$mstime <- df.f$ms - minms
  l <- split(df.f, df.f$mouse)
  l <- lapply(l, make_df)
  l <- lapply(l, add_type)
  df.f.all <- data.table::rbindlist(l)
  df.f.time <- df.f.all %>% filter(type %in% trntypes) %>% group_by(mouse) %>% arrange(mstime) %>% mutate(stepval = row_number())
  df.f.time$val <- ifelse(df.f.time$mouse==mouse, "riser", "other")
  df.f.time$hours <- df.f.time$mstime / (1000 * 60 * 60)
  return(df.f.time)
}

d1=rise_time_df(df = data11, dtime = "2023-08-11 11:00:00 CDT", mouse = 7)
d2=rise_time_df(df = data11, dtime = "2023-08-19 11:00:00 CDT", mouse = 4)
d3=rise_time_df(df = data13, dtime = "2023-09-08 11:00:00 CDT", mouse = 2)
d4=rise_time_df(df = data13, dtime = "2023-09-16 11:00:00 CDT", mouse = 8)
d5=rise_time_df(df = data12, dtime = "2023-08-07 11:00:00 CDT", mouse = 11)
d6=rise_time_df(df = data12, dtime = "2023-08-15 11:00:00 CDT", mouse = 7)
d7=rise_time_df(df = data12, dtime = "2023-08-23 11:00:00 CDT", mouse = 10)
d8=rise_time_df(df = data14, dtime = "2023-09-04 11:00:00 CDT", mouse = 5)
d9=rise_time_df(df = data14, dtime = "2023-09-12 11:00:00 CDT", mouse = 8)
d10=rise_time_df(df = data14, dtime = "2023-09-20 11:00:00 CDT", mouse = 7)

actdf1 <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10) %>%
  select(mouse,cohort,val,hours,type,mstime,stepval) %>%
  mutate(mouseid=paste(mouse,cohort,sep="-")) 

actdf1.sum <- actdf1 %>%
  group_by(val,stepval) %>%
  summarize(med = median(mstime),
            lq = quantile(mstime,.25),
            uq = quantile(mstime,.75)
            )

ggplot(actdf1.sum, aes(x = stepval, y = med, color=val, fill=val)) +
  geom_line() + 
  geom_ribbon(aes(ymin = lq, ymax = uq), alpha = 0.3) + 
  coord_flip()




### Compared to Sham removals;



### Follows analysis

## Just doing for one cohort, at one rising time


tubetrans <- c("9-1:9-2", "9-2:9-1",
               "1-1:1-2", "1-2:1-1",
               "2-1:2-2", "2-2:2-1",
               "3-1:3-2", "3-2:3-1",
               "4-1:4-2", "4-2:4-1",
               "8-1:8-2", "8-2:8-1"
)

tubetrans2 <- c("19-1:19-2", "19-2:19-1",
               "16-1:16-2", "16-2:16-1",
               "21-1:21-2", "21-2:21-1",
               "20-1:20-2", "20-2:20-1",
               "17-1:17-2", "17-2:17-1",
               "18-1:18-2", "18-2:18-1"
)



riser_follow <- function(dtime,datad,tubes=tubetrans,mouseid) {

dtime <- as.POSIXct(dtime, format = "%Y-%m-%d %H:%M:%S")
d.pre <- datad %>% filter(datetime>dtime-hours(24)) %>% filter(datetime<=dtime)
d.post <- datad %>% filter(datetime>dtime) %>% filter(datetime<=dtime+hours(24))
lpre <- split(d.pre, d.pre$mouse)
lpost <- split(d.post, d.post$mouse)
lpre <- lapply(lpre, make_df)
lpre <- lapply(lpre, add_type)
lpost <- lapply(lpost, make_df)
lpost <- lapply(lpost, add_type)
allpre <- map_dfr(tubes, ~ get_pairs_df(lpre, tt = .x, win = 500), .id = "tubes")
allpre$tubes <- tubes[as.numeric(allpre$tubes)]
allpost <- map_dfr(tubes, ~ get_pairs_df(lpost, tt = .x, win = 500), .id = "tubes")
allpost$tubes <- tubes[as.numeric(allpost$tubes)]
#get initial time.
ms.start <- round((max(d.pre$ms) + min(d.post$ms))/2,0)
all <- rbind(allpre,allpost) %>% mutate(time = value2-ms.start)
all$hours <- all$time / (1000 * 60 * 60)
all <- all %>% group_by(vector2) %>% arrange(hours) %>% mutate(value = row_number())
all$condition <- ifelse(all$vector2==mouseid, "riser", "other")

ggplot(all, aes(x=hours, y=value, group=factor(vector2), color=condition))+ 
  geom_step() + 
  theme_minimal() +
  ylab("Follows") +
  xlab("Hours") +
  theme(legend.position='none') +
  scale_color_manual(values = c("firebrick", "darkblue")) +
  geom_vline(xintercept = 0, color='black', lty=2)

}

riser_follow(dtime = "2023-08-11 11:00:00 CDT", datad=data11,tubes=tubetrans, mouseid=7)

dd1=riser_follow(datad = data11, dtime = "2023-08-11 11:00:00 CDT",tubes=tubetrans, mouse = 7)
dd2=riser_follow(datad = data11, dtime = "2023-08-19 11:00:00 CDT",tubes=tubetrans, mouse = 4)
dd3=riser_follow(datad = data13, dtime = "2023-09-08 11:00:00 CDT",tubes=tubetrans, mouse = 2)
dd4=riser_follow(datad = data13, dtime = "2023-09-16 11:00:00 CDT",tubes=tubetrans, mouse = 8)
dd5=riser_follow(datad = data12, dtime = "2023-08-07 11:00:00 CDT",tubes=tubetrans2, mouse = 11)
dd6=riser_follow(datad = data12, dtime = "2023-08-15 11:00:00 CDT",tubes=tubetrans2, mouse = 7)
dd7=riser_follow(datad = data12, dtime = "2023-08-23 11:00:00 CDT",tubes=tubetrans2, mouse = 10)
#dd8=riser_follow(datad = data14, dtime = "2023-09-04 11:00:00 CDT",tubes=tubetrans2, mouse = 5)
dd9=riser_follow(datad = data14, dtime = "2023-09-12 11:00:00 CDT",tubes=tubetrans2, mouse = 8)
#dd10=riser_follow(datad = data14, dtime = "2023-09-20 11:00:00 CDT",tubes=tubetrans2, mouse = 7)


##dd8 and dd10 not working in function for some reason - so doing manually

#dd8 troubleshoot
dtime <- as.POSIXct('2023-09-04 11:00:00 CDT', format = "%Y-%m-%d %H:%M:%S")
d.pre <- data14%>% filter(datetime>dtime-hours(24)) %>% filter(datetime<=dtime)
d.post <- data14 %>% filter(datetime>dtime) %>% filter(datetime<=dtime+hours(24))
lpre <- split(d.pre, d.pre$mouse)
lpost <- split(d.post, d.post$mouse)
lpre <- lapply(lpre, make_df)
lpre <- lapply(lpre, add_type)
lpost <- lapply(lpost, make_df)
lpost <- lapply(lpost, add_type)
tubes <- tubetrans2

res<-NULL
for(i in 1:length(tubetrans2)){
vecs <- map(lpre, ~ .x %>% filter(trans == tt) %>% pull(ms))
res[[i]] <- find_pairs1(vecs, w=500)
}
outdf <- data.table::rbindlist(res)

matchdf <- data.frame(vector = paste0('vec',1:length(names(lpre))),
                      mouseid = names(lpre)
)
outdf$vector1<- matchdf$mouseid[match(outdf$vector1, matchdf$vector)]
outdf$vector2<- matchdf$mouseid[match(outdf$vector2, matchdf$vector)]
outdf

res1<-NULL
for(i in 1:length(tubetrans2)){
  vecs <- map(lpost, ~ .x %>% filter(trans == tt) %>% pull(ms))
  res1[[i]] <- find_pairs1(vecs, w=500)
}
outdf1 <- data.table::rbindlist(res1)
outdf1$vector1<- matchdf$mouseid[match(outdf1$vector1, matchdf$vector)]
outdf1$vector2<- matchdf$mouseid[match(outdf1$vector2, matchdf$vector)]
outdf1


#get initial time.
ms.start <- round((max(d.pre$ms) + min(d.post$ms))/2,0)
all <- rbind(outdf,outdf1) %>% mutate(time = value2-ms.start)
all$hours <- all$time / (1000 * 60 * 60)
all <- all %>% group_by(vector2) %>% arrange(hours) %>% mutate(value = row_number())
all$condition <- ifelse(all$vector2==5, "riser", "other")

dd8<-ggplot(all, aes(x=hours, y=value, group=factor(vector2), color=condition))+ 
  geom_step() + 
  theme_minimal() +
  ylab("Follows") +
  xlab("Hours") +
  theme(legend.position='none') +
  scale_color_manual(values = c("firebrick", "darkblue")) +
  geom_vline(xintercept = 0, color='black', lty=2)


#dd10 troubleshoot
dtime <- as.POSIXct('2023-09-20 11:00:00 CDT', format = "%Y-%m-%d %H:%M:%S")
d.pre <- data14%>% filter(datetime>dtime-hours(24)) %>% filter(datetime<=dtime)
d.post <- data14 %>% filter(datetime>dtime) %>% filter(datetime<=dtime+hours(24))
lpre <- split(d.pre, d.pre$mouse)
lpost <- split(d.post, d.post$mouse)
lpre <- lapply(lpre, make_df)
lpre <- lapply(lpre, add_type)
lpost <- lapply(lpost, make_df)
lpost <- lapply(lpost, add_type)
tubes <- tubetrans2

res<-NULL
for(i in 1:length(tubetrans2)){
  vecs <- map(lpre, ~ .x %>% filter(trans == tt) %>% pull(ms))
  res[[i]] <- find_pairs1(vecs, w=500)
}
outdf <- data.table::rbindlist(res)

matchdf <- data.frame(vector = paste0('vec',1:length(names(lpre))),
                      mouseid = names(lpre)
)
outdf$vector1<- matchdf$mouseid[match(outdf$vector1, matchdf$vector)]
outdf$vector2<- matchdf$mouseid[match(outdf$vector2, matchdf$vector)]
outdf

res1<-NULL
for(i in 1:length(tubetrans2)){
  vecs <- map(lpost, ~ .x %>% filter(trans == tt) %>% pull(ms))
  res1[[i]] <- find_pairs1(vecs, w=500)
}
outdf1 <- data.table::rbindlist(res1)
outdf1$vector1<- matchdf$mouseid[match(outdf1$vector1, matchdf$vector)]
outdf1$vector2<- matchdf$mouseid[match(outdf1$vector2, matchdf$vector)]
outdf1


#get initial time.
ms.start <- round((max(d.pre$ms) + min(d.post$ms))/2,0)
all <- rbind(outdf,outdf1) %>% mutate(time = value2-ms.start)
all$hours <- all$time / (1000 * 60 * 60)
all <- all %>% group_by(vector2) %>% arrange(hours) %>% mutate(value = row_number())
all$condition <- ifelse(all$vector2==7, "riser", "other")

dd10<-ggplot(all, aes(x=hours, y=value, group=factor(vector2), color=condition))+ 
  geom_step() + 
  theme_minimal() +
  ylab("Follows") +
  xlab("Hours") +
  theme(legend.position='none') +
  scale_color_manual(values = c("firebrick", "darkblue")) +
  geom_vline(xintercept = 0, color='black', lty=2)


grid.arrange(dd1+ggtitle("m7c11"),
             dd2+ggtitle("m4c11"),
             dd3+ggtitle("m2c12"),
             dd4+ggtitle("m8c12"),
             dd5+ggtitle("m11c13"),
             dd6+ggtitle("m7c13"),
             dd7+ggtitle("m10c13"),
             dd8+ggtitle("m5c14"),
             dd9+ggtitle("m8c14"),
             dd10+ggtitle("m7c14"),
             nrow=2)


