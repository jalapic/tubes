
#### Mouse 11, 2, 6

library(tidyverse)

df11 <- read_csv("mouse11cohort11.csv")
df2 <- read_csv("mouse2cohort11.csv")
df6 <- read_csv("mouse6cohort11.csv")


# function to get back tags in time order
make_df  <- function(df){
  df %>% 
    arrange(ms) %>% 
    filter(tag=="back_tag") %>%
    mutate(id = paste(deviceid,antennaID,sep="-")) %>%
    mutate(idlag = lag(id)) %>%
    mutate(trans = paste(idlag, id, sep=":"))
}

ddf11 <- make_df(df11)
ddf2 <- make_df(df2)
ddf6 <- make_df(df6)


### Using Mouse 11's transitions to make table to match in transitions for all

## freq table of transitions
table(ddf11$trans)[rev(order(table(ddf11$trans)))]
# make into df so can annotate
ddf <- data.frame(table(ddf11$trans)) %>% arrange(-Freq)
# each combination - what type of movement it was
ddf$type <- c(rep("tt",12),rep("eq",3),rep("tr",5),"tb",rep("tr",2),"eq","tr",rep("eq",3),
              "tb","eq","tb","eq","tb","tr",rep("tb",2),rep("eq",2),rep("tr",4),"ta","eq",
              rep("tb",3),"ta","tr",rep("ta",2),"tb","ta","tab",rep("tb",2),"tab","tb","tab","x",
              "ta","x",rep("ta",2),"tb","tab","ta",rep("tab",2),"ta","x","ta","x","tab",
              rep("x",2),"tab",rep("x",3),"tab",rep("ta",2),"tab","x","tab",rep("x",2),"tab",
              rep("x",21),"ta",rep("x",28),"tab",rep("x",4),"start")
ddf


#match in mouse 11
ddf11$type <- ddf$type[match(ddf11$trans, ddf$Var1)]
#check no NA
sum(is.na(ddf11$type)) #0

#match in mouse 2
ddf2$type <- ddf$type[match(ddf2$trans, ddf$Var1)]
#check no NA
sum(is.na(ddf2$type)) #0

#match in mouse 6
ddf6$type <- ddf$type[match(ddf6$trans, ddf$Var1)]
#check no NA
sum(is.na(ddf6$type)) #0


### Example: 9-2:9-1

ddf6
ddf11
ddf2

ddf11$trans

ddf11.9291 <- ddf11 %>% filter(trans=="9-2:9-1") %>% .$ms
ddf2.9291 <- ddf2 %>% filter(trans=="9-2:9-1") %>% .$ms
ddf6.9291 <- ddf6 %>% filter(trans=="9-2:9-1") %>% .$ms

df.9291<-data.frame(
 mouse_id = c(rep('m11',length(ddf11.9291)),
   rep('m2',length(ddf2.9291)),
   rep('m6',length(ddf6.9291))
 ),
 ms = c(ddf11.9291,ddf2.9291,ddf6.9291)
)
head(df.9291)

ggplot(df.9291, aes(x=ms,y=mouse_id,color=mouse_id)) +
  geom_point(shape = "|") +
  theme_minimal() +
  ggtitle("9-2 to 9-1 tube transition")


## zoom in 

ggplot(df.9291 %>% filter(ms<100000000), aes(x=ms,y=mouse_id,color=mouse_id)) +
  geom_point(shape = "|", size=7) +
  theme_minimal() +
  ggtitle("9-2 to 9-1 tube transition")


### Find which ones are close in time.

# created find_pairs1 function in separate R file.

vecs <- list(ddf11.9291, ddf2.9291, ddf6.9291)
vecs

outdf <- find_pairs1(vecs, w=500)

matchdf <- data.frame(vector = c('vec1','vec2','vec3'),
           mouseid = c("m11","m2","m6"))

outdf$vector1<- matchdf$mouseid[match(outdf$vector1, matchdf$vector)]
outdf$vector2<- matchdf$mouseid[match(outdf$vector2, matchdf$vector)]

outdf




