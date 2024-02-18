library(tidyverse)

### Testing on Mouse 11

df <- read_csv("mouse11cohort11.csv")
head(df)

# put in time order
df <- df %>% arrange(ms)

# just focus on back tags (otherwise lags will go wrong - though need to add these in eventually and figure out how to order data)
df <- df %>% filter(tag=="back_tag")

### Transitions

df$id <- paste(df$deviceid,df$antennaID,sep="-") #make antenna id specifically


df$idlag <- lag(df$id) # add column that is the lagged id

df$trans <- paste(df$idlag, df$id, sep=":") # which transition

## freq table of transitions
table(df$trans)[rev(order(table(df$trans)))]

# make into df so can annotate
ddf <- data.frame(table(df$trans)) %>% arrange(-Freq)

# each combination - what type of movement it was
ddf$type <- c(rep("tt",12),rep("eq",3),rep("tr",5),"tb",rep("tr",2),"eq","tr",rep("eq",3),
  "tb","eq","tb","eq","tb","tr",rep("tb",2),rep("eq",2),rep("tr",4),"ta","eq",
  rep("tb",3),"ta","tr",rep("ta",2),"tb","ta","tab",rep("tb",2),"tab","tb","tab","x",
  "ta","x",rep("ta",2),"tb","tab","ta",rep("tab",2),"ta","x","ta","x","tab",
  rep("x",2),"tab",rep("x",3),"tab",rep("ta",2),"tab","x","tab",rep("x",2),"tab",
  rep("x",21),"ta",rep("x",28),"tab",rep("x",4),"start")
ddf

#match in
df$type <- ddf$type[match(df$trans, ddf$Var1)]


df %>% as.data.frame() %>% head(300)


## histogram of frequencies of types
df$type <- reorder(df$type, table(df$type)[df$type])
ggplot(df, aes(x=type)) + 
  geom_bar(color='black',fill='#3aaeef',alpha=.75) +
  theme_classic() +
  coord_flip()



### back tag vs leg tag
df <- read_csv("C:/Users/jpc3454/Downloads/mouse11cohort11.csv")
head(df)

table(df$tag)



