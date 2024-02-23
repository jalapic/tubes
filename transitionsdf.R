### Transitions df

## Temporary way of getting list of transition types
df11 <- read_csv("data/mouse11cohort11.csv")
ddf11 <- make_df(df11)
ddf <- data.frame(table(ddf11$trans)) %>% arrange(-Freq)
ddf$type <- c(rep("tt",12),rep("eq",3),rep("tr",5),"tb",rep("tr",2),"eq","tr",rep("eq",3),
              "tb","eq","tb","eq","tb","tr",rep("tb",2),rep("eq",2),rep("tr",4),"ta","eq",
              rep("tb",3),"ta","tr",rep("ta",2),"tb","ta","tab",rep("tb",2),"tab","tb","tab","x",
              "ta","x",rep("ta",2),"tb","tab","ta",rep("tab",2),"ta","x","ta","x","tab",
              rep("x",2),"tab",rep("x",3),"tab",rep("ta",2),"tab","x","tab",rep("x",2),"tab",
              rep("x",21),"ta",rep("x",28),"tab",rep("x",4),"start")
ddf


trdf <- read_csv("transitions.csv")

ddf <- rbind(ddf[c(1,3)],trdf)

write.csv(ddf, "transitions.csv",row.names=F)
