

### Try and automate code for a "given" tube transition.

# Dataframes for each mouse
ddf6
ddf11
ddf2

# list of dataframes
#  tube transition

tt<-"9-2:9-1"
l <- list(ddf6,ddf11,ddf2)
names(l) <- c("m6","m11","m2")

follows <- function(l, tt, win){

  vecs <- map(l, ~ .x %>% filter(trans == tt) %>% pull(ms))
  outdf <- find_pairs1(vecs, w=win)
  
  matchdf <- data.frame(vector = c('vec1','vec2','vec3'),
                      mouseid = names(l))

outdf$vector1<- matchdf$mouseid[match(outdf$vector1, matchdf$vector)]
outdf$vector2<- matchdf$mouseid[match(outdf$vector2, matchdf$vector)]

return(outdf)
}

follows(l, tt="9-2:9-1", win=500)
follows(l, tt="9-1:9-2", win=500)


### Make list of tube transitions

tubetrans <- c("9-1:9-2", "9-2:9-1",
               "1-1:1-2", "1-2:1-1",
               "2-1:2-2", "2-2:2-1",
               "3-1:3-2", "3-2:3-1",
               "4-1:4-2", "4-2:4-1",
               "8-1:8-2", "8-2:8-1"
)

tubetrans

# iterate over list
result_df <- map_dfr(tubetrans, ~ follows(l, tt = .x, win = 500), .id = "tubetrans")
result_df

# change into a win loss matrix
head(result_df)
chdf <- data.frame(winner=result_df[,5],
           loser = result_df[,4]
)


compete::org_matrix(compete::get_wl_matrix(chdf), method = "ds")


# organize by time

result_df$tubetrans <- tubetrans[as.numeric(result_df$tubetrans)]

result_df1 <- result_df %>% 
  arrange(value1) %>% 
  mutate(dif = value1 - lag(value1))

result_df1[167:320,]
result_df1[321:396,]
