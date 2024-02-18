data <- readRDS("cohort11_track_data.RDS")
head(data)
table(data$date)
table(data$day)

## All data prior to first removal.

# 8th August 12pm (sham removal 1)
# 11th August 11am (actual removal 11)

d8 <-  data %>% filter(day<=8)
d8


## get each mouse in list and make dataset of ms times 
l8 <- split(d8, d8$mouse)
l8 <- lapply(l8, make_df)

#function to add in transition type
add_type <- function(dfdf){
dfdf$type <- ddf$type[match(dfdf$trans, ddf$Var1)]
return(dfdf)
}

l8 <- lapply(l8, add_type)

#check no missing transition types
lapply(l8, function(x) sum(is.na(x$type)))


### Get follows ( this is just for 9-2:9-1)

vecs <- map(l8, ~ .x %>% filter(trans == tt) %>% pull(ms))
vecs

# lapply(vecs, head) # mouse number 9 (element 7 is empty)
# d8 %>% filter(mouse=='9') %>% filter(tag=="leg_tag") #seems to have no back tag, only 1 day of data
# data %>% filter(mouse=='9')


#need a step to discard empty elements in this list.

names(vecs)
vecs0 <- which(unlist(lapply(vecs, length))==0)


drop_els <- function(lst, ind) {
  if (is.null(ind) || length(ind) == 0) {
    return(lst)
  }
# Filter out elements at specified indices
  rem <- setdiff(seq_along(lst), ind)
  return(lst[rem])
}

vecs1 <- drop_els(vecs, vecs0)
names(vecs1)


# find pairs
outdf <- find_pairs1(vecs1, w=500)
 
matchdf <- data.frame(vector = paste0('vec',1:length(vecs1)),
                        mouseid = names(vecs1)
                        )
outdf$vector1<- matchdf$mouseid[match(outdf$vector1, matchdf$vector)]
outdf$vector2<- matchdf$mouseid[match(outdf$vector2, matchdf$vector)]
  
outdf


### Put above into one function:

get_pairs_df <- function(ll,tt,win=500){
vecs <- map(ll, ~ .x %>% filter(trans == tt) %>% pull(ms))
vecs0 <- which(unlist(lapply(vecs, length))==0)
vecs1 <- drop_els(vecs, vecs0)
outdf <- find_pairs1(vecs1, w=win)# find pairs
matchdf <- data.frame(vector = paste0('vec',1:length(vecs1)),
                      mouseid = names(vecs1)
)
outdf$vector1<- matchdf$mouseid[match(outdf$vector1, matchdf$vector)]
outdf$vector2<- matchdf$mouseid[match(outdf$vector2, matchdf$vector)]
return(outdf)
}

out.9291 <- get_pairs_df(l8,tt="9-2:9-1", win=500)
out.9291

out.9192 <- get_pairs_df(l8,tt="9-1:9-2", win=500)
out.9192


### Repeat above for all


tubetrans <- c("9-1:9-2", "9-2:9-1",
               "1-1:1-2", "1-2:1-1",
               "2-1:2-2", "2-2:2-1",
               "3-1:3-2", "3-2:3-1",
               "4-1:4-2", "4-2:4-1",
               "8-1:8-2", "8-2:8-1"
)

tubetrans

# iterate over list
alldf <- map_dfr(tubetrans, ~ get_pairs_df(l8, tt = .x, win = 500), .id = "tubetrans")
alldf %>% arrange(value1)

alldf$tubetrans <- tubetrans[as.numeric(alldf$tubetrans)]

alldf1 <- alldf %>% 
  arrange(value1) %>% 
  mutate(dif = value1 - lag(value1))

alldf1


### Calculate hierarchy dynamics

# change into a win loss matrix
head(alldf)
chdf <- data.frame(winner=alldf[,5],
                   loser = alldf[,4]
)


compete::org_matrix(compete::get_wl_matrix(chdf), method = "ds")
compete::devries(compete::get_wl_matrix(chdf))
#h-modified` [1] 0.8583333
#$`p-value` [1] 5e-04

compete::ds(compete::get_wl_matrix(chdf))
# 1         10         11         12          2          4          6          7          8 
# 5.940667 -10.974766  29.558294  -4.735198   1.637647 -19.198283  -4.366852  13.765625 -11.627134 

compete::get_di_matrix(compete::org_matrix(compete::get_wl_matrix(chdf), method = "ds"))
compete::isi13(compete::get_wl_matrix(chdf))


### Do heatmap of which chases are most common?
# Which sequences of movements are most common?
table(alldf1$tubetrans)


### What are the last days each animal appears?
head(data)
data %>% 
  group_by(mouse) %>% 
  filter(day == max(day)) %>% 
  filter(ms == max(ms)) %>% 
  select(mouse,date,time,ms,day)


### create glicko graphs based on chases?



### Perhaps do a David's score based on each period (use last ms - but want to be more precise)

# just remove animal 9 
# 654597017  remove 1
# 910640264  remove 11
# 1000630788  remove 7
# 1346319559  remove 6
# 1601133758  remove 2
# all the rest until end

data1 <- data %>% filter(mouse!=9) # just remove animal 9 

data1.1 <- data1 %>% filter(ms <= 654597017) # 654597017  remove 1
data1.2 <- data1 %>% filter(ms > 654597017 & ms <= 910640264) # 910640264  remove 11
data1.3 <- data1 %>% filter(ms > 910640264 & ms <= 1000630788) #  1000630788  remove 7
data1.4 <- data1 %>% filter(ms > 1000630788 & ms <= 1346319559) #  1346319559  remove 6
data1.5 <- data1 %>% filter(ms > 1346319559 & ms <= 1601133758) #  1601133758  remove 2
data1.6 <- data1 %>% filter(ms > 1601133758 & ms <= 1692034607) #  1692034607 remove 4
data1.7 <- data1 %>% filter(ms > 1692034607) #  all the rest until end


# 654597017  remove 1
l1.1 <- split(data1.1, data1.1$mouse)
l1.1 <- lapply(l1.1, make_df)
l1.1 <- lapply(l1.1, add_type)
unlist(lapply(l1.1, function(x) sum(is.na(x$type))))

df1.1 <- map_dfr(tubetrans, ~ get_pairs_df(l1.1, tt = .x, win = 500), .id = "tubetrans")
df1.1$tubetrans <- tubetrans[as.numeric(df1.1$tubetrans)]
alldf1.1 <- df1.1 %>% arrange(value1) %>% mutate(dif = value1 - lag(value1))
alldf1.1
chdf1.1 <- data.frame(winner=alldf1.1[,5],loser = alldf1.1[,4])
compete::org_matrix(compete::get_wl_matrix(chdf1.1), method = "ds")
compete::get_di_matrix(compete::org_matrix(compete::get_wl_matrix(chdf1.1), method = "ds"))
compete::devries(compete::get_wl_matrix(chdf1.1))
compete::ds(compete::get_wl_matrix(chdf1.1))
compete::isi13(compete::get_wl_matrix(chdf1.1))


# 910640264  remove 11
l1.2 <- split(data1.2, data1.2$mouse)
l1.2 <- lapply(l1.2, make_df)
l1.2 <- lapply(l1.2, add_type)
unlist(lapply(l1.2, function(x) sum(is.na(x$type)))) #maybe an issue???

df1.2 <- map_dfr(tubetrans, ~ get_pairs_df(l1.2, tt = .x, win = 500), .id = "tubetrans")
df1.2$tubetrans <- tubetrans[as.numeric(df1.2$tubetrans)]
alldf1.2 <- df1.2 %>% arrange(value1) %>% mutate(dif = value1 - lag(value1))
alldf1.2
chdf1.2 <- data.frame(winner=alldf1.2[,5],loser = alldf1.2[,4])
compete::org_matrix(compete::get_wl_matrix(chdf1.2), method = "ds")
compete::get_di_matrix(compete::org_matrix(compete::get_wl_matrix(chdf1.2), method = "ds"))
compete::devries(compete::get_wl_matrix(chdf1.2))
compete::ds(compete::get_wl_matrix(chdf1.2))
compete::isi13(compete::get_wl_matrix(chdf1.2))



# #  1000630788  remove 7
l1.3 <- split(data1.3, data1.3$mouse)
l1.3 <- lapply(l1.3, make_df)
l1.3 <- lapply(l1.3, add_type)
unlist(lapply(l1.3, function(x) sum(is.na(x$type)))) #maybe an issue???

df1.3 <- map_dfr(tubetrans, ~ get_pairs_df(l1.3, tt = .x, win = 500), .id = "tubetrans")
df1.3$tubetrans <- tubetrans[as.numeric(df1.3$tubetrans)]
alldf1.3 <- df1.3 %>% arrange(value1) %>% mutate(dif = value1 - lag(value1))
alldf1.3
chdf1.3 <- data.frame(winner=alldf1.3[,5],loser = alldf1.3[,4])
compete::org_matrix(compete::get_wl_matrix(chdf1.3), method = "ds")
compete::get_di_matrix(compete::org_matrix(compete::get_wl_matrix(chdf1.3), method = "ds"))
compete::devries(compete::get_wl_matrix(chdf1.3))
compete::ds(compete::get_wl_matrix(chdf1.3))
compete::isi13(compete::get_wl_matrix(chdf1.3))


# #  1346319559  remove 6
l1.4 <- split(data1.4, data1.4$mouse)
l1.4 <- lapply(l1.4, make_df)
l1.4 <- lapply(l1.4, add_type)
unlist(lapply(l1.4, function(x) sum(is.na(x$type)))) #maybe an issue???

df1.4 <- map_dfr(tubetrans, ~ get_pairs_df(l1.4, tt = .x, win = 500), .id = "tubetrans")
df1.4$tubetrans <- tubetrans[as.numeric(df1.4$tubetrans)]
alldf1.4 <- df1.4 %>% arrange(value1) %>% mutate(dif = value1 - lag(value1))
alldf1.4
chdf1.4 <- data.frame(winner=alldf1.4[,5],loser = alldf1.4[,4])
compete::org_matrix(compete::get_wl_matrix(chdf1.4), method = "ds")
compete::get_di_matrix(compete::org_matrix(compete::get_wl_matrix(chdf1.4), method = "ds"))
compete::devries(compete::get_wl_matrix(chdf1.4))
compete::ds(compete::get_wl_matrix(chdf1.4))
compete::isi13(compete::get_wl_matrix(chdf1.4))


# #  1601133758  remove 2
l1.5 <- split(data1.5, data1.5$mouse)
l1.5 <- lapply(l1.5, make_df)
l1.5 <- lapply(l1.5, add_type)
unlist(lapply(l1.5, function(x) sum(is.na(x$type)))) #maybe an issue???

df1.5 <- map_dfr(tubetrans, ~ get_pairs_df(l1.5, tt = .x, win = 500), .id = "tubetrans")
df1.5$tubetrans <- tubetrans[as.numeric(df1.5$tubetrans)]
alldf1.5 <- df1.5 %>% arrange(value1) %>% mutate(dif = value1 - lag(value1))
alldf1.5
chdf1.5 <- data.frame(winner=alldf1.5[,5],loser = alldf1.5[,4])
compete::org_matrix(compete::get_wl_matrix(chdf1.5), method = "ds")
compete::get_di_matrix(compete::org_matrix(compete::get_wl_matrix(chdf1.5), method = "ds"))
compete::devries(compete::get_wl_matrix(chdf1.5))
compete::ds(compete::get_wl_matrix(chdf1.5))
compete::isi13(compete::get_wl_matrix(chdf1.5))



# #  1692034607 remove 4
l1.6 <- split(data1.6, data1.6$mouse)
l1.6 <- lapply(l1.6, make_df)
l1.6 <- lapply(l1.6, add_type)
unlist(lapply(l1.6, function(x) sum(is.na(x$type)))) #maybe an issue???

df1.6 <- map_dfr(tubetrans, ~ get_pairs_df(l1.6, tt = .x, win = 500), .id = "tubetrans")
df1.6$tubetrans <- tubetrans[as.numeric(df1.6$tubetrans)]
alldf1.6 <- df1.6 %>% arrange(value1) %>% mutate(dif = value1 - lag(value1))
alldf1.6
chdf1.6 <- data.frame(winner=alldf1.6[,5],loser = alldf1.6[,4])
compete::org_matrix(compete::get_wl_matrix(chdf1.6), method = "ds")
compete::get_di_matrix(compete::org_matrix(compete::get_wl_matrix(chdf1.6), method = "ds"))
compete::devries(compete::get_wl_matrix(chdf1.6))
compete::ds(compete::get_wl_matrix(chdf1.6))
compete::isi13(compete::get_wl_matrix(chdf1.6))


# #  #  all the rest until end
l1.7 <- split(data1.7, data1.7$mouse)
l1.7 <- lapply(l1.7, make_df)
l1.7 <- lapply(l1.7, add_type)
unlist(lapply(l1.7, function(x) sum(is.na(x$type)))) #maybe an issue???

df1.7 <- map_dfr(tubetrans, ~ get_pairs_df(l1.7, tt = .x, win = 500), .id = "tubetrans")
df1.7$tubetrans <- tubetrans[as.numeric(df1.7$tubetrans)]
alldf1.7 <- df1.7 %>% arrange(value1) %>% mutate(dif = value1 - lag(value1))
alldf1.7
chdf1.7 <- data.frame(winner=alldf1.7[,5],loser = alldf1.7[,4])
compete::org_matrix(compete::get_wl_matrix(chdf1.7), method = "ds")
compete::get_di_matrix(compete::org_matrix(compete::get_wl_matrix(chdf1.7), method = "ds"))
compete::devries(compete::get_wl_matrix(chdf1.7))
compete::ds(compete::get_wl_matrix(chdf1.7))
compete::isi13(compete::get_wl_matrix(chdf1.7))




# #  #  all ignoring removals
l1all <- split(data1, data1$mouse)
l1all <- lapply(l1all, make_df)
l1all <- lapply(l1all, add_type)
unlist(lapply(l1all, function(x) sum(is.na(x$type)))) #maybe an issue???

df1all <- map_dfr(tubetrans, ~ get_pairs_df(l1all, tt = .x, win = 500), .id = "tubetrans")
df1all$tubetrans <- tubetrans[as.numeric(df1all$tubetrans)]
alldf1all <- df1all %>% arrange(value1) %>% mutate(dif = value1 - lag(value1))
alldf1all
chdf1all <- data.frame(winner=alldf1all[,5],loser = alldf1all[,4])
compete::org_matrix(compete::get_wl_matrix(chdf1all), method = "ds")
compete::get_di_matrix(compete::org_matrix(compete::get_wl_matrix(chdf1all), method = "ds"))
compete::devries(compete::get_wl_matrix(chdf1all))
compete::ds(compete::get_wl_matrix(chdf1all))
compete::isi13(compete::get_wl_matrix(chdf1all))

isi <- compete::isi13(compete::get_wl_matrix(chdf1all))
mat <- compete::org_matrix(compete::get_wl_matrix(chdf1all), method = 'ds')
levs <- isi$best_order
mat <- mat[levs,levs]
sum(mat)
matdf <- as.data.frame(mat)
mat1 <- mat/(mat + t(mat))
dfmat <- as.data.frame(mat1)
dfmat$loser <- factor(dfmat$loser, levels = rownames(mat))
dfmat$winner <- factor(dfmat$winner, levels = rev(rownames(mat)))

matdi <- compete::get_di_matrix(compete::org_matrix(compete::get_wl_matrix(chdf1all), method = "ds"))
matdi <- matdi[levs,levs]
dfmatdi <- reshape2::melt(matdi)
dfmat <- cbind(dfmat, val=dfmatdi[,3])
dfmat <- cbind(dfmat, tot = matdf[,3])
dfmat
dfmat$Freq <- ifelse(dfmat$val==0, NA, dfmat$Freq)
dfmat$tot <- ifelse(dfmat$val==0, NA, dfmat$tot)

# Plot heatmap using ggplot2
formatted_x <- ifelse(is.na(dfmat$Freq), NA, sprintf("%.2f", dfmat$Freq))

ggplot(dfmat, aes(x = loser, y = winner, fill = Freq)) +
  geom_tile(color='black') +
  geom_text(label=formatted_x, color = "black", size = 3, na.rm = TRUE) +
  scale_fill_gradient(low = "white", high = "red", na.value = "white") +
  coord_equal() +
  theme_classic() + 
  theme(axis.line = element_blank()) +
  scale_x_discrete(position = "top") 
