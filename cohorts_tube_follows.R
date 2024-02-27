# #  #  all cohorts - full matrix


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

l11all <- split(data11, data11$mouse)
l11all <- lapply(l11all, make_df)
l11all <- lapply(l11all, add_type)
unlist(lapply(l11all, function(x) sum(is.na(x$type)))) #

df11all <- map_dfr(tubetrans, ~ get_pairs_df(l11all, tt = .x, win = 500), .id = "tubetrans")

df11all$tubetrans <- tubetrans[as.numeric(df11all$tubetrans)]
alldf11all <- df11all %>% arrange(value1) %>% mutate(dif = value1 - lag(value1))
alldf11all


## Cohort 13

l13all <- split(data13, data13$mouse)
l13all <- lapply(l13all, make_df)
l13all <- lapply(l13all, add_type)
unlist(lapply(l13all, function(x) sum(is.na(x$type)))) #

df13all <- map_dfr(tubetrans, ~ get_pairs_df(l13all, tt = .x, win = 500), .id = "tubetrans")

df13all$tubetrans <- tubetrans[as.numeric(df13all$tubetrans)]
alldf13all <- df13all %>% arrange(value1) %>% mutate(dif = value1 - lag(value1))
alldf13all

## Cohort 12

l12all <- split(data12, data12$mouse)
l12all <- lapply(l12all, make_df)
l12all <- lapply(l12all, add_type)
unlist(lapply(l12all, function(x) sum(is.na(x$type)))) #

df12all <- map_dfr(tubetrans2, ~ get_pairs_df(l12all, tt = .x, win = 500), .id = "tubetrans")

df12all$tubetrans <- tubetrans2[as.numeric(df12all$tubetrans)]
alldf12all <- df12all %>% arrange(value1) %>% mutate(dif = value1 - lag(value1))
alldf12all

## Cohort 14

l14all <- split(data14, data14$mouse)
l14all <- lapply(l14all, make_df)
l14all <- lapply(l14all, add_type)
unlist(lapply(l14all, function(x) sum(is.na(x$type)))) #

df14all <- map_dfr(tubetrans2, ~ get_pairs_df(l14all, tt = .x, win = 500), .id = "tubetrans")

df14all$tubetrans <- tubetrans2[as.numeric(df14all$tubetrans)]
alldf14all <- df14all %>% arrange(value1) %>% mutate(dif = value1 - lag(value1))
alldf14all



## Save Data
tubefollows <- c(alldf11all, alldf12all, alldf13all, alldf14all)
saveRDS(tubefollows, "data/tubefollows.RData")
tubefollows <- readRDS("data/tubefollows.RData")

alldf11all<-data.frame(tubefollows[1:6])
alldf12all<-data.frame(tubefollows[7:12])
alldf13all<-data.frame(tubefollows[13:18])
alldf14all<-data.frame(tubefollows[19:24])

### Hierarchy Metrics

# Cohort 11
chdf11all <- data.frame(winner=alldf11all[,5],loser = alldf11all[,4])
compete::org_matrix(compete::get_wl_matrix(chdf11all), method = "ds")
compete::get_di_matrix(compete::org_matrix(compete::get_wl_matrix(chdf11all), method = "ds"))
compete::devries(compete::get_wl_matrix(chdf11all))
compete::ds(compete::get_wl_matrix(chdf11all))
compete::isi13(compete::get_wl_matrix(chdf11all))

isi <- compete::isi13(compete::get_wl_matrix(chdf11all))
mat <- compete::org_matrix(compete::get_wl_matrix(chdf11all), method = 'ds')
levs <- isi$best_order
mat <- mat[levs,levs]
sum(mat)
matdf <- as.data.frame(mat)
mat1 <- mat/(mat + t(mat))
dfmat <- as.data.frame(mat1)
dfmat$loser <- factor(dfmat$loser, levels = rownames(mat))
dfmat$winner <- factor(dfmat$winner, levels = rev(rownames(mat)))

matdi <- compete::get_di_matrix(compete::org_matrix(compete::get_wl_matrix(chdf11all), method = "ds"))
matdi <- matdi[levs,levs]
dfmatdi <- reshape2::melt(matdi)
dfmat <- cbind(dfmat, val=dfmatdi[,3])
dfmat <- cbind(dfmat, tot = matdf[,3])
dfmat
dfmat$Freq <- ifelse(dfmat$val==0, NA, dfmat$Freq)
dfmat$tot <- ifelse(dfmat$val==0, NA, dfmat$tot)

# Plot heatmap using ggplot2
formatted_x <- ifelse(is.na(dfmat$Freq), NA, sprintf("%.2f", dfmat$Freq))

p1<-ggplot(dfmat, aes(x = loser, y = winner, fill = Freq)) +
  geom_tile(color='black') +
  geom_text(label=formatted_x, color = "black", size = 3, na.rm = TRUE) +
  scale_fill_gradient(low = "white", high = "red", na.value = "white") +
  coord_equal() +
  theme_classic() + 
  theme(axis.line = element_blank()) +
  scale_x_discrete(position = "top") 



### Cohort 12
chdf12all <- data.frame(winner=alldf12all[,5],loser = alldf12all[,4])
compete::org_matrix(compete::get_wl_matrix(chdf12all), method = "ds")
compete::get_di_matrix(compete::org_matrix(compete::get_wl_matrix(chdf12all), method = "ds"))
compete::devries(compete::get_wl_matrix(chdf12all))
compete::ds(compete::get_wl_matrix(chdf12all))
compete::isi13(compete::get_wl_matrix(chdf12all))

isi <- compete::isi13(compete::get_wl_matrix(chdf12all))
mat <- compete::org_matrix(compete::get_wl_matrix(chdf12all), method = 'ds')
levs <- isi$best_order
mat <- mat[levs,levs]
sum(mat)
matdf <- as.data.frame(mat)
mat1 <- mat/(mat + t(mat))
dfmat <- as.data.frame(mat1)
dfmat$loser <- factor(dfmat$loser, levels = rownames(mat))
dfmat$winner <- factor(dfmat$winner, levels = rev(rownames(mat)))

matdi <- compete::get_di_matrix(compete::org_matrix(compete::get_wl_matrix(chdf12all), method = "ds"))
matdi <- matdi[levs,levs]
dfmatdi <- reshape2::melt(matdi)
dfmat <- cbind(dfmat, val=dfmatdi[,3])
dfmat <- cbind(dfmat, tot = matdf[,3])
dfmat
dfmat$Freq <- ifelse(dfmat$val==0, NA, dfmat$Freq)
dfmat$tot <- ifelse(dfmat$val==0, NA, dfmat$tot)

# Plot heatmap using ggplot2
formatted_x <- ifelse(is.na(dfmat$Freq), NA, sprintf("%.2f", dfmat$Freq))

p2<-ggplot(dfmat, aes(x = loser, y = winner, fill = Freq)) +
  geom_tile(color='black') +
  geom_text(label=formatted_x, color = "black", size = 3, na.rm = TRUE) +
  scale_fill_gradient(low = "white", high = "red", na.value = "white") +
  coord_equal() +
  theme_classic() + 
  theme(axis.line = element_blank()) +
  scale_x_discrete(position = "top") 





### Cohort 13
chdf13all <- data.frame(winner=alldf13all[,5],loser = alldf13all[,4])
compete::org_matrix(compete::get_wl_matrix(chdf13all), method = "ds")
compete::get_di_matrix(compete::org_matrix(compete::get_wl_matrix(chdf13all), method = "ds"))
compete::devries(compete::get_wl_matrix(chdf13all))
compete::ds(compete::get_wl_matrix(chdf13all))
compete::isi13(compete::get_wl_matrix(chdf13all))

isi <- compete::isi13(compete::get_wl_matrix(chdf13all))
mat <- compete::org_matrix(compete::get_wl_matrix(chdf13all), method = 'ds')
levs <- isi$best_order
mat <- mat[levs,levs]
sum(mat)
matdf <- as.data.frame(mat)
mat1 <- mat/(mat + t(mat))
dfmat <- as.data.frame(mat1)
dfmat$loser <- factor(dfmat$loser, levels = rownames(mat))
dfmat$winner <- factor(dfmat$winner, levels = rev(rownames(mat)))

matdi <- compete::get_di_matrix(compete::org_matrix(compete::get_wl_matrix(chdf13all), method = "ds"))
matdi <- matdi[levs,levs]
dfmatdi <- reshape2::melt(matdi)
dfmat <- cbind(dfmat, val=dfmatdi[,3])
dfmat <- cbind(dfmat, tot = matdf[,3])
dfmat
dfmat$Freq <- ifelse(dfmat$val==0, NA, dfmat$Freq)
dfmat$tot <- ifelse(dfmat$val==0, NA, dfmat$tot)

# Plot heatmap using ggplot2
formatted_x <- ifelse(is.na(dfmat$Freq), NA, sprintf("%.2f", dfmat$Freq))

p3<-ggplot(dfmat, aes(x = loser, y = winner, fill = Freq)) +
  geom_tile(color='black') +
  geom_text(label=formatted_x, color = "black", size = 3, na.rm = TRUE) +
  scale_fill_gradient(low = "white", high = "red", na.value = "white") +
  coord_equal() +
  theme_classic() + 
  theme(axis.line = element_blank()) +
  scale_x_discrete(position = "top") 






### Cohort 14
chdf14all <- data.frame(winner=alldf14all[,5],loser = alldf14all[,4])
compete::org_matrix(compete::get_wl_matrix(chdf14all), method = "ds")
compete::get_di_matrix(compete::org_matrix(compete::get_wl_matrix(chdf14all), method = "ds"))
compete::devries(compete::get_wl_matrix(chdf14all))
compete::ds(compete::get_wl_matrix(chdf14all))
compete::isi13(compete::get_wl_matrix(chdf14all))

isi <- compete::isi13(compete::get_wl_matrix(chdf14all))
mat <- compete::org_matrix(compete::get_wl_matrix(chdf14all), method = 'ds')
levs <- isi$best_order
mat <- mat[levs,levs]
sum(mat)
matdf <- as.data.frame(mat)
mat1 <- mat/(mat + t(mat))
dfmat <- as.data.frame(mat1)
dfmat$loser <- factor(dfmat$loser, levels = rownames(mat))
dfmat$winner <- factor(dfmat$winner, levels = rev(rownames(mat)))

matdi <- compete::get_di_matrix(compete::org_matrix(compete::get_wl_matrix(chdf14all), method = "ds"))
matdi <- matdi[levs,levs]
dfmatdi <- reshape2::melt(matdi)
dfmat <- cbind(dfmat, val=dfmatdi[,3])
dfmat <- cbind(dfmat, tot = matdf[,3])
dfmat
dfmat$Freq <- ifelse(dfmat$val==0, NA, dfmat$Freq)
dfmat$tot <- ifelse(dfmat$val==0, NA, dfmat$tot)

# Plot heatmap using ggplot2
formatted_x <- ifelse(is.na(dfmat$Freq), NA, sprintf("%.2f", dfmat$Freq))

p4<-ggplot(dfmat, aes(x = loser, y = winner, fill = Freq)) +
  geom_tile(color='black') +
  geom_text(label=formatted_x, color = "black", size = 3, na.rm = TRUE) +
  scale_fill_gradient(low = "white", high = "red", na.value = "white") +
  coord_equal() +
  theme_classic() + 
  theme(axis.line = element_blank()) +
  scale_x_discrete(position = "top") 

library(gridExtra)
grid.arrange(p1 +ggtitle("Cohort 11"),
             p2 +ggtitle("Cohort 12"),
             p3 +ggtitle("Cohort 13"),
             p4 +ggtitle("Cohort 14"),
             nrow=1)