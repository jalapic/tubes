### Correlating follow and behavior matrices

tubefollows <- readRDS("data/tubefollows.RData")

alldf11all<-data.frame(tubefollows[1:6])
alldf12all<-data.frame(tubefollows[7:12])
alldf13all<-data.frame(tubefollows[13:18])
alldf14all<-data.frame(tubefollows[19:24])

## Load matrices
mats <- readRDS("data/matrices1.RDS")
m11 <- mats[[1]]
m12 <- mats[[2]]
m13 <- mats[[3]]
m14 <- mats[[4]]

### Hierarchy Metrics

# Cohort 11
chdf11all <- data.frame(winner=alldf11all[,5],loser = alldf11all[,4])
isi.11 <- compete::isi13(compete::get_wl_matrix(chdf11all))
mat11 <- compete::org_matrix(compete::get_wl_matrix(chdf11all), method = 'ds')
levs11 <- isi.11$best_order
mat11 <- mat11[levs11,levs11]
m11 <- m11[levs11,levs11]
vegan::mantel(mat11,m11)

# Cohort 12
chdf12all <- data.frame(winner=alldf12all[,5],loser = alldf12all[,4])
isi.12 <- compete::isi13(compete::get_wl_matrix(chdf12all))
mat12 <- compete::org_matrix(compete::get_wl_matrix(chdf12all), method = 'ds')
levs12 <- isi.12$best_order
mat12 <- mat12[levs12,levs12]
m12 <- m12[levs12,levs12]
vegan::mantel(mat12,m12)

# Cohort 13
chdf13all <- data.frame(winner=alldf13all[,5],loser = alldf13all[,4])
isi.13 <- compete::isi13(compete::get_wl_matrix(chdf13all))
mat13 <- compete::org_matrix(compete::get_wl_matrix(chdf13all), method = 'ds')
levs13 <- isi.13$best_order
mat13 <- mat13[levs13,levs13]
m13 <- m13[levs13,levs13]
vegan::mantel(mat13,m13)




# Cohort 14
chdf14all <- data.frame(winner=alldf14all[,5],loser = alldf14all[,4])
isi.14 <- compete::isi13(compete::get_wl_matrix(chdf14all))
mat14 <- compete::org_matrix(compete::get_wl_matrix(chdf14all), method = 'ds')
levs14 <- isi.14$best_order
mat14 <- mat14[levs14,levs14]
m14 <- m14[levs14,levs14]
vegan::mantel(mat14,m14)


## Correlate Ranks 
cor.test(1:10,match(rownames(mat11),rownames(mats[[1]])),method='s')
cor.test(1:10,match(rownames(mat12),rownames(mats[[2]])),method='s')
cor.test(1:10,match(rownames(mat13),rownames(mats[[3]])),method='s')
cor.test(1:10,match(rownames(mat14),rownames(mats[[4]])),method='s')

## DS Correlation

dsdf11 <- data.frame(
  follow = compete::ds(mat11,norm = T),
  fight = compete::ds(m11, norm=T),
  cohort = "11")

dsdf12 <- data.frame(
  follow = compete::ds(mat12,norm = T),
  fight = compete::ds(m12, norm=T),
  cohort = "12")

dsdf13 <- data.frame(
  follow = compete::ds(mat13,norm = T),
  fight = compete::ds(m13, norm=T),
  cohort = "13")

dsdf14 <- data.frame(
follow = compete::ds(mat14,norm = T),
fight = compete::ds(m14, norm=T),
cohort = "14")

cor.test(dsdf11$follow, dsdf11$fight)
cor.test(dsdf12$follow, dsdf12$fight)
cor.test(dsdf13$follow, dsdf13$fight)
cor.test(dsdf14$follow, dsdf14$fight)


dsdf <- rbind(dsdf11,dsdf12,dsdf13,dsdf14)
dsdf

ggplot(dsdf, aes(x=follow,y=fight,color=cohort,group=cohort))+
  geom_point(size=3) +
  theme_minimal()+
  xlab("David's Score - Follow") +
  ylab("David's Score - Agonistic") +
  stat_smooth(method='lm',se=F) +
  scale_color_manual(values=c("firebrick","chocolate","red","orange"))
