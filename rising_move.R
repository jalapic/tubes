
#### Activity plots of animals following removals.

# # 910640264  remove 11
# what happens just after removal of 11.

## Just plot activity of all mice over time.
# any antennas.

l1all
d <- data.table::rbindlist(l1all)
d
table(d$type) # activity should be ta, tab, tb, tr, tt.  not start, eq, x.
# we need to check for each mouse whether we are not successfully tracking animals for periods of time

trns <- c("tt","ta","tb","tr","tab")

d <- d %>% filter(type %in% trns)
d <- d %>% group_by(mouse) %>%arrange(ms) %>% mutate(trns = row_number())

times <- c(654597017,910640264,1000630788,1346319559,1601133758,1692034607)


ggplot(d, aes(x=ms, y=trns, color=factor(mouse)), group=factor(mouse)) +
  geom_step() +
  geom_vline(xintercept = times, lty=2, color='gray77') +
  theme_bw()
 
