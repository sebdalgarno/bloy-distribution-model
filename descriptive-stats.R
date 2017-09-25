source('header.R')

set_sub("tidy")

segs <- load_data("segs") %>%
  select(IT50, HumanDist, SurveyYear)

segslog <- load_data("segs") %>%
  select(IslandArea, TreeDist, Fetch, SurveyYear)

s05 <- filter(segs, SurveyYear == 2005)
s10 <- filter(segs, SurveyYear == 2010)
sall <- rbind(s05, s10)
sarea <- filter(segs, is.na(SurveyYear))

s05.l <- filter(segslog, SurveyYear == 2005)
s10.l <- filter(segslog, SurveyYear == 2010)
sall.l <- rbind(s05.l, s10.l)
sarea.l <- segslog

descriptive(s05)
descriptive(s10)
descriptive(sall)
descriptive(sarea)

descriptive_log(s05.l)
descriptive_log(s10.l)
descriptive_log(sall.l)
descriptive_log(sarea.l)
