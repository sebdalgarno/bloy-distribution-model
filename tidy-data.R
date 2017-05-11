source('header.R')

set_sub("clean")

load_datas()

set_sub("tidy")

# summarise points by segments into linestring
pts %<>% group_by(LineID) %>% summarise(SegLength = first(SegLength),
                                        Fetch = mean(Fetch),
                                        TreeDist = mean(TreeDist),
                                        HumanDist = mean(HumanDist),
                                        ITArea50 = mean(ITArea50),
                                        ITArea200 = mean(ITArea200),
                                        ITArea500 = mean(ITArea500),
                                        ITArea1000 = mean(ITArea1000),
                                        Zone = first(Zone),
                                        Fold = first(Fold)) %>% 
  st_cast("MULTIPOINT") %>%
  st_cast("LINESTRING")

# join to segs
dat <- left_join(pts, segs, by = "LineID")

dat %<>% mutate(Fetch = log10(Fetch + 0.001),
                TreeDist = log10(TreeDist + 0.001),
                IslandArea = log10(IslandArea))

dat %<>% select(LineID:ITArea1000, BioExp:Active, Zone)

# misclassified (field coords of pairs too far from shore)
mis <- c(8885, 8245, 8060, 7445, 5886, 5406, 5650, 3688, 4024, 4087, 12092, 12104)
dat %<>% filter(!(LineID %in% mis))

rm(pts, segs)

save_datas()
