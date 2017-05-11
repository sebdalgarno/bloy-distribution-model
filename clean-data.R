source('header.R')

set_sub("input")

load_datas()

ggplot(data = pts1000m, aes(x = RASTERVALU, y = pointID)) + geom_point()
# combine preds from pts
pts %<>% dplyr::select(X, Y, PointID = sehgID, LineID = predID, SegLength = seglength, 
                       Fetch = hgten_su_2, TreeDist = treedist, HumanDist = HumDist)

# join function (remove negative values)
join_pts <- function(data, data2, radius){
  data2 %<>% mutate(RASTERVALU = ifelse(RASTERVALU < 0, NA, RASTERVALU))
  data2 %<>% dplyr::select(PointID = sehgID, RASTERVALU)
  names(data2) <- c("PointID", paste0("ITArea", radius))
  data %<>% left_join(data2, by = "PointID")
}

pts %<>% join_pts(data2 = pts50m, radius = "50") %>%
  join_pts(pts200m, radius = "200") %>%
  join_pts(pts500m, radius = "500") %>%
  join_pts(pts1000m, radius = "1000")

rm(pts50m, pts200m, pts500m, pts1000m)

# find northernmost 75% of segments
top <- arrange(pts, -Y) %>% slice(1:(.75*n())) 
index <- top$PointID
n = nrow(pts)
k <- 5
pts %<>% arrange(-Y) %>% 
  mutate(row = 1:n()) %>%
  mutate(Fold = ifelse(row < n/k, 1, 
                       ifelse(row > n/k-1 & row < n/k*2, 2, 
                              ifelse(row > n/k*2-1 & row < n/k*3, 3,
                                     ifelse(row > n/k*3-1 & row < n/k*4, 4,
                                            ifelse(row > n/k*4-1, 5, NA))))))

pts %<>% mutate(Zone = ifelse(pts$PointID %in% index, "North", "South"))
  
# transform sf objects
pts %<>% st_as_sf(coords = c("X", "Y"), crs = bcalbers)
aoi %<>% st_transform(crs = bcalbers)
hg %<>% st_transform(crs = bcalbers)
occ %<>% st_as_sf(coords = c("FieldX", "FieldY"), crs = wgs84)
occ %<>% st_transform(crs = bcalbers)
  
# segs
segs %<>% dplyr::select(LineID = predID, Class = COASTAL_CL, BioExp = EXP_FINAL, 
                        Fucus = FUC, Mussel = MUS, IslandArea = area, RatStatus = Rats2010NA,
                        Survey2010 = F2010srv, Survey2005 = F2005_any, Occur2010 = F2010oc,
                        Active2010 = F2010ac, Occur2005 = F2005_oc, Active2005 = F2005_ac)
  
segs %<>% mutate(BioExp = ordered(BioExp, levels = c("VP", "P", "SP", "SE", "E")))

# create shoretype predictor
segs %<>% mutate(ShoreType = ifelse(Class < 3, "Wide rock", 
                                    ifelse(Class == 0, NA,
                                           ifelse(Class > 2 & Class < 6, "Narrow rock",
                                                  ifelse(Class > 5 & Class < 8, "Wide rock and gravel beach",
                                                         ifelse(Class > 7 & Class < 11, "Narrow rock and gravel beach",
                                                                ifelse(Class > 10 & Class < 13, "Wide rock and sand/gravel beach",
                                                                       ifelse(Class > 12 & Class < 16, "Narrow rock and sand/gravel beach",
                                                                              ifelse(Class > 15 & Class < 18, "Wide rock and sand/gravel beach",
                                                                                     ifelse(Class > 17 & Class < 21, "Narrow rock and sand/gravel beach",
                                                                                            ifelse(Class > 20 & Class < 24, "Gravel beach",
                                                                                                   ifelse(Class > 23 & Class < 27, "Sand and Gravel beach",
                                                                                                          ifelse(Class > 26 & Class < 29, "Sand beach",
                                                                                                                 ifelse(Class == 29, "Estuary/lagoon",
                                                                                                                        ifelse(Class == 30, "Sand beach", NA)))))))))))))))
# create slope predictor
low <- c(2, 5, 7, 10, 12, 15, 17, 20, 23, 24, 26, 28, 29, 31, 35)
mid <- c(1, 4, 6, 9, 11, 14, 16, 19, 22, 25, 27, 30)
high <- c(3, 8, 13, 18, 21)
none <- c(0, 32, 33)

segs %<>% mutate(Slope = ifelse(Class %in% none, NA,
                                ifelse(Class %in% low, "<5",
                                       ifelse(Class %in% mid, "5-20",
                                              ifelse(Class %in% high, ">20", NA)))))

segs %<>% mutate(RatStatus = ifelse(RatStatus == 1, "Present", "Absent"))

segs %<>% select(-Class)

segs %<>% mutate_if(is.character, factor)

# set NA
segs[segs == 999] <- NA
segs$RatStatus[segs$RatStatus == 9] <- NA

# eliminate overlap in surveys
segs %<>% mutate(Survey2005 = ifelse(Survey2005 > 0 & Survey2010 > 0, 0, Survey2005))

segs %<>% mutate(Occur2005 = ifelse(Survey2005 == 0, NA, Occur2005),
                 Active2005 = ifelse(Survey2005 == 0, NA, Active2005))
segs %<>% mutate(Survey2010 = ifelse(Survey2010 == 2, 1, Survey2010))



# combine 2005 and 2010 surveys
segs %<>% mutate(SurveyYear = ifelse(Survey2010 == 1, 2010,
                                     ifelse(Survey2005 == 1, 2005, NA)))

# for segments with presence but not surveyed, modify so that surveyed (3 pairs, likely due to 'nearest' operation)
segs %<>% mutate(Survey2005 = ifelse(Occur2005 > 0 & is.na(Survey2005), 1, Survey2005))
segs %<>% mutate(Survey2010 = ifelse(Occur2010 > 0 & is.na(Survey2010), 1, Survey2010))


# combine 2005 and 2010 surveys
segs %<>% mutate(SurveyYear = ifelse(Survey2010 > 0, 2010,
                                     ifelse(Survey2005 == 1, 2005, NA)))

segs %<>% mutate(Occur = ifelse(is.na(SurveyYear), NA, 
                                ifelse(!is.na(Occur2010), Occur2010,
                                       ifelse(!is.na(Occur2005), Occur2005, NA))))

segs %<>% mutate(Active = ifelse(is.na(SurveyYear), NA, 
                                 ifelse(!is.na(Active2010), Active2010,
                                        ifelse(!is.na(Active2005), Active2005, NA))))

segs %<>% select(-Survey2010, - Survey2005, -Occur2010, 
                 -Occur2005, - Active2010, - Active2005)

set_sub("clean")

save_datas()

# ptsbuff <- pts %<>% st_buffer(50)
# 
# system.time(ptsinter <- st_intersection(ptsbuff, hgpoly))
