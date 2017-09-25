source('header.R')

set_sub("clean")

load_datas()

segs %<>% mutate(Occur2010 = replace(Occur2010, Occur2010 == 999, NA),
                 Active2010 = replace(Active2010, Active2010 == 999, NA))

segs %<>% mutate(SegLength = st_length(.))

segs %<>% mutate(BioExp = ordered(BioExp, levels = c("VP", "P", "SP", "SE", "E")))

# get log10 of area
segs %<>% mutate(IslandArea = log10(IslandArea))

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

segs %<>% mutate(RatStatus = ifelse(RatStatus == 1, "Present", 
                                    ifelse(RatStatus == 0, "Absent", NA)))

segs %<>% dplyr::select(-Class)

segs %<>% mutate_if(is.character, factor)

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

segs %<>% select(LineID:RatStatus, SegLength:Slope, Zone, SurveyYear:geometry)


set_sub("tidy")

save_datas()
