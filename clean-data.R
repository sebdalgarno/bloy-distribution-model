source('header.R')

set_sub("input")

load_datas()

# transform sf objects
occ %<>% st_as_sf(coords = c("FieldX", "FieldY"), crs = wgs84)
occ %<>% st_transform(crs = bcalbers)

suppressWarnings(st_crs(segs) <- bcalbers)
suppressWarnings(st_crs(hgline) <- bcalbers)

segs %<>% dplyr::select(LineID = predID, 
                        Class = COASTAL_CL, 
                        BioExp = EXP_FINAL, 
                        Fucus = FUC, 
                        Mussel = MUS, 
                        IslandArea = area, 
                        Fetch = log.sum10, 
                        TreeDist = log.treedi,
                        IT50 = it50, 
                        IT200 = it200, 
                        IT500 = it500, 
                        IT1000 = it1000,
                        HumanDist = humdist, 
                        RatStatus = rats2010_1,
                        Survey2010 = F2010srv, 
                        Survey2005 = F2005_any, 
                        Occur2010 = F2010oc,
                        Active2010 = F2010ac, 
                        Occur2005 = F2005_oc, 
                        Active2005 = F2005_ac,
                        Zone = zone)

segs %<>% mutate(Fetch = as.numeric(Fetch),
                 TreeDist = as.numeric(TreeDist),
                 IT50 = as.numeric(IT50),
                 IT200 = as.numeric(IT200),
                 IT500 = as.numeric(IT500),
                 IT1000 = as.numeric(IT1000),
                 HumanDist = as.numeric(HumanDist))

set_sub("clean")

save_datas()


