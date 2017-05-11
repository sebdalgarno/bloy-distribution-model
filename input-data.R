source('header.R')

# predictors by point
pts <- read_csv('inputs/SEHG_10mpnt_humdist.csv') 

pts50m <- read_csv('inputs/chaobu_extrct_50.csv', )
pts200m <- read_csv('inputs/chaobu_extrct_200.csv')
pts500m <- read_csv('inputs/chaobu_extrct_500.csv')
pts1000m <- read_csv('inputs/chaobu_extrct_1000.csv')

# get wave exposure estimates

# predictors by line segment
segs <- read_csv('inputs/SEHG_segs_predocc_0804.csv')

# occurence
occ <- read_csv('inputs/occurence.csv')

# spatial
hg <- st_read(dsn = 'inputs/HG_SHZN_KNG_segs.shp')
aoi <- st_read(dsn = 'inputs/PRED_AOI_Hec_KNG_segs.shp')
hgpoly <- st_read(dsn = 'inputs/SHZN_HG_poly.shp') 
suppressWarnings(st_crs(hgpoly) <- bcalbers)
ghbound <- st_read(dsn = 'inputs/GH_boundary_north.shp')
suppressWarnings(st_crs(ghbound) <- bcalbers)
canada <- st_read('inputs/Canada.shp')
st_crs(canada) <- canalb

set_sub("input")

save_datas()

