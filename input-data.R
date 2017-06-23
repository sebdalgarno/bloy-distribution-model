source('header.R')

# predictors by point
segs <- st_read('inputs/varsall_0909_2.shp')

# occurence
occ <- read_csv('inputs/occurence.csv')

# maps
hgline <- st_read(dsn = 'inputs/HG_SHZN_KNG_segs.shp')
hgpoly <- st_read(dsn = 'inputs/SHZN_HG_poly.shp') 
suppressWarnings(st_crs(hgpoly) <- bcalbers)
ghbound <- st_read(dsn = 'inputs/GH_boundary_north.shp')
suppressWarnings(st_crs(ghbound) <- bcalbers)
canada <- st_read('inputs/Canada.shp')
st_crs(canada) <- canalb

set_sub("input")

save_datas()

