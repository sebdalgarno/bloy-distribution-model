### create function for finding mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}
#### Function for summary statistics
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}

# retrieve sf abject section from database BLOB
sf_retrieve = function(data, dbname = "output/dbs/quesnel-exploitation.sqlite3") {
  conn  <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname)
  retr <- DBI::dbGetQuery(conn, paste(c("select", data, "from", data), collapse = " "))
  sf <- lapply(retr[,data], 'unserialize')[[1]]
}

# convert data.frame with xy coords containing 'x_contains' and 'y_contains' to sf object
sf_convert = function(data, epsg = 26910, x_contains = 'Easting', y_contains = 'Northing') {
  coords <- dplyr::select(data, contains(x_contains), contains(y_contains))
  if(length(names(coords)) > 2)
    error("More than two columns selected as coordinates.")
  data %<>% st_as_sf(coords = names(coords)) %>%
    st_set_crs(epsg)
}

# quick sp conversion from sf
sp <- function(data) {
  data %<>% as("Spatial")
}

get_nearest <- function(data1 = capture, data2 = section, crs = bcalbers) {
  
  suffix = c(".x", ".y")
  data1 %<>% mutate(ID = 1:n())
  
  # find those outside
  i <- data1[data2,]
  o <- filter(data1, !(ID %in% i$ID))
  i %<>% data.frame(st_coordinates(.)) %>% dplyr::select(-dplyr::contains("geom"))
  o %<>% data.frame(st_coordinates(.)) %>% dplyr::select(-dplyr::contains("geom"))
  y <- data.frame(st_coordinates(data2)) %>% dplyr::select(X, Y)
  
  # get coords of nearest vertex
  o %<>% merge(y, by = NULL, suffixes = suffix) %>%
    dplyr::mutate_(Distance = ~sqrt((X.x - X.y)^2 + (Y.x - Y.y)^2)) %>%
    plyr::ddply("ID", function(x) dplyr::slice_(x, ~which.min(Distance))) %>%
    dplyr::rename_(X = ~X.y, Y = ~Y.y) %>%
    dplyr::select_(~-X.x, ~-Y.x, ~-Distance)
  
  # bind in/out
  near <- rbind(o, i)
  near %<>% st_as_sf(coords = c("X", "Y"), crs = crs) %>%
    dplyr::select(-ID)
}

# conveneint scale bar
get_scale <- function(dist = 5, height = 0.008, st.size = 3.5) {
  
  set_sub("tidy")
  lake <- crop(load_data("lake"))
  scale <- st_coordinates(lake) %>% tbl_df()
  xmin = min(scale$X)
  xmax = max(scale$X)
  ymin = min(scale$Y)
  ymax = max(scale$Y)
  
  ggsn::scalebar(data = NULL, location = "bottomright", dist = dist, height = height, st.size = st.size,
                 x.min = xmin, x.max = xmax, y.min = ymin, y.max = ymax)
}


