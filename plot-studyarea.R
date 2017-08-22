source('header.R')

set_sub("tidy")

segs <- load_data("segs")

segs %<>% mutate(SurveyYear = ifelse(is.na(SurveyYear), "Predicted", 
                                     ifelse(SurveyYear == 2005, "Surveyed in 2005", "Surveyed in 2010")))

set_sub("input")

hg <- load_data("hgpoly")
can <- load_data("canada")

set_sub("maps")

segsalb <- st_transform(segs, crs = canalb)

lims <- st_bbox(segs)
limsalb <- st_bbox(segsalb)

segs2 <- segs
segs2 %<>% st_cast("POINT") 
segs2 %<>% data.frame(st_coordinates(.))

ymax <- 2210000
ymin <- 1400000
xmax <- -1900000
xmin <- -2400000

study <- ggplot(segs) + 
  ggplot2::geom_sf(data = hg, fill = "grey90", color = "black", lwd = 0.01) +
  ggplot2::coord_sf(xlim = c(lims[1], lims[3]), ylim = c(lims[2], lims[4])) +
  ggplot2::geom_line(data = segs2, aes(x = X, y = Y, group = LineID, color = SurveyYear), lwd = 0.2) +
  scale_color_manual(values = c("black", "blue", "red"), name = "") + 
  labs(x = "Longitude", y = "Latitude") +
  guides(colour = guide_legend(override.aes = list(size = 1))) +
  ggsn::scalebar(data = NULL, location = "topright", dist = 10, 
                 height = 0.007, st.size = 2.3, st.dist = 0.015,
                 x.min = lims[1], x.max = lims[3], y.min = lims[2], y.max = lims[4]) +
  ggsn::north(data = NULL, location = "topright", scale = 0.1, symbol = 7,
                 x.min = lims[1], x.max = lims[3], y.min = lims[2], y.max = lims[4])


inset <- ggplot(can) +
  geom_sf(data = can, lwd = 0.2) + 
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  geom_rect(xmin = limsalb[[1]], xmax = limsalb[[3]], ymin = limsalb[[2]], ymax = limsalb[[4]], 
            color = "red", fill = "transparent", lwd = 0.5) +
  labs(x = "", y = "") + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))


grid.newpage()
vp_b <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vp_a <- viewport(width = 0.31, height = 0.31, x = 0.719, y = 0.831)  # the inset in upper left
print(study, vp = vp_b)
print(inset, vp = vp_a)

