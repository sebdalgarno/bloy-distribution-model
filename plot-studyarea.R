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
  ggplot2::geom_line(data = segs2, aes(x = X, y = Y, group = LineID, color = SurveyYear), lwd = 0.4) +
  scale_color_manual(values = c("black", "blue", "red"), name = "") + 
  labs(x = "Longitude", y = "Latitude") +
  guides(colour = guide_legend(override.aes = list(size = 1))) +
  ggsn::scalebar(data = NULL, location = "bottomleft", dist = 10, 
                 height = 0.007, st.size = 2.3, st.dist = 0.015,
                 x.min = lims[1], x.max = lims[3], y.min = lims[2], y.max = lims[4]) +
  ggsn::north(data = NULL, location = "bottomleft", scale = 0.1, symbol = 7,
                 x.min = lims[1], x.max = lims[3], y.min = lims[2], y.max = lims[4]) +
  theme(axis.title = element_text(size = 13),
        legend.text = element_text(size = 11),
        axis.text = element_text(size = 9))


inset <- ggplot(can) +
  geom_sf(data = can, lwd = 0.2) + 
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  geom_rect(xmin = limsalb[[1]], xmax = limsalb[[3]], ymin = limsalb[[2]], ymax = limsalb[[4]], 
            color = "black", fill = "transparent", lwd = 0.6) +
  labs(x = "", y = "") + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

subfoldr::save_plot(plot = study, x = "study-area", csv = F, report = F, width = 7, height = 8)
subfoldr::save_plot(plot = inset, x = "inset", csv = F, report = F, width = 6, height = 9)
