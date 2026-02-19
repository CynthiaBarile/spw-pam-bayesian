library(giscoR)
library(tmap)

##### Base maps ####
area.box <- as(extent(-16, -8, 48.5, 56.5), 'SpatialPolygons') # create a box as a Spatial Object
crs(area.box) <- "+proj=longlat +datum=WGS84 +no_defs" # define the crs of this box
box <- spTransform(area.box, CRS("+init=epsg:32629")) # project the spatial object (WGS84 UTM Z29: EPSG 32629)
bbox_new <- st_bbox(box) %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon
base <- gisco_get_countries(country = c("Ireland", "United Kingdom"), 
                            year = "2020", epsg = "4326",
                            resolution = 1, spatialtype = "RG") %>%
  st_transform(projection) # project it
base <- tm_shape(base, bbox = bbox_new) +
  tm_borders(lwd = 0.5, col = "grey10") +
  tm_fill(col="grey20")
print(base)

graticules <- tm_graticules(lines = FALSE, labels.size = 1)
# element with scale only for the last plot, as to not repeat it all the time
scale <- tm_scale_bar(position=c("RIGHT", "BOTTOM"), breaks = c(0,100,200), 
                      lwd = 0.1, text.size = 0.7)
print(base + graticules + scale)

my_palette <- colorRampPalette(c("#264653", "#86c9dd", "#e5b761", "#efd199"))

#### RelSST ####
relsst1 <- base + graticules + tm_shape(relSST.month[[1]][[1]]) +
  tm_raster(style = "cont", palette = my_palette(100), breaks = seq(-1.5,2.7,0.1), 
            legend.show = FALSE, midpoint = NA) +
  tm_credits("PAM1", fontfamily = "serif", position = c("LEFT", "BOTTOM"), size = 1.2)

relsst2 <- base + graticules + tm_shape(relSST.month[[1]][[2]]) +
  tm_raster(style = "cont", palette = my_palette(100), breaks = seq(-1.5,2.7,0.1), 
            legend.show = FALSE, midpoint = NA) +
  tm_credits("PAM2", fontfamily = "serif", position = c("LEFT", "BOTTOM"), size = 1.2)

relsst3 <- base + graticules + tm_shape(relSST.month[[1]][[3]]) +
  tm_raster(style = "cont", palette = my_palette(100), breaks = seq(-1.5,2.7,0.1), 
            legend.show = FALSE, midpoint = NA) +
  tm_credits("PAM3", fontfamily = "serif", position = c("LEFT", "BOTTOM"), size = 1.2)

relsst4 <- base + graticules +tm_shape(relSST.month[[1]][[4]]) +
  tm_raster(style = "cont", palette = my_palette(100), breaks = seq(-1.5,2.7,0.1), 
            legend.show = FALSE, midpoint = NA) +
  tm_credits("PAM4", fontfamily = "serif", position = c("LEFT", "BOTTOM"), size = 1.2)

relsst5 <- base + graticules +tm_shape(relSST.month[[1]][[5]]) +
  tm_raster(style = "cont", palette = my_palette(100), breaks = seq(-1.5,2.7,0.1), 
            legend.show = FALSE, midpoint = NA) +
  tm_credits("PAM5", fontfamily = "serif", position = c("LEFT", "BOTTOM"), size = 1.2)

relsst6 <- base + graticules + scale + tm_shape(relSST.month[[1]][[6]]) +
  tm_raster(style = "cont", palette = my_palette(100), breaks = seq(-1.5,2.7,0.1), 
            legend.show = FALSE, midpoint = NA) +
  tm_credits("PAM6", fontfamily = "serif", position = c("LEFT", "BOTTOM"), size = 1.2)

#### Topographic ####
eastness <- eastness.layer %>% 
  mask(mask = pred.zone) %>% 
  crop(y = extent(pred.zone))
depth.p <- base + graticules + tm_shape(depth) +
  tm_raster(style = "cont", palette = my_palette(100), 
            legend.show = FALSE, midpoint = NA) +
  tm_credits("a", fontfamily = "serif", position = c("LEFT", "BOTTOM"), size = 1.2)


#### Fronts ####
fst1 <- frontstrength$front.gradient.density..with.smoothing.sigma.5.1 %>% 
  mask(mask = pred.zone) %>% 
  crop(y = extent(pred.zone))
fst2 <- frontstrength$front.gradient.density..with.smoothing.sigma.5.2 %>%
  mask(mask = pred.zone) %>%
  crop(y = extent(pred.zone))
fst3 <- frontstrength$front.gradient.density..with.smoothing.sigma.5.3 %>%
  mask(mask = pred.zone) %>%
  crop(y = extent(pred.zone))
fst4 <- frontstrength$front.gradient.density..with.smoothing.sigma.5.4 %>%
  mask(mask = pred.zone) %>%
  crop(y = extent(pred.zone))
fst5 <- frontstrength$front.gradient.density..with.smoothing.sigma.5.5 %>%
  mask(mask = pred.zone) %>%
  crop(y = extent(pred.zone))
fst6 <- frontstrength$front.gradient.density..with.smoothing.sigma.5.6 %>%
  mask(mask = pred.zone) %>%
  crop(y = extent(pred.zone))


fst1 <- base + graticules + tm_shape(fst1) +
  tm_raster(style = "cont", palette = my_palette(100), 
            legend.show = FALSE, midpoint = NA, breaks = seq(0,0.7,length.out=100)) +
  tm_credits("PAM1", fontfamily = "serif", position = c("LEFT", "BOTTOM"), size = 1.2)

fst2 <- base + graticules + tm_shape(fst2) +
  tm_raster(style = "cont", palette = my_palette(100), 
            legend.show = FALSE, midpoint = NA, breaks = seq(0,0.7,length.out=100)) +
  tm_credits("PAM2", fontfamily = "serif", position = c("LEFT", "BOTTOM"), size = 1.2)

fst3 <- base + graticules + tm_shape(fst3) +
  tm_raster(style = "cont", palette = my_palette(100), 
            legend.show = FALSE, midpoint = NA, breaks = seq(0,0.7,length.out=100)) +
  tm_credits("PAM3", fontfamily = "serif", position = c("LEFT", "BOTTOM"), size = 1.2)

fst4 <- base + graticules + tm_shape(fst4) +
  tm_raster(style = "cont", palette = my_palette(100), 
            legend.show = FALSE, midpoint = NA, breaks = seq(0,0.7,length.out=100)) +
  tm_credits("PAM4", fontfamily = "serif", position = c("LEFT", "BOTTOM"), size = 1.2)

fst5 <- base + graticules + tm_shape(fst5) +
  tm_raster(style = "cont", palette = my_palette(100), 
            legend.show = FALSE, midpoint = NA, breaks = seq(0,0.7,length.out=100)) +
  tm_credits("PAM5", fontfamily = "serif", position = c("LEFT", "BOTTOM"), size = 1.2)
fst6 <- base + graticules + tm_shape(fst6) +
  tm_raster(style = "cont", palette = my_palette(100), 
            legend.show = FALSE, midpoint = NA, breaks = seq(0,0.7,length.out=100)) +
  tm_credits("PAM6", fontfamily = "serif", position = c("LEFT", "BOTTOM"), size = 1.2)

#### SLA #####
sla <- stacks$pam6$D05.10.2016 %>% # take the first day of each survey
  mask(mask = pred.zone) %>% 
  crop(y = extent(pred.zone))
sla6 <- base + graticules + scale + tm_shape(sla) +
  tm_raster(style = "cont", palette = my_palette(100), 
            legend.show = FALSE, midpoint = NA, breaks = seq(-0.20,0.30,length.out=100)) +
  tm_credits("PAM6\n05.10.2016", fontfamily = "serif", position = c("LEFT", "BOTTOM"), size = 1.2)

#### Legend ####
legend.dist <- 
  tm_shape(sla) +
  tm_raster(style='cont', palette=my_palette(100), 
            # set showNA = F or it adds something for missing data in the legend
            showNA = FALSE, breaks = seq(-0.20,0.30,0.1)) +
  tm_layout(legend.only = TRUE, legend.text.fontfamily = "serif", 
            legend.title.fontface = "bold.italic", legend.title.fontfamily = "serif",
            legend.bg.color = "white", legend.text.size = 1, legend.title.size = 1) 


asp <- (573871.4 - (-16944.78))/(6283704 - 5372358)
tmap_save(relsst1, filename="C:/Users/cynth/Desktop/relsst1.png", 
          dpi=600, height=asp*15, width=15, units="cm")
