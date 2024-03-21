# Plot multiple map
library(ggplot2)
library(sp)
library(dtplyr)
library(raster)
library(lattice)
library(RColorBrewer)
library(rasterVis)
library(terra)

path <- "/Users/macbookairdemilo/Desktop/SenegalMODIS_NDVI"
files <- list.files(path, pattern = ".tif$")


rastlist <- list.files(path, pattern='.tif$', all.files=TRUE, full.names=TRUE)
allrasters1 <- lapply(rastlist, terra::rast)
st <- stack(rastlist)
crs(st)
STT <- stack(mget(rep("st")))
cols <- colorRampPalette(brewer.pal(4,"RdYlGn"))
names(STT)
rasternames <- gsub("NDVI","",names(STT))
rasternames

levelplot(STT,
          layout=c(6, 6), 
          col.regions=cols, 
          names.attr=rasternames, zlim=c(0,0.5), 
          scales=list(draw=FALSE))

STT_df <- as.data.frame(STT, xy = TRUE)
ggplot(STT_df) +
  geom_histogram(aes(value)) + 
  facet_wrap(~variable)

bwplot(STT)



NDVI2010241 <- raster::subset(STT, grep('NDVI2010241', names(STT)))
plot(NDVI2010241)
