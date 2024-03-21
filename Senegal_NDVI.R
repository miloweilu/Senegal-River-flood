library(terra)
library(ggplot2)
library(reshape2)

input <- '/Users/macbookairdemilo/Desktop/SenegalMODIS_NDVI'
setwd(input)
listfichiers <- list.files(path='.', pattern = '.tif$')
stack_NDVI <- rast(listfichiers)

## Color composite
plotRGB(stack_NDVI, r=1, b=3, g=6, stretch='lin')


## Replace NaN values with 0
stack_NDVI[is.nan(stack_NDVI)] <- 0

NDVImin <- min(stack_NDVI)
plot(NDVImin)
NDVImax <- max(stack_NDVI)
plot(NDVImax)


## We can calculate the magnitude of NDVI variations within the year :
amplitude <- NDVImax - NDVImin
stack_max_min_amp <- c(NDVImin, NDVImax, amplitude)
plotRGB(stack_max_min_amp, r=3, g=2, b=1, stretch='lin', axes=TRUE)
# high amplitude = red, high NDVI = green, low NDVI = blue

## Now we will map the big categories using kmeans. 
stackmm <- c(NDVImin, NDVImax)
set.seed(99)
kmncluster <- kmeans(stackmm[], centers=2, iter.max = 500, nstart = 5)
kmeans_raster <- rast(stackmm, nlyr=1)
kmeans_raster[] <- kmncluster$cluster
plot(kmeans_raster)



## To produce a map of water : 
## We can also extract a map of water by empirical thresholds, 
## in a way to keep pixels with negative NDVI:
water <- rast(kmeans_raster)
water[,] <- 0
water[kmeans_raster == 2] <-1
plot(water)

NDVI_water <- stack_NDVI*water
plotRGB(NDVI_water, r=4, g=3, b=2, stretch='lin') 
set.seed(99)
kmncluster_water <- kmeans(NDVI_water[], centers=2, iter.max=500, nstart=5)
kmeans_raster_water <- rast(NDVI_water, nlyr=1)
kmeans_raster_water[] <- kmncluster_water$cluster


## We extract the temporal NDVI signature of each class:
centres <- kmncluster_water$centers
Datevec <-c(1,9,17,25,33,41,49,57,65,73,81,89,97,105,113,121,129,137,145,153,161,169,177,
            185,193,201,209,217,225,233,241,249,257,265,273,281,289,297,305,313,321,329,
            337,345,353,361)
colnames(centres) <- Datevec
centres_reshaped <- melt(centres)
colnames(centres_reshaped) <- c('Groupe', 'Date', 'NDVI')
ggplot(centres_reshaped,aes(x = Date, y = NDVI, colour=as.factor(Groupe), group = as.factor(Groupe))) + 
  geom_point() +
  ylab("NDVI Value") +
  xlab("Day of year") +
  geom_line()


## Days when NDVI > 0.25
NDVI_gt_025_indices <- which(centres > 0.25, arr.ind = TRUE)
days_with_gt_025_NDVI <- numeric(0)
# Check if there are any NDVI values greater than 0.25
if (nrow(NDVI_gt_025_indices) > 0) {
  # Get the corresponding dates from Datevec
  days_with_gt_025_NDVI <- Datevec[NDVI_gt_025_indices[, "col"]]
}
days_with_gt_025_NDVI