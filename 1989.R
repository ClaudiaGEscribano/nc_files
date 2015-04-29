## This script will describe and evaluate the data of EUROCORDEX simulation for 1989 year.

library(raster)
library(rasterVis)
library(maps)
library(maptools)
library(mapdata)
library(rgdal)
library(zoo)
library(ncdf4)

## Which are the variables in the file?

rsds <- nc_open("rsds_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc")
print(rsds)
names(rsds$var)

## Read the lon lat values in the file.  

lat <- raster("rsds_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc", varname="lat")
lon <- raster("rsds_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc", varname="lon")
lonlat <- cbind(lon = lon[], lat = lat[])


## The projection of my file

mycrs <- CRS("+proj=lcc +lat_1=57 +lat_2=40 +lat_0=44.5 +lon_0=20.5 +x_0=252 +y_0=196 +ellps=clrk66 +units=km +no_defs")
lonlat <- SpatialPoints(lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))
lonlat <- spTransform(lonlat, CRSobj = mycrs)

extent(lonlat)

SIS <- stack("rsds_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc", varname="rsds")
idx <- seq(as.Date('1989-01-01'), as.Date('1990-12-31'), 'day')
SIS <- setZ(SIS, idx)
names(SIS) <- idx

## Subseting 1989 year

SIS <- dropLayer(SIS, c(366:730))
idx <- seq(as.Date('1989-01-01'), as.Date('1989-12-31'), 'day')
SIS <- setZ(SIS, idx)
names(SIS) <- idx

## Aggregate data. In each layer of the file ther will be the mean of each month.

SISmm <- zApply(SIS, by=as.yearmon, fun='mean')

## I give the projection to the raster (I choose between SIS and SISmm)
projection(SISmm) <- mycrs
extent(SISmm) <- extent(lonlat)

SISmm
plot(SISmm)
levelplot(SISmm,layers=5)

## Project to long lat grid
r <- projectRaster(SISmm, crs=CRS("+proj=longlat +datum=WGS84"))

## Take a look
r
plot(r)

## Add contours
contour(r, add=TRUE)

## Add country lines
library("maps")
map(add=TRUE, col="blue")

## Selecting the month to be represented
levelplot(r, layers=10)

## GDAM data
proj <- CRS('+proj=longlat +ellps=WGS84')
mapa <- readShapeLines('ESP_adm0.shp', proj4string=proj)
levelplot(r,layers=10)+layer(sp.lines(mapa))
 
## Con maps
ext <- as.vector(extent(r))
boundaries <- map('worldHires', xlim=ext[1:2], ylim= ext[3:4], plot=FALSE)
boundaries <- map2SpatialLines(boundaries, proj4string=CRS(projection(r)))
levelplot(r, layers=10) + layer(sp.lines(boundaries))

dev.copy(png, file="rsds89_oct")
dev.off

## Subset IP extension
NewExtent <- c(-11,3.5,36,44)
boundaries <- map('worldHires', xlim=NewExtent[1:2], ylim=NewExtent[3:4], plot=FALSE) 
boundaries <- map2SpatialLines(boundaries, proj4string=CRS(projection(r))) 
peninsula <- crop(r, boundaries) ## for subseting the raster
levelplot(peninsula, layers=10)+layer(sp.lines(boundaries))

dev.copy(png, file="rsds_spain")
dev.off()

## ---------------------------
## I will find the layers where the max of radiation is and then the point where it happens.

max <- which.max(SISmm)
min <- which.min(SISmm)
extent(max) <- extent(lonlat)
extent(min) <- extent(lonlat)

max_rsds <- projectRaster(max, crs=CRS("+proj=longlat +datum=WGS84"))
min_rsds <- projectRaster(min, crs=CRS("+proj=longlat +datum=WGS84"))

## In order to know the numeric value of the max and min radiation:

peninsula  <- as.data.frame(peninsula) ## If I want the hole extension, I would do it with SISmm
class(peninsula)
head(peninsula)

maximos_mensuales <- c(lapply(SISmm, which.max))

maximos_mensuales <- function(x) c(lapply(x, which.max))
minimos_mensuales <- function(x) c(lapply(x, which.min))

## Where is the max (and min) located

SISmm <- zApply(SIS, by=as.yearmon, fun='mean') ## It is a rasterstack again
max <- which.max(peninsula)
min <- which.min(peninsula)

## extent(max) <- extent(lonlat)
## extent(min) <- extent(lonlat)

max_rsds <- projectRaster(max, crs=CRS("+proj=longlat +datum=WGS84"))
min_rsds <- projectRaster(min, crs=CRS("+proj=longlat +datum=WGS84"))

iMax <- which.max(max_rsds)
imin <- which.min(min_rsds)
xyMax <- xyFromCell(max_rsds, iMax)
xymin <- xyFromCell(min_rsds, imin)

levelplot(max_rsds)+layer(sp.lines(boundaries))
xyMax <- SpatialPoints(xyMax, proj4string = CRS("+proj=longlat +datum=WGS84"))
xymin <- SpatialPoints(xymin, proj4string = CRS("+proj=longlat +datum=WGS84"))
levelplot(max_rsds)+layer(sp.points(xyMax))+layer(sp.lines(boundaries))
levelplot(min_rsds)+layer(sp.points(xymin))+layer(sp.lines(boundaries))

## drawing with basic plots and maps

plot(max_rsds)
map(add=TRUE)
points(xyMax)

## (Los maximos de radiacion aparecen en el norte de la peninsula, hay que ver si es un error o no)

