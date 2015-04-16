## This script will represent wind speed within the IP.

## Cargamos las librerias necesarias

library(raster)
library(rasterVis)
library(maps)
library(maptools)
library(mapdata)
library(rgdal)
library(zoo)
library(ncdf4)

## Leemos dos ficheros de las dos variables de viento

ua <- nc_open("ua850_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc")
va <- nc_open("va850_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc")

names(va$var)
names(ua$var)

lat <- raster("ua850_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc", varname="lat")
lon <- raster("ua850_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc", varname="lon")

## Convert to points and match the lat and lons
plat <- rasterToPoints(lat)
plon <- rasterToPoints(lon)
head(plat)
head(plon)

## lon and lat
lonlat <- cbind(plon[,3], plat[,3])
head(lonlat)

## Specify the lonlat as spatial points with projection as long/lat

lonlat <- SpatialPoints(lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))
extent(lonlat)

## ¿proj4 string?
mycrs <- CRS("+proj=lcc +lat_1=57 +lat_2=40 +lat_0=44.5 +lon_0=20.5 +x_0=0 +y_0=0 +ellps=clrk66 +units=km +no_defs")
plonlat <- spTransform(lonlat, CRSobj = mycrs)
# Take a look
plonlat
extent(plonlat)

## Now we can properly set the coordinate infromation for the raster

stackUA <- stack("ua850_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc")
stackVA <-stack("va850_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc")
idx <- seq(as.Date('1989-01-01'), as.Date('1990-12-31'), 'day')
UA <- setZ(stackUA, idx)
VA <- setZ(stackVA, idx)

## Fix the projection and extent

projection(UA) <- mycrs
extent(UA) <- extent(plonlat)
projection(VA) <- mycrs
extent(VA) <- extent(plonlat)

## I will represent the wind vector field of one day

## I subset the day from de UA and VA objects

u <- subset(UA, 1)
v <- subset(VA, 1)
w <- brick(u, v)

## project to a lat lon grid

wind <-projectRaster(w, crs=CRS("+proj=longlat +datum=WGS84"))
vectorplot(wind, isField=TRUE, par.settings=BTCTheme(), colorkey=FALSE, scales=list(draw=FALSE))

ext <- as.vector(extent(wind))
boundaries <- map('worldHires', xlim=ext[1:2], ylim= ext[3:4], plot=FALSE)
boundaries <- map2SpatialLines(boundaries, proj4string=CRS(projection(wind)))
vectorplot(wind, isField=TRUE, par.settings=BTCTheme(), colorkey=FALSE, scales=list(draw=FALSE)) + layer(sp.lines(boundaries))

## Subseting the IP.

NewExtent <- c(-11,3.5,36,44)
boundaries <- map('worldHires', xlim=NewExtent[1:2], ylim=NewExtent[3:4], plot=FALSE)              boundaries <- map2SpatialLines(boundaries, proj4string=CRS(projection(wind)))
Peninsula <- crop(wind, boundaries)

vectorplot(Peninsula, isField=TRUE, par.settings=BTCTheme(), colorkey=FALSE, scales=list(draw=FALSE), narrows=3000)+ layer(sp.lines(boundaries))

## -------------------------

## As it only is represented one day, you are not able to see the commond trends within the IP. I would try it with a mean month.

UAmm <- zApply(UA, by = as.yearmon, fun = 'mean') ## Es un calculo que demanda un tiempo
VAmm <- zApply(VA, by = as.yearmon, fun = 'mean')

## Represnto enero del 89

UA1989 <- subset(UAmm, 1)
VA1989 <- subset(VAmm, 1)
wind89 <- brick(UA1989, VA1989)

windEne89 <-projectRaster(wind89, crs=CRS("+proj=longlat +datum=WGS84"))
vectorplot(windEne89, isField=TRUE, par.settings=BTCTheme(), colorkey=FALSE, scales=list(draw=FALSE))

ext <- as.vector(extent(windEne89))
boundaries <- map('worldHires', xlim=ext[1:2], ylim= ext[3:4], plot=FALSE)
boundaries <- map2SpatialLines(boundaries, proj4string=CRS(projection(wind)))
vectorplot(windEne89, isField=TRUE, par.settings=BTCTheme(), colorkey=FALSE, scales=list(draw=FALSE)) + layer(sp.lines(boundaries))


## Subseting the IP.

NewExtent <- c(-11,3.5,36,44)
boundaries <- map('worldHires', xlim=NewExtent[1:2], ylim=NewExtent[3:4], plot=FALSE)              boundaries <- map2SpatialLines(boundaries, proj4string=CRS(projection(wind)))
Peninsula <- crop(windEne89, boundaries)

vectorplot(Peninsula, isField=TRUE, par.settings=BTCTheme(), colorkey=FALSE, scales=list(draw=FALSE), narrows=3000)+ layer(sp.lines(boundaries))


