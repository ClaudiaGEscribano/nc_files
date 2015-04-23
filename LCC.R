## Representacion de los datos de los ficheros nc en lcc en lugar de en lat lon

library(raster)
library(rasterVis)
library(maps)
library(maptools)
library(mapdata)
library(rgdal)
library(zoo)
library(ncdf4)

## Voy a utilizar unicamente el fichero de la temperatura

Temp <- nc_open("tas_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc")
lat <- raster("tas_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc", varname="lat")
lon <- raster("tas_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc", varname="lon")
lonlat <- cbind(lon = lon[], lat = lat[])


mycrs <- CRS("+proj=lcc +lat_1=57 +lat_2=40 +lat_0=44.5 +lon_0=20.5 +ellps=clrk66 +units=km +no_defs")
TAS<- stack("tas_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc", varname="tas")
projection(TAS) <- mycrs
extent(TAS) <- extent(lonlat)
idx <- seq(as.Date('1989-01-01'), as.Date('1990-12-31'), 'day')
TAS <- setZ(TAS, idx)
names(TAS) <- idx

## Una vez que he leidolos datos, voy a proyectar los mapas a lcc

library("maps")
ext <- as.vector(extent(TAS))
boundaries <- map('worldHires', xlim=ext[1:2], ylim= ext[3:4], plot=FALSE)
boundaries <- map2SpatialLines(boundaries, proj4string=CRS(projection(TAS)))
levelplot(TAS, layers=10) + layer(sp.lines(boundaries))
## png(filename="...") para lanzar el device
## dev.off() para cerrarlo. Me genera el png pero me esta dando problemas para abrirlo.


## Al hacer la representacion se observa que el problema esta en que la proyeccion (la del nc no es la indicada.



