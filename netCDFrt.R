## Eurocordex files representation

library(raster)
library(rasterVis)
library(maps)
library(maptools)
library(mapdata)
library(rgdal)
library(zoo)
library(ncdf4)


## We start with the lat and long, then project it to the Lambert Conformal projection

TAS <- nc_open("tas_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc")
## SIS <- nc_open("rsds_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc")

print(SIS)
names(SIS$var)

## ncdf4 lo utilizo para leer el nombre de las variables del fichero nc.

## Vamos a tomar las valores de lon lat y pasarlos a puntos.  
lat <- raster("tas_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc", varname="lat")
## lat <- raster("rsds_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc", varname="lat")
lon <- raster("tas_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc", varname="lon")
## lon <- raster("rsds_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc", varname="lon")

## Convert to points and match the lat and lons
plat <- rasterToPoints(lat)
plon <- rasterToPoints(lon)
head(plat)
## lon and lat
lonlat <- cbind(plon[,3], plat[,3])

## Specify the lonlat as spatial points with projection as long/lat

lonlat <- SpatialPoints(lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))
extent(lonlat)

## ¿proj4 string?
mycrs <- CRS("+proj=lcc +lat_1=57 +lat_2=40 +lat_0=44.5 +lon_0=20.5 +x_0=0 +y_0=0 +ellps=clrk66 +units=km +no_defs")
plonlat <- spTransform(lonlat, CRSobj = mycrs)
# Take a look
plonlat
extent(plonlat)

## Now we can properly set the coordinate information for the raster
SIS <- stack("rsds_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc", varname="rsds")
## list <- dir(pattern=".nc")
## stackSIS <- stack(list)
## stackSIS <- stackSIS*24
idx <- seq(as.Date('1989-01-01'), as.Date('1990-12-31'), 'day')
SIS <- setZ(stackSIS, idx)
names(SIS) <- idx

## Fix the projection and extent
projection(SIS) <- mycrs
extent(SIS) <- extent(plonlat)
## Take a look
SIS
plot(SIS)
levelplot(SIS,layers=200)

## Project to long lat grid
r <- projectRaster(SIS, crs=CRS("+proj=longlat +datum=WGS84"))
## Take a look
r
plot(r)
## Representa unicamente 12 graficas.
## Add contours
contour(r, add=TRUE)
## Add country lines
library("maps")
map(add=TRUE, col="blue")

## Para representar la capa que seleccione puedo utilizar levelplot
levelplot(r, layers=10)
## Utilizo los datos de GDAM
proj <- CRS('+proj=longlat +ellps=WGS84')
mapa <- readShapeLines('ESP_adm0.shp', proj4string=proj)
levelplot(r,layers=10)+layer(sp.lines(mapa))

## Con maps

ext <- as.vector(extent(r))
boundaries <- map('worldHires', xlim=ext[1:2], ylim= ext[3:4], plot=FALSE)
boundaries <- map2SpatialLines(boundaries, proj4string=CRS(projection(r))

levelplot(r, layers=10) + layers(sp.lines(boundaries))

## Subset IP extension
NewExtent <- c(-11,3.5,36,44)
boundaries <- map('worldHires', xlim=NewExtent[1:2], ylim=NewExtent[3:4], plot=FALSE)              boundaries <- map2SpatialLines(boundaries, proj4string=CRS(projection(r)))                         peninsula <- crop(r, boundaries)
levelplot(peninsula, layers=10+layer(sp.lines(boundaries))                             


##----------------------------------------------------------------------

## Temperature representation


TAS  <- stack("tas_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc", varname="tas")
idx <- seq(as.Date('1989-01-01'), as.Date('1990-12-31'), 'day')
TAS <- setZ(TAS,idx)
names(TAS) <- idx
          
##  Projection and extent
lat <- raster("tas_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc", varname="lat")
lon <- raster("tas_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc", varname="lon")

projection(TAS) <- mycrs
extent(TAS) <- extent(plonlat)
## Take a look
TAS
plot(TAS)
levelplot(TAS,layers=1)

## Project to long lat grid
t <- projectRaster(TAS, crs=CRS("+proj=longlat +datum=WGS84"))
## Take a look
t
plot(t)
## Only can display 12 graphics.
## Add contours
contour(r, add=TRUE)
## Add country lines
library("maps")
map(add=TRUE, col="black")

## maps

ext <- as.vector(extent(t))
boundaries <- map('worldHires', xlim=ext[1:2], ylim= ext[3:4], plot=FALSE)
boundaries <- map2SpatialLines(boundaries, proj4string=CRS(projection(t)))
levelplot(t, layers=10) + layer(sp.lines(boundaries))                               

## Subseting the IP.

NewExtent <- c(-11,3.5,36,44)
boundaries <- map('worldHires', xlim=NewExtent[1:2], ylim=NewExtent[3:4], plot=FALSE)              boundaries <- map2SpatialLines(boundaries, proj4string=CRS(projection(t)))                         peninsula <- crop(t, boundaries)
levelplot(peninsula, layers=10)+layer(sp.lines(boundaries)))

##-----------------------------------------
## Calculo la media de mensuales: "## En `raster` tienes `zApply` para aplicar una función a grupos definidos respecto del índice `z`.
## Si utilizas `as.yearmon` del paquete `zoo` tendrás las medias mensuales de *cada año*."

idx <- seq(as.Dates)
tmm <- zApply(t, by = as.yearmon, fun = 'mean')
temp1989 <- levelplot(tmm, layers=1:12)+layer(sp.lines(boundaries))



