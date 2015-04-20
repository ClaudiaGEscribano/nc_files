## prueba

TAS <- nc_open("tas_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc")
lat <- raster("tas_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc", varname="lat")
lon <- raster("tas_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc", varname="lon")
## Convert to points and match the lat and lons
plat <- rasterToPoints(lat)
plon <- rasterToPoints(lon)
head(plat)
## lon and lat
lonlat <- cbind(plon[,3], plat[,3])

mycrs <- CRS("+proj=lcc +lat_1=57 +lat_2=40 +lat_0=44.5 +lon_0=20.5 +x_0=0 +y_0=0 +ellps=clrk66 +units=km +no_defs")
lonlat <- SpatialPoints(lonlat, proj4string = CRS("+proj=lcc +lat_1=57 +lat_2=40 +lat_0=44.5 +lon_0=20.5 +x_0=0 +y_0=0 +ellps=clrk66 +units=km +no_defs"))
extent(lonlat)

SIS <- stack("rsds_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_UCLM-PROMES_v1_day_19890101-19901231.nc", varname="rsds")
idx <- seq(as.Date('1989-01-01'), as.Date('1990-12-31'), 'day')
SIS <- setZ(stackSIS, idx)
names(SIS) <- idx

projection(SIS) <- mycrs
extent(SIS) <- extent(lonlat)

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
boundaries <- map('worldHires', xlim=NewExtent[1:2], ylim=NewExtent[3:4], plot=FALSE) 
boundaries <- map2SpatialLines(boundaries, proj4string=CRS(projection(r))) 
peninsula <- crop(r, boundaries)
levelplot(peninsula, layers=10+layer(sp.lines(boundaries))
                                         
