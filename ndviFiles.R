## script to read and represent ndvi files

library(raster)
library(rasterVis)

## leer los datos

data <- raster("geo00apr15a.n14-VI3g.tif")

## representar los datos

levelplot(data)

## recorte espacial del raster

e <- extent(-85,-30,-90,-20) ## extensión que queremos recortar
recorte <- crop(data, e) ##recorte del raster

levelplot(recorte, margin=FALSE)

## leemos varios ficheros

listFich <- dir(pattern="^geo00.*\\.n14-VI3g\\.tif$")

## creo un rasterStack

data2 <- stack(listFich)

## represento 2 capas

levelplot(data2, layers=1:2)

e <- extent(-85,-30,-90,-20) ## extensión que queremos recortar
recorte <- crop(data2, e) ##recorte del raster

## FUnción calc. ejemplo

## suma de las 4 capas:

foo <- calc(data2, fun=function(x) sum(x))

## paso a dataframe

foo.df <- as.data.frame(data2)
foo.df <- as.matrix(foo.df)

## vuelvo a raster

rasterVuelta <- stack(data)
rasterVuelta <- setValues(rasterVuelta, foo.df)

## calculos espaciales sobre las capas.

medias <- cellStats(data2, stat='mean')


