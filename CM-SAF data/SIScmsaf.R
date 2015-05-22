## This script describes rsds within the IP for the 1989-2008 period and for the data of the CM-SAF
## The files has been manage with CDO (climate data operators) in order to make easy some of the calculations.

library(raster)
library(rasterVis)
library(maps)
library(maptools)
library(mapdata)
library(rgdal)
library(zoo)
library(ncdf4)

## variabilidad anual. Calcula la variabilidad de la radiaicon global anual. En lugar de calcularse la media y multiplicarlo por el numero de horas, podriamos multiplicar los valores diarios por 24 y sumarlos.
##  Como los ficheros descargados de CM-SAF contienen dos variables, no se pueden utilizar los mismo operadores de cdo que hemos utilizado para los archivos de la simulacion.

## cdo splitname remapSIS


ficheros <- dir(pattern="^remap.*SIS\\.nc$")
SISdm <- lapply(ficheros, function(x) stack(x, varname='SIS')) ## Tenemos 20 rasterstack, uno por año, y cada uno de ellos contiene una capa por dia.
idx <- seq(as.Date('1989-01-01'), as.Date('2008-12-31'), 'year')
names(SISdm) <- idx


########################## VARIABILIDAD INTERANUAL ############################

## Calculo de la radiacion global anual en el plano horizontal

years <- function(x) as.numeric(format(x,'%y'))
## SISym <- zApply(SISdm, by=years, fun='mean') ## puedo aplicar la suma y despues multiplicar por 24. Faltaban datos de algunos dias del año, por lo que se ha optado por hacer la media diaria y multiplicar por las horas anuales.
SISym <- lapply(SISdm, function(x) calc(x, fun=function(x) mean(x,na.rm=1)))
SISym <- lapply(SISym, function(x) x*8760/1000) ## 20 capas con la radiacion anual. Puedo unir estas 20 capas en un unico raster. ¿Como puedo hacerlo en un bucle de manera elegante?

SISym <- stack(SISym[[1]], SISym[[2]], SISym[[3]], SISym[[4]], SISym[[5]],SISym[[6]], SISym[[7]], SISym[[8]], SISym[[9]], SISym[[10]], SISym[[11]], SISym[[12]], SISym[[13]], SISym[[14]], SISym[[15]], SISym[[16]], SISym[[17]], SISym[[18]], SISym[[19]], SISym[[20]])

## mask fot the IP

ext <- as.vector(extent(SISym))
boundaries <- map('worldHires', region=c('Spain','Portugal'),fill=TRUE, xlim=ext[1:2], ylim= ext[3:4], plot=FALSE)
boundaries$names
IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
boundaries_sp<- map2SpatialPolygons(boundaries, IDs=IDs, proj4string=CRS(projection(SISym)))

IP <- mask(SISym, boundaries_sp)
IP <- setZ(IP, idx)

## representacion radiacion global

levelplot(IP, contour=TRUE)
dev.copy(png, file='radiacionAnualCMsaf')
dev.off()

## Calculo del coeficiente de variabilidad

## cdo mergetime remap*SIS.nc SISdCMsaf89_08.nc
## cdo yearmean SISdCMsaf89_08.nc SISymCMsaf89_08.nc
## cdo mulc,8760 SISymCMsaf89_08.nc SISymCMsaf.nc
## cdo divc,1000 SISymCMsaf.nc SISymCMsafkw.nc
## cdo timstd SISymCMsafkw.nc std_anualCMsaf.nc

VariabilidadInteranual <- function(x) {
    sd <- calc(x, fun=function(x) sd(x))
    media <- mean(x, na.rm=TRUE)
    COV <- sd/media
    levelplot(mask(COV, boundaries_sp), contour=TRUE)
}

VariabilidadInteranual(SISym)
dev.copy(png, file='VariabilidadInteranualcmsaf')
dev.off()

sd <- calc(SISym, fun=function(x) sd(x))
media <- mean(SISym, na.rm=TRUE)
COV <- sd/media

histogram(COV)
boxplot(variabilidadInteranual)

##################### VARIABILIDAD MENSUAL ####################################################


## Variabilidad mensual. Calculo el valor de la radiaicon global mensual en el plano horizontal.
## El caclulo de la media mensual de cada mes y la media global de cada mes, se ha calculado con cdo porque han apareceido algunos problemas al intentarlo con R.

## cdo ymonmean remap89_08dmSIS.nc SISmm89_08.nc ## media de eneros, feb ... 
## cdo muldpm SISmm89_08.nc
## cdo mulc,24 SISmm89_08.nc SISmm.nc
## cdo div,1000 SISmm.nc SISmmk.nc ## Media de la radiacion mensual
## cdo monmean SISd89_08.nc SISm89_08.nc 
## cdo muldpm SISm89_08.nc SISm.nc
## cdo mulc,24 SISm.nc SISmensual.nc
## cdo divc,1000 SISmensual.nc SISmk.nc ## Media de cada uno de los meses de toda la serie
## cdo ymonstd SISmk.nc std_mensual.nc

## El siguiente objeto raster contiene las 12 medias mensuales de la serie temporal

SISmm <- stack("SISmmk.nc", varname="rsds")
idx <- seq(as.Date('2008-01-01'), as.Date('2008-12-02'), 'month')
SISmm <- setZ(SISmm, idx)
names(SISmm) <- idx

## Take a look

IPmm <- mask(SISmm, boundaries_sp)
IPmm <- setZ(IPmm, idx)
levelplot(IPmm)

## Calculo de la variabilidad por meses

## El siguiente objeto raster contiene 240 capas, cada una con la media mensual de radiacion.

SISmk <- stack("SISmk.nc", varname='SIS')
SISmk <- setZ(SISmk, idx)

VariabilidadMensual <- function(x) {
    sd <- zApply(x, by=months, fun=function(x) calc(x, fun=function(x) sd(x)))
    media <- SISmm
    COV <- sd/media
    levelplot(mask(COV, boundaries_sp), contour=TRUE)
}



stdMensual <- stack("std_mensual.nc", varname="rsds")
variabilidadMensual <- stdMensual/mean(IPmm)
levelplot(variabilidadMensual, contour=TRUE)
names(variabilidadMensual) <- idx
dev.copy(png, file='variabilidadMensualcmsaf')
dev.off()

histogram(variabilidadMensual)
boxplot(variabilidadMensual)

## Variabilidad de la radiacion global diaria

## cdo mulc,24 rsds89_08.nc SISd.nc
## cdo divc,1000 SISd.nc SISdkw.nc
## cdo ydaymean SISdkw.nc SISday.nc Calcula la media de radiacion global diaria para cada dia del año.

SISd <- stack("SISdkw.nc", varname="rsds")
idx <- seq(as.Date('1989-01-01'), as.Date('2008-12-31'), 'day')
SISd <- setZ(SISd, idx)
names(SISd) <- idx

IPd <- mask(SISd, boundaries_sp)
IPd <- setZ(IPd, idx)



















## Variabilidad diaria

cdo timselstd,20[,[365]] rsds89_09.nc std_diaria.nc ##ydaystd
