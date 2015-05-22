## This script describes rsds within the IP for the 1989-2008 period.

## libraries that I need

library(raster)
library(rasterVis)
library(maps)
library(maptools)
library(mapdata)
library(rgdal)
library(zoo)
library(ncdf4)

## The nc files are merge in time with cdo tools.

################ VARIABILIDAD INTERANUAL #######

## Variabilidad anual (o interanual). Variabilidad de la irradiacion anual

## cdo mergetime *.nc rsds89_08.nc
## cdo yearmean rsds89_08.nc rsdsym.nc
## cdo mulc,8760 rsdsym.nc SISym.nc
## cdo divc,1000 SISym.nc SISymk.nc
## cdo timstd SISymk.nc std_anual.nc

SISym <- stack("rsdsym.nc", varname="rsds")
SISym <- SISym*8760/1000
idx <- seq(as.Date('1989-07-02'), as.Date('2008-07-02'), 'year')
SISym <- setZ(SISym, idx)
names(SISym) <- idx

## mask fot the IP

ext <- as.vector(extent(SISym))
boundaries <- map('worldHires', region=c('Spain','Portugal'),fill=TRUE, xlim=ext[1:2], ylim= ext[3:4], plot=FALSE)
boundaries$names
IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
boundaries_sp<- map2SpatialPolygons(boundaries, IDs=IDs, proj4string=CRS(projection(SISym)))

IP <- mask(SISym, boundaries_sp)
IP <- setZ(IP, idx)

## Take a look

levelplot(IP, contour=TRUE)
dev.copy(png, file='radiacionAnualModelo')
dev.off()

## coeficiente de variabilidad

years <- function(x) as.numeric(format(x,'%y'))

VariabilidadInteranual <- function(x) {
    sd <- calc(x, fun=function(x) sd(x))
    media <- mean(x, na.rm=TRUE)
    COV <- sd/media
    levelplot(mask(COV, boundaries_sp), contour=TRUE)
    
}

VariabilidadInteranual(SISym)
dev.copy(png, file='variabilidadInteranualmodelo')
dev.off()

sd <- calc(SISym, fun=function(x) sd(x))
media <- mean(SISym, na.rm=TRUE)
COV <- sd/media

histogram(COV)
boxplot(COV)

######################## VARIABILIDAD MENSUAL ###############################################

## Variabilidad mensual. Con las cdo calculo las medias mensuales de irracdiancia global en el plano horizontal y calculo la desviacion tipica de la irradiancia mensual.

## cdo ymonmean rsds89_08.nc rsdsmm89_08.nc ##media de eneros, feb
## cdo muldpm rsdsmm89_08.nc SISmm89_08.nc
## cdo mulc,24 SISmm89_08.nc SISmm.nc
## cdo divc,1000 SISmm.nc SISmmk.nc
## cdo monmean rsds89_08.nc rsdsmm.nc
## cdo muldpm rsdsmm.nc SISm.nc
## cdo mulc,24 SISm.nc SIS.nc
## cdo divc,1000 SIS.nc SISkwm.nc
## cdo timselstd,20[,[12]] SISkwm.nc std_mensual.nc


## El siguiente objeto raster contiene las 12 medias globales de radiacion mensual

SISmm <- stack("SISmmk.nc", varname="rsds")
idx <- seq(as.Date('2008-01-01'), as.Date('2008-12-02'), 'month')
SISmm <- setZ(SISmm, idx)
names(SISmm) <- idx

## take a look

IPmm <- mask(SISmm, boundaries_sp)
IPmm <- setZ(IPmm, idx)
levelplot(IPmm)

## Calculo de la variabilidad mensual
## Para este calculo necesitoel raster de 250 medias mensuales

SISmon <- stack("SISkwm.nc", varname='rsds')
idx <- seq(as.Date('1989-01-01'), as.Date('2008-12-31'), 'month')
SISmon <- setZ(SISmon, idx)
names(SISmon) <- idx

month <- function(x) as.numeric(format(x, '%m'))

VariabilidadMensual <- function(x) {
    sd <- zApply(x, by=month, fun='sd')
    media <- zApply(x, by=month, fun='mean')
    COV <- sd/media
    names(COV) <- c('Enero','Febrero', 'Marzo', 'Abtil', 'Mayo', 'Junio', 'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'diciembre')
    levelplot(mask(COV, boundaries_sp), contour=TRUE)
    
}

VariabilidadMensual(SISmon)
dev.copy(png, file='VariabilidadMensualModelo')
dev.off()


sd <- zApply(SISmon, by=month, fun='sd')
media <- zApply(SISmon, by=month, fun='mean')
COV <- sd/media

histogram(COV)
boxplot(COV)

################  Variabilidad de la radiacion global diaria ######################################

## cdo mulc,24 rsds89_08.nc SISd.nc
## cdo divc,1000 SISd.nc SISdkw.nc

SISd <- stack("SISdkw.nc", varname="rsds")
idx <- seq(as.Date('1989-01-01'), as.Date('2008-12-31'), 'day')
SISd <- setZ(SISd, idx)
names(SISd) <- idx

IPd <- mask(SISd, boundaries_sp)
IPd <- setZ(IPd, idx)

days <- function(x) as.numeric(format(x, '%d'))
days(as.Date('1989-01-01'))
SISd <- zApply(SISd, by=days, fun='mean') # tarda demasiado

## cdo ydaymean SISdkw.nc SISday.nc Calcula la media de radiacion global diaria para cada dia del año.

SISd <- stack('SISday.nc', varname='rsds')
idx <- seq(1:366)
SISd <- setZ(SISd, idx)
names(SISd) <- idx

IPd <- mask(SISd, boundaries_sp)
IPd <- setZ(IPd, idx)

## cdo timselstd,20[,[365]] SISdkw.nc std_diaria.nc ##Corregir, es otro comando de CDO el que habria que utilizar

stdDiaria <- stack ("std_diaria.nc", varname="rsds")
variabilidadDiaria <- stdDiaria/mean(IPd)

## Se puede calcular la variabilidad de la radiacion en toda la peninsula para comparar con los valores de cada sitio.

mean <- cellStats(SISym, stat='mean')
sd <- cellStats(SISym, stat='sd')
cvIP <- sd/mean
summary(cvIP)

                  

