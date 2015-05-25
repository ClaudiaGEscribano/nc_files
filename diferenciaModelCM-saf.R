## Este script muestra las diferencias de radiacion entre la simulacion del modelo y lso datos de CM-saf

library(raster)
library(rasterVis)
library(maps)
library(maptools)
library(mapdata)
library(rgdal)
library(zoo)
library(ncdf4)

## RADIACION ANUAL MODELO

SISymModelo <- stack("rsdsym.nc", varname="rsds")
SISymModelo <- SISymModelo*8760/1000
idx <- seq(as.Date('1989-07-02'), as.Date('2008-07-02'), 'year')
SISymModelo <- setZ(SISymModelo, idx)
names(SISymModelo) <- idx

## RADIACION ANUAL CM-SAF

setwd("/home/claudia/EUROCORDEXdata/CM-SAF data")

ficheros <- dir(pattern="^remap.*SIS\\.nc$")
SISdm <- lapply(ficheros, function(x) stack(x, varname='SIS')) 
idx <- seq(as.Date('1989-01-01'), as.Date('2008-12-31'), 'year')
names(SISdm) <- idx

SISymCMsaf <- lapply(SISdm, function(x) calc(x, fun=function(x) mean(x,na.rm=1)))
SISymCMsaf <- lapply(SISymCMsaf, function(x) x*8760/1000)

SISymCMsaf <- stack(SISymCMsaf[[1]], SISymCMsaf[[2]], SISymCMsaf[[3]], SISymCMsaf[[4]], SISymCMsaf[[5]],SISymCMsaf[[6]], SISymCMsaf[[7]], SISymCMsaf[[8]], SISymCMsaf[[9]], SISymCMsaf[[10]], SISymCMsaf[[11]], SISymCMsaf[[12]], SISymCMsaf[[13]], SISymCMsaf[[14]], SISymCMsaf[[15]], SISymCMsaf[[16]], SISymCMsaf[[17]], SISymCMsaf[[18]], SISymCMsaf[[19]], SISymCMsaf[[20]])

## mask the IP

ext <- as.vector(extent(SISymModelo))
boundaries <- map('worldHires', region=c('Spain','Portugal'),fill=TRUE, xlim=ext[1:2], ylim= ext[3:4], plot=FALSE)
IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
boundaries_sp<- map2SpatialPolygons(boundaries, IDs=IDs, proj4string=CRS(projection(SISymModelo)))

## DIFERENCIA DE RADIACION ANUAL

IPmodelo <- mask(SISymModelo, boundaries_sp)
IPcmsaf <- mask(SISymCMsaf, boundaries_sp)

levelplot(IPmodelo-IPcmsaf, contour=TRUE, par.settings=RdBuTheme)
dev.copy(png, file='DiferenciaRadiacionAnualModelo-CMSAF', width=800, height=800)
dev.off()

## DIFERENCIA EN EL COEFICIENTE DE VARIABILIDAD

## COV del modelo

VariabilidadInteranual <- function(x) {
    sd <- calc(x, fun=function(x) sd(x))
    media <- mean(x, na.rm=TRUE)
    COV <- sd/media
      
}

COVmodelo <- VariabilidadInteranual(SISymModelo)
COVcmsaf <- VariabilidadInteranual(SISymCMsaf)

levelplot(mask(COVmodelo, boundaries_sp), contour=TRUE)  
levelplot(mask(COVcmsaf, boundaries_sp), contour=TRUE)

levelplot(mask(COVmodelo-COVcmsaf,boundaries_sp), contour=TRUE, par.settings=RdBuTheme)

## Valor de la radiacion del modelo sobre la observada.

sesgo <- (SISymModelo-SISymCMsaf)/SISymCMsaf
sesgo <- sesgo*100
levelplot(mask(sesgo, boundaries_sp), contour=TRUE, par.settings=RdBuTheme)
dev.copy(png, file='sesgo del modelo con respecto a obs. interanual')
dev.off()
