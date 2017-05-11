library(maptools)
library(raster)
library(rgdal)
library(gdalUtils)
library(RStoolbox)
library(bfastSpatial)
library(rts)
library(RCurl)

#Check for Modis available data
modisProducts()

#Time range of desired data
dates_mo<-c("2005.03.01","2005.07.31")

#Downloading modis data
ModisDownload("MOD13Q1", h=22, v=04, dates = dates_mo, proj=T, MRTpath= 'c:/Modis/bin', 
              bands_subset= "1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1", utm_zone=42, datum="WGS84", proj_type="UTM",  pixel_size=250)
