library(maptools)
library(raster)
library(rgdal)
library(gdalUtils)
library(RStoolbox)
library(bfastSpatial)
library(rts)
library(plotly)


#Login details for downloading Modis data from NASA website
setNASAauth("ahmed.ebaid","Elgehadawy85")

#Check for Modis available data
modisProducts()

#Time range of desired data
dates_mo=c("1999.01.01","2017.04.01")

#Downloading modis data
ModisDownload("MOD11A2", h=22, v=8, dates = dates_mo, proj= FALSE)

setwd("F:\\somalia\\Tiff_modis")


#List of downloaded HDF files
lst <- list.files(pattern='.hdf$',full.names=FALSE)
lst

#Get dates from modis tiles name
tif_lst<-list.files(pattern="?_hdf.tif$", full.names = TRUE)
tif_lst
dates<-getMODISinfo(lst[])
dates

dates_as_date<-as.Date.default(dates$date, format="%Y/%m/%d" )
dates_as_date

date_year<-format.Date(dates_as_date, format='20%y')
date_year

date_month<-format.Date(dates_as_date, format='%m')
date_month

tif_lst_df<-data.frame(tif_lst)
tif_lst_df$year<-date_year
tif_lst_df$month<-date_month
tif_lst_df$path<-tif_lst
head(tif_lst_df)
sel
list_of_shapes<-list.files(pattern="?.shp$", recursive = TRUE ,full.names = TRUE)
tif_lst_df
#Stack cliped rasters per year
for (i in 1:length(unique(tif_lst_df$year))){
  begin<-1999
  yearno<-begin+i
  
  year_name<-paste("year_",yearno, sep = "")
  
  sel<-data.frame(tif_lst_df[which(tif_lst_df$year[] == yearno),]) 
  
  
  for(i in 1:length(unique(sel$month))) {
    m_sel<-data.frame(sel[which(sel$month[] == unique(sel$month)[i]),])
    m<-unique (sel$month)[i]
    out<-stack(m_sel$path[])
    out_mean<-overlay(out, fun=mean)
    
    for ( i in 1 : length(list_of_shapes)) {
      
      vec<-readOGR(list_of_shapes[i])
      n<-as.vector(list_of_shapes[i])
      n<-sub(".*/", ""  , n)
      n<-sub(".shp", ""  , n)
      v<-spTransform(vec, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 
                              +b=6371007.181 +units=m +no_defs"))
      r_mask<-mask(out_mean, v)
      writeRaster(r_mask, paste(year_name, m , n , sep = "_"), 
                  format="GTiff", overwrite=TRUE)
    }
    
  }
  
}




#list of translated tif scenes
tif_lst<-list.files(pattern="?_hdf.tif$", full.names = TRUE)
tif_lst

tif_lst_df<- as.data.frame(tif_lst)
tif_lst_df
tif_lst_df$year<-date_year
#Names of tiles
lst_nopunct<-gsub("\\.", "_"  , lst)
lst_nopunct

filename1 <- paste(lst_nopunct,".tif", sep = "")
filename1

#Extracting & ranslating HDF LST tile to tif format
for (i in 1:length(lst)){
  modis_tile <- get_subdatasets(lst[i])
  gdal_translate(modis_tile[1], dst_dataset= filename1[i])
}



#Read raster stacks
tif_lst_year<-list.files(pattern="?.gri$", full.names = TRUE)
tif_lst_year



#Gotopo DEM tile
dem_raster<-raster("gt30e020n40.tif")
dem_raster

#Creating polygon shape file for the extent of Modis LST scene
mod_extent<-extent(raster(tif_lst[2]))
extent_polygon<-as(mod_extent, "SpatialPolygons")
proj4string(extent_polygon)<-"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

extent_polygon

#Projecting the polygon to match the DEM proj
trans_extent<-spTransform(extent_polygon, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
trans_extent

#croping the DEM using polygon
croped_dem<-crop(dem_raster, trans_extent)
croped_dem
rm(dem_raster)

#Overview of DEM cells values distripution
hist(croped_dem)

#Creating four polygons shape files correspnding to low, mid, and high-lands aaaaaaaand stuckkkkk!
high_land<-rasterToPolygons(croped_dem, fun = function(x){x>1000}, dissolve = TRUE)
high_lands_diss<-unionSpatialPolygons(high_land, IDs = 1,length(high_land))
high_lands_diss


high_lands<-as(high_lands_diss, "SpatialPolygonsDataFrame")
writeOGR(high_lands, "high_lands","high_lands", driver = "ESRI Shapefile", overwrite_layer = TRUE)
rm(high_land)

mid_land<-rasterToPolygons(croped_dem, fun = function(x){x<=1000 & x>=500}, dissolve = TRUE)
mid_lands_diss<-unionSpatialPolygons(mid_land, rep(1, length(mid_land)))
mid_lands_diss


mid_lands<-as(mid_lands_diss, "SpatialPolygonsDataFrame")
writeOGR(mid_lands, "mid_lands","mid_lands", driver = "ESRI Shapefile", overwrite_layer = TRUE)
rm(mid_land)

low_land<-rasterToPolygons(croped_dem, fun = function(x){x<200 & x>0}, dissolve = TRUE)
low_land_diss<-unionSpatialPolygons(low_land, rep(1, length(low_land)))
low_land_diss


low_lands<-as(low_land_diss, "SpatialPolygonsDataFrame")
writeOGR(low_lands, "low_lands","low_lands", driver = "ESRI Shapefile", overwrite_layer = TRUE)
rm(low_land)



v_low_land<-rasterToPolygons(croped_dem, fun = function(x){x<500 & x>=200}, dissolve = TRUE)
v_low_land_diss<-unionSpatialPolygons(v_low_land, rep(1, length(v_low_land)))
v_low_land_diss


v_low_lands<-as(v_low_land_diss, "SpatialPolygonsDataFrame")
writeOGR(v_low_lands, "v_low_lands","v_low_lands", driver = "ESRI Shapefile", overwrite_layer = TRUE)
rm(v_low_land)

#List of polygon shape files
list_of_shapes<-list.files(pattern="?.shp$", recursive = TRUE ,full.names = TRUE)
list_of_shapes


high_land<-readOGR(list_of_shapes[1])
high_land<- spTransform(high_land, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))

mid_land<-readOGR(list_of_shapes[3])
mid_land<- spTransform(mid_land, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))

low_land<-readOGR(list_of_shapes[2])
low_land<-spTransform(low_land, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))

v_low_land<-readOGR(list_of_shapes[4])
v_low_land<-spTransform(v_low_land, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))


tif_lst<-list.files(pattern="?_hdf.tif$", full.names = TRUE)

for (i in 1:length(tif_lst) ) {
  r<-raster(tif_lst[i])
  r_name<-r@data@names
  for ( z in 1 : length(list_of_shapes)) {
    v<-readOGR(list_of_shapes[z])
    n<-as.vector(list_of_shapes[z])
    n<-sub(".*/", ""  , n)
    n<-sub(".shp", ""  , n)
    v<-spTransform(v, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 
                          +b=6371007.181 +units=m +no_defs"))
    r_mask<-mask(r, v)
    writeRaster(r_mask, paste(r_name, n , sep = "_"), 
                format="GTiff", overwrite=TRUE)
    rm(r_mask)
  }
  
  rm(r)
}


#Stack cliped rasters per year
for (i in 1:length(unique(tif_lst_df$year))){
  begin<-1999
  yearno<-begin+i
  year_name<-paste("year_",yearno, sep = "")
  
  sel<-which(tif_lst_df$year == yearno)
  
  stacking<-stack(tif_lst[sel[]])
  writeRaster(stacking, year_name, format="raster", overwrite=TRUE)
}

#Create data.frame for clculations
mod_df<-data.frame(tif_lst, ncol=1, nrow=783)
str(mod_df)
mod_df$ncol<-dates_as_date

str(mod_df$ncol)



#Stacking tif files
modis_stack<-timeStackMODIS(tif_lst)

# Low lands_max & min
tif_lst_l_lands<-list.files(pattern="year_[[:digit:]]{4}.[[:digit:]]{2}_low_lands.tif$", full.names = TRUE)
tif_lst_l_lands
low_lands_stck<-stack(tif_lst_l_lands)


low_ld_df<-data.frame(tif_lst_l_lands)
low_ld_df
str(low_ld_df)

n<-c(tif_lst_l_lands)
n
n<-sub("_low_lands.tif", "/01"  , n)
n
n<-sub("./year_", ""  , n)
n
n<-sub("_", "/"  , n)
n
n_d<-as.Date(n,format="%Y/%m/%d")
str(n_d)


low_ld_df$date<-n_d

low_ld_df$min<-minValue(low_lands_stck)-273.15

low_ld_df$max<-maxValue(low_lands_stck)-273.15



low_ld_df
str(low_ld_df)
plot(low_ld_df$date, low_ld_df$min, main="Minimum Temperatures in the Horn of Africa below 200 masl",
     xlab="Year", ylab="Temp C°", type="l")
plot(low_ld_df$date, low_ld_df$ax, main="Maximum High Temperatures in the Horn of Africa below 200 masl",
     xlab="Year", ylab="Temp C°", type="l")

plot_ly(low_ld_df, x=~date, y=~min, type = "scatter", mode="lines+markers", name="Minimum")%>%
  
  add_trace(y = ~max, name = 'Maximum', mode = 'lines+markers') %>%
  layout(title = "Maximum High & Low Temperatures in the Horn of Africa below 200 masl ",
         paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
         xaxis = list(title = "Date",
                      gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE),
         yaxis = list(title = "Temperature (degrees C)",
                      gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE))


#mid lands max and min
tif_lst_l_lands<-list.files(pattern="year_[[:digit:]]{4}.[[:digit:]]{2}_v_low_lands.tif$", full.names = TRUE)
tif_lst_l_lands
low_lands_stck<-stack(tif_lst_l_lands)


low_ld_df<-data.frame(tif_lst_l_lands)
low_ld_df
str(low_ld_df)

n<-c(tif_lst_l_lands)
n
n<-sub("_v_low_lands.tif", "/01"  , n)
n
n<-sub("./year_", ""  , n)
n
n<-sub("_", "/"  , n)
n
n_d<-as.Date(n,format="%Y/%m/%d")
str(n_d)


low_ld_df$date<-n_d

low_ld_df$min<-minValue(low_lands_stck)-273.15
low_ld_df$max<-maxValue(low_lands_stck)-273.15


low_ld_df
str(low_ld_df)
plot(low_ld_df$date, low_ld_df$min, main="Minimum Temperatures in the Horn of Africa from 200:500 masl ",
     xlab="Year", ylab="Temp C°", type="l")
plot(low_ld_df$date, low_ld_df$max, main="Maximum high Temperatures in the Horn of Africa from 200:500 masl ",
     xlab="Year", ylab="Temp C°", type="l")

plot_ly(low_ld_df, x=~date, y=~min, type = "scatter", mode="lines+markers", name="Minimum")%>%
  
  add_trace(y = ~max, name = 'Maximum', mode = 'lines+markers') %>%
  layout(title = "Maximum High & Low Temperatures in the Horn of Africa from 200:500 masl ",
         paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
         xaxis = list(title = "Date",
                      gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE),
         yaxis = list(title = "Temperature (degrees C)",
                      gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE))


# sub-high lands_max & min
tif_lst_l_lands<-list.files(pattern="year_[[:digit:]]{4}.[[:digit:]]{2}_mid_lands.tif$", full.names = TRUE)
tif_lst_l_lands
low_lands_stck<-stack(tif_lst_l_lands)


low_ld_df<-data.frame(tif_lst_l_lands)
low_ld_df
str(low_ld_df)

n<-c(tif_lst_l_lands)
n
n<-sub("_mid_lands.tif", "/01"  , n)
n
n<-sub("./year_", ""  , n)
n
n<-sub("_", "/"  , n)
n
n_d<-as.Date(n,format="%Y/%m/%d")
str(n_d)

n
low_ld_df$date<-n_d

low_ld_df$min<-minValue(low_lands_stck)-273.15
low_ld_df$max<-maxValue(low_lands_stck)-273.15


low_ld_df
str(low_ld_df)
plot(low_ld_df$date, low_ld_df$min, main="Minimum low Temperatures in the Horn of Africa from 500:1000 masl",
     xlab="Year", ylab="Temp C°",type="l")

plot(low_ld_df$date, low_ld_df$max, main="Maximum High Temperatures in the Horn of Africa from 500:1000 masl",
     xlab="Year", ylab="Temp C°", type="l")

plot_ly(low_ld_df, x=~date, y=~min, type = "scatter", mode="lines+markers", name="Minimum")%>%
  
  add_trace(y = ~max, name = 'Maximum', mode = 'lines+markers') %>%
  layout(title = "Maximum High & Low Temperatures in the Horn of Africa from 500:1000 masl ",
         paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
         xaxis = list(title = "Date",
                      gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE),
         yaxis = list(title = "Temperature (degrees C)",
                      gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE))

# high lands_max & min
tif_lst_l_lands<-list.files(pattern="year_[[:digit:]]{4}.[[:digit:]]{2}_high_lands.tif$", full.names = TRUE)
tif_lst_l_lands
low_lands_stck<-stack(tif_lst_l_lands)


low_ld_df<-data.frame(tif_lst_l_lands)
low_ld_df
str(low_ld_df)

n<-c(tif_lst_l_lands)
n
n<-sub("_high_lands.tif", "/01"  , n)
n
n<-sub("./year_", ""  , n)
n
n<-sub("_", "/"  , n)
n
n_d<-as.Date(n,format="%Y/%m/%d")
str(n_d)


low_ld_df$date<-n_d

low_ld_df$min<-minValue(low_lands_stck) -273.15
low_ld_df$max<-maxValue(low_lands_stck) -273.15


low_ld_df
str(low_ld_df)
plot(low_ld_df$date, low_ld_df$min, main="Minimum Low Temperatures in the Horn of Africa above 1000 masl",
     xlab="Year", ylab="Temp C°", type="l")

plot(low_ld_df$date, low_ld_df$max, main="Maximum High Temperatures in the Horn of Africa above 1000 masl",
     xlab="Year", ylab="Temp C°", type="l")

plot_ly(low_ld_df, x=~date, y=~min, type = "scatter", mode="lines+markers", name="Minimum")%>%
  
  add_trace(y = ~max, name = 'Maximum', mode = 'lines+markers') %>%
  layout(title = "Maximum High & Low Temperatures in the Horn of Africa above 1000 masl ",
         paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
         xaxis = list(title = "Date",
                      gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE),
         yaxis = list(title = "Temperature (degrees C)",
                      gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE))


