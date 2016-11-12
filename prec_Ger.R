# This is the first script on precepetation of Germany

library(raster)
raster()

Germany1<-getData("GADM",country="DEU",level=1)
plot(Germany1)


Germany2<-getData("GADM",country="DEU",level=2)
plot(Germany2)

Ger_prec<-getData("worldclim", var="prec", res=.5,lon=10,lat=51)
plot(Ger_prec)

Ger_prec2<-getData("worldclim", var="prec", res=.5,lon=20,lat=51)
plot(Ger_prec2)

prec_clip<-crop(Ger_prec2,Germany1)
plot(prec_clip)

prec_clip_mask<-mask(prec_clip,Germany1)


plot(prec_clip_mask)