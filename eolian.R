library(ncdf4)
library(fields)
library(raster)
library(dplyr)
library(sp)
library(rgdal)
library(terra)

nc=nc_open("eolian.nc")
print(nc)

nc$var
elev=ncvar_get(nc,'elevation')
lon=ncvar_get(nc,"lon")
lat=ncvar_get(nc,"lat")
dim(elev)

#1 sposób
temp=flatten(lapply(lon,function(x) lapply(lat,function(y) c(x,y,elev[x,y]))))
result= data.frame(matrix(unlist(temp),ncol=3,byrow=TRUE))
names(result)= c("x","y","elevation")

#2 sposób
y=rast('NETCDF:"dane.nc"')
x=raster::brick("dane.nc",varname='elevation')
result2=as.data.frame(x[[1]],xy=TRUE)

#3 sposób
cartesian_product<-expand.grid(lon,lat)
result4=cartesian_product
result4['elevation']=as.vector(elev)
names(result4)= c("x","y","elevation")

##############

coordinates(result4)<-c("x","y")
proj4string(result4)<-CRS("+proj=longlat +datum=WGS84")
result4<-spTransform(result4, CRS("+proj=utm +zone=33 ellps=WGS84"))
result4<-as.data.frame(result4)
result4<-dplyr::mutate(result4, id=1:nrow(result4))
head(result4)
result4<-dplyr::select(result4, c(2,3,1,4))
head(result4)


write.csv(x=result4, file = "eolian_UTM.csv", sep=" ", row.names = F, col.names = F )

nc_close(nc)

