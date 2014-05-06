library(raster)
library(rgdal)
library(fields)


#setwd('C:\\Users\\smcafee4.UA\\Documents\\RainSnow\\psnow_tave_regions2')
#setwd('/Volumes/SMCAFEE1/backup_work/RainSnow/psnow_tave_regions2')
setwd('C:\\Users\\smcafee4.UA\\Documents\\RainSnow\\psnow_tave_regions2')
load('coefs.arcticB.RData')
load('coefs.interiorB.RData')
load('coefs.westB.RData')
load('coefs.swinteriorB.RData')
load('coefs.swislandB.RData')
load('coefs.cookinletB.RData')
load('coefs.southcoastB.RData')

coefs.arctic = round(apply(coefs.arctic[,1:2],2,mean),3)
coefs.interior = matrix(NA,2,2)
coefs.interior[1,1] = round(mean(coefs1.interior[,1],na.rm=T),3)
coefs.interior[2,1] = round(mean(coefs1.interior[,2],na.rm=T),3)
coefs.interior[1,2] = round(mean(coefs2.interior[,1],na.rm=T),3)
coefs.interior[2,2] = round(mean(coefs2.interior[,2],na.rm=T),3)
rm(coefs1.interior,coefs2.interior)

coefs.swinterior = matrix(NA,2,2)
coefs.swinterior[1,1] = round(mean(coefs1.swinterior[,1],na.rm=T),3)
coefs.swinterior[2,1] = round(mean(coefs1.swinterior[,2],na.rm=T),3)
coefs.swinterior[1,2] = round(mean(coefs2.swinterior[,1],na.rm=T),3)
coefs.swinterior[2,2] = round(mean(coefs2.swinterior[,2],na.rm=T),3)
rm(coefs1.swinterior,coefs2.swinterior)

coefs.swisland = matrix(NA,2,2)
coefs.swisland[1,1] = round(mean(coefs1.swisland[,1],na.rm=T),3)
coefs.swisland[2,1] = round(mean(coefs1.swisland[,2],na.rm=T),3)
coefs.swisland[1,2] = round(mean(coefs2.swisland[,1],na.rm=T),3)
coefs.swisland[2,2] = round(mean(coefs2.swisland[,2],na.rm=T),3)
rm(coefs1.swisland,coefs2.swisland)

coefs.west = matrix(NA,2,2)
coefs.west[1,1] = round(mean(coefs1.west[,1],na.rm=T),3)
coefs.west[2,1] = round(mean(coefs1.west[,2],na.rm=T),3)
coefs.west[1,2] = round(mean(coefs2.west[,1],na.rm=T),3)
coefs.west[2,2] = round(mean(coefs2.west[,2],na.rm=T),3)
rm(coefs1.west,coefs2.west)

coefs.cookinlet = matrix(NA,2,2)
coefs.cookinlet[1,1] = round(mean(coefs1.cookinlet[,1],na.rm=T),3)
coefs.cookinlet[2,1] = round(mean(coefs1.cookinlet[,2],na.rm=T),3)
coefs.cookinlet[1,2] = round(mean(coefs2.cookinlet[,1],na.rm=T),3)
coefs.cookinlet[2,2] = round(mean(coefs2.cookinlet[,2],na.rm=T),3)
rm(coefs1.cookinlet,coefs2.cookinlet)

coefs.southcoast = matrix(NA,2,2)
coefs.southcoast[1,1] = round(mean(coefs1.southcoast[,1],na.rm=T),3)
coefs.southcoast[2,1] = round(mean(coefs1.southcoast[,2],na.rm=T),3)
coefs.southcoast[1,2] = round(mean(coefs2.southcoast[,1],na.rm=T),3)
coefs.southcoast[2,2] = round(mean(coefs2.southcoast[,2],na.rm=T),3)
rm(coefs1.southcoast,coefs2.southcoast)

#load in shapefiles describing the regiuons
setwd('C:\\Users\\smcafee4.UA\\Documents\\GISbase\\PFE-alaska\\')
#setwd('/Volumes/SMCAFEE1/backup_work/GISbase//PFE-alaska')
arctic = shapefile('Arctic.shp')
cookinlet = shapefile('Cook Inlet.shp')
interior = shapefile('Interior.shp')
southeast = shapefile('Southeast Panhandle.shp')
swisland = shapefile('Soutwest Islands.shp')
swinterior = shapefile('Soutwest Interior.shp')
west = shapefile('Westcentral.shp')

#load in a decadal temperature file
setwd('C:\\Users\\smcafee4.UA\\Documents\\Data\\SNAP\\ts31\\tas\\decadal_mean')
#setwd('/Users/stephaniemcafee/documents/data/snap/771m/decadal/cruts31')
test = raster('tas_decadal_mean_monthly_mean_c_cru_TS31_historical_01_1910_1919.tif')

#create a mask that's 1 where the temperature files have data and 0 otherwise
akmask = test
akmask[!is.na(akmask)] = 1

#make a blank raster for rasterizing regions
blank=raster(test)

#calculate the area of each cell in each of the regions
f.ar = rasterize(arctic,blank,getCover=T)
f.in = rasterize(interior,blank,getCover=T)
f.wc = rasterize(west,blank,getCover=T)
f.is = rasterize(swisland,blank,getCover=T)
f.sw = rasterize(swinterior,blank,getCover=T)
f.ci = rasterize(cookinlet,blank,getCover=T)
f.se = rasterize(southeast,blank,getCover=T)

rm(test,blank)

#transfer to the directory that will hold the output files
setwd('C:\\Users\\smcafee4.UA\\Documents\\RainSnow\\psnow_tave_regions2\\FS\\20c2')
#setwd('/volumes/smcafee1/backup_work/RainSnow/psnow_tave_regions2/FS')

#set up a matrix of months associated with seasons
m = c('09','10','11','12','01','02')
m=rbind(m,c('03','04','05','06','07','08'))

decs = c('1910_1919','1920_1929','1930_1939','1940_1949','1950_1959','1960_1969','1970_1979','1980_1989','1990_1999','2000_2009')
#set up the functions for Fs
#the algorithms are all of the format 1/(1+exp(-(a+b*T))) with a in the first row and b in the second row
#the columns hold the values for SONDJF(1), MAMJJA(2), except for the Arctic region, where there is only one equation
#rows 3 and 4 are the standard errors androws 5 and 6 the p-values

for (s in 1:2) {
  fn.ar = function(x) {1/(1+exp(-1*(coefs.arctic[1]+coefs.arctic[2]*x)))}
  fn.in = function(x) {1/(1+exp(-1*(coefs.interior[1,s]+coefs.interior[2,s]*x)))}
  fn.wc = function(x) {1/(1+exp(-1*(coefs.west[1,s]+coefs.west[2,s]*x)))}
  fn.is = function(x) {1/(1+exp(-1*(coefs.swisland[1,s]+coefs.swisland[2,s]*x)))}
  fn.sw = function(x) {1/(1+exp(-1*(coefs.swinterior[1,s]+coefs.swinterior[2,s]*x)))}
  fn.ci = function(x) {1/(1+exp(-1*(coefs.cookinlet[1,s]+coefs.cookinlet[2,s]*x)))}
  fn.se = function(x) {1/(1+exp(-1*(coefs.southcoast[1,s]+coefs.southcoast[2,s]*x)))}
  
    for (i in 1:6) {
      for (y in 1:10) {
        tas = raster(paste('C:\\Users\\smcafee4.UA\\Documents\\Data\\SNAP\\ts31\\tas\\decadal_mean\\tas_decadal_mean_monthly_mean_c_cru_TS31_historical','_',m[s,i],'_',decs[y],'.tif',sep=''))
        #tas = raster(paste('/users/stephaniemcafee/Documents/Data/SNAP/771m/decadal/cruts31/tas_decadal_mean_monthly_mean_c_cru_TS31_historical','_',m[s,i],'_',decs[y],'.tif',sep=''))
        #apply the regional algorithms to the temperature data, multiplying by the cell fraction
        fs.ar = f.ar*calc(mask(tas,f.ar),fn.ar)
        fs.in = f.in*calc(mask(tas,f.in),fn.in)
        fs.wc = f.wc*calc(mask(tas,f.wc),fn.wc)
        fs.is = f.is*calc(mask(tas,f.is),fn.is)
        fs.sw = f.sw*calc(mask(tas,f.sw),fn.sw)
        fs.ci = f.ci*calc(mask(tas,f.ci),fn.ci)
        fs.se = f.se*calc(mask(tas,f.se),fn.se)

        #patch the regions back together  by adding all the raters
        fs = fs.ar+fs.in+fs.wc+fs.is+fs.sw+fs.ci+fs.se
        
        #write out fs to a geotiff
        writeRaster(round(fs,0),paste('fs_decadal_mean_monthly_mean_pct_cru_TS31_historical','_',m[s,i],'_',decs[y],'.tif',sep=''),format = 'GTiff',options='COMPRESS=LZW',datatype='FLT4S',overwrite=T)
        rm(tas)
        }
        }
        
        rm(fn.ar,fn.in,fn.wc,fn.is,fn.sw,fn.ci,fn.se)
        cat('s=', s,'\n')
        }
    
