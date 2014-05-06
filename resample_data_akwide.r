setwd("C:/Users/smcafee4.UA/Documents/RainSnow/psnow_tave")
load('m.tave_p.snow.RData')

setwd("C:/Users/smcafee4.UA/Documents/RainSnow/psnow_tave_regions")
rm(months,m.maxtave_snowdays,m.mintave_raindays,maxTsnow,minTrain,allraindays,allraindays10,allsnowdays10,ays10)
rm(m.dtr, m.tave_allraindays, m.tave_allsnowdays, m.tave_drydays, m.tave_mixdays, m.tave_raindays, m.tave_snowdays)
rm(m.tave_wetdays, m.tmax,m.tmin,mixdays10,n.allraindays,n.allsnowdays,n.allsnowdays10,n.mixdays,n.mixdays10)
rm(n.raindays,n.snowdays,n.trace_allsnowdays,n.trace_raindays,n.trace_snowdays,n.trace_wetdays,m.prcp,p.snow)
rm(p.mix,p.trace_allsnow,p.trace_wet,xnza1)

load('resampled_10yraves.RData')

#NOTE 1/15/2013  -- I'm not sure why this is here b/c I don't think I ever use mtave.10 and pallsnow.10
#Use all months -- presumably there will be some months when the model isn't statistically significant

#run noaa regions to apply sttation assignments
source('noaa_regions.r')

#any months in which there were neither wet days nor snowy days will have a p.snow value of NaN
#replace any instances of n.wetdays ==0 with NA in p.snow and m.tave
a = which(n.wetdays == 0)
p.allsnow[a] = NA
m.tave[a] = NA
rm(a)        #this actually removes a large number of months -- will need to recalculate the number of good years per month

#make sure -- again that NA in any variable forces NA in all others
a = which(is.na(p.allsnow))
m.tave[a] = NA
rm(a)
a = which(is.na(m.tave))
p.allsnow[a] = NA
rm(a)

use.sites2 = matrix(NA,ns,12)
for (m in 1:12) {
  for (s in 1:ns) {
  use.sites2[s,m] = length(which(!is.na(m.tave[s,m,])))
  }}
rm(s,m)

#set up arrays to hold resampled combinations of temperature and percent snow for each region
mtave.10 = array(NA,c(ns,12,1000))
pallsnow.10 = array(NA,c(ns,12,1000))

#for each station and month, create 1000 random samples of 10-year averages -- any 10-years,
#not necessarily consecutive
for (s in 1:ns) {
	for (m in 1:12) {
			nyr = length(which(!is.na(m.tave[s,m,])))
			temptave = m.tave[s,m,which(!is.na(m.tave[s,m,]))]
			tempallsnow = p.allsnow[s,m,which(!is.na(m.tave[s,m,]))]
			if (use.sites2[s,m] >=30) {
				for (i in 1:1000) {
				x = sample(c(1:nyr),10,replace = F)
				mtave.10[s,m,i] = mean(temptave[x])
				pallsnow.10[s,m,i] = mean(tempallsnow[x])
				rm(x)}
			rm(nyr,temptave,tempallsnow)}
			}}
rm(s,m,i)


#beta regression can't handle values of 0 or 1, so replace with values that are very close
pallsnow.10[pallsnow.10 == 0] = 0.000001
pallsnow.10[pallsnow.10 == 1] = 0.999999

save.image('resampled_10yraves.RData')

