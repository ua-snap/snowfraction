setwd("C:/Users/smcafee4.UA/Documents/RainSnow/psnow_tave")
load('m.tave_p.snow.RData')

setwd("C:/Users/smcafee4.UA/Documents/RainSnow/psnow_tave_regions")
rm(months,m.maxtave_snowdays,m.mintave_raindays,maxTsnow,minTrain,allraindays,allraindays10,allsnowdays10,ays10)
rm(m.dtr, m.tave_allraindays, m.tave_allsnowdays, m.tave_drydays, m.tave_mixdays, m.tave_raindays, m.tave_snowdays)
rm(m.tave_wetdays, m.tmax,m.tmin,mixdays10,n.allraindays,n.allsnowdays,n.allsnowdays10,n.mixdays,n.mixdays10)
rm(n.raindays,n.snowdays,n.trace_allsnowdays,n.trace_raindays,n.trace_snowdays,n.trace_wetdays,m.prcp,p.snow)
rm(p.mix,p.trace_allsnow,p.trace_wet,xnza1)


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
mtave.10.arctic = array(NA,c(length(arctic),12,1000))
pallsnow.10.arctic = array(NA,c(length(arctic),12,1000))

mtave.10.west = array(NA,c(length(west),12,1000))
pallsnow.10.west = array(NA,c(length(west),12,1000))

mtave.10.interior = array(NA,c(length(interior),12,1000))
pallsnow.10.interior = array(NA,c(length(interior),12,1000))

mtave.10.swinterior = array(NA,c(length(swinterior),12,1000))
pallsnow.10.swinterior = array(NA,c(length(swinterior),12,1000))

mtave.10.swisland = array(NA,c(length(swisland),12,1000))
pallsnow.10.swisland = array(NA,c(length(swisland),12,1000))

mtave.10.cookinlet = array(NA,c(length(cookinlet),12,1000))
pallsnow.10.cookinlet = array(NA,c(length(cookinlet),12,1000))

mtave.10.southcoast = array(NA,c(length(southcoast),12,1000))
pallsnow.10.southcoast = array(NA,c(length(southcoast),12,1000))

#for each station and month, create 1000 random samples of 10-year averages -- any 10-years,
#not necessarily consecutive
for (s in 1:length(arctic)) {
	for (m in 1:12) {
			nyr = length(which(!is.na(m.tave[arctic[s],m,])))
			temptave = m.tave[arctic[s],m,which(!is.na(m.tave[arctic[s],m,]))]
			tempallsnow = p.allsnow[arctic[s],m,which(!is.na(m.tave[arctic[s],m,]))]
			if (use.sites2[arctic[s],m] >=30) {
				for (i in 1:1000) {
				x = sample(c(1:nyr),10,replace = F)
				mtave.10.arctic[s,m,i] = mean(temptave[x])
				pallsnow.10.arctic[s,m,i] = mean(tempallsnow[x])
				rm(x)}
			rm(nyr,temptave,tempallsnow)}
			}}
rm(s,m,i)

for (s in 1:length(west)) {
	for (m in 1:12) {
			nyr = length(which(!is.na(m.tave[west[s],m,])))
			temptave = m.tave[west[s],m,which(!is.na(m.tave[west[s],m,]))]
			tempallsnow = p.allsnow[west[s],m,which(!is.na(m.tave[west[s],m,]))]
			if (use.sites2[west[s],m] >=30) {
				for (i in 1:1000) {
				x = sample(c(1:nyr),10,replace = F)
				mtave.10.west[s,m,i] = mean(temptave[x])
				pallsnow.10.west[s,m,i] = mean(tempallsnow[x])
				rm(x)}
			rm(nyr,temptave,tempallsnow)}
			}}
rm(s,m,i)

for (s in 1:length(interior)) {
	for (m in 1:12) {
			nyr = length(which(!is.na(m.tave[interior[s],m,])))
			temptave = m.tave[interior[s],m,which(!is.na(m.tave[interior[s],m,]))]
			tempallsnow = p.allsnow[interior[s],m,which(!is.na(m.tave[interior[s],m,]))]
			if (use.sites2[interior[s],m] >=30) {
				for (i in 1:1000) {
				x = sample(c(1:nyr),10,replace = F)
				mtave.10.interior[s,m,i] = mean(temptave[x])
				pallsnow.10.interior[s,m,i] = mean(tempallsnow[x])
				rm(x)}
			rm(nyr,temptave,tempallsnow)}
			}}
rm(s,m,i)

for (s in 1:length(swisland)) {
	for (m in 1:12) {
			nyr = length(which(!is.na(m.tave[swisland[s],m,])))
			temptave = m.tave[swisland[s],m,which(!is.na(m.tave[swisland[s],m,]))]
			tempallsnow = p.allsnow[swisland[s],m,which(!is.na(m.tave[swisland[s],m,]))]
			if (use.sites2[swisland[s],m] >=30) {
				for (i in 1:1000) {
				x = sample(c(1:nyr),10,replace = F)
				mtave.10.swisland[s,m,i] = mean(temptave[x])
				pallsnow.10.swisland[s,m,i] = mean(tempallsnow[x])
				rm(x)}
			rm(nyr,temptave,tempallsnow)}
			}}
rm(s,m,i)

for (s in 1:length(swinterior)) {
	for (m in 1:12) {
			nyr = length(which(!is.na(m.tave[swinterior[s],m,])))
			temptave = m.tave[swinterior[s],m,which(!is.na(m.tave[swinterior[s],m,]))]
			tempallsnow = p.allsnow[swinterior[s],m,which(!is.na(m.tave[swinterior[s],m,]))]
			if (use.sites2[swinterior[s],m] >=30) {
				for (i in 1:1000) {
				x = sample(c(1:nyr),10,replace = F)
				mtave.10.swinterior[s,m,i] = mean(temptave[x])
				pallsnow.10.swinterior[s,m,i] = mean(tempallsnow[x])
				rm(x)}
			rm(nyr,temptave,tempallsnow)}
			}}
rm(s,m,i)


for (s in 1:length(cookinlet)) {
	for (m in 1:12) {
			nyr = length(which(!is.na(m.tave[cookinlet[s],m,])))
			temptave = m.tave[cookinlet[s],m,which(!is.na(m.tave[cookinlet[s],m,]))]
			tempallsnow = p.allsnow[cookinlet[s],m,which(!is.na(m.tave[cookinlet[s],m,]))]
			if (use.sites2[cookinlet[s],m] >=30) {
				for (i in 1:1000) {
				x = sample(c(1:nyr),10,replace = F)
				mtave.10.cookinlet[s,m,i] = mean(temptave[x])
				pallsnow.10.cookinlet[s,m,i] = mean(tempallsnow[x])
				rm(x)}
			rm(nyr,temptave,tempallsnow)}
			}}
rm(s,m,i)

for (s in 1:length(southcoast)) {
	for (m in 1:12) {
			nyr = length(which(!is.na(m.tave[southcoast[s],m,])))
			temptave = m.tave[southcoast[s],m,which(!is.na(m.tave[southcoast[s],m,]))]
			tempallsnow = p.allsnow[southcoast[s],m,which(!is.na(m.tave[southcoast[s],m,]))]
			if (use.sites2[southcoast[s],m] >=30) {
				for (i in 1:1000) {
				x = sample(c(1:nyr),10,replace = F)
				mtave.10.southcoast[s,m,i] = mean(temptave[x])
				pallsnow.10.southcoast[s,m,i] = mean(tempallsnow[x])
				rm(x)}
			rm(nyr,temptave,tempallsnow)}
			}}
rm(s,m,i)

#beta regression can't handle values of 0 or 1, so replace with values that are very close
pallsnow.10.arctic[pallsnow.10.arctic == 0] = 0.000001
pallsnow.10.arctic[pallsnow.10.arctic == 1] = 0.999999

pallsnow.10.west[pallsnow.10.west == 0] = 0.000001
pallsnow.10.west[pallsnow.10.west == 1] = 0.999999

pallsnow.10.interior[pallsnow.10.interior == 0] = 0.000001
pallsnow.10.interior[pallsnow.10.interior == 1] = 0.999999

pallsnow.10.swinterior[pallsnow.10.swinterior == 0] = 0.000001
pallsnow.10.swinterior[pallsnow.10.swinterior == 1] = 0.999999

pallsnow.10.swisland[pallsnow.10.swisland == 0] = 0.000001
pallsnow.10.swisland[pallsnow.10.swisland == 1] = 0.999999

pallsnow.10.cookinlet[pallsnow.10.cookinlet == 0] = 0.000001
pallsnow.10.cookinlet[pallsnow.10.cookinlet == 1] = 0.999999

pallsnow.10.southcoast[pallsnow.10.southcoast == 0] = 0.000001
pallsnow.10.southcoast[pallsnow.10.southcoast == 1] = 0.999999

save.image('resampled_10yraves.RData')

