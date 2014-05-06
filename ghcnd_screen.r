
#load in data that's been qc'd
load('C:\\Users\\smcafee4.UA\\Documents\\RainSnow\\ghcn_daily\\ak_ghcnd_screened2.RData')

#clean up variables that are no longer needed
rm(da,data,mflag.prcp,mflag.snow,mflag.tmax,mflag.tmin,qflag.prcp,qflag.snow,qflag.tmax,qflag.tmin,n.qflag.prcp,n.qflag.snow,n.qflag.tmax,n.qflag.tmin)

months = c('station','J','F','M','A','M','J','J','A','S','O','N','D')
ndm = c(31,28,31,30,31,30,31,31,30,31,30,31)

#Screen for temperature outliers --
#Calculate the z-scores of temperature for each site and month
ztmax = matrix(NA,length(tmax),1)
ztmin = matrix(NA,length(tmin),1)

for (s in 1:ns) {
	i = which(id == sites[s])
	for (m in 1:12) {	
		n = which(mo == m)
		ni = intersect(n,i)
		ztmax[ni] = (tmax[ni] - mean(tmax[ni],na.rm=T))/sd(tmax[ni],na.rm=T)
		ztmin[ni] = (tmin[ni] - mean(tmin[ni],na.rm=T))/sd(tmin[ni],na.rm=T)
		rm(ni,n)
	}
	rm(i,m)
}
rm(s)

#there are z-scores extenting to +/-10

#remove anything with abs(zscore) >5
tmax[which(abs(ztmax) >5)] = NA #236
tmin[which(abs(ztmin) >5)] = NA #480

#Screen for days on which tmax <tmin
a = which(tmax < tmin)
tmax[a] = NA
tmin[a] = NA
rm(a)

#screen for days on which there is snow recorded but not any precipitation
sd = which(snow >0)
wd = which(prcp >0)
dd = setdiff(sd,wd) #snow but no precip , 15958

#make all of the days in dd NA in snow
snow[dd] = NA
rm(sd,wd,dd)

#screen for days on which the temperatures are unreasonable for the type of precipitation recorded 
#tmax <-10 on days with rain and tmin >10 on days with snow 
sd = which(snow >0)
wd = which(prcp >0)
rd = setdiff(wd,sd) #rain = precip but no snow
too.cold = which(tmax < -10)   
too.warm = which(tmin > 10)
crd = intersect(rd,too.cold)  #9281
wsd = intersect(sd,too.warm)  #42
prcp[crd] = NA
snow[wsd] = NA
rm(sd,wd,rd,too.cold,too.warm, crd,wsd)

#now make sure that an NA in any variable makes for a NA in all variables
p = which(is.na(prcp))
s = which(is.na(snow))
ps = union(p,s)
x = which(is.na(tmax))
psx = union(ps,x)
n = which(is.na(tmin))
psxn = union(psx,n)   #598106
rm(p,s,x,n)
prcp[psxn] = NA
snow[psxn] = NA
tmax[psxn] = NA
tmin[psxn] = NA

#Find out how many stations are still good
#Find all data points associated with a given site
good.yrs.prcp = matrix(NA,ns,13)
good.yrs.snow = matrix(NA,ns,13)
good.yrs.tmax = matrix(NA,ns,13)
good.yrs.tmin = matrix(NA,ns,13)

good.yrs.prcp[,1] = sites
good.yrs.snow[,1] = sites
good.yrs.tmax[,1] = sites
good.yrs.tmin[,1] = sites

for (s in 1:ns) {
	i = which(id == sites[s])
	year = unique(yr[i])
	for (m in 1:12) {	
		n = which(mo == m)
		temp = matrix(NA,length(year),4)
		for (y in 1:(length(year))) {
			t = which(yr == year[y])
			h = intersect(i,n)
			g = intersect(h,t)
			if (length(which(!is.na(prcp[g]))) > (ndm[m]-3)) {
				temp[y,1] = 1}
			if (length(which(!is.na(snow[g]))) > (ndm[m]-3)) {
				temp[y,2] = 1}
			if (length(which(!is.na(tmax[g]))) > (ndm[m]-3)) {
				temp[y,3] = 1}
			if (length(which(!is.na(tmin[g]))) > (ndm[m]-3)) {
				temp[y,4] = 1}
			rm(t,g)
		} #close the year loop
		good.yrs.prcp[s,m+1] = length(which(!is.na(temp[,1])))
		good.yrs.snow[s,m+1] = length(which(!is.na(temp[,2])))
		good.yrs.tmax[s,m+1] = length(which(!is.na(temp[,3])))
		good.yrs.tmin[s,m+1] = length(which(!is.na(temp[,4])))
		rm(n,h,temp)
	} #close the month loop
	rm(year,i)
} #close the site loop
rm(s,m,y)

rm(ps,psx,psxn)
out = file('good_yrs_prcp.txt')
write.table(good.yrs.prcp,file = out,sep=',',col.names = months,row.names = F)

out = file('good_yrs_snow.txt')
write.table(good.yrs.snow,file = out,sep=',',col.names = months,row.names = F)

out = file('good_yrs_tmax.txt')
write.table(good.yrs.tmax,file = out,sep=',',col.names = months,row.names = F)

out = file('good_yrs_tmin.txt')
write.table(good.yrs.tmin,file = out,sep=',',col.names = months,row.names = F)

rm(good.yrs.prcp,good.yrs.snow,good.yrs.tmax,good.yrs.tmin,out)

		
save.image('C:\\Users\\smcafee4.UA\\Documents\\RainSnow\\ghcn_daily\\ak_ghcnd_screened3.RData')