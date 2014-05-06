#load in all data
load('C:\\Users\\smcafee4.UA\\Documents\\RainSnow\\ghcn_daily\\ak_ghcnd_screened3.RData')

#do any necessary cleaning up
rm(sites,ztmax,ztmin)

#read in a table tallying up the number of good years in any given month of data
use = as.matrix(read.table('C:\\Users\\smcafee4.UA\\Documents\\RainSnow\\ghcn_daily\\good_yrs_prcp.txt',sep=',',header = T))

#NA any months associated with <30 years in length
for (s in 1:ns) {
	for (m in 1:12) {
		if (use[s,(m+1)] <30) {
			i = which(id == use[s,1])
			n = which(mo == m)
			ni = intersect(i,n)
			prcp[ni] = NA
			snow[ni] = NA
			tmax[ni] = NA
			tmin[ni] = NA
			rm(i,n,ni)}}}


#Get rid of all rows that have no data
dat = which(!is.na(prcp))
id = id[dat]
yr = yr[dat]
mo = mo[dat]
prcp = prcp[dat]
snow = snow[dat]
tmax = tmax[dat]
tmin = tmin[dat]
rm(dat)

#now trim the use file to only those sites that have enough data to analyze at least one month
sites = as.matrix(levels(droplevels(id)))
ns = length(sites) #104

a = match(sites,use[,1])
use = use[a,]
use.sites = matrix(NA,ns,12)
for (m in 1:12) {
	use.sites[,m] = as.numeric(use[,(m+1)])}
rm(m)

row.names(use.sites) = use[,1]
rm(use,a)

#for each site and month, remove data for years for which there is less than 90% coverage
for (s in 1:ns) {
	x = which(id == sites[s])
	for (m in 1:12) {
	n = which(mo == m) #Identify days that fall in month m
	#get the values associated with that year and month
	xn = intersect(x,n)
	#find the number of years for which there are data
	year = unique(yr[xn])
	nyr = length(year) #how many year are there

	for (y in 1:nyr) {
		z = which(yr == year[y])
		#get data in year year[y] for station s and month m
		xnz = intersect(xn,z)
		nn = length(xnz)
		if (nn <=(ndm[m]-3)) {
			id[xnz] = NA
			yr[xnz] = NA
			mo[xnz] = NA
			prcp[xnz] = NA
			snow[xnz] = NA
			tmax[xnz] = NA
			tmin[xnz] = NA} #close if loop
		rm(nn,z)} #close year loop
	rm(year,nyr,xn,n) }#close month loop
rm(x)} #close year loop
rm(s,m,y)

#Get rid of all rows that have no data
dat = which(!is.na(prcp))
id = id[dat]
yr = yr[dat]
mo = mo[dat]
prcp = prcp[dat]
snow = snow[dat]
tmax = tmax[dat]
tmin = tmin[dat]
rm(dat)

#calculate dtr and average daily temperature
temp = cbind(tmax,tmin)
tave = apply(temp,1,mean,na.rm=T)
dtr = tmax-tmin
rm(temp)


#make variables that indicate a day was snowy (1) or not (0) and rainy (1) or not (0) and whether it had mixed precipitatin
#rainy means there was precipitation and NO snow
snowy = matrix(0,length(snow),1)
snowy[which(snow >0)] = 1
wet = matrix(0,length(prcp),1)
wet[which(prcp >0)] = 1
rainy = wet
rainy[snowy == 1] = 0

drydays = which(wet == 0)
rainydays = which(rainy == 1)
snowydays = which(snowy == 1)
wetdays = which(wet == 1)
rm(wet,snowy,rainy)

#let's try calling any day where precipitation >0.25*snow a mixed precip day
a = which(prcp >0.25*snow)
mixdays = intersect(a,snowydays)  #60555
rm(a)
allsnowdays = setdiff(snowydays,mixdays)
allraindays = setdiff(rainydays,mixdays)

#also try calling any day where precipitation >0.1*snow a mixed precip dat
a = which(prcp >0.10*snow)
mixdays10 = intersect(a,snowydays)  #60555
rm(a)
allsnowdays10 = setdiff(snowydays,mixdays10)
allraindays10 = setdiff(rainydays,mixdays10)

#for each site and month, find the maximum and minimum temperatures on days during which snow and rain are recorded
#daily tave at which rain
maxTsnow = matrix(NA,ns,12) 
minTrain = matrix(NA,ns,12)

#Also calculate annual monthly average temperatures, monthly total precipitation and snowfall
m.tave = array(NA,c(ns,12,100))
m.tmax = array(NA,c(ns,12,100))
m.tmin = array(NA,c(ns,12,100))
m.dtr = array(NA,c(ns,12,100))
m.prcp = array(NA,c(ns,12,100))
m.snow = array(NA,c(ns,12,100))
n.snowdays = array(NA,c(ns,12,100))
n.raindays = array(NA,c(ns,12,100))
n.wetdays = array(NA,c(ns,12,100))
n.mixdays = array(NA,c(ns,12,100))
n.mixdays10 = array(NA,c(ns,12,100))
n.allsnowdays = array(NA,c(ns,12,100))
n.allsnowdays10 = array(NA,c(ns,12,100))
n.allraindays = array(NA,c(ns,12,100))
n.trace_snowdays = array(NA,c(ns,12,100))
n.trace_allsnowdays = array(NA,c(ns,12,100))
n.trace_raindays = array(NA,c(ns,12,100))
n.trace_wetdays = array(NA,c(ns,12,100))
m.tave_snowdays = array(NA,c(ns,12,100))
m.tave_raindays = array(NA,c(ns,12,100))
m.tave_drydays = array(NA,c(ns,12,100))
m.tave_wetdays = array(NA,c(ns,12,100))
m.tave_mixdays = array(NA,c(ns,12,100))
m.tave_allsnowdays = array(NA,c(ns,12,100))
m.maxtave_snowdays = array(NA,c(ns,12,100))
m.mintave_raindays = array(NA,c(ns,12,100))
m.tave_allraindays = array(NA,c(ns,12,100))

for (s in 1:ns) {
	x = which(id == sites[s])
	for (m in 1:12) {#loop over months
		n = which(mo == m) #Identify days that fall in month m
		#get the values associated with that year and month
		xn = intersect(x,n)
		rd = intersect(rainydays,xn) #Find all rainy days at station s that fall in month m
		sd = intersect(snowydays,xn) #Find all snowy days at station s that fall in month m
		if (length(rd) >= 1 && length(sd) >= 1) {
			maxTsnow[s,m] = max(tmax[sd],na.rm=T)
			minTrain[s,m] = min(tmin[rd],na.rm=T)}
	 #find the number of years for which there are data
	 year = unique(yr[xn])
	 nyr = length(year) #how many year are there
	for (y in 1:nyr) {
		z = which(yr == year[y])
		xnz = intersect(xn,z)	  		#get data in year year[y] for station s and month m
		if (length(xnz) > (ndm[m]-3)) {
		m.tave[s,m,y] = mean(tave[xnz])
		m.tmax[s,m,y] = mean(tmax[xnz])
		m.tmin[s,m,y] = mean(tmin[xnz])
		m.dtr[s,m,y] = mean(dtr[xnz])
		m.prcp[s,m,y] = sum(prcp[xnz])
		m.snow[s,m,y] = sum(snow[xnz])
		xnzs = intersect(xnz,snowydays)
		xnzr = intersect(xnz,rainydays)
		xnzw = intersect(xnz,wetdays)
		xnzd = intersect(xnz,drydays)
		xnzm = intersect(xnz,mixdays)
		xnza = intersect(xnz,allsnowdays)
		xnzm1 = intersect(xnz,mixdays10)
		xnza1 = intersect(xnz,allsnowdays10)
		n.snowdays[s,m,y] = length(xnzs)
		n.raindays[s,m,y] = length(xnzr)
		n.wetdays[s,m,y] = length(xnzw)
		n.mixdays[s,m,y] = length(xnzm)
		n.allsnowdays[s,m,y] = length(xnza)
		n.mixdays10[s,m,y] = length(xnzm1)
		n.allsnowdays10[s,m,y] = length(xnza1)
    		  if (length(xnzs) >= 1) {
		    m.tave_snowdays[s,m,y] = mean(tave[xnzs])
		    m.maxtave_snowdays[s,m,y] = max(tave[xnzs])
        n.trace_snowdays[s,m,y] = length(which(snow[xnzs] == 0.01))}
		  rm(xnzs)
		  if (length(xnzr) >= 1) {
		    m.tave_raindays[s,m,y] = mean(tave[xnzr])
		    m.mintave_raindays[s,m,y] = min(tave[xnzr])
        n.trace_raindays[s,m,y] = length(which(prcp[xnzr] == 1))}
		    rm(xnzr)
      if (length(xnzw) >= 1) {
        m.tave_wetdays[s,m,y] = mean(tave[xnzw])
        n.trace_wetdays[s,m,y] = length(which(prcp[xnzw] == 1))}
      rm(xnzw)
      if (length(xnzd) >= 1) {
        m.tave_drydays[s,m,y] = mean(tave[xnzd])}
      rm(xnzd)
		  if (length(xnzm) >= 1) {
		    m.tave_mixdays[s,m,y] = mean(tave[xnzm])}
      rm(xnzm)
		  if (length(xnza) >= 1) {
		    m.tave_allsnowdays[s,m,y] = mean(tave[xnza])
        n.trace_allsnowdays[s,m,y] = length(which(snow[xnza] == 0.1))}
      rm(xnza)}   # close ndm if loop
            rm(xnz,z)} #close year loop
		rm(xn,rd,sd,n) }#close month loop
		rm(x)
} #close site loop

save.image('ak_daily_prepped.RData')