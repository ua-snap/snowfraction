library(nls2)
#Use the first model
setwd("C:/Users/smcafee4.UA/Documents/RainSnow/psnow_tave_regions2")
cols = c('lightblue','cyan','darkgreen','green','yellowgreen','yellow','orange','red','pink','magenta','purple','blue')
months = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

load('resampled_10yraves.RData')

setwd("C:/Users/smcafee4.UA/Documents/RainSnow/psnow_tave_regions2")


#WEST
b1= matrix(NA,500,2)
b2= matrix(NA,500,2)
p1= matrix(NA,500,2)
p2 = matrix(NA,500,2)
mos1 = c(1,2,9,10,11,12) 
mos2 = c(3:8)

for (i in 1:500) {
#SONDJF
temp1 = as.vector(mtave.10.cookinlet[,mos1,i])
temp2 = as.vector(pallsnow.10.cookinlet[,mos1,i])
a = intersect(which(!is.na(temp1)),which(!is.na(temp2)))
clim = data.frame(cbind(temp1[a],temp2[a]))
colnames(clim) = c('tave','psnow')

fit.start =glm(psnow~tave,data = clim,family=gaussian(link = 'logit'))
b = fit.start$coefficients

fit = nls2(psnow ~ 1/(1+exp(-(b1+b2*tave))), start = list(b1=b[1],b2=b[2]),data=clim,trace=F,na.action=na.exclude)

b1[i,1] = round(summary(fit)$coefficients[1,1],3)
b2[i,1] = round(summary(fit)$coefficients[2,1],3)
p1[i,1] = round(summary(fit)$coefficients[1,4],3)
p2[i,1] = round(summary(fit)$coefficients[2,4],3)
rm(fit,fit.start,b,a,clim,temp1,temp2)  

#MAMJJA
temp1 = as.vector(mtave.10.cookinlet[,mos2,i])
temp2 = as.vector(pallsnow.10.cookinlet[,mos2,i])
a = intersect(which(!is.na(temp1)),which(!is.na(temp2)))
clim = data.frame(cbind(temp1[a],temp2[a]))
colnames(clim) = c('tave','psnow')

fit.start =glm(psnow~tave,data = clim,family=gaussian(link = 'logit'))
b = fit.start$coefficients

fit = nls2(psnow ~ 1/(1+exp(-(b1+b2*tave))), start = list(b1=b[1],b2=b[2]),data=clim,trace=F,na.action=na.exclude,control = nls.control(maxiter=500,warnOnly=TRUE))

b1[i,2] = round(summary(fit)$coefficients[1,1],3)
b2[i,2] = round(summary(fit)$coefficients[2,1],3)
p1[i,2] = round(summary(fit)$coefficients[1,4],3)
p2[i,2] = round(summary(fit)$coefficients[2,4],3)
rm(fit,fit.start,b,a,clim,temp1,temp2)
}

coefs1.cookinlet= cbind(b1[,1],b2[,1],p1[,1],p2[,1])
coefs2.cookinlet= cbind(b1[,2],b2[,2],p1[,2],p2[,2])
save(list=c('coefs1.cookinlet','coefs2.cookinlet'),file='coefs.cookinletB.RData')
