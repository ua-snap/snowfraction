setwd("C:/Users/smcafee4.UA/Documents/RainSnow/psnow_tave_regions2")
library(nls2)
library(fields)

cols = c('lightblue','cyan','darkgreen','green','yellowgreen','yellow','orange','red','pink','magenta','purple','blue')
months = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

load('resampled_10yraves.RData')

setwd("C:/Users/smcafee4.UA/Documents/RainSnow/psnow_tave_regions2")

#now set up matrices to hold the output for each method
b1= matrix(NA,500,1)
b2= matrix(NA,500,1)
p1= matrix(NA,500,1)
p2 = matrix(NA,500,1)

#run the regression 500 times using 1 replicate each

#ARCTIC
for ( i in 1:500) {
temp1 = as.vector(mtave.10.arctic[,,i])
temp2 = as.vector(pallsnow.10.arctic[,,i])

a = intersect(which(!is.na(temp1)),which(!is.na(temp2)))
clim = data.frame(cbind(temp1[a],temp2[a]))
colnames(clim) = c('tave','psnow')

fit.start =glm(psnow~tave,data = clim,family=gaussian(link = 'logit'))
b = fit.start$coefficients
fit = nls2(psnow ~ 1/(1+exp(-(b1+b2*tave))), start = list(b1=b[1],b2=b[2]),data=clim,trace=F,na.action=na.exclude)

b1[i] = round(summary(fit)$coefficients[1,1],3)
b2[i] = round(summary(fit)$coefficients[2,1],3)
p1[i] = round(summary(fit)$coefficients[1,4],3)
p2[i] = round(summary(fit)$coefficients[2,4],3)
rm(fit,fit.start,b,a,clim,temp1,temp2)}

coefs.arctic = cbind(b1,b2,p1,p2)



save(coefs.arctic,file='coefs.arcticB.RData')
                                                                    