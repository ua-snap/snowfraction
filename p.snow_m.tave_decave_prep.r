setwd('C:\\Users\\smcafee4.UA\\Documents\\RainSnow\\ghcn_daily')

#load in all data
load('ak_daily_prepped.RData')

setwd('C:\\Users\\smcafee4.UA\\Documents\\RainSnow\\psnow_tave')

#do any necessary cleaning up
rm(m,s,y,prcp,snow,tmax,tmin,tave,dtr,wetdays,drydays,snowydays,rainydays,mixdays,allsnowdays,id,mo,yr,year,ndm,nyr,xnzm1,xnza)

p.snow = n.snowdays/n.wetdays
p.mix = n.mixdays/n.wetdays
p.allsnow = n.allsnowdays/n.wetdays
p.trace_wet = n.trace_wetdays/n.wetdays
p.trace_allsnow = n.trace_allsnowdays/n.allsnowdays
save.image('m.tave_p.snow.RData')
