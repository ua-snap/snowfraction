setwd('C:\\Users\\smcafee4.UA\\Documents\\RainSnow\\ghcn_daily')

#Read in the GHCN file retrieved from the CDO.
#The format of these files, saved as csvs is 
#Col. 1: STATION as GHCND:COUNTRYNETWORKID; string
#Col. 2: YEAR
#Col. 3: MONTH
#Col. 4: DAY
#Col. 5: PRCP in tenths of mm; floating point number #f
#Col. 6: PRCP Measurement Flag; string #s
#Col. 7: PRCP Quality Flag; string #s
#Col. 8: PRCP Source Flag; string #s
#Col. 9: PRCP Time of Observations; unsigned integer #u
#Col. 10: SNOW in mm; floating point number #f
#Col. 11: SNOW Measurement Flag; string #s
#Col. 12: SNOW Quality Flag; string #s
#Col. 13: SNOW Source Flag; string #s
#Col. 14: SNOW Time of Observations; unsigned integer #u
#Col. 15: TMAX in tenths of degC; floating point numbe\r #f
#Col. 16: TMAX Measurement Flag; string #s
#Col. 17: TMAX Quality Flag; string #s
#Col. 18: TMAX Source Flag; string #s
#Col. 19: TMAX Time of Observations; unsigned integer #u
#Col. 20: TMIN in tenths of degC; floating point number #f
#Col. 21: TMIN Measurement Flag; string #s
#Col. 22: TMIN Quality Flag; string #s
#Col. 21: TMIN Source Flag; string #s
#Col. 22: TMIN Time of Observations; unsigned integer #u

#The following quality flags show up: D, G, I, K, L, M, N, O, R, S, T, W, X
#According to the README file, these flags are defined as
#D = failed duplicate check
#G =  failed gap check
#I =  failed internal consistency check
#K = failed streak/frequent-value check
#L = failed check on length of multi-day period
#M = failed magaconsistency check
#N = failed naught check
#O = failed climatological outlier check
#R = failed lagged range check
#S = failed spatial consistency check
#T = failed temporal consistency check
#W = temperature too warm for snow
#X = failed bounds check


#Create a list of batch numbers because the data were downloaded in batches of 25 stations

batch = c(18265,18271,18304,18314,18366,18369,18371,18372,22687,22689,22867)
nb = length(batch)

#Read in the csv files of daily data, combining into a large data matrix
data = read.csv(paste('batch',batch[1],'a.csv',sep=''),header = T,na.strings = 9999)
for (i in 2:nb) {
	dat = read.csv(paste('batch',batch[i],'a.csv',sep=''),header = T,na.strings = 9999)
	data = rbind(data,dat)
	rm(dat)
}
rm(i,nb,batch)


#pull out the first column of data and call it id, bringing it through a matrix, first to delete all of the old facto levels associated with i
#and then calling it a factor again
id = as.factor(as.matrix(data[,1]))

#Use the fact that R interprets this as a factor to get a listing of all the sites in this batch
sites = levels(id)
ns = length(sites)

#Extract the year and month
yr = data$YEAR
mo = data$MONTH
da = data$DAY

#Read in precip data, converting from tenths of mm to mm
prcp = 0.1*data$PRCP

#Read in measurement and quality flags associated with precipitation data
mflag.prcp = data$Measurement.Flag
qflag.prcp = data$Quality.Flag

#Read in snow data, already in mm
snow = data$SNOW

#Read in measurement and quality flags associated with precipitation data
mflag.snow = data$Measurement.Flag.1
qflag.snow = data$Quality.Flag.1

#Read in TMAX, converting from tneths of degrees C to degrees C
tmax = 0.1*data$TMAX

#Read in TMIN, converting from tenths of degrees C to degrees C
tmin = 0.1*data$TMIN

#Read in quality  and measurement flags associated with TMAX and TMIN
qflag.tmax = data$Quality.Flag.2
qflag.tmin = data$Quality.Flag.3
mflag.tmax = data$Measurement.Flag.2
mflag.tmin = data$Measurement.Flag.3

#DEAL WITH QUALITY FLAGS
n.qflag.prcp = table(qflag.prcp)
#D = 288 instances
#I = 102 instances
#K = 74 instances
#L = 214 instances
#O = 127 instances
#S = 12 instances
#G = 7 instances
#X = 1 instance
#N = 0 instances

#D -- DUPLICATE CHECK FLAG FOR PRECIPITATION
#this identifies duplicated months (i.e., Jan and Feb 1960 are the same or Jan 1960 and 1961 are the same).  
#These have to be deleted
f = which(qflag.prcp == 'D')
prcp[f] = NA
rm(f)

#I INTERNAL CONSISTENCY FLAG FOR PRECIPITATION
#As far as I can tell this corresponds to the snowfall to precipitation check and applies to days when snow >100 times prcp
#This could be ascribed to 1) undercatch or 2) error in either the prcp or snow data; 
#Since here the interest is in occurence and not amount,will ignore I flags in prcp and snow

#K - streak check for 20+ "consecutive identical non-zero, non-missing values) or frequent-value check for "5-9 identical moderate to heavy daily totals"

#L CHECK ON LENGTH OF MULTIDAY PERIOD

#O CLIMATOLOGICAL OUTLIER CHECK

#S SPATIAL CONSISTENCY CHECK

#G GAP CHECK FOR VALUES 300 MM GREATER THAN OTHER MONTHLY TOTALS

#X FAILED BOUNDS CHECK
f = which(qflag.prcp == 'X')
test = as.matrix(levels(droplevels(id[f]))) #imapcts one stations USC005025713
s = which(id == test[1])
sf = intersect(f,s)
check = data.frame(yr[sf],mo[sf],da[sf],prcp[sf])
#this is a day with 2032.5 mm precipitation on a single day in October.
#this is clearly a problem -- no one ever get 80" precip in a single day, but it did probably rain on that day, so I'll leave that in for the time being
#no snow is recorded
rm(f,test,s,sf,check)

#DEAL WITH QUALITY FLAGS ASSOCIATED WITH THE SNOW VARIABLE
n.qflag.snow = table(qflag.snow)
#     I       L       O       W       X       D       N 
#    2152     25      74      13      9      180      4 

#I'm not going to worry about I, L or O flags.  X isn't a huge concern, but I'll check for values <0 associated with X
#D and W flags should lead to NA  ; N flags modify mflag.snow

#D duplicate
f = which(qflag.snow == 'D')
snow[f] = NA
rm(f)

#N flags
f = which(qflag.snow == 'N')
mflag.snow[f] = ' '
rm(f)

#W (too warm for snow) flag
f = which(qflag.snow == 'W')
test = as.matrix(levels(droplevels(id[f]))) #13 occurences at 7 stations
snow[f] = NA
rm(f)

#QUALITY FLAGS ASSOCIATED WITH TMAX
n.qflag.tmax = table(qflag.tmax)

#D DUPLICATE FLAG
f = which(qflag.tmax =='D')
tmax[f] = NA
rm(f)

#G GAP TEST - all G flags lead to NA in Tmax
f = which(qflag.tmax =='G')
tmax[f] = NA
rm(f)

#I INTERNAL CONSISTENCY CHECK
#per the QC document, "checks for consistency among max, min, and tobs temperature within a 3-day window" 
#It could be, as described on http://cdiac.ornl.gov/epubs/ndp/ushcn/daily_doc.html#tempqc, as 
#"Checks for daily maximum temperatures that are less than the minimum temperatures on the preceding, current, and following days as well as for minimum temperatures that are greater than the maximum temperatures during the relevant three-day window."
#I'm not sure that this will be a huge problem; there are a number of stations where tmax and tmin are, in fact, very close, but will
#need to keep in mind that I didn't reject these data out of hand

#N NAUGHT CHECK == Tmax and Tmin are the same and 0C or -7.8C (0F) -- these all lead to NA
f = which(qflag.tmax =='N')
tmax[f] = NA
rm(f)

#O OUTLIER CHECK -- leave these in and run own outlier check

#S SPATIAL CONSISTENCY CHECK
#Leave these in b/c of concerns about the fact that stations may not be close by, the potential for inversions, etc.


#T TEMPORAL CONSISTENCY CHECK -- DOES T CHANGE BY >25C FROM ONE DAY TO THE NEXT?
#for the time being, I'll leave these in and remember that
#I've left them in and check for this in a personal Q


#X BOUNDS CHECK == these are all NA
f = which(qflag.tmax =='X')
tmax[f] = NA
rm(f)

#R LAGGED RANGE CHECK  -- leave these in and assess as part of personal QC
f = which(qflag.tmax == 'R') 
rm(f)

#QUALITY FLAGS ASSOCIATED WITH TMIN
n.qflag.tmin = table(qflag.tmin)

#As with Tmax,replace values flagged D, G, N, and X with NA.  Keep values flagged with I, O, S, T, R and investigate more closely if need be
#need to check on the 20 K flags, as none of those showed up in the tmax Qflags
f = which(qflag.tmin == 'D')
tmin[f] = NA
rm(f)
f = which(qflag.tmin == 'G')
tmin[f] = NA
rm(f)
f = which(qflag.tmin == 'N')
tmin[f] = NA
rm(f)
f = which(qflag.tmin == 'X')
tmin[f] = NA
rm(f)

#K, frequent-value flag check
f = which(qflag.tmin == 'K')
test = as.matrix(levels(droplevels(id[f]))) #impacts USC00509919 Wrangell Airport
check = data.frame(id[f],yr[f],mo[f],da[f],prcp[f],snow[f],tmax[f],tmin[f])
#for a 20-day stretch of days in October 1967, at Wrangell Airport, minimum temperature is 1.1C; for the time being I;ll let this stand
rm(f,test,check)


#DEAL WITH MEASUREMENT FLAGS
#PRECIPITATION
table(mflag.prcp)
#Measurement flags of T indicate trace precipitation; find occurences of trace precipitation and replace
#those in the precip data with 0.1 mm 
t = which(mflag.prcp == 'T')
prcp[t] = 0.1
rm(t)

#Measurement flags of P indicate "missing presumed zero"; find occurences of P flags and replace
#those in the precip data with 0
p = which(mflag.prcp == 'P')
prcp[p] = 0
rm(p)

#SNOW
#Measurement flags of T indicate trace precipitation; find occurences of trace precipitation and replace
#those in the snow data with 1 mm
table(mflag.snow)
t = which(mflag.snow == 'T')
snow[t] = 1
rm(t)

#Measurement flags of P indicate "missing presumed zero"; find occurences of P flags and replace
#those in the precip data with 0
p = which(mflag.snow == 'P')
snow[p] = 0
rm(p)

#TMAX
table(mflag.tmax)#only L flags show up

#TMIN
table(mflag.tmin)

#save the data
save.image('ak_ghcnd_screened2.RData')