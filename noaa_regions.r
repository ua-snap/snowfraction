#load in a file on station location
locs = read.table('C:\\Users\\smcafee4.UA\\Documents\\RainSnow\\station_inventory\\ak_data_inventory.csv',sep=',',header = T)
#this is all the potential stations -- just get the ones being used
a = match(sites,locs[,1])
locs = locs[a,]
rm(a)
locs2 = locs

#note that longitudes are in +/- format -- need to turn them all into 360, and there are a few that are positive
a = which(locs[,4] <0)
locs[a,4] = 360+locs[a,4]
rm(a)

arctic = which(locs[,3] >68) #3 stations
west.stations = c('BETHEL','CAPE ROMANZOF','UNALAKLEET','NOME','WALES','KOTZEBUE')
nws = length(west.stations)
west = matrix(NA,nws,1)
for (i in 1:nws)  {
west[i] = pmatch(west.stations[i],locs[,2])}
rm(i,west.stations)

swisland.stations = c('ST PAUL','PORT HEIDEN','COLD BAY','DUTCH HARBOR','ADAK','ATTU','SHEMYA')
nsi =length(swisland.stations)
swisland = matrix(NA,nsi,1)
for (i in 1:nsi) {
swisland[i] = pmatch(swisland.stations[i],locs[,2])}
rm(i,swisland.stations)

swinterior.stations = c('DILLINGHAM','KING SALMON','ILIAMNA','PORT ALSWORTH','PUNTILLA','SKWENTNA','TALKEETNA')
nsw = length(swinterior.stations)
swinterior = matrix(NA,nsw,1)
for (i in 1:nsw) {
swinterior[i] = pmatch(swinterior.stations[i],locs[,2])}
rm(i,swinterior.stations)

inlet.stations = c('MOOSE PASS','COOPER LAKE','TONSINA','BIG RIVER LAKES','KASILOF','KENAI','ANCHORAGE INT','ANCHORAGE WB','ELMENDORF','MATANUSKA','GLEN ALPS','WHITES CROSSING','ALYESKA','EKLUTNA','BENS FARM','ANDERSON LAKE','WASILLA','PALMER','SUTTON','INTRICATE BAY')
nci = length(inlet.stations)
cookinlet = matrix(NA,nci,1)
for (i in 1:nci) {
cookinlet[i] = pmatch(inlet.stations[i],locs[,2])}
rm(i,inlet.stations)

sse.stations = c('KITOI','KODIAK','HOMER 8','HOMER AP','SEWARD','WHITTIER','CANNERY CREEK','VALDEZ','CORDOVA N','CORDOVA AP')
sse = which(locs[,4] >= 220)
nse1 = length(sse.stations)
nse2 = length(sse)
nse = nse1+nse2
sc = matrix(NA,nse1,1)
for (i in 1:nse1) {
sc[i] = pmatch(sse.stations[i],locs[,2])}
southcoast = c(sse,sc)
rm(sse.stations,sse,nse1,nse2,sc,i)

#everything else is interior
notint = c(west,swisland,swinterior,cookinlet,southcoast,arctic)
interior = setdiff(c(1:104),notint)
rm(notint)


rm(nse,nci,nws,nsi,nsw)