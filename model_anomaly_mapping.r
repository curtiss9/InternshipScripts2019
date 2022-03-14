library(ncdf4)
library(RColorBrewer)
library(rgdal)
library(raster)
library(rasterVis)
library(data.table)


coul = brewer.pal(10, "RdBu")   # load palette
coul = colorRampPalette(coul)(40)   # add more colors to palette

coast_shapefile = "C:/Users/sabrina.curtis/Documents/coastlines/ne_110m_coastline.shp"  # load coastline shapefile
layer <- ogrListLayers(coast_shapefile)
coast_lines <- readOGR(coast_shapefile, layer=layer)

##### LOVECLIM #####

ncfile = "C:\\Users\\sabrina.curtis\\Desktop\\Data\\models\\loveclim_mhdiff.nc"   # Load model
ncin <- nc_open(ncfile)

loveclim <- ncvar_get(ncin,"mhdiff")

# Dimensions
lonL <- ncvar_get(ncin,"longitude")
for (i in seq_along(lonL)){
    if (lonL[i] > 180){
        lonL[i] = lonL[i]-360
    }
}
latL <- ncvar_get(ncin,"latitude")
dim(loveclim)
nmodel = dim(loveclim)[1]

nc_close(ncin)

grid <- expand.grid(lon=lonL, lat=latL)
cutpts <- c(-5,-4,-3,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,3,4,5)
plt <- levelplot(loveclim ~ lon * lat, data=grid, at=cutpts, cuts=20, pretty=T, xlab = "Longitude", ylab = "Latitude", 
                 main = "LOVECLIM Transient Model", col.regions=rev(coul), 
                 scales=list(x=list(at=c(-180,-90,0,90,179),labels=c('180°','90°W','0°','90°E','180°'),cex=1), 
                 y=list(at=c(-90,-45,0,45,89),labels=c('90°S','45°S',"0°",'45°N','90°N'),cex=1)),
                 colorkey=list(at=seq(-10, 10, 1), labels=list(at=c(-10,-8,-6,-4,-2,0,2,4,6,8,10), 
                 labels=c('-10','-8','-6','-4','-2','0','2','4','6','8','10'), cex=1)), aspect="iso")
plt + layer(sp.lines(coast_lines, col="black", lwd=0.5))


##### FAMOUS #####

ncfile = "C:\\Users\\sabrina.curtis\\Desktop\\Data\\models\\famous_mhdiff.nc"   # Load model
ncin <- nc_open(ncfile)

famous <- ncvar_get(ncin,"mhdiff")

# Dimensions
lonF <- ncvar_get(ncin,"longitude")
for (i in seq_along(lonF)){
    if (lonF[i] > 180){
        lonF[i] = lon[i]-360
    }
}
latF <- ncvar_get(ncin,"latitude")
dim(famous)
nmodel = dim(famous )[1]

nc_close(ncin)

grid <- expand.grid(lon=lonF, lat=latF)
cutpts <- c(-5,-4,-3,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,3,4,5)
plt <- levelplot(famous ~ lon * lat, data=grid, at=cutpts, cuts=20, pretty=T, xlab = "Longitude", ylab = "Latitude", 
                 main = "FAMOUS Transient Model", col.regions=rev(coul), 
                 scales=list(x=list(at=c(-180,-90,0,90,179),labels=c('180°','90°W','0°','90°E','180°'),cex=1), 
                 y=list(at=c(-90,-45,0,45,89),labels=c('90°S','45°S',"0°",'45°N','90°N'),cex=1)),
                 colorkey=list(at=seq(-5, 5, 0.5), labels=list(at=c(-4,-2,0,2,4), 
                 labels=c('-4','-2','0','2','4'), cex=1)), aspect="iso")
plt + layer(sp.lines(coast_lines, col="black", lwd=0.5))


##### TRACE #####

ncfile = "C:\\Users\\sabrina.curtis\\Desktop\\Data\\models\\trace_mhdiff.nc"   # Load model
ncin <- nc_open(ncfile)

trace <- ncvar_get(ncin,"mhdiff")

# Dimensions
lonT <- ncvar_get(ncin,"longitude")
for (i in seq_along(lonT)){
    if (lonT[i] > 180){
        lonT[i] = lonT[i]-360
    }
}
latT <- ncvar_get(ncin,"latitude")
dim(trace)
nmodel = dim(trace)[1]

nc_close(ncin)

grid <- expand.grid(lon=lonT, lat=latT)
cutpts <- c(-5,-4,-3,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,3,4,5)
plt <- levelplot(trace ~ lon * lat, data=grid, at=cutpts, cuts=20, pretty=T, xlab = "Longitude", ylab = "Latitude", 
                 main = "TRACE Transient Model", col.regions=rev(coul), 
                 scales=list(x=list(at=c(-180,-90,0,90,179),labels=c('180°','90°W','0°','90°E','180°'),cex=1), 
                 y=list(at=c(-60,-30,0,30,60),labels=c('60°S','30°S',"0°",'30°N','60°N'),cex=1)),
                 colorkey=list(at=seq(-10, 10, 1), labels=list(at=c(-10,-8,-6,-4,-2,0,2,4,6,8,10), 
                 labels=c('-10','-8','-6','-4','-2','0','2','4','6','8','10'), cex=1)), aspect="iso")
plt + layer(sp.lines(coast_lines, col="black", lwd=0.5))


##### Export as CSVs #####

#write.csv(loveclim, "C:\\Users\\sabrina.curtis\\Desktop\\Data\\exported\\loveclim.csv")
#write.csv(famous, "C:\\Users\\sabrina.curtis\\Desktop\\Data\\exported\\famous.csv")
#write.csv(trace, "C:\\Users\\sabrina.curtis\\Desktop\\Data\\exported\\trace.csv")


##### Plot Proxy Data over Models #####

proxy <- read.csv("C:\\Users\\sabrina.curtis\\Desktop\\Data\\exported\\temp_anomalies.csv", row.names = 1)
proxy <- data.table(proxy)

# Prepare data for color assignment to temp.
proxy$order <- findInterval(proxy$Temp, sort(proxy$Temp))

cols <- rev(brewer.pal(10, "RdBu"))
cols <- colorRampPalette(cols)(40)

# Plot the data points
points(proxy$Lon, proxy$Lat, pch = 20, col = cols[proxy$order])


##### RMSE For TRACE Model #####

## By Latitude ##

# average down a column (latitude)
latMeans <- colMeans(trace)
latMeansModel <- data.frame(Lat = latT, Temp = latMeans)

# average the temperatures based on latitude
latMeansProxy <- aggregate(Temp ~ Lat, data = proxy, FUN = mean)

# create a new column to hold the latitude bin information
latMeansModel$LatBin <- -999
latMeansProxy$LatBin <- -999

for(i in seq(-90, 90, 5)){
	# Assign the latitudes to a 5 degree bin
	latMeansModel$LatBin <- ifelse(latMeansModel$Lat >= i & latMeansModel$Lat < i+5, i, latMeansModel$LatBin)
	latMeansProxy$LatBin <- ifelse(latMeansProxy$Lat >= i & latMeansProxy$Lat < i+5, i, latMeansProxy$LatBin)
}

# average the temperatures based on 5 degree latitude bins
latMeansModel <- aggregate(Temp ~ LatBin, data = latMeansModel, FUN = mean)
latMeansProxy <- aggregate(Temp ~ LatBin, data = latMeansProxy, FUN = mean)

# Sanity checks
barplot(latMeansModel$Temp, horiz=TRUE, main='model')
barplot(latMeansProxy$Temp, horiz=TRUE, main='proxy')
length(latMeansModel$Lat)
length(latMeansProxy$Lat)

# lengths not equal, so remove latitudes that are not in both data frames
latMeansModel$matchRows <- match(latMeansModel$LatBin, latMeansProxy$LatBin)
latMeansModel <- na.omit(latMeansModel)
latMeansProxy$matchRows <- match(latMeansProxy$LatBin, latMeansModel$LatBin)
latMeansProxy <- na.omit(latMeansProxy)

# Calculate the RMSE (root mean square error)
rmse <- sqrt( (latMeansModel$Temp - latMeansProxy$Temp)^2 )

barplot(rmse, names.arg = latMeansModel$LatBin, 
	xlab = "Latitude", ylab = "Root Mean Square Error", col = "blue")

# put into a format for comparison with other models
rmseTrace <- data.frame(rmseT = rmse, Lat = latMeansModel$LatBin)


##### RMSE For LOVECLIM Model #####

## By Latitude ##

# average down a column (average over a latitude)
latMeans <- colMeans(loveclim)
latMeansModel <- data.frame(Lat = latL, Temp = latMeans)

# average the temperatures based on latitude
latMeansProxy <- aggregate(Temp ~ Lat, data = proxy, FUN = mean)

# create a new column to hold the latitude bin information
latMeansModel$LatBin <- -999
latMeansProxy$LatBin <- -999

for(i in seq(-90, 90, 5)){
	# Assign the latitudes to a 5 degree bin
	latMeansModel$LatBin <- ifelse(latMeansModel$Lat >= i & latMeansModel$Lat < i+5, i, latMeansModel$LatBin)
	latMeansProxy$LatBin <- ifelse(latMeansProxy$Lat >= i & latMeansProxy$Lat < i+5, i, latMeansProxy$LatBin)
}

# average the temperatures based on 5 degree latitude bins
latMeansModel <- aggregate(Temp ~ LatBin, data = latMeansModel, FUN = mean)
latMeansProxy <- aggregate(Temp ~ LatBin, data = latMeansProxy, FUN = mean)

# Sanity checks
barplot(latMeansModel$Temp, horiz=TRUE, main='model')
barplot(latMeansProxy$Temp, horiz=TRUE, main='proxy')
length(latMeansModel$Lat)
length(latMeansProxy$Lat)

# lengths not equal, so remove latitudes that are not in both data frames
latMeansModel$matchRows <- match(latMeansModel$LatBin, latMeansProxy$LatBin)
latMeansModel <- na.omit(latMeansModel)
latMeansProxy$matchRows <- match(latMeansProxy$LatBin, latMeansModel$LatBin)
latMeansProxy <- na.omit(latMeansProxy)

# Calculate the RMSE (root mean square error)
rmse <- sqrt( (latMeansModel$Temp - latMeansProxy$Temp)^2 )

barplot(rmse, names.arg = latMeansModel$LatBin, 
	xlab = "Latitude", ylab = "Root Mean Square Error", col = "blue")

# put into a format for comparison with other models
rmseLoveclim <- data.frame(rmseL = rmse, Lat = latMeansModel$LatBin)


##### RMSE For FAMOUS Model #####

## By Latitude ##

# average down a column (latitude)
latMeans <- colMeans(famous)
latMeansModel <- data.frame(Lat = latF, Temp = latMeans)

# average the temperatures based on latitude
latMeansProxy <- aggregate(Temp ~ Lat, data = proxy, FUN = mean)

# create a new column to hold the latitude bin information
latMeansModel$LatBin <- -999
latMeansProxy$LatBin <- -999

for(i in seq(-90, 90, 5)){
	# Assign the latitudes to a 5 degree bin
	latMeansModel$LatBin <- ifelse(latMeansModel$Lat >= i & latMeansModel$Lat < i+5, i, latMeansModel$LatBin)
	latMeansProxy$LatBin <- ifelse(latMeansProxy$Lat >= i & latMeansProxy$Lat < i+5, i, latMeansProxy$LatBin)
}

# average the temperatures based on 5 degree latitude bins
latMeansModel <- aggregate(Temp ~ LatBin, data = latMeansModel, FUN = mean)
latMeansProxy <- aggregate(Temp ~ LatBin, data = latMeansProxy, FUN = mean)

# Sanity checks
barplot(latMeansModel$Temp, horiz=TRUE, main='model')
barplot(latMeansProxy$Temp, horiz=TRUE, main='proxy')
length(latMeansModel$Lat)
length(latMeansProxy$Lat)

# lengths not equal, so remove latitudes that are not in both data frames
latMeansModel$matchRows <- match(latMeansModel$LatBin, latMeansProxy$LatBin)
latMeansModel <- na.omit(latMeansModel)
latMeansProxy$matchRows <- match(latMeansProxy$LatBin, latMeansModel$LatBin)
latMeansProxy <- na.omit(latMeansProxy)

# Calculate the RMSE (root mean square error)
rmse <- sqrt( (latMeansModel$Temp - latMeansProxy$Temp)^2 )

barplot(rmse, names.arg = latMeansModel$LatBin, 
	xlab = "Latitude", ylab = "Root Mean Square Error", col = "blue")

# put into a format for comparison with other models
rmseFamous <- data.frame(rmseF = rmse, Lat = latMeansModel$LatBin)


##### Compare RMSEs #####

rmseAll <- data.frame(Lat = seq(-90,85,5))

rmseAll <- merge(rmseAll, rmseTrace, by = "Lat", all.x = TRUE)
rmseAll <- merge(rmseAll, rmseLoveclim, by = "Lat", all.x = TRUE)
rmseAll <- merge(rmseAll, rmseFamous, by = "Lat", all.x = TRUE)

# For plotting purposes, NAs are being replaced by a negative number
rmseAll[is.na(rmseAll)] <- -0.1

# get into a matrix format for plotting
rmseAll <- as.matrix(rmseAll)
rownames(rmseAll) <- rmseAll[,1]
rmseAll <- rmseAll[,-1]
rmseAll <- t(rmseAll)

moreCols <- brewer.pal(3, "Dark2")

barplot(rmseAll, beside=T,
	main = "Root Mean Square Errors by Latitude", 
	col = moreCols, 
	legend = c('TRACE','LOVECLIM','FAMOUS'), args.legend = list(x = "topleft"),
	ylab = "RMSE", xlab = 'Latitude'
	)
mtext("(A negative RMSE indicates a missing value.)")


##### Test the Colors for Colorblind Readability #####

library(dichromat)

barplot(rmseAll, beside=T,
	main = "Root Mean Square Errors by Latitude", 
	col = dichromat(moreCols, type = "deutan"), 
	legend = c('TRACE','LOVECLIM','FAMOUS'), args.legend = list(x = "topleft"),
	ylab = "RMSE", xlab = 'Latitude'
	)
mtext("(A negative RMSE indicates a missing value.)")





##### Average #####

# have to have the same dimensions for this to work
avg_temp <- (loveclim + famous + trace) / 3

grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-10,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,10)
plt <- levelplot(avg_temp ~ lon * lat, data=grid, at=cutpts, cuts=20, pretty=T, xlab = "Longitude", ylab = "Latitude", 
                 main = "TRACE Transient Model", col.regions=rev(coul), 
                 scales=list(x=list(at=c(-180,-90,0,90,179),labels=c('180°','90°W','0°','90°E','180°'),cex=1), 
                 y=list(at=c(-90,-45,0,45,89),labels=c('90°S','45°S',"0°",'45°N','90°N'),cex=1)),
                 colorkey=list(at=seq(-10, 10, 1), labels=list(at=c(-10,-8,-6,-4,-2,0,2,4,6,8,10), 
                 labels=c('-10','-8','-6','-4','-2','0','2','4','6','8','10'), cex=1)), aspect="iso")
plt + layer(sp.lines(coast_lines, col="black", lwd=0.5))




