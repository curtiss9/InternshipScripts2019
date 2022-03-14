library(ncdf4)
library(RColorBrewer)
library(rgdal)
library(raster)
library(rasterVis)
library(data.table)
library(ggplot2)


coul = brewer.pal(10, "RdBu")   # load palette
coul = colorRampPalette(coul)(40)   # add more colors to palette

coast_shapefile = "~/Desktop/Data_and_Scripts/Data/ne_110m_coastline/ne_110m_coastline.shp"  # load coastline shapefile
layer <- ogrListLayers(coast_shapefile)
coast_lines <- readOGR(coast_shapefile, layer=layer)

##### LOVECLIM #####

ncfile = "~/Desktop/Data_and_Scripts/Data/models/loveclim_mhdiff_4deg.nc"   # Load model
ncin <- nc_open(ncfile)

loveclim <- ncvar_get(ncin,"temp4")

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
                 scales=list(x=list(at=c(-180,-90,0,90,179),labels=c('180?','90?W','0?','90?E','180?'),cex=1), 
                 y=list(at=c(-90,-45,0,45,89),labels=c('90?S','45?S',"0?",'45?N','90?N'),cex=1)),
                 colorkey=list(at=seq(-10, 10, 1), labels=list(at=c(-10,-8,-6,-4,-2,0,2,4,6,8,10), 
                 labels=c('-10','-8','-6','-4','-2','0','2','4','6','8','10'), cex=1)), aspect="iso")
plt + layer(sp.lines(coast_lines, col="black", lwd=0.5))


##### FAMOUS #####

ncfile = "~/Desktop/Data_and_Scripts/Data/models/famous_mhdiff_4deg.nc"   # Load model
ncin <- nc_open(ncfile)

famous <- ncvar_get(ncin,"temp4")

# Dimensions
lonF <- ncvar_get(ncin,"longitude")
for (i in seq_along(lonF)){
    if (lonF[i] > 180){
        lonF[i] = lonF[i]-360
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
                 scales=list(x=list(at=c(-180,-90,0,90,179),labels=c('180?','90?W','0?','90?E','180?'),cex=1), 
                 y=list(at=c(-90,-45,0,45,89),labels=c('90?S','45?S',"0?",'45?N','90?N'),cex=1)),
                 colorkey=list(at=seq(-5, 5, 0.5), labels=list(at=c(-4,-2,0,2,4), 
                 labels=c('-4','-2','0','2','4'), cex=1)), aspect="iso")
plt + layer(sp.lines(coast_lines, col="black", lwd=0.5))


##### TRACE #####

ncfile = "~/Desktop/Data_and_Scripts/Data/models/trace_mhdiff_4deg.nc"   # Load model
ncin <- nc_open(ncfile)

trace <- ncvar_get(ncin,"temp4")

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


##### Average #####

#reformat famous and trace matrices for plotting
traceEast <- trace[c(1:45),]
traceWest <- trace[c(46:90),]
trace <- rbind(traceWest, traceEast)

famousEast <- famous[c(1:45),]
famousWest <- famous[c(46:90),]
famous <- rbind(famousWest, famousEast)

# have to have the same dimensions for this to work
avg_temp <- (loveclim + famous + trace) / 3

# some rows near 0 longitude are NA in the models; making the average fail for those rows
# remove those rows
avg_temp <- avg_temp[-c(44,45),]
lonNaRemoved <- lonL[-c(44,45)]

grid <- expand.grid(lon=lonNaRemoved, lat=latL)
cutpts <- c(-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-0.5,0.0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
plt <- levelplot(avg_temp ~ lon * lat, data=grid, at=cutpts, cuts=20, pretty=T, xlab = "Longitude", ylab = "Latitude", 
                 main = "Mean of Three Transient Climate Models", col.regions=rev(coul), 
                 scales=list(x=list(at=c(-180,-90,0,90,179),labels=c('180°','90°W','0°','90°E','180°'),cex=1), 
                 y=list(at=c(-90,-45,0,45,89),labels=c('90°S','45°S',"0°",'45°N','90°N'),cex=1)),
                 colorkey=list(at=seq(-5, 5, 0.5), labels=list(at=c(-5,-2.5,0,2.5,5), 
                 labels=c('-5','-2.5','0','2.5','5'), cex=1)), aspect="iso")
plt + layer(sp.lines(coast_lines, col="black", lwd=0.5))

#dev.copy(png, 'C:\\Users\\sabrina.curtis\\Desktop\\Maps\\models_averaged4x4.png')
#dev.off()


##### Proxy Data #####

proxy <- read.csv("~/Desktop/Data_and_Scripts/Data/exported/temp_anomalies_annual_v4.csv", row.names = 1)
proxy <- data.table(proxy)


##### RMSE For Average of 3 Models (Annual Mean Temps) #####

## By Latitude ##

# average model by latitude while correcting for the shape of the Earth
lat = seq(-89,89,2)  # if needed, create 1-D latitude array with specified latitude spacing
coslat = cos(lat*pi/180)  # calculate cosines of latitudes
cosweight= mean(coslat)  # cosmean is average of cosine of latitude weights  
weightModel = sweep(avg_temp,2,coslat/cosweight,FUN="*") # trace is 2-D temperature array (lat x lon)

# Remove NA rows and columns
weightModel <- weightModel[,-c(1,90)]
latNAremoved <- latL[-c(1,90)]

# average down a column (average over weighted latitudes)
latMeans <- colMeans(weightModel)
latMeansModel <- data.frame(Lat = latNAremoved, Temp = latMeans)

# average the temperatures based on latitude
latMeansProxy <- aggregate(Temp ~ Lat, data = proxy, FUN = mean)

# create a new column to hold the 4 degree latitude bin information
latMeansModel$LatBinFour <- -999
latMeansProxy$LatBinFour <- -999
for(i in seq(-90, 90, 4)){
  # Assign the latitudes to a 15 degree bin
  latMeansModel$LatBinFour <- ifelse(latMeansModel$Lat >= i & latMeansModel$Lat < i+4, i, latMeansModel$LatBinFour)
  latMeansProxy$LatBinFour <- ifelse(latMeansProxy$Lat >= i & latMeansProxy$Lat < i+4, i, latMeansProxy$LatBinFour)
}

### At this point the proxies and models are stored as mean temp anomalies per 4 degree latitude bands
### Calculate the differences here
rmse <- merge(latMeansProxy, latMeansModel, by = 'LatBinFour', all.x=TRUE)
names(rmse) <- c("LatBinFour", "LatProxy","TempProxy","LatModel","TempModel")

rmse$difference <- (rmse$TempModel - rmse$TempProxy)

# this data frame will have no averaging; it is intended to keep track of the individual sites so they can be counted by 15 lat bands later
# there is an easier way to do this, but this is quicker to write with my existing code
latCountProxy <- proxy

# create a new column to hold the 15 degree latitude bin information
latMeansModel$LatBin <- -999
latMeansProxy$LatBin <- -999
rmse$LatBin15 <- -999
latCountProxy$LatBin <- -999

for(i in seq(-90, 90, 15)){
	# Assign the latitudes to a 15 degree bin
	latMeansModel$LatBin <- ifelse(latMeansModel$Lat >= i & latMeansModel$Lat < i+15, i, latMeansModel$LatBin)
	latMeansProxy$LatBin <- ifelse(latMeansProxy$Lat >= i & latMeansProxy$Lat < i+15, i, latMeansProxy$LatBin)
	rmse$LatBin15 <- ifelse(rmse$LatBinFour >= i & rmse$LatBinFour < i+15, i, rmse$LatBin15)
	latCountProxy$LatBin <- ifelse(latCountProxy$Lat >= i & latCountProxy$Lat < i+15, i, latCountProxy$LatBin)
}

# Get count of how many sites in each 15 degree latitude band
as.data.frame(table(latCountProxy$LatBin))

# average the temperatures based on 15 degree latitude bins
latMeansModel <- aggregate(Temp ~ LatBin, data = latMeansModel, FUN = mean)
latMeansProxy <- aggregate(Temp ~ LatBin, data = latMeansProxy, FUN = mean)

#average the differences by 15 degree lat bins
rmse <- aggregate(difference ~ LatBin15, data = rmse, FUN = mean)
# take the square and root of them
rmse$Error <- sqrt( rmse$difference^2 )

# lengths not equal, so remove latitudes that are not in both data frames
latMeansModel$matchRows <- match(latMeansModel$LatBin, latMeansProxy$LatBin)
latMeansModel <- na.omit(latMeansModel)
latMeansProxy$matchRows <- match(latMeansProxy$LatBin, latMeansModel$LatBin)
latMeansProxy <- na.omit(latMeansProxy)

# Calculate the RMSE (root mean square error)
### This is incorrect ###
#rmse <- data.frame('LatBin' = latMeansModel$LatBin, 'ModelTemp' = latMeansModel$Temp, 'ProxyTemp' = latMeansProxy$Temp)
#rmse$Error <- sqrt( (latMeansModel$Temp - latMeansProxy$Temp)^2 )

# Match color palette from other maps/plots
blueList <- brewer.pal(7, "RdBu")
blue <- blueList[7]

# add column for y-axis labels
rmse$Labels <- c('90°S-75°S','75°S-60°S','60°S-45°S','45°S-30°S','30°S-15°S','15°S-0°',
                 '0°-15°N','15°N-30°N','30°N-45°N','45°N-60°N','60°N-75°N','75°N-90°N')

#plot as barplot
plt <- ggplot(data = rmse, aes(x = Labels, y = Error)) + 
  geom_bar(stat = 'identity', color = blue, fill = blue) + 
  labs(title = "Root Mean Square Error by Latitude",
       x = "Latitude") + scale_x_discrete(limits = rmse$Labels) + 
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),  
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size=24))
plt + coord_flip()

# barplot(rmse, main = "Root Mean Square Error\n(Models vs Reconstructed Temperatures)", 
# 	xlab = "Root Mean Square Error", ylab = "Latitude (Degrees North)", col = blue,
# 	horiz=TRUE, names.arg = latMeansModel$LatBin, las=1, width = 5)

### Bias
rmse$Bias <- latMeansModel$Temp - latMeansProxy$Temp

#plot as barplot
plt <- ggplot(data = rmse, aes(x = Labels, y = Bias)) + 
  geom_bar(stat = 'identity', color = blue, fill = blue) + 
  labs(title = "Bias by Latitude",
       x = "Latitude", y = "Bias (°C)") + 
  ylim(-2.25,1.5) + scale_x_discrete(limits = rmse$Labels) + 
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),  
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size=24)) #+ 
  #theme(axis.text.x = element_text(angle = 35, hjust = 1))
plt + coord_flip()


##### Compare global average RMSE between proxy types #####

#take average

# correct for the shape of the Earth
lat = seq(-89,89,2)  # if needed, create 1-D latitude array with specified latitude spacing
coslat = cos(lat*pi/180)  # calculate cosines of latitudes
cosweight= mean(coslat)  # cosmean is average of cosine of latitude weights  
weightModel = sweep(avg_temp,2,coslat/cosweight,FUN="*") # avg_temp is 2-D temperature array (lat x lon)

# Remove NA rows and columns
weightModel <- weightModel[,-c(1,45)]

#average first by latitude then by longitude
globalMeanModel <- colMeans(weightModel)
globalMeanModel <- mean(globalMeanModel)

# get count of number of unique sites per proxy
counts <- as.data.frame(table(proxy$Proxy))
names(counts) <- c("Proxy", "Count")

# take global mean for each proxy type
globalMeanProxy <- proxy[, list(Mean_Age = mean(Mean_Age),
                                  Temp = mean(Temp)), by = Proxy]

# remove the chironomids
globalMeanProxy <- globalMeanProxy[-c(12),]

#compare to average temp given by each proxy type
# Calculate the RMSE (root mean square error)
rmseProxy <- globalMeanProxy
rmseProxy$RMSE <- sqrt( (globalMeanModel - globalMeanProxy$Temp)^2 )
rmseProxy$ProxyFormatted <- c('Pollen','Alkenones','Foram\nAssemblages','Isotopes from\nForams','Mg/Ca Ratio','TEX86',
                              'Sr/Ca Ratio','MBT/CBT','Ice Cores','Radiolarian\nAssemblages','Spleothem')

# Match color palette from other maps/plots
blueList <- brewer.pal(7, "RdBu")
blue <- blueList[7]
#plot as barplot
plt2 <- ggplot(data = rmseProxy, aes(x = ProxyFormatted, y = RMSE)) + 
  geom_bar(stat = 'identity', color = blue, fill = blue) + 
  labs(title = "Root Mean Square Error by Proxy",
       x = "Proxy") + ylim(0,2.17)
plt2 + coord_flip()

##baseplot version of plot
# barplot(rmseProxy, main = "Root Mean Square Error\n(Models vs Reconstructed Temperatures)", 
#         xlab = "Root Mean Square Error", ylab = "Proxy", col = blue,
#         horiz=TRUE, names.arg = proxyTypes, las=1, width = 5)

### Bias
rmseProxy$Bias <- globalMeanModel - globalMeanProxy$Temp

#plot as barplot
ggplot(data = rmseProxy, aes(x = ProxyFormatted, y = Bias)) + 
  geom_bar(stat = 'identity', color = blue, fill = blue) + 
  labs(title = "Bias by Proxy",
       x = "Proxy", y = "Bias (°C)") + 
  ylim(-2.25,1.5) + coord_flip() + 
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),  
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size=24))





##### Compare global average by location (ocean/land) #####

# get the global mean temperature (and put in an easy format for plotting later)
globalMeanReconstructed <- data.frame(ocean = "All Sites", Mean_Age = mean(proxy$Mean_Age), Temp = mean(proxy$Temp))

# determine whether each row in the data frame is an ocean point or not
proxy$ocean <- 0
proxy$ocean <- ifelse( grepl("Ocean", proxy$Location, fixed=TRUE), "Oceanic\nSites", "Continental\nSites")

# get count of number of unique sites per proxy
as.data.frame(table(proxy$ocean))

# take global mean for ocean and continent
globalMeanLocation <- proxy[, list(Mean_Age = mean(Mean_Age),
                                Temp = mean(Temp)), by = ocean]

globalMeanLocation <- rbind(globalMeanLocation, globalMeanReconstructed)

# Calculate and plot the bias vs. the models
globalMeanLocation$Bias <- globalMeanModel - globalMeanLocation$Temp

#plot as barplot
ggplot(data = globalMeanLocation, aes(x = ocean, y = Bias)) + 
  geom_bar(stat = 'identity', color = blue, fill = blue) + 
  labs(title = "Bias by Location",
       x = "Location", y = "Bias (°C)") + 
  ylim(-2.25,1.5) + coord_flip() + 
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),  
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size=24))
#theme(axis.text.x = element_text(angle = 35, hjust = 1))



