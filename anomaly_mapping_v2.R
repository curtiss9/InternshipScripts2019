library(mapproj)
library(rworldmap)
library(RColorBrewer)
library(rgdal)
library(raster)
library(rasterVis)
library(mcr)
library(sp)

library(openxlsx)
library(data.table)

library(ggplot2)

##################
# 
# Notes:
#
# - Anomalies
# - Mean Annual
#
##################


##### Marsicek #####

# Read in the Marsicek data as a data frame
dataMarsicekAnn <- readWorkbook("~/Desktop/Data_and_Scripts/Data/anomalies/NHSignificantSites_tAvg.xlsx", sheet=2, startRow=2)

# Save as a different data frame that will be altered
# (can restart the process from this line without having to read the Excel file all over again)
dataAnn <- data.frame(Lat = dataMarsicekAnn$Lat, Lon = dataMarsicekAnn$Long,
			Temp = dataMarsicekAnn$annT.anom,
			Proxy = "pollen", Seasonality = "annual",
			Age = dataMarsicekAnn$Age, Study = "Marsicek"
			)

dataAnn$LatLonSeasonProxy <- paste(dataAnn$Lat, dataAnn$Lon, dataAnn$Seasonality, dataAnn$Proxy)
dataAnn <- na.omit(dataAnn)

# Colapse rows together by site location (average the temperatures by site)
dataAnn <- data.table(dataAnn, key = "LatLonSeasonProxy")
dataAnn <- dataAnn[, list(Lat = Lat[1],
				Lon = Lon[1],
				Proxy = Proxy[1],
				Mean_Age = mean(Age),
				Temp = mean(Temp),
				Seasonality = Seasonality[1],
				Study = Study[1]), by = LatLonSeasonProxy]


##### Pre-calculated temp anomaly #####

# Bring in the temp anomaly data, and get to same format as Marsicek data

dataInput <- readWorkbook("~/Desktop/Data_and_Scripts/Data/anomalies/temp_anomaly_data_v2.xlsx", sheet=1)

dataAnom <- subset(dataInput, Seasonality == "annual")
# Make a key unique to each location, seasonality, and proxy
dataAnom$LatLonSeasonProxy <- paste(dataAnom$Lat, dataAnom$Lon, dataAnom$Seasonality, dataAnom$Proxy)

dataAnom <- na.omit(dataAnom)

# Colapse rows together by site location (average the temperatures by site)
dataAnom <- data.table(dataAnom, key = "LatLonSeasonProxy")
dataAnom <- dataAnom[, list(Lat = Lat[1],
				Lon = Lon[1],
				Proxy = Proxy[1],
				Mean_Age = mean(Age),
				Temp = mean(Temp),
				Seasonality = Seasonality[1],
				Study = Study[1]), by = LatLonSeasonProxy]


##### Calculate Anomaly #####

# calculate the temp anomaly for data sets that do not already have it

## NOAA ##

# Read in the NOAA data as a data frame
dataNOAA <- readWorkbook("~/Desktop/Data_and_Scripts/Data/6ka/temps_for_r_v2.xlsx", sheet=1)
data1 <- subset(dataNOAA, Seasonality == "annual")

# Make a key unique to each location, seasonality, and proxy
data1$LatLonSeasonProxy <- paste(data1$Lat, data1$Lon, data1$Seasonality, data1$Proxy)

data1 <- data.table(data1, key = "LatLonSeasonProxy")
data1 <- data1[, list(Lat = Lat[1],
				Lon = Lon[1],
				Proxy = Proxy[1],
				Mean_Age = mean(Age),
				Temp = mean(Temp),
				Seasonality = Seasonality[1],
				Study = Study[1]), by = LatLonSeasonProxy]

## PANGAEA ##

# Read in the PANGAEA data as a data frame
dataPANGAEA <- readWorkbook("~/Desktop/Data_and_Scripts/Data/6ka/Pangaea_data_v2.xlsx", sheet=1)
data2 <- subset(dataPANGAEA, Seasonality == "annual")

# Make a key unique to each location, seasonality, and proxy
data2$LatLonSeasonProxy <- paste(data2$Lat, data2$Lon, data2$Seasonality, data2$Proxy)

data2 <- data.table(data2, key = "LatLonSeasonProxy")
data2 <- data2[, list(Lat = Lat[1],
				Lon = Lon[1],
				Proxy = Proxy[1],
				Mean_Age = mean(Age),
				Temp = mean(Temp),
				Seasonality = Seasonality[1],
				Study = Study[1]), by = LatLonSeasonProxy]

temp6k <- rbind(data1, data2)


## Core tops from NOAA and PANGAEA data ##

dataCoreTops <- readWorkbook("~/Desktop/Data_and_Scripts/Data/present/annual_coretops_v2.xlsx", sheet=1)
coretops <- dataCoreTops

# Make a key unique to each location, seasonality, and proxy
coretops$LatLonSeasonProxy <- paste(coretops$Lat, coretops$Lon, coretops$Seasonality, coretops$Proxy)

# Colapse rows together by site location (average the temperatures by site)
coretops <- data.table(coretops, key = "LatLonSeasonProxy")
coretops <- coretops[, list(PresentTemp = mean(Temp)), by = LatLonSeasonProxy]

# Combine the two data frames
# the function automatically removes rows with NA's (rows w/o present temperatures)
dataAnom2 <- merge(temp6k, coretops, by="LatLonSeasonProxy")

# calculate the temperature anomaly
dataAnom2$Temp <- dataAnom2$Temp - dataAnom2$PresentTemp

# rework data frame so it matches the other anomaly data
dataAnom2 <- dataAnom2[,-9]

# 
# ##### Buizert 2018 Greenland #####
# 
# dataGreen <- readWorkbook("~/Desktop/Data_and_Scripts/Data/6ka/greenland_buizert.xlsx", sheet=1, startRow = 17)
# green1 <- data.frame(Temp = dataGreen$ANN, Age = dataGreen$Age,
# 				Proxy = "ice core", Seasonality = 'annual',
# 				Study = "Buizert 2018 NEEM",
# 				Lat = 77.45, Lon = -51.06)
# green2 <- data.frame(Temp = dataGreen$ANN.1, Age = dataGreen$Age.1,
# 				Proxy = "ice core", Seasonality = 'annual',
# 				Study = "Buizert 2018 NGRIP",
# 				Lat = 75.1, Lon = -42.32)
# green3 <- data.frame(Temp = dataGreen$ANN.2, Age = dataGreen$Age.2,
# 				Proxy = "ice core", Seasonality = 'annual',
# 				Study = "Buizert 2018 GISP2",
# 				Lat = 72.58, Lon = -38.48)
# green4 <- data.frame(Temp = dataGreen$ANN.3, Age = dataGreen$Age.3,
# 				Proxy = "ice core", Seasonality = 'annual',
# 				Study = "Buizert 2018 Dye-3",
# 				Lat = 65.2, Lon = -43.8)
# green5 <- data.frame(Temp = dataGreen$ANN.4, Age = dataGreen$Age.4,
# 				Proxy = "ice core", Seasonality = 'annual',
# 				Study = "Buizert 2018 ReCAP/Renland",
# 				Lat = 71.3, Lon = -26.72)
# green6 <- data.frame(Temp = dataGreen$ANN.5, Age = dataGreen$Age.5,
# 				Proxy = "ice core", Seasonality = 'annual',
# 				Study = "Buizert 2018 Agassiz",
# 				Lat = 80.82, Lon = -72.9)
# green7 <- data.frame(Temp = dataGreen$ANN.6, Age = dataGreen$Age.6,
# 				Proxy = "ice core", Seasonality = 'annual',
# 				Study = "Buizert 2018 Hans Tausen Iskappe 1995",
# 				Lat = 82.88, Lon = -36.47)
# green8 <- data.frame(Temp = dataGreen$ANN.7, Age = dataGreen$Age.7,
# 				Proxy = "ice core", Seasonality = 'annual',
# 				Study = "Buizert 2018 Camp Century",
# 				Lat = 77.2, Lon = -61.1)
# green9 <- data.frame(Temp = dataGreen$ANN.8, Age = dataGreen$Age.8,
# 				Proxy = "ice core", Seasonality = 'annual',
# 				Study = "Buizert 2018 EGRIP",
# 				Lat = 75.63, Lon = -35.99)
# 
# # Combine into one more useful data frame
# greenAll <- rbind(green1,green2,green3,green4,green5,green6,green7,green8,green9)
# # Make a key unique to each location, seasonality, and proxy
# greenAll$LatLonSeasonProxy <- paste(greenAll$Lat, greenAll$Lon, greenAll$Seasonality, greenAll$Proxy)
# # Reorder the columns so that it matches the other data
# greenAll <- greenAll[,c(6,7,1,2,3,4,5,8)]
# 
# 
# ## Core Tops ##
# 
# dataGreen <- readWorkbook("~/Desktop/Data_and_Scripts/Data/present/greenland_buizert_core_tops.xlsx", sheet=1, startRow = 17)
# green1 <- data.frame(Temp = dataGreen$ANN, Age = dataGreen$Age,
# 				Proxy = "ice core", Seasonality = 'annual',
# 				Study = "Buizert 2018 NEEM",
# 				Lat = 77.45, Lon = -51.06)
# green2 <- data.frame(Temp = dataGreen$ANN.1, Age = dataGreen$Age.1,
# 				Proxy = "ice core", Seasonality = 'annual',
# 				Study = "Buizert 2018 NGRIP",
# 				Lat = 75.1, Lon = -42.32)
# green3 <- data.frame(Temp = dataGreen$ANN.2, Age = dataGreen$Age.2,
# 				Proxy = "ice core", Seasonality = 'annual',
# 				Study = "Buizert 2018 GISP2",
# 				Lat = 72.58, Lon = -38.48)
# green4 <- data.frame(Temp = dataGreen$ANN.3, Age = dataGreen$Age.3,
# 				Proxy = "ice core", Seasonality = 'annual',
# 				Study = "Buizert 2018 Dye-3",
# 				Lat = 65.2, Lon = -43.8)
# green5 <- data.frame(Temp = dataGreen$ANN.4, Age = dataGreen$Age.4,
# 				Proxy = "ice core", Seasonality = 'annual',
# 				Study = "Buizert 2018 ReCAP/Renland",
# 				Lat = 71.3, Lon = -26.72)
# green6 <- data.frame(Temp = dataGreen$ANN.5, Age = dataGreen$Age.5,
# 				Proxy = "ice core", Seasonality = 'annual',
# 				Study = "Buizert 2018 Agassiz",
# 				Lat = 80.82, Lon = -72.9)
# green7 <- data.frame(Temp = dataGreen$ANN.6, Age = dataGreen$Age.6,
# 				Proxy = "ice core", Seasonality = 'annual',
# 				Study = "Buizert 2018 Hans Tausen Iskappe 1995",
# 				Lat = 82.88, Lon = -36.47)
# green8 <- data.frame(Temp = dataGreen$ANN.7, Age = dataGreen$Age.7,
# 				Proxy = "ice core", Seasonality = 'annual',
# 				Study = "Buizert 2018 Camp Century",
# 				Lat = 77.2, Lon = -61.1)
# green9 <- data.frame(Temp = dataGreen$ANN.8, Age = dataGreen$Age.8,
# 				Proxy = "ice core", Seasonality = 'annual',
# 				Study = "Buizert 2018 EGRIP",
# 				Lat = 75.63, Lon = -35.99)
# 
# # Combine into one more useful data frame
# greenAllPresent <- rbind(green1,green2,green3,green4,green5,green6,green7,green8,green9)
# # Make a key unique to each location, seasonality, and proxy
# greenAllPresent$LatLonSeasonProxy <- paste(greenAllPresent$Lat, greenAllPresent$Lon, greenAllPresent$Seasonality, greenAllPresent$Proxy)
# # Reorder the columns so that it matches the other data
# greenAllPresent <- greenAllPresent[,c(6,7,1,2,3,4,5,8)]
# 
# # Calculate anomalies
# greenAll<- data.table(greenAll, key = "LatLonSeasonProxy")
# greenAll<- greenAll[, list(Lat = Lat[1],
# 				Lon = Lon[1],
# 				Proxy = Proxy[1],
# 				Mean_Age = mean(Age),
# 				Temp = mean(Temp),
# 				Seasonality = Seasonality[1],
# 				Study = Study[1]), by = LatLonSeasonProxy]
# 
# greenAllPresent <- data.table(greenAllPresent, key = "LatLonSeasonProxy")
# greenAllPresent <- greenAllPresent[, list(Age = mean(Age),
# 							PresentTemp = mean(Temp)), by = LatLonSeasonProxy]
# 
# # Combine the two data frames
# # the function automatically removes rows with NA's (rows w/o present temperatures)
# greenlandAnom <- merge(greenAll, greenAllPresent, by="LatLonSeasonProxy")
# 
# # calculate the temperature anomaly
# greenlandAnom$Temp <- greenlandAnom$Temp - greenlandAnom$PresentTemp
# greenlandAnom <- greenlandAnom[,-c(9,10)]


##### Arctic Database #####

## Anomalies ##

dataArctic <- readWorkbook("~/Desktop/Data_and_Scripts/Data/anomalies/arctic_database_anomalies.xlsx", sheet=1)
arcticAnom <- subset(dataArctic, Seasonality == "annual")

# Make a key unique to each location, seasonality, and proxy
arcticAnom$LatLonSeasonProxy <- paste(arcticAnom$Lat, arcticAnom$Lon, arcticAnom$Seasonality, arcticAnom$Proxy)

arcticAnom <- data.table(arcticAnom, key = "LatLonSeasonProxy")
arcticAnom <- arcticAnom[, list(Lat = Lat[1],
				Lon = Lon[1],
				Proxy = Proxy[1],
				Mean_Age = mean(Age),
				Temp = mean(Temp),
				Seasonality = Seasonality[1],
				Study = ""), by = LatLonSeasonProxy]


## Cacluate anomalies for the other sites ##

dataArctic2 <- readWorkbook("~/Desktop/Data_and_Scripts/Data/6ka/arctic_database_temps_v2.xlsx", sheet=1)

data3 <- subset(dataArctic2, Seasonality == "annual")
# Make a key unique to each location, seasonality, and proxy
data3$LatLonSeasonProxy <- paste(data3$Lat, data3$Lon, data3$Seasonality, data3$Proxy)

data3 <- data.table(data3, key = "LatLonSeasonProxy")
data3 <- data3[, list(Lat = Lat[1],
				Lon = Lon[1],
				Proxy = Proxy[1],
				Mean_Age = mean(Age),
				Temp = mean(Temp),
				Seasonality = Seasonality[1],
				Study = ""), by = LatLonSeasonProxy]

dataArctic3 <- readWorkbook("~/Desktop/Data_and_Scripts/Data/present/arctic_database_core_top_v2.xlsx", sheet=1)

data4 <- subset(dataArctic3, Seasonality == "annual")
# Make a key unique to each location, seasonality, and proxy
data4$LatLonSeasonProxy <- paste(data4$Lat, data4$Lon, data4$Seasonality, data4$Proxy)

data4 <- data.table(data4, key = "LatLonSeasonProxy")
data4 <- data4[, list(PresentTemp = mean(Temp)), by = LatLonSeasonProxy]

# Combine the two data frames
arcticAnom2 <- merge(data3, data4, by="LatLonSeasonProxy")

# calculate the temperature anomaly
arcticAnom2$Temp <- arcticAnom2$Temp - arcticAnom2$PresentTemp
arcticAnom2 <- arcticAnom2[,-9]


##### Combine #####

# Combine to one data table
data <- rbind(dataAnn, dataAnom, dataAnom2, arcticAnom, arcticAnom2)

# order by Temp
data <- data[order(data$Temp),]

# Note that the most extreme warm and cold points are almost double the next point on each end
# remove those points
data <- data[-c(1,length(data$Temp)),]


##### Export a Data Frame #####

#write.csv(data, "~/Desktop/Data_and_Scripts/Data/exported/temp_anomalies_annual_v2.csv")


##### Plot #####

# Get the number of rows in the data table so that it can be displayed on the map
numRows <- nrow(data)

# Load the background map
map <- getMap(resolution = "low")

# Plot the map
plot(map, main = paste0("Mean Annual Temperature Anomalies\n(n = ", numRows, ")" ))

# Bring in color palettes
cols <- rev(brewer.pal(10, "RdYlBu"))
cols <- colorRampPalette(cols)

# Prepare data for color assignment to temp.
data$order <- findInterval(data$Temp, sort(data$Temp))

# Plot the data points
points(data$Lon, data$Lat, 
		col = cols(nrow(data))[data$order],
		pch = 20
	)
legend("bottom", col = cols(10), pch = 15, legend = levels(cut(data$Temp, breaks=10)),
		title = "Temperature Range", ncol = 5)


# ##### Plot 2 Degree Grid #####
# 
# data2Deg <- data
# 
# # compute grid cell coordinates
# ji <- function(xy, origin=c(0,0), cellsize=c(2,2)) {
#   t(apply(xy, 1, function(z) cellsize/2+origin+cellsize*(floor((z - origin)/cellsize))))
# }
# 
# JI <- ji(cbind(data2Deg$Lon, data2Deg$Lat))
# data2Deg$Lon <- JI[, 1]
# data2Deg$Lat <- JI[, 2]
# data2Deg$LatLon <- paste(data2Deg$Lat, data2Deg$Lon)
# 
# # calculate the average temp and age for points that share coordinates
# data2DegGrid <- data2Deg[, list(Lat = Lat[1],
# 				Lon = Lon[1],
# 				Proxy = Proxy[1],
# 				Mean_Age = mean(Mean_Age),
# 				Temp = mean(Temp)), by = LatLon]
# 
# # Get the number of rows in the data table so that it can be displayed on the map
# numRows <- nrow(data2DegGrid)
# # Load the background map
# map <- getMap(resolution = "low")
# 
# #dev.new(width=10,height=8,unit="in")
# 
# # Plot the map
# plot(map, main = paste0("Mean Annual Temperature Anomalies\nin a 2 Degree Grid" ))
# # Bring in color palettes
# cols <- rev(brewer.pal(10, "RdYlBu"))
# cols <- colorRampPalette(cols)
# 
# # Prepare data for color assignment to temp.
# data2DegGrid$order <- findInterval(data2DegGrid$Temp, sort(data2DegGrid$Temp))
# 
# points(data2DegGrid$Lon, data2DegGrid$Lat, 
# 		col = cols(nrow(data2DegGrid))[data2DegGrid$order], 
# 		pch = 15, cex = .6
# 	)
# legend("bottom", col = cols(10), pch = 15, legend = levels(cut(data2DegGrid$Temp, breaks=10)),
# 		title = "Temperature Range", ncol = 5)
# 
# #dev.copy(png, 'C:\\Users\\sabrina.curtis\\Desktop\\Maps\\proxy_data2x2.png')
# #dev.off()

##### Plot 4 Degree Grid #####

data4Deg <- data

ji <- function(xy, origin=c(0,0), cellsize=c(4,4)) {
  t(apply(xy, 1, function(z) cellsize/2+origin+cellsize*(floor((z - origin)/cellsize))))
}

JI <- ji(cbind(data4Deg$Lon, data4Deg$Lat))
data4Deg$Lon <- JI[, 1]
data4Deg$Lat <- JI[, 2]
#data4Deg$LatLonSeasonProxy <- paste(data4Deg$Lat, data4Deg$Lon, data4Deg$Seasonality, data4Deg$Proxy)

data4DegGrid <- data4Deg[, list(Lat = Lat[1],
				Lon = Lon[1],
				Proxy = Proxy[1],
				Mean_Age = mean(Mean_Age),
				Temp = mean(Temp)), by = LatLonSeasonProxy]

# Load the background map
coast_shapefile = "~/Desktop/Data_and_Scripts/Data/ne_10m_coastline/ne_10m_coastline.shp"  # load coastline shapefile
layer <- ogrListLayers(coast_shapefile)
coast_lines <- readOGR(coast_shapefile, layer=layer)

data4DegGrid <- subset(data4DegGrid, Temp >= -4 & Temp <= 4)


##### Ggplot #####

ggplot() + theme_light() + 
  geom_path(data = coast_lines, aes(long, lat, group=group)) + 
  geom_point(data = data4DegGrid, aes(Lon, Lat, color = Temp), shape=15, size=2.5) + 
  scale_color_distiller(palette = "RdBu",
                        breaks = c(-4,-2,0,2,4),
                        labels = c('-4','-2','0','2','4')) + 
  xlab("Longitude") + ylab("Latitude") +
  scale_x_continuous(breaks = c(-180,-90,0,90,180), 
                     labels = c("180\u00b0","90\u00b0W","0\u00b0","90\u00b0E","180\u00b0")) + 
  scale_y_continuous(breaks = c(-90,-45,0,45,90),
                     labels = c("90\u00b0S","45\u00b0S","0\u00b0","45\u00b0N","90\u00b0N")) + 
  coord_fixed(ratio = 1) + 
  labs(color = "Temperature\nAnomaly\n(\u00b0C)") + 
  ggtitle("Mean Annual Temperature Anomalies\nin a 4 Degree Grid") + 
  theme(plot.title = element_text(hjust = 0.5))


# ##### Base Plot #####
# 
# #dev.new(width=10,height=8,unit="in")
# 
# # Plot the map
# plot(map, main = paste0("Mean Annual Temperature Anomalies\nin a 4 Degree Grid" ))
# # Bring in color palettes
# cols <- rev(brewer.pal(10, "RdYlBu"))
# cols <- colorRampPalette(cols)
# 
# # Prepare data for color assignment to temp.
# data4DegGrid$order <- findInterval(data4DegGrid$Temp, sort(data4DegGrid$Temp))
# 
# points(data4DegGrid$Lon, data4DegGrid$Lat, 
# 		col = cols(nrow(data4DegGrid))[data4DegGrid$order], 
# 		pch = 15, cex = 1
# 	)
# legend("bottom", col = cols(10), pch = 15, legend = levels(cut(data4DegGrid$Temp, breaks=10)),
# 		title = "Temperature Range", ncol = 5)
# 
# #dev.copy(png, 'C:\\Users\\sabrina.curtis\\Desktop\\Maps\\proxy_data4x4.png')
# #dev.off()

##### Average Temperature ####

# Global
avgTemp <- mean(data4DegGrid$Temp)

# 30-90 degree latitudes
upperLat <- data4DegGrid[Lat > 30]
avgUpperLat <- mean(upperLat$Temp)
print(avgUpperLat)

# -30-30 degree latitudes
midLat <- data4DegGrid[Lat <= 30 & Lat >= -30]
avgMidLat <- mean(midLat$Temp)
print(avgMidLat)

# -90--30 degree latitudes
lowerLat <- data4DegGrid[Lat < -30]
avgLowerLat <- mean(lowerLat$Temp)
print(avgLowerLat)



