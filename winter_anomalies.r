library(openxlsx)
library(data.table)

##### Marsicek #####

# Read in the Marsicek data as a data frame
dataMarsicekCold <- readWorkbook("C:\\Users\\sabrina.curtis\\Desktop\\Data\\anomalies\\NHSignificantSites_MTCO.xlsx", sheet=2, startRow=2)

# Save as a different data frame that will be altered
# (can restart the process from this line without having to read the Excel file all over again)
dataCold <- data.frame(Lat = dataMarsicekCold$Lat, Lon = dataMarsicekCold$Long,
			Temp = dataMarsicekCold$MTCO.anom,
			Proxy = "pollen", Seasonality = "winter",
			Mean_Age = dataMarsicekCold$Age, Study = "Marsicek",
			LatLon = paste(dataMarsicekAnn$Lat, dataMarsicekCold$Long)
			)
dataCold <- na.omit(dataCold)


##### Calculate Anomaly #####

# calculate the temp anomaly for data sets that do not already have it

## NOAA ##

# Read in the NOAA data as a data frame
dataNOAA <- readWorkbook("C:\\Users\\sabrina.curtis\\Desktop\\Data\\6ka\\temps_for_r.xlsx", sheet=1)

# Save as a different data frame that will be altered
# (can restart the process from this line without having to read the Excel file all over again)
data1 <- dataNOAA

# Make a column with unique Lat Lon pairs (some Lat or Lon values are repeated between sites, but the pairs are unique)
data1$LatLon <- paste(data1$Lat, data1$Lon)

data1 <- data.table(data1, key = "LatLon")
data1 <- data1[, list(Lat = Lat[1],
				Lon = Lon[1],
				Proxy = Proxy[1],
				Mean_Age = mean(Age),
				Temp = mean(Temp),
				Seasonality = Seasonality[1],
				Study = Study[1]), by = LatLon]


## PANGAEA ##

# Read in the PANGAEA data as a data frame
dataPANGAEA <- readWorkbook("C:\\Users\\sabrina.curtis\\Desktop\\Data\\6ka\\Pangaea_data.xlsx", sheet=1)

# Save as a different data frame that will be altered
# (can restart the process from this line without having to read the Excel file all over again)
data2 <- dataPANGAEA

# Make a column with unique Lat Lon pairs (some Lat or Lon values are repeated between sites, but the pairs are unique)
data2$LatLon <- paste(data2$Lat, data2$Lon)

data2 <- data.table(data2, key = "LatLon")
data2 <- data2[, list(Lat = Lat[1],
				Lon = Lon[1],
				Proxy = Proxy[1],
				Mean_Age = mean(Age),
				Temp = mean(Temp),
				Seasonality = Seasonality[1],
				Study = Study[1]), by = LatLon]

temp6k <- rbind(data1, data2)

temp6k <- subset(temp6k, Seasonality == "winter" | Seasonality == "january" | 
				Seasonality == "coldest month" | Seasonality == "february" | 
				Seasonality == "cold month" | Seasonality == "cold season")


## Core tops from NOAA and PANGAEA data ##

dataCoreTops <- readWorkbook("C:\\Users\\sabrina.curtis\\Desktop\\Data\\present\\winter_coretops.xlsx", sheet=1)
coretops <- dataCoreTops[,-6]
coretops$LatLon <- paste(coretops$Lat, coretops$Lon)

# Colapse rows together by site location (average the temperatures by site)
coretops <- data.table(coretops, key = "LatLon")
coretops <- coretops[, list(PresentTemp = mean(Temp)), by = LatLon]

# Combine the two data frames
# the function automatically removes rows with NA's (rows w/o present temperatures)
dataAnom2 <- merge(temp6k, coretops, by="LatLon")

# calculate the temperature anomaly
dataAnom2$Temp <- dataAnom2$Temp - dataAnom2$PresentTemp

# rework data frame so it matches the other anomaly data
dataAnom2 <- dataAnom2[,-9]


##### Buizert 2018 Greenland #####

dataGreen <- readWorkbook("C:\\Users\\sabrina.curtis\\Desktop\\Data\\6ka\\greenland_buizert.xlsx", sheet=1, startRow = 17)
green1 <- data.frame(Temp = dataGreen$DJF, Age = dataGreen$Age,
				Proxy = "ice core", Seasonality = 'winter',
				Study = "Buizert 2018 NEEM",
				Lat = 77.45, Lon = -51.06)
green2 <- data.frame(Temp = dataGreen$DJF.1, Age = dataGreen$Age.1,
				Proxy = "ice core", Seasonality = 'winter',
				Study = "Buizert 2018 NGRIP",
				Lat = 75.1, Lon = -42.32)
green3 <- data.frame(Temp = dataGreen$DJF.2, Age = dataGreen$Age.2,
				Proxy = "ice core", Seasonality = 'winter',
				Study = "Buizert 2018 GISP2",
				Lat = 72.58, Lon = -38.48)
green4 <- data.frame(Temp = dataGreen$DJF.3, Age = dataGreen$Age.3,
				Proxy = "ice core", Seasonality = 'winter',
				Study = "Buizert 2018 Dye-3",
				Lat = 65.2, Lon = -43.8)
green5 <- data.frame(Temp = dataGreen$DJF.4, Age = dataGreen$Age.4,
				Proxy = "ice core", Seasonality = 'winter',
				Study = "Buizert 2018 ReCAP/Renland",
				Lat = 71.3, Lon = -26.72)
green6 <- data.frame(Temp = dataGreen$DJF.5, Age = dataGreen$Age.5,
				Proxy = "ice core", Seasonality = 'winter',
				Study = "Buizert 2018 Agassiz",
				Lat = 80.82, Lon = -72.9)
green7 <- data.frame(Temp = dataGreen$DJF.6, Age = dataGreen$Age.6,
				Proxy = "ice core", Seasonality = 'winter',
				Study = "Buizert 2018 Hans Tausen Iskappe 1995",
				Lat = 82.88, Lon = -36.47)
green8 <- data.frame(Temp = dataGreen$DJF.7, Age = dataGreen$Age.7,
				Proxy = "ice core", Seasonality = 'winter',
				Study = "Buizert 2018 Camp Century",
				Lat = 77.2, Lon = -61.1)
green9 <- data.frame(Temp = dataGreen$DJF.8, Age = dataGreen$Age.8,
				Proxy = "ice core", Seasonality = 'winter',
				Study = "Buizert 2018 EGRIP",
				Lat = 75.63, Lon = -35.99)

# Combine into one more useful data frame
greenAll <- rbind(green1,green2,green3,green4,green5,green6,green7,green8,green9)
# Get the LatLon combined colume
greenAll$LatLon <- paste(greenAll$Lat, greenAll$Lon)
# Reorder the columns so that it matches the other data
greenAll <- greenAll[,c(6,7,1,2,3,4,5,8)]


## Core Tops ##

dataGreen <- readWorkbook("C:\\Users\\sabrina.curtis\\Desktop\\Data\\present\\greenland_buizert_core_tops.xlsx", sheet=1, startRow = 17)
green1 <- data.frame(Temp = dataGreen$DJF, Age = dataGreen$Age,
				Proxy = "ice core", Seasonality = 'winter',
				Study = "Buizert 2018 NEEM",
				Lat = 77.45, Lon = -51.06)
green2 <- data.frame(Temp = dataGreen$DJF.1, Age = dataGreen$Age.1,
				Proxy = "ice core", Seasonality = 'winter',
				Study = "Buizert 2018 NGRIP",
				Lat = 75.1, Lon = -42.32)
green3 <- data.frame(Temp = dataGreen$DJF.2, Age = dataGreen$Age.2,
				Proxy = "ice core", Seasonality = 'winter',
				Study = "Buizert 2018 GISP2",
				Lat = 72.58, Lon = -38.48)
green4 <- data.frame(Temp = dataGreen$DJF.3, Age = dataGreen$Age.3,
				Proxy = "ice core", Seasonality = 'winter',
				Study = "Buizert 2018 Dye-3",
				Lat = 65.2, Lon = -43.8)
green5 <- data.frame(Temp = dataGreen$DJF.4, Age = dataGreen$Age.4,
				Proxy = "ice core", Seasonality = 'winter',
				Study = "Buizert 2018 ReCAP/Renland",
				Lat = 71.3, Lon = -26.72)
green6 <- data.frame(Temp = dataGreen$DJF.5, Age = dataGreen$Age.5,
				Proxy = "ice core", Seasonality = 'winter',
				Study = "Buizert 2018 Agassiz",
				Lat = 80.82, Lon = -72.9)
green7 <- data.frame(Temp = dataGreen$DJF.6, Age = dataGreen$Age.6,
				Proxy = "ice core", Seasonality = 'winter',
				Study = "Buizert 2018 Hans Tausen Iskappe 1995",
				Lat = 82.88, Lon = -36.47)
green8 <- data.frame(Temp = dataGreen$DJF.7, Age = dataGreen$Age.7,
				Proxy = "ice core", Seasonality = 'winter',
				Study = "Buizert 2018 Camp Century",
				Lat = 77.2, Lon = -61.1)
green9 <- data.frame(Temp = dataGreen$DJF.8, Age = dataGreen$Age.8,
				Proxy = "ice core", Seasonality = 'winter',
				Study = "Buizert 2018 EGRIP",
				Lat = 75.63, Lon = -35.99)

# Combine into one more useful data frame
greenAllPresent <- rbind(green1,green2,green3,green4,green5,green6,green7,green8,green9)
# Get the LatLon combined colume
greenAllPresent$LatLon <- paste(greenAllPresent$Lat, greenAllPresent$Lon)
# Reorder the columns so that it matches the other data
greenAllPresent <- greenAllPresent[,c(6,7,1,2,3,4,5,8)]

# Calculate anomalies
greenAll<- data.table(greenAll, key = "LatLon")
greenAll<- greenAll[, list(Lat = Lat[1],
				Lon = Lon[1],
				Proxy = Proxy[1],
				Mean_Age = mean(Age),
				Temp = mean(Temp),
				Seasonality = Seasonality[1],
				Study = Study[1]), by = LatLon]

greenAllPresent <- data.table(greenAllPresent, key = "LatLon")
greenAllPresent <- greenAllPresent[, list(Age = mean(Age),
							PresentTemp = mean(Temp)), by = LatLon]

# Combine the two data frames
# the function automatically removes rows with NA's (rows w/o present temperatures)
greenlandAnom <- merge(greenAll, greenAllPresent, by="LatLon")

# calculate the temperature anomaly
greenlandAnom$Temp <- greenlandAnom$Temp - greenlandAnom$PresentTemp
greenlandAnom <- greenlandAnom[,-c(9,10)]


##### Arctic Database #####

## Anomalies ##

dataArctic <- readWorkbook("C:\\Users\\sabrina.curtis\\Desktop\\Data\\anomalies\\arctic_database_anomalies.xlsx", sheet=1)

dataArctic <-  subset(dataArctic, Seasonality == "winter" | Seasonality == "january" | 
				Seasonality == "coldest month" | Seasonality == "february" | 
				Seasonality == "cold month" | Seasonality == "cold season")

arcticAnom <- dataArctic
arcticAnom$LatLon <- paste(arcticAnom$Lat, arcticAnom$Lon)

arcticAnom <- data.table(arcticAnom, key = "LatLon")
arcticAnom <- arcticAnom[, list(Lat = Lat[1],
				Lon = Lon[1],
				Proxy = Proxy[1],
				Mean_Age = mean(Age),
				Temp = mean(Temp),
				Seasonality = Seasonality[1],
				Study = ""), by = LatLon]

## Cacluate anomalies for the other sites ##

dataArctic2 <- readWorkbook("C:\\Users\\sabrina.curtis\\Desktop\\Data\\6ka\\arctic_database_temps.xlsx", sheet=1)

dataArctic2 <-  subset(dataArctic2, Seasonality == "winter" | Seasonality == "january" | 
				Seasonality == "coldest month" | Seasonality == "february" | 
				Seasonality == "cold month" | Seasonality == "cold season")

data3 <- dataArctic2
data3$LatLon <- paste(data3$Lat, data3$Lon)

data3 <- data.table(data3, key = "LatLon")
data3 <- data3[, list(Lat = Lat[1],
				Lon = Lon[1],
				Proxy = Proxy[1],
				Mean_Age = mean(Age),
				Temp = mean(Temp),
				Seasonality = Seasonality[1],
				Study = ""), by = LatLon]

dataArctic3 <- readWorkbook("C:\\Users\\sabrina.curtis\\Desktop\\Data\\present\\arctic_database_core_top.xlsx", sheet=1)

dataArctic3 <-  subset(dataArctic3, Seasonality == "winter" | Seasonality == "january" | 
				Seasonality == "coldest month" | Seasonality == "february" | 
				Seasonality == "cold month" | Seasonality == "cold season")

data4 <- dataArctic3
data4$LatLon <- paste(data4$Lat, data4$Lon)

data4 <- data.table(data4, key = "LatLon")
data4 <- data4[, list(PresentTemp = mean(Temp)), by = LatLon]

# Combine the two data frames
arcticAnom2 <- merge(data3, data4, by="LatLon")

# calculate the temperature anomaly
arcticAnom2$Temp <- arcticAnom2$Temp - arcticAnom2$PresentTemp
arcticAnom2 <- arcticAnom2[,-9]


##### Combine #####

# Combine to one data table
data <- rbind(dataCold, dataAnom2, greenlandAnom, arcticAnom, arcticAnom2)

# order by Temp
data <- data[order(data$Temp),]

# Note that the most extreme warm and cold points are almost double the next point on each end
# remove those points
#data <- data[-c(1,length(data$Temp)),]


##### Plot #####

library(mapproj)
library(rworldmap)
library(RColorBrewer)

# Get the number of rows in the data table so that it can be displayed on the map
numRows <- nrow(data)

# Load the background map
map <- getMap(resolution = "low")

# Plot the map
plot(map, main = paste0("Temperature Anomalies\n(n = ", numRows, ")" ))

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


### Northern Hemisphere Winter ###

dataWin <- data[Lat >= 0]

# Get the number of rows in the data table so that it can be displayed on the map
numRows <- nrow(dataWin)

# Load the background map
map <- getMap(resolution = "low")

# Plot the map
plot(map, main = paste0("Winter Temperature Anomalies\n(Northern Hemisphere; n = ", numRows, ")" ))
# Bring in color palettes
cols <- rev(brewer.pal(10, "RdYlBu"))
cols <- colorRampPalette(cols)
# Prepare data for color assignment to temp.
dataWin$order <- findInterval(dataWin$Temp, sort(dataWin$Temp))

points(dataWin$Lon, dataWin$Lat, 
		col = cols(nrow(dataWin))[dataWin$order], 
		pch = 20, cex = 2
	)
legend("topleft", col = cols(2), pch = 15, legend = c(round(range(dataWin$Temp),1)),
		title = "Temperature Range", horiz = TRUE)




