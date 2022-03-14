library(mapproj)
library(rworldmap)
library(RColorBrewer)
#library(lattice)
library(rgdal)
library(raster)
library(rasterVis)
library(mcr)

library(openxlsx)
library(data.table)

##################
# 
# Notes:
#
# - Actual temperatures, not anomalies
# - No correction for seasonality yet
#
##################

##### NOAA #####
# Read in the NOAA data as a data frame
dataNOAA <- readWorkbook("C:\\Users\\sabrina.curtis\\Desktop\\temps_for_r.xlsx", sheet=1)
# Save as a different data frame that will be altered
# (can restart the process from this line without having to read the Excel file all over again)
data1 <- dataNOAA
# Make a column with unique Lat Lon pairs (some Lat or Lon values are repeated between sites, but the pairs are unique)
data1$LatLon <- paste(data1$Lat, data1$Lon)

##### PANGAEA #####
# Read in the PANGAEA data as a data frame
dataPANGAEA <- readWorkbook("C:\\Users\\sabrina.curtis\\Desktop\\Pangaea_data.xlsx", sheet=1)
# Save as a different data frame that will be altered
# (can restart the process from this line without having to read the Excel file all over again)
data2 <- dataPANGAEA
# Make a column with unique Lat Lon pairs (some Lat or Lon values are repeated between sites, but the pairs are unique)
data2$LatLon <- paste(data2$Lat, data2$Lon)

##### Buizert 2018 Greenland #####
dataGreen <- readWorkbook("C:\\Users\\sabrina.curtis\\Desktop\\greenland_buizert.xlsx", sheet=1, startRow = 17)
green1 <- data.frame(Temp = dataGreen$ANN, Age = dataGreen$Age.,
				Proxy = "ice core", Seasonality = '',
				Study = "Buizert 2018 NEEM",
				Lat = 77.45, Lon = -51.06)
green2 <- data.frame(Temp = dataGreen$ANN.1, Age = dataGreen$Age..1,
				Proxy = "ice core", Seasonality = '',
				Study = "Buizert 2018 NGRIP",
				Lat = 75.1, Lon = -42.32)
green3 <- data.frame(Temp = dataGreen$ANN.2, Age = dataGreen$Age..2,
				Proxy = "ice core", Seasonality = '',
				Study = "Buizert 2018 GISP2",
				Lat = 72.58, Lon = -38.48)
green4 <- data.frame(Temp = dataGreen$ANN.3, Age = dataGreen$Age..3,
				Proxy = "ice core", Seasonality = '',
				Study = "Buizert 2018 Dye-3",
				Lat = 65.2, Lon = -43.8)
green5 <- data.frame(Temp = dataGreen$ANN.4, Age = dataGreen$Age..4,
				Proxy = "ice core", Seasonality = '',
				Study = "Buizert 2018 ReCAP/Renland",
				Lat = 71.3, Lon = -26.72)
green6 <- data.frame(Temp = dataGreen$ANN.5, Age = dataGreen$Age..5,
				Proxy = "ice core", Seasonality = '',
				Study = "Buizert 2018 Agassiz",
				Lat = 80.82, Lon = -72.9)
green7 <- data.frame(Temp = dataGreen$ANN.6, Age = dataGreen$Age..6,
				Proxy = "ice core", Seasonality = '',
				Study = "Buizert 2018 Hans Tausen Iskappe 1995",
				Lat = 82.88, Lon = -36.47)
green8 <- data.frame(Temp = dataGreen$ANN.7, Age = dataGreen$Age..7,
				Proxy = "ice core", Seasonality = '',
				Study = "Buizert 2018 Camp Century",
				Lat = 77.2, Lon = -61.1)
green9 <- data.frame(Temp = dataGreen$ANN.8, Age = dataGreen$Age..8,
				Proxy = "ice core", Seasonality = '',
				Study = "Buizert 2018 EGRIP",
				Lat = 75.63, Lon = -35.99)

# Combine into one more useful data frame
greenAll <- rbind(green1,green2,green3,green4,green5,green6,green7,green8,green9)
# Get the LatLon combined colume
greenAll$LatLon <- paste(greenAll$Lat, greenAll$Lon)
# Reorder the columns so that it matches the NOAA and PANGAEA data
greenAll <- greenAll[,c(6,7,1,2,3,4,5,8)]

##### Kobashi 2017 Greenland #####
dataKobashi <- readWorkbook("C:\\Users\\sabrina.curtis\\Desktop\\greenland_kobashi.xlsx", sheet=1)
kobashi <- data.frame(Temp = dataKobashi$Temp, Age = dataKobashi$Age,
				Proxy = "ice core", Seasonality = '',
				Study = "Kobashi 2017",
				Lat = 72.6, Lon = -38.5)
kobashi$LatLon <- paste(kobashi$Lat, kobashi$Lon)
kobashi <- kobashi[,c(6,7,1,2,3,4,5,8)]

##### Hessler #####
dataHessler <- readWorkbook("C:\\Users\\sabrina.curtis\\Desktop\\Data\\hessler_data.xlsx", sheet=1)
dataHessler$Age..ka.BP. <- dataHessler$Age..ka.BP. * 1000

hessler_ann <- data.frame(Lat = dataHessler$Lat, Lon = dataHessler$Lon,
				Temp = dataHessler$SST..1.12...Â.C., 
				Proxy = dataHessler$Material, Seasonality = "annual",
				Age = dataHessler$Age..ka.BP.,
				Study = dataHessler$Reference,
				LatLon = paste(dataHessler$Lat, dataHessler$Lon)
				)
hessler_sum <- data.frame(Lat = dataHessler$Lat, Lon = dataHessler$Lon,
				Temp = dataHessler$SST.sum..Â.C., 
				Proxy = dataHessler$Material, Seasonality = "summer",
				Age = dataHessler$Age..ka.BP.,
				Study = dataHessler$Reference,
				LatLon = paste(dataHessler$Lat, dataHessler$Lon)
				)
hessler_win <- data.frame(Lat = dataHessler$Lat, Lon = dataHessler$Lon,
				Temp = dataHessler$SST.win..Â.C., 
				Proxy = dataHessler$Material, Seasonality = "winter",
				Age = dataHessler$Age..ka.BP.,
				Study = dataHessler$Reference,
				LatLon = paste(dataHessler$Lat, dataHessler$Lon)
				)
hessler_all <- rbind(hessler_ann, hessler_sum, hessler_win)
hessler_all <- na.omit(hessler_all)



##### All #####
# Combine the above data frames into one for plotting and analysis
data <- rbind(data1, data2, greenAll, kobashi, hessler_all)

# Colapse rows together by site location (average the temperatures by site)
data <- data.table(data, key = "LatLon")
data <- data[, list(Lat = Lat[1],
				Lon = Lon[1],
				Proxy = Proxy[1],
				Mean_Age = mean(Age),
				Temp = mean(Temp),
				Study = Study[1]), by = LatLon]

# Get the number of rows in the data table so that it can be displayed on the map
numRows <- nrow(data)

# Load the background map
map <- getMap(resolution = "low")
# Plot the map
plot(map, main = paste0("Distribution of Temperature Reconstruction Data\n(n = ", numRows, ")" ))

# Bring in color palettes
cols <- rev(brewer.pal(10, "RdYlBu"))
cols <- colorRampPalette(cols)

# Prepare data for color assignment to temp.
data$order <- findInterval(data$Temp, sort(data$Temp))

# Plot the data points; shape represents proxy (still working on color for temp)
points(data$Lon, data$Lat, 
		col = cols(nrow(data))[data$order], 
		pch = ifelse(data$Proxy == 'alkenone', 20, 
				ifelse(data$Proxy == 'Mg/Ca', 18, 
					ifelse(data$Proxy == 'ice core', 19, 
						ifelse(data$Proxy == 'foram assemblage'|| 
								data$Proxy == 'faunal estimates' || 
								data$Proxy == 'diatom assemblage' || 
								data$Proxy == 'radiolarian assemblage' || 
								data$Proxy == 'dinocyst assemblage', 17, 
							ifelse(data$Proxy == 'GDGT' || data$Proxy == 'MBT/CBT' || data$Proxy == 'TEX86', 16,
								ifelse(data$Proxy == 'chironomid', 15,
									ifelse(data$Proxy == 'pollen', 4, 
										ifelse(data$Proxy == 'foram isotopes', 8, 1))))))))
	)

legend("topleft", col = cols(2), pch = 15, legend = c(round(range(data$Temp),1)),
		title = "Temperature Range", horiz = TRUE)

legend("bottom", legend = c('Alkenone','Mg/Ca','Ice Core','Faunal Assemblages','GDGT','Chironomid','Pollen','Foram Isotopes','Other'), 
		title = "Proxy", ncol = 3,
		col = "black",
		pch = c(20,18,19,17,16,15,4,8,1)
		)


###################
#
# Plot on a globe
#
###################

#library(threejs)
#library(flipChartBasics)

 
# Plot the data on the globe
#globejs(lat = data$Lat,
#        long = data$Lon,
#        val = data$Temp),
#        color = cols(nrow(data))[data$order],
#        pointsize = 0.5,
#        atmosphere = TRUE)







