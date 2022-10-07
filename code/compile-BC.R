###############################################################################
# Exploration of stream temperature data downloaded from 
# https://www.env.gov.bc.ca/wsd/data_searches/water/
# November 18, 2021
###############################################################################
library(PBSmapping)
library(dplyr)
library(leaflet)
library(caTools)

# Define functions
lengthUnique <- function(x){length(unique(x))}

extractUnique <- function(x){
	u <- unique(x)
	if(length(u) == 1) return(u) else return(-99)}

SWPcol <- c(brown = "#663700", 
						red = "#B32008", 
						tan1 = "#FDC180",
						tan2 = "#E9DBCB", 
						greengrey1 = "#ECF1ED", 
						greengrey2 = "#DEE5DF",
						greengrey3 = "#BECCC1",
						greengrey4 = "#9FADA0")

SWPcol2 <- c(bluegrey = "#8DA8A1",
						 greengrey = "#8C9C8D",
						 redbrown = "#985A3F",
						 rose = "#CB7258",
						 tan = "#CFAD86")

#------------------------------------------------------------------------------
# Stitch together data from multiple years/archives to yield daily means
#------------------------------------------------------------------------------

# dat1 <- read.csv("raw-data/BC Stream Temperature/WaterTemp_Archive.csv")
# dat2 <- read.csv("raw-data/BC Stream Temperature/WaterTemp.csv")
# dat3 <- read.csv("raw-data/BC Stream Temperature/WaterTemperature_Archive_2005Oct_2007Oct.csv")
# dat4 <- read.csv("raw-data/BC Stream Temperature/WaterTemperature.csv")
# 
# dat <- rbind(dat1, dat2, dat3[, 2:11], dat4[, 2:11])
# 
# # Create date variable
# dat$Date <- as.Date(dat$Date.Time.UTC., format = "%Y-%m-%d %H:%M:%S")
# dat$Year <- as.numeric(strftime(dat$Date, format = "%Y"))
# 
# # Remove NAs
# dat <- dat[!is.na(dat$Value), ]
# 
# saveRDS(dat, "raw-data/BC Stream Temperature/WaterTemperature_Compiled.rds")

dat <- readRDS("data/raw-data/stream-temperature/BC Stream Temperature/WaterTemperature_Compiled.rds")


#------------------------------------------------------------------------------
# Summarize locations
#------------------------------------------------------------------------------

dat$LocDay <- paste(dat$Location.ID, dat$Date, sep = ":")

dailyDat <- data.frame(
	Location.ID = tapply(dat$Location.ID, dat$LocDay, unique),
  Date = tapply(dat$Date, dat$LocDay, unique),
	Tmean = tapply(dat$Value, dat$LocDay, mean),
	Tmin = tapply(dat$Value, dat$LocDay, min),
	Tmax = tapply(dat$Value, dat$LocDay, max),
	Nb = tapply(dat$Value, dat$LocDay, length)
)
dailyDat$Date <- as.Date(dailyDat$Date, origin = "1970-01-01")
dailyDat$Month <- as.numeric(strftime(dailyDat$Date, format = "%m"))
dailyDat$Year <- as.numeric(strftime(dailyDat$Date, format = "%Y"))
dailyDat$Day <- as.numeric(strftime(dailyDat$Date, format = "%d"))
dailyDat$locYear <- paste0(dailyDat$Location.ID, dailyDat$Year)


# extract site location info
Location.ID <- sort(unique(dailyDat$Location.ID))
ind <- match(Location.ID, dat$Location.ID)
siteDat <- data.frame(
	Location.ID = Location.ID,
	Location.Name = dat$Location.Name[ind],
	Latitude = dat$Latitude[ind],
	Longitude = dat$Longitude[ind],
	nObs = tapply(dat$Value, dat$Location.ID, length),
	nDays = tapply(dat$Date, dat$Location.ID, lengthUnique),
	nYears = as.numeric(tapply(dat$Year, dat$Location.ID, lengthUnique)),
	latestYear = as.numeric(tapply(dat$Year, dat$Location.ID, max))
	)

# Correct site that has lat/lon 0/0
siteDat$Longitude[siteDat$Longitude == 0] <- NA
siteDat$Latitude[siteDat$Latitude == 0] <- NA


#------------------------------------------------------------------------------
# Save RDS for compilation with other sources
#------------------------------------------------------------------------------
dailyDat_rds <- dailyDat[, c("Location.ID", "Year", "Month", "Day", "Tmax", "Tmin", "Tmean", "Nb")]
names(dailyDat_rds) <- c("StationID", "yr", "mo", "dd", "Tmax", "Tmin", "Tmean", "Nb")
saveRDS(dailyDat_rds, file = "compiled-data/stream-temp/dailyt_BC.rds")

# saveRDS(siteDat, file = "output/compiled-data/stream-temp/stations_BC.rds")

siteDat <- readRDS("output/compiled-data/stream-temp/stations_BC.rds")	
# View locations of data
leaflet() %>%
	addProviderTiles(providers$Esri.WorldTopoMap, options = providerTileOptions(noWrap = TRUE)
	) %>%
	addCircleMarkers(lat = siteDat$Latitude,
									 lng = siteDat$Longitude,
									 popup = siteDat$Location.Name,
									 labelOptions = labelOptions(
									 	noHide = TRUE,
									 	direction = "center",
									 	textOnly = TRUE,
									 	style = list("font-style" = "bold",
									 							 "font-size" = "10px")),#"color" = "white",
									 color = c("#DF536B","#0000FF")[as.numeric(siteDat$nYears > 2) + 1],
									 fillOpacity = 0.8,
									 opacity = 1,
									 radius = 3, #ifelse(dat$category[!is.na(dat$Latitude)] == 4, 6, 8),
									 stroke = TRUE,
									 weight = 1,
									 fillColor = c("#DF536B","#0000FF")[as.numeric(siteDat$nYears > 2) + 1]
	)


# Map for print
gshhg <- "~/Documents/Mapping/gshhg-bin-2.3.7/"
xlim <- c(-140, -114)
ylim <- c(48.17, 60.5)
# five resolutions: crude(c), low(l), intermediate(i), high(h), and full(f).
res <- "i"
land <- importGSHHS(paste0(gshhg,"gshhs_", res, ".b"), xlim = xlim + 360, ylim = ylim, maxLevel = 2, useWest = TRUE)
rivers <- importGSHHS(paste0(gshhg,"wdb_rivers_", res, ".b"), xlim = xlim  + 360, ylim = ylim, useWest = TRUE)
borders <- importGSHHS(paste0(gshhg,"wdb_borders_", res, ".b"), xlim = xlim  + 360, ylim = ylim, useWest = TRUE, maxLevel = 1)

# Plot
quartz(width = 5, height = 4, pointsize = 10)
plotMap(land, col = SWPcol['tan2'], bg = SWPcol['greengrey1'], las = 1, border = SWPcol['brown'], lwd = 0.6, ylim = c(48.2, 60.5), xlim = c(-135, -114), xlab = "Longitude", ylab = "Latitude")

addLines(rivers, col = SWPcol['brown'], lwd = 0.5)
addLines(borders, lwd = 1.5)

points(siteDat$Longitude, siteDat$Latitude, pch = 19, cex = 1.2, col = "#FFFFFF20")
points(siteDat$Longitude, siteDat$Latitude, cex = 1.2, col = SWPcol['red'])


# ###############################################################################
# # Spatiotemporal coverage
# ###############################################################################
# 
# # Create columns to store whether there's data for certain years
# yrs <- as.numeric(sort(unique(dat$Year)))
# cols <- length(names(siteDat)) + c(1:length(yrs))
# 	
# siteDat[, cols] <- NA
# names(siteDat)[cols] <- yrs
# 
# for(i in 1:nrow(siteDat)){
# 	for(j in 1:length(yrs)){
# 		q <- which(dat$Location.ID == siteDat$Location.ID[i] & dat$Year == yrs[j])
# 		if(length(q) > 0){
# 			dat.i <- dat[q, ]
# 			d <- unique(dat$Date[q])
# 			m <- as.numeric(unique(strftime(d, format = "%m")))
# 			if(sum(c(6:8) %in% m) > 0){
# 				siteDat[i, j + 3] <- 1 
# 			} else if(length(m > 4)){
# 				siteDat[i, j + 3] <- 2
# 			} else if(length(m > 1)){
# 				siteDat[i, j + 3] <- 3
# 			} else {
# 				siteDat[i, j + 3] <- 4
# 				}
# 		}
# 	}}
# 
# siteDat <- siteDat[order(siteDat$Latitude), ]
# plot(c(1, nrow(siteDat)), range(yrs), "n", ylab = "", xlab = "Location (South to North)", xaxs ="i", las = 1)
# for(i in 1:nrow(siteDat)){
# 	y <- as.numeric(is.na(siteDat[i, 4:(4 + length(yrs) - 1)]) == FALSE)
# 	y[y == 0] <- NA
# 	
# 	points(y*i, yrs, pch = 15, cex = 0.6, col = SWPcol2[c("rose", "tan", "greengrey", "bluegrey")][pmax(1, siteDat[i, 4:(4 + length(yrs) - 1)], na.rm = TRUE)])
# }
# 
# # Look at time series for the more covered sites
# i <- 35
# i = 100
# i <- 201
# plot(dat$Date[dat$Location.ID == aqDat$Location.ID[i]], dat$Value[dat$Location.ID == aqDat$Location.ID[i]], pch = 19, cex = 0.1, xlab = "", ylab = "Temperature (C)", las = 1)

###############################################################################
# Stream temperature proposed metrics
###############################################################################

#------------------------------------------------------------------------------
# Mean August temperature
#------------------------------------------------------------------------------
months <- 8 # Can change to 6:8 if wanting JJA

metric1 <- data.frame(
	Tmean = tapply(dailyDat$TW[dailyDat$Month %in% months], dailyDat$locYear[dailyDat$Month %in% months], mean),
	loc = tapply(dailyDat$Location.ID[dailyDat$Month %in% months], dailyDat$locYear[dailyDat$Month %in% months], unique),
	year = tapply(dailyDat$Year[dailyDat$Month %in% months], dailyDat$locYear[dailyDat$Month %in% months], mean))

# All years
quartz(width = 6, height = 4)
plot(metric1$year, metric1$Tmean, pch = 19, col = SWPcol2[c('bluegrey', 'rose')][as.numeric(metric1$Tmean >= 15) + 1], xlab = "Year", ylab =expression(paste("Mean August stream temperature (", degree, "C)")), las = 1)
abline(h = 15, lty = 2)

# Most recent years
dum <- tapply(metric1$year, metric1$loc, max)
ind <- which(paste(metric1$loc, metric1$year) %in% paste(names(dum), as.numeric(dum)))
quartz(width = 6, height = 4)
plot(metric1$year[ind], metric1$Tmean[ind], pch = 19, col = SWPcol2[c('bluegrey', 'rose')][as.numeric(metric1$Tmean[ind] >= 15) + 1], xlab = "Year", ylab =expression(paste("Mean August stream temperature (", degree, "C)")), las = 1)
abline(h = 15, lty = 2)

b <- seq(2, 28, 1)
par(mfrow = c(1,1), mar = c(4,4,2,1))
hist(metric1$Tmean[ind], las = 1, col = SWPcol2[c('bluegrey', 'rose')][as.numeric(b >= 15) + 1], main = "Mean August (n = 255)", xlab = expression(paste("Mean August stream temperature (", degree, "C)")), breaks = b)
abline(v = 15, lty = 2, lwd = 2)

# Most recent year within last 5
ind2 <- ind[which(metric1$year[ind] >= 2017)]

thresh <- 18

plot(siteDat$Latitude[match(metric1$loc[ind2], siteDat$Location.ID)], metric1$Tmean[ind2], col = SWPcol2[c('bluegrey', 'rose')][as.numeric(metric1$Tmean[ind2] >= thresh) + 1], xlab = "Latitude", ylab = expression(paste("Mean August stream temperature (", degree, "C)")), las = 1, pch = 19)
abline(h = thresh, lty = 2)


# Plot on map
quartz(width = 5, height = 4, pointsize = 10)
plotMap(land, col = grey(0.8), bg = "white", las = 1, border = grey(0.4), lwd = 0.6, ylim = c(48.2, 58), xlim = c(-131, -114), xlab = "Longitude", ylab = "Latitude")

addLines(rivers, col = grey(0.4), lwd = 0.5)
addLines(borders, lwd = 1.5)

points(aqDat$Longitude, aqDat$Latitude, pch = 21, bg = "white", col = 1)

siteDat$AugT <- metric1$Tmean[ind2][match(siteDat$Location.ID, metric1$loc[ind2])]
points(siteDat$Longitude[siteDat$AugT < thresh], siteDat$Latitude[siteDat$AugT < thresh], pch = 21, col = 1,  bg = SWPcol2['bluegrey'])
points(siteDat$Longitude[siteDat$AugT >= thresh], siteDat$Latitude[siteDat$AugT >= thresh], pch = 21, col = 1,  bg = SWPcol2['rose'])


#--------#
# Focus on specific location
#--------#

# Plot with leaflet
leaflet() %>%
	addProviderTiles(providers$Esri.WorldTopoMap, options = providerTileOptions(noWrap = TRUE)
	) %>%
	addCircleMarkers(lat = siteDat$Latitude,
									 lng = siteDat$Longitude,
									 popup = paste(siteDat$Location.ID, siteDat$nYears, sep = "-"),
									 labelOptions = labelOptions(
									 	noHide = TRUE,
									 	direction = "center",
									 	textOnly = TRUE,
									 	style = list("font-style" = "bold",
									 							 "font-size" = "10px")),#"color" = "white",
									 color = c("#8DA8A1", "#CB7258")[as.numeric(siteDat$AugT > thresh) + 1],
									 fillOpacity = 0.8,
									 opacity = 1,
									 radius = 3, #ifelse(dat$category[!is.na(dat$Latitude)] == 4, 6, 8),
									 stroke = TRUE,
									 weight = 1,
									 fillColor = c("#8DA8A1", "#CB7258")[as.numeric(siteDat$AugT > thresh) + 1]
	)

chosenLoc <- "MAX003"			
dat.i <- dat[which(dat$Location.ID == chosenLoc), ]
dailyDat.i <- dailyDat[which(dailyDat$Location.ID == chosenLoc), ]

plot(dat.i$Date, dat.i$Value, "n", las = 1, xlab = "", ylab = expression(paste("Stream temperature (", degree, "C)")))
for(i in unique(dailyDat.i$Year)){
	polygon(x = c(as.Date(paste(i, 08, 01, sep = "-")), as.Date(paste(i, 08, 01, sep = "-")), as.Date(paste(i, 08, 31, sep = "-")), as.Date(paste(i, 08, 31, sep = "-"))), y = c(-10, 30, 30, -10), border = NA, col = SWPcol2['tan'])
}
# points(dat.i$Date, dat.i$Value, pch = 19, cex = 0.05, col = c("#8DA8A1", "#CB7258")[as.numeric(dat.i$Value > thresh) + 1])
abline(h = thresh, lty = 2)
points(dailyDat.i$Date, dailyDat.i$TW, pch = 19, cex = 0.1)
points(as.Date(paste(metric1$year[metric1$loc == chosenLoc], 08, 15, sep = "-")), metric1$Tmean[metric1$loc == chosenLoc], pch = 1, col =  c("#8DA8A1", "#CB7258")[as.numeric(metric1$Tmean[metric1$loc == chosenLoc] > thresh) + 1], lwd = 2)


#------------------------------------------------------------------------------
# Maximum 7-day average of the daily maxima
#------------------------------------------------------------------------------

# Focus on single location for example
metric2 <- data.frame(
	DOY = rep(c(1:92), 9),
	year = rep(c(2009:2014, 2016:2018), each = 92),
	DM7DA = NA
)
for(y in c(2009:2014, 2016:2018)){
	
	dum <- dailyDat.i[which(dailyDat.i$Date >= as.Date(paste(y, "06", "25", sep = "-")) & dailyDat.i$Date <= as.Date(paste(y, "09", "30", sep = "-"))), ]
	
	if(nrow(dum) < 92){ stop("Missing days")} else {
		# dumDates <- c(as.Date(paste(y, "06", "25", sep = "-")):as.Date(paste(y, "09", "30", sep = "-")))
		# 
		# dum2 <- match(dumDates, dum$Date)
		
		metric2$DM7DA[metric2$year == y] <- runmean(dum$Tmax, k = 7, endrule = "trim", align = "right")
	}
}	


plot(dat.i$Date, dat.i$Value, "n", las = 1, xlab = "", ylab = expression(paste("Stream temperature (", degree, "C)")), xlim = as.Date(c("2016-06-01", "2018-09-30")))
for(i in unique(dailyDat.i$Year)){
	polygon(x = c(as.Date(paste(i, 07, 01, sep = "-")), as.Date(paste(i, 07, 01, sep = "-")), as.Date(paste(i, 09, 30, sep = "-")), as.Date(paste(i, 09, 30, sep = "-"))), y = c(-10, 30, 30, -10), border = NA, col = grey(0.8))
}
abline(h = thresh, lty = 2)
points(as.Date(paste(metric2$year, metric2$DOY + as.numeric(strftime(as.Date("2020-06-30"), format = "%j")), sep = "-"), format = "%Y-%j"), metric2$DM7DA, col = SWPcol2['rose'], pch = 19, cex = 0.2)
lines(dailyDat.i$Date, dailyDat.i$TW, lwd = 0.5)

points(as.Date(paste(unique(metric2$year), 08, 15, sep = "-")), tapply( metric2$DM7DA, metric2$year, max), lwd = 2, col = SWPcol2['rose'])

points(as.Date(paste(metric1$year[metric1$loc == chosenLoc], 08, 15, sep = "-")), metric1$Tmean[metric1$loc == chosenLoc], pch = 1, col = 1, lwd = 2)



###############################################################################
###############################################################################
###############################################################################

# OLDER CODE

###############################################################################
###############################################################################
###############################################################################

# Also plot min and max

# Which site has the most years?
x <- tapply(dailyDat$Year[dailyDat$m %in% c(6:8)], dailyDat$Location.ID[dailyDat$m %in% c(6:8)], unique) # GOA001
unlist(lapply(x, length))

chosenLoc <- "CHE001"			
			
y <- list(
	mean = tapply(dailyDat$TW[dailyDat$Location.ID == chosenLoc & dailyDat$m %in% c(6:8)], dailyDat$locYear[dailyDat$Location.ID == chosenLoc & dailyDat$m %in% c(6:8)], mean),
	max = tapply(dailyDat$TW[dailyDat$Location.ID == chosenLoc & dailyDat$m %in% c(6:8)], dailyDat$locYear[dailyDat$Location.ID == chosenLoc & dailyDat$m %in% c(6:8)], max),
	min = tapply(dailyDat$TW[dailyDat$Location.ID == chosenLoc & dailyDat$m %in% c(6:8)], dailyDat$locYear[dailyDat$Location.ID == chosenLoc & dailyDat$m %in% c(6:8)], min))
	
xDate <- tapply(dailyDat$Year[dailyDat$Location.ID == chosenLoc & dailyDat$m %in% c(6:8)], dailyDat$locYear[dailyDat$Location.ID == chosenLoc & dailyDat$m %in% c(6:8)], mean)
	
plot(xDate, y$mean, pch = 19, col = SWPcol2[c('bluegrey', 'rose')][as.numeric(y$mean >= 19) + 1], xlab = "Year", ylab =expression(paste("Avg. JJA stream temperature (", degree, "C)")), las = 1, ylim = range(JJA_all))
points(xDate, y$max, pch = 2, col = SWPcol2[c('bluegrey', 'rose')][as.numeric(y$max >= 19) + 1])
points(xDate, y$min, pch = 6, col = SWPcol2[c('bluegrey', 'rose')][as.numeric(y$min >= 19) + 1])
abline(h = 19, lty = 2)

# Daily mean 
plot(dailyDat$Date[dailyDat$Year == 2020 & dailyDat$Location.ID == chosenLoc & dailyDat$m %in% c(6:8)], dailyDat$TW[dailyDat$Year == 2020 & dailyDat$Location.ID == chosenLoc& dailyDat$m %in% c(6:8)], "o", pch = 19, cex = 0.6, xlab = "Date (2020)", ylab = expression(paste("Daily mean/min/max (", degree, "C)")), ylim = c(0, 28))

points(dailyDat$Date[dailyDat$Year == 2020 & dailyDat$Location.ID == chosenLoc& dailyDat$m %in% c(6:8)], dailyDat$Tmin[dailyDat$Year == 2020 & dailyDat$Location.ID == chosenLoc& dailyDat$m %in% c(6:8)], pch = 2, col = SWPcol2[c('bluegrey', 'rose')][as.numeric(dailyDat$Tmin[dailyDat$Year == 2020 & dailyDat$Location.ID == chosenLoc& dailyDat$m %in% c(6:8)] >= 19) + 1])

points(dailyDat$Date[dailyDat$Year == 2020 & dailyDat$Location.ID == chosenLoc& dailyDat$m %in% c(6:8)], dailyDat$Tmax[dailyDat$Year == 2020 & dailyDat$Location.ID == chosenLoc& dailyDat$m %in% c(6:8)], pch = 2, col = SWPcol2[c('bluegrey', 'rose')][as.numeric(dailyDat$Tmax[dailyDat$Year == 2020 & dailyDat$Location.ID == chosenLoc& dailyDat$m %in% c(6:8)] >= 19) + 1])

abline(v = as.Date(paste(2020, c(6, 8), c(01, 31), sep = "-")), lty = 2)
for(i in 1:3) segments(x0 = as.Date("2020-06-01"), x1= as.Date("2020-08-31"), y0 = tail(y[[i]], 1), y1 = tail(y[[i]], 1), col = SWPcol2[c('bluegrey')], lwd = 2)

# Daily mean 
dat2 <- dat[which(dat$Location.ID == chosenLoc), ]
dim(dat2)
unique(dat2$Year)


###############################################################################
# Summary JJA daily mean temperature for most recent year
###############################################################################

aqDat$JJA <- NA
aqDat$JJA_year <- NA
aqDat$JJA_numDays <- NA

for(i in 1:nrow(aqDat)){
	dati <- dailyDat[dailyDat$Location.ID == aqDat$Location.ID[i], ]
	dati$m <- as.numeric(strftime(dati$Date, format = "%m"))
	datii <- dati[dati$m %in% c(6:8), ]
	if(nrow(datii) > 0){
		datii$year <- as.numeric(strftime(datii$Date, format = "%Y"))
		datiii <- datii[datii$year == max(datii$year), ]
		
		aqDat$JJA[i] <- mean(datiii$TW)
		aqDat$JJA_year[i] <- unique(datiii$year)
		aqDat$JJA_numDays[i] <- length(datiii$TW)
		}
}

# Hist JJA
b <- seq(2, 28, 1)
par(mfrow = c(1,1), mar = c(4,4,2,1))
hist(aqDat$JJA, las = 1, col = SWPcol2[c('bluegrey', 'rose')][as.numeric(b >= 19) + 1], main = "Avg. in JJA (n = 259)", xlab = "Stream temperature (C)", breaks = b)
abline(v = 19, lty = 2, lwd = 2)

plot(aqDat$Latitude, aqDat$JJA, xlim = c(48, 55), col = SWPcol2[c('bluegrey', 'rose')][as.numeric(aqDat$JJA >= 19) + 1], lwd = 1.5)

plot(aqDat$JJA_year, aqDat$JJA, pch = 19, col = SWPcol2[c('bluegrey', 'rose')][as.numeric(aqDat$JJA >= 19) + 1], xlab = "Year", ylab =expression(paste("Avg. JJA stream temperature (", degree, "C)")), las = 1)
abline(h = 19, lty = 2)

# Plot on map
aqDat2 <- aqDat[!is.na(aqDat$JJA), ]

quartz(width = 5, height = 4, pointsize = 10)
plotMap(land, col = grey(0.8), bg = "white", las = 1, border = grey(0.4), lwd = 0.6, ylim = c(48.2, 58), xlim = c(-131, -114), xlab = "Longitude", ylab = "Latitude")

addLines(rivers, col = grey(0.4), lwd = 0.5)
addLines(borders, lwd = 1.5)

points(aqDat$Longitude, aqDat$Latitude, pch = 21, bg = "white", col = 1)

points(aqDat2$Longitude[aqDat2$JJA < 19], aqDat2$Latitude[aqDat2$JJA < 19], pch = 21, col = 1,  bg = SWPcol2['bluegrey'])
points(aqDat2$Longitude[aqDat2$JJA >= 19], aqDat2$Latitude[aqDat2$JJA >= 19], pch = 21, col = 1,  bg = SWPcol2['rose'])

points(aqDat$Longitude[aqDat$Location.ID == eg], aqDat$Latitude[aqDat$Location.ID == eg], pch = 8)
aqDat[aqDat$Location.ID == eg, ]
aqDat[which(aqDat$JJA > 19),]
eg <- "08NM0044"
JJAi <- JJA_all[JJA_all$loc == eg, ]

plot(dat$Date[dat$Location.ID == eg], dat$Value[dat$Location.ID == eg], "l", las = 1, xlab= "", ylab = "Temperature (C)")
lines(dailyDat$Date[dailyDat$Location.ID == eg], dailyDat$TW[dailyDat$Location.ID == eg], col = SWPcol2['bluegrey'], lwd = 1.5)
abline(h = 19, lty = 2)
for(i in 1:nrow(JJAi)) segments(x0=as.Date(paste(JJAi$year[i], 06, 01, sep = "-")), as.Date(paste(JJAi$year[i], 08, 31, sep = "-")), y0 = JJAi$Tmean[i], y1 = JJAi$Tmean[i], col = SWPcol2['rose'], lwd = 3)
			

mean(dailyDat$TW[dailyDat$Location.ID == eg & dailyDat$Year == 2020 & dailyDat$m %in% c(6:8)])												

# Proportion of days over 19*C in JJA

tw <- dailyDat$TW[dailyDat$Location.ID == eg & dailyDat$Year == 2020 & dailyDat$m %in% c(6:8)]

plot(dailyDat$Date[dailyDat$Location.ID == eg & dailyDat$Year == 2020 & dailyDat$m %in% c(6:8)], cumsum(tw > 19), "l", xlab = "", ylab = "Cumulative days > 19*C")


cumD <- tapply(dailyDat$TW[dailyDat$Year == 2020 & dailyDat$m %in% c(6:8)] > 19, dailyDat$Location.ID[dailyDat$Year == 2020 & dailyDat$m %in% c(6:8)], sum)

hist(cumD, breaks = c(0:80), main = "", xlab = "Cumulative days >19*C")

propD <- tapply(dailyDat$TW[dailyDat$Year == 2020 & dailyDat$m %in% c(6:8)] > 19, dailyDat$Location.ID[dailyDat$Year == 2020 & dailyDat$m %in% c(6:8)], sum)/tapply(dailyDat$TW[dailyDat$Year == 2020 & dailyDat$m %in% c(6:8)], dailyDat$Location.ID[dailyDat$Year == 2020 & dailyDat$m %in% c(6:8)], length)

hist(propD, breaks = seq(0, 1, 0.05))

################################################################################
# Trend in JJA temp
################################################################################

# Require at least three years?

head(JJA_all)
nYears <- tapply(JJA_all$Tmean, JJA_all$loc, length)

hist(nYears, breaks = c(1:20), xlab = "Number of years", main = "Number of JJA estimates for 259 locations")

length(nYears)
length(which(nYears >=5))

ind <- which(nYears > 10)

par(mfrow = c(3,1))
for(i in 1:length(ind)){
	JJAi <- JJA_all[JJA_all$loc == names(nYears)[ind[i]], ]
	plot(JJAi$year, JJAi$Tmean)
	abline(lm(JJAi$Tmean ~ JJAi$year))
}