###############################################################################
# Compile daily temperature mean, min, max from Province and WSC into single
# dataset for viewing
###############################################################################


#------------------------------------------------------------------------------
# Daily water temps
#------------------------------------------------------------------------------

dat_BC <- readRDS("output/dailyt_BC.rds")
dat_WSC <- readRDS("output/dailyt_WSC.rds")
dat_YT <- readRDS("output/dailyt_YT.rds")

head(dat_BC)
head(dat_WSC)
head(dat_YT)

dat <- rbind(dat_WSC, dat_BC, dat_YT)

# Flesh out time series for each site, adding NAs for missing periods
length(unique(dat$StationID))
dat.full <- dat[1, ]
dat.full$date <- as.Date(paste(dat.full$yr, dat.full$mo, dat.full$dd, sep = "-"))
ind <- 1

for(i in 1:length(unique(dat$StationID))){ # For each station
  dat.i <- dat[dat$StationID == unique(dat$StationID)[i], ]
  dat.i$date <- as.Date(paste(dat.i$yr, dat.i$mo, dat.i$dd, sep = "-"))
  r <- range(dat.i$date)
  dd <- as.Date(r[1]:r[2], origin = "1970-01-01")
  dat.full[ind:(ind + length(dd) - 1), 'date'] <- dd
  dat.full[ind:(ind + length(dd) - 1), 'StationID'] <- unique(dat$StationID)[i]
  dat.full[c(ind:(ind + length(dd) - 1))[match(dat.i$date, dd)], c("Tmax", "Tmin", "Tmean", "Nb")] <- dat.i[, c("Tmax", "Tmin", "Tmean", "Nb")]
  
  ind <- ind + length(dd)
}

dat.full$yr <- as.numeric(strftime(dat.full$date, format = "%Y"))
dat.full$mo <- as.numeric(strftime(dat.full$date, format = "%m"))
dat.full$dd <- as.numeric(strftime(dat.full$date, format = "%d"))

saveRDS(dat.full, "output/dailyt.rds")
#------------------------------------------------------------------------------
# Station information
#------------------------------------------------------------------------------

stations_BC <- readRDS("output/stations_BC.rds")
stations_WSC <- readRDS("output/stations_WSC.rds")
stations_YT <- read.csv("data/Yukon-Water-Temperature-Data-CSV/station.metadata.csv")

names(stations_BC)[1:2] <- c("StationID", "StreamName")

stations_YT <- stations_YT[, c("stationID", "station_seriesName", "latitude", "longitude")]
names(stations_YT) <- c("StationID", "StreamName", "Latitude", "Longitude")
stations <- rbind(
	stations_BC[, c("StationID", "StreamName", "Latitude", "Longitude")],
	stations_WSC[, c("StationID", "StreamName", "Latitude", "Longitude")],
	stations_YT
)
stations$Source <- c(rep("BC", nrow(stations_BC)), rep("WSC", nrow(stations_WSC)), rep("PSC", nrow(stations_YT)))

saveRDS(stations, "output/stations.rds")

stations <- readRDS("output/stations.rds")
# ###############################################################################
# # Map sites
# ###############################################################################
# 
# prov <- readRDS("data/mapping/layers/BC_lowRes.rds")
# lakes <- readRDS("data/mapping/layers/waterbodies_lowRes.rds")
# rivers <- readRDS("data/mapping/layers/watercourse_lowRes.rds")
# 
# st_stations <- st_as_sf(stations[!is.na(stations$Latitude), ], coords = c("Longitude", "Latitude"), crs = 4326)
# 
# pdf(width = 8.5, height = 11, file = "compiled-data/stream-temp/map.pdf")
# plot(st_geometry(prov),xlim = c(-135, -115))
# plot(st_geometry(lakes), border = "#4472C4", col = "#DAE3F3", add = TRUE)
# plot(st_geometry(rivers), col = "#4472C4", add = TRUE)
# plot(st_stations['Source'], add = TRUE, lwd = 1.2, col = c(2,7)[as.numeric(as.factor(st_stations$Source))], pch = 19)
# legend("topright", pt.lwd = 1.2, col = c(2,7), pch = 19, c("BC", "WSC"))
# 
# 
# dev.off()
# 
# # Leaflet
# 
# # View locations of data
# leaflet() %>%
# 	addProviderTiles(providers$Esri.WorldTopoMap, options = providerTileOptions(noWrap = TRUE)
# 	) %>%
# 	addCircleMarkers(lat = stations$Latitude,
# 									 lng = stations$Longitude,
# 									 popup = paste(stations$Source, stations$StreamName, sep = "-"),
# 									 labelOptions = labelOptions(
# 									 	noHide = TRUE,
# 									 	direction = "center",
# 									 	textOnly = TRUE,
# 									 	style = list("font-style" = "bold",
# 									 							 "font-size" = "10px")),#"color" = "white",
# 									 color = c("#DF536B", "#0000FF")[as.numeric(as.factor(stations$Source))],
# 									 fillOpacity = 0.8,
# 									 opacity = 1,
# 									 radius = 3, #ifelse(dat$category[!is.na(dat$Latitude)] == 4, 6, 8),
# 									 stroke = TRUE,
# 									 weight = 1,
# 									 fillColor = c("#DF536B", "#0000FF")[as.numeric(as.factor(stations$Source))]
# 	)
# 
# 
# dat1 <- dat[dat$StationID == stations$StationID[stations$StreamName == "BELLA COOLA RIVER ABOVE HAMMER CREEK"], ]
# dat1$date <- as.Date(paste(dat1$yr, dat1$mo, dat1$dd, sep = "-"))
# plot(dat1$date, dat1$Tmean, "l", pch = 19, cex = 0.5)
# polygon(x = c(dat1$date, rev(dat1$date)), c(dat1$Tmin, rev(dat1$Tmax)), col = "#00000040", border= NA)
# 
