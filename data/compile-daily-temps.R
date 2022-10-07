###############################################################################
# Compile daily temperature mean, min, max from province and WSC into single
# dataset for viewing
###############################################################################


#------------------------------------------------------------------------------
# Daily water temps
#------------------------------------------------------------------------------

dat_BC <- readRDS("output/dailyt_BC.rds")
dat_WSC <- readRDS("output/dailyt_WSC.rds")

head(dat_BC)
head(dat_WSC)

dat <- rbind(dat_WSC, dat_BC)

saveRDS(dat, "output/dailyt.rds")
#------------------------------------------------------------------------------
# Station information
#------------------------------------------------------------------------------

stations_BC <- readRDS("output/stations_BC.rds")
stations_WSC <- readRDS("coutput/stations_WSC.rds")

names(stations_BC)[1:2] <- c("StationID", "StreamName")

stations <- rbind(
	stations_BC[, c("StationID", "StreamName", "Latitude", "Longitude")],
	stations_WSC[, c("StationID", "StreamName", "Latitude", "Longitude")]
)
stations$Source <- c(rep("BC", nrow(stations_BC)), rep("WSC", nrow(stations_WSC)))

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
