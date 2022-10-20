###############################################################################
# WSC temperature data provided to PSF via secure link in email
# after written request. Data were provided in .mdb (Microsoft Access) format.
# Katy opened the Access Database and extracted all tables as .csv
# Date: July 20, 2022
###############################################################################


###############################################################################
# Read in data
###############################################################################

path <- "data/WSC/"

# Daily temperature 
dailyt <- read.csv(paste0(path, "Envcanada_dailyt_data_envcanada.csv"))

head(dailyt)

# Station info
stations <- read.csv(paste0(path, "Envcanada_ec_tw_stations_location.csv"))
head(stations)

# Extract stations in BC
stationsPac <- stations[stations$Province %in% c("BC", "YT"), ] # 153 stations in BC 36 in YT = 189 stations total

dailyt$StationName %in% stationsPac$StreamName
dailytPac <- dailyt[dailyt$StationName %in% stationsPac$StreamName, ]

# Explore other files
allFiles <- c(
	"Envcanada_dailyt_data_envcanada.csv", #1
	"Envcanada_ec_info_instrument.csv", #2
	"Envcanada_ec_tw_active_instrument.csv", #3
	"Envcanada_ec_tw_data_rawseuil.csv", #4
	"Envcanada_ec_tw_eval_q_data.csv", #5
	"Envcanada_ec_tw_instruments_access.csv", #6
	"Envcanada_ec_tw_instruments_type.csv", #7
	"Envcanada_ec_tw_province.csv", #8 StationID and Province
	"Envcanada_ec_tw_spot_measurements.csv", #9
	"Envcanada_ec_tw_spotmeasurement_stations.csv", #10
	"Envcanada_ec_tw_stations_location.csv" #11
)

z <- read.csv(paste0(path, allFiles[8]))
head(z)

stationID_BC <- z$StationID[z$Province == "BC"]

# Does a Riv have more than one StationID?
lu <- function(x) length(unique(x))
dum <- tapply(stations$StationID, stations$Riv, lu)
unique(dum) # Yes, some have up to 8 stations


sort(unique(stations$StreamName))[1:10]
sort(unique(dailyt$StationName))[1:20]


###############################################################################
# Extract BC stations
###############################################################################

# StationsName = StreamName

dailytPac$StationID <- stations$StationID[match(dailytPac$StationName, stations$StreamName)]

dailytPac <- dailytPac[, c("StationID", "an_yr", "mo", "jj_dd", "Tmax", "Tmin", "Tmean", "Nb")]
names(dailytPac) <- c("StationID", "yr", "mo", "dd", "Tmax", "Tmin", "Tmean", "Nb")

stationsPac <- stationsPac[, c("StationID", "StreamName", "Riv", "Latitude", "Longitude", "etat")]

saveRDS(dailytPac, file = "output/dailyt_WSC.rds")
saveRDS(stationsPac, file = "output/stations_WSC.rds")

