


# comments ----------------------------------------------------------------

#author: Michael Folkes, Canadian Department of Fisheries and Oceans, Nanaimo, BC, Canada (michael.folkes@dfo-mpo.gc.ca)

#this is an import of the Yukon River water temperature data series produced by Al von Finster (Whitehorse, Yukon) funded by the Yukon River Panel's R&E fund.

#this script merges two data files: the yukon river water temperature data series with station meta data to then allow data grouping by additional variables. 
#In particular, note the variable: station_seriesName, which aggregates stations of a common river-reach when one station was decommissioned but is located close to the currently active station. The metadata also includes geographic coordinates, basin, subbasin, and subsubbasin groupings (as developed by the Water Survey of Canada)

#when complete, the series will run to the end of 2022. Currently there is no plan for additional data collection past 2022.

#revised R scripts and data updates can be obtained from the website of the Pacific Salmon Commission (www.psc.org) or by contacting Michael Folkes


# setup -------------------------------------------------------------------

# rm(list=ls())

root <- "data/Yukon-Water-Temperature-Data-CSV/"
# data import -------------------------------------------------------------

data.station.df <- read.csv(paste0(root, "station.metadata.csv"), stringsAsFactors = FALSE)

data.long <- read.csv(file = paste0(root, "yukon_water_temperature_vonfinster.csv"), stringsAsFactors = FALSE,comment.char = "#")
data.long$datetime <- as.POSIXct(data.long$datetime_gmt , tz="GMT")

data.long.complete <- merge(data.long, data.station.df, by="stationID", sort = FALSE)

#row count should match:
nrow(data.long)
nrow(data.long.complete)
str(data.long.complete)

# From Steph: summarize daily temps
# Variables: StationID   yr mo dd Tmax Tmin Tmean Nb

sum(is.na(data.long$temperature)) # No NAs in dataset

dailyt_YT <- data.frame(StationID = NA, yr = NA, mo = NA, dd = NA, Tmax = NA, Tmin = NA, Tmean = NA, Nb = NA)
nS <- length(unique(data.long$stationID))

count <- 1
for(i in 1:nS){ # For each station
  data.longi <- data.long[which(data.long$stationID == unique(data.long$stationID)[i]), ]
  data.longi <- data.longi[order(data.longi$datetime_gmt), ] # Order oldest to newest
  data.longi$date <- strftime(data.longi$datetime_gmt, format = "%Y-%m-%d")
  
  uniqueDates <- unique(data.longi$date) 
  nD <- length(uniqueDates) # Number of days
  
  dailyt_YT[count:(count + nD - 1), ] <- data.frame(
    StationID = rep(unique(data.long$stationID)[i], nD),
    yr = as.numeric(strftime(uniqueDates, "%Y")),
    mo = as.numeric(strftime(uniqueDates, "%m")),
    dd = as.numeric(strftime(uniqueDates, "%d")),
    Tmax = as.numeric(tapply(data.longi$temperature, data.longi$date, max)),
    Tmin = as.numeric(tapply(data.longi$temperature, data.longi$date, min)),
    Tmean = as.numeric(tapply(data.longi$temperature, data.longi$date, mean)),
    Nb = as.numeric(tapply(data.longi$temperature, data.longi$date, length))
  )
  
  count <- count + nD
}

saveRDS(dailyt_YT, file = "output/dailyt_YT.rds")
