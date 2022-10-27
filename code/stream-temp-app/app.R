###############################################################################
# Packages
###############################################################################

library(shiny)
library(shinymanager)
library(shinyBS)
library(shinyWidgets)
library(leaflet)

###############################################################################
# Load data
###############################################################################

dat <- readRDS("data/dailyt.rds")
stations <- readRDS("data/stations.rds")

dat$date <- as.Date(paste(dat$yr, dat$mo, dat$dd, sep = "-"), format = "%Y-%m-%d")

src <- sort(unique(stations$Source))
ptCol <- c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e')[c(1:length(src))]


###############################################################################
# Define user interface
###############################################################################

ui <- fluidPage(
	
	tags$head(
		tags$style(HTML("hr {border-top: 1px solid #000000;}"))
	),
	
	HTML("<h2 style='background: #D9D9D9; padding: 20px'>Stream Temperature Data</h2>"),
	
	HTML("<p> These data are provided as-is by the Province of British Columbia (BC), the Water Survey of Canada (WSC), and downloaded from the <a href='https://www.yukonriverpanel.com/publications/data-sets/'>Pacific Salmon Commission (PSC)</a>. No quality assurance or quality control has been done and the accurancy of the data is not guaranteed. </p> <p> To view the timeseries for a specific station, click on the coloured circle on that map. The timeseries shows the daily mean temeprature (black line), and the daily range (minimum and maximum temperatures; grey polygon). </p>"),
	br(),
	br(),
	fluidRow(
		# Output: Leaflet map
		leafletOutput(outputId = "map", width = "100%", height = 600)
	),
	
	fluidRow(
		plotOutput(outputId = "dailyt")
	)
	)

###############################################################################
# Define server
###############################################################################

server <- function(input, output, session) {

  
	output$map <- renderLeaflet({
		leaflet() %>%
			addProviderTiles(providers$Esri.WorldTopoMap, options = providerTileOptions(noWrap = TRUE)) %>% 
			addCircleMarkers(
				lat = stations$Latitude, 
				lng = stations$Longitude, 
				layerId = stations$StationID, 
				popup = stations$StreamName,
				fillColor = ptCol[as.numeric(factor(stations$Source, levels = src))],
				fillOpacity = 0.5, 
				opacity = 1,
				radius = 5,
				weight = 1,
				color = ptCol[as.numeric(factor(stations$Source, levels = src))]) %>% addLegend(position = "topright", colors = ptCol, labels = src, opacity = 0.5)
	 }) 

	observeEvent(input$map_marker_click, { 
		p <- input$map_marker_click
		output$dailyt <- renderPlot({
			if(length(which(dat$StationID == p$id)) > 0){
			dat.p <- dat[dat$StationID == p$id, ]
			rangeD <- range(dat.p$date)
			mo <- strftime(rangeD, format = "%m")
			yr <- strftime(rangeD, format = "%Y")
			
			plot(dat.p$date, dat.p$Tmean, "n", xlab = "", ylab = expression(paste("Daily temperature (", degree, "C)")), las = 1, ylim = range(dat.p$Tmin, dat.p$Tmax), xaxt = "n")
			
			# Add pretty date axes
		
	
		for(i in 1:length(unique(dat.p$yr))){
			axis(side = 1, at = as.Date(paste(unique(dat.p$yr), "01", "01", sep = "-")), labels = unique(dat.p$yr), tck = -0.1)
			
			if(i == 1){
				axis(side = 1, at = as.Date(paste(yr[1], c(mo[1]:12), "01", sep = "-")), labels = FALSE, tck = -0.02)
				axis(side = 1, at = as.Date(paste(yr[1], seq(mo[1], 12, 2), "01", sep = "-")), labels = strftime(as.Date(paste(yr[1], seq(mo[1], 12, 2), "01", sep = "-")), format = "%b"), tck = -0.05, cex.axis =0.8, gap.axis = 0.25, las = 2)
				
			} else if(i == length(unique(dat.p$yr))){
				axis(side = 1, at = as.Date(paste(yr[2], c(1: mo[2]), "01", sep = "-")), labels = FALSE, tck = -0.02)
				axis(side = 1, at = as.Date(paste(yr[2], seq(1, mo[2], 2), "01", sep = "-")), labels = strftime(as.Date(paste(yr[2], seq(1, mo[2], 2), "01", sep = "-")), format = "%b"), tck = -0.05, cex.axis =0.8, gap.axis = 0.25, las = 2)
			} else {
				axis(side = 1, at = as.Date(paste(unique(dat.p$yr)[i], c(1:12), "01", sep = "-")), labels = FALSE, tck = -0.02)
				axis(side = 1, at = as.Date(paste(unique(dat.p$yr)[i], seq(1, 12, 2), "01", sep = "-")), labels = strftime(as.Date(paste(unique(dat.p$yr)[i], seq(1, 12, 2), "01", sep = "-")), format = "%b"), tck = -0.05, cex.axis =0.8, gap.axis = 0.25, las = 2)
			}
			# polygon(x = c(as.Date(paste(unique(dat.p$yr)[i], c(6,8), c("01", "31"), sep = "-")), rev(as.Date(paste(unique(dat.p$yr)[i], c(6,8), c("01", "31"), sep = "-")))), y = c(-1, -1, 60, 60), col = "#A9264930", border = NA)
		}
		
			polygon(x = c(dat.p$date, rev(dat.p$date)), y = c(dat.p$Tmin, rev(dat.p$Tmax)), border = NA, col = "#00000030")
			lines(dat.p$date, dat.p$Tmean)
			if(length(unique(dat.p$yr)) == 2){
				mtext(side = 1, line = 3, paste(unique(dat.p$yr)[1], unique(dat.p$yr)[2], sep = "/"))
			} else if(length(unique(dat.p$yr)) == 1){
			mtext(side = 1, line = 3, unique(dat.p$yr)[1])
			}
			} else {
				plot(1,1, "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
				text(1,1, "No data to display.")
			}
			mtext(side = 3, line = 1, paste0(p$id, ": ", stations$StreamName[which(stations$StationID == p$id)], " (", stations$Source[which(stations$StationID == p$id)], ")"))
		})
	})
	
}

###############################################################################
# Execute
###############################################################################

shinyApp(ui = ui, server = server)
