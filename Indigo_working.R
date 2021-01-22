library(raster)
library(sp)
library(rgdal)
library(sf)
library(ggplot2)
library(geosphere)
source("C:/Users/ethan/Desktop/data_tools/functions/data_manager.R")
source("C:/Users/ethan/Desktop/data_tools/functions/localization.R")

###EDIT THESE VALUES
infile <- "C:/Users/ethan/Dropbox/Flat Tub/Data/SS/!Need-to-be-processed/sectioned-by-grid/"
outpath <- "C:/Users/ethan/Dropbox/Flat Tub/Data/SS/!Results/CTT RStudio/"

tags <- read.csv("C:/Users/ethan/Dropbox/Flat Tub/Data/SS/csv/snake_tags.csv", as.is=TRUE, na.strings=c("NA", ""), colClasses="character") #colClasses="character" addresses letter "E" in Node and Tag IDs

all_data <- load_data(infile)
#load("indigo.RData")
beep_data <- all_data[[1]][[1]]
#beep_data <- beep_data[beep_data$Time > as.POSIXct("2020-08-10"),]

#nodes <- node_file(all_data[[2]][[1]])
###looking for a file with the column names NodeId, lat, lng IN THAT ORDER

myfreqs <- c("1 min", "15 min", "30 min", "1 hour", "1 day")
#myfreqs <- c("1 day")

dates <- list(
  c("2020-03-13", "2020-04-10"),
  c("2020-04-11", "2020-04-16"),
  c("2020-04-17", "2020-04-24"),
  c("2020-04-25", "2020-06-22"), 
  c("2020-06-23", "2020-07-13"),
  c("2020-07-14", "2020-07-23"),
  c("2020-07-24", "2020-10-13"),
  c("2020-10-14", "2021-01-04"),
  c("2021-01-05", "2021-01-06"),
  c("2021-01-07", "2021-01-11"),
  c("2021-01-12", "2021-01-18")#,
  #c("2021-01-19", "2021-01-20")
)

freqs <- lapply(myfreqs, function(myfreq) {
  all2 <- Map(function(df, g) {
    thisfile <- list.files("C:/Users/ethan/Dropbox/Flat Tub/Data/SS/csv/Nodes", full.names = TRUE)[df]
    print(thisfile)
    nodes <- read.csv(thisfile, as.is=TRUE, na.strings=c("NA", ""), strip.white=TRUE) #uppercase node letters
    print(df)
    if(df > 6) {
      colnames(nodes)[colnames(nodes)=="ï..NodeId"] <- "NodeId" #address issues with excel to R
      #  colnames(nodes)[colnames(nodes)=="Latitude"] <- "lat"
      #  colnames(nodes)[colnames(nodes)=="Longitude"] <- "lng"
    }
    nodes <- nodes[,c("NodeId", "lat", "lng")]
    nodes$NodeId <- toupper(nodes$NodeId)
    
    mybeep_data <- beep_data[beep_data$NodeId %in% nodes$NodeId,] #c("326317", "326584", "3282fa", "3285ae", "3288f4")
    starttime <- as.POSIXct(paste(g[1], "00:00:00"), tz="UTC")
    print(starttime)
    endtime <- as.POSIXct(paste(g[2], "23:59:59"), tz="UTC")
    print(endtime)
    mybeep_data <- mybeep_data[mybeep_data$Time > starttime & mybeep_data$Time < endtime,]
    
    ###UNCOMMENT THESE AND FILL WITH YOUR DESIRED VALUES IF YOU WANT YOUR OUTPUT AS ONLY A SUBSET OF THE DATA
    #channel <- a vector of RadioId value(s)
    #tag_id <- a vector of TagId value(s)
    #n_tags <- how many tags go into the "top tags"
    #freq <- "5 min" The interval of the added datetime variable. Any character string that would be accepted by seq.Date or seq.POSIXt
    
    #EXAMPLE POSSIBLE VALUES
    tag_id <- tags$TagId
    #
    #channel <- c(2)
    
    #resampled <- advanced_resampled_stats(beep_data = beep_data, node = nodes, freq = freq[1], tag_id = tag_id)
    #p3 = ggplot(data=resampled, aes(x=freq, y=max_rssi, group=NodeId, colour=NodeId)) +
    #  geom_line()
    thishealth <- all_data[[2]][[1]]
    thishealth <- thishealth[thishealth$Time > starttime & thishealth$Time < endtime,]
    resampled <- advanced_resampled_stats(beeps = mybeep_data, node = nodes, freq = myfreq, tag_id = tag_id)
    locations <- weighted_average(myfreq, mybeep_data, nodes, tag_id = tag_id)
    #multi_freq <- lapply(freq, weighted_average, beeps=beep_data, node=nodes) 
    #export_locs(freq, beep_data, nodes, tag_id, outpath)
    
    n <- 3 #this is an example of filtering out locations based on a minimum number of nodes
    #locations <- locations[locations$unique_nodes > n,]
    
    #locations$ID <- paste(locations$TagId, locations$freq, sep="_")
    #locations <- locations[!duplicated(locations$ID),]
    locations <- cbind(locations, locations@coords)
    loc_df <- locations@data
    return(list(loc_df, resampled))}, c(8), dates)
  getlocs <- lapply(all2, "[[", 1)
  locs <- rbindlist(getlocs)
  
  getlocs <- lapply(all2, "[[", 2)
  summstats <- rbindlist(getlocs)
  write.csv(locs,paste(outpath,gsub(" ", "", paste("estimates_",myfreq,".csv",sep=""), fixed = TRUE),sep=""))
  return(list(locs, summstats))})

houdini_locs <- freqs[[1]][[1]][freqs[[1]][[1]]$TagId == "66665578",]

houdini <- freqs[[1]][[2]][freqs[[1]][[2]]$TagId == "66665578",]

tsr <- readOGR(dsn="../data/indigo/Indigo_telemetry_082020_cumulative", layer="Indigo_telemetry_082020_cumulative")
tsr@data$Time <- as.POSIXct(tsr@data$time, tz = "UTC") 
tsr <- tsr[tsr@data$Time > as.POSIXct("2020-03-13", tz="UTC") & tsr@data$Time < as.POSIXct("2020-10-13", tz="UTC"),]
houdini_real <- tsr[tsr@data$Name == "Houdini",]
houdini_times <- as.Date(as.POSIXct(as.character(houdini_real$time), tz="UTC"))

houdini_raw <- beep_data[beep_data$TagId == "66665578",]
houdini_raw$date <- as.Date(houdini_raw$Time)
houdini_check <- houdini_raw[houdini_raw$date %in% houdini_times,]

real <- st_as_sf(tsr)

boulder_df <- freqs[[1]][[1]][,c("TagId","avg_x","avg_y")]
coordinates(boulder_df) <- 2:3
crs(boulder_df) <- CRS("+proj=longlat +datum=WGS84") 
#utm <- CRS(paste0("+proj=utm +zone=", locations$zone[1], "+datum=WGS84"))
#crs(boulder_df) <- utm
#boulder_df_geog <- spTransform(locations, proj4string(nodes_spatial))
#my_locs <- locations[,1]
locs <- st_as_sf(boulder_df)
#my_nodes <- st_as_sf(nodes_spatial)

library(ggrepel)
#library(ggsflabel)
ggplot() + 
  #geom_point(data=my_locs, aes(x=long,y=lat))
  #  ggmap(ph_basemap) +
  geom_sf(data = locs, aes(colour=TagId)) +
  geom_sf(data = real, aes(colour=Name), size = 4) + 
  geom_text_repel(data = tsr@data, aes(x = coordinates(tsr)[,1], y = coordinates(tsr)[,2], label = time))
#+
#geom_text(data = nodes, aes(x=lng, y=lat, label = NodeId), size = 5)

