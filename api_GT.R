start <- Sys.time()
library(httr)
library(DBI)
library(RSQLite)
library(data.table)
outpath <- "C:/Users/ethan/Dropbox/Flat Tub/Data/SS/!Need-to-be-processed/"
setwd(outpath)
myfiles <- list.files(outpath, recursive = TRUE)
files_loc <- sapply(strsplit(myfiles, "/"), tail, n=1)
my_token <- "4a9b1d7d408c122af81bbb8442fa89c0ca44907b4775eedb760edb3d677dac94"

#github_pat <- function() {
#  pat <- Sys.getenv('GITHUB_PAT')
#  if (identical(pat, "")) {
#    stop("Please set env var GITHUB_PAT to your github personal access token",
#         call. = FALSE)
#  }
  
#  pat
#}

host <- 'https://account.celltracktech.com'
project <- '/station/api/projects/'
stations <- '/station/api/stations/'
files <- '/station/api/file-list/'
file_types <- c("data", "node-data", "gps", "log", "telemetry", "sensorgnome")

post <- function(endpoint, payload=NULL) {
  payload_to_send <- list(token=my_token)
  if (!is.null(payload)) {
    payload_to_send <- c(payload_to_send, payload)
  }
  response <- POST(host, path = endpoint, body=payload_to_send,encode="json") 
  stop_for_status(response)
  return(content(response))
}

projects <- content(POST(host, path = project, body = list(token=my_token), encode="json", verbose()), "parsed")

getStations <- function(project_id) {
  out <- post(endpoint=stations, payload=list("project-id"=project_id))
  return(out)
}

conn <- dbConnect(RSQLite::SQLite(), "CTT.db")
dbExecute(conn, "CREATE TABLE IF NOT EXISTS ctt_project
  (
    id	INTEGER PRIMARY KEY,
    name	TEXT NOT NULL UNIQUE
  )")
          #
dbExecute(conn, "CREATE TABLE IF NOT EXISTS station
  (
    station_id	TEXT PRIMARY KEY
  )")

dbExecute(conn, "CREATE TABLE IF NOT EXISTS nodes
  (
    NodeId TEXT NOT NULL PRIMARY KEY
  )")

dbExecute(conn, "CREATE TABLE IF NOT EXISTS data_file
  (
    path TEXT PRIMARY KEY
  )")

dbExecute(conn, "CREATE TABLE IF NOT EXISTS ctt_project_station 
  (
    db_id	INTEGER PRIMARY KEY,
    project_id	INTEGER NOT NULL,
    station_id	TEXT NOT NULL,
    deploy_at	DATETIME,
    end_at	DATETIME,
    FOREIGN KEY (project_id) 
      REFERENCES ctt_project (id) 
        ON DELETE NO ACTION
        ON UPDATE NO ACTION,
    FOREIGN KEY (station_id) 
      REFERENCES station (station_id) 
        ON DELETE NO ACTION
        ON UPDATE NO ACTION
  )")
  
  #    id	INTEGER PRIMARY KEY, --Autoincrement
dbExecute(conn, "CREATE TABLE IF NOT EXISTS raw 
  (
    path  TEXT,
    RadioId INTEGER,
    TagId TEXT,
    NodeId TEXT,
    TagRSSI INTEGER,
    Validated INTEGER,
    Time DATETIME,
    station_id TEXT,
    PRIMARY KEY (TagId, Time, RadioId, NodeId, station_id),
    FOREIGN KEY (station_id) 
      REFERENCES station (station_id) 
        ON DELETE NO ACTION
        ON UPDATE NO ACTION,
    FOREIGN KEY (NodeId) 
      REFERENCES nodes (NodeId) 
        ON DELETE NO ACTION
        ON UPDATE NO ACTION,
    FOREIGN KEY (path) 
      REFERENCES data_file (path) 
        ON DELETE NO ACTION
        ON UPDATE NO ACTION
  )")
  
  #    
  # id	INTEGER PRIMARY KEY, --Autoincrement
dbExecute(conn, "CREATE TABLE IF NOT EXISTS node 
  (
    path  TEXT,
    RadioId INTEGER,
    NodeId TEXT,
    NodeRSSI INTEGER,
    Battery NUMERIC,
    Time DATETIME,
    RecordedAt DATETIME,
    Firmware TEXT,
    SolarVolts NUMERIC,
    SolarCurrent NUMERIC,
    CumulativeSolarCurrent NUMERIC,
    Latitude NUMERIC,
    Longitude NUMERIC,
    Celsius NUMERIC,
    station_id TEXT,
    PRIMARY KEY (RadioId, NodeId, Time, station_id),
    FOREIGN KEY (NodeId) 
      REFERENCES nodes (NodeId) 
        ON DELETE NO ACTION
        ON UPDATE NO ACTION,
    FOREIGN KEY (path) 
      REFERENCES data_file (path) 
        ON DELETE NO ACTION
        ON UPDATE NO ACTION
  )")
  
  #    id	INTEGER PRIMARY KEY, --Autoincrement
dbExecute(conn, "CREATE TABLE IF NOT EXISTS gps
  (
    path  TEXT,
    latitude NUMERIC,
    longitude NUMERIC,
    altitude NUMERIC,
    quality INTEGER,
    gps_at DATETIME,
    recorded_at DATETIME,
    station_id TEXT,
    mean_lat NUMERIC,
    mean_lng NUMERIC,
    n_fixes INTEGER,
    PRIMARY KEY (gps_at, station_id),
    FOREIGN KEY (path) 
      REFERENCES data_file (path) 
        ON DELETE NO ACTION
        ON UPDATE NO ACTION
  )")
  
 # dbExecute(conn, "CREATE TABLE tag
#  (
#    id	TEXT PRIMARY KEY
#  )")

sapply(projects[['projects']], function(a) {
  b <- as.data.frame(a)
  vars <- paste(":",dbListFields(conn, "ctt_project"), sep="", collapse=",")
  insertnew <- dbSendQuery(conn, paste("INSERT OR IGNORE INTO ","ctt_project"," VALUES (",vars,")",sep=""))
  dbBind(insertnew, params=b)
  dbClearResult(insertnew)
})

getStationFileList <- function(station_id, begin, filetypes=NULL, end=NULL) {
  endpoint <- files
  payload <- list("station-id" = station_id, begin = as.Date(begin))
  if (!is.null(filetypes)) {
    add_types <- filetypes[filetypes %in% file_types]
    if(length(which(!filetypes %in% file_types)) > 0) {print(paste("WARNING: invalid file type specified - ignoring:",filetypes[!filetypes %in% file_types]))}
    payload[['file-types']] = add_types
  } 
  if (!is.null(end)) {payload[['end']] = as.Date(end)}
return(post(endpoint=endpoint, payload=payload))}

downloadFiles <- function(file_id) {
  endpoint <- "/station/api/download-file/"
  payload <- list("file-id"=file_id)
  return(post(endpoint=endpoint, payload=payload))
}

get_data <- function(project) {
  basename <- project$name
  id <- project[['id']]
  dir.create(file.path(outpath, basename), showWarnings = FALSE)
  my_stations <- getStations(project_id=id)
  mystations <- lapply(my_stations$stations, function(c) {
    c <- as.data.frame(t(unlist(c)), stringsAsFactors=FALSE)
    c$project_id <- id
    colnames(c)[colnames(c)=="station.db-id"] <- "db_id"
    colnames(c)[colnames(c)=="station.id"] <- "station_id"
    colnames(c)[colnames(c)=="deploy-at"] <- "deploy_at"
    if (is.null(c$`end-at`)) c$end_at <- NA
  return(c)})
  mystations <- rbindlist(mystations, fill=TRUE)

  vars <- paste(":",dbListFields(conn, "ctt_project_station"), sep="", collapse=",")
  insertnew <- dbSendQuery(conn, paste("INSERT OR IGNORE INTO ","ctt_project_station"," VALUES (",vars,")",sep=""))
  dbBind(insertnew, params=mystations)
  dbClearResult(insertnew)
  
  insertnew <- dbSendQuery(conn, paste("INSERT OR IGNORE INTO ","station (station_id)"," VALUES (?)",sep=""))
  dbBind(insertnew, params=list(unique(mystations$station_id)))
  dbClearResult(insertnew)
#station <- my_stations[['stations']][[35]]
#kwargs <- list(
#  station_id = station[["station"]][["id"]],
#  begin = as.POSIXct(station[['deploy-at']],format="%Y-%m-%dT%H:%M:%OS",tz = "UTC", optional=TRUE)
#)
#if(!is.null(station[['end-at']])) {
#  kwargs[['end']] = as.POSIXct(station[['end-at']],format="%Y-%m-%dT%H:%M:%OS",tz = "UTC", optional=TRUE)
#}
#file_info <- do.call(getStationFileList, kwargs)
#files = file_info[['files']]

  files_avail <- lapply(my_stations[["stations"]], function(station) {
    print(station)
    kwargs <- list(
      station_id = station[["station"]][["id"]],
      begin = as.POSIXct(station[['deploy-at']],format="%Y-%m-%dT%H:%M:%OS",tz = "UTC", optional=TRUE)
    )
  
    if(!is.null(station[['end-at']])) {
      kwargs[['end']] = as.POSIXct(station[['end-at']],format="%Y-%m-%dT%H:%M:%OS",tz = "UTC", optional=TRUE)
    }
    file_info <- do.call(getStationFileList, kwargs)
    outfiles <- file_info[['files']]
  return(outfiles)})
  
  filenames <- unname(rapply(files_avail, grep, pattern = "CTT", value=TRUE))
  files_to <- filenames[!filenames %in% files_loc]

  allfiles <- rapply(files_avail, function(z) z %in% files_to, how = "unlist")
  ids <- unlist(files_avail)[which(allfiles) - 1]
  file_names <- unlist(files_avail)[which(allfiles)]

  get_files <- function(x, y) {
    print(x)
    print(y)
    
    insertnew <- dbSendQuery(conn, paste("INSERT OR IGNORE INTO ","data_file (path)"," VALUES (?)",sep=""))
    dbBind(insertnew, params=list(y))
    dbClearResult(insertnew)

    splitfile <- unlist(strsplit(y, "CTT-"))
    fileinfo <- splitfile[2]
    sensorid <- unlist(strsplit(fileinfo,"-"))
    sensor <- sensorid[1]
    filenameinfo <- sensorid[2]
    file_info <- unlist(strsplit(filenameinfo, "\\."))[1]
    filetype <- ifelse(is.na(as.integer(file_info)),file_info,"sensorgnome") 
    
    if (filetype != "log" & filetype != "telemetry") {
      contents = downloadFiles(file_id = x)
      dir.create(file.path(outpath, basename, sensor), showWarnings = FALSE)
      dir.create(file.path(outpath, basename, sensor, filetype), showWarnings = FALSE)
      print(paste("downloading",y,"to",file.path(outpath, basename, sensor, filetype)))
      write.csv(contents, file=gzfile(file.path(outpath, basename, sensor, filetype, y)), row.names=FALSE)
      contents$station_id <- sensor
      contents$path <- y
    }
    
    if (filetype == "gps") {
      colnames(contents)[colnames(contents)=="recorded at"] <- "recorded_at"
      contents$recorded_at <- as.character(contents$recorded_at)
      colnames(contents)[colnames(contents)=="gps at"] <- "gps_at"
      contents$gps_at <- as.character(contents$gps_at)
      if ("mean lat" %in% colnames(contents)) {
        colnames(contents)[colnames(contents)=="mean lat"] <- "mean_lat"
        colnames(contents)[colnames(contents)=="mean lng"] <- "mean_lng"
        colnames(contents)[colnames(contents)=="n fixes"] <- "n_fixes"
      } else {
        contents$mean_lat <- NA
        contents$mean_lng <- NA
        contents$n_fixes <- NA
      }
    } else if (filetype == "raw") {
      contents$Time <- as.character(contents$Time)
      nodeids <- unique(contents$NodeId)
      insertnew <- dbSendQuery(conn, paste("INSERT OR IGNORE INTO ","nodes (NodeId)"," VALUES (?)",sep=""))
      dbBind(insertnew, params=list(unique(nodeids)))
      dbClearResult(insertnew)
    } else if (filetype == "node") {
      if("RecordedAt" %in% colnames(contents)) {
        contents$RecordedAt <- as.character(contents$RecordedAt)}
      else {
        contents$RecordedAt <- NA
        contents$Firmware <- NA
        contents$SolarVolts <- NA
        contents$SolarCurrent <- NA
        contents$CumulativeSolarCurrent <- NA
        contents$Latitude <- NA
        contents$Longitude <- NA
      }
      
      contents$Time <- as.character(contents$Time)
      nodeids <- unique(contents$NodeId)
      insertnew <- dbSendQuery(conn, paste("INSERT OR IGNORE INTO ","nodes (NodeId)"," VALUES (?)",sep=""))
      dbBind(insertnew, params=list(unique(nodeids)))
      dbClearResult(insertnew)
    } else {nodeids <- c()}
    if (filetype %in% c("raw", "node", "gps")) {
      vars <- paste(":",dbListFields(conn, filetype), sep="", collapse=",")
      insertnew <- dbSendQuery(conn, paste("INSERT OR IGNORE INTO ",filetype," VALUES (",vars,")",sep=""))
      dbBind(insertnew, params=contents)
      dbClearResult(insertnew)
    }
      #dbWriteTable(conn, filetype, contents, append=TRUE, row.names=FALSE)
  }
  Map(get_files, ids, file_names)
}

lapply(projects[['projects']], get_data)

#insertnew <- dbSendQuery(conn, paste("INSERT OR IGNORE INTO ","nodes (NodeId)"," VALUES (?)",sep=""))
#dbBind(insertnew, params=list(unique(allnodes)))
#dbClearResult(insertnew)

dbDisconnect(conn)
time_elapse <- Sys.time() - start

pop <- function(x) {
  allnode <- dbReadTable(x, "node")
  allgps <- dbReadTable(x, "gps")
  allbeep <- dbReadTable(x, "raw")
  insertnew <- dbSendQuery(conn, paste("INSERT OR IGNORE INTO ","data_file (path)"," VALUES (?)",sep=""))
  dbBind(insertnew, params=list(unique(c(allnode$path, allgps$path, allbeep$path))))
  dbClearResult(insertnew)
  
  insertnew <- dbSendQuery(conn, paste("INSERT OR IGNORE INTO ","nodes (NodeId)"," VALUES (?)",sep=""))
  dbBind(insertnew, params=list(unique(allnode$NodeId)))
  dbClearResult(insertnew)
}

update_db <- function(d) {
  allnode <- dbReadTable(d, "data_file")
  files_import <- myfiles[which(!files_loc %in% allnode$path)]
  lapply(files_import, get_files_import)
}

get_files_import <- function(e) {
  print(e)
  y <- tail(unlist(strsplit(e, "/")), n=1)
  insertnew <- dbSendQuery(conn, paste("INSERT OR IGNORE INTO ","data_file (path)"," VALUES (?)",sep=""))
  dbBind(insertnew, params=list(y))
  dbClearResult(insertnew)
  splitfile <- unlist(strsplit(y, "CTT-"))
  fileinfo <- splitfile[2]
  sensorid <- unlist(strsplit(fileinfo,"-"))
  sensor <- sensorid[1]
  filenameinfo <- sensorid[2]
  file_info <- unlist(strsplit(filenameinfo, "\\."))[1]
  filetype <- ifelse(is.na(as.integer(file_info)),file_info,"sensorgnome") 
  if (filetype %in% c("raw", "node", "gps")) {
  contents <- tryCatch({
    if (file.size(e) > 0) {
      read.csv(e, as.is=TRUE, na.strings=c("NA", ""), header=TRUE, skipNul = TRUE, colClasses=c("NodeId"="character","TagId"="character"))
    }}, error = function(err) {
      # error handler picks up where error was generated, in Bob's script it breaks if header is missing
      print(paste("error reading", e))
    })
    if(is.data.frame(contents)) {
    contents$station_id <- sensor
    contents$path <- y
    if (filetype == "gps") {
      colnames(contents)[colnames(contents)=="recorded.at"] <- "recorded_at"
      contents$recorded_at <- as.character(contents$recorded_at)
      colnames(contents)[colnames(contents)=="gps.at"] <- "gps_at"
      contents$gps_at <- as.character(contents$gps_at)
      if ("mean.lat" %in% colnames(contents)) {
      colnames(contents)[colnames(contents)=="mean.lat"] <- "mean_lat"
      colnames(contents)[colnames(contents)=="mean.lng"] <- "mean_lng"
      colnames(contents)[colnames(contents)=="n.fixes"] <- "n_fixes"
      } else {
        contents$mean_lat <- NA
        contents$mean_lng <- NA
        contents$n_fixes <- NA
      }
    } else if (filetype == "raw") {
      contents$Time <- as.character(contents$Time)
      nodeids <- unique(contents$NodeId)
      insertnew <- dbSendQuery(conn, paste("INSERT OR IGNORE INTO ","nodes (NodeId)"," VALUES (?)",sep=""))
      dbBind(insertnew, params=list(unique(nodeids)))
      dbClearResult(insertnew)
    } else {
      if("RecordedAt" %in% colnames(contents)) {
        contents$RecordedAt <- as.character(contents$RecordedAt)}
      else {
        contents$RecordedAt <- NA
        contents$Firmware <- NA
        contents$SolarVolts <- NA
        contents$SolarCurrent <- NA
        contents$CumulativeSolarCurrent <- NA
        contents$Latitude <- NA
        contents$Longitude <- NA
      }
      contents$Time <- as.character(contents$Time)
      nodeids <- unique(contents$NodeId)
      insertnew <- dbSendQuery(conn, paste("INSERT OR IGNORE INTO ","nodes (NodeId)"," VALUES (?)",sep=""))
      dbBind(insertnew, params=list(unique(nodeids)))
      dbClearResult(insertnew)
    }
    vars <- paste(":",dbListFields(conn, filetype), sep="", collapse=",")
    
    insertnew <- dbSendQuery(conn, paste("INSERT OR IGNORE INTO ",filetype," VALUES (",vars,")",sep=""))
    print(contents)
    dbBind(insertnew, params=contents)
    dbClearResult(insertnew)
    }} else {nodeids <- c()}
}
