#' Request DWD Forecast data along a set of coordinates
#'
#' @param IDsDWDWeatherStations character. Vector with IDs of DWD weather stations
#' @param ParametersToExtract character. Vektor mit zu extrahierenden Parametern aus den DWD `kml`-Dateien
#' @param ConvertKelvinToCelsius bool. Convert Kelvin into Celius. default: `TRUE`
#' @param tmap bool. Generate map (html) with weather stations with tmap? default: `FALSE`
#' @param tmapOutputDir. character. tmap output directory. default: `tempdir()`
#'
#' @details
#' Available parameter - see also data object `MOSMIXLParameter`:
#'
#' - RR1c: Gesamtniederschlag (1-stündig) konsistent mit ww - Einheit kg/m2 - Stündlich
#'
#' - ww: Signifikantes Wetter - Einheit % - Stündlich
#'
#' - T5cm: Temperatur am Erdboden in 5cm Hoehe - Einheit K - Stündlich
#'
#' - TTT: 2m-Temperatur - Einheit K - Stündlich
#'
#' - TN: Minimumtemperatur - letzte 12 Stunden - Einheit K - 2xTag: 06:00 AM, 18:00 PM
#'
#' - TX: Maximumtemperatur - letzte 12 Stunden - Einheit K - 2xTag: 06:00 AM, 18:00 PM
#'
#' - FF: Windgeschwindigkeit - Einheit m/s - Stündlich
#'
#' - SundD: Sonnenscheindauer innerhalb der letzten 24 Stunden - Einheit s - Täglich
#'
#' - N: Gesamtbedeckung - Einheit % - Stündlich
#'
#'
#' @return list. Meta data and data.frame with forecast data of the extracted weather stations.
#' @export
#'
#' @examples
#' # Example 1: Weather stations with different available parameters
#' IDsDWDWeatherStationsExample1 <- c("11120", "06753", "74783", "E273", "E355")
#' # Request weather data
#' ForecastingDWDStationsExample1 <- DWDForecasting(IDsDWDWeatherStations = IDsDWDWeatherStationsExample1)
#' # Get a glimpse of the data
#' head(ForecastingDWDStationsExample1)
#' # Number of forecast time points of each station
#' table(ForecastingDWDStationsExample1$StationsID)
#'
#' # Example 2: Weather stations with same available parameters
#' # Request weather data
#' ForecastingDWDStationsExample2 <- DWDForecasting(IDsDWDWeatherStations = c("E273", "10224", "A762", "K2689",
#'                                                                            "E355",  "H542",  "10210", "A981",
#'                                                                            "E078",  "H330",  "H573",  "P232",
#'                                                                            "10400", "H744",  "10442", "E426"))
#' # Get a glimpse of the data
#' head(ForecastingDWDStationsExample2)
#' # Number of forecast time points of each station
#' table(ForecastingDWDStationsExample2$StationsID)
#'
#'
#' #Example 3 - Special case: only a single weather station
#' ForecastingDWDStationsExample3 <- DWDForecasting(IDsDWDWeatherStations = "E273")
DWDForecasting <- function(IDsDWDWeatherStations,
                           ParametersToExtract = c("RR1c", "ww", "T5cm",
                                                   "TTT", "TN", "TX", "FF",
                                                   "SunD", "N"),
                           ConvertKelvinToCelsius = TRUE,
                           tmap = TRUE,
                           tmapOutputDir = tempdir()) {


  ## Install/Load required packages:
  DWDForecastPackageHandling()


  ## Create http-addresses to get the data from DWD Server: Generates a data.frame with weatherID and URL
  URLDataFrame <- CreateURLQuery(DWDWeatherStationID = IDsDWDWeatherStations)


  ## Retrieve data and save to temp folder
  DWDDataDF <- data.frame()
  for (i in 1:nrow(URLDataFrame)) {
    DWDDataDF[i,1] <- RequestMOSMIXLData(DWDURLsDF = URLDataFrame[i,],
                                         ColDWDStationID = "DWDWeatherStationID",
                                         ColURL = "URL")
  }
  # Output of the kmz file list (data.frame)
  print(DWDDataDF)


  ## Extract XML and write to list
  DWDDataExtractedXMLList <- list()
  #   Extract XML documents
  for (i in 1:nrow(DWDDataDF)) {
    DWDDataExtractedXMLList[[i]] <- ExtractDWDForecastingData(PathFolderFileName = DWDDataDF[i,1])
  }
  # Number of extracted DWS weather station IDs
  message(paste0("Number of extracted XML records (one XML record per weather station): ",
                 length(DWDDataExtractedXMLList)))


  ## Extract station data
  #   Create empty list
  DWDDataExtractedStationInfoList <- list()
  #   Extract station information
  for (i in 1:length(DWDDataExtractedXMLList)) {
    DWDDataExtractedStationInfoList[[i]] <-
      ExtractXMLStationData(DWDXMLData = DWDDataExtractedXMLList[[i]])
  }
  # Store and print stations
  DWDStationInfoDF <- do.call(rbind, DWDDataExtractedStationInfoList)
  print(data.frame(DWDStationInfoDF))


  ## Extract the available variables - based of the list in ParametersToExtract - of a weather station
  DWDStationVariables <- list()
  #   Extract variable names per station
  for (i in 1:length(DWDDataExtractedXMLList)) {
    DWDStationVariables[[i]] <- ExtractAvailableVariables(DWDXMLData = DWDDataExtractedXMLList[[i]],
                                                          ParametersToExtract,
                                                          DataFrame = FALSE)
    }
  # Set Station ID
  names(DWDStationVariables) <- DWDStationInfoDF$name
  # Print
  print(DWDStationVariables)


  ## Check whether the requested parameters - in ParametersToExtract - are present in the query
  VarInList <- list()
  # Check station data
  for (i in 1:length(DWDStationVariables)) {
    VarInList[[i]] <- ParametersToExtract %in% DWDStationVariables[[i]]
  }
  # convert into a single data.frame/s and add column- and row-names
  dfVarInList <- data.frame(do.call(rbind, VarInList))
  colnames(dfVarInList) <- ParametersToExtract
  dfVarInList$DWDWeatherStationID <- URLDataFrame$DWDWeatherStationID
  print(dfVarInList)


  ## Extract the data - based of the list in ParametersToExtract - of a weather station
  message("Extract data from weather stations \n")
  DWDStationDf <- list()
  #   Extract variable names per station
  for (i in 1:length(DWDDataExtractedXMLList)) {
    DWDStationDf[[i]] <- ExtractAvailableVariables(DWDXMLData = DWDDataExtractedXMLList[[i]],
                                                   ParametersToExtract,
                                                   DataFrame = TRUE)
    }


  # Add station ID
  #  As name
  names(DWDStationDf) <- URLDataFrame$DWDWeatherStationID
  #  As variable
  for (i in 1:length(DWDStationDf)) {
    DWDStationDf[[i]]$StationsID <- names(DWDStationDf)[i]
  }


  ## Build one long data.frame
  #   dplyr::bind_rows() function will be used
  message("Building data.frame consisting of all weather stations")
  dplyr::bind_rows()
  DataDfComplete <- Reduce(f = dplyr::bind_rows, x = DWDStationDf, accumulate = FALSE)
  # convert back into a data.frame
  DataDfComplete <- data.frame(DataDfComplete)
  # Number of entries of each weather station
  print(table(DataDfComplete$StationsID))


  # Convenience: Convert Kelvin in Celsius
  message("Converting Kelvin in Celsius")
  if (ConvertKelvinToCelsius) {
    DataDfComplete <- ConvertKelvinIntoCelsius(DWDForecastDf = DataDfComplete)
  }


  # Convert SunD into hours
  message("Converting sun duration in seconds to hours")
  if ("SunD" %in% colnames(DataDfComplete)) {
    DataDfComplete$SunD <- DataDfComplete$SunD/60/60
    }


  ## Get the spatial data
  #   SpatialPointsDataFrame
  DWDStationInfoDF
  #browser()

  ## tmap-part: results will be stored ina folder
  try({
    if (tmap) {
      message("Generating interactive map with weather stations")
      # Generate interactive chart with coordinates of weather stations
      m2 <- tmap::tm_shape(DWDStationInfoDF) + tmap::tm_symbols(col = "tomato", size = .5, scale = .5)
      # Save interactive map
      tmap::tmap_save(tm = m2, filename = paste0(tmapOutputDir,"\\", "WeatherStationMap.html"))
      }
    })


  ## Return data.frame
  return(DataDfComplete)
  }




#' Installation or loading of the required packages for obtaining the forecasting data of the DWD
#'
#' @return message. Returns a message that the packages are loaded or not loaded. If packages are not loaded,
#' check carefully what happend.
#' @export
#'
#' @examples
#' DWDForecastPackageHandling()
DWDForecastPackageHandling <- function() {
  # Install and/or load package devtools
  suppressWarnings(if (!require(devtools)) install.packages("devtools"))
  library(devtools)
  # Known error: Github personal access token set (and not guilty any more)
  #  Delete temporarly
  try({if (!nchar(Sys.getenv("GITHUB_PAT")) == 0) {
    # Copy into a variable to get a marker (the information itself is useless)
    GITHUB_PAT <- Sys.getenv("GITHUB_PAT")
    # Delete the path - will restored later
    Sys.unsetenv("GITHUB_PAT")
  }})
  # Install package Rmosmix from CRAN
  suppressWarnings(if (!require(mosmix)) install_github("retostauffer/Rmosmix"))
  library(mosmix)

  # Restore Github personal access token
  try({if (exists("GITHUB_PAT")) Sys.setenv(GITHUB_PAT = gitcreds::gitcreds_get(use_cache = FALSE)$password)})

  # Check if all required packages are loaded
  if (all(c("devtools","mosmix") %in% loadedNamespaces())) return(message("Packages loaded"))
  # Not all packages are loaded
  return(message("Not all packages could have loaded - please check carefully"))
  }




#' Creates a URL to query the weather data based on the ID of the weather station
#'
#' @description Generates a URL in the form "https://opendata.dwd.de/weather/local_forecasts/mos/MOSMIX_L/single_stations/01001/kml/MOSMIX_L_LATEST_01001.kmz"
#'
#' @param DWDWeatherStationID character. Weather station ID as character.
#'
#' @return data.frame. URL weather station with ID. ID of the weather station.
#' @export
#'
#' @examples
#' CreateURLQuery(DWDWeatherStationID = "E273")
#' CreateURLQuery(DWDWeatherStationID = "E355")
CreateURLQuery <- function(DWDWeatherStationID) {
  # Start
  message(paste("\nCreate query URL for the weather station ID", DWDWeatherStationID))
  # URL Textstring
  urlTextString <- paste0("https://opendata.dwd.de/weather/local_forecasts/mos/MOSMIX_L/single_stations/",
                          DWDWeatherStationID ,
                          "/kml/MOSMIX_L_LATEST_")
  # URL erzeugen
  QueryURL <- paste0(urlTextString, DWDWeatherStationID, ".kmz")
  # Message
  message(paste("\nURL generated with ID of the weather station:", QueryURL))
  # generate data.frame: URL and ID
  QueryURLIDdf <- data.frame(DWDWeatherStationID = DWDWeatherStationID, URL = QueryURL)
  # zurückgegen
  return(QueryURLIDdf)
}




#' Download the forecast weather data of the DWD
#'
#' @description Downloads the latest L-type DWD MOSMIX forecast file: MOSMIX-L: produced 4 times a day up to 240 hours
#' ahead, provides up to 115 parameters (location dependent)
#'
#' @details Stored in the tempdir(). Please note, however, that this does not save the files permanently,
#' since the tempdir is deleted again after an R session is ended.
#'
#' @param DWDURLsDF data.frame. URL weather station with ID. ID of the weather station
#' @param ColDWDStationID character. Weatherstation DWD ID
#' @param ColURL character. Column name generated query URL
#'
#' @return character. Returns the path and folder of the download.
#' @export
#'
#' @examples
#' TestRequestData <-
#'   data.frame(DWDWeatherStationID = "11120",
#'              URL = "https://opendata.dwd.de/weather/local_forecasts/mos/MOSMIX_L/single_stations/11120/kml/MOSMIX_L_LATEST_11120.kmz")
#' RequestMOSMIXLData(DWDURLsDF = TestRequestData, ColDWDStationID = "DWDWeatherStationID", ColURL = "URL")
RequestMOSMIXLData <- function(DWDURLsDF, ColDWDStationID = "DWDWeatherStationID", ColURL = "URL") {
  # Create path to folder with file name - folder is tempdir()
  FileName <- paste0(DWDURLsDF[,ColDWDStationID], ".kmz")
  PathFolderFileName <- paste0(tempdir(), "\\", FileName)
  message(paste("The forcast file is saved in:", PathFolderFileName))
  # Download file to temp directory
  utils::download.file(url = DWDURLsDF[,ColURL],
                       destfile = PathFolderFileName,
                       method = "curl")
  # If successful, return path and FileName
  if (any(FileName %in%  dir(tempdir()))) {
    message(paste("File", FileName, "successfuly stored in", PathFolderFileName))
    return(PathFolderFileName)
  }
  ## Falls nicht erfolgreich, NA zurückgeben
  message("File could not be downloaded, return NA")
  return(NA)
}




#' DWD Forecasting file - kmz format - unpack and return as XML document
#'
#' @param PfadZumOrdnerUndDateiName character. Folder where the kmz file is stored
#'
#' @return XMLInternalDocument / XMLAbstractDocument. Generated XML document for further processing
#' @export
#'
#' @examples
#' ## Station K057
#' TestRequestData <-
#'   data.frame(DWDWeatherStationID = "K057",
#'              URL = "https://opendata.dwd.de/weather/local_forecasts/mos/MOSMIX_L/single_stations/K057/kml/MOSMIX_L_LATEST_K057.kmz")
#' K057PathAndFolder <- RequestMOSMIXLData(DWDURLsDF = TestRequestData,
#'                                         ColDWDStationID = "DWDWeatherStationID",
#'                                         ColURL = "URL")
#' # Extract from XML file
#' K057ExtractedForecastData <- ExtractDWDForecastingData(PathFolderFileName = K057PathAndFolder)
#' class(K057ExtractedForecastData)
ExtractDWDForecastingData <- function(PathFolderFileName) {
  ## Unzip kml file in folder
  kml <- utils::unzip(zipfile = PathFolderFileName, exdir = tempdir())
  ## Parsing the unzip kml file (XML format) into object doc
  doc <- XML::xmlParse(kml)
  ## Reutrn xml object
  return(doc)
}




#' Extract station data
#'
#' @param DWDXMLData XMLInternalDocument / XMLAbstractDocument. Extracted XML document from DWD kmz file.
#'
#' @return SpatialPointsDataFrame. Object with `coordinates`, `name`, `desc` and `alt`
#' @export
#'
#' @examples
#' ## Station K057
#' TestRequestData <-
#'   data.frame(DWDWeatherStationID = "K057",
#'              URL = "https://opendata.dwd.de/weather/local_forecasts/mos/MOSMIX_L/single_stations/K057/kml/MOSMIX_L_LATEST_K057.kmz")
#' K057PathAndFolder <- RequestMOSMIXLData(DWDURLsDF = TestRequestData,
#'                                         ColDWDStationID = "DWDWeatherStationID",
#'                                         ColURL = "URL")
#' # Extract from XML file
#' K057ExtractedForecastData <- ExtractDWDForecastingData(PathFolderFileName = K057PathAndFolder)
#' # Extract station data
#' DWDXMLData(DWDXMLData = K057ExtractedForecastData)
ExtractXMLStationData <- function(DWDXMLData){
  ## Check if the file is the expected XML object
  stopifnot(inherits(x = DWDXMLData, what = c("XMLInternalDocument", "XMLAbstractDocument")))
  ## Unpack station data
  stations <- mosmix::get_station_information(DWDXMLData)
  ## Return: station meta info
  return(stations)
}



#' Extract available parameters of a weather station - returns either a list or a vector with names
#'
#' @param DWDXMLData XMLInternalDocument / XMLAbstractDocument. Extracted XML document from DWD kmz file.
#' @param ParametersToExtract character vector. Vector with variable names to extract.
#' default: `c("RR1c", "ww", "T5cm", "TTT", "TN", "TX", "FF","SunD", "N")`
#' @param DataFrame bool. Return a data.frame or a character vector with the variable names.
#' default `TRUE`  returns a data.frame.
#'
#' @return either character vector or data.frame. Returns either a data.frame with parameters or a data.frame with the
#' extracted data.
#' @export
#'
#' @examples
#' ## Station K057
#' TestRequestData <-
#'   data.frame(DWDWeatherStationID = "K057",
#'              URL = "https://opendata.dwd.de/weather/local_forecasts/mos/MOSMIX_L/single_stations/K057/kml/MOSMIX_L_LATEST_K057.kmz")
#' K057PathAndFolder <- RequestMOSMIXLData(DWDURLsDF = TestRequestData,
#'                                         ColDWDStationID = "DWDWeatherStationID",
#'                                         ColURL = "URL")
#' # Extract from XML file
#' K057ExtractedForecastData <- ExtractDWDForecastingData(PathFolderFileName = K057PathAndFolder)
#' # Extract station data
#' ExtractXMLStationData(DWDXMLData = K057ExtractedForecastData)
#' # Extract names of available parameters of a weather station
#' ExtractAvailableVariables(DWDXMLData = K057ExtractedForecastData, DataFrame = FALSE)
#' # data.frame
#' ExtractAvailableVariables(DWDXMLData = K057ExtractedForecastData, DataFrame = TRUE)
ExtractAvailableVariables <- function(DWDXMLData,
                                      ParametersToExtract = c("RR1c", "ww", "T5cm",
                                                              "TTT", "TN", "TX", "FF",
                                                              "SunD", "N"),
                                      DataFrame = TRUE){
  ## Check if the file is the expected XML object
  stopifnot(inherits(x = DWDXMLData, what = c("XMLInternalDocument", "XMLAbstractDocument")))
  ## Extract variable names
  DWDVarList <- list()
  DWDVarList$datetime <- mosmix::get_datetime(DWDXMLData)
  DWDVarList$meta     <- mosmix::get_meta_info(DWDXMLData)
  DWDVarList$stations <- mosmix::get_station_information(DWDXMLData)
  ## Extract selected parameter and write it to the list
  fcst1 <- get_forecasts(station = paste0("'",DWDVarList$stations$name,"'"),
                         doc = DWDXMLData,
                         datetime = DWDVarList$datetime,
                         meta = DWDVarList$meta,
                         as.zoo = FALSE,
                         parameter = ParametersToExtract)
  # return DataFrame (default) or character vector with names
  if (DataFrame) {
    return(as.data.frame(fcst1))
  } else {return(colnames(fcst1))}

}





#' Converts Kelvin into Celsius for a predefined set of variables
#'
#' @description if set to TRUE variables `TX`, `TTT`, `TN`, `T5cm` will be converted into Celsius.
#'
#' @details Conversion factor Kelvin in Celsius is -273.15
#'
#' @param DWDDataFrame data.frame. Extracted parameters
#'
#' @return
#' @export
#'
#' @examples
#' # print internal data.frame
#' ForecastDWDStationE273
#' ConvertKelvinIntoCelsius(DWDForecastDf = ForecastDWDStationE273)
ConvertKelvinIntoCelsius <- function(DWDForecastDf) {
  # define Temp-Variables in Kelvin
  TempCelsiusVars <- c("TX", "TTT", "TN", "T5cm")
  # Convert
  try({
    for (TempCelsiusVar in TempCelsiusVars) {
      print(TempCelsiusVar)
      DWDForecastDf[,TempCelsiusVar] <- DWDForecastDf[,TempCelsiusVar] - 273.15
    }
  })

  # return
  return(DWDForecastDf)
}



