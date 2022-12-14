% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MainFunctionsForecastData.R
\name{ExtractXMLStationData}
\alias{ExtractXMLStationData}
\title{Extract station data}
\usage{
ExtractXMLStationData(DWDXMLData)
}
\arguments{
\item{DWDXMLData}{XMLInternalDocument / XMLAbstractDocument. Extracted XML document from DWD kmz file.}
}
\value{
SpatialPointsDataFrame. Object with `coordinates`, `name`, `desc` and `alt`
}
\description{
Extract station data
}
\examples{
## Station K057
TestRequestData <-
  data.frame(DWDWeatherStationID = "K057",
             URL = "https://opendata.dwd.de/weather/local_forecasts/mos/MOSMIX_L/single_stations/K057/kml/MOSMIX_L_LATEST_K057.kmz")
K057PathAndFolder <- RequestMOSMIXLData(DWDURLsDF = TestRequestData,
                                        ColDWDStationID = "DWDWeatherStationID",
                                        ColURL = "URL")
# Extract from XML file
K057ExtractedForecastData <- ExtractDWDForecastingData(PathFolderFileName = K057PathAndFolder)
# Extract station data
DWDXMLData(DWDXMLData = K057ExtractedForecastData)
}
