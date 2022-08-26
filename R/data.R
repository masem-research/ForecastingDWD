#' 5 rows of data from DWD station E273
#'
#' The dataset contains 5 lines of a typical DWD weather station output generated with the functions in the
#' package `DWDForecasting`
#'
#'
#' @format A data.frame with 5 rows and 11 variables:
#' \describe{
#'   \item{datetime}{chr, date/time variable saved as character}
#'   \item{TX}{num, eine kurze Informazion des Projektes: enthält Vertragsnummer, Golfplatzort, Vertragsbeginn}
#'   \item{TTT}{num, Die geographische Breite eines Golfplatzes}
#'   \item{TN}{num, Die geographische Länge eines Golfplatzes}
#'   \item{T5cm}{num, Löcheranzahl je Golfplatz}
#'   \item{FF}{num, eindeutige DWD Stationsnummer}
#'   \item{N}{num, Entfernung vom Golfplatz zur nächsten DWD Station, km}
#'   \item{ww}{num, Entfernung vom Golfplatz zur nächsten DWD Station, km}
#'   \item{RR1c}{num, Entfernung vom Golfplatz zur nächsten DWD Station, km}
#'   \item{SunD}{num, Entfernung vom Golfplatz zur nächsten DWD Station, km}
#'   \item{StationsID}{chr, Entfernung vom Golfplatz zur nächsten DWD Station, km}
#'
#'
#' }
#' @source {"data/"}
"ForecastDWDStationE273"


#' MOSMIX-L parameter description
#'
#' The dataset contains the Deutsche Wetterdienst (DWD) MOSMIX-L parameter description in German language
#'
#'
#' @format A data.frame with 9 rows and 4 variables:
#' \describe{
#'   \item{abbreviation}{chr, variablename}
#'   \item{description}{chr, short description}
#'   \item{description_en}{chr, short description in English}
#'   \item{unit}{chr, please note: unit can be changed during processing, e.g. K in °C, seconds in minutes}
#'   \item{frequency}{chr, measured how often?}
#' }
#' @source {"data/"}
"MOSMIXLParameter"


