# MOSMIX-L parameter description


# Variables
abbreviation <- c("RR1c", "ww", "T5cm", "TTT", "TN", "TX", "FF", "SunD", "N")
description <- c("Gesamtniederschlag (1-stündig) konsistent mit ww",
                 "Signifikantes Wetter", "Temperatur am Erdboden in 5cm Hoehe",
                 "2m-Temperatur", "Minimumtemperatur - letzte 12 Stunden",
                 "Maximumtemperatur - letzte 12 Stunden", "Windgeschwindigkeit",
                 "Sonnenscheindauer innerhalb der letzten 24 Stunden", "Gesamtbedeckung")
description_en <- c("Total precipitation during the last 6 hours",
                    "Probability of significant weather within the last 12 hours",
                    "Temperature 5cm above surface", "Temperature 2m above surface",
                    "Minimum temperature - within the last 12 hours",
                    "Maximum temperature - within the last 12 hours",
                    "Wind speed", "Yesterdays total sunshine duration", "Total cloud cover")
unit <- c("kg/m2", "%", "K", "K", "K", "K", "m/s", "s", "%")
frequency <- c("Stündlich", "Stündlich", "Stündlich", "Stündlich", "2xTag: 06:00 AM, 18:00 PM",
               "2xTag: 06:00 AM, 18:00 PM", "Stündlich", "Taglich", "Stündlich")


# Build data.frame
MOSMIXLParameter <- data.frame(abbreviation = abbreviation,
                               description = description,
                               description_en = description_en,
                               unit = unit,
                               frequency = frequency)


## Provide data.frame in package
usethis::use_data(MOSMIXLParameter, overwrite = TRUE)


