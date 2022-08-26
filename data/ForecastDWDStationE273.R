# Generate some data from a typical DWD data request


# Variables
datetime <- c( "2022-08-26 04:00:00 UTC", "2022-08-26 05:00:00 UTC", "2022-08-26 06:00:00 UTC", "2022-08-26 07:00:00 UTC", "2022-08-26 08:00:00 UTC")
TX <- c(NA, NA, 298.05, NA, NA)
TTT <- c(291.45, 291.95, 292.85, 294.05, 295.65)
TN <- c(NA, NA, 290.95, NA, NA)
T5cm <- c(289.85, 291.05, 292.55, 295.35, 297.85)
FF <- c(2.06, 2.06, 2.06, 2.57, 2.57)
N <- c(61, 64, 68, 67, 66)
ww <- c(1, 1, 2, 2, 2)
RR1c <- rep(0,5)
SunD <- c(NA, NA, 31680, NA, NA)
StationsID <- rep("E273", 5)
# Build data.frame
ForecastDWDStationE273 <- data.frame(datetime = datetime, TX = TX, TTT = TTT, TN = TN, T5cm = T5cm, FF = FF,
                                     N = N, ww = ww, RR1c = RR1c, SunD = SunD, StationsID = StationsID)


## Provide data.frame in package
usethis::use_data(ForecastDWDStationE273, overwrite = TRUE)
