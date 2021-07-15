library(pryr)
library(dplyr)
library(readr)

climateWWO = read.csv("C://Users/stark/OneDrive/Documents2021/biocro-dev/55,37.csv")
View(climateWWO)

climateWWO = within(climateWWO, {
  doy = lubridate::yday(date_time)})
climateWWO = within(climateWWO, {
  hour = lubridate::hour(date_time)})
climateWWO = within(climateWWO, {
  year = lubridate::year(date_time)})

climateWWO = climateWWO[c('year', 'doy', 'hour', 'precipMM')]
names(climateWWO) = c('year', 'doy', 'hour', 'precip')

climateSolcast = read.csv("C://Users/stark/OneDrive/Documents2021/biocro-dev/55_37_Solcast_PT60M.csv")
View(climateSolcast)

climateSolcast = within(climateSolcast, {
  date = lubridate::ymd_hms(PeriodStart)})
climateSolcast = within(climateSolcast, {
  doy = lubridate::yday(date)})
climateSolcast = within(climateSolcast, {
  hour = lubridate::hour(date)})
climateSolcast = within(climateSolcast, {
  year = lubridate::year(date)})

climateSolcast = climateSolcast[c('year','doy','hour','Dni','RelativeHumidity','WindSpeed10m','AirTemp')]
names(climateSolcast) = c('year','doy','hour','solar','rh','windspeed','temp')
climateSolcast1 <- climateSolcast
climateSolcast <- full_join(climateSolcast1,climateSolcast2,by=c('year','doy','hour','solar','rh','windspeed','temp'))
climate = inner_join(climateWWO,climateSolcast,by = c('year','doy','hour'))
View(climateSolcast)

climate = within(climate, {
  rh = rh / 100  # dimensionless
  solar = solar * 4.6  # micromole / m^2 / s. Converted from W / m^2 / s
  time = doy + hour / 24
  doy_time = doy * 24 + hour
  # WindSpeed  # m / s.
  # temp  # degrees Celsius.
  # hour  # 0 - 23.
})
climate = subset(climate, (year == 2014 & doy > 37) | year == 2015)
climate = climate[complete.cases(climate), ]
View(climate)
write.csv(climate,"C://Users/stark/OneDrive/Documents2021/biocro-dev/miscanthus/moscow_weather.csv")

climate = read.csv("C://Users/stark/OneDrive/Documents2021/biocro-dev/miscanthus/moscow_weather.csv")
climate = within(climate, {
  time = doy + hour / 24 + (year - 2014) * 365
})
