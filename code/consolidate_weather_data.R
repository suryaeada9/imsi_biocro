library(pryr)
library(dplyr)
library(readr)

#Currently getting precipitation data from WWO and other data from Solcast
#There is some overlap in values provided, and unfortunately, the numerical values can be quite different
#For example, temperature is up to 5 degrees Celsius different for the same hour on the same day

#read in and format WWO data
climateWWO = read.csv("C://Users/stark/OneDrive/Documents2021/biocro-dev/40.063377450813086,-88.2057459957978.csv")
climateWWO = within(climateWWO, {
  doy = lubridate::yday(date_time)})
climateWWO = within(climateWWO, {
  hour = lubridate::hour(date_time)})
climateWWO = within(climateWWO, {
  year = lubridate::year(date_time)})

climateWWO = climateWWO[c('year', 'doy', 'hour', 'precipMM')]
names(climateWWO) = c('year', 'doy', 'hour', 'precip')

#read in and format Solcast data
climateSolcast = read.csv("C://Users/stark/OneDrive/Documents2021/biocro-dev/40.063377450813086_-88.2057459957978_Solcast_PT60M.csv")
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

#some of the miscanthus seasons are more than a year and required 2 Solcast downloads
#In that case, run the above Solcast formatting code for both files, and join them with the line of code below
#climateSolcast <- full_join(climateSolcast1,climateSolcast2,by=c('year','doy','hour','solar','rh','windspeed','temp'))

#combine WWO and Solcast weather
climate = inner_join(climateWWO,climateSolcast,by = c('year','doy','hour'))

#change some units to fit BioCro format
climate = within(climate, {
  rh = rh / 100  # dimensionless
  solar = solar * 4.6  # micromole / m^2 / s. Converted from W / m^2 / s
  time = doy + hour / 24 + (year - 2014) * 365
  doy_time = doy * 24 + hour
})

#For 2019 weather for Energy Farm site at Illinois ONLY
climate = subset(climate, (year == 2019))
climate = climate[complete.cases(climate), ]
write.csv(climate,"C://Users/stark/OneDrive/Documents2021/biocro-dev/EF2019_weather.csv")
