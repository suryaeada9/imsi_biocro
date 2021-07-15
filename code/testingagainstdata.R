library(BioCro)

library(ggplot2)

library(dplyr)

#Justin's code for getting and cleaning climate data

climate = read.delim(file.path("C://Users/stark/OneDrive/Documents2021/biocro-dev", 'filled_climate.tsv'))
climate = subset(climate, year == 2016 & doy > 167)  # According the these notes dated June 16, sorghum at SoyFACE had emerged, but not at the Energy Farm. It's a decent estimate.
#climate = subset(climate, year == 2017 & doy > 151)
climate = climate[c('year', 'doy', 'hour', 'par', 'temperature', 'relative_humidity', 'windspeed', 'precipitation')]

names(climate) = c('year', 'doy', 'hour', 'solar', 'temp', 'rh', 'windspeed', 'precip')
climate = within(climate, {
  rh = rh / 100  # dimensionless. NOAA reports relative humidity as a percent of saturated vapor pressure.
  solar = solar * 4.6  # micromole / m^2 / s. The NOAA sensor outputs micromoles / m^2 / s and they divide by 4.6 to convert to W / m^2 / s. See excerpt from NOAA SURFRAD README above.
  precip = precip * 25.4  # mm. NOAA reports precipitation in inches.
  time = doy + hour / 24
  doy_time = doy * 24 + hour
  # WindSpeed  # m / s.
  # temp  # degrees Celsius.
  # hour  # 0 - 23.
})

climate = climate[complete.cases(climate), ]

#run the BioCro model on the climate data
sorghum_steady_state_modules <- list(
  "soil_type_selector",
  "stomata_water_stress_linear",
  "leaf_water_stress_exponential",
  "parameter_calculator",
  "soil_evaporation",
  "c4_canopy",
  "partitioning_coefficient_selector",
  "partitioning_growth_calculator"
)

sorghum_derivative_modules <- list(
  "thermal_time_senescence",
  "partitioning_growth",
  "thermal_time_linear",
  "one_layer_soil_profile"
)

sorghum_result <- Gro_solver(
  sorghum_initial_values,
  sorghum_parameters,
  climate,
  sorghum_steady_state_modules,
  sorghum_derivative_modules
)

byDay <- sorghum_result %>% 
  group_by(doy) %>%
  summarize(meanYield = mean(Leaf) + mean(Stem))

View(byDay)

#biomass file was inputted via "Import Dataset"
#this calculates doy in order to join to the model result data
biomass.yield = within(biomass.yield, {
  date = lubridate::yday(as.Date(date))
})

siteData = subset(biomass.yield,biomass.yield$site_id=="EF")

byDay <- sorghum_result %>% 
  group_by(doy) %>%
  summarize(meanYield = mean(Leaf) + mean(Stem))


View(siteData)

View(inner_join(byDay,siteData,by = c("doy" = "date")))

View(sorghum_result)

#calculate RMSE, MAE, MAPE, and Chi Square for data from a given siteId vs the BioCro model
Statistics <- function(siteId,model_result){
  siteData = subset(biomass.yield,biomass.yield$site_id==siteId) #filter by site_id
  byDay <- model_result %>% 
    group_by(doy) %>%
    summarize(meanYield = mean(Leaf) + mean(Stem))
  innerJoinDf <- inner_join(byDay,siteData,by = c("doy" = "date")) #joins the two dataframes so we can graph on the same axes
  innerJoinDf <- mutate(innerJoinDf,actualMinusModel = above_ground_mass - meanYield) #makes a column for actual data minus model data
  innerJoinDf <- mutate(innerJoinDf,actualMinusModelAbs = abs(actualMinusModel)) #makes a column for absolute value of difference
  innerJoinDf <- mutate(innerJoinDf,diffSquares = actualMinusModel * actualMinusModel) #makes a column for the square of the difference
  innerJoinDf <- mutate(innerJoinDf,absPercentError = actualMinusModelAbs/above_ground_mass * 100) #makes a column for absolute value of percentage difference
  innerJoinDf <- mutate(innerJoinDf,chiSquareToSum = diffSquares/above_ground_mass) #makes a column for square of difference over actual value (for chi square calculation)
  
  #find sums of a few columns for stats calculations
  SumOfDiffsSquared = sum(innerJoinDf[,'diffSquares'])
  SumOfAbsDiffs = sum(innerJoinDf[,'actualMinusModelAbs'])
  
  #number of rows
  n = nrow(innerJoinDf)
  
  #stats calculations
  RMSE = sqrt(SumOfDiffsSquared/n)
  MAE = SumOfAbsDiffs/n
  SumOfAbsPercs = sum(innerJoinDf[,'absPercentError'])
  MAPE = SumOfAbsPercs/n
  ChiSquare = sum(innerJoinDf[,'chiSquareToSum'])
  
  #return as string to be printed on graph
  return(paste("RMSE:",round(RMSE,digits=2),"MAE:",round(MAE,digits=2),"MAPE:",round(MAPE,digits=2),"Chi Square:",round(ChiSquare,digits=2)))
}

#function returns a graph of the model data with the actual data
getGraph <- function(siteId,model_result) {
  siteData = subset(biomass.yield,biomass.yield$site_id==siteId) #filter by site_id
  fullJoinDf <- full_join(model_result,siteData,by = c("time" = "date")) #joins the two dataframes so we can graph on the same axes
  return(ggplot(fullJoinDf,aes(time)) + #time as x-axis
          geom_point(aes(y = (Leaf+Stem), color = "BioCro Model")) + #model data
          geom_point(aes(y = above_ground_mass, color = paste(siteId,"Actual Yield"))) + #actual data from siteId
           xlab("Day of Year") + #x-axis label
           ylab("Leaf and Stem Combined Mass (Mg/ha)") + #y-axis label
          labs(title = paste("Sorghum Model Yield vs. Actual Data from",siteId,"Site"), #graph title
                caption = Statistics(siteId,model_result)) + #put stats in caption
           scale_color_manual(name = "Data Source",values = c("blue", "black")) + #model in blue, actual in black
           theme(legend.position = "bottom")) #put legend at bottom
}




#different site_ids
ids = list("EF","EF_check","MW_trans_border")

#make a pdf file graph for each site_id
for(id in ids){
  pdf(
    file = paste0(id,".pdf"),
    width = 7,          # inches
    height = 6,         # inches
    useDingbats = FALSE # make sure symbols are rendered properly in the PDF
  )
  
  print(getGraph(id,sorghum_result))
  
  dev.off()
}

