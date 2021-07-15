library(dfoptim)
library(pryr)
library(dplyr)

library(BioCro)

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

climate = read.delim(file.path("C://Users/stark/OneDrive/Documents2021/biocro-dev", 'filled_climate.tsv'))
#climate2016 = subset(climate, year == 2016 & doy > 167)  # According the these notes dated June 16, sorghum at SoyFACE had emerged, but not at the Energy Farm. It's a decent estimate.
#climate2017 = subset(climate, year == 2017 & doy > 151)


get_climate <- function(Year,startdate){
  yr_climate = subset(climate, year == Year & doy > startdate)
  yr_climate = yr_climate[c('year', 'doy', 'hour', 'par', 'temperature', 'relative_humidity', 'windspeed', 'precipitation')]
  names(yr_climate) = c('year', 'doy', 'hour', 'solar', 'temp', 'rh', 'windspeed', 'precip')
  yr_climate = within(yr_climate, {
    rh = rh / 100  # dimensionless. NOAA reports relative humidity as a percent of saturated vapor pressure.
    solar = solar * 4.6  # micromole / m^2 / s. The NOAA sensor outputs micromoles / m^2 / s and they divide by 4.6 to convert to W / m^2 / s. See excerpt from NOAA SURFRAD README above.
    precip = precip * 25.4  # mm. NOAA reports precipitation in inches.
    time = doy + hour / 24
    doy_time = doy * 24 + hour
    # WindSpeed  # m / s.
    # temp  # degrees Celsius.
    # hour  # 0 - 23.
  })
  
  yr_climate = yr_climate[complete.cases(yr_climate), ]
  
  return(yr_climate)
  
}
View(climate_2016)

climate_2016 <- get_climate(2016,167)
climate_2017 <- get_climate(2017,151)

yrClimate <- function(year){
  if(year==2016)
  {
    return(climate_2016)
  }
  else
  {
    return(climate_2017)
  }
}

model_2016 <- Gro_solver(
  sorghum_initial_values,
  sorghum_parameters,
  climate_2016,
  sorghum_steady_state_modules,
  sorghum_derivative_modules
)

model_2017 <- Gro_solver(
  sorghum_initial_values,
  sorghum_parameters,
  climate_2017,
  sorghum_steady_state_modules,
  sorghum_derivative_modules
)

#biomass file was inputted via "Import Dataset"
#this calculates doy in order to join to the model result data
biomass.yield = within(biomass.yield, {
  date = lubridate::yday(as.Date(date))
})

biomass.yield[,"yield"] = biomass.yield[,'above_ground_mass'] * 10 / (biomass.yield[,'row_length'] * biomass.yield[,'row_spacing'])

actual_yield_2016 <- biomass.yield %>%
  filter(site_id == "EF") %>%
  group_by(date) %>%
  summarize(ActualMeanYield = mean(yield))

actual_yield_2017 <- biomass.yield %>%
  filter(site_id != "EF") %>%
  group_by(date) %>%
  summarize(ActualMeanYield = mean(yield))


df_inner_join <- function(actualData,modelResult){
  byDay <- modelResult %>% 
    group_by(doy) %>%
    summarize(meanYield = mean(Leaf) + mean(Stem)) #makes a new dataframe that gives the average yield per day (over all 24 values in the model_result dataframe)
  #Leaf + Stem is specific to sorghum and needs to be changed for other crops
  innerJoinDf <- inner_join(byDay,actualData,by = c("doy" = "date")) #joins the two dataframes so we can compare values and run stats
  innerJoinDf <- mutate(innerJoinDf,actualMinusModel = ActualMeanYield - meanYield) #makes a column for actual data minus model data
  innerJoinDf <- mutate(innerJoinDf,actualMinusModelAbs = abs(actualMinusModel)) #makes a column for absolute value of difference
  innerJoinDf <- mutate(innerJoinDf,diffSquares = actualMinusModel * actualMinusModel) #makes a column for the square of the difference
  innerJoinDf <- mutate(innerJoinDf,absPercentError = actualMinusModelAbs/ActualMeanYield * 100) #makes a column for absolute value of percentage difference
  innerJoinDf <- mutate(innerJoinDf,chiSquareToSum = diffSquares/ActualMeanYield) #makes a column for square of difference over actual value (for chi square calculation)
  return(innerJoinDf)
}

data_2016 <- df_inner_join(actual_yield_2016,model_2016)

data_2017 <- df_inner_join(actual_yield_2017,model_2017)

all_data <- rbind(data_2016,data_2017)

#calculate RMSE, MAE, MAPE, and Chi Square for data from a given siteId vs the BioCro model
Statistics <- function(x,sorghum_partial){
  result_2017 = sorghum_partial(2017)(x)
  innerJoinDf <- df_inner_join(actual_yield_2017,result_2017)
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
  
  #return as a dataframe
  stats = data.frame(RMSE = c(RMSE), MAE = c(MAE), MAPE = c(MAPE), ChiSquare = c(ChiSquare))
  
  return(stats)
}

normalize_kVals <- function(current_params){
  kVals <- data.frame(Leaf = c(current_params$kLeaf1,current_params$kLeaf2,current_params$kLeaf3,current_params$kLeaf4,current_params$kLeaf5,current_params$kLeaf6),
                      Stem = c(current_params$kStem1,current_params$kStem2,current_params$kStem3,current_params$kStem4,current_params$kStem5,current_params$kStem6),
                      Root = c(current_params$kRoot1,current_params$kRoot2,current_params$kRoot3,current_params$kRoot4,current_params$kRoot5,current_params$kRoot6),
                      Grain = c(current_params$kGrain1,current_params$kGrain2,current_params$kGrain3,current_params$kGrain4,current_params$kGrain5,current_params$kGrain6))
                      
  kVals <- mutate(kVals, Sum = Leaf + Stem + Root + Grain)
  kVals <- mutate(kVals, Leaf = Leaf/Sum)
  kVals <- mutate(kVals, Stem = Stem/Sum)
  kVals <- mutate(kVals, Root = Root/Sum)
  kVals <- mutate(kVals, Grain = Grain/Sum)
  
  current_params$kLeaf1 <- kVals[1,1]
  current_params$kLeaf2 <- kVals[2,1]
  current_params$kLeaf3 <- kVals[3,1]
  current_params$kLeaf4 <- kVals[4,1]
  current_params$kLeaf5 <- kVals[5,1]
  current_params$kLeaf6 <- kVals[6,1]
  current_params$kStem1 <- kVals[1,2]
  current_params$kStem2 <- kVals[2,2]
  current_params$kStem3 <- kVals[3,2]
  current_params$kStem4 <- kVals[4,2]
  current_params$kStem5 <- kVals[5,2]
  current_params$kStem6 <- kVals[6,2]
  current_params$kRoot1 <- kVals[1,3]
  current_params$kRoot2 <- kVals[2,3]
  current_params$kRoot3 <- kVals[3,3]
  current_params$kRoot4 <- kVals[4,3]
  current_params$kRoot5 <- kVals[5,3]
  current_params$kRoot6 <- kVals[6,3]
  current_params$kGrain1 <- kVals[1,4]
  current_params$kGrain2 <- kVals[2,4]
  current_params$kGrain3 <- kVals[3,4]
  current_params$kGrain4 <- kVals[4,4]
  current_params$kGrain5 <- kVals[5,4]
  current_params$kGrain6 <- kVals[6,4]
  
  return(current_params)
}


new_parameters <- function(params_to_change,current_params,min_val){
  sorghum_partial <- function(year){
    
    #normalize k values
    current_params <- normalize_kVals(current_params)
    
    return(partial_gro_solver(sorghum_initial_values,current_params,
                                        yrClimate(year),
                                        sorghum_steady_state_modules,sorghum_derivative_modules,params_to_change))
  }
  #st is stat: RMSE, MAE, MAPE, or ChiSquare
  sorghum_stat <- function(x,st="ChiSquare"){
    return(Statistics(x,sorghum_partial)[,c(st)])
  }
  para <- as.numeric(unname(current_params[params_to_change]))
  ans <- hjkb(para,sorghum_stat,lower=min_val)
  new_params <- ans$par
  names(new_params) <- params_to_change
  altered_params <- current_params
  for(p in params_to_change){
    altered_params[[p]] <- new_params[[p]]
  }
  return(altered_params)
}

sorghum_parameters$vmax_n_intercept <- 0

new_params <- new_parameters(c("iSp","tp1",
                               "tp2","tp3","tp4","tp5",
                               "kLeaf1","kLeaf2","kLeaf3","kLeaf4","kLeaf5","kLeaf6",
                               "kStem1","kStem2","kStem3","kStem4","kStem5","kStem6",
                               "kRoot1","kRoot2","kRoot3","kRoot4","kRoot5","kRoot6"),sorghum_parameters,0.0000000000001)

normalize_kVals(new_params)
View(new_params)


new_result_2016 <- Gro_solver(
  sorghum_initial_values,
  new_params,
  climate_2016,
  sorghum_steady_state_modules,
  sorghum_derivative_modules
)

new_result_2017 <- Gro_solver(
  sorghum_initial_values,
  new_params,
  climate_2017,
  sorghum_steady_state_modules,
  sorghum_derivative_modules
)

#calculate RMSE, MAE, MAPE, and Chi Square for data from a given siteId vs the BioCro model
Statistics1 <- function(model_result,actual_yield){
  byDay <- model_result %>% 
    group_by(doy) %>%
    summarize(meanYield = mean(Leaf) + mean(Stem)) #makes a new dataframe that gives the average yield per day (over all 24 values in the model_result dataframe)
  #Leaf + Stem is specific to sorghum and needs to be changed for other crops
  innerJoinDf <- inner_join(byDay,actual_yield,by = c("doy" = "date")) #joins the two dataframes so we can compare values and run stats
  innerJoinDf <- mutate(innerJoinDf,actualMinusModel = ActualMeanYield - meanYield) #makes a column for actual data minus model data
  innerJoinDf <- mutate(innerJoinDf,actualMinusModelAbs = abs(actualMinusModel)) #makes a column for absolute value of difference
  innerJoinDf <- mutate(innerJoinDf,diffSquares = actualMinusModel * actualMinusModel) #makes a column for the square of the difference
  innerJoinDf <- mutate(innerJoinDf,absPercentError = actualMinusModelAbs/ActualMeanYield * 100) #makes a column for absolute value of percentage difference
  innerJoinDf <- mutate(innerJoinDf,chiSquareToSum = diffSquares/ActualMeanYield) #makes a column for square of difference over actual value (for chi square calculation)
  
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
  
  stats = data.frame(RMSE = c(RMSE), MAE = c(MAE), MAPE = c(MAPE), ChiSquare = c(ChiSquare))
  
  return(stats)
}

#function returns a graph of the model data with the actual data
getGraph <- function(model_result,actual_yield) {
  fullJoinDf <- full_join(model_result,actual_yield,by = c("time" = "date")) #joins the two dataframes so we can graph on the same axes
  stats_df = Statistics1(model_result,actual_yield)
  stats_str = ""
  for(stat in colnames(stats_df)){
    stats_str=paste0(stats_str,stat,": ",round(stats_df[1,stat],digits=2),"  ")
  }
  
  return(ggplot(fullJoinDf,aes(time)) + #time as x-axis
           geom_point(aes(y = (Leaf+Stem), color = "BioCro Model")) + #model data
           geom_point(aes(y = ActualMeanYield, color = "Actual Yield")) + #actual data from siteId
           xlab("Day of Year") + #x-axis label
           ylab("Leaf and Stem Combined Mass (Mg/ha)") + #y-axis label
           labs(title = "Sorghum Model Yield vs. Actual Data", #graph title
                caption = stats_str) + #put stats in caption
           scale_color_manual(name = "Data Source",values = c("red", "blue")) + #model in blue, actual in black
           theme(legend.position = "bottom")) #put legend at bottom
}


x11()

print(getGraph(new_result_2016,actual_yield_2016))

#different site_ids
ids = list("EF","EF_check","MW_trans_border")

#make a pdf file graph
pdf(
  file = "sorghumdata.pdf",
  width = 7,          # inches
  height = 6,         # inches
  useDingbats = FALSE # make sure symbols are rendered properly in the PDF
)

print(getGraph(new_result))

dev.off()

#make a pdf file graph
pdf(
  file = "newEFdata.pdf",
  width = 7,          # inches
  height = 6,         # inches
  useDingbats = FALSE # make sure symbols are rendered properly in the PDF
)

print(getGraph(new_result_2016,actual_yield_2016))

dev.off()
