library(dfoptim)
library(pryr)
library(dplyr)
library(BioCro)
library(ggplot2)
library(readxl)
library(nloptr)


## Justin's code for climate input to the model
climate = read.delim(file.path("~/imsi/Internship/data", 'filled_climate.tsv'))

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
climate_2018 = read.csv("../data/EF2018_weather.csv")
climate_2019 = read.csv("../data/EF2019_weather.csv")

climate16 = subset(climate, year == 2016 & doy > 167)  # According the these notes dated June 16, sorghum at SoyFACE had emerged, but not at the Energy Farm. It's a decent estimate.
climate17 = subset(climate, year == 2017 & doy > 151)
climate18 =  climate_2018  %>% filter(doy>158)
climate19 =  climate_2019  %>% filter(doy>170)


### Function to use a particular year's climate to input into the model as required.
get_yr_climate <-  function(year) {
  climate <- eval(parse(text =  paste0("climate", substr(as.character(year), 3, 4))))
  climate
}

#partitioning logistic modules
sorghum_steady_state_modules_logistic <- list(
  "soil_type_selector",
  "stomata_water_stress_linear",
  "leaf_water_stress_exponential",
  "parameter_calculator",
  "soil_evaporation",
  "c4_canopy",
  "partitioning_coefficient_logistic",
  "partitioning_growth_calculator",
  "thermal_time_development_rate_calculator"
)

sorghum_derivative_modules_logistic <- list(
  "thermal_time_senescence",
  "partitioning_growth",
  "thermal_time_trilinear",
  "one_layer_soil_profile",
  "development_index"
)

sorghum_integrator <- list(
  type = 'auto',
  output_step_size = 1.0,
  adaptive_rel_error_tol = 1e-5,
  adaptive_abs_error_tol = 1e-5,
  adaptive_max_steps = 200
)

# Do the calculations inside an empty list so that temporary variables are not created in .Global.
#logistic "default" parameters
sorghum_parameters_logistic = with(list(), {
  datalines =
    "symbol                     value
    absorptivity_par            0.8
    alpha1                      0.04
    alphab1                     0
    alphaLeaf                   15
    alphaRoot                   18
    alphaStem                   15
    atmospheric_pressure        101325
    b0                          0.08
    b1                          3
    beta                        0.93
    betaLeaf                    -15
    betaRoot                    -18
    betaStem                    -15
    Catm                        400
    chil                        1
    et_equation                 0
    Gs_min                      1e-3
    heightf                     3
    iSp                         3
    kd                          0.1
    kparm                       0.7
    kpLN                        0.2
    kRhizome_emr                -1e-8
    lat                         40
    LeafN                       2
    LeafN_0                     2
    leafwidth                   0.04
    leaf_transmittance          0.2
    leaf_reflectance            0.2
    lnb0                        -5
    lnb1                        18
    lnfun                       0
    lowerT                      3
    mrc1                        0.02
    mrc2                        0.03
    nalphab0                    0.02367
    nalphab1                    0.000488
    nileafn                     85
    nkln                        0.5
    nkpLN                       0.17
    nlayers                     10
    nlnb0                       -5
    nlnb1                       18
    nRdb0                       -4.5917
    nRdb1                       0.1247
    nvmaxb0                     -16.25
    nvmaxb1                     0.6938
    par_energy_content          0.235
    par_energy_fraction         0.5
    phi1                        0.01
    phi2                        10
    Rd                          0.8
    remobilization_fraction     0.6
    retrans                     0.9
    retrans_rhizome             1.0
    rsec                        0.2
    seneLeaf                    3000
    seneRhizome                 4000
    seneRoot                    4000
    seneStem                    3500
    soil_clod_size              0.04
    soil_depth                  1
    soil_reflectance            0.2
    soil_transmission           0.01
    soil_type_indicator         6
    specific_heat_of_air        1010
    Sp_thermal_time_decay       0.0004
    stefan_boltzman             5.67e-8
    tbase                       0
    topt_lower                  21
    topt_upper                  34
    tmax                        43
    theta                       0.83
    timestep                    1
    TTemr                       50
    TTveg                       750
    TTrep                       1450
    upperT                      37.5
    vmax1                       53
    vmax_n_intercept            0
    water_stress_approach       1"
  
  data_frame = utils::read.table(textConnection(datalines), header=TRUE)
  values = as.list(data_frame$value)
  names(values) = data_frame$symbol
  
  values
})

init_vals_logistic <- sorghum_initial_values
init_vals_logistic[['DVI']] <- -1 #need DVI for initial values when running logistic model
init_vals_logistic[['sowing_time']] <- 1


## This can be a full model or a partial model run depends on the given values.
partial_model_run <- function(params_to_change, yr, current_params) {
    return(partial_run_biocro(init_vals_logistic, current_params, get_yr_climate(yr), sorghum_steady_state_modules_logistic, sorghum_derivative_modules_logistic, params_to_change))
}

## partial_model <- partial_model_run(c("tmax", "theta"),  2017, sorghum_parameters_logistic)(x)



## Actual Datasets with lai,  leaf yield, stem yield from various years:
biomass_yield1 <- read_excel("~/imsi/Internship/data/biomass-yield1.xlsx")
above_ground_final_yield1 <- read_excel("~/imsi/Internship/data/above-ground-final-yield.xlsx")
biomass_partitioning1 <- read_excel("~/imsi/Internship/data/biomass-partitioning1.xlsx")
leaf_area1 <- read_excel("~/imsi/Internship/data/leaf-area1.xlsx")
lidar_lai1 <- read_excel("~/imsi/Internship/data/lidar-lai.xlsx")

## biomass_yield and above_ground_final_yield1
biomass_yield1$doy = as.numeric(strftime(biomass_yield1$date, format = "%j"))
biomass_yield1$year = format(as.Date(biomass_yield1$date, format ="%d/%m/%Y"),"%Y")
biomass_yield1$yield_Mg_ha = biomass_yield1$yield * 10
biomass_yield1[['first_row']] <- as.numeric(sub("_.*","",biomass_yield1[['row_set']]))

above_ground_final_yield1$doy = as.numeric(format(as.Date(above_ground_final_yield1$date, format ="%Y-%m-%d"),"%j"))
above_ground_final_yield1$yield_Mg_ha = above_ground_final_yield1$above_ground_dry_yield * 10
above_ground_final_yield1$year =  as.character(above_ground_final_yield1$year)

all_yield <- full_join(biomass_yield1, above_ground_final_yield1,c('site_id','doy','year','range','first_row','yield_Mg_ha'))
## for the harvest only run
## all_yield = above_ground_final_yield1

all_yield <- all_yield %>% filter(yield_Mg_ha != Inf & !is.na(yield_Mg_ha))



## biomass partitioning and leaf area datasets
biomass_partitioning1$doy   = as.numeric(strftime(biomass_partitioning1$date, format = "%j"))
biomass_partitioning1$year = format(as.Date(biomass_partitioning1$date, format ="%d/%m/%Y"),"%Y")
biomass_partitioning1[['first_row']] <- as.numeric(sub("_.*","",biomass_partitioning1[['row_set']]))

leaf_area1$doy   =  as.numeric(strftime(leaf_area1$date, format = "%j"))
leaf_area1$year = format(as.Date(leaf_area1$date, format ="%Y-%m-%d"),"%Y")

lidar_lai1$doy =  as.numeric(strftime(lidar_lai1$date, format = "%j"))
lidar_lai1$year = format(as.Date(lidar_lai1$date, format ="%d/%m/%Y"),"%Y")
lidar_lai1$site_id  = "EF"
lidar_lai1$LAI = lidar_lai1$lai
lidar_lai1 =  subset(lidar_lai1 ,  select  =  -c(lai))

# renamed LAI to avoid confusion between actual and modeled values of LAI
data_with_leaf <- inner_join(biomass_yield1, leaf_area1, by = c('site_id','doy','year','range','row_set'))
data_with_leaf[['LAI']] <- data_with_leaf[['leaf_area']] / (data_with_leaf[['row_length']] * data_with_leaf[['row_spacing']])
data_with_leaf[['first_row']] <- as.numeric(sub("_.*","",data_with_leaf[['row_set']]))

all_lai <- full_join(data_with_leaf, lidar_lai1,c('site_id','doy','year','range','first_row','LAI'))
# harvest only run:  all_lai <- lidar_lai1
all_lai <- all_lai %>% filter(!is.na(LAI) & LAI != Inf)

## Data with partitioning to allow for evaluation of leaf yield, stem yield
data_with_partitioning <- inner_join(all_yield,biomass_partitioning1,by=c('site_id','doy','year','range','row_set'))

data_with_partitioning[,'leaf_yield'] <- data_with_partitioning[,'leaf_mass'] / (data_with_partitioning[,'leaf_mass'] + data_with_partitioning[,'stem_mass']) * data_with_partitioning[,'yield_Mg_ha']
data_with_partitioning[,'stem_yield'] <- data_with_partitioning[,'stem_mass'] / (data_with_partitioning[,'leaf_mass'] + data_with_partitioning[,'stem_mass']) * data_with_partitioning[,'yield_Mg_ha']

data_with_partitioning <- data_with_partitioning %>% filter(leaf_yield!=Inf & !is.na(leaf_yield))
data_with_partitioning[['first_row']] <- data_with_partitioning[['first_row.x']]
data_with_partitioning <- subset(data_with_partitioning,select = -c(first_row.x,first_row.y))


## All data for respective years of both harvest and prior data of available lai's and yield:

#all 2017 data

actual_leaf_stem_2017 <- data_with_partitioning %>%
  filter(year == 2017)

mean_var_leaf_stem_2017 <- actual_leaf_stem_2017 %>%
  group_by(doy) %>%
  summarize(ActualMeanLeaf = mean(leaf_yield), VarianceLeaf = var(leaf_yield), ActualMeanStem = mean(stem_yield), VarianceStem = var(stem_yield))

actual_yield_2017 <- all_yield %>%
  filter(year == 2017)

mean_var_yield_2017 <- actual_yield_2017 %>%
  group_by(doy) %>%
  summarize(ActualYield = mean(yield_Mg_ha), VarianceYield = var(yield_Mg_ha))

actual_leaf_area_2017 <- all_lai %>%
  filter(year == 2017)

mean_var_leaf_area_2017 <- actual_leaf_area_2017 %>%
  group_by(doy) %>%
  summarize(ActualMeanLeafAreaIndex = mean(LAI), VarianceLAI = var(LAI))

#all 2018 data
actual_leaf_stem_2018 <- data_with_partitioning %>%
  filter(year == 2018)

mean_var_leaf_stem_2018 <- actual_leaf_stem_2018 %>%
  group_by(doy) %>%
  summarize(ActualMeanLeaf = mean(leaf_yield), VarianceLeaf = var(leaf_yield), ActualMeanStem = mean(stem_yield), VarianceStem = var(stem_yield))

actual_yield_2018 <- all_yield %>%
  filter(year == 2018)

mean_var_yield_2018 <- actual_yield_2018 %>%
  group_by(doy) %>%
  summarize(ActualYield = mean(yield_Mg_ha), VarianceYield = var(yield_Mg_ha))

actual_leaf_area_2018 <- all_lai %>%
  filter(year == 2018)

mean_var_leaf_area_2018 <- actual_leaf_area_2018 %>%
  group_by(doy) %>%
  summarize(ActualMeanLeafAreaIndex = mean(LAI), VarianceLAI = var(LAI))

#all 2019 data
actual_leaf_stem_2019 <- data_with_partitioning %>%
  filter(year == 2019)

mean_var_leaf_stem_2019 <- actual_leaf_stem_2019 %>%
  group_by(doy) %>%
  summarize(ActualMeanLeaf = mean(leaf_yield), VarianceLeaf = var(leaf_yield), ActualMeanStem = mean(stem_yield), VarianceStem = var(stem_yield))

actual_yield_2019 <- all_yield %>%
  filter(year == 2019)

mean_var_yield_2019 <- actual_yield_2019 %>%
  group_by(doy) %>%
  summarize(ActualYield = mean(yield_Mg_ha), VarianceYield = var(yield_Mg_ha))

actual_leaf_area_2019 <- all_lai %>%
  filter(year == 2019)

mean_var_leaf_area_2019 <- actual_leaf_area_2019 %>%
  group_by(doy) %>%
  summarize(ActualMeanLeafAreaIndex = mean(LAI), VarianceLAI = var(LAI))

## Obtain leaf stem yield functions to obtain actual data from a year
yrClimate <- function(year){
  if(year==2016)
  {
    return(climate16)
  }
  else if(year==2017)
  {
    return(climate17)
  }
  else if(year==2018)
  {
    return(climate18)
  }
  else
  {
    return(climate19)
  }
}

yrLeafStem <- function(year){
  if(year==2016)
  {
    return(actual_leaf_stem_2016)
  }
  else if(year==2017)
  {
    return(actual_leaf_stem_2017)
  }
  else if(year==2018)
  {
    return(actual_leaf_stem_2018)
  }
  else
  {
    return(actual_leaf_stem_2019)
  }
}

yrMeanVarLeafStem <- function(year){
  if(year==2016)
  {
    return(mean_var_leaf_stem_2016)
  }
  else if(year==2017)
  {
    return(mean_var_leaf_stem_2017)
  }
  else if(year==2018)
  {
    return(mean_var_leaf_stem_2018)
  }
  else
  {
    return(mean_var_leaf_stem_2019)
  }
}

yrLeafArea <- function(year){
  if(year==2016)
  {
    return(actual_leaf_area_2016)
  }
  else if(year==2017)
  {
    return(actual_leaf_area_2017)
  }
  else if(year==2018)
  {
    return(actual_leaf_area_2018)
  }
  else
  {
    return(actual_leaf_area_2019)
  }
}

yrMeanVarLeafArea <- function(year){
  if(year==2016)
  {
    return(mean_var_leaf_area_2016)
  }
  else if(year==2017)
  {
    return(mean_var_leaf_area_2017)
  }
  else if(year==2018)
  {
    return(mean_var_leaf_area_2018)
  }
  else
  {
    return(mean_var_leaf_area_2019)
  }
}

yrYield <- function(year){
  if(year==2016)
  {
    return(actual_yield_2016)
  }
  else if(year==2017)
  {
    return(actual_yield_2017)
  }
  else if(year==2018)
  {
    return(actual_yield_2018)
  }
  else
  {
    return(actual_yield_2019)
  }
}

yrMeanVarYield <- function(year){
  if(year==2016)
  {
    return(mean_var_yield_2016)
  }
  else if(year==2017)
  {
    return(mean_var_yield_2017)
  }
  else if(year==2018)
  {
    return(mean_var_yield_2018)
  }
  else
  {
    return(mean_var_yield_2019)
  }
}


# yrLeafStem <- function(year){eval(parse(text =  paste0("actual_leaf_stem_", year)))}
# yrMeanVarLeafStem <- function(year){eval(parse(text =  paste0("mean_var_leaf_stem_", year)))}
# yrLeafArea <- function(year){eval(parse(text =  paste0("actual_leaf_area_", year)))}
# yrMeanVarLeafArea <- function(year){eval(parse(text =  paste0("mean_var_leaf_area_", year)))}
# yrYield <- function(year){eval(parse(text =  paste0("actual_yield_", year)))}
# yrMeanVarYield <- function(year){eval(parse(text =  paste0("mean_var_yield_", year)))}


## df_inner_join_yield
df_inner_join <- function(actualLeafStem,actualMeanVar,modelResult){
  byDay <- modelResult %>% 
    group_by(doy) %>%
    summarize(meanLeaf = mean(Leaf), meanStem = mean(Stem)) #makes a new dataframe that gives the average yield per day (over all 24 values in the model_result dataframe)
  #Leaf + Stem is specific to sorghum and needs to be changed for other crops
  temp <- inner_join(byDay,actualLeafStem,by = c("doy")) #joins the two dataframes so we can compare values and run stats
  innerJoinDf <- inner_join(temp,actualMeanVar,by=c("doy")) #add the statistics
  innerJoinDf <- mutate(innerJoinDf,actualMinusModelLeaf = leaf_yield - meanLeaf) #makes a column for actual data minus model data
  innerJoinDf <- mutate(innerJoinDf,actualMinusModelStem = stem_yield - meanStem) #makes a column for actual data minus model data
  innerJoinDf <- mutate(innerJoinDf,actualMinusModelLeafAbs = abs(actualMinusModelLeaf)) #makes a column for absolute value of difference
  innerJoinDf <- mutate(innerJoinDf,actualMinusModelStemAbs = abs(actualMinusModelStem)) #makes a column for absolute value of difference
  innerJoinDf <- mutate(innerJoinDf,diffSquaresLeaf = actualMinusModelLeaf * actualMinusModelLeaf) #makes a column for the square of the difference
  innerJoinDf <- mutate(innerJoinDf,diffSquaresStem = actualMinusModelStem * actualMinusModelStem) #makes a column for the square of the difference
  innerJoinDf <- mutate(innerJoinDf,absPercentErrorLeaf = actualMinusModelLeafAbs/leaf_yield * 100) #makes a column for absolute value of percentage difference
  innerJoinDf <- mutate(innerJoinDf,absPercentErrorStem = actualMinusModelStemAbs/stem_yield * 100) #makes a column for absolute value of percentage difference
  innerJoinDf <- mutate(innerJoinDf,chiSquareToSumLeaf = diffSquaresLeaf/VarianceLeaf) #makes a column for square of difference over actual value (for chi square calculation)
  innerJoinDf <- mutate(innerJoinDf,chiSquareToSumStem = diffSquaresStem/VarianceStem) #makes a column for square of difference over actual value (for chi square calculation)
  actual_mean_leaf <- as.numeric(actualLeafStem %>% summarize(mean = mean(leaf_yield)))
  innerJoinDf <- mutate(innerJoinDf,totSquareToSumLeaf = (leaf_yield - actual_mean_leaf)^2)
  actual_mean_stem <- as.numeric(actualLeafStem %>% summarize(mean = mean(stem_yield)))
  innerJoinDf <- mutate(innerJoinDf,totSquareToSumStem = (stem_yield - actual_mean_stem)^2)
  return(innerJoinDf)
}

df_inner_join_yield <- function(actualYield,YieldMeanVar,modelResult){
  byDay <- modelResult %>% 
    group_by(doy) %>%
    summarize(meanYield = mean(Leaf) + mean(Stem)) #makes a new dataframe that gives the average yield per day (over all 24 values in the model_result dataframe)
  #Leaf + Stem is specific to sorghum and needs to be changed for other crops
  temp <- inner_join(byDay,actualYield,by = c("doy")) #joins the two dataframes so we can compare values and run stats
  innerJoinDf <- inner_join(temp,YieldMeanVar,by=c("doy"))
  innerJoinDf <- mutate(innerJoinDf,actualMinusModel = yield_Mg_ha - meanYield) #makes a column for actual data minus model data
  innerJoinDf <- mutate(innerJoinDf,actualMinusModelAbs = abs(actualMinusModel)) #makes a column for absolute value of difference
  innerJoinDf <- mutate(innerJoinDf,diffSquares = actualMinusModel * actualMinusModel) #makes a column for the square of the difference
  innerJoinDf <- mutate(innerJoinDf,absPercentError = actualMinusModelAbs/yield_Mg_ha * 100) #makes a column for absolute value of percentage difference
  innerJoinDf <- mutate(innerJoinDf,chiSquareToSum = diffSquares/VarianceYield) #makes a column for square of difference over actual value (for chi square calculation)
  actual_mean_yield <- as.numeric(actualYield %>% summarize(mean = mean(yield_Mg_ha)))
  innerJoinDf <- mutate(innerJoinDf,totSquareToSum = (yield_Mg_ha - actual_mean_yield)^2)
  return(innerJoinDf)
}

df_inner_join_leaf_area <- function(actualData,LAIMeanVar,modelResult){
  byDay <- modelResult %>% 
    group_by(doy) %>%
    summarize(leafAreaIndex = mean(lai)) #makes a new dataframe that gives the average lai per day (over all 24 values in the model_result dataframe)
  temp <- inner_join(byDay,actualData,by = c("doy")) #joins the two dataframes so we can compare values and run stats
  innerJoinDf <- inner_join(temp,LAIMeanVar,by=c("doy"))
  innerJoinDf <- mutate(innerJoinDf,actualMinusModel = LAI - leafAreaIndex) #makes a column for actual data minus model data
  innerJoinDf <- mutate(innerJoinDf,actualMinusModelAbs = abs(actualMinusModel)) #makes a column for absolute value of difference
  innerJoinDf <- mutate(innerJoinDf,diffSquares = actualMinusModel * actualMinusModel) #makes a column for the square of the difference
  innerJoinDf <- mutate(innerJoinDf,absPercentError = actualMinusModelAbs/LAI * 100) #makes a column for absolute value of percentage difference
  innerJoinDf <- mutate(innerJoinDf,chiSquareToSum = diffSquares/VarianceLAI) #makes a column for square of difference over actual value (for chi square calculation)
  actual_mean_lai <- as.numeric(actualData %>% summarize(mean = mean(LAI)))
  innerJoinDf <- mutate(innerJoinDf,totSquareToSum = (LAI - actual_mean_lai)^2)
  return(innerJoinDf)
}

## all the statistics are obtained here: model_result is important as this should run for various values of model_result.
Statistics1 <- function(model_result, year){
  actual_leaf_stem <- yrLeafStem(year)
  mean_var_leaf_stem <- yrMeanVarLeafStem(year)
  actual_leaf_data <- yrLeafArea(year)
  mean_var_leaf_area <- yrMeanVarLeafArea(year)
  actual_yield <- yrYield(year)
  mean_var_yield <- yrMeanVarYield(year)
  innerJoinDf1 <- df_inner_join(actual_leaf_stem,mean_var_leaf_stem,model_result)
  innerJoinDf2 <- df_inner_join_leaf_area(actual_leaf_data,mean_var_leaf_area,model_result)
  innerJoinDf3 <- df_inner_join_yield(actual_yield,mean_var_yield,model_result)
  
  #find sums of a few columns for stats calculations
  SumOfDiffsSquared = sum(innerJoinDf1[,'diffSquaresLeaf']) + sum(innerJoinDf1[,'diffSquaresStem']) + sum(innerJoinDf2[,'diffSquares']) + sum(innerJoinDf3[,'diffSquares'])
  SumOfAbsDiffs = sum(innerJoinDf1[,'actualMinusModelLeafAbs']) + sum(innerJoinDf1[,'actualMinusModelStemAbs']) + sum(innerJoinDf2[,'actualMinusModelAbs']) + sum(innerJoinDf3[,'actualMinusModelAbs'])
  SumOfAbsPercs = sum(innerJoinDf1[,'absPercentErrorLeaf']) + sum(innerJoinDf1[,'absPercentErrorLeaf']) + sum(innerJoinDf2[,'absPercentError']) + sum(innerJoinDf3[,'absPercentError'])
  
  #number of rows
  n = nrow(innerJoinDf1) * 2 + nrow(innerJoinDf2) + nrow(innerJoinDf3)
  
  #stats calculations
  RMSE = sqrt(SumOfDiffsSquared/n)
  MAE = SumOfAbsDiffs/n
  MAPE = SumOfAbsPercs/n
  ChiSquare = sum(innerJoinDf1[,'chiSquareToSumLeaf']) + sum(innerJoinDf1[,'chiSquareToSumStem']) + sum(innerJoinDf2[,'chiSquareToSum']) + sum(innerJoinDf3[,'chiSquareToSum'])
  ChiSquareNormed = ChiSquare/n
  TotSquare = sum(innerJoinDf1[,'totSquareToSumLeaf']) + sum(innerJoinDf1[,'totSquareToSumStem']) + sum(innerJoinDf2[,'totSquareToSum']) + sum(innerJoinDf3[,'totSquareToSum'])
  RSquared = 1- (SumOfDiffsSquared/TotSquare)
  
  Count1 = innerJoinDf1 %>% count(doy)
  Count1 <- mutate(Count1,recip_sqrt = 1/sqrt(n))
  Mahal1 <- innerJoinDf1 %>%
    group_by(doy) %>%
    summarize(MahalLeafSum = sqrt(sum(chiSquareToSumLeaf)),MahalStemSum = sqrt(sum(chiSquareToSumStem)))
  MahalFinal1 <- mutate(full_join(Count1,Mahal1,by=c('doy')),MahalLeafNormed = MahalLeafSum/n,MahalStemNormed =MahalStemSum/n)
  Count2 = innerJoinDf2 %>% count(doy)
  Count2 <- mutate(Count2,recip_sqrt = 1/sqrt(n))
  Mahal2 <- innerJoinDf2 %>%
    group_by(doy) %>%
    summarize(MahalLAISum = sqrt(sum(chiSquareToSum)))
  MahalFinal2 <- mutate(full_join(Count2,Mahal2,by=c('doy')),MahalLAINormed = MahalLAISum/n)
  Count3 = innerJoinDf3 %>% count(doy)
  Count3 <- mutate(Count3,recip_sqrt = 1/sqrt(n))
  factor_to_norm <- 1/(sum(Count1[,'recip_sqrt'])+sum(Count2[,'recip_sqrt'])+sum(Count3[,'recip_sqrt']))
  Mahal3 <- innerJoinDf3 %>%
    group_by(doy) %>%
    summarize(MahalYieldSum = sqrt(sum(chiSquareToSum))/n)
  MahalFinal3 <- mutate(full_join(Count3,Mahal3,by=c('doy')),MahalYieldNormed = MahalYieldSum/n)
  Mahalanobis = (sum(MahalFinal1[,'MahalLeafNormed']) + sum(MahalFinal1[,'MahalStemNormed']) + sum(MahalFinal2[,'MahalLAINormed']) + sum(MahalFinal3[,'MahalYieldNormed'])) * factor_to_norm
  stats = data.frame(RSquared = c(RSquared), RMSE = c(RMSE), MAE = c(MAE), MAPE = c(MAPE), MahalToSum = c(ChiSquare), MahalToSum_normed = c(ChiSquareNormed), Mahalanobis_normed = c(Mahalanobis))
  
  return(stats)
}

## for harvest only model
Statistics2 <- function(model_result, year){
  actual_leaf_stem <- yrLeafStem(year)
  mean_var_leaf_stem <- yrMeanVarLeafStem(year)
  actual_leaf_data <- yrLeafArea(year)
  mean_var_leaf_area <- yrMeanVarLeafArea(year)
  actual_yield <- yrYield(year)
  mean_var_yield <- yrMeanVarYield(year)
  
  innerJoinDf1 <- df_inner_join(actual_leaf_stem, mean_var_leaf_stem,model_result)
  innerJoinDf2 <- df_inner_join_leaf_area(actual_leaf_data,mean_var_leaf_area,model_result)
  innerJoinDf3 <- df_inner_join_yield(actual_yield,mean_var_yield,model_result)
  
  #find sums of a few columns for stats calculations
  SumOfDiffsSquared = sum(innerJoinDf1[,'diffSquaresLeaf']) + sum(innerJoinDf1[,'diffSquaresStem']) + sum(innerJoinDf2[,'diffSquares']) + sum(innerJoinDf3[,'diffSquares'])
  SumOfAbsDiffs = sum(innerJoinDf1[,'actualMinusModelLeafAbs']) + sum(innerJoinDf1[,'actualMinusModelStemAbs']) + sum(innerJoinDf2[,'actualMinusModelAbs']) + sum(innerJoinDf3[,'actualMinusModelAbs'])
  SumOfAbsPercs = sum(innerJoinDf1[,'absPercentErrorLeaf']) + sum(innerJoinDf1[,'absPercentErrorLeaf']) + sum(innerJoinDf2[,'absPercentError']) + sum(innerJoinDf3[,'absPercentError'])
  
  #number of rows
  n = nrow(innerJoinDf1) * 2 + nrow(innerJoinDf2) + nrow(innerJoinDf3)
  
  #stats calculations
  RMSE = sqrt(SumOfDiffsSquared/n)
  MAE = SumOfAbsDiffs/n
  MAPE = SumOfAbsPercs/n
  ChiSquare = sum(innerJoinDf1[,'chiSquareToSumLeaf']) + sum(innerJoinDf1[,'chiSquareToSumStem']) + sum(innerJoinDf2[,'chiSquareToSum']) + sum(innerJoinDf3[,'chiSquareToSum'])
  ChiSquareNormed = ChiSquare/n
  TotSquare = sum(innerJoinDf1[,'totSquareToSumLeaf']) + sum(innerJoinDf1[,'totSquareToSumStem']) + sum(innerJoinDf2[,'totSquareToSum']) + sum(innerJoinDf3[,'totSquareToSum'])
  RSquared = 1- (SumOfDiffsSquared/TotSquare)
  
  Count1 = innerJoinDf1 %>% count(doy)
  Count1 <- mutate(Count1,recip_sqrt = 1/sqrt(n))
  Mahal1 <- innerJoinDf1 %>%
    group_by(doy) %>%
    summarize(MahalLeafSum = sqrt(sum(chiSquareToSumLeaf)),MahalStemSum = sqrt(sum(chiSquareToSumStem)))
  MahalFinal1 <- mutate(full_join(Count1,Mahal1,by=c('doy')),MahalLeafNormed = MahalLeafSum/n,MahalStemNormed =MahalStemSum/n)
  Count2 = innerJoinDf2 %>% count(doy)
  Count2 <- mutate(Count2,recip_sqrt = 1/sqrt(n))
  Mahal2 <- innerJoinDf2 %>%
    group_by(doy) %>%
    summarize(MahalLAISum = sqrt(sum(chiSquareToSum)))
  MahalFinal2 <- mutate(full_join(Count2,Mahal2,by=c('doy')),MahalLAINormed = MahalLAISum/n)
  Count3 = innerJoinDf3 %>% count(doy)
  Count3 <- mutate(Count3,recip_sqrt = 1/sqrt(n))
  factor_to_norm <- 1/(sum(Count1[,'recip_sqrt'])+sum(Count2[,'recip_sqrt'])+sum(Count3[,'recip_sqrt']))
  Mahal3 <- innerJoinDf3 %>%
    group_by(doy) %>%
    summarize(MahalYieldSum = sqrt(sum(chiSquareToSum))/n)
  MahalFinal3 <- mutate(full_join(Count3,Mahal3,by=c('doy')),MahalYieldNormed = MahalYieldSum/n)
  Mahalanobis = (sum(MahalFinal2[,'MahalLAINormed']) + sum(MahalFinal3[,'MahalYieldNormed'])) * factor_to_norm
  stats = data.frame(RSquared = c(RSquared), RMSE = c(RMSE), MAE = c(MAE), MAPE = c(MAPE), MahalToSum = c(ChiSquare), MahalToSum_normed = c(ChiSquareNormed), Mahalanobis_normed = c(Mahalanobis))
  
  return(stats)
}



## This allows to use the year and partial  model to return the required statistic.
StatisticsLeafAndYield <- function(x, sorghum_partial, year){
  result = sorghum_partial(year)(x) #this runs the model on the current parameters
  return(Statistics2(result,year))
}


#optimizes lai and yield fit simultaneously
new_parameters_logistic <- function(params_to_change,lower_bounds,upper_bounds,current_params,year, st, optim.control){
  sorghum_partial <- function(year){
    return(partial_run_biocro(init_vals_logistic,current_params,
                              yrClimate(year),
                              sorghum_steady_state_modules_logistic,sorghum_derivative_modules_logistic, sorghum_integrator, params_to_change))
  }
  #st is stat: RMSE, MAE, MAPE, or ChiSquare
  sorghum_stat <- function(x, stat = st){
    return(as.numeric(StatisticsLeafAndYield(x,sorghum_partial,year)[,c(stat)]))
  }
  para <- as.numeric(unname(current_params[params_to_change]))
  ans <- hjkb(para, sorghum_stat, lower = lower_bounds, upper = upper_bounds, control =  optim.control)
  new_params <- ans$par
  names(new_params) <- params_to_change
  altered_params <- current_params
  for(p in params_to_change){
    altered_params[[p]] <- new_params[[p]]
  }
  return(altered_params)
}

## Fitting on 2017:
## initial parameters:
## iSp = 3, Sp_thermal_time_decay = 4e-04
## alphaRoot = 18, alphaStem = 15
## alphaLeaf  = 15,  betaRoot= -18
##  betaStem = -15, betaLeaf = -15
## TTemr =  50, TTveg =  750
## TTrep =  1450
## resulting value =3.618

## resulting parameters:
## iSp = 5.34, Sp_thermal_time_decay = -0.0004847716
## alphaRoot = 17.47616, alphaStem = 15.22067
## alphaLeaf  = 15.5809,  betaRoot= -17.77189
##  betaStem = -14.64219, betaLeaf = -15.63811
## TTemr =  45.28689, TTveg =  1058.565
## TTrep =  2300.578
## resulting value = 3.095


getGraphLeafArea <- function(model_result, actual_leaf_area, sub="") {
  fullJoinDf <- full_join(model_result, actual_leaf_area, by = c("doy")) #joins the two dataframes so we can graph on the same axes
  #stats_df = Statistics1(model_result,actual_yield)
  #stats_str = ""
  #for(stat in colnames(stats_df)){
  #  stats_str=paste0(stats_str,stat,": ",round(stats_df[1,stat],digits=2),"  ")
  #}
  
  return(ggplot(fullJoinDf, aes(time)) + #time as x-axis
           geom_point(aes(y = lai, color = "BioCro Model")) + #model data
           geom_point(aes(y = LAI, color = "Actual LAI")) + #actual data
           xlab("Day of Year") + #x-axis label
           ylab("Leaf Area Index (unitless)") + #y-axis label
           labs(title = "Sorghum Model Leaf Area Index vs. Actual Data", #graph title
                subtitle = "") +  #subtitle + #put stats in caption
           scale_color_manual(name = "",values = c("red", "blue")) + #model in blue, actual in red
           theme(legend.position = "bottom")) #put legend at bottom
}

#function returns a graph of the model data with the actual data
getGraphYield <- function(model_result, actual_yield, sub="") {
  fullJoinDf <- full_join(model_result,actual_yield,by = c("doy")) #joins the two dataframes so we can graph on the same axes
  #stats_df = Statistics1(model_result,actual_yield,actual_leaf_area)
  #stats_str = "Statistics for Leaf Area and Yield (combined) "
  #for(stat in colnames(stats_df)){
  #  stats_str=paste0(stats_str,stat,": ",round(stats_df[1,stat],digits=2),"  ")
  #}
  
  return(ggplot(fullJoinDf,aes(time)) + #time as x-axis
           geom_point(aes(y = (Leaf + Stem), color = "BioCro Model")) + #model data
           geom_point(aes(y = yield_Mg_ha, color = "Actual Yield")) + #actual data
           xlab("Day of Year") + #x-axis label
           ylab("Leaf + Stem (Mg/ha)") + #y-axis label
           labs(title = "Sorghum Model Yield vs. Actual Data", #graph title
                subtitle = sub) + #subtitle caption
           scale_color_manual(name = "Data Source",values = c("red", "blue")) + #model in blue, actual in red
           theme(legend.position = "bottom")) #put legend at bottom
}

library(cowplot)

#4 graphs side by side
getGraphAll <- function(year, model_result, subleaf="", subyield="") {
  actual_leaf_stem <- yrLeafStem(year)
  fullJoinDf <- full_join(model_result,actual_leaf_stem,by = c("doy")) #joins the two dataframes so we can graph on the same axes
  stats_df = Statistics1(model_result, year)
  stats_str = "Statistics for Leaf Area, Leaf, Stem (combined) "
  for(stat in c("RMSE","MAPE", "Mahalanobis_normed")){
    stats_str=paste0(stats_str,stat,": ",round(stats_df[1,stat],digits=2),"  ")
  }
  
  print(stats_str)
  
  actual_leaf_area <- yrLeafArea(year)
  actual_yield <- yrYield(year)
  
  LeafArea = getGraphLeafArea(model_result, actual_leaf_area, sub=subleaf)
  Yield = getGraphYield(model_result, actual_yield, sub=subyield)
  
  Leaf = ggplot(fullJoinDf, aes(time)) + #time as x-axis
    geom_point(aes(y = Leaf, color = "BioCro Model")) + #model data
    geom_point(aes(y = leaf_yield, color = "Actual Leaf")) + #actual data
    xlab("Day of Year") + #x-axis label
    ylab("Leaf (Mg/ha)") + #y-axis label
    labs(title = "Sorghum Model Leaf vs. Actual Data", #graph title
         subtitle = subyield) + #subtitle
    scale_color_manual(name = "Data Source",values = c("red", "blue")) + #model in blue, actual in red
    theme(legend.position = "bottom") #put legend at bottom
  
  Stem = ggplot(fullJoinDf,aes(time)) + #time as x-axis
    geom_point(aes(y = Stem, color = "BioCro Model")) + #model data
    geom_point(aes(y = stem_yield, color = "Actual Stem")) + #actual data
    xlab("Day of Year") + #x-axis label
    ylab("Stem (Mg/ha)") + #y-axis label
    labs(title = "Sorghum Model Stem vs. Actual Data", #graph title
         subtitle = subyield, #subtitle
         caption = stats_str) + #put stats in caption
    scale_color_manual(name = "Data Source",values = c("red", "blue")) + #model in blue, actual in red
    theme(legend.position = "bottom") #put legend at bottom
  
  return(cowplot::plot_grid(LeafArea, Yield, Leaf, Stem, ncol = 1))
}


###### if somehow there are constraints, two ways can be used. One is to use constroptim or nloptr::auglag
###### which can be seen in "2016_comparison_EF.Rmd" file.


# sorghum_parameters_logistic_final = sorghum_parameters_logistic
# for(p in params_to_change){
#   sorghum_parameters_logistic_final[[p]] <- new_params_final[[p]]
# }


model_result_2017_final  <- run_biocro(
  init_vals_logistic,
  new_params_final,
  yrClimate(2017),
  sorghum_steady_state_modules_logistic,
  sorghum_derivative_modules_logistic)


sorghum_parameters_logistic = with(list(), {
  datalines =
    "symbol                     value
    absorptivity_par            0.8
    alpha1                      0.04
    alphab1                     0
    alphaLeaf                   15
    alphaRoot                   18
    alphaStem                   15
    atmospheric_pressure        101325
    b0                          0.08
    b1                          3
    beta                        0.93
    betaLeaf                    -15
    betaRoot                    -18
    betaStem                    -15
    Catm                        400
    chil                        1
    et_equation                 0
    Gs_min                      1e-3
    heightf                     3
    iSp                         3
    kd                          0.1
    kparm                       0.7
    kpLN                        0.2
    kRhizome_emr                -1e-8
    lat                         40
    LeafN                       2
    LeafN_0                     2
    leafwidth                   0.04
    leaf_transmittance          0.2
    leaf_reflectance            0.2
    lnb0                        -5
    lnb1                        18
    lnfun                       0
    lowerT                      3
    mrc1                        0.02
    mrc2                        0.03
    nalphab0                    0.02367
    nalphab1                    0.000488
    nileafn                     85
    nkln                        0.5
    nkpLN                       0.17
    nlayers                     10
    nlnb0                       -5
    nlnb1                       18
    nRdb0                       -4.5917
    nRdb1                       0.1247
    nvmaxb0                     -16.25
    nvmaxb1                     0.6938
    par_energy_content          0.235
    par_energy_fraction         0.5
    phi1                        0.01
    phi2                        10
    Rd                          0.8
    remobilization_fraction     0.6
    retrans                     0.9
    retrans_rhizome             1.0
    rsec                        0.2
    seneLeaf                    3000
    seneRhizome                 4000
    seneRoot                    4000
    seneStem                    3500
    soil_clod_size              0.04
    soil_depth                  1
    soil_reflectance            0.2
    soil_transmission           0.01
    soil_type_indicator         6
    specific_heat_of_air        1010
    Sp_thermal_time_decay       0.0004
    stefan_boltzman             5.67e-8
    tbase                       0
    topt_lower                  21
    topt_upper                  34
    tmax                        43
    theta                       0.83
    timestep                    1
    TTemr                       50
    TTveg                       750
    TTrep                       1450
    upperT                      37.5
    vmax1                       53
    vmax_n_intercept            0
    water_stress_approach       1"
  
  data_frame = utils::read.table(textConnection(datalines), header=TRUE)
  values = as.list(data_frame$value)
  names(values) = data_frame$symbol
  values
})


## Iteration 1: All data 2000 iterations
init_par1 <-  data.frame()
init_par1  <- read.delim("~/imsi/Internship/results/Surya_init_params/init_params1_2017_alldata.txt", sep = " ", header=  FALSE)
init_par1 <- t(init_par1)
colnames(init_par1) <- c("parameter", "value")

init_par1_list  <- list()
for (j in 1:length(init_par1[,1])) {
init_par1_list[init_par1[j,1]]  <- init_par1[j,2]}

# init_par1_list  <- sorghum_parameters_logistic

model_result_2017_initial  <- run_biocro(
  init_vals_logistic,
  init_par1_list,
  yrClimate(2017),
  sorghum_steady_state_modules_logistic,
  sorghum_derivative_modules_logistic)

model_result_2018_initial  <- run_biocro(
  init_vals_logistic,
  init_par1_list,
  yrClimate(2018),
  sorghum_steady_state_modules_logistic,
  sorghum_derivative_modules_logistic)

model_result_2019_initial  <- run_biocro(
  init_vals_logistic,
  init_par1_list,
  yrClimate(2019),
  sorghum_steady_state_modules_logistic,
  sorghum_derivative_modules_logistic)

Statistics1(model_result_2017_initial, 2017)   ## mahal_norm = 2.53
Statistics1(model_result_2018_initial, 2018) 

Statistics2(model_result_2017_initial, 2017)   ## mahal_norm = 2.53
Statistics2(model_result_2018_initial, 2018) 

# getGraphLeafArea(model_result_2017_init, actual_leaf_area_2017)
# getGraphYield(model_result_2017_init, actual_leaf_area_2017)
getGraphAll(2017, model_result_2017_initial)


new_params_final <- new_parameters_logistic(c("iSp","Sp_thermal_time_decay","alphaRoot","alphaStem","alphaLeaf",
                                              "betaRoot","betaStem","betaLeaf",
                                              "TTemr","TTveg","TTrep"),
                                            c(0.3,-1e-2,-50,-50,-50,
                                              -50,-50,-50,
                                              0,300,300),
                                            c(200,1e-2,50,50,50,
                                              50,50,50,
                                              10000,10000,10000),
                                            init_par1_list, 2017,
                                            "Mahalanobis_normed", list(maxfeval  = 2000))

init_par1_list[c("iSp","Sp_thermal_time_decay","alphaRoot","alphaStem","alphaLeaf",
                 "betaRoot","betaStem","betaLeaf",
                 "TTemr","TTveg","TTrep")]

write.csv(new_params_final, "~/imsi/Internship/results/Surya_init_params/final_params_2017_harvestdata.txt")

model_result_2017_final  <- run_biocro(
  init_vals_logistic,
  new_params_final,
  yrClimate(2017),
  sorghum_steady_state_modules_logistic,
  sorghum_derivative_modules_logistic)

model_result_2018_final  <- run_biocro(
  init_vals_logistic,
  new_params_final,
  yrClimate(2018),
  sorghum_steady_state_modules_logistic,
  sorghum_derivative_modules_logistic)

Statistics1(model_result_2017_final, 2017)
Statistics1(model_result_2018_final, 2018)

getGraphAll(2017, model_result_2017_final)
getGraphAll(2018, model_result_2018_final)

getGraphAll(2017, model_result_2017_initial)
getGraphAll(2018, model_result_2018_initial)


plot_grid(getGraphAll(2017, model_result_2017_initial), getGraphAll(2017, model_result_2017_final), ncol = 2)
plot_grid(getGraphAll(2018, model_result_2018_initial), getGraphAll(2018, model_result_2018_final), ncol = 2)

#### Same with only harvest data (Run the same above by removing biomass_yield)


