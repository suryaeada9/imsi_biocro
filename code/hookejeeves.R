library(dfoptim)
library(dplyr)

library(BioCro)

library(ggplot2)

library(patchwork)
library(readxl)

#partitioning selector modules
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
  "thermal_time_trilinear",
  "one_layer_soil_profile"
)

# Do the calculations inside an empty list so that temporary variables are not created in .Global.
sorghum_parameters = with(list(), {
  datalines =
    "symbol                     value
    absorptivity_par            0.8
    alpha1                      0.04
    alphab1                     0
    atmospheric_pressure        101325
    b0                          0.08
    b1                          3
    beta                        0.93
    Catm                        400
    chil                        1.43    # An estimate for sorghum. Table 15.1, page 253 of Campbell and Norman. An introduction to environmental biophysics. 2nd edition.
    et_equation                 0
    Gs_min                      1e-3
    heightf                     3
    iSp                         1.7
    kd                          0.1
    kGrain1                     0
    kGrain2                     0
    kGrain3                     0
    kGrain4                     0
    kGrain5                     0
    kGrain6                     0
    kLeaf1                      0.8
    kLeaf2                      0.7
    kLeaf3                      0.49
    kLeaf4                      0.49
    kLeaf5                      0.49
    kLeaf6                      0.49
    kparm                       0.7
    kpLN                        0.2
    kRhizome1                   -0.0008
    kRhizome2                   0
    kRhizome3                   0
    kRhizome4                   0
    kRhizome5                   0
    kRhizome6                   0
    kRoot1                      0.1
    kRoot2                      0.01
    kRoot3                      0.01
    kRoot4                      0.01
    kRoot5                      0.01
    kRoot6                      0.01
    kStem1                      0.1
    kStem2                      0.29
    kStem3                      0.5
    kStem4                      0.5
    kStem5                      0.5
    kStem6                      0.5
    lat                         40
    LeafN                       2
    LeafN_0                     2
    leafwidth                   0.04
    leaf_reflectance            0.2
    leaf_transmittance          0.2
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
    Sp_thermal_time_decay       0
    stefan_boltzman             5.67e-8
    tbase                       0
    topt_lower                  21
    topt_upper                  34
    tmax                        43
    theta                       0.83
    timestep                    1
    tp1                         562
    tp2                         1312
    tp3                         2063
    tp4                         2676
    tp5                         3211
    upperT                      37.5
    vmax1                       55
    vmax_n_intercept            0
    water_stress_approach       1"
  
  data_frame = utils::read.table(textConnection(datalines), header=TRUE)
  values = as.list(data_frame$value)
  names(values) = data_frame$symbol
  values
})

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

# Do the calculations inside an empty list so that temporary variables are not created in .Global.
#logistic "default" parameters
#TTemr is set to 50, because our starting date estimates seem very close to actual time of emergence
#TTrep is set to 5000, since energy sorghum doesn't flower
#estimated alphaLeaf, alphaRoot, alphaStem, betaLeaf, betaRoot, betaStem using averages taken from the paper linked in comments of partitioning_coefficient_logistic file
#These aren't accurate estimates, since that paper is talking about soybeans and maize and crops very different from energy sorghum
#Estimated iSp=3 and Sp_thermal_time_decay=0.0004 from https://www.researchgate.net/figure/Time-course-of-specific-leaf-area-SLA-panels-a-b-and-c-and-leaf-area-index-LAI_fig2_235797972
#These iSp and Sp_thermal_time_decay starting values made the fitting a lot better
#Took vmax1 as an average from the vmax file in the TERRA drive folder
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
    TTrep                       5000
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

#climate data from TERRA drive folder
climate = read.delim(file.path("C://Users/stark/OneDrive/Documents2021/biocro-dev", 'filled_climate.tsv'))

#unit conversion
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

climate_2016 <- get_climate(2016,167) #got these starting dates from Justin
climate_2017 <- get_climate(2017,151) #got these starting dates from Justin
climate_2018 = read.csv(file.path("C://Users/stark/OneDrive/Documents2021/biocro-dev", 'EF2018_weather.csv'))
climate_2019 = read.csv(file.path("C://Users/stark/OneDrive/Documents2021/biocro-dev", 'EF2019_weather.csv'))
climate_2018 <- climate_2018 %>% filter(doy>158) #got these by going a few days before very small LAI values in the lidar lai file
climate_2019 <- climate_2019 %>% filter(doy>170) #got these by going a few days before very small LAI values in the lidar lai file

#returns the climate for a given year
yrClimate <- function(year){
  if(year==2016)
  {
    return(climate_2016)
  }
  else if(year==2017)
  {
    return(climate_2017)
  }
  else if(year==2018)
  {
    return(climate_2018)
  }
  else
  {
    return(climate_2019)
  }
}

#read in data
biomass_yield1 <- read_excel("C:/Users/stark/OneDrive/Documents2021/biocro-dev/biomass-yield1.xlsx")
above_ground_final_yield1 <- read_excel("C:/Users/stark/OneDrive/Documents2021/biocro-dev/above-ground-final-yield.xlsx")
biomass_partitioning1 <- read_excel("C:/Users/stark/OneDrive/Documents2021/biocro-dev/biomass-partitioning1.xlsx")
leaf_area1 <- read_excel("C:/Users/stark/OneDrive/Documents2021/biocro-dev/leaf-area1.xlsx")
lidar_lai1 <- read_excel("C:/Users/stark/OneDrive/Documents2021/biocro-dev/lidar-lai.xlsx")


#format dates and other things about the data

biomass_yield1 = within(biomass_yield1, {
  doy = lubridate::yday(as.Date(date))
})
biomass_yield1 = within(biomass_yield1, {
  year = lubridate::year(as.Date(date))
})
biomass_yield1 = within(biomass_yield1, {
  yield_Mg_ha = yield * 10
})

biomass_yield1[['first_row']] <- as.numeric(sub("_.*","",biomass_yield1[['row_set']]))


above_ground_final_yield1 = within(above_ground_final_yield1, {
  doy = lubridate::yday(as.Date(date))
})
above_ground_final_yield1 = within(above_ground_final_yield1, {
  year = lubridate::year(as.Date(date))
})
above_ground_final_yield1 = within(above_ground_final_yield1, {
  yield_Mg_ha = above_ground_dry_yield * 10
})

all_yield <- full_join(biomass_yield1,above_ground_final_yield1,c('site_id','doy','year','range','first_row','yield_Mg_ha'))

all_yield <- all_yield %>% filter(yield_Mg_ha != Inf & !is.na(yield_Mg_ha)) #get rid of null values


final_yield_only <- above_ground_final_yield1 %>% filter(yield_Mg_ha != Inf & !is.na(yield_Mg_ha))

biomass_partitioning1 = within(biomass_partitioning1, {
  doy = lubridate::yday(as.Date(date))
})

biomass_partitioning1 = within(biomass_partitioning1, {
  year = lubridate::year(as.Date(date))
})

biomass_partitioning1[['first_row']] <- as.numeric(sub("_.*","",biomass_partitioning1[['row_set']]))

leaf_area1 = within(leaf_area1, {
  doy = lubridate::yday(as.Date(date))
})

leaf_area1 = within(leaf_area1, {
  year = lubridate::year(as.Date(date))
})

lidar_lai1 = within(lidar_lai1, {
  doy = lubridate::yday(as.Date(date))
})

lidar_lai1 = within(lidar_lai1, {
  year = lubridate::year(as.Date(date))
})

lidar_lai1[['site_id']] <- "EF" #all years in lidar lai file are at EF

#rename lai to LAI so as not to get confused with BioCro's predicted lai later
lidar_lai1[['LAI']] <- lidar_lai1[['lai']]
lidar_lai1 <- subset(lidar_lai1, select = -c(lai) )


data_with_leaf <- inner_join(biomass_yield1,leaf_area1,by = c('site_id','doy','year','range','row_set'))

#calculate leaf area index from leaf area
data_with_leaf[['LAI']] <- data_with_leaf[['leaf_area']] / (data_with_leaf[['row_length']] * data_with_leaf[['row_spacing']])
data_with_leaf[['first_row']] <- as.numeric(sub("_.*","",data_with_leaf[['row_set']]))


all_lai <- full_join(data_with_leaf,lidar_lai1,c('site_id','doy','year','range','first_row','LAI'))
all_lai <- all_lai %>% filter(!is.na(LAI) & LAI != Inf) #get rid of null values
#NOTE - in order to calculate MAPE for 2018 and not get an Inf value, we would need to add the condition that LAI > 1e-10
#Otherwise, we are dividing by values too close to 0 in the MAPE calculation

data_with_partitioning <- inner_join(all_yield,biomass_partitioning1,by=c('site_id','doy','year','range','row_set'))

data_with_partitioning[,'leaf_yield'] <- data_with_partitioning[,'leaf_mass'] / (data_with_partitioning[,'leaf_mass'] + data_with_partitioning[,'stem_mass']) * data_with_partitioning[,'yield_Mg_ha']
data_with_partitioning[,'stem_yield'] <- data_with_partitioning[,'stem_mass'] / (data_with_partitioning[,'leaf_mass'] + data_with_partitioning[,'stem_mass']) * data_with_partitioning[,'yield_Mg_ha']

data_with_partitioning <- data_with_partitioning %>% filter(leaf_yield!=Inf & !is.na(leaf_yield))
data_with_partitioning[['first_row']] <- data_with_partitioning[['first_row.x']]
data_with_partitioning <- subset(data_with_partitioning,select = -c(first_row.x,first_row.y))


#calculate relevant data for each year

yrLeafStem <- function(Year){
  return(data_with_partitioning %>% filter(year == Year))
}

yrMeanVarLeafStem <- function(Year){
  return(yrLeafStem(Year) %>%
    group_by(doy) %>%
    summarize(ActualMeanLeaf = mean(leaf_yield), VarianceLeaf = var(leaf_yield), ActualMeanStem = mean(stem_yield), VarianceStem = var(stem_yield)))
}

yrLeafArea <- function(Year){
  return(all_lai %>% filter(year == Year))
}

yrMeanVarLeafArea <- function(Year){
  return(yrLeafArea(Year) %>%
           group_by(doy) %>%
           summarize(ActualMeanLeafAreaIndex = mean(LAI), VarianceLAI = var(LAI)))
}

yrYield <- function(Year){
  return(all_yield %>% filter(year == Year))
}

yrMeanVarYield <- function(Year){
  return(yrYield(Year) %>%
           group_by(doy) %>%
           summarize(ActualYield = mean(yield_Mg_ha), VarianceYield = var(yield_Mg_ha)))
}

yrFinalYield <- function(Year){
  return(final_yield_only %>% filter(year == Year))
}

yrMeanVarFinalYield <- function(Year){
  return(yrFinalYield(Year) %>%
           group_by(doy) %>%
           summarize(ActualYield = mean(yield_Mg_ha), VarianceYield = var(yield_Mg_ha)))
}

#precalculate some stuff about the actual and model data to make Statistics functions easier
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
  
  #the following are means for the entire data set, to be used for calculating R-squared
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
  
  #the following are means for the entire data set, to be used for calculating R-squared
  actual_mean_lai <- as.numeric(actualData %>% summarize(mean = mean(LAI)))
  innerJoinDf <- mutate(innerJoinDf,totSquareToSum = (LAI - actual_mean_lai)^2)
  return(innerJoinDf)
}

#calculate RMSE, MAE, MAPE, normed Mahalanobis for just end of year yield
Statistics0 <- function(model_result,year){
  actual_yield <- yrFinalYield(year)
  mean_var_yield <- yrMeanVarFinalYield(year)
  innerJoinDf <- df_inner_join_yield(actual_yield,mean_var_yield,model_result)
  
  #find sums of a few columns for stats calculations
  SumOfDiffsSquared = sum(innerJoinDf[,'diffSquares'])
  SumOfAbsDiffs = sum(innerJoinDf[,'actualMinusModelAbs'])
  SumOfAbsPercs = sum(innerJoinDf[,'absPercentError'])
  
  #number of rows
  n = nrow(innerJoinDf)
  
  #stats calculations
  RMSE = sqrt(SumOfDiffsSquared/n)
  MAE = SumOfAbsDiffs/n
  MAPE = SumOfAbsPercs/n
  ChiSquare = sum(innerJoinDf[,'chiSquareToSum'])
  ChiSquareNormed = ChiSquare/n
  TotSquare = sum(innerJoinDf[,'totSquareToSum'])
  RSquared = 1- (SumOfDiffsSquared/TotSquare)
  
  #normed Mahalanobis calculations
  Count = innerJoinDf %>% count(doy)
  Count <- mutate(Count,recip_sqrt = 1/sqrt(n))
  factor_to_norm <- 1/sum(Count[,'recip_sqrt'])
  Mahal <- innerJoinDf %>%
    group_by(doy) %>%
    summarize(MahalYieldSum = sqrt(sum(chiSquareToSum))/n)
  MahalFinal <- mutate(full_join(Count,Mahal,by=c('doy')),MahalYieldNormed = MahalYieldSum/n)
  Mahalanobis = sum(MahalFinal[,'MahalYieldNormed']) * factor_to_norm
  stats = data.frame(RSquared = c(RSquared), RMSE = c(RMSE), MAE = c(MAE), MAPE = c(MAPE), MahalToSum = c(ChiSquare), MahalToSum_normed = c(ChiSquareNormed), Mahalanobis_normed = c(Mahalanobis))
  
  return(stats)
}

#calculate RMSE, MAE, MAPE, normed Mahalanobis for LAI, Leaf, Stem, and yield
Statistics1 <- function(model_result,year){
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
  
  #Mahalanobis calculations
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

#wrapper functions to calculate statistics for actual data vs the BioCro model

#for all data
#different scales of LAI and Yield may affect RMSE and MAE in weird ways
StatisticsLeafAndYield <- function(x,sorghum_partial,year){
  result = sorghum_partial(year)(x) #this runs the model on the current parameters
  return(Statistics1(result,year))
}

#this is for final yield only
StatisticsFinalYield <- function(x,sorghum_partial,year){
  result = sorghum_partial(year)(x) #this runs the model on the current parameters
  return(Statistics0(result,year))
}

#optimizes fitting all data (lai, Leaf, Stem, final yield) or end of year yield only
new_parameters_logistic <- function(params_to_change,lower_bounds,upper_bounds,current_params,year,st,final_only=FALSE){
  sorghum_partial <- function(year){
    
    return(partial_run_biocro(init_vals_logistic,current_params,
                              yrClimate(year),
                              sorghum_steady_state_modules_logistic,sorghum_derivative_modules_logistic,sorghum_integrator,params_to_change))
  }
  #st is stat: RMSE, MAE, MAPE, or ChiSquare
  sorghum_stat <- function(x,stat=st){
    if(final_only){
      return(as.numeric(StatisticsFinalYield(x,sorghum_partial,year)[,c(stat)]))
    }
    else{
      return(as.numeric(StatisticsLeafAndYield(x,sorghum_partial,year)[,c(stat)]))
    }
  }
  para <- as.numeric(unname(current_params[params_to_change]))
  ans <- hjkb(para,sorghum_stat,lower=lower_bounds,upper = upper_bounds)
  new_params <- ans$par
  names(new_params) <- params_to_change
  altered_params <- current_params
  for(p in params_to_change){
    altered_params[[p]] <- new_params[[p]]
  }
  return(altered_params)
}

#function returns a graph of lai for the model data and the actual data
getGraphLeafArea <- function(model_result,actual_yield,sub="") {
  fullJoinDf <- full_join(model_result,actual_yield,by = c("doy")) #joins the two dataframes so we can graph on the same axes
  #stats_df = Statistics1(model_result,actual_yield)
  #stats_str = ""
  #for(stat in colnames(stats_df)){
  #  stats_str=paste0(stats_str,stat,": ",round(stats_df[1,stat],digits=2),"  ")
  #}
  
  return(ggplot(fullJoinDf,aes(time)) + #time as x-axis
           geom_point(aes(y = (lai), color = "BioCro Model")) + #model data
           geom_point(aes(y = LAI, color = "Actual LAI")) + #actual data
           xlab("Day of Year") + #x-axis label
           ylab("Leaf Area Index (unitless)") + #y-axis label
           labs(title = "Sorghum Model Leaf Area Index vs. Actual Data", #graph title
                subtitle = sub) +  #subtitle
                #caption = stats_str) + #put stats in caption
           scale_color_manual(name = "",values = c("red", "blue")) + #model in blue, actual in red
           theme(legend.position = "bottom")) #put legend at bottom
}

#function returns a graph of the model data with the actual data
getGraphYield <- function(model_result,actual_yield,sub="") {
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
           subtitle = sub) + #subtitle
           scale_color_manual(name = "Data Source",values = c("red", "blue")) + #model in blue, actual in red
           theme(legend.position = "bottom")) #put legend at bottom
}
  
#4 graphs side by side
getGraphAll <- function(year,model_result,subleaf="",subyield="") {
  actual_leaf_stem <- yrLeafStem(year)
  fullJoinDf <- full_join(model_result,actual_leaf_stem,by = c("doy")) #joins the two dataframes so we can graph on the same axes
  stats_df = Statistics1(model_result,year)
  stats_str = "Statistics for Leaf Area, Leaf, Stem (combined) "
  for(stat in colnames(stats_df)){
    stats_str=paste0(stats_str,stat,": ",round(stats_df[1,stat],digits=2),"  ")
  }
  
  print(stats_str)
  
  actual_leaf_area <- yrLeafArea(year)
  actual_yield <- yrYield(year)
  
  LeafArea = getGraphLeafArea(model_result,actual_leaf_area,sub=subleaf)
  
  Yield = getGraphYield(model_result,actual_yield,sub=subyield)
  
  Leaf = ggplot(fullJoinDf,aes(time)) + #time as x-axis
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
  
  return(LeafArea + Yield + Leaf + Stem)
}


setwd("C://Users/stark/OneDrive/Documents2021/biocro-dev/with_partitioning_logistic/Mahalanobis_normed")

#runs hjkb and prints graphs
run_and_print_graphs <- function(to_minimize,init_val,old_params,year_of_testset,params_to_change,lower,upper){
  
  new_params_final <- new_parameters_logistic(params_to_change,lower,upper,
                                     old_params,year_of_testset,to_minimize)
  
  for(yr in 2016:2019){
    old_model <- run_biocro(
      init_vals_logistic,
      old_params,
      yrClimate(yr),
      sorghum_steady_state_modules_logistic,
      sorghum_derivative_modules_logistic
    )
    new_model <- run_biocro(
      init_vals_logistic,
      new_params_final,
      yrClimate(yr),
      sorghum_steady_state_modules_logistic,
      sorghum_derivative_modules_logistic
    )
    #make a pdf file graph
    pdf(
      file = paste0(init_val,"_preAndPostParam",yr,"_fit_partitioningLogistic",year_of_testset,"_minimize",to_minimize,".pdf"),
      width = 18,          # inches
      height = 15,         # inches
      useDingbats = FALSE # make sure symbols are rendered properly in the PDF
    )
    pre <- getGraphAll(yr,old_model,subleaf=paste(yr,"default parameters"),
                       subyield=paste(yr,"default parameters"))

    post <- getGraphAll(yr,new_model,subleaf=paste(yr,"parameters fitted to",to_minimize),
                        subyield=paste(yr,"parameters fitted to",to_minimize))
    
    print(pre / post)
    
    dev.off()
  }
  
  #store the list of parameters for future reference
  write.table(old_params,file=paste0(init_val,"_Params.txt"))
  write.table(new_params_final,file=paste0(init_val,"_Params_fitted_to_logistic_",year_of_testset,"_minimizing_",to_minimize,".txt"))
  
  return(new_params_final)
}

#just print the graphs, if it's not needed to run hjkb first
print_graphs <- function(to_minimize,init_val,old_params,new_params,year_of_testset){
  for(yr in 2016:2019){
    old_model <- run_biocro(
      init_vals_logistic,
      old_params,
      yrClimate(yr),
      sorghum_steady_state_modules_logistic,
      sorghum_derivative_modules_logistic
    )
    new_model <- run_biocro(
      init_vals_logistic,
      new_params,
      yrClimate(yr),
      sorghum_steady_state_modules_logistic,
      sorghum_derivative_modules_logistic
    )
    #make a pdf file graph
    pdf(
      file = paste0(init_val,"_preAndPostParam",yr,"_fit_partitioningLogistic_minimize_",year_of_testset,"_",to_minimize,".pdf"),
      width = 15,          # inches
      height = 15,         # inches
      useDingbats = FALSE # make sure symbols are rendered properly in the PDF
    )
    pre <- getGraphAll(yr,old_model,subleaf=paste(yr,"default parameters"),
                       subyield=paste(yr,"default parameters"))
    
    post <- getGraphAll(yr,new_model,subleaf=paste(yr,"parameters fitted to",to_minimize),
                        subyield=paste(yr,"parameters fitted to",to_minimize))
    
    print(pre / post)
    
    dev.off()
  }
  
  #store the list of parameters for future reference
  write.table(old_params,file=paste0(init_val,"_Params.txt"))
  write.table(new_params,file=paste0(init_val,"_Params_fitted_to_",year_of_testset,"_minimizing_",to_minimize,".txt"))
}


#these lines of code will run and print the graphs
#the bounds currently need to be changed in 2 places, because R is clunky, but there may be a better way to code this
params_and_bounds <- data.frame("params" = c("iSp","Sp_thermal_time_decay","alphaRoot","alphaLeaf","alphaStem",
                                            "betaRoot","betaLeaf","betaStem",
                                            "TTemr","TTveg","TTrep"),
                                "lower" = c(0.3,-5e-4,-10,-10,-10,
                                            -150,-150,-150,
                                            10,300,301),
                                "upper" = c(200,5e-4,
                                            150,150,150,
                                            10,10,10,
                                            10000,10001,10002))

#I keep track of the iteration number of hjkb (see below for what I do for future iterations), and the starting iSp value (as a double check)
new_params <- run_and_print_graphs("Mahalanobis_normed","iter1_iSp3",sorghum_parameters_logistic,2017,params_and_bounds[,'params'],c(0.3,-5e-4,-10,-10,-10,
                                                                                                    -150,-150,-150,
                                                                                                    10,300,301),c(200,5e-4,
                                                                                                                  150,150,150,
                                                                                                                  10,10,10,
                                                                                                                  10000,10001,10002))


#for sorghum - sometimes TTrep comes out too low and needs to be changed to higher, since energy sorghum doesn't flower
#new_params$TTrep <- 5000

#these lines of code are to import a previously generated file of parameters and get them into the right format for BioCro to run
#new_params_final_temp <- BEST_ONE_iter1_iSp3_Params_fitted_to_logistic_2018_minimizing_Mahalanobis_normed
#temp_parameters <- new_params_final_temp[2,]
#names(temp_parameters) <- new_params_final_temp[1,]

#hjkb often needs to be run for a few iterations, to help avoid local optima
#usually, for parameter value x that had value x_fitted after fitting, I will re-run hjkb with the starting value now assigned to x_fitted + (x_fitted - x)
#of course, there are many other methods to do this
#if you have a very fast computer and/or can let it run for awhile (probably a couple weeks), you could make a lattice of possible starting values and start at each one
temp_parameters <- new_params
temp_parameters$alphaLeaf <- 57.2
temp_parameters$alphaRoot <- -10
temp_parameters$alphaStem <- 7
temp_parameters$betaLeaf <- -64.2
temp_parameters$betaRoot <- -100
temp_parameters$betaStem <- 10
temp_parameters$Sp_thermal_time_decay <- 0.00049
temp_parameters$iSp <- 2
temp_parameters$TTemr <- 70.6
temp_parameters$TTveg <- 1171
temp_parameters$TTrep <- 5000