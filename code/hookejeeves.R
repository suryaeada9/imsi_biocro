library(dfoptim)
library(pryr)
library(dplyr)

library(BioCro)

library(ggplot2)

library(patchwork)

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
    tp2                         10000
    tp3                         10001
    tp4                         10002
    tp5                         10003
    upperT                      37.5
    vmax1                       39
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
    iSp                         1.7
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
    Sp_thermal_time_decay       0
    stefan_boltzman             5.67e-8
    tbase                       0
    topt_lower                  21
    topt_upper                  34
    tmax                        43
    theta                       0.83
    timestep                    1
    TTemr                       50
    TTveg                       700
    TTrep                       700
    upperT                      37.5
    vmax1                       39
    vmax_n_intercept            0
    water_stress_approach       1"
  
  data_frame = utils::read.table(textConnection(datalines), header=TRUE)
  values = as.list(data_frame$value)
  names(values) = data_frame$symbol
  values
})

init_vals_logistic <- sorghum_initial_values
init_vals_logistic[['DVI']] <- -1 #need DVI for initial values when running logistic model

#climate data from Justin
climate = read.delim(file.path("C://Users/stark/OneDrive/Documents2021/biocro-dev", 'filled_climate.tsv'))
#climate2016 = subset(climate, year == 2016 & doy > 167)  # According the these notes dated June 16, sorghum at SoyFACE had emerged, but not at the Energy Farm. It's a decent estimate.
#climate2017 = subset(climate, year == 2017 & doy > 151)

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

climate_2016 <- get_climate(2016,167)
climate_2017 <- get_climate(2017,151)

#returns the climate for a given year
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

#biomass and leaf area files must be inputted via "Import Dataset"
#this calculates doy in order to join to the model result data
biomass.yield = within(biomass.yield, {
  date = lubridate::yday(as.Date(date))
})

biomass.partitioning = within(biomass.partitioning, {
  date = lubridate::yday(as.Date(date))
})

leaf.area = within(leaf.area, {
  date = lubridate::mdy(date)
})

leaf.area = within(leaf.area, {
  date = lubridate::yday(as.Date(date))
})

#calculate yield from mass
biomass.yield[,"yield"] = biomass.yield[,'above_ground_mass'] * 10 / (biomass.yield[,'row_length'] * biomass.yield[,'row_spacing'])

#all 2016 data was from EF
actual_yield_2016 <- biomass.yield %>%
  filter(site_id == "EF") %>%
  group_by(date) %>%
  summarize(ActualMeanYield = mean(yield))

#all 2017 data was from not EF (MW and EF_check)
actual_yield_2017 <- biomass.yield %>%
  filter(site_id != "EF") %>%
  group_by(date) %>%
  summarize(ActualMeanYield = mean(yield))

#compare Pacesetter genotypes, because all 2016 (training) data is on Pacesetter
actual_yield_pacesetter_2017 <- biomass.yield %>%
  filter(site_id != "EF", grepl("Pacesetter",genotype_name)) %>%
  group_by(date) %>%
  summarize(ActualMeanYield = mean(yield))

data_with_partitioning <- inner_join(biomass.yield,biomass.partitioning,by=c('site_id','date','range'))

data_with_partitioning[,'leaf_yield'] <- data_with_partitioning[,'leaf_mass'] / (data_with_partitioning[,'leaf_mass'] + data_with_partitioning[,'stem_mass']) * data_with_partitioning[,'yield']
data_with_partitioning[,'stem_yield'] <- data_with_partitioning[,'stem_mass'] / (data_with_partitioning[,'leaf_mass'] + data_with_partitioning[,'stem_mass']) * data_with_partitioning[,'yield']
data_with_partitioning <- data_with_partitioning %>% slice(-c(68))
View(data_with_partitioning)

#all 2016 data was from EF
actual_leaf_stem_2016 <- data_with_partitioning %>%
  filter(site_id == "EF") %>%
  group_by(date) %>%
  summarize(ActualMeanLeaf = mean(leaf_yield), ActualMeanStem = mean(stem_yield))

#all 2017 data was from not EF (MW and EF_check)
actual_leaf_stem_2017 <- data_with_partitioning %>%
  filter(site_id != "EF") %>%
  group_by(date) %>%
  summarize(ActualMeanLeaf = mean(leaf_yield), ActualMeanStem = mean(stem_yield))

#compare Pacesetter genotypes, because all 2016 (training) data is on Pacesetter
actual_leaf_stem_pacesetter_2017 <- data_with_partitioning %>%
  filter(site_id != "EF", grepl("Pacesetter",genotype_name)) %>%
  group_by(date) %>%
  summarize(ActualMeanLeaf = mean(leaf_yield), ActualMeanStem = mean(stem_yield))

data_with_leaf <- inner_join(biomass.yield,leaf.area,by = c('site_id','date','range'))

#calculate leaf area index from leaf area
data_with_leaf[['lai']] <- data_with_leaf[['leaf_area']] / (data_with_leaf[['row_length']] * data_with_leaf[['row_spacing']])

actual_leaf_area_2016 <- data_with_leaf %>%
  filter(site_id == "EF") %>%
  group_by(date) %>%
  summarize(ActualMeanLeafAreaIndex = mean(lai))

actual_leaf_area_2017 <- data_with_leaf %>%
  filter(site_id != "EF") %>%
  group_by(date) %>%
  summarize(ActualMeanLeafAreaIndex = mean(lai))

actual_leaf_area_pacesetter_2017 <- data_with_leaf %>%
  filter(site_id != "EF",grepl("Pacesetter",genotype_name)) %>%
  group_by(date) %>%
  summarize(ActualMeanLeafAreaIndex = mean(lai))

#get rid of NA values - this could be coded better (these are hard coded because I looked at where they were)
actual_leaf_area_2017 <- actual_leaf_area_2017 %>% slice(-c(5))
actual_leaf_area_pacesetter_2017 <- actual_leaf_area_pacesetter_2017 %>% slice(-c(3))

#these help return the right dataset depending on an identifying string
datasets <- data.frame("Str" = c("2016EF","2017All","2017Pacesetter"),
                       "Str_with_spaces" = c("EF 2016","2017 all genotypes","2017 Pacesetter"),
                       "Year" = c(2016,2017,2017))

yrLeafStem <- function(dataset_str){
  if(dataset_str=="2016EF")
  {
    return(actual_leaf_stem_2016)
  }
  else if(dataset_str=="2017All")
  {
    return(actual_leaf_stem_2017)
  }
  else
  {
    return(actual_leaf_stem_pacesetter_2017)
  }
}

yrLeafArea <- function(dataset_str){
  if(dataset_str=="2016EF")
  {
    return(actual_leaf_area_2016)
  }
  else if(dataset_str=="2017All")
  {
    return(actual_leaf_area_2017)
  }
  else
  {
    return(actual_leaf_area_pacesetter_2017)
  }
}

#precalculate some stuff about the actual and model data to make Statistics functions easier
df_inner_join <- function(actualData,modelResult){
  byDay <- modelResult %>% 
    group_by(doy) %>%
    summarize(meanLeaf = mean(Leaf), meanStem = mean(Stem)) #makes a new dataframe that gives the average yield per day (over all 24 values in the model_result dataframe)
  #Leaf + Stem is specific to sorghum and needs to be changed for other crops
  innerJoinDf <- inner_join(byDay,actualData,by = c("doy" = "date")) #joins the two dataframes so we can compare values and run stats
  innerJoinDf <- mutate(innerJoinDf,actualMinusModelLeaf = ActualMeanLeaf - meanLeaf) #makes a column for actual data minus model data
  innerJoinDf <- mutate(innerJoinDf,actualMinusModelStem = ActualMeanStem - meanStem) #makes a column for actual data minus model data
  innerJoinDf <- mutate(innerJoinDf,actualMinusModelLeafAbs = abs(actualMinusModelLeaf)) #makes a column for absolute value of difference
  innerJoinDf <- mutate(innerJoinDf,actualMinusModelStemAbs = abs(actualMinusModelStem)) #makes a column for absolute value of difference
  innerJoinDf <- mutate(innerJoinDf,diffSquaresLeaf = actualMinusModelLeaf * actualMinusModelLeaf) #makes a column for the square of the difference
  innerJoinDf <- mutate(innerJoinDf,diffSquaresStem = actualMinusModelStem * actualMinusModelStem) #makes a column for the square of the difference
  innerJoinDf <- mutate(innerJoinDf,absPercentErrorLeaf = actualMinusModelLeafAbs/ActualMeanLeaf * 100) #makes a column for absolute value of percentage difference
  innerJoinDf <- mutate(innerJoinDf,absPercentErrorStem = actualMinusModelStemAbs/ActualMeanStem * 100) #makes a column for absolute value of percentage difference
  innerJoinDf <- mutate(innerJoinDf,chiSquareToSumLeaf = diffSquaresLeaf/meanLeaf) #makes a column for square of difference over actual value (for chi square calculation)
  innerJoinDf <- mutate(innerJoinDf,chiSquareToSumStem = diffSquaresLeaf/meanStem) #makes a column for square of difference over actual value (for chi square calculation)
  return(innerJoinDf)
}

df_inner_join_leaf_area <- function(actualData,modelResult){
  byDay <- modelResult %>% 
    group_by(doy) %>%
    summarize(leafAreaIndex = mean(lai)) #makes a new dataframe that gives the average lai per day (over all 24 values in the model_result dataframe)
  innerJoinDf <- inner_join(byDay,actualData,by = c("doy" = "date")) #joins the two dataframes so we can compare values and run stats
  innerJoinDf <- mutate(innerJoinDf,actualMinusModel = ActualMeanLeafAreaIndex - leafAreaIndex) #makes a column for actual data minus model data
  innerJoinDf <- mutate(innerJoinDf,actualMinusModelAbs = abs(actualMinusModel)) #makes a column for absolute value of difference
  innerJoinDf <- mutate(innerJoinDf,diffSquares = actualMinusModel * actualMinusModel) #makes a column for the square of the difference
  innerJoinDf <- mutate(innerJoinDf,absPercentError = actualMinusModelAbs/ActualMeanLeafAreaIndex * 100) #makes a column for absolute value of percentage difference
  innerJoinDf <- mutate(innerJoinDf,chiSquareToSum = diffSquares/ActualMeanLeafAreaIndex) #makes a column for square of difference over actual value (for chi square calculation)
  return(innerJoinDf)
}

#We're not using this right now. It looks at horizontal differences in TTc. No good statistical motivation.
HorizontalStatistics <- function(x,sorghum_partial,year,dataset_str){
  result <- sorghum_partial(year)(x)
  return(Statistics3(result,yrLeafStem(dataset_str),yrLeafArea(dataset_str)))
}

#calculate RMSE, MAE, MAPE, and Chi Square for actual data vs the BioCro model
#currently training on 2016 EF data
#won't be accurate for RMSE and MAE, due to different scales of Leaf Area and Yield, but should be fine for MAPE and ChiSquare
StatisticsLeafAndYield <- function(x,sorghum_partial,year,dataset_str){
  result = sorghum_partial(year)(x) #this runs the model on the current parameters
  return(Statistics1(result,yrLeafStem(dataset_str),yrLeafArea(dataset_str)))
}

#normalize kLeaf1, kStem1, kRoot1, kGrain1, etc.
#necessary for the partitioning selector model
normalize_kVals <- function(current_params){
  kVals <- data.frame(Leaf = as.numeric(c(current_params$kLeaf1,current_params$kLeaf2,current_params$kLeaf3,current_params$kLeaf4,current_params$kLeaf5,current_params$kLeaf6)),
                      Stem = as.numeric(c(current_params$kStem1,current_params$kStem2,current_params$kStem3,current_params$kStem4,current_params$kStem5,current_params$kStem6)),
                      Root = as.numeric(c(current_params$kRoot1,current_params$kRoot2,current_params$kRoot3,current_params$kRoot4,current_params$kRoot5,current_params$kRoot6)),
                      Grain = as.numeric(c(current_params$kGrain1,current_params$kGrain2,current_params$kGrain3,current_params$kGrain4,current_params$kGrain5,current_params$kGrain6)))
                      
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

#this is partial_run_biocro with one line of code added that normalizes the k values before running the model
#haven't changed this to Justin's suggestion yet
partial_run_biocro_with_normalize <- function(
  initial_values = list(),
  parameters = list(),
  drivers,
  steady_state_module_names = list(),
  derivative_module_names = list(),
  integrator = default_integrator,
  arg_names,
  verbose = FALSE
)
{
  # Accepts the same parameters as run_biocro() with an additional
  # 'arg_names' parameter, which is a vector of character variables.
  #
  # Returns a function that runs run_biocro() with all of the
  # parameters (except those in 'arg_names') set as their default values. The
  # only parameter in the new function is a numerical vector specifying the
  # values of the quantities in 'arg_names'. This technique is called "partial
  # application," hence the name partial_run_biocro.
  #
  # initial_values: same as run_biocro()
  # parameters: same as run_biocro()
  # drivers: same as run_biocro()
  # steady_state_module_names: same as run_biocro()
  # derivative_module_names: same as run_biocro()
  # integrator: same as run_biocro()
  # arg_names: vector of character variables. The names of the arguments that
  #            the new function accepts. Note: 'arg_names' can only contain
  #            the names of parameters in 'initial_values', 'parameters', or
  #            'drivers'.
  # verbose: same as run_biocro()
  #
  # returns f(arg).
  #
  # Example: varying the TTc values at which senescence starts for a sorghum
  # simulation; here we set them all to 2000 degrees C * day instead of the
  # default sorghum values.
  #
  #     senescence_simulation <- partial_run_biocro(
  #         sorghum_initial_values,
  #         sorghum_parameters,
  #         get_growing_season_climate(weather05),
  #         sorghum_steady_state_modules,
  #         sorghum_derivative_modules,
  #         sorghum_integrator,
  #         c('seneLeaf', 'seneStem', 'seneRoot', 'seneRhizome'),
  #         TRUE
  #     )
  #
  #     result = senescence_simulation(c(2000, 2000, 2000, 2000))
  
  arg_list = list(
    initial_values=initial_values,
    parameters=parameters,
    drivers=drivers,
    steady_state_module_names=steady_state_module_names,
    derivative_module_names=derivative_module_names,
    integrator=integrator,
    verbose=verbose
  )
  
  df = data.frame(
    control=character(),
    arg_name=character(),
    stringsAsFactors=FALSE
  )
  
  for (i in seq_along(arg_list)) {
    if (length(names(arg_list[[i]])) > 0) {
      df = rbind(
        df,
        data.frame(
          control = names(arg_list)[i],
          arg_name=names(arg_list[[i]]),
          stringsAsFactors=FALSE
        )
      )
    }
  }
  
  # Find the locations of the parameters specified in arg_names and check for
  # errors
  controls = df[match(arg_names, df$arg_name), ]
  if (any(is.na(controls))) {
    missing = arg_names[which(is.na(controls$control))]
    stop(paste('The following arguments in "arg_names" are not in any of the paramter lists:', paste(missing, collapse=', ')))
  }
  
  # Make a function that calls run_biocro with new values for the
  # parameters specified in arg_names
  function(x)
  {
    if (length(x) != length(arg_names)) {
      stop("The length of x does not match the length of arguments when this function was defined.")
    }
    x = unlist(x)
    temp_arg_list = arg_list
    for (i in seq_along(arg_names)) {
      c_row = controls[i, ]
      temp_arg_list[[c_row$control]][[c_row$arg_name]] = x[i]
    }
    
    temp_arg_list[['parameters']] <- normalize_kVals(temp_arg_list[['parameters']])
    do.call(run_biocro, temp_arg_list)
  }
}

#used this function to see if we could theoretically reach the large value around day 270 - turns out we could
new_parameters_max_yield <- function(params_to_change,lower_bounds,upper_bounds,current_params){
  sorghum_partial <- function(year){
    return(partial_run_biocro(init_vals_logistic,current_params,
                                        yrClimate(year),
                                        sorghum_steady_state_modules_logistic,sorghum_derivative_modules_logistic,sorghum_integrator,params_to_change))
  }
  sorghum_270 <- function(x){
    result = sorghum_partial(2016)(x)
    avg270 <- result %>% filter(doy==270) %>% summarize(meanYield = mean(Leaf) + mean(Stem))
    return(-avg270[1,1]) #minimizing negative the value at day 270 will maximize the value at day 270
  }
  para <- as.numeric(unname(current_params[params_to_change]))
  ans <- hjkb(para,sorghum_270,lower=lower_bounds,upper=upper_bounds)
  new_params <- ans$par
  names(new_params) <- params_to_change
  altered_params <- current_params
  for(p in params_to_change){
    altered_params[[p]] <- new_params[[p]]
  }
  return(altered_params)
}

#optimizes lai and yield fit simultaneously
new_parameters <- function(params_to_change,lower_bounds,upper_bounds,current_params,stat_to_minimize,year,dataset_str){
  sorghum_partial <- function(year){
    
    return(partial_run_biocro_with_normalize(sorghum_initial_values,current_params,
                              yrClimate(year),
                              sorghum_steady_state_modules,sorghum_derivative_modules,sorghum_integrator,params_to_change))
  }
  #st is stat: RMSE, MAE, MAPE, or ChiSquare
  sorghum_stat <- function(x,st=stat_to_minimize){
    return(StatisticsLeafAndYield(x,sorghum_partial,year,dataset_str)[,c(st)])
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

#optimizes lai and yield fit simultaneously
new_parameters_logistic <- function(params_to_change,lower_bounds,upper_bounds,current_params,stat_to_minimize,year,dataset_str){
  sorghum_partial <- function(year){
    
    return(partial_run_biocro(init_vals_logistic,current_params,
                                             yrClimate(year),
                                             sorghum_steady_state_modules_logistic,sorghum_derivative_modules_logistic,sorghum_integrator,params_to_change))
  }
  #st is stat: RMSE, MAE, MAPE, or ChiSquare
  sorghum_stat <- function(x,st=stat_to_minimize){
    return(StatisticsLeafAndYield(x,sorghum_partial,year,dataset_str)[,c(st)])
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

#easily adjust starting values without affecting "sorghum_parameters"
sorg_parameters <- sorghum_parameters
sorg_parameters$iSp <- 2.3
sorg_parameters$kLeaf1 <- 0.3
sorg_parameters$kStem1 <- 0.44
sorg_parameters$kRoot1 <- 0.26
sorg_parameters$kLeaf2 <- 0.849
sorg_parameters$kStem2 <- 0.001
sorg_parameters$kRoot2 <- 0.15
sorg_parameters$kLeaf3 <- 0.23
sorg_parameters$kStem3 <- 0.76
sorg_parameters$kRoot3 <- 0.01
sorg_parameters$kLeaf4 <- 0.49
sorg_parameters$kStem4 <- 0.5
sorg_parameters$kRoot4 <- 0.01
sorg_parameters$kLeaf5 <- 0.96
sorg_parameters$kStem5 <- 0.01
sorg_parameters$kRoot5 <- 0.03
sorg_parameters$kLeaf6 <- 0.49
sorg_parameters$kStem6 <- 0.5
sorg_parameters$kRoot6 <- 0.01
sorg_parameters$tp1 <- 562
sorg_parameters$tp2 <- 1312
sorg_parameters$tp3 <- 2063
sorg_parameters$tp4 <- 2676
sorg_parameters$tp5 <- 3211
sorg_parameters$Sp_thermal_time_decay <- 0

#the lines below will run hjkb once

#use these lines when using selector model
new_params_final <- new_parameters(c("iSp","Sp_thermal_time_decay","tp1",'tp2','tp3',"tp4",'tp5',
                                     "kLeaf1",'kLeaf2','kLeaf3',"kLeaf4",'kLeaf5','kLeaf6',
                                     "kStem1",'kStem2','kStem3',"kStem4",'kStem5','kStem6',
                                     "kRoot1",'kRoot2','kRoot3',"kRoot4",'kRoot5','kRoot6'
                                     ),
                                   c(0.3,-9e-4,20,300,300,300,300,
                                     1e-8,1e-8,1e-8,1e-8,1e-8,1e-8,
                                     1e-8,1e-8,1e-8,1e-8,1e-8,1e-8,
                                     1e-8,1e-8,1e-8,1e-8,1e-8,1e-8
                                     ),
                                   c(200,9e-4,10000,10000,10000,10000,10000,
                                     1,1,1,1,1,1,
                                     1,1,1,1,1,1,
                                     1,1,1,1,1,1
                                     ),
                                   sorghum_parameters,
                                   "MAE",2016,"EF2016")

new_params_final <- normalize_kVals(new_params_final)

View(new_params_final)

#use these lines when using logistic model; need to change the modules above also (should eventually take module lists as inputs in new_parameters)
new_params_final <- new_parameters_logistic(c("iSp","Sp_thermal_time_decay","alphaRoot","alphaStem","alphaLeaf",
                                     "betaRoot","betaStem","betaLeaf",
                                     "TTemr","TTveg","TTrep"),
                                   c(0.3,-1e-2,0,0,0,
                                     -50,-50,-50,
                                     20,300,300),
                                   c(200,1e-2,50,50,50,
                                     0,0,0,
                                     10000,10000,10000),
                                   sorghum_parameters_logistic,
                                   "MAE",2016,"EF2016")
View(new_params_final)

#calculate RMSE, MAE, MAPE, and Chi Square for LAI, Leaf, and Stem
Statistics1 <- function(model_result,actual_leaf_stem,actual_leaf_data){
  innerJoinDf1 <- df_inner_join(actual_leaf_stem,model_result)
  innerJoinDf2 <- df_inner_join_leaf_area(actual_leaf_data,model_result)

  #find sums of a few columns for stats calculations
  SumOfDiffsSquared = sum(innerJoinDf1[,'diffSquaresLeaf']) + sum(innerJoinDf1[,'diffSquaresStem']) + sum(innerJoinDf2[,'diffSquares'])
  SumOfAbsDiffs = sum(innerJoinDf1[,'actualMinusModelLeafAbs']) + sum(innerJoinDf1[,'actualMinusModelStemAbs']) + sum(innerJoinDf2[,'actualMinusModelAbs'])
  SumOfAbsPercs = sum(innerJoinDf1[,'absPercentErrorLeaf']) + sum(innerJoinDf1[,'absPercentErrorLeaf']) + sum(innerJoinDf2[,'absPercentError'])
  
  #number of rows
  n = nrow(innerJoinDf1) + nrow(innerJoinDf2)
  
  #stats calculations
  RMSE = sqrt(SumOfDiffsSquared/n)
  MAE = SumOfAbsDiffs/n
  MAPE = SumOfAbsPercs/n
  ChiSquare = sum(innerJoinDf1[,'chiSquareToSumLeaf']) + sum(innerJoinDf1[,'chiSquareToSumStem']) + sum(innerJoinDf2[,'chiSquareToSum'])
  
  stats = data.frame(RMSE = c(RMSE), MAE = c(MAE), MAPE = c(MAPE), ChiSquare = c(ChiSquare))
  
  return(stats)
}

#Looks at difference in TTc values
#Does a decent job at modeling, but there's no statistical motivation for it
Statistics2 <- function(model_result,actual_yield,actual_leaf_data){
  byDay <- model_result %>% 
    group_by(doy) %>%
    summarize(meanYield = mean(Leaf) + mean(Stem),leafAreaIndex=mean(lai),meanTTc = mean(TTc))
  first_doy <- as.numeric(unname(byDay[1,1]))
  last_doy <- as.numeric(unname(byDay[nrow(byDay),1]))
  absdiffs <- vector()
  sqdiffs <- vector()
  percdiffs <- vector()
  chisq <- vector()
  for(i in 1:nrow(actual_leaf_data)){
    actual_lai <- as.numeric(unname(actual_leaf_data[i,2]))
    actual_doy <- as.numeric(unname(actual_leaf_data[i,1]))
    before <- FALSE
    after <- FALSE
    row <- 1
    for(j in first_doy:actual_doy){
      if(as.numeric(unname(byDay[row,'leafAreaIndex']))>actual_lai){
        before_doy <- j
        before <- TRUE
        break
      }
      row <- row + 1
    }
    
    start <- actual_doy + 1
    row <- actual_doy - first_doy + 2
    if(!before){
      for(j in start:last_doy){
        if(as.numeric(unname(byDay[row,'leafAreaIndex']))>actual_lai){
          after_doy <- j
          after <- TRUE
          break
        }
        row <- row + 1
      }
    }
    else{
      for(j in start:last_doy){
        if(as.numeric(unname(byDay[row,'leafAreaIndex']))<actual_lai){
          after_doy <- j
          after <- TRUE
          break
        }
        row <- row + 1
      }
    }
    
    actual_row <- actual_doy - first_doy + 1
    actual_TTc <- as.numeric(unname(byDay[actual_row,'meanTTc']))
    if(!before & !after){
      stat <- actual_TTc
    }
    else if(before & !after){
      before_row <- before_doy - first_doy + 1
      stat <- abs(actual_TTc - as.numeric(unname(byDay[before_row,'meanTTc'])))
    }
    else if(after & !before){
      after_row <- after_doy - first_doy + 1
      stat <- abs(as.numeric(unname(byDay[after_row,'meanTTc'])) - actual_TTc)
    }
    else{
      before_row <- before_doy - first_doy + 1
      statbef <- abs(actual_TTc - as.numeric(unname(byDay[before_row,'meanTTc'])))
      stataft <- abs(as.numeric(unname(byDay[after_row,'meanTTc'])) - actual_TTc)
      stat <- min(c(statbef,stataft))
    }
    absdiffs <- c(absdiffs,stat)
    sq <- stat * stat
    sqdiffs <- c(sqdiffs,sq)
    percdiffs <- c(percdiffs,stat/actual_TTc * 100)
    chisq <- c(chisq,stat * stat/actual_TTc)
  }
  for(i in 1:nrow(actual_yield)){
    yield <- as.numeric(unname(actual_yield[i,2]))
    actual_doy <- as.numeric(unname(actual_yield[i,1]))
    before <- FALSE
    after <- FALSE
    row <- 1
    for(j in first_doy:actual_doy){
      if(as.numeric(unname(byDay[row,'meanYield']))>yield){
        before_doy <- j
        before <- TRUE
        break
      }
      row <- row + 1
    }
    
    start <- actual_doy + 1
    row <- actual_doy - first_doy + 2
    if(!before){
      for(j in start:last_doy){
        if(as.numeric(unname(byDay[row,'meanYield']))>yield){
          after_doy <- j
          after <- TRUE
          break
        }
        row <- row + 1
      }
    }
    else{
      for(j in start:last_doy){
        if(as.numeric(unname(byDay[row,'meanYield']))<yield){
          after_doy <- j
          after <- TRUE
          break
        }
        row <- row + 1
      }
    }
    actual_row <- actual_doy - first_doy + 1
    actual_TTc <- as.numeric(unname(byDay[actual_row,'meanTTc']))
    if(!before & !after){
      stat <- actual_TTc
    }
    else if(before & !after){
      before_row <- before_doy - first_doy + 1
      stat <- abs(actual_TTc - as.numeric(unname(byDay[before_row,'meanTTc'])))
    }
    else if(after & !before){
      after_row <- after_doy - first_doy + 1
      stat <- abs(as.numeric(unname(byDay[after_row,'meanTTc'])) - actual_TTc)
    }
    else{
      before_row <- before_doy - first_doy + 1
      statbef <- abs(actual_TTc - as.numeric(unname(byDay[before_row,'meanTTc'])))
      stataft <- abs(as.numeric(unname(byDay[after_row,'meanTTc'])) - actual_TTc)
      stat <- min(c(statbef,stataft))
    }
    absdiffs <- c(absdiffs,stat)
    sq <- stat * stat
    sqdiffs <- c(sqdiffs,sq)
    percdiffs <- c(percdiffs,stat/actual_TTc * 100)
    chisq <- c(chisq,stat * stat/actual_TTc)
  }
  print(absdiffs)
  SumOfDiffsSquared = as.numeric(sum(sqdiffs))
  SumOfAbsDiffs = as.numeric(sum(absdiffs))
  SumOfAbsPercs = as.numeric(sum(percdiffs))

  n = length(absdiffs)
  
  RMSE = sqrt(SumOfDiffsSquared/n)
  MAE = SumOfAbsDiffs/n
  MAPE = SumOfAbsPercs/n
  ChiSquare = as.numeric(sum(chisq))
  
  print(paste(RMSE,MAE,MAPE,ChiSquare))
  
  #return as a dataframe
  stats = data.frame(RMSE = c(RMSE), MAE = c(MAE), MAPE = c(MAPE), ChiSquare = c(ChiSquare))
  
  return(stats)
}

View(actual_leaf_stem_2016)

#Looks at difference in TTc values
#Does a decent job at modeling, but there's no statistical motivation for it
Statistics3 <- function(model_result,actual_leaf_stem,actual_leaf_data){
  byDay <- model_result %>% 
    group_by(doy) %>%
    summarize(meanLeaf = mean(Leaf), meanStem = mean(Stem),leafAreaIndex=mean(lai),meanTTc = mean(TTc))
  first_doy <- as.numeric(unname(byDay[1,1]))
  last_doy <- as.numeric(unname(byDay[nrow(byDay),1]))
  absdiffs <- vector()
  sqdiffs <- vector()
  percdiffs <- vector()
  chisq <- vector()
  for(i in 1:nrow(actual_leaf_data)){
    actual_lai <- as.numeric(unname(actual_leaf_data[i,2]))
    actual_doy <- as.numeric(unname(actual_leaf_data[i,1]))
    before <- FALSE
    after <- FALSE
    row <- 1
    for(j in first_doy:actual_doy){
      if(as.numeric(unname(byDay[row,'leafAreaIndex']))>actual_lai){
        before_doy <- j
        before <- TRUE
        break
      }
      row <- row + 1
    }
    
    start <- actual_doy + 1
    row <- actual_doy - first_doy + 2
    if(!before){
      for(j in start:last_doy){
        if(as.numeric(unname(byDay[row,'leafAreaIndex']))>actual_lai){
          after_doy <- j
          after <- TRUE
          break
        }
        row <- row + 1
      }
    }
    else{
      for(j in start:last_doy){
        if(as.numeric(unname(byDay[row,'leafAreaIndex']))<actual_lai){
          after_doy <- j
          after <- TRUE
          break
        }
        row <- row + 1
      }
    }
    
    actual_row <- actual_doy - first_doy + 1
    actual_TTc <- as.numeric(unname(byDay[actual_row,'meanTTc']))
    if(!before & !after){
      stat <- actual_TTc
    }
    else if(before & !after){
      before_row <- before_doy - first_doy + 1
      stat <- abs(actual_TTc - as.numeric(unname(byDay[before_row,'meanTTc'])))
    }
    else if(after & !before){
      after_row <- after_doy - first_doy + 1
      stat <- abs(as.numeric(unname(byDay[after_row,'meanTTc'])) - actual_TTc)
    }
    else{
      before_row <- before_doy - first_doy + 1
      after_row <- after_doy - first_doy +1
      statbef <- abs(actual_TTc - as.numeric(unname(byDay[before_row,'meanTTc'])))
      stataft <- abs(as.numeric(unname(byDay[after_row,'meanTTc'])) - actual_TTc)
      stat <- min(c(statbef,stataft))
    }
    absdiffs <- c(absdiffs,stat)
    sq <- stat * stat
    sqdiffs <- c(sqdiffs,sq)
    percdiffs <- c(percdiffs,stat/actual_TTc * 100)
    chisq <- c(chisq,stat * stat/actual_TTc)
  }
  for(i in 1:nrow(actual_leaf_stem)){
    leaf <- as.numeric(unname(actual_leaf_stem[i,2]))
    stem <- as.numeric(unname(actual_leaf_stem[i,3]))
    actual_doy <- as.numeric(unname(actual_leaf_stem[i,1]))
    before_leaf <- FALSE
    after_leaf <- FALSE
    before_stem <- FALSE
    after_stem <- FALSE
    row <- 1
    for(j in first_doy:actual_doy){
      if(as.numeric(unname(byDay[row,'meanLeaf']))>leaf){
        before_leaf_doy <- j
        before_leaf <- TRUE
        break
      }
      row <- row + 1
    }
    row <- 1
    for(j in first_doy:actual_doy){
      if(as.numeric(unname(byDay[row,'meanStem']))>stem){
        before_stem_doy <- j
        before_stem <- TRUE
        break
      }
      row <- row + 1
    }
    
    start <- actual_doy + 1
    row <- actual_doy - first_doy + 2
    if(!before_leaf){
      for(j in start:last_doy){
        if(as.numeric(unname(byDay[row,'meanLeaf']))>leaf){
          after_leaf_doy <- j
          after_leaf <- TRUE
          break
        }
        row <- row + 1
      }
    }
    else{
      for(j in start:last_doy){
        if(as.numeric(unname(byDay[row,'meanLeaf']))<leaf){
          after_leaf_doy <- j
          after_leaf <- TRUE
          break
        }
        row <- row + 1
      }
    }
    
    start <- actual_doy + 1
    row <- actual_doy - first_doy + 2
    if(!before_stem){
      for(j in start:last_doy){
        if(as.numeric(unname(byDay[row,'meanStem']))>stem){
          after_stem_doy <- j
          after_stem <- TRUE
          break
        }
        row <- row + 1
      }
    }
    else{
      for(j in start:last_doy){
        if(as.numeric(unname(byDay[row,'meanStem']))<stem){
          after_stem_doy <- j
          after_stem <- TRUE
          break
        }
        row <- row + 1
      }
    }
    
    actual_row <- actual_doy - first_doy + 1
    actual_TTc <- as.numeric(unname(byDay[actual_row,'meanTTc']))
    if(!before_leaf & !after_leaf){
      stat <- actual_TTc
    }
    else if(before_leaf & !after_leaf){
      before_row <- before_leaf_doy - first_doy + 1
      stat <- abs(actual_TTc - as.numeric(unname(byDay[before_row,'meanTTc'])))
    }
    else if(after_leaf & !before_leaf){
      after_row <- after_leaf_doy - first_doy + 1
      stat <- abs(as.numeric(unname(byDay[after_row,'meanTTc'])) - actual_TTc)
    }
    else{
      before_row <- before_leaf_doy - first_doy + 1
      after_row <- after_leaf_doy - first_doy + 1
      statbef <- abs(actual_TTc - as.numeric(unname(byDay[before_row,'meanTTc'])))
      stataft <- abs(as.numeric(unname(byDay[after_row,'meanTTc'])) - actual_TTc)
      stat <- min(c(statbef,stataft))
    }
    absdiffs <- c(absdiffs,stat)
    sq <- stat * stat
    sqdiffs <- c(sqdiffs,sq)
    percdiffs <- c(percdiffs,stat/actual_TTc * 100)
    chisq <- c(chisq,stat * stat/actual_TTc)
    
    if(!before_stem & !after_stem){
      stat <- actual_TTc
    }
    else if(before_stem & !after_stem){
      before_row <- before_stem_doy - first_doy + 1
      stat <- abs(actual_TTc - as.numeric(unname(byDay[before_row,'meanTTc'])))
    }
    else if(after_stem & !before_stem){
      after_row <- after_stem_doy - first_doy + 1
      stat <- abs(as.numeric(unname(byDay[after_row,'meanTTc'])) - actual_TTc)
    }
    else{
      before_row <- before_stem_doy - first_doy + 1
      after_row <- after_stem_doy - first_doy + 1
      statbef <- abs(actual_TTc - as.numeric(unname(byDay[before_row,'meanTTc'])))
      stataft <- abs(as.numeric(unname(byDay[after_row,'meanTTc'])) - actual_TTc)
      stat <- min(c(statbef,stataft))
    }
    absdiffs <- c(absdiffs,stat)
    sq <- stat * stat
    sqdiffs <- c(sqdiffs,sq)
    percdiffs <- c(percdiffs,stat/actual_TTc * 100)
    chisq <- c(chisq,stat * stat/actual_TTc)
  }
  print(absdiffs)
  SumOfDiffsSquared = as.numeric(sum(sqdiffs))
  SumOfAbsDiffs = as.numeric(sum(absdiffs))
  SumOfAbsPercs = as.numeric(sum(percdiffs))
  
  n = length(absdiffs)
  
  RMSE = sqrt(SumOfDiffsSquared/n)
  MAE = SumOfAbsDiffs/n
  MAPE = SumOfAbsPercs/n
  ChiSquare = as.numeric(sum(chisq))
  
  print(paste(RMSE,MAE,MAPE,ChiSquare))
  
  #return as a dataframe
  stats = data.frame(RMSE = c(RMSE), MAE = c(MAE), MAPE = c(MAPE), ChiSquare = c(ChiSquare))
  
  return(stats)
}

#function returns a graph of lai for the model data and the actual data
getGraphLeafArea <- function(model_result,actual_yield,sub="") {
  fullJoinDf <- full_join(model_result,actual_yield,by = c("time" = "date")) #joins the two dataframes so we can graph on the same axes
  #stats_df = Statistics1(model_result,actual_yield)
  #stats_str = ""
  #for(stat in colnames(stats_df)){
  #  stats_str=paste0(stats_str,stat,": ",round(stats_df[1,stat],digits=2),"  ")
  #}
  
  return(ggplot(fullJoinDf,aes(time)) + #time as x-axis
           geom_point(aes(y = (lai), color = "BioCro Model")) + #model data
           geom_point(aes(y = ActualMeanLeafAreaIndex, color = "Actual LAI")) + #actual data
           xlab("Day of Year") + #x-axis label
           ylab("Leaf Area Index (unitless)") + #y-axis label
           labs(title = "Sorghum Model Leaf Area Index vs. Actual Data", #graph title
                subtitle = sub) +  #subtitle
                #caption = stats_str) + #put stats in caption
           scale_color_manual(name = "",values = c("red", "blue")) + #model in blue, actual in red
           theme(legend.position = "bottom")) #put legend at bottom
}

#function returns a graph of the model data with the actual data
getGraphYield <- function(model_result,actual_yield,actual_leaf_area,sub="") {
  fullJoinDf <- full_join(model_result,actual_yield,by = c("time" = "date")) #joins the two dataframes so we can graph on the same axes
  stats_df = Statistics1(model_result,actual_yield,actual_leaf_area)
  stats_str = "Statistics for Leaf Area and Yield (combined) "
  for(stat in colnames(stats_df)){
    stats_str=paste0(stats_str,stat,": ",round(stats_df[1,stat],digits=2),"  ")
  }
  
  return(ggplot(fullJoinDf,aes(time)) + #time as x-axis
           geom_point(aes(y = (Leaf + Stem), color = "BioCro Model")) + #model data
           geom_point(aes(y = ActualMeanYield, color = "Actual Yield")) + #actual data
           xlab("Day of Year") + #x-axis label
           ylab("Leaf + Stem (Mg/ha)") + #y-axis label
           labs(title = "Sorghum Model Yield vs. Actual Data", #graph title
           subtitle = sub, #subtitle
           caption = stats_str) + #put stats in caption
           scale_color_manual(name = "Data Source",values = c("red", "blue")) + #model in blue, actual in red
           theme(legend.position = "bottom")) #put legend at bottom
}

#3 graphs side by side
getGraphAll <- function(model_result,actual_leaf_stem,actual_leaf_area,subleaf="",subyield="") {
  fullJoinDf <- full_join(model_result,actual_leaf_stem,by = c("time" = "date")) #joins the two dataframes so we can graph on the same axes
  stats_df = Statistics3(model_result,actual_leaf_stem,actual_leaf_area)
  stats_str = "Statistics for Leaf Area, Leaf, Stem (combined) "
  for(stat in colnames(stats_df)){
    stats_str=paste0(stats_str,stat,": ",round(stats_df[1,stat],digits=2),"  ")
  }
  print(stats_str)
  
  LeafArea = getGraphLeafArea(model_result,actual_leaf_area,sub=subleaf)
  
  Leaf = ggplot(fullJoinDf,aes(time)) + #time as x-axis
    geom_point(aes(y = Leaf, color = "BioCro Model")) + #model data
    geom_point(aes(y = ActualMeanLeaf, color = "Actual Leaf")) + #actual data
    xlab("Day of Year") + #x-axis label
    ylab("Leaf (Mg/ha)") + #y-axis label
    labs(title = "Sorghum Model Leaf vs. Actual Data", #graph title
         subtitle = subyield) + #subtitle
    scale_color_manual(name = "Data Source",values = c("red", "blue")) + #model in blue, actual in red
    theme(legend.position = "bottom") #put legend at bottom
  
  Stem = ggplot(fullJoinDf,aes(time)) + #time as x-axis
           geom_point(aes(y = Stem, color = "BioCro Model")) + #model data
           geom_point(aes(y = ActualMeanStem, color = "Actual Stem")) + #actual data
           xlab("Day of Year") + #x-axis label
           ylab("Stem (Mg/ha)") + #y-axis label
           labs(title = "Sorghum Model Stem vs. Actual Data", #graph title
                subtitle = subyield, #subtitle
                caption = stats_str) + #put stats in caption
           scale_color_manual(name = "Data Source",values = c("red", "blue")) + #model in blue, actual in red
           theme(legend.position = "bottom") #put legend at bottom
  
  return(LeafArea + Leaf + Stem)
}

#input previously run parameters that we liked
new_params_final_temp <- iSp2.786_Params_fitted_to_EF2016LeafAreaAndYield_minimizing_ChiSquareMod
sorghum_parameters <- as.list(new_params_final_temp[2,])
names(sorghum_parameters) <- new_params_final_temp[1,]
View(sorghum_parameters)

setwd("C://Users/stark/OneDrive/Documents2021/biocro-dev/with_partitioning_selector/fixed_0707")

#runs hjkb and prints graphs
run_and_print_graphs <- function(to_minimize,init_val,old_params,year_of_testset,testset,params_to_change,lower,upper){
  
  new_params_final <- new_parameters(params_to_change,lower,upper,
                                     old_params,to_minimize,year_of_testset,testset)
  
  new_params_final <- normalize_kVals(new_params_final)
  
  n <- nrow(datasets)
  
  for(i in 1:n){
    old_model <- run_biocro(
      sorghum_initial_values,
      old_params,
      yrClimate(datasets[i,'Year']),
      sorghum_steady_state_modules,
      sorghum_derivative_modules
    )
    new_model <- run_biocro(
      sorghum_initial_values,
      new_params_final,
      yrClimate(datasets[i,'Year']),
      sorghum_steady_state_modules,
      sorghum_derivative_modules
    )
    #make a pdf file graph
    pdf(
      file = paste0(init_val,"_preAndPostParam",datasets[i,'Str'],"_fit",testset,"_partitioningSelector_minimize",to_minimize,".pdf"),
      width = 15,          # inches
      height = 15,         # inches
      useDingbats = FALSE # make sure symbols are rendered properly in the PDF
    )
    pre <- getGraphAll(old_model,yrLeafStem(datasets[i,'Str']),yrLeafArea(datasets[i,'Str']),
                       subleaf=paste(datasets[i,'Str_with_spaces'],"default parameters"),
                       subyield=paste(datasets[i,'Str_with_spaces'],"default parameters"))

    post <- getGraphAll(new_model,yrLeafStem(datasets[i,'Str']),yrLeafArea(datasets[i,'Str']),
                        subleaf=paste(datasets[i,'Str_with_spaces'],"parameters fitted to",to_minimize),
                        subyield=paste(datasets[i,'Str_with_spaces'],"parameters fitted to",to_minimize))
    
    print(pre / post)
    
    dev.off()
  }
  
  #store the list of parameters for future reference
  write.table(old_params,file=paste0(init_val,"_Params.txt"))
  write.table(new_params_final,file=paste0(init_val,"_Params_fitted_to_",testset,"_minimizing_",to_minimize,".txt"))
}



params_and_bounds <- data.frame("params" = c("iSp","Sp_thermal_time_decay","tp1",
                                            "kLeaf1",
                                            "kStem1",
                                            "kRoot1"),
                                "lower" = c(0.3,-9e-4,20,
                                            1e-8,
                                            1e-8,
                                            1e-8),
                                "upper" = c(200,9e-4,10000,
                                            1,
                                            1,
                                            1))


run_and_print_graphs("ChiSquare","iSp1.7",sorghum_parameters,2016,"2016EF",params_and_bounds[,'params'],c(0.3,-9e-4,20,
                                                                                                          1e-8,
                                                                                                          1e-8,
                                                                                                          1e-8),c(200,9e-4,10000,
                                                                                                                  1,
                                                                                                                  1,
                                                                                                                  1))

setwd("C://Users/stark/OneDrive/Documents2021/biocro-dev/with_partitioning_selector/optimize_ttc_diff/With_randoms")

#runs with random starting values
n <- nrow(params_and_bounds)
set.seed(5)
for(i in 1:6){
  for(j in 1:n){
    if(!params_and_bounds[j,'params'])
    sorghum_parameters[[params_and_bounds[j,'params']]] <- as.numeric(runif(1,params_and_bounds[j,'lower'],params_and_bounds[j,'upper']))
  }
  run_and_print_graphs("MAE",paste0("iSp",sorghum_parameters[['iSp']]),sorghum_parameters,2016,"2016EF")
}

#this would run every possible set of starting values with a given granularity...that would take too long usually
#num_steps <- 2
#for(i in 0:(num_steps+1)^n){
#  value <- i
#  for(j in 1:n){
#    multiplier <- mod(value,num_steps+1)
#    delta <- (params_and_bounds[j,'upper'] - params_and_bounds[j,'lower']) / num_steps
#    sorghum_parameters[[params_and_bounds[j,'params']]] <- params_and_bounds[j,'lower'] + multiplier * delta
#    value <- floor(value/(num_steps+1))
#  }
#  run_and_print_graphs("MAE",paste0("iSp",sorghum_parameters[['iSp']]),sorghum_parameters,2016,"2016EF")
#}
