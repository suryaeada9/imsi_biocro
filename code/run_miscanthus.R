library(BioCro)
library(ggplot2)
library(dplyr)
library(patchwork)
library(dfoptim)

# Some modules are included as named list elements so they can be easily changed
# on-the-fly to a different value, e.g.,
# CROP_steady_state_modules[['canopy_photosynthesis']] <- 'ten_layer_rue_canopy'
miscanthus_x_giganteus_steady_state_modules <- list(
  "soil_type_selector",
  stomata_water_stress = "stomata_water_stress_linear",
  "leaf_water_stress_exponential",
  "parameter_calculator",
  "soil_evaporation",
  canopy_photosynthesis = "c4_canopy",
  partitioning_coefficients = "partitioning_coefficient_selector",
  partitioning_growth_calculator = "partitioning_growth_calculator"
)

miscanthus_x_giganteus_derivative_modules <- list(
  senescence = "thermal_time_senescence",
  "partitioning_growth",
  thermal_time = "thermal_time_trilinear",
  soil_profile = "one_layer_soil_profile"
)

# Error tolerances greater than 1e-5 may cause problems with the regression test
miscanthus_x_giganteus_integrator <- list(
  type = 'auto',
  output_step_size = 1.0,
  adaptive_rel_error_tol = 1e-5,
  adaptive_abs_error_tol = 1e-5,
  adaptive_max_steps = 200
)

# Do the calculations inside an empty list so that temporary variables are not created in .Global.
miscanthus_x_giganteus_initial_values = with(list(), {
  datalines =
    "symbol                     value
    Grain                       0
    Leaf                        7e-04
    LeafLitter                  0
    leaf_senescence_index       0
    Rhizome                     7
    RhizomeLitter               0
    rhizome_senescence_index    0
    Root                        0.007
    RootLitter                  0
    root_senescence_index       0
    soil_water_content          0.32
    Stem                        0.007
    StemLitter                  0
    stem_senescence_index       0
    TTc                         0"
  
  data_frame = utils::read.table(textConnection(datalines), header=TRUE)
  values = as.list(data_frame$value)
  names(values) = data_frame$symbol
  values
})

# Do the calculations inside an empty list so that temporary variables are not created in .Global.
miscanthus_x_giganteus_parameters = with(list(), {
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
    chil                        1
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
    kLeaf1                      0.33
    kLeaf2                      0.14
    kLeaf3                      0.01
    kLeaf4                      0.01
    kLeaf5                      0.01
    kLeaf6                      0.01
    kparm                       0.7
    kpLN                        0.2
    kRhizome1                   -8e-04
    kRhizome2                   -5e-04
    kRhizome3                   0.35
    kRhizome4                   0.35
    kRhizome5                   0.35
    kRhizome6                   0.35
    kRoot1                      0.3
    kRoot2                      0.01
    kRoot3                      0.01
    kRoot4                      0.01
    kRoot5                      0.01
    kRoot6                      0.01
    kStem1                      0.37
    kStem2                      0.85
    kStem3                      0.63
    kStem4                      0.63
    kStem5                      0.63
    kStem6                      0.63
    lat                         40
    LeafN                       2
    LeafN_0                     2
    leaf_transmittance          0.2
    leaf_reflectance            0.2
    leafwidth                   0.04
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
    tbase                       -6
    topt_lower                  10
    topt_upper                  28
    tmax                        45
    theta                       0.83
    timestep                    1
    tp1                         562
    tp2                         1312
    tp3                         2063
    tp4                         2676
    tp5                         3211
    upperT                      37.5
    vmax1                       39
    vmax_n_intercept            0
    water_stress_approach       1"
  
  data_frame = utils::read.table(textConnection(datalines), header=TRUE)
  values = as.list(data_frame$value)
  names(values) = data_frame$symbol
  values
})

#precalculate some stuff about the actual and model data to make Statistics functions easier
df_inner_join <- function(actualData,modelResult){
  byDay <- modelResult %>% 
    group_by(doy) %>%
    summarize(meanYield = mean(Leaf) + mean(Stem),meanTTc = mean(TTc)) #makes a new dataframe that gives the average yield per day (over all 24 values in the model_result dataframe)
  modelResult['Yield'] <- modelResult[['Leaf']] + modelResult[['Stem']]
  model_TTc <- modelResult[which.max(modelResult$Yield),'TTc']
  actual_peak_doy <- as.numeric(actualData[1,2])
  first_doy <- as.numeric(byDay[1,1])
  peak_row <- actual_peak_doy - first_doy + 1
  actual_TTc <- as.numeric(byDay[peak_row,'meanTTc'])
  diff_TTc <- model_TTc - actual_TTc
  #Leaf + Stem is specific to sorghum and needs to be changed for other crops
  innerJoinDf <- inner_join(byDay,actualData,by = c("doy")) #joins the two dataframes so we can compare values and run stats
  innerJoinDf <- mutate(innerJoinDf,actualMinusModel = yield - meanYield) #makes a column for actual data minus model data
  innerJoinDf <- mutate(innerJoinDf,actualMinusModelAbs = abs(actualMinusModel)) #makes a column for absolute value of difference
  innerJoinDf <- mutate(innerJoinDf,diffSquares = actualMinusModel * actualMinusModel) #makes a column for the square of the difference
  innerJoinDf <- mutate(innerJoinDf,absPercentError = actualMinusModelAbs/yield * 100) #makes a column for absolute value of percentage difference
  #the model was overfitting lai and ignoring yield
  #absolute percent error is good for values that are close together (say, up to 20% off)
  #then it doesn't matter whether the actual or the model value are in the denominator
  #our data is up to 3000% off on the default parameters
  #the actual lai values are smaller than the model, and the actual yield values are larger than the model
  #if there's a 10x error between the values, this would result in a calculated 900% error for lai and only 90% for yield
  #that's why hjkb seems to prefer to fix lai when minimizing MAPE
  #the line below divides by the minimum of the actual and model value to hopefully fix this problem
  innerJoinDf <- mutate(innerJoinDf,absPercentErrorByMin = actualMinusModelAbs/min(c(yield,meanYield)) * 100)
  innerJoinDf <- mutate(innerJoinDf,chiSquareToSum = diffSquares/yield) #makes a column for square of difference over actual value (for chi square calculation)
  innerJoinDf <- mutate(innerJoinDf,chiSquareModToSum = diffSquares/min(c(yield,meanYield))) #makes a column for square of difference over actual value (for chi square calculation)
  
  innerJoinDf[nrow(innerJoinDf) + 1,] = list(-1,-1,-1,"",-1,-1,diff_TTc,abs(diff_TTc),diff_TTc * diff_TTc, abs(diff_TTc)/actual_TTc * 100, abs(diff_TTc)/min(c(actual_TTc,model_TTc)) * 100, diff_TTc * diff_TTc / actual_TTc, diff_TTc * diff_TTc /min(c(actual_TTc,model_TTc))) 
  return(innerJoinDf)
}

df_inner_join(city_data("Adana"),adana_result)

#calculate RMSE, MAE, MAPE, and Chi Square for data from a given siteId vs the BioCro model
StatisticsMultipleCities <- function(city_names,parameters,steady_state_mods,deriv_mods){
  df = data.frame()
  for(city_name in city_names){
    innerJoinDf <- df_inner_join(city_data(city_name),misc_result(city_name,parameters,steady_state_mods,deriv_mods))
    df <- rbind(df,innerJoinDf)
  }
  #find sums of a few columns for stats calculations
  SumOfDiffsSquared = sum(df[,'diffSquares'])
  SumOfAbsDiffs = sum(df[,'actualMinusModelAbs'])
  SumOfAbsPercs = sum(df[,'absPercentError'])
  SumOfAbsPercsByMin = sum(df[,'absPercentErrorByMin'])
  
  #number of rows
  n = nrow(df)
  
  #stats calculations
  RMSE = sqrt(SumOfDiffsSquared/n)
  MAE = SumOfAbsDiffs/n
  MAPE = SumOfAbsPercs/n
  MAPE_mod = SumOfAbsPercsByMin/n
  ChiSquare = sum(df[,'chiSquareToSum'])
  ChiSquareMod = sum(df[,'chiSquareModToSum'])
  
  stats = data.frame(RMSE = c(RMSE), MAE = c(MAE), MAPE = c(MAPE), MAPE_mod = c(MAPE_mod), ChiSquare = c(ChiSquare), ChiSquareMod = c(ChiSquareMod))
  
  return(stats)
}

misc_result <- function(city_name,parameters,steady_state_mods,deriv_mods){
  file_path <- paste0("C://Users/stark/OneDrive/Documents2021/biocro-dev/miscanthus/",city_name,"_weather.csv")
  climate <- read.csv(file_path)
  result <- run_biocro(miscanthus_x_giganteus_initial_values,parameters,
                       climate,steady_state_mods,deriv_mods,miscanthus_x_giganteus_integrator)
  return(result)
}

adana_result <- misc_result("Adana",miscanthus_x_giganteus_parameters,miscanthus_x_giganteus_steady_state_modules,miscanthus_x_giganteus_derivative_modules)

Miscanthus_gigantus_data = within(Miscanthus_gigantus_data, {
  Harvest_Doy = lubridate::yday(Harvest_Date)})

city_data <- function(city_name){
  r <- Miscanthus_gigantus_data %>% filter(Location == city_name)
  city_df <- data.frame(location = c(city_name,city_name),doy= c(as.numeric(r[1,'Peak_Doy']),365 + as.numeric(r[1,'Harvest_Doy'])),year= c(2014,2015), yield = c(as.numeric(r[1,'Peak_Yield']),as.numeric(r[1,'Harvest_Yield'])))
  return(city_df)
}

model_and_actual <- function(city_name,parameters,steady_state_mods,deriv_mods){
  final_data <- full_join(misc_result(city_name,parameters,steady_state_mods,deriv_mods),city_data(city_name),by=c('doy','year'))
  return(final_data)
}




GraphMultipleCities <- function(city_names,parameters,steady_state_mods,deriv_mods){
  stats_df = StatisticsMultipleCities(city_names,parameters,steady_state_mods,deriv_mods)
  stats_str = "Statistics "
  for(stat in colnames(stats_df)){
    stats_str=paste0(stats_str,stat,": ",round(stats_df[1,stat],digits=2),"  ")
  }
  
  graph <- ggplot() + theme_void()
  
  
  l = length(city_names)
  i <- 1
  for(city_name in city_names){
    data <- model_and_actual(city_name,parameters,steady_state_mods,deriv_mods)
    
    if(i<l){
      city_graph <- ggplot(data,aes(time))+geom_point(aes(y = (Leaf + Stem), color = "BioCro Model")) + #model data
        geom_point(aes(y = yield, color = "Actual Yield")) + 
        xlab("Day of Year") + #x-axis label
        ylab("Leaf + Stem (Mg/ha)") + #y-axis label
        labs(title = "Miscanthus Model Yield vs. Actual Data", #graph title
          subtitle = city_name) + #put stats in caption
        scale_color_manual(name = "Data Source",values = c("red", "blue")) + 
        theme(legend.position = "bottom")
    }
    else{
      city_graph <- ggplot(data,aes(time))+geom_point(aes(y = (Leaf + Stem), color = "BioCro Model")) + #model data
        geom_point(aes(y = yield, color = "Actual Yield")) + 
        xlab("Day of Year") + #x-axis label
        ylab("Leaf + Stem (Mg/ha)") + #y-axis label
        labs(title = "Miscanthus Model Yield vs. Actual Data", #graph title
             subtitle = city_name,
             caption = stats_str) + #put stats in caption
        scale_color_manual(name = "Data Source",values = c("red", "blue")) + 
        theme(legend.position = "bottom")
    }
    
    graph <- graph / city_graph
    
    i <- i+1
  }
  
  return(graph)
}

#normalize kLeaf1, kStem1, kRoot1, kGrain1, etc.
#necessary for the partitioning selector model
normalize_kVals <- function(current_params){
  kVals <- data.frame(Leaf = c(current_params$kLeaf1,current_params$kLeaf2,current_params$kLeaf3,current_params$kLeaf4,current_params$kLeaf5,current_params$kLeaf6),
                      Stem = c(current_params$kStem1,current_params$kStem2,current_params$kStem3,current_params$kStem4,current_params$kStem5,current_params$kStem6),
                      Root = c(current_params$kRoot1,current_params$kRoot2,current_params$kRoot3,current_params$kRoot4,current_params$kRoot5,current_params$kRoot6),
                      Grain = c(current_params$kGrain1,current_params$kGrain2,current_params$kGrain3,current_params$kGrain4,current_params$kGrain5,current_params$kGrain6),
                      Rhizome = c(current_params$kRhizome1,current_params$kRhizome2,current_params$kRhizome3,current_params$kRhizome4,current_params$kRhizome5,current_params$kRhizome6))
  
  
  kVals <- mutate(kVals, Sum = Leaf + Stem + Root + Grain+Rhizome)
  kVals <- mutate(kVals, Leaf = Leaf/Sum)
  kVals <- mutate(kVals, Stem = Stem/Sum)
  kVals <- mutate(kVals, Root = Root/Sum)
  kVals <- mutate(kVals, Grain = Grain/Sum)
  kVals <- mutate(kVals, Rhizome = Rhizome/Sum)
  
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
  current_params$kRhizome1 <- kVals[1,5]
  current_params$kRhizome2 <- kVals[2,5]
  current_params$kRhizome3 <- kVals[3,5]
  current_params$kRhizome4 <- kVals[4,5]
  current_params$kRhizome5 <- kVals[5,5]
  current_params$kRhizome6 <- kVals[6,5]
  
  return(current_params)
}

#this is partial_run_biocro with one line of code added that normalizes the k values before running the model
#I couldn't figure out how to make this work without copying and pasting the function
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

#calculate RMSE, MAE, MAPE, and Chi Square for data from a given siteId vs the BioCro model
PartialStatisticsMultipleCities <- function(x,misc_partial,city_names,parameters,steady_state_mods,deriv_mods){
  df = data.frame()
  for(city_name in city_names){
    file_path <- paste0("C://Users/stark/OneDrive/Documents2021/biocro-dev/miscanthus/",city_name,"_weather.csv")
    climate <- read.csv(file_path)
    innerJoinDf <- df_inner_join(city_data(city_name),misc_partial(climate)(x))
    df <- rbind(df,innerJoinDf)
  }
  #find sums of a few columns for stats calculations
  SumOfDiffsSquared = sum(df[,'diffSquares'])
  SumOfAbsDiffs = sum(df[,'actualMinusModelAbs'])
  SumOfAbsPercs = sum(df[,'absPercentError'])
  SumOfAbsPercsByMin = sum(df[,'absPercentErrorByMin'])
  
  #number of rows
  n = nrow(df)
  
  #stats calculations
  RMSE = sqrt(SumOfDiffsSquared/n)
  MAE = SumOfAbsDiffs/n
  MAPE = SumOfAbsPercs/n
  MAPE_mod = SumOfAbsPercsByMin/n
  ChiSquare = sum(df[,'chiSquareToSum'])
  ChiSquareMod = sum(df[,'chiSquareModToSum'])
  
  stats = data.frame(RMSE = c(RMSE), MAE = c(MAE), MAPE = c(MAPE), MAPE_mod = c(MAPE_mod), ChiSquare = c(ChiSquare), ChiSquareMod = c(ChiSquareMod))
  
  return(stats)
}

new_parameters <- function(city_names,params_to_change,lower_bounds,upper_bounds,current_params,steady_state_mods,deriv_mods){
  misc_partial <- function(climate){
    return(partial_run_biocro_with_normalize(miscanthus_x_giganteus_initial_values,current_params,
                                             climate,steady_state_mods,deriv_mods,
                                             miscanthus_x_giganteus_integrator,params_to_change))
  }
  #st is stat: RMSE, MAE, MAPE, MAPE_mod, or ChiSquare
  misc_stat <- function(x,st="ChiSquare"){
    return(PartialStatisticsMultipleCities(x,misc_partial,city_names,current_params,steady_state_mods,deriv_mods)[,c(st)])
  }
  para <- as.numeric(unname(current_params[params_to_change]))
  ans <- hjkb(para,misc_stat,lower=lower_bounds,upper = upper_bounds)
  new_params <- ans$par
  names(new_params) <- params_to_change
  altered_params <- current_params
  for(p in params_to_change){
    altered_params[[p]] <- new_params[[p]]
  }
  return(altered_params)
}

new_params <- new_parameters(list("Adana","Aberystwyth","Moscow"),
                             c("iSp","Sp_thermal_time_decay","kLeaf1",'kLeaf2','kLeaf3',"kLeaf4",'kLeaf5','kLeaf6',
                                "kStem1",'kStem2','kStem3',"kStem4",'kStem5','kStem6',
                                "kRoot1",'kRoot2','kRoot3',"kRoot4",'kRoot5','kRoot6',
                               "kRhizome1",'kRhizome2','kRhizome3',"kRhizome4",'kRhizome5','kRhizome6',
                                 "tp1",'tp2','tp3',"tp4",'tp5'),
                             c(0.3,-1e-6,0.00001,0.00001,0.00001,0.00001,0.00001,0.00001,
                               0.00001,0.00001,0.00001,0.00001,0.00001,0.00001,
                               0.00001,0.00001,0.00001,0.00001,0.00001,0.00001,
                               -0.1,-0.1,-0.1,-0.1,-0.1,-0.1,
                               20,300,300,300,300),
                             c(200,1e-6,1,1,1,1,1,1,
                               1,1,1,1,1,1,
                               1,1,1,1,1,1,
                               1,1,1,1,1,1,
                               10000,10000,10000,10000,10000),miscanthus_x_giganteus_parameters,
                             miscanthus_x_giganteus_steady_state_modules,miscanthus_x_giganteus_derivative_modules)

new_params <- normalize_kVals(new_params)
View(new_params)

setwd("C://Users/stark/OneDrive/Documents2021/biocro-dev/miscanthus")

#make a pdf file graph
pdf(
  file = "AdAbM_after_fit_AdAbM_MAPE.pdf",
  width = 7,          # inches
  height = 10.5,         # inches
  useDingbats = FALSE # make sure symbols are rendered properly in the PDF
)

x11()

print(GraphMultipleCities(list("Adana","Aberystwyth","Moscow"),new_params,miscanthus_x_giganteus_steady_state_modules,miscanthus_x_giganteus_derivative_modules))

dev.off()

#store the list of parameters for future reference
write.table(new_params,file="siSp1.7_Params_fitted_to_AdAbM_minimizing_MAPE.txt")
