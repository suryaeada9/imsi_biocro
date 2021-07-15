# This script provides a demonstration of a few important BioCro functions and
# some R basics. It is a sort of "quick start guide" for using BioCro and R. To
# run this script, make sure it is located in the current working directory of
# an R session and type the following command:
#
#     source("tutorial.R")
#
# To determine the current working directory, you can use this command:
#
#    getwd()
#
# To see the documentation for an R command, which can be helpful for
# understanding what it does and what options it has, type a question mark
# followed by the name of the command. For example:
#
#     ?getwd
#
# This script is easiest to view using a text editor with "syntax highlighting"
# such as `Notepad++`. This will make the comments show up in a different color,
# so it's easy to tell them apart from the rest of the code.
#
# This script also contains some commands in the comments. To run them, you will
# need to copy them to the R terminal and enter them that way. (They will not
# automatically be evaluated when this script is run.)


# We will be using functions from the `BioCro` and `lattice` libraries, so make
# sure they are loaded.


#**********************************************************************
# github link may be provided to know where to install Biocro from?
#**********************************************************************

library(BioCro)
library(lattice)

# To run a BioCro simulation, the user must specify lists of "modules" to use.
# A module represents one or more related equations that model some aspect of
# plant biology. Each module has a set of input quantities and a set of output
# quantities; when a module is run, the values of its output quantities are
# determined from the values of its input values according to its equations. 
# Modules come in two types: "derivative modules" calculate the rates of change
# for their output quantities, while "steady state modules" calculate
# instantaneous values for their output quantities.
# 
# Here we will specify separate lists of steady state and derivative modules to
# use for a sorghum simulation. We will do this using the R command `list`,
# which creates a list from its arguments. To specify a module, we just need to
# supply its name, so we will be creating a list of strings.
#
# In R, when assigning a value to a variable, there are two options: a regular
# equals sign (=) or an arrow (<-). For some reason, the arrow is used much more
# often, so we will use it here, although in principle it could be replaced by
# an equals sign.

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



# Besides the modules, the user must also provide parameter values, weather
# data, and the initial values of any quantities that are governed by derivative
# modules. All these pieces of information are passed to the `Gro_solver`
# function, which then determines the plant's state at a sequence of times.
#
# To learn more about the `Gro_solver` function, see the file `R/Gro.R` in the
# BioCro repository, which can be viewed locally or through the GitHub website.
#
# For this example, we will use weather data from 2005, which is provided as
# part of the BioCro package. The `get_growing_season_climate` function is also
# part of the BioCro library; it limits the weather data to just the part of the
# year where crops would typically grow. Default values are also provided for
# the parameters and initial values required for a sorghum simulation.
#
# The `sorghum_initial_values` and `sorghum_parameters` objects are also R
# lists that are included in the BioCro package, but they are a little different
# than the `sorghum_steady_state_modules` and `sorghum_derivative_modules`
# because they have named elements. To see the difference, try typing the
# following commands:
#
#     View(sorghum_steady_state_modules)
#     View(sorghum_parameters)
#
# In the case of `sorghum_steady_state_modules`, each element is a string, but
# the elements do not have names. In `sorghum_parameters`, each element is a
# numerical value with a name attached, for example, there is an element named
# `atmospheric_pressure` whose value is 101325. Its value can be accessed with
# the following command:
#
#     sorghum_parameters[['atmospheric_pressure']]
# 
# It can also be accessed using
#
#     sorghum_parameters$atmospheric_pressure,
#
# but this method is not recommended because the `$` operator uses partial name
# matching by default, which can cause problems.
#
# The output from `Gro_solver` is returned as a "data frame", which is another
# type of R object (in addition to R lists, which we have already seen). An R
# data frame is essentially a table with named columns. In this case,
# `sorghum_result` is a table where each row is a particular time point and each
# column is one of the quantities determined by the simulation.
#
# The whole data frame can be visualized with the `View` function:
#
#     View(sorghum_result)
#
# It's also possible to just view the column names:
#
#     View(colnames(sorghum_result))
#
# There is another command that will print the column names and the values of
# the first few entries in each column:
#
#     str(sorghum_result)
#
# The individual columns can be accessed the same way as the named list elements
# above. For example, if we want to see the `doy` column, which represents the
# Day Of Year (DOY), we could type
#
#     View(sorghum_result[['doy']])
#
# It's also possible to view a subset of the data frame just showing some
# columns:
#
#     View(sorghum_result[,c('doy', 'hour', 'Leaf')])
#
# This method can also be used to view just a single time point or a range of
# times. For example, we could view just the first time point, or all times
# between the 10th and 20th points:
#
#     View(sorghum_result[1,c('doy', 'hour', 'Leaf')])
#     View(sorghum_result[10:20,c('doy', 'hour', 'Leaf')])
#
# In these commands, we have used a colon to specify a range of integers via the
# command
#
#    10:20
#
# and a set of column names via the
#
#    c('doy', 'hour', 'Leaf')
#
# command. Both of these last two commands produce an R vector, which is similar
# to a list but can't have named elements. Instead, its elements are always
# accessed by an index. For example, we could write
#
#     name_vector <- c('doy', 'hour', 'Leaf')
#     name_vector[1]
#
# This will produce "doy" as its output, since the first element of the vector
# is "doy".
#
# Now we have introduced the most important R data types for BioCro analysis
# (lists, data frames, and vectors). So let's keep going with the simulation!


source("~/imsi/Internship/biocro/R/Gro.R")

sorghum_result <- Gro_solver(
    sorghum_initial_values,
    sorghum_parameters,
    get_growing_season_climate(weather04),
    sorghum_steady_state_modules,
    sorghum_derivative_modules
)

View(sorghum_result)

# We can also calculate a soybean simulation using a set of default modules and
# parameters. Here we need to use a different weather file and specify
# additional solver settings (for reasons that are outside the scope of this
# tutorial).

soybean_result <- Gro_solver(
    soybean_initial_values,
    soybean_parameters,
    soybean_weather2002,
    soybean_steadystate_modules,
    soybean_derivative_modules,
    soybean_solver_params
)

# Viewing the result directly as a data frame can be useful at times, but it's
# often more useful to plot one or more columns from the result against an
# independent variable such as time. We can do this with the `xyplot` function
# from the `lattice` library. This function produces an R "trellis" object
# which can be viewed with the `print` function. To ensure that the plot is
# printed to a new window, we can call the `x11` function beforehand, which
# makes a new window.

sorghum_plot_v1 <- xyplot(
    sorghum_result[['Leaf']] ~ sorghum_result[['time']]
)

x11()

print(sorghum_plot_v1)

# In that command, we specified the y values by extracting the `Leaf` column
# from the `sorghum_result` data frame. Similarly, we specified the x values by
# extracting the `time` column. This notation can get really tedious, especially
# when plotting multiple things on the y-axis. As an alternative, we can tell
# `xyplot` that we want to plot columns from `sorghum_result` and then just
# specify the column names. This is done by specifying the optional `data`
# argument when calling `xyplot`:

sorghum_plot_v2 <- xyplot(
    Leaf ~ time,
    data = sorghum_result
)

x11()

print(sorghum_plot_v2)

# Here is a more advanced example where we plot multiple organ masses on the y
# axis, provide axis labels, specify axis ranges, add a legend, etc. Here you
# can notice that we use the `c` command to create a vector when specifying the
# axis limits.

sorghum_plot_v3 = xyplot(
    Stem + Leaf + Root ~ time,                      # Specify multiple data series using `+`
    data = sorghum_result,                          # Plot data from `sorghum_result`
    type = 'b',                                     # Plots using points and a line (set to 'l' for just a line or 'p' for just points)
    pch = 20,                                       # Use a small solid circle for the points
    ylab = "Biomass (Mg / ha)",                     # Y label
    xlab = "Day of year",                           # X label
    auto = TRUE,                                    # Add a legend
    grid = TRUE,                                    # Add horizontal and vertical lines
    main = "Sorghum biomass calculated in 2005",    # Add a main title
    xlim = c(204, 206),                             # Specify the X axis limits
    ylim = c(0,6)                                   # Specify the Y axis limits
)

x11()

print(sorghum_plot_v3)

# In this example, we have specified that the units of `Stem`, `Leaf`, and
# `Root` are Mg / ha (mega grams per hectare). You may be wondering: where did
# this information come from?
#
# Unfortunately, determining the units is not very easy since the units are only
# documented within the source code for individual modules. So, to find out the
# units for `Leaf`, we need to first locate a module which uses `Leaf` as an
# input or an output, and then view its source code.
#
# To do this, we begin by using the `get_all_quantities` function, which is part
# of the BioCro library. This function returns a data frame with three columns:
# `quantity_name`, `quantity_type`, and `module_name`. Each row in this data
# frame represents an input or output of a BioCro module.
#
# Once we get information about all the BioCro quantities, we can take a subset
# of them: we just want the rows where the quantity name is `Leaf`. This will
# give us some possible modules to choose from.

all_quantities <- get_all_quantities()

leaf_quantity_subset <-
    all_quantities[which(all_quantities[['quantity_name']] == "Leaf"),]

leaf_modules <- unique(leaf_quantity_subset[['module_name']])

View(leaf_modules)

# Now we can see there are 11 modules that have `Leaf` as an input or output.
# Let's choose one of them: the `total_biomass` module. We can find its source
# code in `src/module_library/total_biomass.h`. (The source code for all modules
# is stored in the `src/module_library` directory.) This is a C++ header file
# that defines the module. Looking through the code, we can find the units for
# `Leaf` specified in a comment.

# In general, looking at the source code is one of the best ways to learn about
# a module. There is also a way to get a list of its inputs and outputs and its
# type (either derivative or steady state). This can be accomplished with the
# `get_module_info` R function, which prints module info to the R terminal. For
# example:
#
#     info <- get_module_info('total_biomass')

# Sometimes it is also helpful to visualize the output of a module by creating a
# "response curve" where we plot one of its output values against one of its
# input values for a reasonable range. Let's try doing this with one of the
# sorghum modules: the `thermal_time_linear` module.
#
# Looking at the source code for this module (in
# `src/module_library/thermal_time_linear`), we can see that it has two input
# quantities: `temp` (representing the air temperature) and `tbase` (the base
# temperature at which development occurs). It has one output quantitiy: `TTc`
# (the thermal time). Since this is a derivative module, the value of the output
# quantity actually represents the rate of change of the thermal time.
#
# Before making a response curve, we should try to determine typical values for
# the inputs to this module.
#
# During a simulation, the air temperature values would come from the weather
# data. To get an idea of the range of temperatures, we could find the min and
# max values from the weather data in 2005 using the following commands:
#
#     min(weather05[['temp']])
#     max(weather05[['temp']])
#
# Doing this, we can see the range is -18.3333 to 36.27778. So we could test the
# range of -20 to 40. On the other hand, the value of `tbase` comes from the
# `sorghum_parameters` list. We can get its value with this command:
#
#    sorghum_parameters[['tbase']]
#
# Its value for sorghum is 0, although it may be different for other crops.
#
# In general, it's possible to determine typical values for a quantity by
# either consulting a crop parameter list such as `sorghum_parameters`, a
# year of weather data such as `weather05`, or possibly the output of a
# simulation such as `sorghum_result`.
#
# With this in mind, let's make a response curve! We'll be using the
# `evaluate_module` function from the BioCro library to do this. This function
# requires a module name and a list of named quantities; it passes the
# quantities to the module which uses them as inputs to calculate its outputs.
# In turn, the outputs are returned as another list with named elements.

# Specify the number of temperature values to test
num_pts <- 101

# Make a vector of temperature values to test
temp_vec <- seq(
    from = -20,
    to = 40,
    length.out = num_pts
)

# Initialize a vector of TTc values (we will fill this in with real data soon)
ttc_vec <- rep(0, times = num_pts)

# Calculate the TTc values
for (i in seq_along(temp_vec)) {
    # Set up the input list to pass to the module
    inputs <- list(
        temp = temp_vec[i], # Use the ith value of temp from the vector
        tbase = 0           # Use the value of tbase from `sorghum_parameters`
    )
    
    # Get the output list
    outputs <- evaluate_module('thermal_time_linear', inputs)
    
    # Store the TTc output value in the vector
    ttc_vec[i] <- outputs[['TTc']]
}

# Plot the derivative of TTc against temp
thermal_time_linear_plot_v1 <- xyplot(
    ttc_vec ~ temp_vec,
    type = 'l',
    xlab = "Temperature (degrees C)",
    ylab = "TTc derivative (degrees C * day / hr)",
    main = "Testing the `thermal_time_linear` module with tbase = 0 deg. C"
)

x11()

print(thermal_time_linear_plot_v1)

# Looking at this plot, we can see that the derivative of TTc is zero when the
# temperature is below the base temperature. In other words, thermal time does
# not accumulate until the temperature exceeds the plant's base temperature.

# We may also be interested in plotting the derivative of TTc as a function of
# temperature for different values of the base temperature. To do this, it will
# be helpful to write a function that calculates values of TTc vs temp for an
# arbitrary value of the base temperature. Here's one way to do it:

ttc_response_curve <- function(tbase) {
    num_pts <- 11

    temp_vec <- seq(
        from = -20,
        to = 40,
        length.out = num_pts
    )
    
    ttc_vec <- rep(0, times = num_pts)

    for (i in seq_along(temp_vec)) {
        inputs <- list(
            temp = temp_vec[i],
            tbase = tbase
        )
        
        outputs <- evaluate_module('thermal_time_linear', inputs)
        
        ttc_vec[i] <- outputs[['TTc']]
    }
    
    # Make a data frame from the temp and TTc vectors, also including a vector
    # that just repeats the value of tbase
    result <- data.frame(
        temp = temp_vec,
        TTc = ttc_vec,
        tbase = tbase
    )
    
    return(result)
}

# As you may notice, this function is based on the code used above to generate
# the plot called `thermal_time_linear_plot_v1`. In fact, it performs the same
# operations. The main difference is that the function produces a data frame
# rather than two vectors. Nevertheless, we could recreate that plot using the
# new function. Here we'll also autmatically generate the plot title based on
# the value of `tbase` using the `paste` command, which combines strings
# together. This makes it easy to update the plot if a different value of
#  `tbase` is used instead of 0.

tbase_for_plot <- 0

plot_title <- paste(
    "Testing the `thermal_time_linear` module with tbase =",
    tbase_for_plot,
    "deg. C"
)

thermal_time_linear_plot_v2 <- xyplot(
    TTc ~ temp,
    data = ttc_response_curve(tbase_for_plot),
    type = 'l',
    xlab = "Temperature (degrees C)",
    ylab = "TTc derivative (degrees C * day / hr)",
    main = plot_title
)

x11()

print(thermal_time_linear_plot_v2)

# The main advantage of wrapping these calculations in a function is that we can
# easily call it multiple times for different values of the base temperature
# without repeating the code for the calculations. Then we can combine the
# output at each value of `tbase` into one big data frame using the `rbind`
# R command, which combines data frames by stacking their rows. Here's one way
# to do it:

# Initialize a data frame with the correct columns but no rows
ttc_response <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(ttc_response) <- c("temp", "TTc", "tbase")

# Choose values of tbase to test
tbase_vec <- seq(from = -10, to = 30, by = 10)

# Calculate all the results
for (i in seq_along(tbase_vec)) {
    ttc_response <- rbind(
        ttc_response, 
        ttc_response_curve(tbase_vec[i])
    )
}

# Now `ttc_response` is a data frame with three columns: temp, TTc, and tbase.
# We can use `xyplot` to plot TTc against temp, dividing the data into multiple
# lines corresponding to the different values of tbase. To do this, we use the
# `group` argument to `xyplot`, which tells `xyplot` how to break the data
# points into groups.

thermal_time_linear_plot_v3 <- xyplot(
    TTc ~ temp,
    data = ttc_response,    # tell xyplot to use columns from the `ttc_response` data frame
    group = tbase,          # tell xyplot to group the data points by their `tbase` values
    type = 'l',
    xlab = "Temperature (degrees C)",
    ylab = "TTc derivative (degrees C * day / hr)",
    main = "Testing the `thermal_time_linear` module with\nmultiple values of tbase (deg. C)",
    grid = TRUE,
    auto = TRUE
)

x11()

print(thermal_time_linear_plot_v3)

# If a plot has been finalized, it may be useful to save it as a PDF instead of
# plotting it directly in an R session. This can be done by calling the `pdf`
# function to create a PDF document, calling the `print` function (whose output
# will be redirected to the PDF instead of the R environment), and then calling
# `dev.off` to close the PDF file, allowing other programs to open it.

pdf(
    file = "thermal_time_linear_plot_v3.pdf",
    width = 6,          # inches
    height = 5,         # inches
    useDingbats = FALSE # make sure symbols are rendered properly in the PDF
)

print(thermal_time_linear_plot_v3)

dev.off()

# Now that we have a PDF version of the graph, we can share it with others or
# finalize its appearance using other software for editing vector graphics such
# as InkScape or Adobe Illustrator.

# So now we've gone through two important examples: running a full simulation
# for a crop and calculating response curves for a module using different
# parameter values. We've also demonstrated how the results can be visualized,
# and how plots can be saved. Along the way, we also discussed a few important
# data types in R.
#
# It's important to note that the techniques shown here can be extended to other
# crops and modules.

# There are also a few other R commands that may be useful but didn't seem to
# fit anywhere else, so I'll put them here.
#
# At any time, it's possible to see all the objects that have been created
# during an R session by using the `ls` command:
#
#     ls()
#
# If you want to clear out your session, you can delete all the objects with the
# `rm` command as follows:
#
#     rm(list=ls())
#
# It is also possible to check a variable's type using the `class` command, for
# example:
#
#     class(c('doy', 'hour', 'Leaf'))
#
# will return "character", meaning that `c('doy', 'hour', 'Leaf')` produces a
# character vector. On the other hand,
#
#     class(sorghum_parameters)
#
# will return "list".
