The files here are designed to be used with BioCro, a model for predicting crop growth given weather conditions.
The following is a blueprint of how to use the files and folders.

"code" directory
These are files mostly in R that work with BioCro and the data files to generate statistics determine the quality of the BioCro model.

consolidate_weather_data was used to consolidate data from World Weather Online and Solcast. World Weather Online does not give a solar or par value, so Solcast was needed.
If only using the weather data for the same growing seasons that we have gathered (listed below under "data" directory), there is no need to run consolidate_weather_data, as it has already been done.
However, if gathering weather data for a new location, one way is to gather it from World Weather Online and Solcast (both take latitude and longitude coordaintes) and run consolidate_weather_data.
The World Weather Online API is easiest to interact with via Jupyter Notebook. The WorldWeatherOnlineAPI.ipynb file can do this.
Solcast gives $650 in free credits to anyone with an academic email address. Historical weather data costs approximately $100 per year per location.

hookejeeves runs the hjkb function in dfoptim to fit parameters to Leaf, Stem, LAI, and yield data. It generates graphs and statistics so that the model fit quality can be evaluated.

run_miscanthus is similar to hookejeeves, but for miscanthus data. In particular, it fits multiple cities of data simultaneously.

generate_stat_graphs generates some final graphs of model quality statistics post-fitting.

"data" directory
Anything with a latitude and longitude is a raw weather file. The ones that say Solcast are from Solcast. The others are from World Weather Online.

Files that say location_weather are files that have been through consolidate_weather_data.

Miscanthus_gigantus_data is raw data for miscanthus, from this paper: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5447773/

The other .txt and .xlsx files are from the TERRA Google Drive folder and are the raw data for sorghum.

miscanthus_stats and sorghum_stats are stats calculated based on running hookejeeves and run_miscanthus on all this data.

"results" directory
For the results with graphs of yield and other BioCro values, every set of graphs is also accompanied by 1 or 2 .txt files.
For miscanthus, every run started with the default parameters, so only the resulting parameters were written to a file.
For sorghum, pre and post fit parameters were written to files (the post ones say "fitted").
These files can be read back into R to run BioCro with any of these sets of parameters. R automatically puts a "1" at the beginning of line 2, so occasionally this will need to be deleted from the file before importing to R.
There are some lines of code in hookejeeves that change the dataframe read from these files into a named list, which is the format BioCro requires for parameters.

"miscanthus" has results 6 runs of run_miscanthus, each leaving out one of the 6 European cities for which we have data.
Ad - Adana
Ab - Aberystwyth
M - Moscow
P - Potash
S - Stuttgart
W - Wageningen
Each run has 2 printed pdf graphs, one of the training set (5 cities), and one for the validation set (the 6th city).

"miscanthus_final_yield_only" is the same as "miscanthus," except run only to fit harvest yield values.

Miscanthus was fit to minimize the Chi-Squared statistic, which resulted in the best fit.

Sorghum was fit to minimize a normed Mahalanobis statistic, the formula for which is in final_report.pdf. This resulted in the best fit for sorghum, which was more challenging due to issues of variance and scale.

"sorghum_mahalanobis" fits to 2017 all data.
"sorghum_mahalanobis_2018_take_2" fits to 2018 all data.
"sorghum_2017_final_yield_only" fits to 2017 final yield data only.
"sorghum_2018_final_yield_only" fits to 2018 final yield data only.

The stats_graphs folders are various versions of graphs generated using miscanthus_stats and sorghum_stats files (which were compiled by calculating the stats on all of these fits).

For any questions, please contact starkledbetter@gmail.com.
