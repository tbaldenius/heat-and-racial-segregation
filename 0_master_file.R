### MASTER FILE                   ###

# Insert path to folder "Replication" here
path <- "/Volumes/Till1TB/Master_Thesis/Replication"
setwd(path)

### Required packages             ###

main_pkgs <- c(
  "data.table", # Faster data manipulation using multiple cores, 
                #   watch out that installation 
                #   is successful, if on Mac consult
                #   https://github.com/Rdatatable/data.table/wiki/Installation
                #   or for a recent fix
                #   https://github.com/Rdatatable/data.table/issues/5419
  
  "fst",        # Efficient writing and reading of tabular data
  
  "raster",     # Multidimensional data (especially useful for spatial data)
  
  "tmap",       # Visualize maps
  
  "fixest",     # Multiple core/C++ fixest estimation
  
  "tidyverse"   # tidyr, dplyr: 
                #   Easily interpretable data cleaning and manipulation
                # ggplot:
                #   Visualization
                # + many other useful packages
)

other_pkgs <- c(
  "readxl",
  "foreign",
  "rjson",
  "prism",
  "weathermetrics",
  "lubridate",
  "rgdal",
  "SpaDES",
  "sf",
  "parallel",
  "fasterize",
  "plyr",
  "Matrix",
  "Rcpp",
  "RcppArmadillo",
  "MetricsWeighted"
)

# Install if necessary
not_installed <- c(
  main_pkgs[!(main_pkgs %in% installed.packages()[,"Package"])],
  other_pkgs[!(other_pkgs %in% installed.packages()[,"Package"])]
)
if(length(not_installed)) install.packages(not_installed)

# Load frequently used packages
invisible(lapply(
  main_pkgs,
  function(p) require(p, character.only = TRUE)
))

# Settings
theme_set(theme_bw(base_size = 14))     # For plotting 
options(dplyr.summarise.inform = FALSE) # Hide unneeded messages
options(scipen = 999999)                # No scientific notation 
                                        #   (like 1e+06) on plots

### Scripts (chronological order) ###

# Comment: Theoretically, all of 
#  these can be run from here,
#  but it might be more sensible
#  to open the files individually.
#  Some of them take require a lot
#  of computational time.
#
# All of the datasets to be created
#  in the process are already 
#  present in the Processed_Data
#  folder. Should you wish to
#  re-create everything, there is
#  a Processed_Data_copy folder
#  to guard against potential data
#  loss.
#
# Furthermore, it might be
#  faster to run this outside
#  of an IDE, as it was the
#  case for me

### SafeGraph Data and visit      ###
###   isolation index estimation  ###

# 1: Create dataset that shows which weeks     
#     are contained in which parts of the data  
source("Code/1_create_date_table.R")

# 2: Create each two weekly datasets
#     1) selected POI variables:
#         "select"
#     2) visitor home CBG distribution
#         at each POI: "long"
#     for each MSAs and UAs
source("Code/2_create_select_long.R")

# 3: Create weekly data from the
#     home_panel_summary files,  
#     these include number of 
#     devices observed in a week
#     by CBG
source("Code/3_create_home_panel_data.R")

# 4: Explore NAICS codes to 
#     eventually create three
#     files for each outdoor
#     leisure POI, indoor
#     leisure POI, and 
#     grocery stores
source("Code/4_explore_naics_codes.R")

# 5: Calculate weekly visit 
#     isolation index estimates
#     for each MSAs, UAs, and
#     by NAICS-realted subsets
source("Code/5_create_indices.R")

### Everything weather-           ###
###                  related      ###

# 6: Download weather data.
#     Can be skipped, data
#     is already there
download_again <- FALSE
if(download_again) source("Code/6_download_weather_data.R")

# 7: Create heat variables,
#     that is, compute average
#     temperature, the heat index
#     (Steadman 1979), and the
#     heat wave index (Miller et al.
#     2021) from PRISM data
source("Code/7_create_heat_vars.R")

# 8: Aggregate weather data
#     spatially (to MSA/UA
#     level) both as binned
#     weather variables and
#     as unaltered weather
#     variables
source("Code/8_aggregate_weather_data.R")

# 9: Produce all figures
#     and save them to the 
#     figures folder, as well
#     as data for one table.
source("Code/9_figures_and_results.R")


