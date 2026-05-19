# ------------------------------------------------------------------------------
# 0_setup.R
# ------------------------------------------------------------------------------

# 1. Define the computer / working environment ---------------------------------
cpu <- "david"

if (cpu == "david") {main_dir <- "C:/Users/david/SML Dropbox/gitdata/EggCase_Distribution"}

# Stop early if the selected project folder does not exist. This avoids creating
# files in the wrong working directory by accident.
if (!dir.exists(main_dir)) {stop("Project folder does not exist: ", main_dir, call. = FALSE)}

setwd(main_dir)


# 2. Define project folders -----------------------------------------------------
# These are the main folders used throughout the project. They are created if
# they do not already exist, so the rest of the workflow can use them safely.

input_data  <- file.path(main_dir, "input")
temp_data   <- file.path(main_dir, "temp")
output_data <- file.path(main_dir, "output")
script_data <- file.path(main_dir, "R")

for (x in c(input_data, temp_data, output_data, script_data)) {
  if (!dir.exists(x)) dir.create(x, recursive = TRUE)}

# 3. Define specific input folders ---------------------------------------------
# `netcdf_data` is the folder where the environmental netCDF files are stored.
# The name is deliberately generic because the extraction functions can work with
# any compatible netCDF file, independently of where it comes from.

netcdf_data <- file.path(input_data, "cmems")

# 4. Define common input and output files --------------------------------------
# Keeping file names here makes the analysis scripts shorter and easier to adapt.

file_env_2d <- file.path(temp_data, "env_data2D.csv")


# 5. Load packages --------------------------------------------------------------
library(dplyr)
library(ncdf4)
library(raster)

