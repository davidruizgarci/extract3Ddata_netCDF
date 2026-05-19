# ------------------------------------------------------------------------------
# 1_test_extract_netcdf
# Test netCDF extraction functions with automatic variable detection
# ------------------------------------------------------------------------------

# This script tests the netCDF extraction functions using two 3D/4D example files:
#   - NPPV_Reanalysis: variable `nppv`
#   - SAL_Reanalysis: variable `so`
#
# The key goal is to test `var = NULL`. In this mode, the package detects the
# variable automatically from each netCDF file. This is useful when each file
# contains one single environmental variable.
#
# The script tests the following netCDF input formats:
#   - one file path
#   - several file paths as a character vector
#   - list of file paths
#   - opened ncdf4 object
#   - list of opened ncdf4 objects
#   - data frame with a file column
#
# NOTE:
# `extract2d()` is not tested here because this simplified trial uses only 3D/4D
# files with a depth dimension. To test `extract2d()`, use a true 2D netCDF file.

# ------------------------------------------------------------------------------
# 0. Load required packages -----------------------------------------------------
# ------------------------------------------------------------------------------

library(dplyr)
library(ncdf4)


# ------------------------------------------------------------------------------
# 1. Load the observation dataset ----------------------------------------------
# ------------------------------------------------------------------------------

if (!file.exists(file_env_2d)) {
  stop("Input file not found: ", file_env_2d, call. = FALSE)
}

data <- read.csv(file_env_2d, sep = ";", stringsAsFactors = FALSE)

summary(data)
head(data)


# ------------------------------------------------------------------------------
# 2. Prepare one row per unique sampling event ---------------------------------
# ------------------------------------------------------------------------------

data_tows <- data %>%
  distinct(code, .keep_all = TRUE)


# ------------------------------------------------------------------------------
# 3. Standardise date and coordinate columns -----------------------------------
# ------------------------------------------------------------------------------

data_tows <- data_tows %>%
  mutate(
    date  = as.Date(date),
    lon   = as.numeric(gsub(",", ".", lon)),
    lat   = as.numeric(gsub(",", ".", lat)),
    depth = as.numeric(gsub(",", ".", depth)),
    Year  = format(date, "%Y"),
    Month = format(date, "%m"),
    Day   = format(date, "%d")
  )

range(data_tows$date, na.rm = TRUE)
range(data_tows$lon, na.rm = TRUE)
range(data_tows$lat, na.rm = TRUE)
range(data_tows$depth, na.rm = TRUE)


# ------------------------------------------------------------------------------
# 4. Define the netCDF files used in the tests ---------------------------------
# ------------------------------------------------------------------------------

nc_file1 <- paste0(
  netcdf_data,
  "/MEDSEA_MULTIYEAR_BGC_006_008/med-ogs-bio-rean-d/NPPV_Reanalysis/2020/06/18/NPPV_Reanalysis_2020-06-18.nc"
)

nc_file3 <- paste0(
  netcdf_data,
  "/MEDSEA_MULTIYEAR_PHY_006_004/med-cmcc-sal-rean-d/SAL_Reanalysis/2020/06/18/SAL_Reanalysis_2020-06-18.nc"
)

file.exists(nc_file1)
file.exists(nc_file3)

if (!all(file.exists(c(nc_file1, nc_file3)))) {
  stop("At least one test netCDF file does not exist. Check `netcdf_data` and file paths.", call. = FALSE)
}


# ------------------------------------------------------------------------------
# 5. Inspect variables and dimensions ------------------------------------------
# ------------------------------------------------------------------------------

nc_test1 <- ncdf4::nc_open(nc_file1)
names(nc_test1$var)
names(nc_test1$dim)
ncdf4::nc_close(nc_test1)

nc_test3 <- ncdf4::nc_open(nc_file3)
names(nc_test3$var)
names(nc_test3$dim)
ncdf4::nc_close(nc_test3)


# ------------------------------------------------------------------------------
# 6. Prepare a small test dataset ----------------------------------------------
# ------------------------------------------------------------------------------

data_test <- data_tows %>%
  filter(date == as.Date("2020-06-18"))

# Fallback option if needed:
# if (nrow(data_test) == 0) {
#   data_test <- data_tows %>%
#     slice_head(n = 10) %>%
#     mutate(date = as.Date("2020-06-18"))
# }

nrow(data_test)
head(data_test)

if (nrow(data_test) == 0) {
  stop("No rows available for 2020-06-18. Use the fallback block in section 6.", call. = FALSE)
}


# ------------------------------------------------------------------------------
# 7. Helper function to summarise test outputs ---------------------------------
# ------------------------------------------------------------------------------

check_output <- function(x, cols) {
  print(head(x))
  for (cl in cols) {
    if (cl %in% names(x)) {
      print(cl)
      print(summary(x[[cl]]))
    } else {
      warning("Column not found in output: ", cl, call. = FALSE)
    }
  }
  invisible(x)
}


# ==============================================================================
# PART A. TEST GENERIC extract_netcdf() WITH var = NULL
# ==============================================================================

# ------------------------------------------------------------------------------
# A1. One file path -------------------------------------------------------------
# ------------------------------------------------------------------------------
# With method = "bottom" and var = NULL, the output column is automatically named:
#   seabottom_<detected_variable>

test_generic_one_file_bottom <- extract_netcdf(
  data = data_test,
  nc = nc_file1,
  var = NULL,
  method = "bottom",
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date"
)

check_output(test_generic_one_file_bottom, "seabottom_nppv")


# ------------------------------------------------------------------------------
# A2. Several files as a character vector ---------------------------------------
# ------------------------------------------------------------------------------
# Here the package should detect:
#   nc_file1 -> nppv
#   nc_file3 -> so
#
# The output should contain:
#   seabottom_nppv
#   seabottom_so

test_generic_vector_bottom <- extract_netcdf(
  data = data_test,
  nc = c(nc_file1, nc_file3),
  var = NULL,
  method = "bottom",
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date"
)

check_output(test_generic_vector_bottom, c("seabottom_nppv", "seabottom_so"))


# ------------------------------------------------------------------------------
# A3. List of file paths --------------------------------------------------------
# ------------------------------------------------------------------------------

test_generic_list_bottom <- extract_netcdf(
  data = data_test,
  nc = list(nc_file1, nc_file3),
  var = NULL,
  method = "bottom",
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date"
)

check_output(test_generic_list_bottom, c("seabottom_nppv", "seabottom_so"))


# ------------------------------------------------------------------------------
# A4. One opened ncdf4 object ---------------------------------------------------
# ------------------------------------------------------------------------------

nc1 <- ncdf4::nc_open(nc_file1)

test_generic_opened_bottom <- extract_netcdf(
  data = data_test,
  nc = nc1,
  var = NULL,
  method = "bottom",
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date"
)

check_output(test_generic_opened_bottom, "seabottom_nppv")

ncdf4::nc_close(nc1)


# ------------------------------------------------------------------------------
# A5. List of opened ncdf4 objects ---------------------------------------------
# ------------------------------------------------------------------------------

nc1 <- ncdf4::nc_open(nc_file1)
nc3 <- ncdf4::nc_open(nc_file3)

test_generic_list_opened_bottom <- extract_netcdf(
  data = data_test,
  nc = list(nc1, nc3),
  var = NULL,
  method = "bottom",
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date"
)

check_output(test_generic_list_opened_bottom, c("seabottom_nppv", "seabottom_so"))

ncdf4::nc_close(nc1)
ncdf4::nc_close(nc3)


# ------------------------------------------------------------------------------
# A6. Data frame with file column ----------------------------------------------
# ------------------------------------------------------------------------------

nc_df <- data.frame(
  file = c(nc_file1, nc_file3),
  stringsAsFactors = FALSE
)

test_generic_df_bottom <- extract_netcdf(
  data = data_test,
  nc = nc_df,
  var = NULL,
  method = "bottom",
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date",
  file_col = "file"
)

check_output(test_generic_df_bottom, c("seabottom_nppv", "seabottom_so"))


# ==============================================================================
# PART B. TEST WRAPPER FUNCTIONS WITH ONE FILE AND var = NULL
# ==============================================================================

# ------------------------------------------------------------------------------
# B1. extract3d_surface() -------------------------------------------------------
# ------------------------------------------------------------------------------

test_surface <- extract3d_surface(
  data = data_test,
  nc = nc_file1,
  var = NULL,
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date"
)

check_output(test_surface, "surface_nppv")


# ------------------------------------------------------------------------------
# B2. extract3d_bottom() --------------------------------------------------------
# ------------------------------------------------------------------------------

test_bottom <- extract3d_bottom(
  data = data_test,
  nc = nc_file1,
  var = NULL,
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date"
)

check_output(test_bottom, "seabottom_nppv")


# ------------------------------------------------------------------------------
# B3. extract3d_nearest() -------------------------------------------------------
# ------------------------------------------------------------------------------

test_nearest <- extract3d_nearest(
  data = data_test,
  nc = nc_file1,
  var = NULL,
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date",
  depth_col = "depth"
)

check_output(test_nearest, "nearest_nppv")


# ------------------------------------------------------------------------------
# B4. extract3d_all() -----------------------------------------------------------
# ------------------------------------------------------------------------------
# With method = "all", the function returns three summary columns:
#   surface_<detected_variable>
#   nearest_<detected_variable>
#   seabottom_<detected_variable>

test_all <- extract3d_all(
  data = data_test,
  nc = nc_file1,
  var = NULL,
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date",
  depth_col = "depth"
)

check_output(test_all, c("surface_nppv", "nearest_nppv", "seabottom_nppv"))


# ==============================================================================
# PART C. TEST 3D WRAPPERS WITH ALL NETCDF INPUT FORMATS AND var = NULL
# ==============================================================================

# ------------------------------------------------------------------------------
# C1. extract3d_surface() -------------------------------------------------------
# ------------------------------------------------------------------------------

test_surface_vector <- extract3d_surface(
  data = data_test,
  nc = c(nc_file1, nc_file3),
  var = NULL,
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date"
)
check_output(test_surface_vector, c("surface_nppv", "surface_so"))

test_surface_list <- extract3d_surface(
  data = data_test,
  nc = list(nc_file1, nc_file3),
  var = NULL,
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date"
)
check_output(test_surface_list, c("surface_nppv", "surface_so"))

nc1 <- ncdf4::nc_open(nc_file1)
nc3 <- ncdf4::nc_open(nc_file3)
test_surface_list_opened <- extract3d_surface(
  data = data_test,
  nc = list(nc1, nc3),
  var = NULL,
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date"
)
check_output(test_surface_list_opened, c("surface_nppv", "surface_so"))
ncdf4::nc_close(nc1)
ncdf4::nc_close(nc3)

nc_df_3d <- data.frame(file = c(nc_file1, nc_file3), stringsAsFactors = FALSE)
test_surface_df <- extract3d_surface(
  data = data_test,
  nc = nc_df_3d,
  var = NULL,
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date",
  file_col = "file"
)
check_output(test_surface_df, c("surface_nppv", "surface_so"))


# ------------------------------------------------------------------------------
# C2. extract3d_bottom() --------------------------------------------------------
# ------------------------------------------------------------------------------

test_bottom_vector <- extract3d_bottom(
  data = data_test,
  nc = c(nc_file1, nc_file3),
  var = NULL,
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date"
)
check_output(test_bottom_vector, c("seabottom_nppv", "seabottom_so"))

test_bottom_list <- extract3d_bottom(
  data = data_test,
  nc = list(nc_file1, nc_file3),
  var = NULL,
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date"
)
check_output(test_bottom_list, c("seabottom_nppv", "seabottom_so"))

nc1 <- ncdf4::nc_open(nc_file1)
nc3 <- ncdf4::nc_open(nc_file3)
test_bottom_list_opened <- extract3d_bottom(
  data = data_test,
  nc = list(nc1, nc3),
  var = NULL,
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date"
)
check_output(test_bottom_list_opened, c("seabottom_nppv", "seabottom_so"))
ncdf4::nc_close(nc1)
ncdf4::nc_close(nc3)

nc_df_3d <- data.frame(file = c(nc_file1, nc_file3), stringsAsFactors = FALSE)
test_bottom_df <- extract3d_bottom(
  data = data_test,
  nc = nc_df_3d,
  var = NULL,
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date",
  file_col = "file"
)
check_output(test_bottom_df, c("seabottom_nppv", "seabottom_so"))


# ------------------------------------------------------------------------------
# C3. extract3d_nearest() -------------------------------------------------------
# ------------------------------------------------------------------------------

test_nearest_vector <- extract3d_nearest(
  data = data_test,
  nc = c(nc_file1, nc_file3),
  var = NULL,
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date",
  depth_col = "depth"
)
check_output(test_nearest_vector, c("nearest_nppv", "nearest_so"))

test_nearest_list <- extract3d_nearest(
  data = data_test,
  nc = list(nc_file1, nc_file3),
  var = NULL,
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date",
  depth_col = "depth"
)
check_output(test_nearest_list, c("nearest_nppv", "nearest_so"))

nc1 <- ncdf4::nc_open(nc_file1)
nc3 <- ncdf4::nc_open(nc_file3)
test_nearest_list_opened <- extract3d_nearest(
  data = data_test,
  nc = list(nc1, nc3),
  var = NULL,
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date",
  depth_col = "depth"
)
check_output(test_nearest_list_opened, c("nearest_nppv", "nearest_so"))
ncdf4::nc_close(nc1)
ncdf4::nc_close(nc3)

nc_df_3d <- data.frame(file = c(nc_file1, nc_file3), stringsAsFactors = FALSE)
test_nearest_df <- extract3d_nearest(
  data = data_test,
  nc = nc_df_3d,
  var = NULL,
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date",
  depth_col = "depth",
  file_col = "file"
)
check_output(test_nearest_df, c("nearest_nppv", "nearest_so"))


# ------------------------------------------------------------------------------
# C4. extract3d_all() -----------------------------------------------------------
# ------------------------------------------------------------------------------

test_all_vector <- extract3d_all(
  data = data_test,
  nc = c(nc_file1, nc_file3),
  var = NULL,
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date",
  depth_col = "depth"
)
check_output(
  test_all_vector,
  c("surface_nppv", "nearest_nppv", "seabottom_nppv",
    "surface_so", "nearest_so", "seabottom_so")
)

test_all_list <- extract3d_all(
  data = data_test,
  nc = list(nc_file1, nc_file3),
  var = NULL,
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date",
  depth_col = "depth"
)
check_output(
  test_all_list,
  c("surface_nppv", "nearest_nppv", "seabottom_nppv",
    "surface_so", "nearest_so", "seabottom_so")
)

nc1 <- ncdf4::nc_open(nc_file1)
nc3 <- ncdf4::nc_open(nc_file3)
test_all_list_opened <- extract3d_all(
  data = data_test,
  nc = list(nc1, nc3),
  var = NULL,
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date",
  depth_col = "depth"
)
check_output(
  test_all_list_opened,
  c("surface_nppv", "nearest_nppv", "seabottom_nppv",
    "surface_so", "nearest_so", "seabottom_so")
)
ncdf4::nc_close(nc1)
ncdf4::nc_close(nc3)

nc_df_3d <- data.frame(file = c(nc_file1, nc_file3), stringsAsFactors = FALSE)
test_all_df <- extract3d_all(
  data = data_test,
  nc = nc_df_3d,
  var = NULL,
  lon_col = "lon",
  lat_col = "lat",
  date_col = "date",
  depth_col = "depth",
  file_col = "file"
)
check_output(
  test_all_df,
  c("surface_nppv", "nearest_nppv", "seabottom_nppv",
    "surface_so", "nearest_so", "seabottom_so")
)
