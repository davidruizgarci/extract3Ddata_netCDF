# extract3Ddata_netCDF

R functions to extract environmental values from 2D and 3D netCDF files to point-based observations, supporting surface, bottom, nearest-depth and combined extraction workflows.

# Features

- Extract values from:
  - 2D netCDF variables
  - 3D netCDF variables
  - 4D netCDF variables including time

- Extraction methods:
  - Surface layer
  - Bottom available layer
  - Nearest valid depth layer
  - Combined extraction

- Supports:
  - Single netCDF files
  - Multiple netCDF files
  - Lists of netCDF files
  - Opened `ncdf4` objects
  - Data frames containing file paths

- Automatically:
  - Detects dimension names
  - Handles different longitude conventions
  - Reads netCDF time metadata
  - Matches nearest spatial and temporal cells
  - Detects variables automatically when `var = NULL`

---

# Main functions

| Function | Description |
|---|---|
| `extract2d()` | Extract values from 2D netCDF variables |
| `extract3d_surface()` | Extract the surface layer |
| `extract3d_bottom()` | Extract the deepest valid layer |
| `extract3d_nearest()` | Extract the nearest valid depth layer |
| `extract3d_all()` | Extract combined surface, nearest and bottom values |
| `extract_netcdf()` | General extraction interface |

---

# Installation

Currently under development.

Clone the repository:

```r
git clone https://github.com/your_username/extractNetCDF.git
```

Or source the functions directly:

```r
source("extract_netcdf_package.R")
```

---

# Dependencies

```r
ncdf4
```

Optional:

```r
dplyr
```


---

# Important notes

## Nearest-depth extraction

`extract3d_nearest()` extracts the closest valid non-missing depth layer available in the vertical profile.

This behaviour is important in oceanographic products where some depth layers may be missing due to bathymetry.

---

## Bottom extraction

`extract3d_bottom()` extracts the deepest non-missing value available in the profile rather than simply the deepest nominal depth layer.

---

## Automatic variable detection

If `var = NULL`, the package automatically detects the variable stored in the netCDF file.

This works when the file contains a single environmental variable.

If a netCDF file contains several variables, the user must specify `var` manually.

---

# Input requirements

Observation tables should contain at minimum:

| Column | Required |
|---|---|
| Longitude | Yes |
| Latitude | Yes |
| Date | Only if netCDF contains time |
| Depth | Only for nearest-depth extraction |

---

# Supported netCDF inputs

The `nc` argument accepts:

```r
# One file
nc = "file.nc"

# Several files
nc = c("file1.nc", "file2.nc")

# List of files
nc = list("file1.nc", "file2.nc")

# Opened ncdf4 object
nc = ncdf4::nc_open("file.nc")

# List of opened objects
nc = list(nc1, nc2)

# Data frame with file column
nc = data.frame(file = c("file1.nc", "file2.nc"))
```

---

# Future developments

Potential future extensions include:

- Parallel extraction
- Polygon-based extraction
- Interpolation methods

---

# Author

David Ruiz García

---

# License

MIT License
