# Mar.utils

<!-- badges: start -->
![R Package](https://img.shields.io/badge/R-package-blue)
![GitHub last commit](https://img.shields.io/github/last-commit/Maritimes/Mar.utils)
<!-- badges: end -->

**BREAKING CHANGES in Version 2.0.0**️

**Major changes that will impact some users:**

1. **Oracle connections** - Some functions now require existing oracle connections (`cxn`) instead of username/password
2. **Data encryption** - Protected B data is now encrypted when extracted
3. **Deprecated parameters** - Some older parameter names removed (see migration guide)

**Why these changes?** See [NEWS.md](NEWS.md) for detailed explanation.

---

## Overview

This is a comprehensive suite of utility functions used by various Maritimes R packages and useful for general fisheries data analysis. This package contains functions that are both useful standalone or were developed to support other packages in the Maritimes ecosystem.

**Key capabilities:**

- Database extraction and connection management
- Spatial data processing and analysis
- VMS data cleaning and analysis  
- Privacy assessment and data aggregation
- File management and data export
- General utilities for data manipulation

## Installation

```r
# Install from GitHub
library(devtools)
install_github('Maritimes/Mar.utils')
```

## Quick Start

```r
library(Mar.utils)

# Connect to Oracle database
cxn <- ROracle::dbConnect(DBI::dbDriver("Oracle"), <"oracle.username">", <"oracle.password">", "PTRAN")

# Extract custom tables
get_data_tables(schema = "MARFISSCI", 
                tables = c("MARFLEETS_LIC", "NAFO_UNIT_AREAS"),
                cxn = cxn)

# Work with spatial data
my_sf <- df_to_sf(my_dataframe, type = "points")
clipped_data <- clip_by_poly(my_sf, clip.poly = "study_area.shp")
```

## Function Categories

### Database Extraction
**Extract data from DFO Oracle databases**

- `get_data_tables()` - Extract specific tables from any schema
- `make_oracle_cxn()` - Create Oracle database connections
- `connectionCheck()` - Validate connection types
- `deprecationCheck()` - Check for deprecated parameters

```r
# Extract custom tables
cxn <- ROracle::dbConnect(DBI::dbDriver("Oracle"), <"oracle.username">", <"oracle.password">", "PTRAN")
get_data_tables(schema = "MARFISSCI", tables = "MARFLEETS_LIC", cxn = cxn)
```

### Spatial Analysis
**Process and analyze spatial fisheries data**

- `df_to_sf()` - Convert dataframes to spatial objects (points, lines, polygons)
- `clip_by_poly()` - Clip data to geographic areas
- `identify_area()` - Determine which area/zone points fall within
- `make_segments()` - Create line segments from coordinate tracks
- `make_segments_isdb()` - Specialized segments for ISDB fishing sets
- `df_qc_spatial()` - Quality control spatial coordinates
- `df_sf_to_gpkg()` - Export spatial data to GeoPackage format
- `gpkglayers_to_shapefiles()` - Convert GeoPackage layers to shapefiles
- `prepare_shape_fields()` - Prepare field names for shapefile export

```r
# Create fishing tracks from VMS data
tracks <- make_segments(vms_data, objField = "VR_NUMBER", 
                        seqField = "POSITION_UTC_DATE")

# Identify NAFO areas for fishing sets
sets_with_nafo <- identify_area(fishing_sets, 
                                agg.poly.field = "NAFO")
```

### Privacy & Data Aggregation
**Assess and aggregate sensitive fisheries data following DFO protocols**

- `assess_privacy()` - Apply Rule of 5 and aggregate sensitive data
- `aggregator()` - Aggregate coordinates into gridded summaries  
- `plot_hex_data()` - Visualize aggregated hex/grid data

```r
# Aggregate sensitive commercial data following Rule of 5
results <- assess_privacy(df = commercial_data,
                         sens.fields = c("LICENCE_ID", "VR_NUMBER"),
                         agg.fields = c("KEPT_WT", "DISCARD_WT"))
```

** For detailed privacy workflows, see:** `vignette("assess_privacy", package = "Mar.utils")`

### VMS Data Processing
**Clean and analyze Vessel Monitoring System data**

- `VMS_get_recs()` - Extract VMS records from database
- `VMS_clean_recs()` - Clean and filter VMS tracks
- `VMS_from_MARFIS()` - Match VMS data with MARFIS fishing activity
- `subset_by_time()` - Filter records by minimum time intervals

```r
# Extract and clean VMS data
raw_vms <- VMS_get_recs(cxn = cxn, dateStart = "2023-01-01", 
                        vrnList = c("12345", "67890"))
clean_vms <- VMS_clean_recs(raw_vms, minDist_m = 100, maxBreak_mins = 1440)
```

### Data Manipulation
**Transform and clean fisheries datasets**

- `dets_defuddler()` - Convert MARFIS detail tables from long to wide format
- `DDMMx_to_DD()` - Convert coordinates from DDMM format to decimal degrees
- `ISSETPROFILE_enwidener()` - Reshape ISDB set profile data  
- `rename_fields()` - Rename dataframe columns
- `drop_cols()` - Remove columns based on uniformity criteria
- `clean_dfo_fields()` - Remove standard DFO housekeeping fields

```r
# Convert MARFIS details to usable format
wide_data <- dets_defuddler(marfName = "LOG_EFRT_ENTRD_DETS", 
                           df = effort_details)

# Convert coordinates
coords_df <- DDMMx_to_DD(my_data, format = "DDMMMM", 
                        lat.field = "LAT", lon.field = "LON")
```

### File Management
**Secure data storage and file operations**

- `save_encrypted()` - Save data files with encryption for Protected B data
- `load_encrypted()` - Load encrypted data files
- `data.dir_valet()` - Clean up and organize data directories

```r
# Save sensitive data securely
save_encrypted(sensitive_data, file = "protected_data.RData", encrypt = TRUE)

# Load data extracted by another user/computer
load_encrypted("protected_data.RData", 
               extract_user = "original_user", 
               extract_computer = "COMPUTER123")
```

### General Utilities
**Helper functions for data analysis**

- `big_in()` - Create SQL IN statements for large vectors (>1000 items)
- `SQL_in()` - Convert vectors to SQL IN format
- `Mode()` - Calculate statistical mode
- `st_err()` - Calculate standard error
- `combine_lists()` - Merge lists with conflict resolution
- `updateCheck()` - Check for package updates on GitHub

### Debugging & QC
**Tools for troubleshooting and quality control**

- `changeDetector()` - Track changes in dataframes during processing
- `where_now()` - Display current function location for debugging
- `updateExpected()` - Track expected values through processing steps

### Date & Time
**Handle temporal data consistently**

- `vali_dates()` - Validate and standardize date inputs
- `simple_date()` - Convert datetime fields to simple dates

### Survey Tools
**Specialized functions for survey design**

- `set_select()` - Generate randomized survey station locations

## Migration Guide (Breaking Changes)

**Functions with new requirements:**

```r
# OLD - some functions accepted credentials directly
get_data_tables(schema = "MARFISSCI", tables = "MARFLEETS_LIC", username = "user", password = "pass", dsn = "PTRAN, usepkg='roracle")

# NEW - cleaner connection establishment 
cxn <- ROracle::dbConnect(DBI::dbDriver("Oracle"), <"oracle.username">", <"oracle.password">", "PTRAN")
get_data_tables(schema = "MARFISSCI", tables = "MARFLEETS_LIC", cxn = cxn)
```
## Related Packages

- [Mar.datawrangling](https://github.com/Maritimes/Mar.datawrangling) - Database extraction and filtering
- [Mar.fleets](https://github.com/Maritimes/Mar.fleets) - Fleet analysis tools
- [Mar.data](https://github.com/Maritimes/Mar.data) - Reference spatial datasets

## Getting Help

- Check [NEWS.md](NEWS.md) for recent changes
- Browse function documentation: `?function_name`
- View vignettes: `browseVignettes("Mar.utils")`
- Contact: mike.mcmahon@dfo-mpo.gc.ca
- Report issues: [GitHub Issues](https://github.com/Maritimes/Mar.utils/issues)

---

*A suite of functions used by various Maritimes packages - Version 2.0.0*