# Mar.utils NEWS

## Mar.utils 2.0.0

### BREAKING CHANGES

This release contains changes that will likely break existing scripts.

#### Database Connection Functions

* **NEW REQUIREMENT**: Almost all functions that used to accept Oracle credentials no longer do.  Instead, an existing Oracle connections must be passed via a `cxn` parameter
* **WHY**: Improves troubleshooting by separating connection issues from function issues
* **MIGRATION**: 
  ```r
  cxn <- ROracle::dbConnect(DBI::dbDriver("Oracle"), <"oracle.username">", <"oracle.password">", "PTRAN")
  get_data_tables(schema = "MARFISSCI", tables = "MY_TABLE", cxn = cxn)
  ```

#### Data Storage Security

* **NEW**: `save_encrypted()` and `load_encrypted()` functions for Protected B data
* **ENHANCED**: Functions that save Protected B data now use encryption by default
* **WHY**: Ensures proper handling of sensitive data following DFO security protocols

#### Deprecated Parameters

* **REMOVED**: Some common parameters can no longer be used (.g. `data.dir`, `usepkg`, `fn.oracle.username`, `fn.oracle.password`, `fn.oracle.dsn`)
* **MIGRATION**: Update code to use current parameter names (see function documentation)

### What You Need to Do

1. **New projects**: Create an oracle connection prior and pass it as `cxn` to relevant functions (rather than passing your oracle credentials)
2. **Sensitive data**: Review data storage practices and use encrypted options when appropriate

### Need Help?

* Check the updated README.md for migration examples
* Contact mike.mcmahon@dfo-mpo.gc.ca for assistance
* Report issues at https://github.com/Maritimes/Mar.utils/issues

---

## Mar.utils 2019.12.19

### IMPROVEMENTS

* Changed 'quiet' parameter to 'quietly' for consistency with other Mar.* functions

---

## Mar.utils 2019.12.10

### NEW FEATURES

* Adds option to prevent status message output from `make_oracle_cxn()`

---

## Mar.utils 2019.07.29

### IMPROVEMENTS

* Removes @importFrom from all documentation so external functions aren't exported

---

## Mar.utils 2019.07.04

### NEW FEATURES

* Adds `total_fishing_picture()`

---

## Mar.utils 2019.06.27

### IMPROVEMENTS

* Vast improvement to `VMS_clean_recs()`
* Improves `make_segments()`

---

## Mar.utils 2019.06.24

### NEW FEATURES

* Adds `VMS_clean_recs()`

### IMPROVEMENTS

* Moves data files to external repos (Mar.data)

---

## Mar.utils 2019.06.07

### IMPROVEMENTS

* Replaced `drop_NA_cols()` with much more flexible `drop_cols()`

---

## Mar.utils 2019.03.28

### NEW FEATURES

* Added hex data

---

## Mar.utils 2019.03.12

### NEW FEATURES

* Added `updateCheck()` function

---

## Mar.utils 2018.04.06

### NEW FEATURES

* Added method for extracting VMS data by area
* Added function for plotting line segments