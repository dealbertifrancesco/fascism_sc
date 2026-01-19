* ==============================================================================
* MASTER (Stata): Title
* ==============================================================================
clear all
macro drop _all
set more off

* 1. Set wd
cd ".."

display "Project root set to: `c(pwd)'"

* 2. Define globals for paths
global data_dir     "data"
global raw_dir      "$data_dir/raw"
global processed_dir "$data_dir/processed"
global analysis_dir  "$data_dir/analysis"
global output_dir   "output"
global figures_dir  "$output_dir/figures"
global tables_dir   "$output_dir/tables"
global code_dir     "code"

* 3. Run scripts in order
do "$code_dir/001_clean_data.do"


display "Master Stata do-file finished successfully!"
* ==============================================================================