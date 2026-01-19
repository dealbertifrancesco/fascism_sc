# ==============================================================================
# MASTER (R): Title
# ==============================================================================

# 1. Load/Install requirements
if (!require("here")) install.packages("here")
library(here)
library(whatever)

# 2. Define file paths
dir <- here()
raw_data  <- here("data", "raw")
processed_data <- here("data", "processed")
analysis_data <- here("data", "analysis")
output_figures <- here("output", "figures")
output_tables <- here("output", "tables")
code_path <- here("code")

# 3. Run scripts in order
source(file.path(code_path, "001_data_cleaning.R"))


print("Master R script finished successfully!")
# ==============================================================================
