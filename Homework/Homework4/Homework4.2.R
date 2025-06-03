# Project Info (Homeworks from Advanced Macroeconomics 1 - PIMES/UFPE)

# Project      : Cointegration and VEC Models
# Script       : Homework4.2.R
# Authors      : Jairo Macedo
# Created      : June 2025
# Last Modified: [June 2025] | by: Jairo Macedo

# Description  : This script estimates short- and long-run elasticities of gasoline demand 
#                and analyzes the relationship between Tbill and Tbill_3year using VEC models.
#                The tasks follow the methodologies of Engle-Granger, Johansen, and SVEC.

#                   1. Estimate short- and long-run elasticities using Engle-Granger (gasoline)
#                   2. Estimate VEC using Johansen procedure (gasoline)
#                   3. Estimate long-run relation between Tbill and Tbill_3year (both methods)
#                   4. Estimate and plot IRFs from a structural VEC model (SVEC)

# Structure    :
#   1. Libraries and data import
#   2. Engle-Granger methodology (gasoline demand)
#   3. Johansen VEC estimation (gasoline demand)
#   4. Engle-Granger and Johansen for Tbill series
#   5. SVEC estimation and IRF analysis



# 0. Load Configuration ----

# Load user-specific paths and settings from config file
source("config.R")  # defines base_path, data_clean_dir, etc.


# 1. Import & Merge Raw Data ----

## 1.1 Load dataset
df <- as.data.table(read_excel(file.path(general_path, "Homework", "Homework4", "quarterly.xls")))





















