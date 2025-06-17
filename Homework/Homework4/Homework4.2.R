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



# 0. Load Configuration for Engle-Granger method----

# Load user-specific paths and settings from config file
source("config.R")  # defines base_path, data_clean_dir, etc.


# 1. Import & Merge Raw Data  ----

## 1.1 Load dataset
df <- as.data.table(read_excel(file.path(general_path, "Homework", "Homework4", "quarterly.xls")))


# Convert to numeric
df[, Tbill := as.numeric(Tbill)]
df[, Tbill_3year := as.numeric(Tbill_3year)]


# 1.2 Extract year and quarter from 'Date' column
df[, `:=` (
  year = as.integer(substr(Date, 1, 4)),
  quarter = as.integer(substr(Date, 6, 6))
)]

# 1.3 Drop rows with NA in Tbill or Tbill_3year
df <- df[!is.na(Tbill) & !is.na(Tbill_3year)]

# 2. Engle-Granger methodology ----


# 2.1 ADF test for Tbill (with drift)
adf_tbill <- ur.df(df$Tbill, type = "drift", selectlags = "AIC")
summary(adf_tbill)

# 👉 As the test statistic -2.4169 > critical value at 5% (-2.88),
# we do not reject the null hypothesis of a unit root.
# → The series Tbill is non-stationary in levels (as expected for an I(1) process) ✅


# 2.2 ADF test for Tbill_3year (with drift)
adf_tbill3 <- ur.df(df$Tbill_3year, type = "drift", selectlags = "AIC")
summary(adf_tbill3)

# 👉 The test statistic for Tbill_3year is -1.8306, which is higher than the 5% critical value (-2.88).
# → We do not reject the null hypothesis of a unit root.
# The series Tbill_3year is also non-stationary in levels — consistent with an I(1) process. ✅


# 2.3.1 Compute first differences
df[, d_Tbill := c(NA, diff(Tbill))]
df[, d_Tbill_3year := c(NA, diff(Tbill_3year))]

# 2.3.2 ADF on ΔTbill
summary(ur.df(na.omit(df$d_Tbill), type = "drift", selectlags = "AIC"))

# ✅ The test statistic for ΔTbill is -10.67, which is below the 5% critical value (-2.88).
# → We reject the null hypothesis of a unit root in first differences.
# Tbill is stationary in first differences → I(1) confirmed. ✅


# 2.3.3 ADF on ΔTbill_3year
summary(ur.df(na.omit(df$d_Tbill_3year), type = "drift", selectlags = "AIC"))

# ✅ The test statistic for ΔTbill_3year is -10.05, which is below the 5% critical value (-2.88).
# → We reject the null hypothesis of a unit root in first differences.
# Tbill_3year is stationary in first differences → I(1) confirmed. ✅



# 3. Estimate the long-run equilibrium relationship ----

# 3.1 Estimate the long-run equation: Tbill as a function of Tbill_3year
# This captures the potential cointegrating relationship between the two series
long_run_model <- lm(Tbill ~ Tbill_3year, data = df)
summary(long_run_model)


# ✅ Long-run regression results:
# Tbill = -0.681 + 0.958 * Tbill_3year
# Both the intercept and slope are statistically significant at the 1% level.
# The R-squared is 0.92, indicating a strong linear relationship.
# Residuals from this regression will be tested for stationarity in the next step.



# 3.2 Extract residuals (error correction term) for Engle-Granger cointegration test
df[, residuals_eg := resid(long_run_model)]


# 4. Test for stationarity of residuals (Engle-Granger cointegration test) ----

summary(ur.df(df$residuals_eg, type = "none", selectlags = "AIC"))


# ✅ ADF test statistic on residuals is -4.77, which is below the 5% critical value (-1.95).
# → We reject the null hypothesis of a unit root in the residuals.
# ✅ Conclusion: Tbill and Tbill_3year are cointegrated according to the Engle-Granger test.


# 5. Estimate the Error Correction Model (ECM) ----

# 5.1 Create first differences
df[, d_Tbill := c(NA, diff(Tbill))]
df[, d_Tbill_3year := c(NA, diff(Tbill_3year))]

# 5.2 Create lagged residual (error correction term)
df[, ecm_lag := shift(residuals_eg)]

# 5.3 Prepare final ECM dataset (drop NAs after creating variables)
df_ecm <- na.omit(df[, .(d_Tbill, d_Tbill_3year, ecm_lag)])

# 5.4 Estimate ECM
ecm_model <- lm(d_Tbill ~ d_Tbill_3year + ecm_lag, data = df_ecm)
summary(ecm_model)

# ✅ ECM results:
# ΔTbill_t = 0.003 + 0.985 * ΔTbill_3year_t - 0.161 * ecm_lag_t-1
# The adjustment coefficient (-0.161) is negative and significant (p < 0.001),
# indicating convergence to the long-run equilibrium.
# About 16.1% of the disequilibrium is corrected each period.
# The model explains 70% of the short-term variation in Tbill.




# 0. Load Configuration for Johansen method----

rm(list = ls())

# Load user-specific paths and settings from config file
source("config.R")  # defines base_path, data_clean_dir, etc.

# 1. Import & Merge Raw Data ----

## 1.1 Load dataset
df <- as.data.table(read_excel(file.path(general_path, "Homework", "Homework4", "quarterly.xls")))


# Convert to numeric
df[, Tbill := as.numeric(Tbill)]
df[, Tbill_3year := as.numeric(Tbill_3year)]


# 1.2 Extract year and quarter from 'Date' column
df[, `:=` (
  year = as.integer(substr(Date, 1, 4)),
  quarter = as.integer(substr(Date, 6, 6))
)]

# 1.3 Drop rows with NA in Tbill or Tbill_3year
df <- df[!is.na(Tbill) & !is.na(Tbill_3year)]

# 2. Johansen test (trace test, with constant in cointegration equation)
johansen_test <- ca.jo(df[, .(Tbill, Tbill_3year)], type = "trace", ecdet = "const", K = 2)
summary(johansen_test)

# ✅ Johansen trace test indicates one cointegrating relationship:
# r = 0 is rejected (test stat = 32.72 > critical = 19.96)
# r ≤ 1 is not rejected (test stat = 4.53 < critical = 9.24)
# Conclusion: Tbill and Tbill_3year are cointegrated with rank = 1


# 3. Estimate the VECM using the Johansen object ----

vec_model <- cajorls(johansen_test, r = 1)
summary(vec_model$rlm)

# ✅ VECM results show that the adjustment coefficient for Tbill is negative (-0.135),
# indicating that short-term deviations from the long-run equilibrium are partially corrected each period.
# The short-term dynamics of Tbill are significantly affected by past changes in Tbill_3year (p < 0.05),
# while the equation for Tbill_3year does not show significant short-term effects.


# 4. Estimate VAR in levels (same lag as used in VECM: p = 2)
var_model <- VAR(df[, .(Tbill, Tbill_3year)], p = 2, type = "const")


# 4.1 Generate impulse response functions (IRFs)
irf_result <- irf(var_model,
                  impulse = "Tbill",
                  response = "Tbill_3year",
                  n.ahead = 10,
                  boot = TRUE)

# Plot the IRF
plot(irf_result)


# ✅ The IRF shows that a shock to short-term interest rates (Tbill) causes a persistent and significant increase in long-term rates (Tbill_3year).
# The response peaks early and gradually decreases, consistent with expectations from the yield curve dynamics.
# Confidence intervals suggest the effect is statistically significant in the first few periods.




