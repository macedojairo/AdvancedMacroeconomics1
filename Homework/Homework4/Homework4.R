# Project Info (Homeworks from Advanced Macroeconomics 1 - PIMES/UFPE)

# Project      : Cointegration and VEC Models
# Script       : Homework4.R
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
df <- as.data.table(read_excel(file.path(general_path, "Homework", "Homework4", "data_vec_alunos.xls")))

# Log-transforming the relevant variables
df[, l_consumption := log(Cgasolina)]
df[, l_income := log(PIB_cap)]
df[, l_price_gas := log(Pgasolina)]
df[, l_price_eth := log(Petanol)]

# Drop intermediate variables
df[, c("Cgasolina", "PIB_cap", "Pgasolina", "Petanol") := NULL]

summary(df)


## Graphs

# List of variables to plot
vars_to_plot <- c("l_consumption", "l_income", "l_price_gas", "l_price_eth")

# Generate plots using a loop
for (var in vars_to_plot) {
  p <- ggplot(df, aes(x = Ano, y = get(var))) +
    geom_line(color = red_color, size = 1) +  # Apply custom line color
    labs(title = paste("Log-transformed", var), x = "Date", y = paste("Log(", var, ")", sep = "")) +
    theme_minimal() +  # Removes gray background
    theme(
      panel.grid.major = element_line(color = "gray90"),  # Soft major grid lines
      panel.grid.minor = element_line(color = "gray95"),  # Softer minor grid lines
      panel.background = element_rect(fill = "white"),  # White background
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold")  # Centered title
    )
  
  print(p)  # Display the plot
}



# 2. Question 1 ----

# Using the dataset data_vec_alunos.xls, estimate a VEC model and compute 
# the short- and long-run elasticities of gasoline demand.
#
# - Apply the Engle-Granger methodology as seen in class and compare your 
#   results with Alves and Bueno (2003).
# - Repeat the exercise using the Johansen procedure.
# - Include the R code in the appendix and be prepared to explain your results if asked.

# 2.1 Engle-Granger Methodology ----




# Step 1: Test for unit roots in log-level series (ADF test with trend)
summary(ur.df(df$l_consumption, type = "trend", selectlags = "AIC"))


df[, dl_consumption := c(NA, diff(l_consumption))]
summary(ur.df(na.omit(df$dl_consumption), type = "drift", selectlags = "AIC"))



summary(ur.df(df$l_income, type = "trend", selectlags = "AIC"))

df[, dl_income := c(NA, diff(l_income))]
summary(ur.df(na.omit(df$dl_income), type = "drift", selectlags = "AIC"))




summary(ur.df(df$l_price_gas, type = "trend", selectlags = "AIC"))
df[, dl_price_gas := c(NA, diff(l_price_gas))]
summary(ur.df(na.omit(df$dl_price_gas), type = "drift", selectlags = "AIC"))





summary(ur.df(df$l_price_eth, type = "trend", selectlags = "AIC"))
df[, dl_price_eth := c(NA, diff(l_price_eth))]
summary(ur.df(na.omit(df$dl_price_eth), type = "drift", selectlags = "AIC"))



# Step 2: Estimate long-run relationship via OLS
long_run_model <- lm(l_consumption ~ l_income + l_price_gas + l_price_eth, data = df)
summary(long_run_model)

df[, residuals := resid(long_run_model)]

summary(ur.df(df$residuals, type = "none", selectlags = "AIC"))


# We estimate the long-run relationship via OLS and test the residuals for stationarity.
# The ADF test rejects the unit root at 5%, indicating cointegration among the variables.





# Diferenças
df[, dl_consumption := c(NA, diff(l_consumption))]
df[, dl_income := c(NA, diff(l_income))]
df[, dl_price_gas := c(NA, diff(l_price_gas))]
df[, dl_price_eth := c(NA, diff(l_price_eth))]

# Defasagem do erro de longo prazo
df[, residuals_lag := shift(residuals, 1)]



# ECM: mudança no consumo de gasolina explicada pelo erro de longo prazo defasado + variações das variáveis explicativas
ecm_model <- lm(dl_consumption ~ residuals_lag +
                  dl_income + dl_price_gas + dl_price_eth,
                data = df)

summary(ecm_model)


