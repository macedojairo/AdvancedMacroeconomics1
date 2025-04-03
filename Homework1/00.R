
# Advanced Macroeconomics 1- PIMES ----------------------------------------
# Name: Davi Jorge Souza Pontes -------------------------------------------
# Date: March, 2025 -------------------------------------------------------
# Homework 1   ------------------------------------------------------------



# Libraries ---------------------------------------------------------------
library(dplyr)
library(forecast)
library(tseries)
library(aTSA)



# Question 1 --------------------------------------------------------------

# Read databases ----------------------------------------------------------
series <- readxl::read_excel('dados_hmw_1.xls') %>% 
  ts() # Tranforming in timeseries 


# Plot series -------------------------------------------------------------
plot(series)

# Serie 1: does not have trend; it seems a white noise
# Serie 2: has a trend
# Serie 3: seems to be a white noise, but it might not be
# Serie 4: is not possible to say anything about a trend just by looking at it


# Remove the trend  -------------------------------------------------------
## Serie 1  ------
serie1 = series[,1] %>% 
  ts()

# We just need a diff

serie1_no_trend = serie1 %>% 
  diff()



## Serie 2 ------
serie2 = series[,2] %>% 
  ts()

# Diff
serie2_no_trend = serie2 %>% 
  diff()

plot(serie2_no_trend)

# Removing the linear trend by linear regression
fit = lm(serie2 ~ seq(1,149))

serie2_no_trend2 = fit$residuals %>% 
  ts

plot(serie2_no_trend2)



## Serie 3 ------
serie3 = series[,3] %>% 
  ts()

# Diff
serie3_no_trend = serie3 %>% 
  diff()

plot(serie3_no_trend)

# Removing the linear trend by linear regression
fit = lm(serie3 ~ seq(1,149))

serie3_no_trend2 = fit$residuals %>% 
  ts

plot(serie3_no_trend2)



## Serie 4 ------
serie4 = series[,4] %>% 
  ts()

# Diff
serie4_no_trend = serie4 %>% 
  diff()

plot(serie4_no_trend)

# Removing the linear trend by linear regression
fit = lm(serie4 ~ seq(1,149))

serie4_no_trend2 = fit$residuals %>% 
  ts

plot(serie4_no_trend2)




# Apply the Box-Jenkins methodology  --------------------------------------


## Identify the orders of the model ---------------------------------------

#' Serie 1: seems like an MA(1) process, because the acf has a positive spike at 
#' lag 1, and then drops to 0.The pacf exhibits an oscillating decay
acf(serie1_no_trend)
pacf(serie1_no_trend)

acf(serie1)
pacf(serie1)


#' Serie 2 without trend: seems like an ARMA(1,1) process, because the acf has a  
#' decays after lag q = 2, and the pacf exhibits an oscillating decay
acf(serie2_no_trend)
pacf(serie2_no_trend)


#' Serie 3 without trend: seems like an AR(4) process, because the acf has a  
#' decays towards 0, and the the pacf exhibits 0 when n >= 6
acf(serie3_no_trend)
pacf(serie3_no_trend)

#' Serie 4 without trend: seems like an ARMA(2,2) process, because the acf has a  
#' decays after lag q = 2, and the pacf decays after lag q = 2
acf(serie4_no_trend)
pacf(serie4_no_trend)


# Find the Model ---------------------------------------------------------

mod1 = arima(serie1, order = c(1,0,1)) # Model choosed by me
mod1_r <- auto.arima(serie1) # Model by auto arima

mod2 = arima(serie2_no_trend, order = c(1,0,1)) # Model choosed by me
mod2_r <- auto.arima(serie2_no_trend, ) # Model by auto arima

mod3 = arima(serie3_no_trend, order = c(0,0,5)) # Model choosed by me
mod3_r <- auto.arima(serie3_no_trend) # Model by auto arima

mod4 = arima(serie4_no_trend, order = c(1,0,1)) # Model choosed by me
mod4_r <- auto.arima(serie4_no_trend) # Model by auto arima


# Diagnostic --------------------------------------------------------------

checkresiduals(mod1)
checkresiduals(mod1_r)
Box.test(residuals(mod1), type="Ljung-Box")
Box.test(residuals(mod1_r), type="Ljung-Box")
jarque.bera.test(x = mod1$residuals)
jarque.bera.test(mod1_r$residuals)
arch.test(mod1) # there is heteroscedaticity in order <=4


checkresiduals(mod2)
checkresiduals(mod2_r)
Box.test(residuals(mod2), type="Ljung-Box")
Box.test(residuals(mod2_r), type="Ljung-Box")
jarque.bera.test(x = mod2$residuals)
jarque.bera.test(mod2_r$residuals)
arch.test(mod2) # there is heteroscedaticity in order <=4

checkresiduals(mod3)
checkresiduals(mod3_r)
Box.test(residuals(mod3), type="Ljung-Box")
Box.test(residuals(mod3_r), type="Ljung-Box")
jarque.bera.test(x = mod3$residuals)
jarque.bera.test(mod3_r$residuals)
arch.test(mod3) # there is heteroscedaticity in order <=4

checkresiduals(mod4)
checkresiduals(mod4_r)
Box.test(residuals(mod4), type="Ljung-Box")
Box.test(residuals(mod4_r), type="Ljung-Box")
jarque.bera.test(x = mod4$residuals)
jarque.bera.test(mod4_r$residuals)
arch.test(mod4) # there is heteroscedaticity in order <=4


# Forecast ----------------------------------------------------------------

generics::forecast(mod1) %>% 
  autoplot()
generics::forecast(mod1_r)%>% 
  autoplot()


generics::forecast(mod2)%>% 
  autoplot()
generics::forecast(mod2_r)%>% 
  autoplot()

generics::forecast(mod3)%>% 
  autoplot()
generics::forecast(mod3_r)%>% 
  autoplot()

generics::forecast(mod4)%>% 
  autoplot()
generics::forecast(mod4_r)%>% 
  autoplot()



# Question 2 --------------------------------------------------------------

library(ipeadatar)


# Avaliable series --------------------------------------------------------

ipea_series <- ipeadatar::available_series()



# Series ------------------------------------------------------------------

pib <- ipeadatar::ipeadata('SCN104_PIBPM104') %>% 
  dplyr::filter(lubridate::year(date) > 1995) %>% 
  dplyr::select(pib = value) %>% 
  ts(start = 1995,frequency = 4)

ipca <- ipeadatar::ipeadata('PRECOS12_IPCA12') %>% 
  dplyr::filter(lubridate::year(date) > 1995) %>% 
  dplyr::select(ipca = value) %>% 
  ts(start = 1995,frequency = 12)


# plot --------------------------------------------------------------------

plot(pib);
plot(ipca)



# Remove trend ------------------------------------------------------------

pib_no_trend = pib 
pib_no_trend %>% plot


ipca_no_trend = ipca 
ipca_no_trend %>% plot


# Remove covid ------------------------------------------------------------
# create a vector of dates
datas_pib <- seq(as.Date("2004-05-01"), as.Date("2024-12-01"), by = "quarter")
datas_ipca <- seq(as.Date("2004-02-01"), as.Date("2025-02-01"), by = "month")

# Variable to critical months
mes_critico <- ifelse(format(datas_pib, "%Y-%m") %in% c(
  "2020-05", "2020-06", "2020-07",  # 1ª onda
  "2021-03", "2021-04", "2021-05",  # 2ª onda
  "2022-01", "2022-02"              # 3ª onda (Ômicron)
), 1, 0)

mes_critico_ipca <- ifelse(format(datas_ipca, "%Y-%m") %in% c(
  "2020-05", "2020-06", "2020-07",  # 1ª onda
  "2021-03", "2021-04", "2021-05",  # 2ª onda
  "2022-01", "2022-02"              # 3ª onda (Ômicron)
), 1, 0)


# Create dataframe

df_covid_pib = dplyr::tibble(datas_pib, mes_critico) %>%
  dplyr::select(-datas_pib) %>% 
  ts(start = 2004,frequency = 4)
df_covid_ipca = dplyr::tibble(datas_ipca, mes_critico = mes_critico_ipca)%>% 
  dplyr::select(-datas_ipca) %>% 
  ts(start = 2004,frequency = 12) 


## Identify the orders of the model ---------------------------------------

tsdisplay(pib_no_trend) # AR(1)

tsdisplay(ipca_no_trend) # AR(1)


# Find the Model ---------------------------------------------------------

mod1 = arima(pib_no_trend, order = c(1,0,0)) # Model choosed by me
mod1_r <- auto.arima(pib_no_trend) # Model by auto arima

mod2 = arima(ipca_no_trend, order = c(1,0,0)) # Model choosed by me
mod2_r <- auto.arima(ipca_no_trend) # Model by auto arima

# Remove Covid

mod1_covid = arima(pib_no_trend, order = c(1,0,0), xreg = df_covid_pib) # Model choosed by me
mod1_r_covid <- auto.arima(pib_no_trend, xreg = df_covid_pib) # Model by auto arima

mod2_covid = arima(ipca_no_trend, order = c(1,0,0), xreg = df_covid_ipca) # Model choosed by me
mod2_r_covid <- auto.arima(ipca_no_trend, xreg = df_covid_ipca) # Model by auto arima



# Diagnostic --------------------------------------------------------------

checkresiduals(mod1)
checkresiduals(mod1_r)
Box.test(residuals(mod1), type="Ljung-Box")
Box.test(residuals(mod1_r), type="Ljung-Box")
jarque.bera.test(x = mod1$residuals)
jarque.bera.test(mod1_r$residuals)
arch.test(mod1) # there is heteroscedaticity in order <=4


checkresiduals(mod2)
checkresiduals(mod2_r)
Box.test(residuals(mod2), type="Ljung-Box")
Box.test(residuals(mod2_r), type="Ljung-Box")
jarque.bera.test(x = mod2$residuals)
jarque.bera.test(mod2_r$residuals)
arch.test(mod2) # there is heteroscedaticity in order <=4


# Remove Covid

checkresiduals(mod1_covid)
checkresiduals(mod1_r_covid)
Box.test(residuals(mod1_covid), type="Ljung-Box")
Box.test(residuals(mod1_r_covid), type="Ljung-Box")
jarque.bera.test(x = mod1_covid$residuals)
jarque.bera.test(mod1_r_covid$residuals)
arch.test(mod1_covid) # there is heteroscedaticity in order <=4


checkresiduals(mod2_covid)
checkresiduals(mod2_r_covid)
Box.test(residuals(mod2_covid), type="Ljung-Box")
Box.test(residuals(mod2_r_covid), type="Ljung-Box")
jarque.bera.test(x = mod2_covid$residuals)
jarque.bera.test(mod2_r_covid$residuals)
arch.test(mod2_covid) # there is heteroscedaticity in order <=4


# Forecast ----------------------------------------------------------------



plt1 = generics::forecast(mod1) %>% 
  autoplot()

plt2 <- generics::forecast(mod1_r)%>% 
  autoplot()


plt3 <- generics::forecast(mod2)%>% 
  autoplot()

plt4 <- generics::forecast(mod2_r)%>% 
  autoplot()


# Remove Covid - This did not work 


generics::forecast(mod1_covid)%>% 
  autoplot()
generics::forecast(mod1_r_covid)%>% 
  autoplot()

generics::forecast(mod2_covid)%>% 
  autoplot()
generics::forecast(mod2_r_covid)%>% 
  autoplot()


# Question 2. i. ----------------------------------------------------------

forecast_pib <- generics::forecast(mod1)

growth_pib = (0.8627/0.6626)-1; growth_pib


forecast_ipca <- generics::forecast(mod2)
forecast_ipca$x
growth_ipca = (20.40047/93.17)-1; growth_ipca
