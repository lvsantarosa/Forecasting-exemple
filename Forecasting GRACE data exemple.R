library(terra)
library(tidyverse)
library(timetk)

#Create list and import 
#See how to download the GRACE data in Google Colab: https://github.com/lvsantarosa/NASA-data-download

lista <- list.files('GRACE_dataset/', pattern = '.nc4$', full.names = TRUE)
gws <- terra::rast(lista)

#Rename the layers
st <- as.Date('2003/02/01')
en <- as.Date('2022/06/30')
nomes <- seq(from = st, to = en, by= 'day')
names(gws) <- nomes


#convert to data frame and analyse the datetime
###############################################################################

df_gws_month <- gws %>% terra::as.data.frame( xy = TRUE) %>% 
  pivot_longer(cols = starts_with("20"), names_to = "Date", values_to = "GWS")  %>%
  mutate(Date = as.Date(Date)) %>% 
  summarise_by_time(
    .date_var = Date,
    .by       = "month",
    value  = mean(GWS))

write.csv(df_gws_month, "DF_GRACE_Month.csv")

mean = mean(df_gws_month$value)
sd = sd(df_gws_month$value)

df_gws_month$norm <- (df_gws_month$value - mean) / sd

plot(df_gws_month$norm)

###############################################################################
#install.packages("modeltime", dependencies = TRUE)
library(xgboost)
library(tidymodels)
library(modeltime)
library(lubridate)

#Test of pakage and exemple from: https://business-science.github.io/modeltime/articles/getting-started-with-modeltime.html

# This toggles plots from plotly (interactive) to ggplot (static)
interactive <- T

df_gws_month  %>%
  plot_time_series(Date, value, .interactive = interactive)


#Split 
splits <- time_series_split(df_gws_month, initial = "10 years", 
                            assess = "2 years", 
                            slice   = 6,
                            lag     = "1 year")

# Model 2: arima_boost ----
model_fit_arima_boosted <- arima_boost(
  min_n = 6,
  learn_rate = 0.6,
  seasonal_period = "auto"
) %>%
  set_engine(engine = "arima_xgboost") %>%
  fit(value ~ Date + as.numeric(Date) + factor(month(Date, label = TRUE), ordered = F),
      data = training(splits))

# Model 3: ets ----
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(value ~ Date, data = training(splits))

# Model 4: prophet ----
model_fit_prophet <- prophet_reg(
  mode = "regression",
  seasonality_yearly = TRUE) %>%
  set_engine(engine = "prophet") %>%
  fit(value ~ Date, data = training(splits))

# Model 5: lm ----
model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(value ~ as.numeric(Date) + factor(month(Date, label = TRUE), ordered = FALSE),
      data = training(splits))

#models
models_tbl <- modeltime_table(
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet,
  model_fit_lm
)

models_tbl

#Calibration
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl

#Forecasting
calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = df_gws_month
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )

#Acuracy
calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = interactive
  )

#Projections
refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = df_gws_month)

refit_tbl %>%
  modeltime_forecast(h = "3 years", actual_data = df_gws_month) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )


