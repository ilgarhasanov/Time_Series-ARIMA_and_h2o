library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)

library(data.table)
library(rstudioapi)
library(skimr)
library(glue)
library(highcharter)
library(plotly)
library(h2o) 


df <- read.csv("daily-minimum-temperatures-in-me.csv")

df[!complete.cases(df),] %>% view()

df$Daily.minimum.temperatures <- gsub("\\?","",df$Daily.minimum.temperatures)

df %>% glimpse()

df$Date <- df$Date %>% as.Date("%m/%d/%Y")




#--------------------------TASK 1 ---------------------------
#Build h2o::automl(). For this task:
#prepare data using tk_augment_timeseries_signature()
#set stopping metric to "RMSE"
#set exclude_algos = c("DRF", "GBM","GLM",'XGBoost') 

df_new <- df%>%
  tk_augment_timeseries_signature()


df_new <- df_new %>% select(-Date)
#splitting data
splits <- initial_time_split(df_new, prop = 0.9)

h2o.init()

train <- training(splits) %>% as.h2o()
test <- testing(splits) %>% as.h2o()

target <- 'Daily.minimum.temperatures'
features <- df_new %>% select(-Daily.minimum.temperatures) %>% names()

#modelling

model <- h2o.automl(
  x = features,
  y = target,
  training_frame    = train,
  validation_frame  = test,
  leaderboard_frame = test,
  stopping_metric = "RMSE",
  seed = 123,
  exclude_algos = c("DRF", "GBM", "GLM", "XGBoost"),
  max_runtime_secs = 120)


model@leaderboard %>% as.data.frame()
model@leader

y_pred <- model %>% h2o.predict(newdata = test) %>% as.data.frame()
y_pred$predict

test <- test %>% as.data.frame()
residuals = test$value - y_pred$predict

RMSE = sqrt(mean(residuals^2))


#forecasting

one_year <- seq.Date(as.Date("1991-01-01"), as.Date("1991-12-01"),"months")

forecast_data <- tk_augment_timeseries_signature(as.data.frame(one_year)) %>%
  select() %>% rename(Date = one_year) 

forecast_data = forecast_data%>% as.h2o()

forecast_data_predict = model %>% h2o.predict(newdata = forecast_data)%>% as.data.frame()



#--------------------------TASK 2 ---------------------------
#Build modeltime::arima_reg(). For this task set engine to "auto_arima"


#visualize
df %>%
  plot_time_series(Date, Daily.minimum.temperatures, .interactive = interactive)

# Split Data 80/20
splits <- initial_time_split(df, prop = 0.9)


#Create the model----
model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(Daily.minimum.temperatures ~ Date, data = training(splits))


models_tbl <- modeltime_table(model_fit_arima_no_boost)

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))


calibration_tbl %>% modeltime_accuracy()

interactive <- FALSE

calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = df
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, 
    .interactive = interactive
  )


refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = df)



#--------------------------TASK 3 ---------------------------
#Forecast temperatures for next year with model which has lower RMSE.

refit_tbl %>%
  modeltime_forecast(h = "1 year", actual_data = df) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, 
    .interactive = interactive)


