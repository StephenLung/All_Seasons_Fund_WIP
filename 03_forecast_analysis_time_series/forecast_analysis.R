setwd("C:/Users/steph/Dropbox/Business University Science/Personal Portfolio/WIP/All_Seasons_Fund_WIP")
library(pacman)

pacman::p_load(
  # App
  "flexdashboard",
  "shiny",
  "shinyjs",
  "shinyWidgets",
  
  # Core
  "tidyquant",
  "tidyverse",
  "timetk",
  "tidyr",
  "tibble",
  "dplyr",
  "furrr", 
  "purrr",
  "dplyr",
  "glue",
  "forcats",
  "stringr",
  
  # Visualizations
  "plotly",
  "highcharter",
  "correlationfunnel",
  
  # Modeling
  "parsnip",
  
  # Time Series
  "tsibble",
  "fabletools",
  "feasts",
  "fable",
  "future.apply"
)

symbols <- c("VTI", "TLT", "IEF", "GLD", "DBC")
tech_symbols <- c("MSFT", "AMZN", "AAPL", "NFLX", "GOOG") 
# remove fb out as they IPO after financial crisis in 2012
end     <- today()
start   <- end - years(5) + days(1)
w       <- c(0.3,
             0.4,
             0.15,
             0.075,
             0.075)
wts_tbl <- tibble(tech_symbols, w)
window <- 24

benchmark_symbols <- "^GSPC"
benchmark_w <- 1
benchmark_tbl <- tibble(benchmark_symbols,
                        benchmark_w)
rfr <- .0003 #risk free rate 0.3% - 10 year treasury rate


# 1.0 PULL DATA SOURCE ----

stock_data <- multi_asset_price_portfolio(symbols = tech_symbols, end, start, wts_tbl) %>% 
  multi_asset_return_portfolio(period = "yearly") %>% 
  select(symbol, date, returns) %>% 
  group_by(symbol) %>% 
  tq_transmute(select     = returns,
               mutate_fun = periodReturn,
               period     = period,
               col_rename = "returns") %>% 
  ungroup() %>% 
  mutate(date = floor_date(date, unit = "year")) 

# 1.1 MODULARIZE ----

data <- multi_asset_price_portfolio(symbols = tech_symbols, end, start, wts_tbl)

aggregate_returns_portfolio <- function(data, period = "monthly"){
  
  output_tbl <- data %>% 
    multi_asset_return_portfolio(period = period) %>% 
    mutate(
      # date = floor_date(date, unit = unit), #setup floor date
           label_text = str_glue("Symbol: {symbol}
                                 Date: {date}
                                 Returns: {scales::percent(returns, accuracy = 0.01)}")) 
  return(output_tbl)
}

multi_asset_price_portfolio(symbols = symbols, end, start, wts_tbl) %>% 
  aggregate_returns_portfolio(period = "daily")

# Start Date of all seasons portfolio
min_date <- stock_data %>%   
  summarise(min_date = min(date), 
            max_date = max(date)) %>% 
  pull(min_date)

# 2.0 PLOT ----

data <- multi_asset_price_portfolio(symbols = tech_symbols, end, start, wts_tbl) %>% 
  aggregate_returns_portfolio(period = "yearly")


g <- data %>% 
  ggplot(aes(date, returns, color = symbol)) +
  
  geom_line() +
  geom_point(aes(text = label_text), size = 0.1) +
  # geom_smooth(method = "loess", span = 0.2) +
  
  theme_tq() +
  scale_color_tq() + 
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = str_glue("Returns of each asset in the portfolio by {period}"),
    subtitle = "Toggle by the timing",
    x = "", 
    y = "") 

ggplotly(g, tooltip = "text")

# 2.1 PLOT FUNCTION ----
plot_time_series <- function(data, ggplotly = FALSE, facetgrid = FALSE, period = "monthly"){
  
  g <- data %>% 
    ggplot(aes(date, returns, colour = symbol)) +
    
    geom_line() +
    geom_point(aes(text = label_text), size = 0.1) +
    # geom_smooth(method = "loess", span = 0.2) +
    
    theme_tq() +
    scale_color_tq() + 
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = str_glue("Returns of each asset in the portfolio by {period}"),
      subtitle = "Toggle by the timing",
      x = "", 
      y = "") 
    
  
  if(facetgrid == TRUE){
    g <- g + facet_grid(rows = vars(symbol),
                        # scales = "free" #free y-axis
                        )
  }
  
  if(ggplotly == TRUE){
    g <- ggplotly(g, tooltip = "text")
  }
    
  g
  
}

# Plot series with facet grid and plotly
multi_asset_price_portfolio(symbols = tech_symbols, end, start, wts_tbl) %>% 
  aggregate_returns_portfolio(period = "daily") %>% 
  plot_time_series(facetgrid = TRUE, ggplotly = TRUE, period = "daily")

tech_symbols <- c("FB", "AMZN", "AAPL", "NFLX", "GOOG")
fb_xts <- multi_asset_price_portfolio(symbols = tech_symbols, end, start, wts_tbl) %>% 
  aggregate_returns_portfolio(period = "quarterly") %>% 
  filter(symbol %in% "FB") %>% 
  tk_xts(date_col = date) %>% 
  round(., 4) * 100
amzn_xts <- multi_asset_price_portfolio(symbols = tech_symbols, end, start, wts_tbl) %>% 
  aggregate_returns_portfolio(period = "quarterly") %>% 
  filter(symbol %in% "AMZN") %>% 
  tk_xts(date_col = date) %>% 
  round(., 4) * 100
aapl_xts <- multi_asset_price_portfolio(symbols = tech_symbols, end, start, wts_tbl) %>% 
  aggregate_returns_portfolio(period = "quarterly") %>% 
  filter(symbol %in% "AAPL") %>% 
  tk_xts(date_col = date) %>% 
  round(., 4) * 100
nflx_xts <- multi_asset_price_portfolio(symbols = tech_symbols, end, start, wts_tbl) %>% 
  aggregate_returns_portfolio(period = "quarterly") %>% 
  filter(symbol %in% "NFLX") %>% 
  tk_xts(date_col = date) %>% 
  round(., 4) * 100
goog_xts <- multi_asset_price_portfolio(symbols = tech_symbols, end, start, wts_tbl) %>% 
  aggregate_returns_portfolio(period = "quarterly") %>% 
  filter(symbol %in% "GOOG") %>% 
  tk_xts(date_col = date) %>% 
  round(., 4) * 100

# Highcharter
highchart(type = "stock") %>% 
  hc_title(text = "FAANG stocks returns (2015-2020)") %>% 
  hc_subtitle(text = str_glue("With the Dow dropping 2.1K points, you will see that tech stocks are vulnerable")) %>% 
  hc_yAxis(title = list(text = "Return"),
           labels = list(format = "{value}%"),
           opposite = FALSE) %>% 
  hc_add_series(fb_xts,
                name = "Facebook",
                color = palette_light()[[1]]) %>% 
  hc_add_series(amzn_xts,
                name = "Amazon",
                color = palette_light()[[2]]) %>% 
  hc_add_series(aapl_xts,
                name = "Apple",
                color = palette_light()[[3]]) %>% 
  hc_add_series(nflx_xts,
                name = "Netflix",
                color = palette_light()[[4]]) %>% 
  hc_add_series(goog_xts,
                name = "Google",
                color = palette_light()[[5]]) %>% 
  # hc_tooltip(pointFormat = "{point.y}%") %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_exporting(enabled = TRUE)

# 3.0 CREATE PORTFOLIO ----
# Function to enable labelled text for plotting
aggregate_returns_portfolio_2 <- function(data, wts_tbl, period = "monthly"){
  
  output_tbl <- data %>% 
    multi_asset_return_portfolio(period = period) %>% 
    portfolio_return(wts_tbl, name_portfolio = "new portfolio") %>% 
    select(-symbol) %>% 
    mutate(
      # date = floor_date(date, unit = unit), #setup floor date
      label_text = str_glue("
                                 Date: {date}
                                 Returns: {scales::percent(returns)}")) 
  return(output_tbl)
}

# 4.0 CREATE TIME FEATURES ----
data <- multi_asset_price_portfolio(symbols = tech_symbols, end, start, wts_tbl) %>% 
  aggregate_returns_portfolio_2(wts_tbl = wts_tbl, period = "monthly")

# Training data
train_tbl <- data %>% 
  tk_augment_timeseries_signature()

future_data_tbl <- data %>% 
  tk_index() %>% 
  
  # Add full years worth of future data excluding weekends and holidays
  tk_make_future_timeseries(n_future = 300, inspect_weekdays = TRUE, inspect_months = TRUE) %>% 
  
  # Outputs the additional time features of the new dates
  tk_get_timeseries_signature()

# 5.0 ML ----

seed <- 123
set.seed(seed)

model_xgboost <- boost_tree(mode = "regression",
           mtry = 20, #use 2/3 of the columns, 29 columns -> 20 columns
           trees = 500, #used to keep speed fast for real time training
           min_n = 3, #each node must have 3 values minimum
           tree_depth = 8, #max tree depth is 8 levels to prevent overfitting
           learn_rate = 0.01, #make sure we find a high accuracy solution
           loss_reduction = 0.01) %>%  #each split must improve the model by 1% to make a split 
  set_engine(engine = "xgboost") %>% #is the output
  fit.model_spec(returns ~ ., data = train_tbl %>% select(-date, -label_text, -diff)) #fits data into the model

# 5.1 MAKE A PREDICTION ----
future_data_tbl

prediction_tbl <- predict(object = model_xgboost,
        new_data = future_data_tbl) %>% 
  bind_cols(future_data_tbl) %>% 
  select(.pred, index) %>% 
  rename(returns = .pred,
         date = index) %>% 
  mutate(
    label_text = str_glue("Date: {date}
                                 Returns: {scales::percent(returns, accuracy = 0.01)}")) %>% 
  add_column(key = "Prediction")

output_tbl <- data %>% 
  add_column(key = "Actual") %>% 
  bind_rows(prediction_tbl)

output_tbl %>% tail()

# 5.2 GENERATE FUNCTION ----

# xgboost - forecast with xgb except when time_scale is year which is to use regression
generate_forecast_xgb <- function(data, n_future = 12, seed = NULL){
  
  train_tbl <- data %>% 
    tk_augment_timeseries_signature()
  
  future_data_tbl <- data %>% 
    tk_index() %>% 
    
    # Add full years worth of future data excluding weekends and holidays
    tk_make_future_timeseries(n_future = n_future, inspect_weekdays = TRUE, inspect_months = TRUE) %>% 
    
    # Outputs the additional time features of the new dates
    tk_get_timeseries_signature()
  
  # Determine the time_scale by pulling it from tk_get_timeseries_summary
  time_scale <- data %>% 
    tk_index() %>% 
    tk_get_timeseries_summary() %>% 
    pull(scale)
  

  
  if(time_scale == "year"){
  
  model <- linear_reg(mode = "regression") %>% 
    set_engine(engine = "lm") %>% 
    fit.model_spec(returns ~., data = train_tbl %>% select(returns, index.num))
  
  } 
  # else if (time_scale == "day"){
  # 
  #   model <- linear_reg(mode = "regression") %>% 
  #     set_engine(engine = "lm") %>% 
  #     fit.model_spec(returns ~., data = train_tbl %>% select(returns, index.num))
  #   
  # } 
  else {
    
    seed <- seed
    set.seed(seed)
  model <- boost_tree(mode = "regression",
                              mtry = 20, #use 2/3 of the columns, 29 columns -> 20 columns
                              trees = 500, #used to keep speed fast for real time training
                              min_n = 3, #each node must have 3 values minimum
                              tree_depth = 8, #max tree depth is 8 levels to prevent overfitting
                              learn_rate = 0.01, #make sure we find a high accuracy solution
                              loss_reduction = 0.01) %>%  #each split must improve the model by 1% to make a split 
    set_engine(engine = "xgboost") %>% #is the output
    fit.model_spec(returns ~ ., data = train_tbl %>% select(-date, -label_text, -diff)) #fits data into the model
  }
  
  prediction_tbl <- predict(model,
                            new_data = future_data_tbl) %>% 
    bind_cols(future_data_tbl) %>% 
    select(.pred, index) %>% 
    rename(returns = .pred,
           date = index) %>% 
    mutate(
      label_text = str_glue("Date: {date}
                                 Returns: {scales::percent(returns, accuracy = 0.01)}")) %>% 
    add_column(key = "Prediction")
  
  output_tbl <- data %>% 
    add_column(key = "Actual") %>% 
    bind_rows(prediction_tbl)
  
  return(output_tbl)
  
  
}

multi_asset_price_portfolio(symbols = tech_symbols, end, start, wts_tbl) %>% 
  aggregate_returns_portfolio_2(wts_tbl = wts_tbl, period = "daily") %>% 
  generate_forecast_xgb(n_future = 12,
                    seed = 123)




# 5.3 PLOT FUNCTION ----
plot_time_series_portfolio <- function(data, ggplotly = FALSE, period){
  
  g <- data %>% 
    ggplot(aes(date, returns, color = key)) +
    
    geom_line() +
    geom_point(aes(text = label_text), size = 0.1) +
    geom_smooth(method = "loess", span = 0.2) +
    
    theme_tq() +
    scale_color_tq() + 
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = str_glue("Returns of FAANG portfolio by {period}"),
      subtitle = "Toggle by the timing",
      x = "", 
      y = "") 
  
  
  if(ggplotly == TRUE){
    g <- ggplotly(g, tooltip = "text")
  }
  
  g
  
}


# 6.0 PLOT FORECAST ----
data <- multi_asset_price_portfolio(symbols = tech_symbols, end, start, wts_tbl) %>% 
  aggregate_returns_portfolio_2(wts_tbl = wts_tbl, period = "weekly") %>% # cannot fix daily [ISSUE]
  generate_forecast_xgb(n_future = 24,
                    seed = 123) 


data %>% 
  plot_time_series_portfolio(ggplotly = TRUE, period = "month")

data %>% tail()

data %>% 
  ggplot(aes(date, returns, color = key)) +
  geom_line() +
  geom_point(aes(text = label_text), size = 0.1) +
  geom_smooth(method = "loess", span = 0.2) +
  theme_tq() + 
  scale_color_tq() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Forecasted Monthly Returns for FAANG in the next 12 months",
    subtitle = "Forecast with XGBoost",
    caption = str_glue("FAANG"),
    x = "",
    y = ""
  )

data <- multi_asset_price_portfolio(symbols = tech_symbols, end, start, wts_tbl) %>% 
  aggregate_returns_portfolio_2(wts_tbl = wts_tbl, period = "daily") %>% 
  generate_forecast_glmnet(n_future = 12,
                           seed =123) %>% 
  plot_time_series_portfolio(ggplotly = TRUE)


# 7.0 Adding in GLMNet ----

data <- multi_asset_price_portfolio(symbols = tech_symbols, end, start, wts_tbl) %>% 
  aggregate_returns_portfolio_2(wts_tbl = wts_tbl, period = "daily")

# Training data
train_tbl <- data %>% 
  tk_augment_timeseries_signature()

future_data_tbl <- data %>% 
  tk_index() %>% 
  
  # Add full years worth of future data excluding weekends and holidays
  tk_make_future_timeseries(n_future = 200, inspect_weekdays = TRUE, inspect_months = TRUE) %>% 
  
  # Outputs the additional time features of the new dates
  tk_get_timeseries_signature()

seed <- 123
set.seed(seed)

model_glmnet <- linear_reg(mode = "regression", penalty = 1, mixture = 0.5) %>% 
  set_engine(engine = "glmnet") %>% #is the output
  fit.model_spec(returns ~ ., data = train_tbl %>% select(-date, -label_text, -diff)) #fits data into the model

future_data_tbl

prediction_tbl <- predict(object = model_glmnet,
                          new_data = future_data_tbl) %>% 
  bind_cols(future_data_tbl) %>% 
  select(.pred, index) %>% 
  rename(returns = .pred,
         date = index) %>% 
  mutate(
    label_text = str_glue("Date: {date}
                                 Returns: {scales::percent(returns)}")) %>% 
  add_column(key = "Prediction")

output_tbl <- data %>% 
  add_column(key = "Actual") %>% 
  bind_rows(prediction_tbl)

output_tbl %>% tail()

output_tbl %>% 
  plot_time_series_portfolio(ggplotly = TRUE, period = "day")

# 7.1 GENERATE GLMNET FUNCTION ----

# glm  - forecast with glm except when time_scale is year which is to use regression
generate_forecast_glmnet <- function(data, n_future = 12, seed = NULL, penalty = 1, mixture = 0.5){
  
  train_tbl <- data %>% 
    tk_augment_timeseries_signature()
  
  future_data_tbl <- data %>% 
    tk_index() %>% 
    
    # Add full years worth of future data excluding weekends and holidays
    tk_make_future_timeseries(n_future = n_future, inspect_weekdays = TRUE, inspect_months = TRUE) %>% 
    
    # Outputs the additional time features of the new dates
    tk_get_timeseries_signature()
  
  # Determine the time_scale by pulling it from tk_get_timeseries_summary
  time_scale <- data %>% 
    tk_index() %>% 
    tk_get_timeseries_summary() %>% 
    pull(scale)
  
  
  
  if(time_scale == "year"){
    
    model <- linear_reg(mode = "regression") %>% 
      set_engine(engine = "lm") %>% 
      fit.model_spec(returns ~., data = train_tbl %>% select(returns, index.num))
    
  } 
  # else if (time_scale == "day"){
  # 
  #   model <- linear_reg(mode = "regression") %>%
  #     set_engine(engine = "lm") %>%
  #     fit.model_spec(returns ~., data = train_tbl %>% select(returns, index.num))
  # 
  # }
  else {
    
    seed <- seed
    set.seed(seed)
    model <- linear_reg(mode = "regression", penalty = penalty, mixture = mixture) %>% 
      set_engine(engine = "glmnet") %>% #is the output
      fit.model_spec(returns ~ ., data = train_tbl %>% select(-date, -label_text, -diff)) #fits data into the model
  }
  
  prediction_tbl <- predict(model,
                            new_data = future_data_tbl) %>% 
    bind_cols(future_data_tbl) %>% 
    select(.pred, index) %>% 
    rename(returns = .pred,
           date = index) %>% 
    mutate(
      label_text = str_glue("Date: {date}
                                 Returns: {scales::percent(returns)}")) %>% 
    add_column(key = "Prediction")
  
  output_tbl <- data %>% 
    add_column(key = "Actual") %>% 
    bind_rows(prediction_tbl)
  
  return(output_tbl)
  
  
}

#daily, weekly, monthly, quarterly do not work
multi_asset_price_portfolio(symbols = tech_symbols, end, start, wts_tbl) %>% 
  aggregate_returns_portfolio_2(wts_tbl = wts_tbl, period = "quarterly") %>% 
  generate_forecast_glmnet(n_future = 8) %>% 
  plot_time_series_portfolio(period = "month")

multi_asset_price_portfolio(symbols = tech_symbols, end, start, wts_tbl) %>% 
  aggregate_returns_portfolio_2(wts_tbl = wts_tbl, period = "daily") %>% 
  generate_forecast_glmnet(n_future = 365) %>% 
  plot_time_series_portfolio(period = "day")

#daily, weekly do not work. monthly, quarterly, yearly work

multi_asset_price_portfolio(symbols = tech_symbols, end, start, wts_tbl) %>% 
  aggregate_returns_portfolio_2(wts_tbl = wts_tbl, period = "weekly") %>% 
  generate_forecast_xgb(n_future = 365) %>% 
  plot_time_series_portfolio(period = "day")

multi_asset_price_portfolio(symbols = tech_symbols, end, start, wts_tbl) %>% 
  aggregate_returns_portfolio_2(wts_tbl = wts_tbl, period = "quarterly") %>% 
  generate_forecast_xgb(n_future = 8) %>% 
  plot_time_series_portfolio(period = "quarter")

multi_asset_price_portfolio(symbols = tech_symbols, end, start, wts_tbl) %>% 
  aggregate_returns_portfolio_2(wts_tbl = wts_tbl, period = "monthly") %>% 
  generate_forecast_xgb(n_future = 24) %>% 
  plot_time_series_portfolio(period = "month")

multi_asset_price_portfolio(symbols = tech_symbols, end, start, wts_tbl) %>% 
  aggregate_returns_portfolio_2(wts_tbl = wts_tbl, period = "yearly") %>% 
  generate_forecast_xgb(n_future = 2) %>% 
  plot_time_series_portfolio(period = "year")

#8.1 TEST OUT ARIMA with TSIBBLE PACKAGE ----
data <- multi_asset_price_portfolio(symbols = tech_symbols, end, start, wts_tbl) %>% 
  aggregate_returns_portfolio_2(wts_tbl = wts_tbl, period = "daily") %>%  
  # tk_augment_timeseries_signature() %>% 
  select(-label_text) %>% 
  add_column(portfolio = "new portfolio") %>% 
  na.omit()

data <- data %>% 
  # mutate(trading_day = row_number()) %>% 
  as_tsibble(index = date,
             key = portfolio,
             regular = TRUE) %>% 
  fill_gaps(returns = 0, .full=TRUE)

data %>% autoplot(returns)
data %>% ACF(returns) %>% autoplot()

fit <- data %>% 
  model(
    ets = ETS(returns)
  )

fit %>% 
  forecast(h = "2 years") %>% 
  autoplot(returns)


#9.0 SAVE FUNCTIONS ----
dump(c("aggregate_returns_portfolio_2", 
       "generate_forecast_xgb",
       "generate_forecast_glmnet",
       "plot_time_series_portfolio"),
     file = "00_Scripts/04_forecast.R")

dump(c("aggregate_returns_portfolio",
       "plot_time_series"),
     file = "00_Scripts/05_basic.R")
