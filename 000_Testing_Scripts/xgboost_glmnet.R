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
  "recipes"
)

wts_tbl <- FANG %>% 
  distinct(symbol) %>% 
  add_column(weight = c(0.25, 0.25, 0.25, 0.25))

data <- FANG %>% 
  group_by(symbol) %>% 
  
  # convert into daily returns
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "returns") %>% 
  ungroup(symbol) %>% 
  
  # convert to portfolio returns
  tq_portfolio(assets_col = symbol, 
               returns_col = returns,
               weights = wts_tbl,
               rebalance_on = "years",
               col_rename = "returns")

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

normalized_data <- data %>% 
  mutate(returns = normalize(returns))


train_tbl <- normalized_data %>% 
  tk_augment_timeseries_signature()

future_data_tbl <- normalized_data %>% 
  tk_index() %>% 
  tk_make_future_timeseries(n_future = 5, inspect_weekdays = TRUE, inspect_months = TRUE) %>% 
  tk_get_timeseries_signature()

seed <- 123
set.seed(seed)

model_glmnet <- linear_reg(mode = "regression", penalty = 1, mixture = 0.5) %>% 
  set_engine(engine = "glmnet") %>% 
  fit.model_spec(returns ~., data = train_tbl %>% select(-date, -diff))

predict(object = model_glmnet,
        new_data = future_data_tbl)

model_xgb <- boost_tree(mode = "regression", mtry = 20,
                        trees = 500,
                        min_n = 3,
                        tree_depth = 8,
                        learn_rate = 0.01,
                        loss_reduction = 0.01) %>% 
  set_engine(engine = "xgboost") %>% 
  fit.model_spec(returns ~., data = train_tbl %>% select(-date, -diff))

prediction_tbl <- predict(object = model_xgb,
        new_data = future_data_tbl) %>% 
  bind_cols(future_data_tbl) %>% 
  select(.pred, index) %>%
  rename(returns = .pred, 
         date        = index) %>%
  add_column(key = "Prediction")

output_tbl <- normalized_data %>%
  add_column(key = "Actual") %>%
  bind_rows(prediction_tbl) 

output_tbl %>% 
  tail(10)


train_tbl %>% 
  mutate(label_text = str_glue("{scales::percent(returns)}")) %>% 
  select(label_text)
  
