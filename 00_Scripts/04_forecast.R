aggregate_returns_portfolio_2 <-
function(data, wts_tbl, period = "monthly"){
  
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
generate_forecast_xgb <-
function(data, n_future = 12, seed = NULL){
  
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
generate_forecast_glmnet <-
function(data, n_future = 12, seed = NULL, penalty = 1, mixture = 0.5){
  
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
  else if (time_scale == "day"){

    model <- linear_reg(mode = "regression") %>%
      set_engine(engine = "lm") %>%
      fit.model_spec(returns ~., data = train_tbl %>% select(returns, index.num))

  }
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
plot_time_series_portfolio <-
function(data, ggplotly = FALSE, period){
  
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
