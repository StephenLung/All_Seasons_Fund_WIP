
p_load(dplyr,
       rlang)

symbols <- c("VTI", "TLT", "IEF", "GLD", "DBC")
end     <- today()
start   <- end - years(20) + days(1)
w       <- c(0.3,
             0.4,
             0.15,
             0.075,
             0.075)
wts_tbl <- tibble(symbols, w)

# Pulls a full list of daily stock prices ----
multi_asset_price_portfolio <- function(symbols, end, start, wts_tbl){
  
  # Download Data
  download_data <- symbols %>% 
    tq_get(get = "stock.prices",
           from = start,
           to = end) 
  
  # Determine earliest date with full set of data
  min_date <- download_data %>%   
    group_by(symbol) %>% 
    summarise(min_date = min(date), 
              max_date = max(date)) %>% 
    ungroup() %>% 
    pull(min_date) %>% 
    max()
  
  stock_data <- download_data %>%
    select(symbol, date, adjusted) %>% 
    filter(date >= min_date)
  
  return(stock_data)

  
}

stock_price_tbl <- multi_asset_price_portfolio(symbols, end, start, wts_tbl)

# Transmutes daily stock prices to returns by specified period ----
multi_asset_return_portfolio <- function(stock_price_tbl, period = "monthly"){
    
    # Transform to Returns
    period_return_tbl <- stock_price_tbl %>% 
      select(symbol, date, adjusted) %>% 
      group_by(symbol) %>% 
      tq_transmute(select     = adjusted,
                   mutate_fun = periodReturn,
                   period     = period,
                   col_rename = "returns") %>% 
      ungroup() %>% 
      #rollback to first day of the month - ETF Issue ----
    mutate(date = lubridate::rollback(date, roll_to_first = TRUE))
    
    return(period_return_tbl)
    
    
}

# Creates a portfolio given the weights ----
wealth_index <- function(return_data, wts_tbl, name_portfolio){
  
  name_portfolio <- as_name(name_portfolio)
  
  return_data %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = returns,
               weights = wts_tbl,
               wealth.index = TRUE) %>% 
    mutate(investment.growth = portfolio.wealthindex * 10000) %>% 
    add_column(portfolio = name_portfolio, .before = 1)
}

# Plot the portfolio ----


# All seasons portfolio ----
all_seasons_data <- multi_asset_price_portfolio(symbols, end, start, wts_tbl) %>% 
  multi_asset_return_portfolio(period = "monthly") %>% 
  wealth_index(wts_tbl = wts_tbl, name_portfolio = "all_seasons")

# Start Date of all seasons portfolio
min_date <- all_seasons_data %>%   
  summarise(min_date = min(date), 
            max_date = max(date)) %>% 
  pull(min_date)


