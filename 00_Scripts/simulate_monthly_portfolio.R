setwd("C:/Users/steph/Dropbox/Business University Science/Personal Portfolio/WIP/All_Seasons_Fund_WIP")

if(!require(pacman)) install.packages("pacman")
pacman::p_load("tidyquant",
               "tidyr",
               "furrr", 
               "plotly",
               "tibble",
               "dplyr",
               "highcharter",
               "purrr",
               "correlationfunnel",
               "dplyr",
               "glue",
               "tibble",
               "truncnorm", 
               "janitor")


simulate_monthly_portfolio <- function(sims, symbols, end, start, N){
  
  starts <- rep(1, sims) %>% 
  # set_names(paste("asset_", 1:sims, sep = ""))
  set_names(paste(symbols))
  
  simulation_func <- function(init_value, N){
  tibble(c(init_value, rtruncnorm(n = N,
                                  a = 0,
                                  b = 1))) 
    }

# tibble with 5 columns
simulation_tbl <- map_dfc(starts, simulation_func, N = N) %>% 
  # set_names(glue("asset_{1:sims}")) %>% 
  set_names(glue("{symbols}")) %>% 
  
  #adding descriptive variable for janitor to work
  rownames_to_column() %>% 
  
  #janitor package to convert value to percentages
  adorn_percentages() %>% 
  
  #check to make sure it sums to 1
  # mutate(sum = rowSums(.[2:6])) %>% 
  as_tibble() %>% 
  select(-rowname)

simulation_vector <- as.vector(t(simulation_tbl)) #t(df) coerces to a numeric matrix, and as.vector removes the names

weights_table <- tibble(symbols) %>% 
  tq_repeat_df(n = N+1) %>% 
  bind_cols(tibble(simulation_vector)) %>% 
  group_by(portfolio)

stock_returns_monthly_multi <- single_portfolio(symbols = symbols, end = end, start = start) %>% 
  tq_repeat_df(n = N+1)

simulation_portfolio_monthly <- stock_returns_monthly_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = monthly.returns,
               weights = weights_table,
               wealth.index = TRUE) %>% 
  mutate(investment.growth = portfolio.wealthindex * 10000)

return(simulation_portfolio_monthly)
}

simulation_tbl <- function(sims, symbols, end, start, N){

starts <- rep(1, sims) %>% 
  # set_names(paste("asset_", 1:sims, sep = ""))
  set_names(paste(symbols))

simulation_func <- function(init_value, N){
  tibble(c(init_value, rtruncnorm(n = N,
                                  a = 0,
                                  b = 1))) 
}

# tibble with 5 columns
simulation_tbl <- map_dfc(starts, simulation_func, N = N) %>% 
  # set_names(glue("asset_{1:sims}")) %>% 
  set_names(glue("{symbols}")) %>% 
  
  #adding descriptive variable for janitor to work
  rownames_to_column() %>% 
  
  #janitor package to convert value to percentages
  adorn_percentages() %>% 
  
  #check to make sure it sums to 1
  # mutate(sum = rowSums(.[2:6])) %>% 
  as_tibble() %>% 
  select(-rowname)

return(simulation_tbl)
}
