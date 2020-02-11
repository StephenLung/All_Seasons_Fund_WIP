# Modularizing Preprocessing ----
setwd("C:/Users/steph/Dropbox/Business University Science/102 Course/DS4B_102_R_Shiny_Apps_1/Personal Portfolio")
library(pacman)
pacman::p_load("DT",
               "flexdashboard",
               "shiny",
               "tidyverse",
               "tidyquant",
               "shiny",
               "shinyWidgets",
               "plotly",
               "shinyjs")


# 1.0 IMPORT DATA ----

rollback(start - month(1), roll_to_first = TRUE)

symbols <- c("VTI", "TLT", "IEF", "GLD", "DBC")
end     <- "2019-11-30" %>% ymd()
start   <- end - years(5) + days(1)
w <- c(0.3,
       0.4,
       0.15,
       0.075,
       0.075)
wts_tbl <- tibble(symbols, w)
N       <- 100

# Function - pull wealthindex 

symbols %>% 
  
  # Pull the prices
  tq_get(get = "stock.prices",
         from = start,
         to = end) %>% 
  
  # Convert to returns, tq_mutate will not work as its not compatible
  group_by(symbol) %>%
  tq_transmute(select = adjusted, 
               mutate_fun = periodReturn,
               period = "monthly") %>% 
  ungroup() %>% 
  
  #rollback to first day of the month - ETF Issue ----
  mutate(date = lubridate::rollback(date, roll_to_first = TRUE)) %>% 
  
  # Investment growth
  tq_portfolio(assets_col = symbol,
               returns_col = monthly.returns,
               weights = wts_tbl,
               wealth.index = TRUE) %>% 
  mutate(investment.growth = portfolio.wealthindex * 10000) %>% 
  
  # Addition of base row with 10K starting point
  add_row(date = rollback(start - month(1), roll_to_first = TRUE),
          portfolio.wealthindex = 1,
          investment.growth = 10000, .before = 1)


input$asset_name_2 %>%   
  tq_get(
    get = "stock.prices",
    from = input$date_range[1],
    to = input$date_range[2]) %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = input$return_timing) %>% 
  ungroup() %>% 
  #rollback to first day of the month - ETF Issue ----
mutate(date = lubridate::rollback(date, roll_to_first = TRUE)) %>% 
  tq_repeat_df(n = 6) %>% 
  # Investment growth
  tq_portfolio(assets_col = symbol,
               returns_col = monthly.returns,
               weights = weights_tbl,
               wealth.index = TRUE) %>% 
  mutate(investment.growth = portfolio.wealthindex * 10000)