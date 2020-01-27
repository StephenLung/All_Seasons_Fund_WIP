pacman::p_load("DT",
               "flexdashboard",
               "shiny",
               "tidyverse",
               "tidyquant",
               "shiny",
               "shinyWidgets",
               "plotly",
               "PortfolioAnalytics")

data(edhec)

symbols <- c("VTI", "TLT", "IEF", "GLD", "DBC")
end     <- "2019-12-31" %>% ymd()
start   <- end - years(5) + days(1)

source(file = "00_Scripts/portfolio_multi_period_data.R")
source(file = "00_Scripts/import_FF.R")
source(file = "00_Scripts/simulate_monthly_portfolio.R")

# 1. Data

returns_xts <- multi_asset_portfolio(symbols = symbols, end = end, start = start) %>% 
  pivot_wider(names_from = "symbol", values_from = "monthly.returns") %>% 
  tk_xts(date_var = date)

fund.names <- colnames(returns_xts)
fund.names

# 2. Portfolio Object

pspec <- portfolio.spec(assets = fund.names)
print.default(pspec)


# 3. Adding Constraints

pspec <- add.constraint(portfolio = pspec,
                        type = "full_investment")
pspec
print(pspec)
summary(pspec)

# 4. Adding Objectives

pspec <- add.objective(portfolio = pspec,
                       type = 'return',
                       name = 'mean')

# [next update: minimize portfolio tail loss with CI 95%] ------
pspec <- add.objective(portfolio = pspec,
                       type = 'risk',
                       name = 'ETL',
                       arguments = list(p=0.95))

print(pspec)
