pacman::p_load("DT",
               "flexdashboard",
               "shiny",
               "tidyverse",
               "tidyquant",
               "shiny",
               "shinyWidgets",
               "plotly",
               "PortfolioAnalytics",
               "DEoptim",
               "ROI",
               "ROI.plugin.glpk",
               "ROI.plugin.quadprog")

data(edhec)

symbols <- c("VTI", "TLT", "IEF", "GLD", "DBC")
end     <- "2019-12-31" %>% ymd()
start   <- end - years(10) + days(1)

source(file = "00_Scripts/portfolio_multi_period_data.R")
source(file = "00_Scripts/import_FF.R")
source(file = "00_Scripts/simulate_monthly_portfolio.R")

# 1. Data

# Pull financial data and convert into xts format
returns_xts <- multi_asset_portfolio(symbols = symbols, end = end, start = start) %>% 
  pivot_wider(names_from = "symbol", values_from = "monthly.returns") %>% 
  tk_xts(date_var = date)

fund.names <- colnames(returns_xts)
fund.names

# 2. Portfolio Object

# Name the portfolio object
pspec <- portfolio.spec(assets = fund.names)
print.default(pspec)


# 3. Adding Constraints

pspec <- add.constraint(portfolio = pspec,
                        type = "full_investment")
pspec <- add.constraint(portfolio = pspec,
                        type = "long_only") #same as type = "long_only"
# type = "box",
# min = 0,
# max = 1

pspec
print(pspec) #concise summary
summary(pspec) #detailed summary

# 4. Adding Objectives

# Objective to maximize return
pspec <- add.objective(portfolio = pspec,
                       type = 'return',
                       name = 'mean')

# Objective to minimize risk
pspec <- add.objective(portfolio = pspec,
                        type = "risk",
                        name = "StdDev")

# [next update: minimize portfolio tail loss with CI 95%] ------
# maxret <- add.objective(portfolio = pspec,
#                        type = 'risk',
#                        name = 'ETL',
#                        arguments = list(p=0.95))

minvar <- add.objective(portfolio = pspec,
                       type = 'risk',
                       name = "var")

print(pspec)

# 5. Optimize ROI
# Optimize using ROI package for best return and risk
opt_maxret <- optimize.portfolio(R = returns_xts, portfolio = pspec,
                                 optimize_method = "ROI",
                                 trace = TRUE)

opt_minvar <- optimize.portfolio(R = returns_xts, portfolio = minvar,
                                 optimize_method = "ROI", 
                                 trace = TRUE)

# Optimize using ROI package for best sharpe ratio (maximize mean return per unit of stdDev)
maxSR_opt_maxret <- optimize.portfolio(R = returns_xts, portfolio = pspec,
                                       optimize_method = "ROI",
                                       maxSR = TRUE, trace = TRUE)

# 6. Plotting risk-return

# Plotting the optimize Sharpe Ratio function
plot(maxSR_opt_maxret, risk.col = "StdDev", return.col = "mean",
     main = "Sharpe Ratio Optimization", chart.assets = TRUE)
chart.RiskReward(maxSR_opt_maxret, risk.col = "StdDev", return.col = "mean")

# Plotting the maximize return function
plot(opt_maxret, risk.col = "StdDev", return.col = "mean",
     main = "Maximum Return Optimization", chart.assets = TRUE)


optim_1 <- opt_maxret$weights %>% 
  as_tibble() %>% 
  add_column("optimization" = 1)

plot(opt_minvar, risk.col = "StdDev", return.col = "mean",
     main = "Minimum Variance Optimization", chart.assets = TRUE)


optim_2 <- opt_minvar$weights

opt_minvar$weights
opt_maxret$weights
plot()

chart.RiskReward()

# 7. Extract the weights from the optimize function

print(maxSR_opt_maxret)
extractWeights(maxSR_opt_maxret)
print(opt_maxret)
extractWeights(opt_maxret)
