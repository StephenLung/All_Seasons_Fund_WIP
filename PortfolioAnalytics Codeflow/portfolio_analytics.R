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
pspec <- add.constraint(portfolio = pspec,
                        type = "box",
                        min = 0,
                        max = 1)

pspec
print(pspec)
summary(pspec)

# 4. Adding Objectives

maxret <- add.objective(portfolio = pspec,
                       type = 'return',
                       name = 'mean')

# [next update: minimize portfolio tail loss with CI 95%] ------
maxret <- add.objective(portfolio = pspec,
                       type = 'risk',
                       name = 'ETL',
                       arguments = list(p=0.95))

minvar <- add.objective(portfolio = pspec,
                       type = 'risk',
                       name = "var")

print(pspec)

# 5. Optimize ROI

opt_maxret <- optimize.portfolio(R = returns_xts, portfolio = maxret,
                                 optimize_method = "ROI",
                                 trace = TRUE)
print(opt_maxret)

opt_minvar <- optimize.portfolio(R = returns_xts, portfolio = minvar,
                                 optimize_method = "ROI",
                                 trace = TRUE)

# 6. Plotting risk-return

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

extractWeights(opt_maxret)
