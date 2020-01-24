setwd("C:/Users/steph/Dropbox/Business University Science/Personal Portfolio/WIP/All_Seasons_Fund_WIP")
remove(list=ls())
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
               "tibble")

# 1.0 IMPORT DATA ----

symbols <- c("VTI", "TLT", "IEF", "GLD", "DBC")
end     <- "2019-11-30" %>% ymd()
start   <- end - years(5) + days(1)
w <- c(0.3,
       0.4,
       0.15,
       0.075,
       0.075)
wts_tbl <- tibble(symbols, w)
source(file = "00_Scripts/portfolio_multi_period_data.R")
source(file = "00_Scripts/import_FF.R")
source(file = "00_Scripts/simulate_monthly_portfolio.R")

simulation_tbl <- simulation_tbl(sims = 5, symbols = symbols, end = end, start = start, N = 51)
simulation_tbl
simulation_portfolio_monthly <- simulate_monthly_portfolio(sims = 5, symbols = symbols, end = end, start = start, N = 51)
simulation_portfolio_monthly

# Portfolio with highest/lowest return 
max_sim_port_tbl <- simulation_portfolio_monthly %>%
  ungroup() %>% 
  slice(which.max(simulation_portfolio_monthly$investment.growth)) %>% 
  pull(portfolio)

min_sim_port_tbl <- simulation_portfolio_monthly %>% 
  ungroup() %>% 
  slice(which.min(simulation_portfolio_monthly$investment.growth)) %>% 
  pull(portfolio)

simulation_tbl %>% 
  rownames_to_column() %>% 
  filter(rowname %in% c(max_sim_port_tbl, min_sim_port_tbl))


sim_summary <- simulation_portfolio_monthly %>% 
  summarize(final = last(investment.growth)) %>% 
  summarize(
    max = max(final),
    min = min(final),
    median = median(final)
  )

sim_summary

# Plotting
simulation_portfolio_monthly %>% 
  ungroup() %>% 
  mutate(portfolio = as_factor(portfolio)) %>% 
  ggplot(aes(x = date, y = investment.growth, colour = portfolio)) + 
  geom_line(stat = "identity") + 
  theme_tq() +
  theme(legend.position = "none")

# Min Max Plotting
simulation_portfolio_monthly %>% 
  filter(
    last(investment.growth)     == sim_summary$max ||
      last(investment.growth) == sim_summary$min
  ) %>%
  ungroup() %>% 
  mutate(portfolio = as_factor(portfolio)) %>% 
  ggplot(aes(x = date, y = investment.growth, colour = portfolio)) +
  geom_line(stat = "identity")

# Highcharter Plotting 
hchart(simulation_portfolio_monthly, 
       type = 'line',
       hcaes(y = investment.growth,
             x = date,
             group = portfolio)) %>% 
  hc_title(text = "Simulation of All Seasons Portfolio") %>% 
  hc_xAxis(title = "") %>% 
  hc_yAxis(title = list(text = "investment growth"),
           labels = list(format = "${value}")) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_exporting(enabled = TRUE) %>% 
  hc_legend(enabled = FALSE)
