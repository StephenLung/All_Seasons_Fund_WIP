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
end     <- "2019-12-31" %>% ymd()
start   <- end - years(10) + days(1)
w       <- c(0.3,
       0.4,
       0.15,
       0.075,
       0.075)
wts_tbl <- tibble(symbols, w)
N       <- 100
source(file = "00_Scripts/portfolio_multi_period_data.R")
source(file = "00_Scripts/import_FF.R")
source(file = "00_Scripts/simulate_monthly_portfolio.R")


benchmark_tbl <- multi_asset_portfolio(symbols = symbols, end = end, start = start, wts_tbl = wts_tbl) %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = monthly.returns,
               weights = wts_tbl,
               wealth.index = TRUE) %>% 
  mutate(investment.growth = portfolio.wealthindex * 10000) %>% 
  add_column(portfolio = "Benchmark", .before = 1)

simulation_tbl <- simulation_tbl(sims = 5, symbols = symbols, end = end, start = start, N = N)
simulation_tbl
simulation_portfolio_monthly <- simulate_monthly_portfolio(sims = 5, symbols = symbols, end = end, start = start, N = N)
simulation_portfolio_monthly %>% 
  slice(60)

simulation_portfolio_monthly_xts <- simulation_portfolio_monthly %>% 
  ungroup() %>% 
  mutate(portfolio = as.character(portfolio)) %>% 
  rbind(benchmark_tbl) %>% 
  pivot_wider(names_from = "portfolio", values_from = "investment.growth", -portfolio.wealthindex) %>% 
  # convert to xts functions
  timetk::tk_xts(date_var = date)

# Portfolio with highest/lowest return 
benchmark_return_tbl <- wts_tbl %>% 
  pivot_wider(names_from = "symbols", values_from = "w") %>% 
  add_column(Portfolio = "Benchmark") %>% 
  select(Portfolio, everything())
benchmark_return_tbl


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
  filter(rowname %in% c(max_sim_port_tbl, min_sim_port_tbl)) %>% 
  rename(Portfolio = rowname) %>% 
  rbind(benchmark_return_tbl)


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

simulation_portfolio_monthly %>% 
  slice(120)

# Highcharter Plotting 
hchart(simulation_portfolio_monthly, 
       type = "line",
       hcaes(y = investment.growth,
             x = date,
             group = portfolio)) %>% 
  hc_title(text = glue("Simulation of {N} Portfolio")) %>% 
  hc_xAxis(title = "") %>% 
  hc_yAxis(title = list(text = "Investment Growth"),
           labels = list(format = "${value}")) %>% 
  hc_tooltip(pointFormat =
               "<span style=\"color:{series.color}\">{series.name}</span>:<b>${point.y:,.0f}</b><br/>",
             shared=TRUE) %>% 
  # hc_add_theme(hc_theme_flat()) %>% 
  hc_exporting(enabled = TRUE) %>% 
  hc_legend(enabled = FALSE) %>% 
  hc_add_theme(hc_theme_smpl())

highchart(type = "stock") %>%
  hc_add_series(simulation_portfolio_monthly_xts[,max_sim_port_tbl],
                name = "Maximum Return") %>% 
  hc_add_series(simulation_portfolio_monthly_xts[,min_sim_port_tbl],
                name = "Minimum Return") %>% 
  hc_add_series(simulation_portfolio_monthly_xts[,"Benchmark"],
                name = "Benchmark Return") %>% 
  hc_tooltip(pointFormat =
               "<span style=\"color:{series.color}\">{series.name}</span>:<b>${point.y:,.0f}</b><br/>",
             shared=TRUE) %>%
  hc_title(text = "Best & Worst Return Simulation vs Benchmark Return") %>% 
  hc_yAxis(title = list(text = "Investment Growth"),
           labels = list(format = "${value}")) %>% 
  hc_exporting(enabled = TRUE) %>% 
  hc_add_theme(hc_theme_chalk())

# Clean Wealth Index Plot ----
symbols <- c("VTI", "TLT", "IEF", "GLD", "DBC")
end <- "2019-06-30" %>% ymd()
start <- end - years(30) + days(1)
w       <- c(0.3,
             0.4,
             0.15,
             0.075,
             0.075)
wts_tbl <- tibble(symbols, w)
w_2 <- c(0.3, 0.4, 0.15, 0.075, 0.075,
         1, 0, 0, 0, 0,
         0, 1, 0, 0, 0,
         0, 0, 1, 0, 0,
         0, 0, 0, 1, 0,
         0, 0, 0, 0, 1)

# Duplicating symbols over 6 rows
weights_tbl <- tibble(symbols) %>%
  tq_repeat_df(n = 6) %>% 
  bind_cols(tibble(w_2)) %>% 
  group_by(portfolio) 

# Transmute to monthly returns, filter date, and repeat 6 rows
raw_data <- symbols %>% 
  tq_get(get = "stock.prices",
         from = "2006-02-01",
         to = end) %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly") %>% 
  ungroup() %>% 
  #rollback to first day of the month - ETF Issue ----
  mutate(date = lubridate::rollback(date, roll_to_first = TRUE)) %>% 
  tq_repeat_df(n = 6) %>% 

# Investment growth
  tq_portfolio(assets_col = symbol,
               returns_col = monthly.returns,
               weights = weights_tbl,
               wealth.index = TRUE) %>% 
  mutate(investment.growth = portfolio.wealthindex * 10000) %>% 

# Adjusting portfolio # to names
  ungroup() %>% 
  mutate(portfolio = case_when(portfolio == 1 ~ "All Seasons Portfolio",
                             portfolio == 2 ~ "VTI",
                             portfolio == 3 ~ "TLT",
                             portfolio == 4 ~ "IEF",
                             portfolio == 5 ~ "GLD",
                             portfolio == 6 ~ "DBC")) %>% 
  mutate(portfolio = as.factor(portfolio))

end_port_returns_investment_tbl <- last(port_returns_investment_tbl$date)

raw_data %>%
  ggplot(aes(x = date, y = investment.growth, colour = portfolio)) + 
  geom_line(stat = "identity") + 
  geom_smooth(method = "loess") + 
  theme_tq() + 
  scale_color_tq() + 
  scale_y_continuous(labels = scales::dollar) + 
  labs(title = "All Seasons Fund Portfolio Growth vs Standalone Security Growth",
       subtitle = "40% TLT, 30% VTI, 15% IEF, 7.5% GLD and 7.5% DBC",
       caption = "Ray Dalio's All Weather Funds",
       x = "",
       y = "Investment Growth") +
  annotate(geom = "text",
           x = end_port_returns_investment_tbl,
           y = 24000,
           label = "Portfolio",
           fontface = "plain")
