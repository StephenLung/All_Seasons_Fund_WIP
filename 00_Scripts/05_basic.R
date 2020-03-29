aggregate_returns_portfolio <-
function(data, period = "monthly"){
  
  output_tbl <- data %>% 
    multi_asset_return_portfolio(period = period) %>% 
    mutate(
      # date = floor_date(date, unit = unit), #setup floor date
           label_text = str_glue("Symbol: {symbol}
                                 Date: {date}
                                 Returns: {scales::percent(returns, accuracy = 0.01)}")) 
  return(output_tbl)
}
plot_time_series <-
function(data, ggplotly = FALSE, facetgrid = FALSE, period = "monthly"){
  
  g <- data %>% 
    ggplot(aes(date, returns, colour = symbol)) +
    
    geom_line() +
    geom_point(aes(text = label_text), size = 0.1) +
    # geom_smooth(method = "loess", span = 0.2) +
    
    theme_tq() +
    scale_color_tq() + 
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = str_glue("Returns of each asset in the portfolio by {period}"),
      subtitle = "Toggle by the timing",
      x = "", 
      y = "") 
    
  
  if(facetgrid == TRUE){
    g <- g + facet_grid(rows = vars(symbol),
                        # scales = "free"
                        )
  }
  
  if(ggplotly == TRUE){
    g <- ggplotly(g, tooltip = "text")
  }
    
  g
  
}
