library(quantmod)
library(tidyverse)
library(tidyquant)
library(dygraphs)

#SPY xts
spy <- tq_get('SPY', get  = "stock.prices", from = "1993-02-01") %>%
       tq_transmute(select = adjusted, mutate_fun = to.monthly, indexAt = "yearmon") %>%
       select(date, spy = adjusted) %>%
       timetk::tk_xts(date_col = date)
 
#https://fred.stlouisfed.org/series/TEMPHELPS
TEMPHELPS <- tq_get('TEMPHELPS', get = 'economic.data', from = '1993-02-01') %>%
  mutate(symbol = 'TEMPHELPS') %>%
  tq_transmute(select = price, mutate_fun = to.monthly, indexAt = "yearmon") %>%
  select(date, TEMPHELPS=price) %>%
  timetk::tk_xts(date_col = date)

#Union dfs 
final_df <- cbind(spy, TEMPHELPS)

#Plot 
dygraph(final_df) %>%
    dyAxis("y", label = "SPY") %>%
    dyAxis("y2", label = "TEMPHELPS", independentTicks = TRUE) %>%
    dySeries("TEMPHELPS", axis = 'y2')
  
#Cprrelation
cor_assets <- na.omit(as.data.frame(final_df))
cor(cor_assets)


#Calculate lagging between 2 series





  
  
  
  