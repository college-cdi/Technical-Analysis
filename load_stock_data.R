library(quantmod)

load_stock_data<-function(file)
{
  symbols<-readLines(file)
  stocks<-list()
  
  for(symbol in symbols) {
    stocks[[symbol]]<-
  getSymbols(
        symbol,src="yahoo",
  auto.assign=FALSE
      )
  }
  return(stocks)
}
 stocks<-
load_stock_data("portfolio.txt")
 names(stocks)
 
 calculate_statistics <-
 function(stock_data) {
   closing_prices <-
 as.numeric(Cl(stock_data))
   data.frame(
     mean = mean(closing_prices, na.rm = TRUE),
      median = median(closing_prices, na.rm = TRUE),
      sd = sd(closing_prices, na.rm = TRUE)
   )
 }
 stock_statistics<- lapply(stocks, calculate_statistics)
 stock_statistics
 
 display_stock <- function(symbol, stocks) {
   stock <- stocks[[symbol]]
   
  print(head(as.data.frame(stock)))
    chartSeries(stock, name = symbol)
 }
 display_stock(names(stocks)[1], stocks)