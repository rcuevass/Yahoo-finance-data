###
if (!require(BatchGetSymbols)) install.packages('BatchGetSymbols')

library(BatchGetSymbols)
library(tidyverse)

get_symbols <- function(path,file){
  file_name <- paste0(path,file)
  df <- read.csv(file_name)
  list_symbols <- (df$tickers)
  return(list_symbols)
}


get_data_from_symbols <- function(initial.date,
                                  final.date,
                                  frequency.data = 'daily',
                                  list.symbols){
  
  #first.date <- Sys.Date() - 60
  #last.date <- Sys.Date()
  freq.data <- 'daily'
  # set tickers
  tickers <- list.symbols
  
  df <- BatchGetSymbols(tickers = tickers, 
                           first.date = initial.date,
                           last.date = final.date, 
                           freq.data = frequency.data,
                           cache.folder = file.path(tempdir(), 
                                                    'BGS_Cache') ) # cache in tempdir()
  
  # Select only relevant dataframe
  df <- df$df.tickers
  
  # Select needed columns
  df <- df %>%
    dplyr::select(ticker,ref.date,price.close,volume)
  
  
  df <- df %>%
    dplyr::group_by(ticker) %>%
    dplyr::mutate(diff.price.close = price.close - lag(price.close)) %>%
    dplyr::mutate(rel.diff.price.close = 100*diff.price.close/price.close)
  
  return(df)
}


# http://stat545.com/block023_dplyr-do.html

# https://stackoverflow.com/questions/14846547/calculate-difference-between-values-in-consecutive-rows-by-group