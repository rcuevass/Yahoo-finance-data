if (!require(BatchGetSymbols)) install.packages('BatchGetSymbols')
if (!require(tidyverse)) install.packages('tidyverse')

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
                                  list.symbols,
                                  filter.price.change=TRUE,
                                  threshold.price = 10,
                                  time.window=5){
  
  # set tickers
  tickers <- list.symbols
  
  # Collect data from Yahoo
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
  
  # Add changes in price with respect present price
  df <- df %>%
    dplyr::group_by(ticker) %>%
    dplyr::mutate(diff.price.close = price.close - lag(price.close)) %>%
    dplyr::mutate(price.close.prev.date = lag(price.close)) %>%
    dplyr::mutate(rel.diff.price.close = 100*diff.price.close/price.close.prev.date) %>%
    dplyr::select(ticker,
                  ref.date,
                  price.close,
                  price.close.prev.date,
                  rel.diff.price.close)
  
  # If requested, get reduced dataframe for price changes of more than 
  # or equal to a given threshold
  if (filter.price.change == TRUE) {
    df <- df %>%
      dplyr::filter(abs(rel.diff.price.close) >= threshold.price)
    
  }
  
  # Add max and min dates to each date contained in the dataset
  df <- df %>%
    dplyr::mutate(max.date = ref.date + time.window,
                  min.date = ref.date - time.window)
  
  return(df)
}