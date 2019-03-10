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
  
  l.out <- BatchGetSymbols(tickers = tickers, 
                           first.date = initial.date,
                           last.date = final.date, 
                           freq.data = frequency.data,
                           cache.folder = file.path(tempdir(), 
                                                    'BGS_Cache') ) # cache in tempdir()
  
  
  return(l.out)
}