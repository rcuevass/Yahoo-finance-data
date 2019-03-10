source("utilities/functions.R")

list.symbols <- get_symbols(path = "input/",file = "list.csv")


df <- get_data_from_symbols(initial.date = "2018-01-01",
                            final.date = "2019-02-27",
                            list.symbols = list.symbols,
                            filter.price.change = TRUE,
                            threshold.price =  10)
