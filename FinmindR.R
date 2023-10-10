library(httr)
library(data.table)
library(xts)
library(magrittr)
use_token = function(){''}
v3data.url <- function(){"https://api.finmindtrade.com/api/v3/data"}
v4data.url <- function(){'https://api.finmindtrade.com/api/v4/data'}
response_to_df <- function(response){
  data = content(response)
  df = data$data %>% do.call('rbind',.)  
  df = try({
    apply(df,2,unlist) %>% as.data.frame()
  },silent = T)
  if(class(df) != 'try-error'){
    return(df)
  }else{
    return(NULL)
  }
}
TW_to_xts <- function(df){
  date <- df$date %>% unlist %>% as.Date()
  open <- df$open %>% unlist %>% as.numeric()
  high <- df$max %>% unlist %>% as.numeric()
  low <- df$min %>% unlist %>% as.numeric()
  close <- df$close %>% unlist %>% as.numeric()
  volume <-df$Trading_Volume %>% unlist %>% as.numeric()
  xts(data.frame(open,high,low,close,volume),order.by = date)
}

US_to_xts <- function(df){
  date <- df$date %>% unlist %>% as.Date()
  open <- df$Open %>% unlist %>% as.numeric()
  high <- df$High %>% unlist %>% as.numeric()
  low <- df$Low %>% unlist %>% as.numeric()
  close <- df$Close %>% unlist %>% as.numeric()
  volume <-df$Volume %>% unlist %>% as.numeric()
  xts(data.frame(open,high,low,close,volume),order.by = date)
}
# ---------------------------------------------------------------------------- #
TaiwanStockInfo <- function(token = ''){
  response = httr::GET(
    url = v4data.url(),
    query = list(
      dataset = "TaiwanStockInfo",
      token = token
    )
  )
  return(response_to_df(response))
}

TaiwanStockPrice <- function(symbol,from = '1980-01-01', to = Sys.Date(),token = ''){
  response = httr::GET(
    url = v4data.url(),
    query = list(
      dataset ="TaiwanStockPrice",
      data_id = symbol,
      start_date = from,
      end_date = to,
      token = token
    )
  ) 
  return(response_to_df(response))
}

USStockInfo <- function(token = ''){
  response = httr::GET(
    url = v3data.url(),
    query = list(
      dataset = "USStockInfo",
      token = token
    )
  )
  return(response_to_df(response))
}

USStockPrice <- function(symbol,from = '1980-01-01',token = ''){
  response = httr::GET(
    url = v3data.url(),
    query = list(
      dataset =  "USStockPrice",
      stock_id = symbol,
      date = from,
      token = token
    )
  ) 
  return(response_to_df(response))
}

CnnFGIndex <- function(){
  response = httr::GET(
    url = v4data.url(),
    query = list(
      dataset =  "CnnFearGreedIndex",
      start_date = '2011-01-01',
      end_date =  Sys.Date()
    )
  ) 
  x <- response_to_df(response)
  x$fear_greed %<>% as.numeric() 
  x$date %<>% as.Date()
  x <- xts(x$fear_greed,order.by = x$date)
  colnames(x) <-'FGindex'
  return(x)
}
# ---------------------------------------------------------------------------- #
# Given Data Set
TaiwanStock_GivenDataSet <- function(
  symbol = '',
  dataset = 'TaiwanStockPER', 
  start_date = '2019-01-01',
  end_date = Sys.Date() - 1,
  token = ''){
  response = httr::GET(
    url = v4data.url(),
    query = list(
      dataset = dataset,
      data_id = symbol,
      start_date = start_date,
      end_date = end_date,
      token = token
    )
  )
  return(response_to_df(response))
}
# 15 dataset
TaiwanDataSet <- function(){
  c(
    'TaiwanStockPER',
    'TaiwanStockDayTrading',
    
    'TaiwanStockMarginPurchaseShortSale',
    'TaiwanStockInstitutionalInvestorsBuySell',
    'TaiwanStockShareholding',
    'TaiwanStockHoldingSharesPer',
    'TaiwanStockSecuritiesLending',
    'TaiwanDailyShortSaleBalances',
    
    'TaiwanStockFinancialStatements',
    'TaiwanStockBalanceSheet',
    'TaiwanStockCashFlowsStatement',
    'TaiwanStockDividend',
    'TaiwanStockDividendResult',
    'TaiwanStockMonthRevenue',
    'TaiwanStockNews'
  )
} 


# TaiwanStock_GivenDataSet('2330',TaiwanDataSet()[1])
# threshold = 0.2
# a <- TaiwanStock_GivenDataSet('2330', 'TaiwanStockMarginPurchaseShortSale')
# b <- TaiwanStock_GivenDataSet('2330','TaiwanStockInstitutionalInvestorsBuySell')
# p <- TaiwanStock_GivenDataSet('2330','TaiwanStockPrice') %>% TW_to_xts()
# new_a <- a %>% dplyr::group_by(date) %>% 
#   dplyr::summarise(
#     a = ShortSaleTodayBalance %>% as.numeric(),
#     b = MarginPurchaseTodayBalance %>% as.numeric(),
#     ratio = a / b,
#     astatus = ifelse(ratio > threshold,1,-1)
#   )
# new_b <- b %>% dplyr::group_by(date) %>%
#   dplyr::summarise(
#     buy = buy %>% as.numeric() %>% sum,
#     sell = sell %>% as.numeric() %>% sum,
#     diff = buy - sell,
#     bstatus = ifelse(diff > 0,1,-1)
#   )
# 
# sig <- merge(new_a,new_b)
# sig %>% subset(astatus == 1 & bstatus == 1)
# sig %>% subset(astatus == -1 & bstatus == -1)
# TaiwanStock_GivenDataSet(
#   symbol = '6116',
#   dataset = 'TaiwanStockHoldingSharesPer',
#   start_date = '2021-11-01')
# 
# 
# TaiwanStock_GivenDataSet(
#   symbol = '6116',
#   dataset = 'TaiwanStockCashFlowsStatement',
#   start_date = '2021-06-30') -> cashflow
# 
# cashflow %>% dplyr::arrange(type) %>% View
# 
# TaiwanStock_GivenDataSet(
#   symbol = '6116',
#   dataset = 'TaiwanStockMonthRevenue',
#   start_date = '2021-06-30')
# TaiwanStock_GivenDataSet(
#   symbol = '6116',
#   dataset = 'TaiwanStockNews',
#   start_date = '2021-11-20') 
