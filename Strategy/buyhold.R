buyhold <- function(df,symbol = 'df',tradeSize = 1e+6){
  strat_label <- 'buyhold'
  from = index(df)[1]
  # -------------------------------------------------------------------------- #
  currency("TWD")
  Sys.setenv(TZ = "UTC")
  stock(symbol, currency = "TWD", multiplier = 1)
  # initialize strategy
  strategy.st <- portfolio.st <- account.st <- strat_label 
  rm.strat(portfolio.st)
  rm.strat(strategy.st)
  initDate <- from
  initEq <- tradeSize
  initPortf(portfolio.st, 
            symbols = symbol, 
            initDate = initDate, 
            currency = "TWD")
  initAcct(account.st, 
           portfolios = portfolio.st, 
           initDate = initDate, 
           currency = "TWD", 
           initEq = initEq)
  initOrders(portfolio.st, 
             initDate = initDate)
  strategy(strategy.st, store = TRUE)
  
  
  
  # Steps 2-4: Applying trading rule ----------------------------------------
  
  # add the transaction to buy at the beginning:
  FirstDate <- first(index(df))
  # Enter order on the first date

  BuyDate <- FirstDate
  equity = getEndEq(strat_label, FirstDate)
  FirstPrice <- as.numeric(Cl(df[BuyDate,]))
  UnitSize = as.numeric(trunc(equity/FirstPrice))
  Fee = abs(UnitSize) * FirstPrice * -0.001425
  addTxn(strat_label, Symbol = symbol, 
         TxnDate = BuyDate, TxnPrice = FirstPrice,
         TxnQty = UnitSize, TxnFees = Fee)
  
  
  # add the transaction to sell at the end:
  
  LastDate <- last(index(df))
  
  # Exit order on the Last Date
  LastPrice <- as.numeric(Cl(df[LastDate,]))
  Fee = abs(UnitSize) * FirstPrice * -0.004425
  addTxn(strat_label, Symbol = symbol, 
         TxnDate = LastDate, TxnPrice = LastPrice,
         TxnQty = -UnitSize , TxnFees = Fee)
  
  
  # Step 5: Evaluation ------------------------------------------------------
  updatePortf(portfolio.st)
  updateAcct(portfolio.st, Dates = from)
  updateEndEq(account.st)
  # -------------------------------------------------------------------------- #

  graph_obj <- chart.Posn.Dygraph(
    Portfolio = portfolio.st, 
    Symbol = symbol
  )
  tStats <- tradeStats(Portfolios = portfolio.st, use = "trades", inclZeroDays = FALSE)  
  tab.profit <- tStats %>% tStats.profit
  tab.wins <- tStats %>% tStats.wins 
  tab.rets <- PortfReturns(Account = account.st) %>% account.rets
  return(
    list(
      win_info = tab.wins,
      profit_info = tab.profit,
      return_info = tab.rets %>% t,
      pertrade_info = perTradeStats(portfolio.st,Symbol = symbol),
      graph_obj = graph_obj,
      MAE = plot.ME(portfolio.st,Symbol = symbol,type = "MAE", scale = "percent"),
      MFE = plot.ME(portfolio.st,Symbol = symbol,type = "MFE", scale = "percent")
    )
  )
}

