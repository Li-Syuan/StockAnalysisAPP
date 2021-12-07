CycleRSIcross <- function(
  df,
  symbol = 'df',
  tradeSize = 1e+6,
  nFast = 20,
  nSlow = 40
){
  strat_label <- 'CycleRSIcross'
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
  
  add.indicator(strategy = strategy.st, 
                name = 'CycleRSI', 
                arguments = list(x = quote(Cl(mktdata)), 
                                 n = nFast), 
                label = "nFast")
  add.indicator(strategy = strategy.st, 
                name = 'CycleRSI', 
                arguments = list(x = quote(Cl(mktdata)), 
                                 n = nSlow), 
                label = "nSlow")
  
  
  add.signal(strategy = strategy.st,
             name="sigCrossover",
             arguments = list(columns = c("nFast", "nSlow"),
                              relationship = "gte"),
             label = "long.entry")
  add.signal(strategy = strategy.st, 
             name = "sigCrossover",
             arguments = list(columns = c("nFast", "nSlow"),
                              relationship = "lt"),
             label = "long.exit")
  
  
  add.rule(strategy.st, name = "ruleSignal",
           arguments = list(sigcol = "long.entry",
                            sigval = TRUE,
                            ordertype = "market",
                            orderside = "long",
                            replace = FALSE,
                            prefer = "Open",
                            orderqty = 0,
                            osFUN = osMaxDollar,
                            tradeSize = tradeSize,
                            maxSize = tradeSize,
                            TxnFees = "buyFee"),
           type = "enter", path.dep = TRUE, label = "LongEntry")
  add.rule(strategy.st, name='ruleSignal',
           arguments = list(sigcol = "long.exit",
                            sigval = TRUE,
                            orderqty = 'all',
                            ordertype = 'market',
                            orderside = 'long',
                            osFUN = osMaxPos,
                            TxnFees = "sellFee"),
           type = "exit", path.dep = TRUE, label = "LongExit")
  applyStrategy(strategy = strategy.st, portfolios = portfolio.st)
  updatePortf(portfolio.st)
  updateAcct(portfolio.st, Dates = from)
  updateEndEq(account.st)
  # -------------------------------------------------------------------------- #
  TAadd <- cbind(
    CycleRSI(Cl(df),nFast),
    CycleRSI(Cl(df),nSlow)
  )
  colnames(TAadd) <- paste0('CycleRSI',c(nFast,nSlow))
  graph_obj <- chart.Posn.Dygraph(
    Portfolio = portfolio.st, 
    Symbol = symbol,
    TAadd = TAadd
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

