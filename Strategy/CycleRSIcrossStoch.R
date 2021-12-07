CycleRSIcrossStoch <- function(
  df,
  symbol = 'df',
  tradeSize = 1e+6,
  nRSI = 10,
  nStoch = 10,
  opposite = FALSE
){
  strat_label <- 'CycleRSIcrossStoch'
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
                                 n = nRSI), 
                label = "nRSI")
  add.indicator(strategy = strategy.st, 
                name = 'CycleStoch', 
                arguments = list(x = quote(Cl(mktdata)), 
                                 n = nStoch), 
                label = "_")
  add.indicator(strategy.st,
                name = 'prod100',
                arguments = list(x = quote(mktdata[,'CycleStoch._'])),
                label = 'nStoch'
                )
  
  column <- c('nRSI','nStoch')
  if(opposite) column <- column[2:1]
  add.signal(strategy = strategy.st,
             name="sigCrossover",
             arguments = list(columns = column,
                              relationship = "gte"),
             label = "long.entry")
  add.signal(strategy = strategy.st, 
             name = "sigCrossover",
             arguments = list(columns = column,
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
    CycleStoch(Cl(df),nStoch) * 100,
    CycleRSI(Cl(df),nRSI)
  )
  colnames(TAadd) <- c(paste0('CycleRSI',nRSI),paste0('CycleStoch',nStoch))
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

