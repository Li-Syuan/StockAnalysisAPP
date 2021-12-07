TVIcross <- function(
  df,
  symbol = 'df',
  tradeSize = 1e+6,
  period = 20,
  delta = 0.2,
  opposite = FALSE
){
  strat_label <- 'TVIcross'
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
  
  #indicators
  add.indicator(strategy.st, name="TVI", 
                arguments = list(
                  x = quote(Cl(mktdata)), 
                  period = period, 
                  delta = delta), 
                label = "TVI")
  
  label <- c('long.entry','long.exit')
  if(opposite) label <- label[2:1]
  add.signal(strategy.st, name="sigCrossover",
             arguments=list(columns=c("vigor.TVI","trigger.TVI"),
                            relationship="gt"),
             label = label[1])
  add.signal(strategy.st, name="sigCrossover",
             arguments=list(columns=c("vigor.TVI","trigger.TVI"),
                            relationship="lt"),
             label = label[2])
  
  #signals
  # add.signal(strategy.st, name = "sigThreshold", 
  #            arguments=list(
  #              threshold = 1, 
  #              column = "vigor.TVI",
  #              relationship="gte", cross=FALSE),
  #            label="TVIgtThresh")
  # add.signal(strategy.st, name="sigComparison",
  #            arguments=list(
  #              columns = c("vigor.TVI","trigger.TVI"), 
  #              relationship = "gt"),
  #            label="TVIgtLag")
  # add.signal(strategy.st, name = "sigAND",
  #            arguments=list(
  #              columns=c("TVIgtThresh","TVIgtLag"),
  #              cross=TRUE),
  #            label = label[1])
  # add.signal(strategy.st, name="sigCrossover",
  #            arguments=list(
  #              columns = c("vigor.TVI","trigger.TVI"),
  #              relationship="lt"),
  #            label = label[2])
  
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
  TAadd <- TVI(Cl(df),period = period,delta = delta)
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

