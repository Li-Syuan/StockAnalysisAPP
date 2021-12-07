ADXcross <- function(
  df,
  symbol = 'df',
  tradeSize = 1e+6,
  n = 14,
  opposite = F
){
  strat_label <- 'ADX'
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
  
  add.indicator(strategy.st, name = "ADX", 
                arguments = list(HLC = quote(HLC(mktdata)), n = n), 
                label="ADX")
  relation <- c('gt','lt')
  if(opposite) relation <- c('lt','gt')
  add.signal(
    strategy.st, 
    name = "sigComparison",
    arguments = list(columns = c("DIp.ADX","DIn.ADX"), relationship = relation[1]),
    label = "long.entry")
  add.signal(
    strategy.st, 
    name = "sigComparison",
    arguments = list(columns = c("DIp.ADX","DIn.ADX"), relationship = relation[2]), 
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
  adx <- ADX(HLC(df),n = n)
  graph_obj <- chart.Posn.Dygraph(
    Portfolio = portfolio.st, 
    Symbol = symbol,
    TAadd = adx
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
