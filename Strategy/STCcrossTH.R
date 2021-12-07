STCcrossTH <- function(
  df,
  symbol = 'df',
  tradeSize = 1e+6,
  nShort=23,
  nLong=50, 
  nCycle = 10,
  nK = 3, 
  nD = 3,
  LB = 25,
  UB = 75,
  opposite = F
){
  strat_label <- 'STC'
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
  
  add.indicator(strategy.st, name = 'SchaffTrendCycle',
                arguments =
                  list(
                    x = quote(Cl(mktdata)),
                    nShort = nShort,
                    nLong = nLong,
                    nCycle = nCycle,
                    nK = nK,
                    nD = nD
                  ),
                label = strat_label)
  add.indicator(strategy.st, name = 'cross_threshold',
                arguments =
                  list(
                    x = quote(mktdata[,grepl("stc",colnames(mktdata))]),
                    LB = LB,
                    UB = UB
                  ),
                label = '_')
  
  cross_name <- c('up.cross.UB', 'dn.cross.UB', 'up.cross.LB', 'dn.cross.LB')
  cross_name <- paste0(cross_name, '._')
  buy_index <- c(F,T,F,T)
  if(opposite)buy_index <- !buy_index
  sell_index <- !buy_index
  buy_name <- cross_name[buy_index]
  sell_name <- cross_name[sell_index]
  add.signal(strategy.st, name = "sigFormula",
             arguments = list(columns = buy_name,
                              formula = paste0(buy_name,collapse = '|')),
             label = "long.entry")
  add.signal(strategy.st, name = "sigFormula",
             arguments = list(columns = sell_name,
                              formula = paste0(sell_name,collapse = '|')),
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
  stc <- SchaffTrendCycle(
    Cl(mktdata),
    nShort = nShort,
    nLong = nLong,
    nCycle = nCycle,
    nK = nK,
    nD = nD
  ) 
  stc <- cbind(stc,LB,UB)
  colnames(stc)[-1] <- c('LB','UB')
  graph_obj <- chart.Posn.Dygraph(
    Portfolio = portfolio.st, 
    Symbol = symbol,
    TAadd = stc 
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
