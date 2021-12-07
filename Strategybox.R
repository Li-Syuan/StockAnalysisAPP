library(dygraphs)
library(highcharter)
library(RColorBrewer)
if(!exists('TWSinfo'))assign('TWSinfo',TaiwanStockInfo(),envir = .GlobalEnv)
if(!exists('USSinfo'))assign('USSinfo',USStockInfo(),envir = .GlobalEnv)
if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env()
cross_threshold <- function(x, LB = 25,UB = 75){
  x_gt_UB <- x > UB
  x_lt_UB <- x < UB
  
  x_gt_LB <- x > LB
  x_lt_LB <- x < LB
  
  UP_Cross_UB <- diff(x_gt_UB) == 1
  DN_Cross_UB <- diff(x_lt_UB) == 1
  UP_Cross_LB <- diff(x_gt_LB) == 1
  DN_Cross_LB <- diff(x_lt_LB) == 1
  
  cross_signal <- cbind(UP_Cross_UB, DN_Cross_UB, UP_Cross_LB, DN_Cross_LB)
  colnames(cross_signal) <- c('up.cross.UB', 'dn.cross.UB', 'up.cross.LB', 'dn.cross.LB')
  return(cross_signal)
}
# custom transaction fee function based on value of transaction
buyFee <- function(TxnQty, TxnPrice, Symbol, ...){
  # buyCost <- 0.001425
  # buyCost <- 0
  abs(TxnQty) * TxnPrice * -buyCost
}
sellFee <- function(TxnQty, TxnPrice, Symbol, ...){
  # sellCost <- 0.004425
  # sellCost <- 0
  abs(TxnQty) * TxnPrice * -sellCost
}
# ----------------------------------------------------------------------------- #
# Portfolio = portfolio.st
# Symbol = 'df'
chart.Posn.Dygraph <- function(
  Portfolio, Symbol,
  Dates = NULL, title = '', 
  env = .GlobalEnv,
  TAonChart = NULL,
  TAadd = NULL) {
  pname <- Portfolio
  Portfolio <- getPortfolio(pname)
  if (missing(Symbol)) {
    Symbol <- ls(Portfolio$symbols)[[1]]
  }else {
    Symbol <- Symbol[1]
  }
  Prices = get(Symbol, envir = env)
  if (!is.OHLC(Prices)) {
    if (hasArg(prefer)) 
      prefer = eval(match.call(expand.dots = TRUE)$prefer)
    else prefer = NULL
    Prices = getPrice(Prices, prefer = prefer)
  }
  freq = periodicity(Prices)
  switch(
    freq$scale, 
    seconds = {
      mult = 1
    }, minute = {
      mult = 60
    }, hourly = {
      mult = 3600
    }, daily = {
      mult = 86400
    }, {
      mult = 86400
    }
  )
  if (!isTRUE(freq$frequency * mult == round(freq$frequency, 0) * mult)) {
    n = round((freq$frequency/mult), 0) * mult
  }else {
    n = mult
  }
  tzero = xts(0, order.by = index(Prices[1, ]))
  if (is.null(Dates)) Dates <- paste(first(index(Prices)), last(index(Prices)), sep = "::")
  Portfolio$symbols[[Symbol]]$txn <- Portfolio$symbols[[Symbol]]$txn[Dates]
  Portfolio$symbols[[Symbol]]$posPL <- Portfolio$symbols[[Symbol]]$posPL[Dates]
  Trades = Portfolio$symbols[[Symbol]]$txn$Txn.Qty
  Buys = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades >  0)]
  Sells = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades < 0)]
  Position = Portfolio$symbols[[Symbol]]$txn$Pos.Qty
  if (nrow(Position) < 1) 
    stop("no transactions/positions to chart")
  if (as.POSIXct(first(index(Prices))) < as.POSIXct(first(index(Position)))) 
    Position <- rbind(xts(0, order.by = first(index(Prices) - 1)), Position)
  Positionfill = na.locf(merge(Position, index(Prices)))
  CumPL = cumsum(Portfolio$symbols[[Symbol]]$posPL$Net.Trading.PL)
  if (length(CumPL) > 1) {
    CumPL = na.omit(na.locf(merge(CumPL, index(Prices))))
  }else{
    CumPL = NULL
  }
  if (!is.null(CumPL)) {
    CumMax <- cummax(CumPL)
    Drawdown <- -(CumMax - CumPL)
    Drawdown <- rbind(xts(-max(CumPL), order.by = first(index(Drawdown) - 1)), Drawdown)
  } else {
    Drawdown <- NULL
  }
  if (!is.null(Dates)) Prices = Prices[Dates]
  # highcharter
  Buys = merge(Buys, Position)
  Buys = Buys[!is.na(Buys$Txn.Price), ]
  
  Sells = merge(Sells, Position)
  Sells = Sells[!is.na(Sells$Txn.Price), ]
  
  pre.Posn <- NULL
  comb <- merge(Sells[, 1], Buys)
  for(t in which(!is.na(comb[, 1])))
    pre.Posn[t] <- comb[t - 1, 3]
  pre.Posn <- xts(pre.Posn[!is.na(pre.Posn)], time(Sells))
  ### add text on each flag
  
  hc <- highchart(type = "stock",height = 675) %>% 
    hc_yAxis_multiples(create_yaxis(5, height = c(7, 2, 2, 2, 2, 2, 2), turnopposite = TRUE)) %>%
    hc_title(text = 'Simulate Trading Graph') %>% 
    hc_add_series(round(Prices, 2), yAxis = 0, name = title, type = "candlestick",id = 1) %>%
    hc_add_series(Positionfill, yAxis = 1, name = "Position", color = "darkblue", type = "column") %>%
    hc_add_series(round(CumPL, 2), yAxis = 2, name = "CumPL", color = "darkgreen", fillOpacity = 0.3)   %>%
    hc_add_series(round(Drawdown, 2), yAxis = 3, name = "Drawdown", color = "darkred", fillOpacity = 0.3)
  
  color_list <- brewer.pal(9,'Set1')
  if(!is.null(TAonChart)){
    for(i in 1:ncol(TAonChart)){
      hc <- hc %>% 
        hc_add_series(TAonChart[,i], yAxis = 0,name = colnames(TAonChart)[i], color = color_list[i])
    }
  }
  
  if(!is.null(TAadd)){
    for(i in 1:ncol(TAadd)){
      hc <- hc %>% 
        hc_add_series(round(TAadd[,i],4),yAxis = 4,name = colnames(TAadd)[i], color = color_list[i])
    }
  }
  Buy_flags <- data.frame(
    date = index(Buys[, 1]) %>% as.Date(),
    title = 'Buy',
    text = paste0("Price: ", round(Buys[, 1], 2),
                  "<br>Position: ", Buys[, 2], 
                  "<br>Cost: ", round(Buys[, 1] * Buys[, 2] - buyFee(Buys[, 2], Buys[, 1]), 0)),
    shape = "squarepin"
  )
  Sell_flags <- data.frame(
    date = index(Sells[,1 ]) %>% as.Date(),
    title = 'Sell',
    text = paste0("Price: ", round(Sells[, 1], 2), 
                  "<br>Position: ", Sells[, 2],
                  "<br>Revenue: ", 
                  pre.Posn * Sells[, 1] + sellFee(pre.Posn, Sells[, 1])),
    shape = "squarepin"
  )
  hc %>% 
    hc_add_series(
      Buy_flags, 
      hcaes(x = date),
      type = "flags", 
      onSeries = 1
    ) %>% 
    hc_add_series(
      Sell_flags, 
      hcaes(x = date),
      type = "flags", 
      onSeries = 1
    ) %>%
    hc_rangeSelector(inputEnabled = TRUE, inputBoxBorderColor = list(fill = "red")) %>% 
    hc_scrollbar(enabled = TRUE) %>%
    hc_add_theme(hc_theme_538())
}


tStats.profit <- function(tStats){
  tStats %>% 
    dplyr::select(
      Portfolio,
      Symbol,
      Num.Trades,
      Net.Trading.PL, 
      Gross.Profits, 
      Gross.Losses, 
      Profit.Factor)
}

tStats.wins <- function(tStats){
  tStats %>% 
    dplyr::select(
      Portfolio,
      Avg.Trade.PL, 
      Avg.Win.Trade, 
      Avg.Losing.Trade, 
      Avg.WinLoss.Ratio,
      Percent.Positive,
      Percent.Negative,
      Largest.Winner,
      Largest.Loser
    )
}

account.rets <- function(rets){
  table.Arbitrary(
    rets,
    metrics=c(
      "Return.cumulative",
      "Return.annualized",
      "SharpeRatio.annualized",
      "SortinoRatio",
      "CalmarRatio",
      "AverageRecovery",
      "StdDev.annualized",
      "maxDrawdown"),
    metricsNames=c(
      "Cumulative.Return",
      "Annualized.Return",
      "Annualized.Sharpe.Ratio",
      "Sortino.Ratio",
      "Calmar.Ratio",
      "Average.Recovery",
      "Annualized.StdDev",
      "Max.DrawDown")
  ) %>% round(3)
}
prod100 <- function(x) x * 100
# Portfolio <- portfolio.st
# Symbol <- 'df'
# type <- 'MFE'
# scale <- 'percent'

plot.ME <- function(
  Portfolio, 
  Symbol = 'df', 
  type = c("MAE", "MFE"), 
  scale = c("cash", "percent", "tick"),...
){
  type = type[1]
  scale = scale[1]
  trades <- perTradeStats(Portfolio, Symbol, ...)
  trades$Pct.Net.Trading.PL <- 100 * trades$Pct.Net.Trading.PL
  trades$Pct.MAE <- 100 * trades$Pct.MAE
  trades$Pct.MFE <- 100 * trades$Pct.MFE
  profitable <- (trades$Net.Trading.PL > 0)
  switch(scale, cash = {
    .ylab <- "Profit/Loss (cash)"
    if (type == "MAE") {
      .cols <- c("MAE", "Net.Trading.PL")
      .xlab <- "Drawdown (cash)"
      .main <- "Maximum Adverse Excursion (MAE)"
    } else {
      .cols <- c("MFE", "Net.Trading.PL")
      .xlab <- "Run Up (cash)"
      .main <- "Maximum Favourable Excursion (MFE)"
    }
  }, percent = {
    .ylab <- "Profit/Loss (%)"
    if (type == "MAE") {
      .cols <- c("Pct.MAE", "Pct.Net.Trading.PL")
      .xlab <- "Drawdown (%)"
      .main <- "Maximum Adverse Excursion (MAE)"
    } else {
      .cols <- c("Pct.MFE", "Pct.Net.Trading.PL")
      .xlab <- "Run Up (%)"
      .main <- "Maximum Favourable Excursion (MFE)"
    }
  }, tick = {
    .ylab <- "Profit/Loss (ticks)"
    if (type == "MAE") {
      .cols <- c("tick.MAE", "tick.Net.Trading.PL")
      .xlab <- "Drawdown (ticks)"
      .main <- "Maximum Adverse Excursion (MAE)"
    } else {
      .cols <- c("tick.MFE", "tick.Net.Trading.PL")
      .xlab <- "Run Up (ticks)"
      .main <- "Maximum Favourable Excursion (MFE)"
    }
  })
  
  selected_trades <- abs(trades[, .cols])
  colnames(selected_trades) <- c('x','y')
  selected_trades$category <- 'Losing Trade'
  selected_trades$category[profitable] <- 'Profitable Trade'
  
  hc <- selected_trades %>% 
    hchart('scatter', hcaes(x = x, y = y, group = category)) %>% 
    hc_colors(c("#FC4E07","#00AFBB")) %>% 
    hc_xAxis(title = list(text = .xlab)) %>%
    hc_yAxis(title = list(text = .ylab)) %>%
    hc_title(text = .main)
  return(hc)
  # p <- ggplot(selected_trades,aes(x = x, y = y, group = category)) +
  #   geom_point(size = 2,aes(shape = category,color = category)) +
  #   theme_bw() +
  #   theme(legend.position = 'top',
  #         legend.spacing.x = unit(1.0, 'cm')) +
  #   geom_abline(linetype="dashed") +
  #   xlab(.xlab) + ylab(.ylab) + ggtitle(.main) +
  #   theme(plot.title = element_text(hjust = 0.5))
  # return(p)
}

