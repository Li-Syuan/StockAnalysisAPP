library(TTR)
# ----------------------------------------------------------------------------- #
SchaffTrendCycle <- function(x, nShort=23, nLong=50, nCycle = 10, nK = 3, nD = 3){
  useful_fun <- function(x,n) (x - runMin(x,n)) / (runMax(x,n) - runMin(x,n))
  MA1 <- EMA(x,nShort)
  MA2 <- EMA(x,nLong)
  macd <- MA1 - MA2
  k = useful_fun(macd, nCycle)
  d = EMA(k, nK)
  kd = useful_fun(d, nCycle)
  stc = EMA(na.approx(kd, na.rm = FALSE), nD)
  stc <- cbind(x,stc)[,-1]
  try(colnames(stc) <- 'stc',silent = T)
  return(stc * 100)
}
KeltnerChannel <- function(x, nEMA = 10, nATR = 14, sd = 2) {
  mid <- EMA(Cl(x), nEMA)    
  hi <- mid + sd * ATR(HLC = HLC(x), n = nATR)$atr
  lo <- mid - sd * ATR(HLC = HLC(x), n = nATR)$atr
  keltner <- cbind(lo, mid, hi)
  colnames(keltner) <- c("KC.low", "KC.mid", "KC.hi")
  keltner
}
# ----------------------------------------------------------------------------- #
heikin_ashi <- function (x) {
  x <- x[,c('open','high','low','close')]
  heikin_close <- xts::xts(rowMeans(x), order.by = zoo::index(x))
  heikin_open <- quantmod::Op(x)
  for (i in 2:nrow(x)) {
    heikin_open[i] <- (heikin_open[i - 1] + heikin_close[i - 1])/2
  }
  heikin_high <- xts::xts(apply(cbind(quantmod::Hi(x), heikin_open, heikin_close), 1, max), order.by = zoo::index(x))
  heikin_low <- xts::xts(apply(cbind(quantmod::Lo(x), heikin_open, heikin_close), 1, min), order.by = zoo::index(x))
  out <- merge(heikin_open, heikin_high, heikin_low, heikin_close)
  names(out) <- paste0(c("O", "H", "L", "C"),'_HA')
  return(out)
}
