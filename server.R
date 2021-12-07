# author : LiSyaun Hong (lisyuanh@gmail.com)
library(shiny)
library(shinyWidgets)
library(shinythemes)
# Define server logic
server <- function(input, output, session) {
  session$allowReconnect(TRUE)
  withProgress(message = 'Loading...',value=100,{
    library(tidyquant)
    library(timetk)
    library(data.table)
    library(httr)
    library(dygraphs)
    library(magrittr)
    library(quantstrat)
    library(TTR)
    library(IKTrading)
    library(DSTrading)
    library(htmltools)
    library(DT)
    library(openxlsx)
    library(ggplot2)
    library(wesanderson)
    # library(plotly)
    source('Indicator.R')
    source('FinmindR.R')
    source('Strategybox.R')
    for(r in file.path('Strategy',dir('Strategy')))source(r)
    assign('stock_symbol_list',c(),env = .GlobalEnv)
  })
  stock_price <- eventReactive(input$Submit,{
    withProgress(message = 'Searching...',value=100,{
      symbol <- gsub(' ','',input$stock_symbol)
      s <- paste0('code',symbol)
      if(s %in% ls()){
        df <- get(s)
      }else if(symbol %in% TWSinfo$stock_id){
        df <- symbol %>%
          TaiwanStockPrice(token = use_token()) %>%
          TW_to_xts
      }else if(symbol %in% USSinfo$stock_id){
        df <- symbol %>%
          USStockPrice(token = use_token()) %>%
          US_to_xts
      }else{
        df <- tq_get(input$stock_symbol,
                     from = '1990-01-01',
                     to   = Sys.Date() + 1) %>%
          tk_xts(date_col = date) %>%
          na.omit()
      }
      assign(s, df, envir = .GlobalEnv)
      stock_symbol_list <<- c(stock_symbol_list, symbol)
    })
    return(df)
  })
  
  ts_plot <- reactive({
    while(T){
      temp <- paste0(c(input$dates %>% as.character()),collapse = '::')
      symbol <- gsub(' ','',input$stock_symbol)
      temp <- get(paste0('code',symbol))[temp]
      assign(symbol, temp,envir = .GlobalEnv)
      df <- get(symbol) %>% fortify()
      df <- data.frame(ds = df$Index,y = df$close)
      if(paste0('code',symbol) %in% ls(envir = .GlobalEnv))break
    }
    return(df %>% plot_time_series(ds, y, .interactive = T))
  })

  
  output$ts_plot <- plotly::renderPlotly(
    tryCatch({ts_plot()},error = function(msg)NULL)
  )
  
  observeEvent(input$back_n,{
    start_date <- input$dates[1] - input$period_n
    end_date <- input$dates[2] - input$period_n
    updateDateRangeInput(session, "dates",
                         start = start_date,
                         end = end_date)
  })
  
  observeEvent(input$next_n,{
    start_date <- input$dates[1] + input$period_n
    end_date <- input$dates[2] + input$period_n
    updateDateRangeInput(session, "dates",
                         start = start_date,
                         end = end_date
    )
  })
  # date
  observeEvent(input$num_peak_date,{
    peak_list <- list()
    if(input$num_peak_date == 0){
      output$peak_list_date <- renderUI({
        NULL
      })
    }else{
      step_size = ceiling(as.numeric(input$dates[2] - input$dates[1]) / 6)
      for (i in 1:input$num_peak_date) {
        temp_name <- paste0("peak_Date", i)
        local({
          content <- sprintf("
          fluidRow(  
            column(4,
                   dateInput(
                     'peak_Date%s',
                     label = '',
                     value = Sys.Date() - %s,
                     format = 'yyyy/mm/dd'
                   )
            ),
            column(3,
                   numericInputIcon('period_Date%s',label = '',min = 0,value = 20,step = 5)
            ),
            br(),
            column(2,
                   actionBttn('back_Date%s', '', 
                              icon = icon('far fa-angle-double-left'),      
                              style = 'material-flat',block = TRUE)
            ),
            column(2,
                   actionBttn('next_Date%s', '', 
                              icon = icon('fas fa-angle-double-right'),      
                              style = 'material-flat',block = TRUE)
            )
          )",i,step_size * i,i,i,i
          )
          peak_list[[i]] <<- temp_name
          output[[temp_name]] <- renderUI({eval(parse(text = content))})
        })
      }
      output$peak_list_date <- renderUI({
        do.call(tagList, lapply(peak_list, function(x){
          uiOutput(x)
        }))
      })
    }
  })
  
  observeEvent(input$num_peak_date,{
    if(input$num_peak_date > 0){
      for(i in 1:input$num_peak_date){
        content <- 
          sprintf("
            observeEvent(input[['back_Date%s']],{
              temp <- input[['peak_Date%s']] - input[['period_Date%s']]
              updateDateInput(session, 'peak_Date%s',value = temp)
            })
            
            observeEvent(input[['next_Date%s']],{
              temp <- input[['peak_Date%s']] + input[['period_Date%s']]
              updateDateInput(session, 'peak_Date%s',value = temp)
            })",
                  i,i,i,
                  i,i,i,
                  i,i)
        eval(parse(text = content))
      }
    }
  })
  # SMA
  observeEvent(input$num_peak_SMA,{
    peak_list <- list()
    if(input$num_peak_SMA == 0){
      output$peak_list_SMA <- renderUI({
        NULL
      })
    }else{
      for (i in 1:input$num_peak_SMA) {
        local({
          temp_name <- paste0('peak_SMA',i)
          content <- sprintf(
            "column(3,numericInput('period_SMA%s', 
            label = NULL, value = %s, min = 5, step = 5))",i,i*5)
          peak_list[[i]] <<- temp_name
          output[[temp_name]] <- renderUI(eval(parse(text = content)))
          
        })
      }
      output$peak_list_SMA <- renderUI({
        do.call(
          tagList,
          lapply(peak_list,function(x)uiOutput(x))
        )
      })
    }
  })
  # EMA
  observeEvent(input$num_peak_EMA,{
    peak_list <- list()
    if(input$num_peak_SMA == 0){
      output$peak_list_EMA <- renderUI({
        NULL
      })
    }else{
      for (i in 1:input$num_peak_EMA) {
        local({
          temp_name <- paste0('peak_EMA',i)
          content <- sprintf(
            "column(3,numericInput('period_EMA%s', 
            label = NULL, value = %s, min = 5, step = 5))",i,i*5)
          peak_list[[i]] <<- temp_name
          output[[temp_name]] <- renderUI(eval(parse(text = content)))
        })
      }
      output$peak_list_EMA <- renderUI({
        do.call(
          tagList,
          lapply(peak_list,function(x)uiOutput(x))
        )
      })
    }
  })
  # RSI
  observeEvent(input$num_peak_RSI,{
    peak_list <- list()
    if(input$num_peak_RSI == 0){
      output$peak_list_RSI <- renderUI({
        NULL
      })
    }else{
      for (i in 1:input$num_peak_RSI) {
        local({
          temp_name <- paste0('peak_RSI',i)
          content <- sprintf(
            "column(3,numericInput('period_RSI%s', 
            label = NULL, value = %s, min = 5, step = 5))",i,i*5)
          peak_list[[i]] <<- temp_name
          output[[temp_name]] <- renderUI(eval(parse(text = content)))
        })
      }
      output$peak_list_RSI <- renderUI({
        do.call(
          tagList,
          lapply(peak_list,function(x)uiOutput(x))
        )
      })
    }
  })
  # ATR
  observeEvent(input$num_peak_ATR,{
    peak_list <- list()
    if(input$num_peak_ATR == 0){
      output$peak_list_ATR <- renderUI({
        NULL
      })
    }else{
      for (i in 1:input$num_peak_ATR) {
        local({
          temp_name <- paste0('peak_ATR',i)
          content <- sprintf(
            "column(3,numericInput('period_ATR%s', 
            label = NULL, value = %s, min = 5, step = 5))",i,i*5)
          peak_list[[i]] <<- temp_name
          output[[temp_name]] <- renderUI(eval(parse(text = content)))
        })
      }
      output$peak_list_ATR <- renderUI({
        do.call(
          tagList,
          lapply(peak_list,function(x)uiOutput(x))
        )
      })
    }
  })
  # Volatility
  observeEvent(input$num_peak_VOL,{
    peak_list <- list()
    if(input$num_peak_VOL == 0){
      output$peak_list_VOL <- renderUI({
        NULL
      })
    }else{
      for (i in 1:input$num_peak_VOL) {
        local({
          temp_name <- paste0('peak_VOL',i)
          content <- sprintf(
            "column(3,numericInput('period_VOL%s', 
            label = NULL, value = %s, min = 5, step = 5))",i,i*5)
          peak_list[[i]] <<- temp_name
          output[[temp_name]] <- renderUI(eval(parse(text = content)))
        })
      }
      output$peak_list_VOL <- renderUI({
        do.call(
          tagList,
          lapply(peak_list,function(x)uiOutput(x))
        )
      })
    }
  })
  # WPR
  observeEvent(input$num_peak_WPR,{
    peak_list <- list()
    if(input$num_peak_WPR == 0){
      output$peak_list_WPR <- renderUI({
        NULL
      })
    }else{
      for (i in 1:input$num_peak_WPR) {
        local({
          temp_name <- paste0('peak_WPR',i)
          content <- sprintf(
            "column(3,numericInput('period_WPR%s', 
            label = NULL, value = %s, min = 5, step = 5))",i,i*5)
          peak_list[[i]] <<- temp_name
          output[[temp_name]] <- renderUI(eval(parse(text = content)))
        })
      }
      output$peak_list_WPR <- renderUI({
        do.call(
          tagList,
          lapply(peak_list,function(x)uiOutput(x))
        )
      })
    }
  })
  
  # Momentum
  observeEvent(input$num_peak_Mom,{
    peak_list <- list()
    if(input$num_peak_Mom == 0){
      output$peak_list_Mom <- renderUI({
        NULL
      })
    }else{
      for (i in 1:input$num_peak_Mom) {
        local({
          temp_name <- paste0('peak_Mom',i)
          content <- sprintf(
            "column(3,numericInput('period_Mom%s', 
            label = NULL, value = %s, min = 5, step = 1))",i,i)
          peak_list[[i]] <<- temp_name
          output[[temp_name]] <- renderUI(eval(parse(text = content)))
        })
      }
      output$peak_list_Mom <- renderUI({
        do.call(
          tagList,
          lapply(peak_list,function(x)uiOutput(x))
        )
      })
    }
  })
  
  # CMO
  observeEvent(input$num_peak_CMO,{
    peak_list <- list()
    if(input$num_peak_CMO == 0){
      output$peak_list_CMO <- renderUI({
        NULL
      })
    }else{
      for (i in 1:input$num_peak_CMO) {
        local({
          temp_name <- paste0('peak_CMO',i)
          content <- sprintf(
            "column(3,numericInput('period_CMO%s', 
            label = NULL, value = %s, min = 5, step = 5))",i,i*5)
          peak_list[[i]] <<- temp_name
          output[[temp_name]] <- renderUI(eval(parse(text = content)))
        })
      }
      output$peak_list_CMO <- renderUI({
        do.call(
          tagList,
          lapply(peak_list,function(x)uiOutput(x))
        )
      })
    }
  })
  
  # VHF
  observeEvent(input$num_peak_VHF,{
    peak_list <- list()
    if(input$num_peak_VHF == 0){
      output$peak_list_VHF <- renderUI({
        NULL
      })
    }else{
      for (i in 1:input$num_peak_VHF) {
        local({
          temp_name <- paste0('peak_VHF',i)
          content <- sprintf(
            "column(3,numericInput('period_VHF%s', 
            label = NULL, value = %s, min = 10, step = 5))",i,i * 10)
          peak_list[[i]] <<- temp_name
          output[[temp_name]] <- renderUI(eval(parse(text = content)))
        })
      }
      output$peak_list_VHF <- renderUI({
        do.call(
          tagList,
          lapply(peak_list,function(x)uiOutput(x))
        )
      })
    }
  })
  
  # MFI
  observeEvent(input$num_peak_MFI,{
    peak_list <- list()
    if(input$num_peak_MFI == 0){
      output$peak_list_MFI <- renderUI({
        NULL
      })
    }else{
      for (i in 1:input$num_peak_MFI) {
        local({
          temp_name <- paste0('peak_MFI',i)
          content <- sprintf(
            "column(3,numericInput('period_MFI%s',
            label = NULL, value = %s, min = 5, step = 5))",i,i*5)
          peak_list[[i]] <<- temp_name
          output[[temp_name]] <- renderUI(eval(parse(text = content)))
        })
      }
      output$peak_list_MFI <- renderUI({
        do.call(
          tagList,
          lapply(peak_list,function(x)uiOutput(x))
        )
      })
    }
  })
  
  # CycleRSI
  observeEvent(input$num_peak_CycleRSI,{
    peak_list <- list()
    if(input$num_peak_CycleRSI == 0){
      output$peak_list_CycleRSI <- renderUI({
        NULL
      })
    }else{
      for (i in 1:input$num_peak_CycleRSI) {
        local({
          temp_name <- paste0('peak_CycleRSI',i)
          content <- sprintf(
            "column(3,numericInput('period_CycleRSI%s',
            label = NULL, value = %s, min = 5, step = 5))",i,i*5)
          peak_list[[i]] <<- temp_name
          output[[temp_name]] <- renderUI(eval(parse(text = content)))
        })
      }
      output$peak_list_CycleRSI <- renderUI({
        do.call(
          tagList,
          lapply(peak_list,function(x)uiOutput(x))
        )
      })
    }
  })
  
  # CycleStoch
  observeEvent(input$num_peak_CycleStoch,{
    peak_list <- list()
    if(input$num_peak_CycleStoch == 0){
      output$peak_list_CycleStoch <- renderUI({
        NULL
      })
    }else{
      for (i in 1:input$num_peak_CycleStoch) {
        local({
          temp_name <- paste0('peak_CycleStoch',i)
          content <- sprintf(
            "column(3,numericInput('period_CycleStoch%s',
            label = NULL, value = %s, min = 5, step = 5))",i,i*5)
          peak_list[[i]] <<- temp_name
          output[[temp_name]] <- renderUI(eval(parse(text = content)))
        })
      }
      output$peak_list_CycleStoch <- renderUI({
        do.call(
          tagList,
          lapply(peak_list,function(x)uiOutput(x))
        )
      })
    }
  })
  
  # CGO
  observeEvent(input$num_peak_CGO,{
    peak_list <- list()
    if(input$num_peak_CGO == 0){
      output$peak_list_CGO <- renderUI({
        NULL
      })
    }else{
      for (i in 1:input$num_peak_CGO) {
        local({
          temp_name <- paste0('peak_CGO',i)
          content <- sprintf(
            "column(3,numericInput('period_CGO%s',
            label = NULL, value = %s, min = 5, step = 5))",i,i*5)
          peak_list[[i]] <<- temp_name
          output[[temp_name]] <- renderUI(eval(parse(text = content)))
        })
      }
      output$peak_list_CGO <- renderUI({
        do.call(
          tagList,
          lapply(peak_list,function(x)uiOutput(x))
        )
      })
    }
  })
  
  res <- reactive({
    # ------------------------------------------------------------------------ #
    # date range
    date_range <- paste0(c(input$dates %>% as.character()),collapse = '::')
    # ------------------------------------------------------------------------ #
    stock_price <- stock_price()
    my_data <-  OHLC(stock_price)
    # iTrend
    if(input$hasiTrend){
      itrend <- iTrend(HLC(my_data),alpha = input$iTrend_alpha)
      my_data <- cbind(my_data,itrend)
    }
    # SMA
    if(input$hasSMA){
      len <- 1:input$num_peak_SMA
      sma_prms <- sapply(len,function(i)input[[paste0('period_SMA', i)]])
      tryCatch({
        temp <- list()
        for (i in sma_prms) {
          temp[[length(temp) + 1]] <- SMA(Cl(stock_price),i)
        }
        temp <- do.call(cbind,temp)
        if(length(sma_prms) > 0)colnames(temp) <- paste0('sma',sma_prms)
        my_data <- cbind(my_data,temp)
      },error = function(msg)NULL)
    }
    # ------------------------------------------------------------------------ #
    # EMA
    if(input$hasEMA){
      len <- 1:input$num_peak_EMA
      ema_prms <- sapply(len,function(i)input[[paste0('period_EMA', i)]])
      tryCatch({
        temp <- list()
        for (i in ema_prms) {
          temp[[length(temp) + 1]] <- EMA(Cl(stock_price),i)
        }
        temp <- do.call(cbind,temp)
        if(length(ema_prms) > 0)colnames(temp) <- paste0('ema',ema_prms)
        my_data <- cbind(my_data,temp)
      },error = function(msg)NULL)
    }
    # ------------------------------------------------------------------------ #
    # GMMA
    if(input$hasGMMA){
      temp <- GMMA(Cl(my_data))
      my_data <- cbind(my_data,temp)
    }
    # ------------------------------------------------------------------------ #
    # Bollinger Bands
    if(input$hasBBand){
      temp <- BBands(
        HLC(my_data),
        n = input$bband_n,
        sd = input$bband_sd)[,c("dn", "mavg", "up")]
      if(input$hasBBFB){
        H <- temp[,'up']
        L <- temp[,'dn']
        dist <- H - L
        gr <- (sqrt(5) - 1) / 2
        gr <-ifelse(gr > .5, 1 - gr, gr)
        UB1 <- H - dist * gr
        UB2 <- H - dist * gr ^ 2
        LB1 <- H - dist * (1 - gr)
        LB2 <- H - dist * (1 - gr ^ 2)
        ULB <- cbind(UB1,UB2,LB1,LB2)
        colnames(ULB) <- c('bb_UB1', 'bb_UB2', 'bb_LB1', 'bb_LB2')
        temp <- cbind(temp, ULB)
      }
      my_data <- cbind(my_data,temp)
    }
    # ------------------------------------------------------------------------ #
    # Donchian Channel
    if(input$hasDC){
      temp <- DonchianChannel(
        my_data[,c("high","low")],
        n = input$DC_n)
      colnames(temp) <- paste0('dc_',c('H','M','L'))
      if(input$hasDCFB){
        H <- temp[,'dc_H']
        L <- temp[,'dc_L']
        dist <- H - L
        gr <- (sqrt(5) - 1) / 2
        gr <-ifelse(gr > .5, 1 - gr, gr)
        UB1 <- H - dist * gr
        UB2 <- H - dist * gr ^ 2
        LB1 <- H - dist * (1 - gr)
        LB2 <- H - dist * (1 - gr ^ 2)
        ULB <- cbind(UB1,UB2,LB1,LB2)
        colnames(ULB) <- c('dc_UB1', 'dc_UB2', 'dc_LB1', 'dc_LB2')
        temp <- cbind(temp, ULB)
      }
      my_data <- cbind(my_data,temp)
    }
    # ------------------------------------------------------------------------ #
    # Keltner Channel
    if(input$hasKC){
      temp <- KeltnerChannel(
        OHLC(my_data),
        nEMA = input$KC_nEMA,
        nATR = input$KC_nATR,
        sd = input$KC_sd)
      colnames(temp) <- paste0('kc_',c('H','M','L'))
      if(input$hasKCFB){
        H <- temp[,'kc_H']
        L <- temp[,'kc_L']
        dist <- H - L
        gr <- (sqrt(5) - 1) / 2
        gr <-ifelse(gr > .5, 1 - gr, gr)
        UB1 <- H - dist * gr
        UB2 <- H - dist * gr ^ 2
        LB1 <- H - dist * (1 - gr)
        LB2 <- H - dist * (1 - gr ^ 2)
        ULB <- cbind(UB1,UB2,LB1,LB2)
        colnames(ULB) <- c('kc_UB1', 'kc_UB2', 'kc_LB1', 'kc_LB2')
        temp <- cbind(temp, ULB)
      }
      my_data <- cbind(my_data,temp)
    }
    # ------------------------------------------------------------------------ #
    # Parabolic Stop-and-Reverse
    if(input$hasSAR){
      temp <- SAR(
        my_data[,c("high","low")],
        accel = c(input$sar_ac,input$sar_max_ac))
      colnames(temp) <- 'sar'
      my_data <- cbind(my_data,temp)
    }
    # ------------------------------------------------------------------------ #
    # ZigZag
    if(input$hasZZ){
      temp <- my_data[date_range,c("high","low")]
      temp <- ZigZag(temp, change = input$ZZ_n,retrace = input$ZZ_retrace)
      colnames(temp) <- 'zigzag'
      my_data <- cbind(my_data,temp)
    }
    # ------------------------------------------------------------------------ #
    # candle chart
    temp <- my_data[date_range]
    hi_stamp <- seriesHi(temp) %>% Cl()
    low_stamp <- seriesLo(temp) %>% Cl()
    d1 <- dygraph(temp , 
                  main = input$stock_symbol, 
                  group = 'stock',
                  height = input$graph_height, width = input$graph_width) %>% 
      dyCandlestick() %>% 
      dyRangeSelector(height = 50) %>%
      dyAnnotation(hi_stamp %>% index, text = 'H',tooltip = 'High') %>%
      dyAnnotation(low_stamp %>% index, text = 'L',tooltip = 'Low') %>%
      dyOptions(drawGrid = FALSE) %>%
      dyUnzoom() 
    # ------------------------------------------------------------------------ #
    # dash line
    if(input$hasFB_price){
      hi_stamp <- seriesHi(temp) %>% Cl()
      low_stamp <- seriesLo(temp) %>% Cl()
      h <- hi_stamp %>% as.vector
      l <- low_stamp %>% as.vector
      gr <- (sqrt(5) - 1) / 2
      n <- c(1.5,1)
      a <- 1 - gr ^ 2 ^ n
      b <-  gr ^ 2 ^ rev(n)
      ratio <- c(1,a, .5, b, 0)
      color <- wes_palette("Zissou1",7, type = "continuous") %>% rev
      GoldLine <- (l + (h - l) * ratio) %>% round(2)
      for(i in 1:7){
        d1 <- d1 %>% 
          dyLimit(
            GoldLine[i],
            label = GoldLine[i],
            color = color[i],
            strokePattern = 'dotdash'
          )
      }
    }
    # ------------------------------------------------------------------------ #
    # dash line
    if(input$hasiTrend){
      d1 <- d1 %>% dySeries('iTrend', color = 'blue')
      d1 <- d1 %>% dySeries('iTrigger', color = 'red')
    }
    # ------------------------------------------------------------------------ #
    if(input$hasColor){
      parser <- 'function(rawData) {
      var openIdx = 1;
      var closeIdx = 4;
      var ribbonData = [];

      for (var i = 0; i < rawData.length; i++) {
        var row = rawData[i];
        var open = row[openIdx];
        var close = row[closeIdx];

        if (open < close) {
          ribbonData.push(0.5);
        } else if (open > close) {
          ribbonData.push(1);
        } else {
          ribbonData.push(0);
        }
      }
      return ribbonData;
    }'
      
      d1 <- d1 %>%
        dyRibbon(parser = parser,
                 palette = c("#efefef", "#ffe6e6", "#ccebd6"))
    }
    # ------------------------------------------------------------------------ #
    # Moving Average
    if(input$hasSMA){
      ma_list <- colnames(my_data)
      ma_idx <- grep('sma',ma_list)
      ma_len <- length(ma_idx)
      ma_col <- RColorBrewer::brewer.pal(9,'Set1')
      if(ma_len > 0){
        for(i in 1:ma_len){
          d1 <- d1 %>% dySeries(ma_list[ma_idx[i]], color = ma_col[i])
        }
      }
    }
    if(input$hasEMA){
      ma_list <- colnames(my_data)
      ma_idx <- grep('ema',ma_list)
      ma_len <- length(ma_idx)
      ma_col <- RColorBrewer::brewer.pal(9,'Set1')
      if(ma_len > 0){
        for(i in 1:ma_len){
          d1 <- d1 %>% dySeries(ma_list[ma_idx[i]], color = ma_col[i])
        }
      }
    }
    # ------------------------------------------------------------------------ #
    # Moving Average
    if(input$hasGMMA){
      ma_list <- colnames(my_data)
      ma_idx <- grep('short',ma_list)
      ma_len <- length(ma_idx)
      ma_col <- sapply(255 - 1:6*10,function(x)rgb(x,20,20,maxColorValue = 255))
      if(ma_len > 0){
        for(i in 1:ma_len){
          d1 <- d1 %>% dySeries(ma_list[ma_idx[i]], color = ma_col[i])
        }
      }
      ma_idx <- grep('long',ma_list)
      ma_len <- length(ma_idx)
      ma_col <- sapply(255 - 1:6*15,function(x)rgb(20,20,x,maxColorValue = 255))
      if(ma_len > 0){
        for(i in 1:ma_len){
          d1 <- d1 %>% dySeries(ma_list[ma_idx[i]], color = ma_col[i])
        }
      }
    }
    # ------------------------------------------------------------------------ #
    # Bollinger Bands
    if(input$hasBBand) {
      d1 <- d1 %>%
        dySeries("dn", color = 'gray') %>%
        dySeries("up", color = 'gray') %>%
        dySeries("mavg",color = 'coral')
      if(input$hasBBFB){
        d1 <- d1 %>%
          dySeries("bb_UB1", color = 'red', strokePattern = "dashed") %>%
          dySeries("bb_LB1", color = 'red', strokePattern = "dashed") %>%
          dySeries("bb_UB2", color = 'blue', strokePattern = "dashed") %>%
          dySeries("bb_LB2", color = 'blue', strokePattern = "dashed")
      }
    }
    # ------------------------------------------------------------------------ #
    # Donchian Channel
    if(input$hasDC) {
      d1 <- d1 %>%
        dySeries("dc_H", color = 'gray') %>%
        dySeries("dc_L", color = 'gray') %>%
        dySeries("dc_M",color = 'coral')
      if(input$hasDCFB){
        d1 <- d1 %>%
          dySeries("dc_UB1", color = 'red', strokePattern = "dashed") %>%
          dySeries("dc_LB1", color = 'red', strokePattern = "dashed") %>%
          dySeries("dc_UB2", color = 'blue', strokePattern = "dashed") %>%
          dySeries("dc_LB2", color = 'blue', strokePattern = "dashed")
      }
    }
    # ------------------------------------------------------------------------ #
    # Keltner Channel
    if(input$hasKC) {
      d1 <- d1 %>%
        dySeries("kc_H", color = 'gray') %>%
        dySeries("kc_L", color = 'gray') %>%
        dySeries("kc_M",color = 'coral')
      if(input$hasKCFB){
        d1 <- d1 %>%
          dySeries("kc_UB1", color = 'red', strokePattern = "dashed") %>%
          dySeries("kc_LB1", color = 'red', strokePattern = "dashed") %>%
          dySeries("kc_UB2", color = 'blue', strokePattern = "dashed") %>%
          dySeries("kc_LB2", color = 'blue', strokePattern = "dashed")
      }
    }
    # ------------------------------------------------------------------------ #
    # SAR
    if(input$hasSAR) {
      d1 <- d1 %>%
        dySeries("sar", color = 'blue',drawPoints = TRUE,
                 pointSize = 2, pointShape = 'dot',
                 strokeWidth = 0
        )
    }
    # ------------------------------------------------------------------------ #
    # ZigZag
    if(input$hasZZ) {
      d1 <- d1 %>%
        dySeries("zigzag", color = rgb(.7, .7, .2),
                 strokeWidth = 2)
    }
    res <- list(d1)
    # ------------------------------------------------------------------------ #
    if(input$hasVolume){
      # ------------------------------------------------------------------------ #
      # Volume
      d2 <- dygraph(Vo(stock_price)[date_range],main = 'Volumn', group = 'stock', height = input$sub_graph_height, width = input$graph_width) %>%
        dyBarChart()
      res[[length(res) + 1]] <- d2
      my_data <- cbind(my_data, Vo(stock_price))
    }
    if(input$hasKD){
      # ------------------------------------------------------------------------ #
      # stochastic
      stoch_prms <- list(
        nFastK = input$kd_nFastK,
        nFastD = input$kd_nFastD,
        nSlowD = input$kd_nSlowD
      )
      kd <- stoch(HLC(my_data),
                  nFastK = stoch_prms$nFastK,
                  nFastD = stoch_prms$nFastD,
                  nSlowD = stoch_prms$nSlowD) * 100
      d3 <- dygraph(kd[date_range],main = 'KD', group = 'stock', height = input$sub_graph_height, width = input$graph_width) %>%
        dyOptions(colors = c(RColorBrewer::brewer.pal(3, "Set2"))) %>% 
        dyLimit(input$kd_LB,label = input$kd_LB) %>% 
        dyLimit(input$kd_UB,label = input$kd_UB)
      res[[length(res) + 1]] <- d3
      my_data <- cbind(my_data, kd)
    }
    if(input$hasRSI){
      # ------------------------------------------------------------------------ #
      # RSI
      tryCatch({
        len <- 1:input$num_peak_RSI
        rsi_prms <- sapply(len,function(i)input[[paste0('period_RSI', i)]])
        temp <- list()
        for (i in rsi_prms) {
          temp[[length(temp) + 1]] <- RSI(Cl(stock_price),i)
        }
        rsi <- do.call(cbind,temp)
        colnames(rsi) <- paste0('rsi', rsi_prms)
        d4 <- dygraph(rsi[date_range],main = 'RSI', group = 'stock', height = input$sub_graph_height, width = input$graph_width) %>%
          dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1")) %>% 
          dyLimit(input$rsi_LB,label = input$rsi_LB) %>% 
          dyLimit(input$rsi_UB,label = input$rsi_UB)
        res[[length(res) + 1]] <- d4
        my_data <- cbind(my_data, rsi)
      },error = function(msg)NULL)
    }
    if(input$hasMACD){
      # ------------------------------------------------------------------------ #
      # MACD
      macd_prms <- list(
        nFast = input$macd_nFast,
        nSlow = input$macd_nSlow,
        nSig = input$macd_nSig
      )
      macd <- MACD(Cl(my_data),
                   nFast = macd_prms$nFast,
                   nSlow = macd_prms$nSlow,
                   nSig = macd_prms$nSig)
      macd$diff <- macd$macd - macd$signal
      d5 <- dygraph(macd[date_range],main = 'MACD',group = 'stock', height = input$sub_graph_height, width = input$graph_width) %>%
        dyOptions(colors = c('red','blue','gray')) %>%
        dySeries("diff", stepPlot = TRUE, fillGraph = TRUE)
      res[[length(res) + 1]] <- d5
      my_data <- cbind(my_data, macd)
    }
    if(input$hasVolatility){
      # ------------------------------------------------------------------------ #
      # Volatility
      tryCatch({
        len <- 1:input$num_peak_VOL
        vol_prms <- sapply(len,function(i)input[[paste0('period_VOL', i)]])
        temp <- list()
        for (i in vol_prms) {
          temp[[length(temp) + 1]] <- volatility(OHLC(stock_price),i) * 100
        }
        vol <- do.call(cbind,temp)
        colnames(vol) <- paste0('vol', vol_prms)
        d6 <- dygraph(vol[date_range],main = 'Volatility', group = 'stock', height = input$sub_graph_height, width = input$graph_width) %>%
          dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1")) %>% 
          dyLimit(input$vol_LB,label = input$vol_LB) %>% 
          dyLimit(input$vol_UB,label = input$vol_UB)
        res[[length(res) + 1]] <- d6
        my_data <- cbind(my_data, vol)
      },error = function(msg)NULL)
    }
    if(input$hasATR){
      # ------------------------------------------------------------------------ #
      # ATR
      tryCatch({
        len <- 1:input$num_peak_ATR
        atr_prms <- sapply(len,function(i)input[[paste0('period_ATR', i)]])
        temp <- list()
        for (i in atr_prms) {
          temp[[length(temp) + 1]] <- ATR(HLC(stock_price),i)[,'atr']
        }
        atr <- do.call(cbind,temp)
        colnames(atr) <- paste0('atr', atr_prms)
        d7 <- dygraph(atr[date_range],main = 'ATR', group = 'stock', height = input$sub_graph_height, width = input$graph_width) %>%
          dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1")) %>% 
          dyLimit(input$wpr_LB,label = input$atr_LB) %>% 
          dyLimit(input$wpr_UB,label = input$atr_UB)
        res[[length(res) + 1]] <- d7
        my_data <- cbind(my_data, atr)
      },error = function(msg)NULL)
    }
    if(input$hasAroon){
      # ------------------------------------------------------------------------ #
      # Aroon
      aron <- aroon(my_data[,c("high","low")], n = input$aroon_n)
      if(!input$aroon_os) aron <- aron[,-3]
      d8 <- dygraph(aron[date_range],main = 'Aroon', group = 'stock', height = input$sub_graph_height, width = input$graph_width) %>%
        dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1"))
      res[[length(res) + 1]] <- d8
      my_data <- cbind(my_data, aron)
    }
    if(input$hasWPR){
      # ------------------------------------------------------------------------ #
      # WPR
      tryCatch({
        len <- 1:input$num_peak_WPR
        wpr_prms <- sapply(len,function(i)input[[paste0('period_WPR', i)]])
        temp <- list()
        for (i in wpr_prms) {
          temp[[length(temp) + 1]] <- WPR(HLC(stock_price),i) * 100
        }
        wpr <- do.call(cbind,temp)
        colnames(wpr) <- paste0('wpr', wpr_prms)
        d9 <- dygraph(wpr[date_range],main = 'WPR', group = 'stock', height = input$sub_graph_height, width = input$graph_width) %>%
          dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1")) %>% 
          dyLimit(input$wpr_LB,label = input$wpr_LB) %>% 
          dyLimit(input$wpr_UB,label = input$wpr_UB)
        res[[length(res) + 1]] <- d9
        my_data <- cbind(my_data, wpr)
      },error = function(msg)NULL)
    }
    if(input$hasSTC){
      # ------------------------------------------------------------------------ #
      # STC
      stc_prms <- list(
        nShort = input$stc_nShort,
        nLong = input$stc_nLong,
        nCycle = input$stc_nCycle,
        nK = input$stc_nK,
        nD = input$stc_nD
      )
      x <- OHLC(my_data)
      temp <- list()
      if(input$stc_addHL){
        index <- 1:4
        stc_name <- paste0('stc.',c('open','high','low','close'))
      }else{
        index <- c(1,4)
        stc_name <- paste0('stc.',c('open','close'))
      }
      for (i in index) {
        temp[[i]] <-           
          SchaffTrendCycle(
            x = x[,i],
            nShort = stc_prms$nShort,
            nLong = stc_prms$nLong,
            nCycle = stc_prms$nCycle,
            nK = stc_prms$nK,
            nD = stc_prms$nD
          )
      }
      stc <- do.call(cbind, temp)
      colnames(stc) <- stc_name
      d10 <- dygraph(stc[date_range],main = 'Schaff Trend Cycle', group = 'stock', height = input$sub_graph_height, width = input$graph_width) %>%
        dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1")) %>% 
        dyLimit(input$stc_LB,label = input$stc_LB) %>% 
        dyLimit(input$stc_UB,label = input$stc_UB)
      res[[length(res) + 1]] <- d10
      my_data <- cbind(my_data, stc)
    }
    if(input$hasADX){
      # ------------------------------------------------------------------------ #
      # ADX
      adx <- ADX(HLC(my_data),input$adx_n)
      d11 <- dygraph(adx[date_range],main = 'ADX', group = 'stock', height = input$sub_graph_height, width = input$graph_width) %>%
        dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1")) %>% 
        dyLimit(input$adx_LB,label = input$adx_LB) %>% 
        dyLimit(input$adx_UB,label = input$adx_UB)
      # ------------------------------------------------------------------------ #
      res[[length(res) + 1]] <- d11
      my_data <- cbind(my_data, adx)
    }
    if(input$hasMom){
      # ------------------------------------------------------------------------ #
      # Momentum
      tryCatch({
        len <- 1:input$num_peak_Mom
        mom_prms <- sapply(len,function(i)input[[paste0('period_Mom', i)]])
        temp <- list()
        for (i in mom_prms) {
          temp[[length(temp) + 1]] <- momentum(Cl(stock_price),i) * 100
        }
        mom <- do.call(cbind,temp)
        colnames(mom) <- paste0('mom', mom_prms)
        d12 <- dygraph(mom[date_range],main = 'Momentum', group = 'stock', height = input$sub_graph_height, width = input$graph_width) %>%
          dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1")) 
        res[[length(res) + 1]] <- d12
        my_data <- cbind(my_data, mom)
      },error = function(msg)NULL)
    }
    if(input$hasCMO){
      # ------------------------------------------------------------------------ #
      # CMO
      tryCatch({
        len <- 1:input$num_peak_CMO
        cmo_prms <- sapply(len,function(i)input[[paste0('period_CMO', i)]])
        temp <- list()
        for (i in cmo_prms) {
          temp[[length(temp) + 1]] <- CMO(Cl(stock_price),i)
        }
        cmo <- do.call(cbind,temp)
        colnames(cmo) <- paste0('cmo', cmo_prms)
        d13 <- dygraph(cmo[date_range],main = 'CMO', group = 'stock', height = input$sub_graph_height, width = input$graph_width) %>%
          dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1")) %>% 
          dyLimit(input$cmo_LB,label = input$cmo_LB) %>% 
          dyLimit(input$cmo_UB,label = input$cmo_UB)
        res[[length(res) + 1]] <- d13
        my_data <- cbind(my_data, cmo)
      },error = function(msg)NULL)
    }
    if(input$hasVHF){
      # ------------------------------------------------------------------------ #
      # VHF
      tryCatch({
        len <- 1:input$num_peak_VHF
        vhf_prms <- sapply(len,function(i)input[[paste0('period_VHF', i)]])
        temp <- list()
        for (i in vhf_prms) {
          temp[[length(temp) + 1]] <- VHF(Cl(stock_price),i) * 100
        }
        vhf <- do.call(cbind,temp)
        colnames(vhf) <- paste0('vhf', vhf_prms)
        d14 <- dygraph(vhf[date_range],main = 'VHF', group = 'stock', height = input$sub_graph_height, width = input$graph_width) %>%
          dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1")) %>%
          dyLimit(input$vhf_LB,label = input$vhf_LB) %>%
          dyLimit(input$vhf_UB,label = input$vhf_UB)
        res[[length(res) + 1]] <- d14
        my_data <- cbind(my_data, vhf)
      },error = function(msg)NULL)
    }
    if(input$hasTDI){
      # ------------------------------------------------------------------------ #
      # TDI
      tdi <- TDI(Cl(stock_price),n = input$TDI_n, multiple = input$TDI_mult)
      d15 <- dygraph(tdi[date_range],main = 'TDI', group = 'stock', height = input$sub_graph_height, width = input$graph_width) %>%
        dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1"))
      # ------------------------------------------------------------------------ #
      res[[length(res) + 1]] <- d15
      my_data <- cbind(my_data, tdi)
    }
    if(input$hasMFI){
      # ------------------------------------------------------------------------ #
      # MFI
      tryCatch({
        len <- 1:input$num_peak_MFI
        mfi_prms <- sapply(len,function(i)input[[paste0('period_MFI', i)]])
        temp <- list()
        for (i in mfi_prms) {
          temp[[length(temp) + 1]] <- MFI(HLC(stock_price),volume = Vo(stock_price),n = i)
        }
        mfi <- do.call(cbind,temp)
        colnames(mfi) <- paste0('mfi', mfi_prms)
        d16 <- dygraph(mfi[date_range],main = 'MFI', group = 'stock', height = input$sub_graph_height, width = input$graph_width) %>%
          dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1")) %>%
          dyLimit(input$mfi_LB,label = input$mfi_LB) %>%
          dyLimit(input$mfi_UB,label = input$mfi_UB)
        res[[length(res) + 1]] <- d16
        my_data <- cbind(my_data, mfi)
      },error = function(msg)NULL)
    }
    if(input$hasRVI){
      # ------------------------------------------------------------------------ #
      # RVI
      rvi <- RVI(OHLC(stock_price),n = input$RVI_n)
      d17 <- dygraph(rvi[date_range],main = 'RVI', group = 'stock', height = input$sub_graph_height, width = input$graph_width) %>%
        dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1"))
      res[[length(res) + 1]] <- d17
      my_data <- cbind(my_data, rvi)
    }
    if(input$hasCycleRSI){
      # ------------------------------------------------------------------------ #
      # CycleRSI
      tryCatch({
        len <- 1:input$num_peak_CycleRSI
        prms <- sapply(len,function(i)input[[paste0('period_CycleRSI', i)]])
        temp <- list()
        for (i in prms) {
          temp[[length(temp) + 1]] <- CycleRSI(Cl(stock_price),n = i)
        }
        crsi <- do.call(cbind,temp)
        colnames(crsi) <- paste0('CycleRSI', prms)
        d18 <- dygraph(crsi[date_range],main = 'CycleRSI', group = 'stock', height = input$sub_graph_height, width = input$graph_width) %>%
          dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1")) %>%
          dyLimit(input$crsi_LB,label = input$crsi_LB) %>%
          dyLimit(input$crsi_UB,label = input$crsi_UB)
        res[[length(res) + 1]] <- d18
        my_data <- cbind(my_data, crsi)
      },error = function(msg)NULL)
    }
    if(input$hasCycleStoch){
      # ------------------------------------------------------------------------ #
      # CycleStoch
      tryCatch({
        len <- 1:input$num_peak_CycleStoch
        prms <- sapply(len,function(i)input[[paste0('period_CycleStoch', i)]])
        temp <- list()
        for (i in prms) {
          temp[[length(temp) + 1]] <- CycleStoch(Cl(stock_price),n = i) * 100
        }
        cstoch <- do.call(cbind,temp)
        colnames(cstoch) <- paste0('CycleStoch', prms)
        d19 <- dygraph(cstoch[date_range],main = 'CycleStoch', group = 'stock', height = input$sub_graph_height, width = input$graph_width) %>%
          dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1")) %>%
          dyLimit(input$cstoch_LB,label = input$cstoch_LB) %>%
          dyLimit(input$cstoch_UB,label = input$cstoch_UB)
        res[[length(res) + 1]] <- d19
        my_data <- cbind(my_data, cstoch)
      },error = function(msg)NULL)
    }

    if(input$hasTVI){
      # ------------------------------------------------------------------------ #
      # TVI
      tvi <- TVI(Cl(stock_price),period = input$TVI_period,delta = input$TVI_delta)
      d20 <- dygraph(tvi[date_range],main = 'TVI', group = 'stock', height = input$sub_graph_height, width = input$graph_width) %>%
        dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1"))
      res[[length(res) + 1]] <- d20
      my_data <- cbind(my_data, tvi)
    }

    if(input$hasCGO){
      # ------------------------------------------------------------------------ #
      # CGO
      tryCatch({
        len <- 1:input$num_peak_CGO
        prms <- sapply(len,function(i)input[[paste0('period_CGO', i)]])
        temp <- list()
        for (i in prms) {
          temp[[length(temp) + 1]] <- CGO(HLC(stock_price),n = i) * 100
        }
        cgo <- do.call(cbind,temp)
        colnames(cgo) <- paste0('CGO', prms)
        d18 <- dygraph(cgo[date_range],main = 'CGO', group = 'stock', height = input$sub_graph_height, width = input$graph_width) %>%
          dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1"))
        res[[length(res) + 1]] <- d18
        my_data <- cbind(my_data, cgo)
      },error = function(msg)NULL)
    }
    if(input$has_heikin_ashi){
      ha <- heikin_ashi(OHLC(my_data))
      d19 <- dygraph(ha[date_range],main = 'Heikin Ashi', group = 'stock', height = input$sub_graph_height, width = input$graph_width) %>%
        dyCandlestick()
      res[[length(res) + 1]] <- d19
      my_data <- cbind(my_data, ha)
    }
    if(input$has_fgindex){
      x <- CnnFGIndex()
      d20 <- dygraph(x[date_range],main = 'Cnn Fear Greed Index', group = 'stock', height = input$sub_graph_height, width = input$graph_width)
      res[[length(res) + 1]] <- d20
      my_data <- cbind(my_data, x)
    }
    data <- my_data
    if(ncol(data) > 4)data[,-(1:4)] <- data[,-(1:4)] %>% round(2)
    data <- data.frame(date = index(data), coredata(data))
    data <- data[rev(1:nrow(data)),]
    return(list(graph_obj = res, data = data))
  })
  
  output$plot <- renderUI({
    temp <- res()$graph_obj
    # dash line at date
    for(i in 1:length(temp)){
      if(input$num_peak_date > 0){
        for(j in 1:input$num_peak_date){
          temp[[i]] <- temp[[i]] %>% 
            dyEvent(input[[paste0('peak_Date',j)]],
                    label = input[[paste0('peak_Date',j)]])
        }
      }
    }
    temp %>% 
      htmltools::tagList() %>%
      htmltools::browsable()
  })
  output$data <- DT::renderDataTable({
    DT::datatable(
      res()$data,
      filter = 'top', 
      extensions = c('Buttons', 'Scroller'),
      options = list(scrollY = 650,
                     scrollX = 500,
                     scroller = TRUE,
                     buttons = list(
                       list(extend = 'colvis', targets = 0, visible = FALSE)
                     ),
                     dom = "Blfrtip",
                     fixedColumns = TRUE), 
      rownames = FALSE)
  })  
  
  output$TaiwanStockInfo <- DT::renderDataTable({
    DT::datatable(
      TWSinfo,
      filter = 'top', 
      extensions = c('Buttons', 'Scroller'),
      options = list(scrollY = 650,
                     scrollX = 500,
                     scroller = TRUE,
                     buttons = list(
                       list(extend = 'colvis', targets = 0, visible = FALSE)
                     ),
                     dom = "Blfrtip",
                     fixedColumns = TRUE), 
      rownames = FALSE)
  })
  
  output$USStockInfo <- DT::renderDataTable({
    DT::datatable(
      USSinfo,
      filter = 'top', 
      extensions = c('Buttons', 'Scroller'),
      options = list(scrollY = 650,
                     scrollX = 500,
                     scroller = TRUE,
                     buttons = list(
                       list(extend = 'colvis', targets = 0, visible = FALSE)
                     ),
                     dom = "Blfrtip",
                     fixedColumns = TRUE), 
      rownames = FALSE)
  })
  
  output$download_button <- downloadHandler(
    filename = function() {
      paste0(input$stock_symbol,'.xlsx')
    },
    content = function(file) {
      openxlsx::write.xlsx(res()$data, file)
    }
  )
  
  output$TWSinfo_download_button <- downloadHandler(
    filename = function() {
      'TaiwanStockInfo.xlsx'
    },
    content = function(file) {
      openxlsx::write.xlsx(TWSinfo, file)
    }
  )
  
  output$USSinfo_download_button <- downloadHandler(
    filename = function() {
      'USStockInfo.xlsx'
    },
    content = function(file) {
      openxlsx::write.xlsx(USSinfo, file)
    }
  )
  
  # apply Strategy
  applyStrat <- eventReactive(input$apply_strat,{
    withProgress(message = 'Running...',value = 0,{
      incProgress(1/2)
      symbol <- gsub(' ','',input$stock_symbol)
      assign('buyCost',input$buyCost / 100,env = .GlobalEnv)
      assign('sellCost',input$sellCost / 100,env = .GlobalEnv)
      tradeSize <- input$tradeSize * 1e+3
      if(input$strategy_selected == 'MAcross'){
        ret <- MAcross(
          df = get(symbol),
          symbol = symbol,
          tradeSize = tradeSize,
          nFast = input$nFast_MAcv,
          nSlow = input$nSlow_MAcv,
          maType = input$maType_MAcv
        )
      }else if(input$strategy_selected == 'MACDcross0'){
        ret <- MACDcross0(
          df = get(symbol),         
          symbol = symbol,
          tradeSize = tradeSize,
          nFast = input$nFast_MACDcv0,
          nSlow = input$nSlow_MACDcv0, 
          nSig = input$nSig_MACDcv0,
          maType = input$maType_MACDcv0
        )
      }else if(input$strategy_selected == 'Stochcross'){
        ret <- Stochcross(
          df = get(symbol),    
          symbol = symbol,
          tradeSize = tradeSize,
          nFastK = input$nFastK_KDcv,
          nFastD = input$nFastD_KDcv, 
          nSlowD = input$nSlowD_KDcv
        )
      }else if(input$strategy_selected == 'iTrendcross'){
        ret <- iTrendcross(
          df = get(symbol),      
          symbol = symbol,
          tradeSize = tradeSize,
          alpha = input$alpha_iTrendcross
        )
      }else if(input$strategy_selected == 'STCcrossTH'){
        ret <- STCcrossTH(
          df = get(symbol),       
          symbol = symbol,
          tradeSize = tradeSize,
          nShort = input$nShort_STCcrossTH,
          nLong = input$nLong_STCcrossTH,
          nCycle = input$nCycle_STCcrossTH,
          nK = input$nK_STCcrossTH,
          nD = input$nD_STCcrossTH,
          LB = input$LB_STCcrossTH,
          UB = input$UB_STCcrossTH,
          opposite = input$opposite_STCcrossTH
        )
      }else if(input$strategy_selected == 'buyhold'){
        ret <- buyhold(
          df = get(symbol),      
          symbol = symbol,
          tradeSize = tradeSize
        )
      }else if(input$strategy_selected == 'RSIcross'){
        ret <- RSIcross(
          df = get(symbol),        
          symbol = symbol,
          tradeSize = tradeSize,
          nFast = input$nFast_RSIcross,
          nSlow = input$nSlow_RSIcross
        )
      }else if(input$strategy_selected == 'ADXcross'){
        ret <- ADXcross(
          df = get(symbol),        
          symbol = symbol,
          tradeSize = tradeSize,
          n = input$n_ADXcross,
          opposite = input$opposite_ADXcross
        )
      }else if(input$strategy_selected == 'BBandcrossCl'){
        ret <- BBandcrossCl(
          df = get(symbol),       
          symbol = symbol,
          tradeSize = tradeSize,
          n = input$n_BBandcrossCl,
          sd = input$sd_BBandcrossCl,
          maType = input$maType_BBandcrossCl,
          opposite = input$opposite_BBandcrossCl
        )
      }else if(input$strategy_selected == 'KCcrossCl'){
        ret <- KCcrossCl(
          df = get(symbol),       
          symbol = symbol,
          tradeSize = tradeSize,
          nEMA = input$nEMA_KCcrossCl,
          nATR = input$nATR_KCcrossCl,
          sd = input$sd_KCcrossCl,
          opposite = input$opposite_KCcrossCl
        )
      }else if(input$strategy_selected == 'SARcrossCl'){
        ret <- SARcrossCl(
          df = get(symbol),       
          symbol = symbol,
          tradeSize = tradeSize,
          ac = input$ac_SARcrossCl,
          mac = input$mac_SARcrossCl,
          opposite = input$opposite_SARcrossCl
        )
      }else if(input$strategy_selected == 'Arooncross'){
        ret <- Arooncross(
          df = get(symbol),      
          symbol = symbol,
          tradeSize = tradeSize,
          n = input$n_Arooncross,
          opposite = input$opposite_Arooncross
        )
      }else if(input$strategy_selected == 'RVIcross'){
        ret <- RVIcross(
          df = get(symbol),       
          symbol = symbol,
          tradeSize = tradeSize,
          n = input$n_RVIcross,
          opposite = input$opposite_RVIcross
        )
      }else if(input$strategy_selected == 'CycleRSIcross'){
        ret <- CycleRSIcross(
          df = get(symbol),      
          symbol = symbol,
          tradeSize = tradeSize,
          nFast = input$nFast_CycleRSIcross,
          nSlow = input$nSlow_CycleRSIcross
        )
      }else if(input$strategy_selected == 'CycleStochcross'){
        ret <- CycleStochcross(
          df = get(symbol),       
          symbol = symbol,
          tradeSize = tradeSize,
          nFast = input$nFast_CycleStochcross,
          nSlow = input$nSlow_CycleStochcross
        )
      }else if(input$strategy_selected == 'CycleRSIcrossStoch'){
        ret <- CycleRSIcrossStoch(
          df = get(symbol),         
          symbol = symbol,
          tradeSize = tradeSize,
          nRSI = input$nRSI_CycleRSIcrossStoch,
          nStoch = input$nStoch_CycleRSIcrossStoch,
          opposite = input$opposite_CycleRSIcrossStoch
        )
      }else if(input$strategy_selected == 'TVIcross'){
        ret <- TVIcross(
          df = get(symbol),        
          symbol = symbol,
          tradeSize = tradeSize,
          period = input$period_TVIcross,
          delta = input$delta_TVIcross,
          opposite = input$opposite_TVIcross
        )
      }else if(input$strategy_selected == 'CCIthreshold'){
        ret <- CCIthreshold(
          df = get(symbol),        
          symbol = symbol,
          tradeSize = tradeSize,
          n = input$period_CCIthreshold,
          threshold = input$threshold_CCIthreshold,
          opposite = input$opposite_CCIthreshold
        )
      }else if(input$strategy_selected == 'CMOthreshold'){
        ret <- CMOthreshold(
          df = get(symbol),        
          symbol = symbol,
          tradeSize = tradeSize,
          n = input$period_CMOthreshold,
          threshold = input$threshold_CMOthreshold,
          opposite = input$opposite_CMOthreshold
        )

      }else if(input$strategy_selected == 'CGOcross'){
        ret <- CGOcross(
          df = get(symbol),        
          symbol = symbol,
          tradeSize = tradeSize,
          nFast = input$nFast_CGOcross,
          nSlow = input$nSlow_CGOcross
        )
      }else{
        ret <- list()
        tryCatch({
          ret[[length(ret)+1]]  <- MAcross(
            df = get(symbol),      
            symbol = symbol,
            tradeSize = tradeSize,
            nFast = input$nFast_MAcv,
            nSlow = input$nSlow_MAcv,
            maType = input$maType_MAcv)[1:4]},
          error = function(msg) {
            return(NULL)
          })
        tryCatch({
          ret[[length(ret)+1]] <- MACDcross0(
            df = get(symbol),       
            symbol = symbol,
            tradeSize = tradeSize,
            nFast = input$nFast_MACDcv0,
            nSlow = input$nSlow_MACDcv0, 
            nSig = input$nSig_MACDcv0,
            maType = input$maType_MACDcv0)[1:4]},
          error = function(msg) {
            return(NULL)
          })
        
        tryCatch({
          ret[[length(ret)+1]] <- KDcross(
            df = get(symbol),       
            symbol = symbol,
            tradeSize = tradeSize,
            nFastK = input$nFastK_KDcv,
            nFastD = input$nFastD_KDcv, 
            nSlowD = input$nSlowD_KDcv)[1:4]},
          error = function(msg) {
            return(NULL)
          })
        
        tryCatch({
          ret[[length(ret)+1]] <- iTrendcross(
            df = get(symbol),       
            symbol = symbol,
            tradeSize = tradeSize,
            alpha = input$alpha_iTrendcross)[1:4]},
          error = function(msg) {
            return(NULL)
          })
        
        tryCatch({
          ret[[length(ret)+1]] <- STCcrossTH(
            df = get(symbol),       
            symbol = symbol,
            tradeSize = tradeSize,
            nShort = input$nShort_STCcrossTH,
            nLong = input$nLong_STCcrossTH,
            nCycle = input$nCycle_STCcrossTH,
            nK = input$nK_STCcrossTH,
            nD = input$nD_STCcrossTH,
            LB = input$LB_STCcrossTH,
            UB = input$UB_STCcrossTH,
            opposite = input$opposite_STCcrossTH)[1:4]},
          error = function(msg) {
            return(NULL)
          })
        
        tryCatch({
          ret[[length(ret)+1]] <- buyhold(
            df = get(symbol),       
            symbol = symbol,
            tradeSize = tradeSize)[1:4]},
          error = function(msg) {
            return(NULL)
          })
        
        tryCatch({
          ret[[length(ret)+1]] <- RSIcross(
            df = get(symbol),      
            symbol = symbol,
            tradeSize = tradeSize,
            nFast = input$nFast_RSIcross,
            nSlow = input$nSlow_RSIcross)[1:4]},
          error = function(msg) {
            return(NULL)
          })
        
        tryCatch({ 
          ret[[length(ret)+1]] <- ADXcross(
            df = get(symbol),       
            symbol = symbol,
            tradeSize = tradeSize,
            n = input$n_ADXcross,
            opposite = input$opposite_ADXcross)[1:4]},
          error = function(msg) {
            return(NULL)
          })
        
        tryCatch({
          ret[[length(ret)+1]] <- BBandcrossCl(
            df = get(symbol),      
            symbol = symbol,
            tradeSize = tradeSize,
            n = input$n_BBandcrossCl,
            sd = input$sd_BBandcrossCl,
            maType = input$maType_BBandcrossCl,
            opposite = input$opposite_BBandcrossCl)[1:4]},
          error = function(msg) {
            return(NULL)
          })
        
        tryCatch({
          ret[[length(ret)+1]] <- KCcrossCl(
            df = get(symbol),       
            symbol = symbol,
            tradeSize = tradeSize,
            nEMA = input$nEMA_KCcrossCl,
            nATR = input$nATR_KCcrossCl,
            sd = input$sd_KCcrossCl,
            opposite = input$opposite_KCcrossCl)[1:4]},
          error = function(msg) {
            return(NULL)
          })
        
        tryCatch({
          ret[[length(ret)+1]] <- SARcrossCl(
            df = get(symbol),      
            symbol = symbol,
            tradeSize = tradeSize,
            ac = input$ac_SARcrossCl,
            mac = input$mac_SARcrossCl,
            opposite = input$opposite_SARcrossCl)[1:4]},
          error = function(msg) {
            return(NULL)
          })
        
        tryCatch({
          ret[[length(ret)+1]] <- Arooncross(
            df = get(symbol),       
            symbol = symbol,
            tradeSize = tradeSize,
            n = input$n_Arooncross,
            opposite = input$opposite_Arooncross)[1:4]},
          error = function(msg) {
            return(NULL)
          })
        
        tryCatch({
          ret[[length(ret)+1]] <- RVIcross(
            df = get(symbol),      
            symbol = symbol,
            tradeSize = tradeSize,
            n = input$n_RVIcross,
            opposite = input$opposite_RVIcross)[1:4]},
          error = function(msg) {
            return(NULL)
          })
        
        tryCatch({
          ret[[length(ret)+1]] <- CycleRSIcross(
            df = get(symbol),      
            symbol = symbol,
            tradeSize = tradeSize,
            nFast = input$nFast_CycleRSIcross,
            nSlow = input$nSlow_CycleRSIcross)[1:4]},
          error = function(msg) {
            return(NULL)
          })
        
        tryCatch({
          ret[[length(ret)+1]] <- CycleStochcross(
            df = get(symbol),       
            symbol = symbol,
            tradeSize = tradeSize,
            nFast = input$nFast_CycleStochcross,
            nSlow = input$nSlow_CycleStochcross)[1:4]},
          error = function(msg) {
            return(NULL)
          })
        
        tryCatch({
          ret[[length(ret)+1]] <- CycleRSIcrossStoch(
            df = get(symbol),       
            symbol = symbol,
            tradeSize = tradeSize,
            nRSI = input$nRSI_CycleRSIcrossStoch,
            nStoch = input$nStoch_CycleRSIcrossStoch,
            opposite = input$opposite_CycleRSIcrossStoch)[1:4]},
          error = function(msg) {
            return(NULL)
          })
        
        tryCatch({
          ret[[length(ret)+1]] <- TVIcross(
            df = get(symbol),      
            symbol = symbol,
            tradeSize = tradeSize,
            period = input$period_TVIcross,
            delta = input$delta_TVIcross,
            opposite = input$opposite_TVIcross)[1:4]},
          error = function(msg) {
            return(NULL)
          })
        
        tryCatch({
          ret[[length(ret)+1]] <- CGOcross(
            df = get(symbol),     
            symbol = symbol,
            tradeSize = tradeSize,
            nFast = input$nFast_CGOcross,
            nSlow = input$nSlow_CGOcross)[1:4]},
          error = function(msg) {
            return(NULL)
          })
        tryCatch({
          ret[[length(ret)+1]] <- CCIthreshold(
            df = get(symbol),        
            symbol = symbol,
            tradeSize = tradeSize,
            n = input$period_CCIthreshold,
            threshold = input$threshold_CCIthreshold,
            opposite = input$opposite_CCIthreshold
          )[1:4]},
          error = function(msg) {
            return(NULL)
          })
        tryCatch({
          ret[[length(ret)+1]] <- CMOthreshold(
            df = get(symbol),        
            symbol = symbol,
            tradeSize = tradeSize,
            n = input$period_CMOthreshold,
            threshold = input$threshold_CMOthreshold,
            opposite = input$opposite_CMOthreshold
          )[1:4]},
          error = function(msg) {
            return(NULL)
          })

        ret <- do.call(rbind,ret)
        win_info = do.call(rbind,ret[,1])
        profit_info = do.call(rbind,ret[,2])
        return_info = do.call(rbind,ret[,3])
        return_info = data.frame(Portfolio = profit_info$Portfolio,return_info)
        
        temp <- list()
        for(i in 1:nrow(ret)){
          temp[[i]] <- data.frame(Portfolio = profit_info$Portfolio[i],ret[i,4][[1]])
        }
        pertrade_info <- do.call(rbind,temp) %>% as.data.frame()

        ret <- list(
          win_info = win_info,
          profit_info = profit_info,
          return_info = return_info,
          pertrade_info = pertrade_info,
          graph_obj = NULL,
          MAE = NULL,
          MFE = NULL
        )
      }
      ret$pertrade_info[] <- lapply(
        ret$pertrade_info,
        function(x){
          cx <- class(x)
          if("POSIXct" %in% cx){
            return(x %>% as.Date())
          }else if("numeric" %in% cx){
            return(x %>% round(5))
          }else if("difftime" %in% cx){
            return(x / 86400)
          }else{
            return(x)
          }
        })
    })
    return(ret)
  })
  
  output$win_info <- shiny::renderDataTable({
    tryCatch({applyStrat()$win_info},error = function(msg) NULL)
  })
  output$profit_info <- shiny::renderDataTable({
    tryCatch({applyStrat()$profit_info},error = function(msg) NULL)
  })
  output$return_info <- shiny::renderDataTable({
    tryCatch({applyStrat()$return_info},error = function(msg) NULL)
  })
  output$pertrade_info <- DT::renderDataTable({
    tryCatch({
      DT::datatable(
        applyStrat()$pertrade_info,
        extensions = c('Buttons', 'Scroller'),
        options = list(scrollY = 650,
                       scrollX = 500,
                       scroller = TRUE,
                       buttons = list(
                         list(extend = 'colvis', targets = 0, visible = FALSE)
                       ),
                       dom = "Blfrtip",
                       fixedColumns = TRUE), 
        rownames = FALSE
      )},error = function(msg) NULL)
  })
  
  output$Strategy_plot <- renderUI({
    tryCatch({applyStrat()$graph_obj},error = function(msg) NULL)
  })
  output$MAE_plot <- renderUI({
    tryCatch({applyStrat()$MAE},error = function(msg) NULL)
  })
  output$MFE_plot <- renderUI({
    tryCatch({applyStrat()$MFE},error = function(msg) NULL)
  })
  
  output$StrategyResult <- downloadHandler(
    filename = function() {
      paste0('Strategy_2',input$strategy_selected,'_',input$stock_symbol,'.xlsx')
    },
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb,sheetName = 'Profit')
      openxlsx::addWorksheet(wb,sheetName = 'Win')
      openxlsx::addWorksheet(wb,sheetName = 'Return')
      openxlsx::addWorksheet(wb,sheetName = 'PerTrade')
      # openxlsx::addWorksheet(wb,sheetName = 'Graph')
      # openxlsx::addWorksheet(wb,sheetName = 'MAE')
      # openxlsx::addWorksheet(wb,sheetName = 'MFE')
      openxlsx::writeData(wb,'Profit',applyStrat()$profit_info)
      openxlsx::writeData(wb,'Win',applyStrat()$win_info)
      openxlsx::writeData(wb,'Return',applyStrat()$return_info)
      openxlsx::writeData(wb,'PerTrade',applyStrat()$pertrade_info)
      openxlsx::saveWorkbook(wb, file)
    }
  )
  
}