# author : LiSyaun Hong (lisyuanh@gmail.com)
library(shiny)
library(shinyWidgets)
library(dashboardthemes)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
source('themeDIY.R')

header <- shinydashboardPlus::dashboardHeader(
  title = 
    shinyDashboardLogoDIY(
      boldText = "Stock Analysis",
      mainText = '',
      textSize = 18,
      badgeText = 'v1.0',
      badgeTextSize = 3,
      badgeTextColor = 'black',
      badgeBackColor = 'white',
      badgeBorderRadius = 3
    )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    sidebarSearchForm(textId = "stock_symbol", buttonId = "Submit", label = "EX: 2330"),
    menuItem("Document", icon = icon("far fa-star"), tabName = "Doc"),
    menuItem(
      'Stock',icon = icon("th"),
      menuSubItem("Taiwan market", icon = icon("far fa-archive"), tabName = "TaiwanStockInfo"),
      menuSubItem("US market", icon = icon("far fa-archive"), tabName = "USStockInfo")
    ),
    menuItem("Technical Analysis", icon = icon("bar-chart-o"), tabName = "Charts"),
    menuItem("Trading Strategy", icon = icon("bar-chart-o"), tabName = "Strategy"),
    menuItem(
      "TA Options", icon = icon("gear"), tabName = "graph_options",
      dateRangeInput(
        "dates",
        h5('Date range'),
        start = Sys.Date() - 365,
        end = Sys.Date(),
        format = 'yyyy/mm/dd',
        separator = ""
      ),
      sliderInput('period_n',
                  h5('Horizon'),
                  min = 10,
                  value = 30,
                  max = 100,
                  step = 10,
                  ticks = FALSE),
      fluidRow(
        column(5,
               actionBttn("back_n", '',
                          icon = icon("far fa-angle-double-left"),
                          style = "material-flat",block = TRUE)
        ),
        column(5,
               actionBttn("next_n", '',
                          icon = icon("fas fa-angle-double-right"),
                          style = "material-flat",block = TRUE)
        )
      ),
      sliderInput('graph_width',
                  label = h5('Charts width'),
                  value = 1000,
                  min = 500,
                  max = 2000,
                  step = 100,
                  ticks = FALSE),
      sliderInput('graph_height',
                  label = h5('Charts height'),
                  value = 500,
                  min = 300,
                  max = 1000,
                  step = 100,
                  ticks = FALSE),
      sliderInput('sub_graph_height',
                  label = h5('Indicator height'),
                  value = 150,
                  min = 100,
                  max = 300,
                  step = 50,
                  ticks = FALSE)
    )
  )
)

body <- dashboardBody(
  customTheme,
  tabItems(
    tabItem(
      tabName = 'Doc',
      h5('Author : Li-Syuan Hong'),
      h5('Email : lisyuanh@gmail.com'),
      h5('This project analyze stock market price using Finmind and Quantmod API,which provide technical analysis and trading strategy.')
    ),
    tabItem(
      tabName = "TaiwanStockInfo",
      box(
        width = 10,
        title = "Taiwan stock symbol information", 
        status = "warning", 
        collapsible = FALSE,
        DT::dataTableOutput("TaiwanStockInfo")
      ),
      box(
        width = 2,
        status = "danger",
        title = 'Download',
        downloadBttn(
          outputId = "TWSinfo_download_button",
          label = "",
          style = "gradient",
          color = "primary"
        )
      )
    ),
    tabItem(
      tabName = "USStockInfo",
      box(
        width = 10,
        title = "US stock symbol information", 
        status = "warning", 
        collapsible = FALSE,
        DT::dataTableOutput("USStockInfo")
      ),
      box(
        width = 2,
        status = "danger",
        title = 'Download',
        downloadBttn(
          outputId = "USSinfo_download_button",
          label = "",
          style = "gradient",
          color = "primary"
        )
      )
    ),
    tabItem(
      tabName = "Charts",
      tabsetPanel(
        tabPanel(
          'Candle Charts',
          box(
            width = 9,
            title = "Candle Chart and Indicator",
            status = "warning",
            collapsible = FALSE,
            uiOutput('plot')
          ),
          box(
            width = 3,
            status = "danger", 
            title = 'Options',
            prettySwitch(
              "hasColor", 
              label = " Color", 
              value = FALSE,
              status = "success",
              fill = TRUE
            ),
            prettySwitch(
              "hasFB_price", 
              label = " Price dash line", 
              value = TRUE, 
              status = "success",
              fill = TRUE
            ),
            prettySwitch(
              "hasVolume",
              label = " Volume",
              value = TRUE,
              status = "success",
              fill = TRUE
            ),
            prettySwitch(
              "has_heikin_ashi",
              label = " HeikinAshi",
              value = FALSE,
              status = "success",
              fill = TRUE
            ),
            prettySwitch(
              "has_fgindex",
              label = " Cnn Fear Greed Index",
              value = FALSE,
              status = "success",
              fill = TRUE
            ),
            prettySwitch(
              "hasGMMA",
              label = " Guppy Multiple Moving Averages",
              value = FALSE,
              status = "success",
              fill = TRUE
            ),
            fluidRow(
              h5('Date dash line') %>% column(10,.),
              dropdown(
                sliderInput('num_peak_date',
                            label = '',
                            value = 0,
                            min = 0,
                            max = 5,
                            step = 1),
                uiOutput('peak_list_date'),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # iTrend
            fluidRow(
              prettySwitch(
                "hasiTrend",
                label = " Ehlers Instantaneous Trendline Indicator",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                numericInput('iTrend_alpha',
                             label = h5('Alpha'),
                             value = 0.07,
                             min = 0,
                             max = 1,
                             step = 0.01),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # SMA
            fluidRow(
              prettySwitch(
                "hasSMA",
                label = " Simple moving average",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                sliderInput('num_peak_SMA',
                            label = h5('Number of SMA'),
                            value = 1,
                            min = 1,
                            max = 4,
                            step = 1) %>% column(12,.),
                uiOutput('peak_list_SMA'),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # EMA
            fluidRow(
              prettySwitch(
                "hasEMA",
                label = " Exponential moving average.",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                sliderInput('num_peak_EMA',
                            label = h5('Number of EMA'),
                            value = 1,
                            min = 1,
                            max = 4,
                            step = 1),
                uiOutput('peak_list_EMA'),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # Bollinger Bands
            fluidRow(
              prettySwitch(
                "hasBBand",
                label = " Bollinger Bands",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                fluidRow(
                  column(4,numericInputIcon("bband_n", label = "Period", value = 20)),
                  column(4,numericInputIcon("bband_sd", label = "SD.", value = 2)),
                  br(),
                  column(4,
                         prettyCheckbox(
                           "hasBBFB",
                           label = " Fibonacci",
                           value = FALSE,
                           icon = icon("check"),
                           status = "warning",
                           animation = "rotate"
                         )
                  )
                ),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # Donchian Channel
            fluidRow(
              prettySwitch(
                "hasDC",
                label = " Donchian Channel",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                fluidRow(
                  column(4,numericInputIcon("DC_n", label = "Period", value = 10)),
                  br(),
                  column(4,
                         prettyCheckbox(
                           "hasDCFB",
                           label = " Fibonacci",
                           value = FALSE,
                           icon = icon("check"),
                           status = "warning",
                           animation = "rotate"
                         )
                  )
                ),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # Keltner Channel
            fluidRow(
              prettySwitch(
                "hasKC",
                label = " Keltner Channel",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                fluidRow(
                  column(4,numericInputIcon("KC_nEMA", label = "nEMA", value = 10)),
                  column(4,numericInputIcon("KC_nATR", label = "nATR", value = 14)),
                  column(4,numericInputIcon("KC_sd", label = "sd", value = 2)),
                  column(12,
                         prettyCheckbox(
                           "hasKCFB",
                           label = " Fibonacci",
                           value = FALSE,
                           icon = icon("check"),
                           status = "warning",
                           animation = "rotate"
                         )
                  )
                ),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # Parabolic Stop-and-Reverse
            fluidRow(
              prettySwitch(
                "hasSAR",
                label = " Parabolic Stop-and-Reverse",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                fluidRow(
                  column(6,numericInputIcon("sar_ac", label = "Acceleration", value = 0.02)),
                  column(6,numericInputIcon("sar_max_ac", label = "Maximum accel", value = 0.2)),
                ),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # ZigZag
            fluidRow(
              prettySwitch(
                "hasZZ",
                label = " ZigZag",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                fluidRow(
                  column(6,numericInputIcon("ZZ_n", label = "Minimum price movement", value = 10)),
                  br(),
                  column(6,
                         prettyCheckbox(
                           "ZZ_retrace",
                           label = "Retrace",
                           value = FALSE,
                           icon = icon("check"),
                           status = "warning",
                           animation = "rotate"
                         )
                  )
                ),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # Stochastic
            fluidRow(
              prettySwitch(
                "hasKD",
                label = " Stochastic Oscillator",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                fluidRow(
                  column(4,numericInputIcon("kd_nFastK", label = "nFastK", value = 14)),
                  column(4,numericInputIcon("kd_nFastD", label = "nFastD", value = 3)),
                  column(4,numericInputIcon("kd_nSlowD", label = "nSlowD", value = 3)),
                  column(6,sliderInput("kd_LB", label = "Lower Bound", value = 20, min = 0, max = 100, step = 10)),
                  column(6,sliderInput("kd_UB", label = "Upper Bound", value = 80, min = 0, max = 100, step = 10))
                ),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # RSI
            fluidRow(
              prettySwitch(
                "hasRSI",
                label = " Relative Strength Index",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                sliderInput('num_peak_RSI',
                            label = h5('Number of RSI'),
                            value = 1,
                            min = 1,
                            max = 4,
                            step = 1),
                uiOutput('peak_list_RSI') %>% fluidRow(),
                fluidRow(
                  column(6,sliderInput("rsi_LB", label = "Lower Bound", value = 20, min = 0, max = 100,step = 10)),
                  column(6,sliderInput("rsi_UB", label = "Upper Bound", value = 80, min = 0, max = 100,step = 10))
                ),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # MACD
            fluidRow(
              prettySwitch(
                "hasMACD",
                label = " MACD Oscillator",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                column(4,numericInputIcon("macd_nFast", label = "nFast", value = 12)),
                column(4,numericInputIcon("macd_nSlow", label = "nSlow", value = 26)),
                column(4,numericInputIcon("macd_nSig", label = "nSig", value = 9)),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # Volatility
            fluidRow(
              prettySwitch(
                "hasVolatility",
                label = " Volatility",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                sliderInput('num_peak_VOL',
                            label = h5('Number of Volatility'),
                            value = 1,
                            min = 1,
                            max = 4,
                            step = 1),
                uiOutput('peak_list_VOL') %>% fluidRow(),
                fluidRow(
                  column(6,sliderInput("vol_LB", label = "Lower Bound", value = 20, min = 0, max = 100,step = 10)),
                  column(6,sliderInput("vol_UB", label = "Upper Bound", value = 80, min = 0, max = 100,step = 10))
                ),   
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # ATR
            fluidRow(
              prettySwitch(
                "hasATR",
                label = " Average True Range",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                sliderInput('num_peak_ATR',
                            label = h5('Number of ATR'),
                            value = 1,
                            min = 1,
                            max = 4,
                            step = 1),
                uiOutput('peak_list_ATR') %>% fluidRow(),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # Aroon
            fluidRow(
              prettySwitch(
                "hasAroon",
                label = " Aroon",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                column(6,numericInputIcon("aroon_n", label = "Period", value = 20)),
                br(),
                column(6,
                       prettyCheckbox(
                         "aroon_os",
                         label = " Oscillator",
                         value = FALSE,
                         icon = icon("check"),
                         status = "warning",
                         animation = "rotate"
                       )
                ),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # WPR
            fluidRow(
              prettySwitch(
                "hasWPR",
                label = " William's %R",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                sliderInput('num_peak_WPR',
                            label = h5('Number of WPR'),
                            value = 1,
                            min = 1,
                            max = 4,
                            step = 1),
                uiOutput('peak_list_WPR') %>% fluidRow(),
                fluidRow(
                  column(6,sliderInput("wpr_LB", label = "Lower Bound", value = 20, min = 0, max = 100,step = 10)),
                  column(6,sliderInput("wpr_UB", label = "Upper Bound", value = 80, min = 0, max = 100,step = 10))
                ),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # Schaff Trend Cycle
            fluidRow(
              prettySwitch(
                "hasSTC",
                label = " Schaff Trend Cycle",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                fluidRow(
                  column(4,numericInputIcon("stc_nShort", label = "nShort", value = 23)),
                  column(4,numericInputIcon("stc_nLong", label = "nLong", value = 50)),
                  column(4,numericInputIcon("stc_nCycle", label = "nCycle", value = 10))
                ),
                fluidRow(
                  column(4,numericInputIcon("stc_nK", label = "nK", value = 3)),
                  column(4,numericInputIcon("stc_nD", label = "nD", value = 3)),
                  br(),
                  column(4,prettyCheckbox(
                    "stc_addHL",
                    label = " high&close",
                    value = FALSE,
                    icon = icon("check"),
                    status = "warning",
                    animation = "rotate"))
                ),
                fluidRow(
                  column(6,sliderInput("stc_LB", label = "Lower Bound", value = 20, min = 0, max = 100,step = 10)),
                  column(6,sliderInput("stc_UB", label = "Upper Bound", value = 80, min = 0, max = 100,step = 10))
                ),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # ADX
            fluidRow(
              prettySwitch(
                "hasADX",
                label = " Welles Wilder's Directional Movement Index",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                column(6,numericInputIcon("adx_n", label = "Period", value = 14)) %>% fluidRow(),
                fluidRow(
                  column(6,sliderInput("adx_LB", label = "Lower Bound", value = 20, min = 0, max = 100,step = 10)),
                  column(6,sliderInput("adx_UB", label = "Upper Bound", value = 80, min = 0, max = 100,step = 10))
                ),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # Momentum
            fluidRow(
              prettySwitch(
                "hasMom",
                label = " Momentum",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                sliderInput('num_peak_Mom',
                            label = h5('Number of Momentum'),
                            value = 1,
                            min = 1,
                            max = 5,
                            step = 1),
                uiOutput('peak_list_Mom') %>% fluidRow(),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # CMO
            fluidRow(
              prettySwitch(
                "hasCMO",
                label = " Chande Momentum Oscillator",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                sliderInput('num_peak_CMO',
                            label = h5('Number of CMO'),
                            value = 1,
                            min = 1,
                            max = 4,
                            step = 1),
                uiOutput('peak_list_CMO') %>% fluidRow(),
                fluidRow(
                  column(6,sliderInput("cmo_LB", label = "Lower Bound", value = -80, min = -100, max = 100,step = 10)),
                  column(6,sliderInput("cmo_UB", label = "Upper Bound", value = 80, min = -100, max = 100,step = 10))
                ),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # VHF
            fluidRow(
              prettySwitch(
                "hasVHF",
                label = " Vertical Horizontal Filter",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                sliderInput('num_peak_VHF',
                            label = h5('Number of VHF'),
                            value = 1,
                            min = 1,
                            max = 4,
                            step = 1),
                uiOutput('peak_list_VHF') %>% fluidRow(),
                fluidRow(
                  column(6,sliderInput("vhf_LB", label = "Lower Bound", value = 20, min = 0, max = 100,step = 10)),
                  column(6,sliderInput("vhf_UB", label = "Upper Bound", value = 80, min = 0, max = 100,step = 10))
                ),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # TDI
            fluidRow(
              prettySwitch(
                "hasTDI",
                label = " Trend Detection Index",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                column(6,numericInputIcon("TDI_n", label = "Period", value = 20)),
                column(6,numericInputIcon("TDI_mult", label = "Multiple", value = 2)),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # MFI
            fluidRow(
              prettySwitch(
                "hasMFI",
                label = " Money Flow Index",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                sliderInput('num_peak_MFI',
                            label = h5('Number of MFI'),
                            value = 1,
                            min = 1,
                            max = 4,
                            step = 1),
                uiOutput('peak_list_MFI') %>% fluidRow(),
                fluidRow(
                  column(6,sliderInput("mfi_LB", label = "Lower Bound", value = 20, min = 0, max = 100,step = 10)),
                  column(6,sliderInput("mfi_UB", label = "Upper Bound", value = 80, min = 0, max = 100,step = 10))
                ),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # RVI
            fluidRow(
              prettySwitch(
                "hasRVI",
                label = " Ehlers Relative Vigor Index",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                column(6,numericInputIcon("RVI_n", label = "Period", value = 20)),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # TVI
            fluidRow(
              prettySwitch(
                "hasTVI",
                label = " Trend Vigor Indicator",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                column(6,numericInputIcon("TVI_period", label = "Period", value = 20)),
                column(6,numericInputIcon("TVI_delta", label = "Delta", value = 0.2)),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # CycleRSI
            fluidRow(
              prettySwitch(
                "hasCycleRSI",
                label = " Cycle RSI Oscillator",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                sliderInput('num_peak_CycleRSI',
                            label = h5('Number of CycleRSI'),
                            value = 1,
                            min = 1,
                            max = 4,
                            step = 1),
                uiOutput('peak_list_CycleRSI') %>% fluidRow(),
                fluidRow(
                  column(6,sliderInput("crsi_LB", label = "Lower Bound", value = 20, min = 0, max = 100,step = 10)),
                  column(6,sliderInput("crsi_UB", label = "Upper Bound", value = 80, min = 0, max = 100,step = 10))
                ),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # CycleStoch
            fluidRow(
              prettySwitch(
                "hasCycleStoch",
                label = " Cycle Stochastic Oscillator", 
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                sliderInput('num_peak_CycleStoch',
                            label = h5('Number of CycleStoch'),
                            value = 1,
                            min = 1,
                            max = 4,
                            step = 1),
                uiOutput('peak_list_CycleStoch')%>% fluidRow(),
                fluidRow(
                  column(6,sliderInput("cstoch_LB", label = "Lower Bound", value = 20, min = 0, max = 100,step = 10)),
                  column(6,sliderInput("cstoch_UB", label = "Upper Bound", value = 80, min = 0, max = 100,step = 10))
                ),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            ),
            # CGO
            fluidRow(
              prettySwitch(
                "hasCGO",
                label = " Center of Gravity Oscillator",
                value = FALSE,
                status = "success",
                fill = TRUE
              ) %>% column(10,.),
              dropdown(
                sliderInput('num_peak_CGO',
                            label = h5('Number of CGO'),
                            value = 1,
                            min = 1,
                            max = 4,
                            step = 1),
                uiOutput('peak_list_CGO') %>% fluidRow(),
                circle = TRUE, status = "warning",
                style = "unite",icon = icon("gear"), width = "400px",animate = T,right = T,
                tooltip = tooltipOptions()
              ) %>% column(2,.)
            )
          )
        ),
        tabPanel(
          title = 'Stock data',
          box(
            width = 10,
            title = "Stock data and Indicator", 
            status = "warning", 
            collapsible = FALSE,
            DT::dataTableOutput("data")
          ),
          box(
            width = 2,
            status = "danger", 
            title = 'Download',
            downloadBttn(
              outputId = "download_button",
              label = "",
              style = "gradient", 
              color = "primary"
            )
          )
        )
      )
    ),
    tabItem(
      tabName = "Strategy",
      fluidRow(
        box(
          width = 3,
          status = "warning", 
          title = 'Trading Strategy',
          selectInput('strategy_selected',label = 'Strategy',
                      c(
                        "Moving Average cross over" = "MAcross",
                        "RSI cross over" = "RSIcross",
                        'ADX cross over' = 'ADXcross',
                        'Aroon cross over' = 'Arooncross',
                        'RVI cross over' = 'RVIcross',
                        'TVI cross over' = 'TVIcross',
                        'CGO cross over' = 'CGOcross',
                        'CCI cross threshold' = 'CCIthreshold',
                        'CMO cross threshold' = 'CMOthreshold',
                        'Cycle RSI cross over' = 'CycleRSIcross',
                        'Cycle Stoch cross over' = 'CycleStochcross',
                        'Cycle RSI cross over Cycle Stoch' = 'CycleRSIcrossStoch',
                        'Stoch cross over' = 'Stochcross',
                        'Ehlers Trendline cross over' = 'iTrendcross',
                        "MACD signal greater than 0" = "MACDcross0",
                        'SchaffTrendCycle cross over threshold' = 'STCcrossTH',
                        'BBands cross Close' = 'BBandcrossCl',
                        'Keltner Channel cross Close' = 'KCcrossCl',
                        'SAR cross Close' = 'SARcrossCl',
                        'Buy and Hold' = 'buyhold',
                        'Compare Strategies' = 'CompareStrategy'
                      )
          ),
          fluidRow(
            numericInputIcon("buyCost", label = "Entry Cost(%)", value = .1425,step = .0001, min = 0) %>% column(6,.),
            numericInputIcon("sellCost", label = "Exit Cost(%)", value = .4425,step = .0001, min = 0) %>% column(6,.),
            numericInputIcon("tradeSize", label = "Initial capital(k)", value = 1000,step = 100, min = 100) %>% column(6,.),
          ),
          actionBttn("apply_strat", 'Run', color = "primary", style = "gradient", block = TRUE) %>% column(12,.)
        ),
        box(
          width = 3,
          title = 'Parameter Setting',
          status = "warning", 
          conditionalPanel(
            "input.strategy_selected == 'MAcross'",
            numericInputIcon("nFast_MAcv", label = "Fast MA", value = 5,step = 1) %>% column(6,.),
            numericInputIcon("nSlow_MAcv", label = "Slow MA", value = 20,step = 1) %>% column(6,.),
            selectInput("maType_MAcv", "MA type",
                        c(
                          'SMA' = 'SMA',
                          "EMA" = "EMA",
                          'HMA' = 'HMA',
                          'ALMA' = 'ALMA',
                          'DEMA' = 'DEMA',
                          'ZLEMA' = 'ZLEMA'
                        )
            ) %>% column(12,.)
          ),
          conditionalPanel(
            "input.strategy_selected == 'RSIcross'",
            numericInputIcon("nFast_RSIcross", label = "Fast RSI", value = 7,step = 1) %>% column(6,.),
            numericInputIcon("nSlow_RSIcross", label = "Slow RSI", value = 14,step = 1) %>% column(6,.),
          ),
          conditionalPanel(
            "input.strategy_selected == 'MACDcross0'",
            numericInputIcon("nFast_MACDcv0", label = "Fast MA", value = 12,step = 1) %>% column(4,.),
            numericInputIcon("nSlow_MACDcv0", label = "Slow MA", value = 26,step = 1) %>% column(4,.),
            numericInputIcon("nSig_MACDcv0", label = "nSig", value = 9,step = 1) %>% column(4,.),
            selectInput("maType_MACDcv0", "MA type",c('SMA' = 'SMA',"EMA" = "EMA")) %>% column(12,.)
          ),
          conditionalPanel(
            "input.strategy_selected == 'Stochcross'",
            numericInputIcon("nFastK_KDcv", label = "FastK", value = 14,step = 1) %>% column(4,.),
            numericInputIcon("nFastD_KDcv", label = "FastD", value = 3,step = 1) %>% column(4,.),
            numericInputIcon("nSlowD_KDcv", label = "SlowD", value = 3,step = 1) %>% column(4,.),
          ),
          conditionalPanel(
            "input.strategy_selected == 'iTrendcross'",
            numericInputIcon("alpha_iTrendcross", label = "Alpha", value = 0.07,step = 0.01) %>% column(12,.)
          ),
          conditionalPanel(
            "input.strategy_selected == 'STCcrossTH'",
            numericInputIcon("nShort_STCcrossTH", label = "nShort", value = 26,step = 1, min = 0) %>% column(4,.),
            numericInputIcon("nLong_STCcrossTH", label = "nLong", value = 50,step = 1, min = 0) %>% column(4,.),
            numericInputIcon("nCycle_STCcrossTH", label = "nCycle", value = 10,step = 1, min = 0) %>% column(4,.),
            numericInputIcon("nK_STCcrossTH", label = "nK", value = 3,step = 1,min = 0) %>% column(6,.),
            numericInputIcon("nD_STCcrossTH", label = "nD", value = 3,step = 1,min = 0) %>% column(6,.),
            numericInputIcon("LB_STCcrossTH", label = "LB", value = 25,step = 5,min = 10,max = 50) %>% column(6,.),
            numericInputIcon("UB_STCcrossTH", label = "UB", value = 75,step = 5,min = 50,max = 90) %>% column(6,.),
            prettySwitch(
              "opposite_STCcrossTH", 
              label = "Change Buy and Sell", 
              value = FALSE, 
              status = "success",
              fill = TRUE
            )
          ),
          conditionalPanel(
            "input.strategy_selected == 'ADXcross'",
            numericInputIcon("n_ADXcross", label = "Period", value = 14,step = 1),
            prettySwitch(
              "opposite_ADXcross", 
              label = "Change Buy and Sell", 
              value = FALSE, 
              status = "success",
              fill = TRUE
            )
          ),
          conditionalPanel(
            "input.strategy_selected == 'BBandcrossCl'",
            numericInputIcon("n_BBandcrossCl", label = "Period", value = 20,step = 1) %>% column(6,.),
            numericInputIcon("sd_BBandcrossCl", label = "SD", value = 2,step = .05) %>% column(6,.),
            selectInput("maType_BBandcrossCl", "MA type",c('SMA' = 'SMA',"EMA" = "EMA")) %>% column(12,.),
            prettySwitch(
              "opposite_BBandcrossCl", 
              label = "Change Buy and Sell", 
              value = FALSE, 
              status = "success",
              fill = TRUE
            ) %>% column(12,.)
          ),
          conditionalPanel(
            "input.strategy_selected == 'KCcrossCl'",
            numericInputIcon("nEMA_KCcrossCl", label = "EMA", value = 20,step = 1) %>% column(4,.),
            numericInputIcon("nATR_KCcrossCl", label = "ATR", value = 14,step = 1) %>% column(4,.),
            numericInputIcon("sd_KCcrossCl", label = "SD", value = 2,step = .05) %>% column(4,.),
            prettySwitch(
              "opposite_KCcrossCl", 
              label = "Change Buy and Sell", 
              value = FALSE, 
              status = "success",
              fill = TRUE
            ) %>% column(12,.)
          ),
          conditionalPanel(
            "input.strategy_selected == 'SARcrossCl'",
            numericInputIcon("ac_SARcrossCl", label = "Acceleration", value = 0.02,step = 0.01) %>% column(6,.),
            numericInputIcon("mac_SARcrossCl", label = "Max Acceleration", value = 0.2,step = .05) %>% column(6,.),
            prettySwitch(
              "opposite_SARcrossCl", 
              label = "Change Buy and Sell", 
              value = FALSE, 
              status = "success",
              fill = TRUE
            ) %>% column(12,.)
          ),
          conditionalPanel(
            "input.strategy_selected == 'Arooncross'",
            numericInputIcon("n_Arooncross", label = "Period", value = 20,step = 1),
            prettySwitch(
              "opposite_Arooncross", 
              label = "Change Buy and Sell", 
              value = FALSE, 
              status = "success",
              fill = TRUE
            ) %>% column(12,.)
          ),
          conditionalPanel(
            "input.strategy_selected == 'RVIcross'",
            numericInputIcon("n_RVIcross", label = "Period", value = 20,step = 1),
            prettySwitch(
              "opposite_RVIcross", 
              label = "Change Buy and Sell", 
              value = FALSE, 
              status = "success",
              fill = TRUE
            ) %>% column(12,.)
          ),
          conditionalPanel(
            "input.strategy_selected == 'CycleRSIcross'",
            numericInputIcon("nFast_CycleRSIcross", label = "Fast CycleRSI", value = 20,step = 1) %>% column(6,.),
            numericInputIcon("nSlow_CycleRSIcross", label = "Slow CycleRSI", value = 40,step = 1) %>% column(6,.),
          ),
          conditionalPanel(
            "input.strategy_selected == 'CycleStochcross'",
            numericInputIcon("nFast_CycleStochcross", label = "Fast CycleStoch", value = 20,step = 1) %>% column(6,.),
            numericInputIcon("nSlow_CycleStochcross", label = "Slow CycleStoch", value = 40,step = 1) %>% column(6,.),
          ),
          conditionalPanel(
            "input.strategy_selected == 'CycleRSIcrossStoch'",
            numericInputIcon("nRSI_CycleRSIcrossStoch", label = "CycleRSI", value = 10,step = 1) %>% column(6,.),
            numericInputIcon("nStoch_CycleRSIcrossStoch", label = "CycleStoch", value = 10,step = 1) %>% column(6,.),
            prettySwitch(
              "opposite_CycleRSIcrossStoch", 
              label = "Change Buy and Sell", 
              value = FALSE, 
              status = "success",
              fill = TRUE
            ) %>% column(12,.)
          ),
          conditionalPanel(
            "input.strategy_selected == 'TVIcross'",
            numericInputIcon("period_TVIcross", label = "Period", value = 20,step = 1) %>% column(6,.),
            numericInputIcon("delta_TVIcross", label = 'Delta', value = 0.2,step = .1) %>% column(6,.),
            prettySwitch(
              "opposite_TVIcross", 
              label = "Change Buy and Sell", 
              value = FALSE, 
              status = "success",
              fill = TRUE
            ) %>% column(12,.)
          ),
          conditionalPanel(
            "input.strategy_selected == 'CCIthreshold'",
            numericInputIcon("period_CCIthreshold", label = "Period", value = 20,step = 1) %>% column(6,.),
            numericInputIcon("threshold_CCIthreshold", label = "Threshold", value = 100,step = 1) %>% column(6,.),
            prettySwitch(
              "opposite_CCIthreshold", 
              label = "Change Buy and Sell", 
              value = FALSE, 
              status = "success",
              fill = TRUE
            ) %>% column(12,.)
          ),
          conditionalPanel(
            "input.strategy_selected == 'CMOthreshold'",
            numericInputIcon("period_CMOthreshold", label = "Period", value = 14,step = 1) %>% column(6,.),
            numericInputIcon("threshold_CMOthreshold", label = "Threshold", value = 50,step = 1) %>% column(6,.),
            prettySwitch(
              "opposite_CMOthreshold", 
              label = "Change Buy and Sell", 
              value = FALSE, 
              status = "success",
              fill = TRUE
            ) %>% column(12,.)
          ),
          conditionalPanel(
            "input.strategy_selected == 'CGOcross'",
            numericInputIcon("nFast_CGOcross", label = "Fast CGO", value = 10,step = 1) %>% column(6,.),
            numericInputIcon("nSlow_CGOcross", label = "Slow CGO", value = 20,step = 1) %>% column(6,.),
          )
        ),
        box(
          width = 6,
          title = "Trend line", 
          status = "warning",
          plotly::plotlyOutput('ts_plot')
        )
      ),
      fluidRow(
        box(
          width = 12,
          title = "Strategy Applied",
          status = "success",
          downloadBttn(
            outputId = "StrategyResult",
            label = "",
            style = "gradient",
            color = "primary"
          ) %>% column(1,.),
          tabsetPanel(
            tabPanel('Profit',shiny::dataTableOutput('profit_info')),
            tabPanel('Win',shiny::dataTableOutput('win_info')),
            tabPanel('Return', shiny::dataTableOutput('return_info')),
            tabPanel('Per Trade', DT::dataTableOutput('pertrade_info')),
            tabPanel('Graph',uiOutput('Strategy_plot')),
            tabPanel(
              'MAE & MAE',
              uiOutput('MAE_plot') %>% column(6,.),
              uiOutput('MFE_plot') %>% column(6,.)
            )
          )
        )
      )
    )
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)
