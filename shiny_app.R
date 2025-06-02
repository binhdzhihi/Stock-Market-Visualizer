library(shiny)
library(shinydashboard)
library(tidyverse)
library(highcharter)
library(plotly)
library(zoo)

# Load and preprocess dataset
df <- read_csv("stock.csv") %>% 
  mutate(Date = as.Date(Date))  # Ensure Date column is of Date type

# List of available stock symbols
stocks <- c("AAPL","AMZN","GOOGL","MSFT","NVDA")

# UI layout
ui <- dashboardPage(
  dashboardHeader(title = "Stock Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visualize", tabName = "viz", icon = icon("chart-line")),
      # Multi-select input allowing user to pick one or more stocks
      selectInput("stock", "Choose Stock(s):", choices = stocks, multiple = TRUE, selected = stocks[1]),
      # Date range picker to filter data by date
      dateRangeInput("dateRange", "Date Range:",
                     start = min(df$Date), end = max(df$Date),
                     min = min(df$Date), max = max(df$Date)),
      # Radio buttons to choose the type of chart to display
      radioButtons("chartType", "Chart Type:",
                   choices = c(
                     "Candlestick"   = "candlestick",
                     "Volume"        = "volume",
                     "Price Trend"   = "price_trend",
                     "Volatility"    = "volatility"
                   ),
                   selected = "candlestick"),
      # Conditional input: show moving average window selector only if Price Trend chart is chosen
      conditionalPanel(
        condition = "input.chartType == 'price_trend'",
        selectInput("ma_window", "Select Moving Average Window (days):",
                    choices = c(5, 15, 30), selected = 30)
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("viz",
              fluidRow(
                box(
                  # Placeholder UI element for the selected plot output
                  uiOutput("plot_ui"),
                  width = 12, height = 650
                )
              )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive expression to prepare combined data for selected stocks and date range
  multi_stock_data <- reactive({
    req(input$stock)  # Require that at least one stock is selected
    stocks_selected <- input$stock
    req(length(stocks_selected) > 0)  # Require non-empty selection
    
    # Filter dataset by selected date range
    df_filtered <- df %>%
      filter(Date >= input$dateRange[1], Date <= input$dateRange[2])
    
    # Validate stocks have data available after filtering
    valid_stocks <- stocks_selected[sapply(stocks_selected, function(st) {
      nrow(df_filtered %>% filter(!is.na(!!sym(paste0("Close_", st))))) > 0
    })]
    
    # Show notification and return NULL if no valid data for selected stocks
    if(length(valid_stocks) == 0){
      showNotification("No data available for selected stocks and date range.", type = "error", duration = NULL)
      return(NULL)
    }
    
    # For each valid stock, select relevant columns and add stock symbol as a column
    list_of_dfs <- lapply(valid_stocks, function(st) {
      df_filtered %>%
        select(Date,
               Open = !!sym(paste0("Open_", st)),
               High = !!sym(paste0("High_", st)),
               Low = !!sym(paste0("Low_", st)),
               Close = !!sym(paste0("Close_", st)),
               Volume = !!sym(paste0("Volume_", st))
        ) %>%
        filter(!is.na(Close)) %>%  # Remove rows where Close price is missing
        mutate(Stock = st)  # Add stock identifier column
    })
    
    # Combine all individual stock data frames into one
    combined <- bind_rows(list_of_dfs)
    combined
  })
  
  # Dynamically render appropriate plot output UI element based on chartType selection
  output$plot_ui <- renderUI({
    switch(input$chartType,
           "candlestick" = highchartOutput("hc", height = "650px"),
           "volume"      = plotlyOutput("eco", height = "650px"),  # volume chart uses plotly for better multi-stock handling
           "price_trend" = plotlyOutput("pt", height = "650px"),
           "volatility"  = plotlyOutput("vol", height = "650px")
    )
  })
  
  # Candlestick chart using highcharter for multi-stock OHLC visualization
  output$hc <- renderHighchart({
    d <- multi_stock_data()
    req(!is.null(d), nrow(d) > 0)  # Require non-null and non-empty data
    
    # Prepare separate data series for each stock
    series_list <- lapply(unique(d$Stock), function(st) {
      dat <- d %>%
        filter(Stock == st) %>%
        arrange(Date) %>%
        # Convert Date to milliseconds as required by Highcharts
        mutate(Date = as.numeric(as.POSIXct(Date)) * 1000)
      
      # Create named list containing OHLC data for this stock
      list(
        name = st,
        data = list_parse2(dat %>% select(Date, Open, High, Low, Close)),
        type = "candlestick"
      )
    })
    
    # Create highcharter stock chart with zoom and range selector buttons
    hc <- highchart(type = "stock") %>%
      hc_chart(zoomType = "x") %>%
      hc_title(text = paste("OHLC Candlestick - Stocks:", paste(unique(d$Stock), collapse = ", "))) %>%
      hc_rangeSelector(
        enabled = TRUE,
        buttons = list(
          list(type = "month", count = 1, text = "1m"),
          list(type = "month", count = 3, text = "3m"),
          list(type = "month", count = 6, text = "6m"),
          list(type = "all", count = 0, text = "All")
        )
      )
    
    # Add each stock's series to the chart
    for (s in series_list) {
      hc <- hc %>% hc_add_series_list(list(s))
    }
    hc
  })
  
  # Volume trends chart using plotly for multi-stock line charts with smoothing
  output$eco <- renderPlotly({
    d <- multi_stock_data() %>%
      arrange(Stock, Date) %>%
      group_by(Stock) %>%
      # Smooth volume using 10-day rolling mean for better trend visualization
      mutate(SmoothVolume = zoo::rollmean(Volume, 10, fill = NA, align = "right")) %>%
      ungroup()
    
    req(nrow(d) > 0)
    
    # Initialize empty plotly plot
    p <- plot_ly()
    # Add a line for each stock's smoothed volume
    for (st in unique(d$Stock)) {
      dat <- d %>% filter(Stock == st)
      p <- p %>%
        add_lines(x = dat$Date, y = dat$SmoothVolume, name = paste(st, "Volume"))
    }
    # Layout configuration with x-axis range slider enabled
    p %>%
      layout(
        title = "Volume Trends by Stock",
        xaxis = list(
          title = "Date",
          rangeslider = list(visible = TRUE),  
          type = "date"
        ),
        yaxis = list(title = "Volume")
      )
  })
  
  # Price Trend chart with customizable moving average window (5, 15, 30 days)
  output$pt <- renderPlotly({
    # Use selected moving average window or default to 30
    ma_w <- ifelse(is.null(input$ma_window), 30, as.numeric(input$ma_window))
    d <- multi_stock_data()
    req(!is.null(d), nrow(d) > 0)
    
    d <- d %>%
      arrange(Stock, Date) %>%
      group_by(Stock) %>%
      # Calculate rolling moving average of Close prices
      mutate(MA = zoo::rollmean(Close, ma_w, fill = NA, align = "right")) %>%
      ungroup()
    
    # Initialize empty plotly plot
    p <- plot_ly()
    # Add Close price and MA lines for each stock
    for (st in unique(d$Stock)) {
      dat <- d %>% filter(Stock == st)
      p <- p %>%
        add_lines(x = dat$Date, y = dat$Close, name = paste0(st, " Close")) %>%
        add_lines(x = dat$Date, y = dat$MA, name = paste0(st, " ", ma_w, "-Day MA"), line = list(dash = "dash"))
    }
    # Layout with x-axis range slider
    p %>%
      layout(
        title = paste("Closing Price and", ma_w, "Day MA for Stocks:", paste(unique(d$Stock), collapse = ", ")),
        xaxis = list(title = "Date", rangeslider = list(visible = TRUE), type = "date"),
        yaxis = list(title = "Price")
      )
  })
  
  # Volatility chart showing 20-day rolling standard deviation of Close price
  output$vol <- renderPlotly({
    d <- multi_stock_data()
    req(!is.null(d), nrow(d) > 0)
    
    d <- d %>%
      arrange(Stock, Date) %>%
      group_by(Stock) %>%
      # Calculate rolling standard deviation for volatility
      mutate(Volatility = zoo::rollapply(Close, 20, sd, fill = NA, align = "right")) %>%
      ungroup()
    
    # Initialize empty plotly plot
    p <- plot_ly()
    # Add volatility line for each stock
    for (st in unique(d$Stock)) {
      dat <- d %>% filter(Stock == st)
      p <- p %>% add_lines(x = dat$Date, y = dat$Volatility, name = paste0(st, " Volatility"))
    }
    
    # Layout with x-axis range slider
    p %>%
      layout(
        title = paste("20-Day Volatility for Stocks:", paste(unique(d$Stock), collapse = ", ")),
        xaxis = list(title = "Date", rangeslider = list(visible = TRUE), type = "date"),
        yaxis = list(title = "Std. Dev.")
      )
  })
  
}

# Run the Shiny app
shinyApp(ui, server)
