library(randomcoloR)
library(jsonlite)
library(forecast)
library(shiny)


data <- read.csv("STATCAN_NEW_HOUSING_PRICE_INDEXES_CANSIM_327_0046.csv", stringsAsFactors = F)
data$date <- as.Date(paste0(gsub("[/]", "-", data$Ref_Date), "-01"))
data <- data[data$Value != "..",]
data <- data[data$Value != "x",]
data$Value <- as.numeric(data$Value)
dataS <- data[data$GEO == "Ontario",]
dataS <- dataS[dataS$INDEX == "House only",]

title <- "Arima Forecast: New Housing Price Index - Canada"

ui <- fluidPage(
  titlePanel(title, windowTitle = title),
  a("Click Here for original data from Statistics Canada", 
  href = "http://www5.statcan.gc.ca/cansim/a26?lang=eng&retrLang=eng&id=3270056&&pattern=&stByVal=1&p1=1&p2=31&tabMode=dataTable&csid=", target="_blank"),
  tags$div(tags$br()),
  a("Click Here for information about the 'forecast' R package used to create this", 
    href = "http://pkg.robjhyndman.com/forecast/", target="_blank"),
  tags$div(tags$br()),
  selectInput(inputId = "GEO", "Choose GEO", as.list(unique(data$GEO))),
  selectInput(inputId = "INDEX", "Choose INDEX", as.list(unique(data$INDEX))),
  sliderInput(inputId = "num_months",
              label = "Choose the number of months to forecast",
              value = 25, min = 1, max = 100),
  plotOutput("time_series"),
  verbatimTextOutput("model_summary"),
  plotOutput("forecast"),
  verbatimTextOutput("forecast_details")
)


server <- function(input, output) {
  dataSub <- reactive({
    dataS <- data[data$GEO == input$GEO,]
    dataS <- dataS[dataS$INDEX == input$INDEX,]
  })
  
  output$time_series <- renderPlot({
    plot(dataSub()$date, 
         dataSub()$Value, 
         type = "l", 
         # col = randomColor(),
         col = "darkBlue",
         main = paste(input$GEO, "-", input$INDEX),
         xlab = "Date", 
         ylab = "Housing Price Index")
  })
  
  model_fit <- reactive({
    auto.arima(dataSub()$Value)
  })  
  
  output$model_summary <- renderPrint({
    print(summary(model_fit()))
  })
  
  yhat <- reactive({
    forecast(model_fit(), h = input$num_months)
  })
  
  output$forecast <- renderPlot({
    plot(yhat())
  })
  
  output$forecast_details <- renderPrint({
    y_out <- data.frame(yhat())
    y_out <- apply(y_out, 2, function(x) round(x, 1))
    cols <- c("Point.Forecast", "Lo.95", "Lo.80", "Hi.80", "Hi.95")
    y_out <- y_out[,cols]
    y_out
  })
}

shinyApp(ui = ui, server = server)











