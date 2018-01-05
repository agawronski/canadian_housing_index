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

title <- "New Housing Price Index - Canada"

ui <- fluidPage(
  titlePanel(title, windowTitle = title),
  a("Click Here for original data from Statistics Canada", 
  href = "http://www5.statcan.gc.ca/cansim/a26?lang=eng&retrLang=eng&id=3270056&&pattern=&stByVal=1&p1=1&p2=31&tabMode=dataTable&csid="),
  tags$div(tags$br()),
  selectInput(inputId = "GEO", "Choose GEO", as.list(unique(data$GEO))),
  selectInput(inputId = "INDEX", "Choose INDEX", as.list(unique(data$INDEX))),
  sliderInput(inputId = "num_months",
              label = "Choose the number of months to forecast",
              value = 25, min = 1, max = 100),
  plotOutput("time_series"),
  verbatimTextOutput("model_summary"),
  plotOutput("forecast")
)


server <- function(input, output) {
  output$time_series <- renderPlot({
    dataS <- data[data$GEO == input$GEO,]
    dataS <- dataS[dataS$INDEX == input$INDEX,]
    plot(dataS$date, 
         dataS$Value, 
         type = "l", 
         col = randomColor(),
         main = input$GEO,
         xlab = "Date", 
         ylab = "Housing Price Index")
  })
  
  output$model_summary <- renderPrint({
    dataS <- data[data$GEO == input$GEO,]
    dataS <- dataS[dataS$INDEX == input$INDEX,]
    fit <- auto.arima(dataS$Value)
    print(summary(fit))
  })
  
  output$forecast <- renderPlot({
    dataS <- data[data$GEO == input$GEO,]
    dataS <- dataS[dataS$INDEX == input$INDEX,]
    fit <- auto.arima(dataS$Value)
    yhat <- forecast(fit, h = input$num_months)
    plot(yhat)
  })
}

shinyApp(ui = ui, server = server)











