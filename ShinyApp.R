install.packages("shiny")
install.packages("ggplot2")

install.packages('rugarch')
install.packages('forecast')
install.packages('tseries')


library(shiny)
library(ggplot2)
library(dplyr)
library(hrbrthemes)

library("rugarch")
library(forecast)
library(tseries)


ui <- fluidPage(
  titlePanel("Home Owner Risk Analysis"),
  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        "state", "Where do you live?", choices ,
        multiple = FALSE
      ),
      numericInput("HomePrice", "Home Price:", 200000, min = 1),
      numericInput("MortgageValue", "Mortgage Value:", 160000, min = 1)
    ),
   
    
     mainPanel(
       tabsetPanel(
         tabPanel("Value History ",
                  plotOutput("plot")
          ),
         tabPanel("Home Equity Risk / Value at Risk",
                  tableOutput("table"))
       )
    )
  )
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    
    dataset <- filter(HousePriceIndex, place_name == input$state)
    title <- paste("Home Price Index For:", input$state, sep = " ")
    
    ggplot(data= dataset, aes(x=Date, y = index_nsa, group=1))+ 
      geom_line(color="#69b3a2", size=2) +
      ylab("Price Index   (100 = 1995 Price)") +
      ggtitle(title)+
      theme_ipsum()
  })
  output$table <- renderTable({
    dataset <- filter(HousePriceIndex, place_name == input$state)
    
    return = diff(dataset$index_nsa) / dataset$index_nsa[-length(dataset$index_nsa)]
    
    ##GARCH code- partailly adapted from Rpubs
    
    model.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) , 
                            mean.model = list(armaOrder = c(1 , 1)))
    
    model.roll = ugarchroll(spec = model.spec , data = return , n.start = length(return)- 6 , refit.every = 5 ,
                            refit.window = 'moving')
    
    VaR99_td = mean(return) + model.roll@forecast$density[,'Sigma']*qdist(distribution='std', shape=3.7545967917, p=0.01)
    VaR95_td = mean(return) + model.roll@forecast$density[,'Sigma']*qdist(distribution='std', shape=3.7545967917, p=0.05)
    VaR90_td = mean(return) + model.roll@forecast$density[,'Sigma']*qdist(distribution='std', shape=3.7545967917, p=0.10)
    VaR80_td = mean(return) + model.roll@forecast$density[,'Sigma']*qdist(distribution='std', shape=3.7545967917, p=0.20)
    
    CurrentVaR1 <- (VaR99_td[6] + 1)^4 * 100
    CurrentVaR5 <- (VaR95_td[6] + 1)^4 * 100
    CurrentVaR10 <- (VaR90_td[6] + 1)^4 * 100
    CurrentVaR20 <- (VaR80_td[6] + 1)^4 * 100
    
    
    DollarLoss1 <- input$HomePrice * (CurrentVaR1 - 100) / 100
    DollarLoss2 <- input$HomePrice * (CurrentVaR5 - 100) / 100
    DollarLoss3 <- input$HomePrice * (CurrentVaR10 - 100) / 100
    DollarLoss4 <- input$HomePrice * (CurrentVaR20 - 100) / 100
    
    RemaingEquity1 <- (input$HomePrice - input$MortgageValue + DollarLoss1)
    RemaingEquity2 <- (input$HomePrice - input$MortgageValue + DollarLoss2)
    RemaingEquity3 <- (input$HomePrice - input$MortgageValue + DollarLoss3)
    RemaingEquity4 <- (input$HomePrice - input$MortgageValue + DollarLoss4)

    df <- data.frame(Loss_Probability=c("1%","5%","10%","20%"), 
                     Annual_Percent=c(CurrentVaR1 - 100, CurrentVaR5 - 100, CurrentVaR10 - 100, CurrentVaR20 -100),
                     Dollar_Loss=c(DollarLoss1, DollarLoss2, DollarLoss3, DollarLoss4),
                     Remaining_Equity=c(RemaingEquity1,RemaingEquity2,RemaingEquity3,RemaingEquity4))
    
    df
    
    }) 

}

shinyApp(ui, server)
