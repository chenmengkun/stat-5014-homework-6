#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(gridExtra)

# UI
ui<-shinyUI(fluidPage(
  titlePanel("Stock Price & Portfolio Worth"),
  
  sidebarLayout(
    sidebarPanel(
      selectstock <- selectInput(
        "selected_stock",
        "Stock",
        choices = list("AAPL", "MSFT", "MRNA", "BNTX", "PFE"),
        selectize = TRUE
      ),
      
      AAPLamount <-
        numericInput("AAPL_amount", "AAPL Amount", value = 0, min = 0),
      
      MSFTamount <-
        numericInput("MSFT_amount", "MSFT Amount", value = 0, min = 0),
      
      MRNAamount <-
        numericInput("MRNA_amount", "MRNA Amount", value = 0, min = 0),
      
      BNTXamount <-
        numericInput("BNTX_amount", "BNTX Amount", value = 0, min = 0),
      
      PFEamount <-
        numericInput("PFE_amount", "PFE Amount", value = 0, min = 0)
    ),
    
    mainPanel(plotOutput("invest_plot"))
  )
))


# Server
server<-function(input, output) {
  stock<-readRDS("../data/stock.RDS")
  
  stock_selcted<-reactive({
    stock[stock$symbol == input$selected_stock,]
  })
  
  stock_worth<-reactive({
    stock_amount<-c(input$AAPL_amount, input$MSFT_amount, input$MRNA_amount, 
                    input$BNTX_amount, input$PFE_amount)
    
    d<-unique(stock$date)
    
    worth<-rep(0, length(d))
    
    for (i in 1:length(worth)) {
      price<-stock$adjusted[stock$date == d[i]]
      worth[i]<-sum(price*stock_amount)
    }
    
    data.frame(date = d, worth)
  })
  
  output$invest_plot<-renderPlot({
    p1<-ggplot(stock_selcted(), aes(x = date, y = adjusted)) + 
      geom_line() + 
      labs(y = "stock price", title = paste(input$selected_stock, "Stock Price")) + 
      theme_bw()
      
    
    p2<-ggplot(stock_worth(), aes(x = date, y = worth)) + 
      geom_line() + 
      labs(y = "protforlio worth", title = "Investment Portforlio Worth") + 
      theme_bw()
    
    grid.arrange(p1, p2, nrow = 2)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

