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
library(rsconnect)
source("functions.R")

rsconnect::setAccountInfo(name='aliciak', token='6601F96B00D02C18B544AD2F1A39D4E1', secret='7tv++Iayp0PEycb9tKWzhzPJOXJqfMzpGyy/t82g')


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Expected Return Values on Three Different Investing Modes"),
   
   fluidRow(
     
     column(width = 4,
   
       # Sidebar with a slider input for number of bins 
       sliderInput(inputId = "initial", 
                         label = "Initial Amount",
                         value = 1000,
                         min = 1,
                         max = 100000,
                        step = 500),
       
       sliderInput(inputId = "annualContribution", 
                   label = "Annual Contribution",
                   value = 2000,
                   min = 0,
                   max = 50000,
                   step = 500)
       
     ),
   
   
     column(width = 4,
   
   
       sliderInput(inputId = "returnRate", 
                   label = "Return Rate (in %)",
                   value = 5,
                   min = 0,
                   max = 20,
                   step = 0.1),
       
       sliderInput(inputId = "growthRate", 
                   label = "Growth Rate (in %)",
                   value = 2,
                   min = 0,
                   max = 20,
                   step = 0.1)
       
     ),
     
     
     column(width = 4,
            
            
            sliderInput(inputId = "years", 
                        label = "Years",
                        value = 20,
                        min = 0,
                        max = 50,
                        step = 1),
            
            selectInput(inputId = "facet",
                        label = "Facet?",
                        c("No", "Yes"))
     )
     
   ),
   
   #Main panel for displaying outputs...
   
   mainPanel(
     
     # Output: Table...
     
     h4("Timelines"), plotOutput("savingsTimeline"), h4("Balances"), tableOutput("yearlyBalances")
     
     
   )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dataset <- reactive({
    modalitiesFrame(as.numeric(input$initial), as.numeric(input$returnRate * .01), as.numeric(input$annualContribution), as.numeric(input$growthRate * .01), as.numeric(input$years))
    })
  
  graphtypeInput <- reactive({
                          switch(input$facet,
                           "No" = ggplot(data = dataset(), aes(x=year, y = growing_contrib)) + geom_line(aes(y = no_contrib, colour = "blue")) + geom_line(aes(y=growing_contrib, colour = "red"))  +  geom_line(aes(y=fixed_contrib, colour = "green"))  + labs(title = "Three Modes of Investing", x = "Year", y = "Value") + scale_x_continuous(breaks=seq(0, input$years, 1)) + scale_colour_discrete(name = "Modalities", labels = c("no_contrib","fixed_contrib", "growing_contrib")) + theme_minimal(),
                           "Yes" =ggplot(data = melt(dataset(), c("year")), aes(x=year, y = value, fill = variable)) + geom_line() + theme_minimal() + facet_wrap(~ variable) + geom_area(aes(alpha = 0.6)) + geom_point(aes(color = variable)) + labs(title = "Three Modes of Investing", x = "Year", y = "Value") + scale_colour_discrete(name = "Modalities", labels = c("no_contrib","fixed_contrib", "growing_contrib")) + guides(fill=FALSE, alpha = FALSE) 
                           )})
  
  output$savingsTimeline <- renderPlot({
    
    graphtypeInput()
    
  })
  
  output$yearlyBalances <- renderTable({
    dataset()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

