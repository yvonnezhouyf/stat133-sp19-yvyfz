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

#Define some global functions
future_value <- function(amount, rate, years) {
  return(amount * (1 + rate) ** years)
}
annuity <- function(contrib, rate, years) {
  return(contrib * (((1 + rate) ** years) - 1) / rate)
}
growing_annuity <- function(contrib, rate, growth, years) {
  return(contrib * (((1 + rate) ** years - (1 + growth) ** years) / (rate - growth)))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Investment Comparison"),
   
   # Sidebar with a slider input for number of bins 
   #sidebarLayout(
   #    sidebarPanel(
   fluidRow(
     column(4, 
            sliderInput("initial",
                        "Initial Amount",
                        min = 0,
                        max = 100000,
                        value = 1000,
                        step = 500,
                        pre = "$"),
            sliderInput("acontrib",
                        "Annual Contribution",
                        min = 0,
                        max = 50000,
                        value = 2000,
                        step = 500,
                        pre = "$")
            ),
     column(4,
            sliderInput("rrate",
                        "Return Rate (in %)",
                        min = 0,
                        max = 20,
                        value = 5,
                        step = 0.1),
            sliderInput("growthr",
                        "Growth Rate (in %)",
                        min = 0,
                        max = 20,
                        value = 2,
                        step = 0.1)
            ),
     column(4,
            sliderInput("years",
                        "Years",
                        min = 0,
                        max = 50,
                        value = 20,
                        step = 1),
            selectInput("ifFacet",
                        "Facet?",
                        choices = c("No", "Yes"))
            )
      ),
      
      # Show plots, names
      hr(),
      p(strong("Timelines")),
      plotOutput("distPlot"),
      br(),
      p(strong("Balances")),
      verbatimTextOutput("summary_table")
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     # generate data frame based on input
     no_contrib = c(input$initial)
     fixed_contrib = c(input$initial)
     growing_contrib = c(input$initial)
     for (i in 1: input$years) {
       no_contrib = append(no_contrib, future_value(input$initial, input$rrate / 100, i))
       fixed_contrib = append(fixed_contrib, future_value(input$initial, input$rrate / 100, i) 
                              + annuity(input$acontrib, input$rrate / 100, i))
       growing_contrib = append(growing_contrib, future_value(input$initial, input$rrate / 100, i) 
                              + growing_annuity(input$acontrib, input$rrate / 100, input$growthr / 100, i))
     }
     year = 0:input$years
     modalities = data.frame(year, no_contrib, fixed_contrib, growing_contrib)
     # do not modify modalities, it needs to be print out
    
     if (input$ifFacet == "No") {
       # draw the plot
       ggplot(data = modalities) + 
         labs(x = "year", y = "value") +
         ggtitle("Three modes of investing") +
         geom_line(aes(x = year, y = no_contrib, group = 1, color = "#FF6347")) +
         geom_point(aes(x = year, y = no_contrib, group = 1, color = "#FF6347")) +
         geom_line(aes(x = year, y = fixed_contrib, group = 2, color = "blue")) +
         geom_point(aes(x = year, y = fixed_contrib, group = 2, color = "blue")) +
         geom_line(aes(x = year, y = growing_contrib, group = 3, color = "green")) +
         geom_point(aes(x = year, y = growing_contrib, group = 3, color = "green")) +
         scale_color_manual(name  ="variable",
                            labels = c("no_contrib", "fixed_contrib", "growing_contrib"),
                            values = c("#FF6347", "#228B22", "#56B4E9")
         )
       
     } else {
       # create a new data frame with types
       years = 0:input$years
       name = c("Year", "Value", "Type")
       no_c = data.frame(years, no_contrib, "1")
       fixed_c = data.frame(years, fixed_contrib, "2")
       growing_c = data.frame(years, growing_contrib, "3")
       
       names(no_c) = name
       names(fixed_c) = name
       names(growing_c) = name
       
       ratenames = c("1" = "no_contrib", "2" = "fixed_contrib", "3" ="growing_contrib")
       new_modalities = rbind(rbind(no_c, fixed_c), growing_c)
       
       # draw the plot
       ggplot(data = new_modalities, aes(x = Year, y = Value)) +  
         geom_point(aes(color = Type)) +
         geom_line(aes(color = Type)) +
         facet_wrap(~Type, labeller = as_labeller(ratenames)) +
         geom_area(aes(fill = Type, alpha = 1), show.legend = FALSE) +
         ylab("value") +
         xlab("year") +
         labs(colour = "variable") +
         theme_bw() +
         scale_color_manual(labels = c("no_contrib", "fixed_contrib", 
                                       "growing_contrib"), 
                            values = c("#FF6347", "#228B22", "#56B4E9"))
     }
   })
   
   output$summary_table <- renderPrint({
     # generate data frame based on input
     no_contrib = c(input$initial)
     fixed_contrib = c(input$initial)
     growing_contrib = c(input$initial)
     for (i in 1: input$years) {
       no_contrib = append(no_contrib, future_value(input$initial, input$rrate / 100, i))
       fixed_contrib = append(fixed_contrib, future_value(input$initial, input$rrate / 100, i) 
                              + annuity(input$acontrib, input$rrate / 100, i))
       growing_contrib = append(growing_contrib, future_value(input$initial, input$rrate / 100, i) 
                                + growing_annuity(input$acontrib, input$rrate / 100, input$growthr / 100, i))
     }
     year = 0:input$years
     modalities = data.frame(year, no_contrib, fixed_contrib, growing_contrib)
     # do not modify modalities, it needs to be print out
     
     modalities
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

