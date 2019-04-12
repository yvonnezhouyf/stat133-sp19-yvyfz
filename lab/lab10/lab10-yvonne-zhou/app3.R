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

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Drawing Balls Experiment"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("reps",
                  "Number of repetitions:",
                  min = 1,
                  max = 5000,
                  value = 100),
      sliderInput("prob",
                  "Threshold for choosing boxes:",
                  min = 0,
                  max = 1,
                  value = 0.5),
      numericInput("seed",
                   "Choose a random seed",
                   value = 12345)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("linePlot")
    )
  )
)

box1 = c("blue", "blue", "red")
box2 = c("blue", "blue", "red", "red", "red", "white")
proportion = function(n, drawn_balls) {
  if (n == 0) {
    return(c(1, 0, 0, 0, 0))
  } else {
    zero_blue = 0
    one_blue = 0
    two_blue = 0
    three_blue = 0
    four_blue = 0
    for (i in 1:n) {
      one_sample = drawn_balls[i,]
      num = 0
      for (j in 1:4) {
        if (one_sample[j] == "blue") {
          num = num + 1
        }
      }
      if (num == 0) {
        zero_blue = zero_blue + 1
      } else if (num == 1) {
        one_blue = one_blue + 1
      } else if (num == 2) {
        two_blue = two_blue + 1
      } else if (num == 3) {
        three_blue = three_blue + 1
      } else if (num == 4) {
        four_blue = four_blue + 1
      }
    }
    
    zero_blue = zero_blue / n
    one_blue = one_blue / n
    two_blue = two_blue / n
    three_blue = three_blue / n
    four_blue = four_blue / n
    
    return(c(zero_blue, one_blue, two_blue, three_blue, four_blue))
  }
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$linePlot <- renderPlot({
    results = c()
    set.seed(input$seed)
    for (i in 1:input$reps) {
      if (runif(1) > input$prob) {
        add = sample(box1, size = 4, replace = TRUE)
      } else {
        add = sample(box2, size = 4, replace = FALSE)
      }
      results = append(results, add)
    }
    drawn_balls = matrix(data = results, nrow = input$reps, ncol = 4, byrow = TRUE)
    
    balls = data.frame()
    for (i in 0:input$reps) {
      balls = rbind(balls, proportion(i, drawn_balls))
    }
    names(balls) = c("zero", "one", "two", "three", "four")
    balls$reps = 0:input$reps
    # generate bins based on input$bins from ui.R
    #x    <- balls[, 2] 
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    ggplot(data = balls) +
      geom_line(aes(x = reps, y = zero, color = "#F08080")) +
      geom_line(aes(x = reps, y = one, color = "#BDB76B")) +
      geom_line(aes(x = reps, y = two, color = "#7FFFD4")) + 
      geom_line(aes(x = reps, y = three, color = "#00BFFF")) + 
      geom_line(aes(x = reps, y = four, color = "#FF00FF")) +
      ylab("freqs") +
      labs(colour = "number") +
      scale_color_manual(labels = c("0", "1", "2", "3", "4"), values = c("#F08080", "#BDB76B", "#7FFFD4", "#00BFFF", "#FF00FF")) +
      ggtitle("Relative frequencies of number of blue balls")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

