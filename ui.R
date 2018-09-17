library(shiny)

# Define UI for the application of Discrete Distributions
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Discrete Distributions"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("distribution", "Choose a Discrete distribution:",
                  choices = list("Bernoulli", "Binomial", "Geometric","Negative Binomial")
      ),
       sliderInput("p","Probability of Success:",
                   min = 0, max = 1, value = 0.5),
      
       sliderInput("n","Number of trials:",
                   min = 1, max = 100, value = 1, step = 1),
      
       sliderInput("r","Number of successes:",
                   min = 1, max = 25, value = 1, step = 1),
      
      # Show data table -------------------------------------------------------
      checkboxInput(inputId = "show_data",
                    label = "Show Probability Distribution",
                    value = FALSE)
    ),

       
    # Show a plot of the generated distribution
    mainPanel(
      span(h4(textOutput("text1")),style="color:darkblue"),
      plotOutput("Distribution_Plot"),
      br(), 
      DT::dataTableOutput("Probability_Distribution")
       
    )
  )
))
