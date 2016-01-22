library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Aproximaciones de la integral en el intervalo (0,2) para la familia de exponenciales"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      #      radioButtons("dist", "Distribution type:",
      #                  c("Normal" = "norm",
      #                   "Log-normal" = "lnorm",
      #                  "Uniform" = "unif",
      #                 "Exponential" = "exp")),
      # br(),
      
      sliderInput("alpha", 
                  "Nivel de significancia:", 
                  value = .05,
                  min = .01, 
                  max = .1,
                  step=.01),
      
      sliderInput("MinM", 
                  "Muestra Minima:", 
                  value = 500,
                  min = 10, 
                  max = 1000),
      
      sliderInput("NSimulaciones", 
                  "Numero de simulaciones:", 
                  value = 100,
                  min = 10, 
                  max = 10000),
      
      sliderInput("m",
                  "Lambda de funcion original:",
                  min = 0,
                  max = 5,
                  value=.5
      ),
      
      sliderInput("lg",
                  "Lambda de la funcion g:",
                  min = 0,
                  max = 5,
                  value=.5
      )
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Importance Sampling",plotOutput("plot1_1"))
      )
    )
  )
))
    
      
      