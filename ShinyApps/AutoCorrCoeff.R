# Install required packages if not already installed
if (!require("shiny")) install.packages("shiny")
if (!require("ggokabeito")) install.packages("ggokabeito")
# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(ggokabeito)

# Define the UI
ui <- fluidPage(
  titlePanel("Bivariate Normal Distribution Simulator"),

  sidebarLayout(
    sidebarPanel(
      actionButton("go", "Simulate!"),
      sliderInput("n", "Number of Points", min = 10, max = 500, value = 100, step =10),
      sliderInput("a", "Correlation Coefficient", min = 0, max = 1, value = 0.995, step =0.005),
      sliderInput("n_reps", "# of Realizations", min = 1, max = 9, value = 5, step =1)
    ),

    mainPanel(
      plotOutput("plot")
    )
  )
)



# Define the server logic
server <- function(input, output, session) {
  #Reactive data input
  sim_data <- eventReactive(input$go,{
    set.seed(Sys.time())

    # Specify parameters
    n <- input$n             # Max 500
    a <- input$a        # Max 1
    n_reps <- input$n_reps       # Max 9
    sigma <- 1    # This just changes the vertical axis scale; it can be a constant 1

    df <- data.frame()
    for(i in 1:n_reps){
      x1 <- rep(0, n)
      for(j in 2:n){
        x1[j] = a * x1[j-1] + rnorm(1, mean = 0, sd = sigma)
      }

      temp <- data.frame(i = 1:length(x1),
                         x = x1,
                         rep = i)

      df <- rbind(df, temp)
    }
    return(df)
  })

  output$plot<-renderPlot({
    df <- sim_data()
    n_reps <- input$n_reps
    library(ggokabeito)
    ggplot(df, aes(x = i, y = x, color = factor(rep))) +
      geom_line() +
      scale_color_okabeito(
        palette = "full",
        reverse = TRUE,
        order = c(1,7,8,5,6,3,4,2,9),
        aesthetics = "color"
      ) +
      labs(title = paste0(n_reps," Realizations of a Stationary Time Series"),
           x = "Time",
           y = "x") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      guides(color = guide_legend(title = "Realization"))
  })
  # Not sure what this line was for
  # acf(df %>% filter(rep == 1) %>% dplyr::select(x), plot=FALSE, lag.max=5)

}


# Run the application
shinyApp(ui, server)


