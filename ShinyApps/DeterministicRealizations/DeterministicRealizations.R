# Install required packages if not already installed
# if (!require("shiny")) install.packages("shiny")
# if (!require("ggokabeito")) install.packages("ggokabeito")
# if (!requireNamespace("devtools")) install.packages("devtools")
# devtools::install_github("rstudio/packrat")
# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(ggokabeito)
library(see)
library(MASS)

# Define the UI
ui <- fluidPage(
    titlePanel("Stationary Deterministic Time Series"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("n", "Number of Points", min = 10, max = 500, value = 100, step =10),
            sliderInput("sigma", "Sigma", min = 0, max = 5, value = 1, step =0.1),
            sliderInput("scale", "Scale", min = 0.5, max = 20, value = 3, step =0.5),
            sliderInput("n_reps", "# of Realizations", min = 1, max = 9, value = 5, step =1)
        ),

        mainPanel(
          plotOutput("plot")
        )
    )
)



# Define the server logic
server <- function(input, output, session) {
  output$plot<-renderPlot({
    set.seed(Sys.time())
    #Reactive data input
    n <- input$n             # Max 500
    n_reps <- input$n_reps       # Max 9
    sigma <- input$sigma    # Max 5
    scale <- input$scale # Max 20
    extra_values <- 4

    df <- data.frame(i = as.integer(), x = as.numeric(), rep = as.integer())
    t <- rep(0, n + extra_values)
    e1 <- rep(0, n + extra_values)
    for(i in 1:n_reps){
      for(j in 1:(n + extra_values)) {
        e1[j] <- rnorm(1, mean = 0, sd = sigma)
        t[j] <- j
      }

      temp <- data.frame(i = t-1,
                         x = 5 * sin((t-1)*scale*2*pi/100)
                         + e1 + lead(e1,1) + lead(e1,2) + lead(e1,3) + lead(e1,extra_values), # Create autocorrelation
                         rep = i) %>%
        mutate(x = ifelse(i == 0, 0, x)) |>
        na.omit()

      df <- rbind(df, temp)
    }

    library(ggokabeito)
    ggplot(df, aes(x = i, y = x, color = factor(rep))) +
      # geom_line() +
      geom_line(linewidth = ifelse(df$rep == 1, 1, 0.5)) +
      scale_color_okabeito(
        palette = "full",
        reverse = TRUE,
        order = c(1,7,8,5,6,3,4,2,9),
        aesthetics = "color"
      ) +
      labs(title = paste0(n_reps," Realizations of a (Stationary) Deterministic Time Series"),
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

