# Install required packages if not already installed
# if (!require("shiny")) install.packages("shiny")
# if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("ggfortify")) install.packages("ggfortify")
# if (!require("tsibble")) install.packages("tsibble")
# if (!require("dplyr")) install.packages("dplyr")
# if (!require("purrr")) install.packages("purrr")
# if (!require("slider")) install.packages("slider")

library(shiny)
library(ggplot2)
library(tsibble)
library(dplyr)
library(purrr)
library(slider)
library(forecast)
library(ggfortify)

# Define the UI
ui <- fluidPage(
  withMathJax(),
  titlePanel("MA(3) Process Simulation"),
  fluidRow(
    column(3, numericInput("n", label = "$$ \\text{Numerical Value (n)} $$", value = 1000)),
    column(3, numericInput("beta1", label = "$$\\beta_1$$", value = 0.7)),
    column(3, numericInput("beta2", label = "$$\\beta_2$$", value = 0.5)),
    column(3, numericInput("beta3", label = "$$\\beta_3$$", value = 0.2)),
  ),
  fluidRow(
    column(4, offset = 5, actionButton("go", "Simulate!")),
    column(12, h4(""))
  ),
  fluidRow(
    column(6, plotOutput("plot1")),
    column(6, plotOutput("plot3")),
    ),
  fluidRow(
    column(12, plotOutput("plot2"))
  )
)

server <- function(input, output) {

  simulate_data <- eventReactive(input$go, {
    beta1 <- input$beta1
    beta2 <- input$beta2
    beta3 <- input$beta3

    dat <- tibble(
      w = rnorm(input$n),
      betas = list(c(beta1, beta2, beta3))) |>
      mutate(
        w_lag = slide(w, ~.x, .before = 3, .after = -1),
        w_lag = map(w_lag, ~rev(.x)),
        t = 1:n()) |>
      slice(-c(1:3)) |>
      mutate(
        lag_betas = map2_dbl(
          w_lag,
          betas,
          \(.x, .y) sum(.x * .y)),
        x = w + lag_betas) |>
      tsibble::as_tsibble(index = t)

    return(dat)
  })

  output$plot1 <- renderPlot({
    req(input$go)  # Wait for actionButton to be pressed
    beta1 <- input$beta1
    beta2 <- input$beta2
    beta3 <- input$beta3

    rho <- function(k, beta) {
      q <- length(beta) - 1
      if (k > q) ACF <- 0 else {
        s1 <- 0; s2 <- 0
        for (i in 1:(q-k+1)) s1 <- s1 + beta[i] * beta[i+k]
        for (i in 1:(q+1)) s2 <- s2 + beta[i]^2
        ACF <- s1 / s2
      }
      ACF
    }

    acf_dat <- tibble(
      order = 0:10,
      betas = list(c(1, beta1, beta2, beta3)),
      rho.k = map2_dbl(order, betas, ~rho(.x, .y))
    )

    ggplot(acf_dat, aes(x = order, y = rho.k)) +
      geom_hline(yintercept = 0, color = "darkgrey") +
      geom_point() +
      labs(y = expression(rho[k]), x = "lag k") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("Theoretical ACF for the Simulated MA(3) Process")
  })

  output$plot2 <- renderPlot({
    req(input$go)  # Wait for actionButton to be pressed
    # Convert tbl_ts to data frame
    df <- as.data.frame(simulate_data())
    # Plot using plot()
    plot(df$t, df$x, type = "l", xlab = "Time", ylab = "Value", main = "MA(3) Process Simulation")
  })

  output$plot3 <- renderPlot({
    req(input$go)  # Wait for actionButton to be pressed

    # ACF plot data
    acf_plot_data <- simulate_data() |>
      pull(x) |>
      acf() |>
      autoplot(main= "Observed ACF from Simulated MA(3) Process")

    # Plot ACF using ggplot
    acf_plot_data
  })
}


shinyApp(ui = ui, server = server)