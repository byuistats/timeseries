# Install required packages if not already installed
if (!require("shiny")) install.packages("shiny")
if (!require("ggokabeito")) install.packages("ggokabeito")
if (!require("ggplot2")) install.packages("ggplot2")

library(shiny)
library(dplyr)
library(ggplot2)
library(ggokabeito)
library(fpp3)
library(shiny)
library(shinyjs)
library(shinyWidgets)





ui <- fluidPage(
  tags$head(
    tags$link(rel="stylesheet",
              href="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css",
              integrity="sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ",
              crossorigin="anonymous"),
    HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js" integrity="sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij" crossorigin="anonymous"></script>'),
    HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous"></script>'),
    HTML('
    <script>
      document.addEventListener("DOMContentLoaded", function(){
        renderMathInElement(document.body, {
          delimiters: [{left: "$", right: "$", display: false}]
        });
      })
    </script>')
  ),
  withMathJax(),
  useShinyjs(),
  titlePanel("Harmonic Seasonals"),
  fluidRow(
    column(2, materialSwitch(inputId = "toggle_addInputs", value = FALSE, label = "Additional Inputs")),
    column(4, offset = 5, actionButton("reset", "Reset Parameters"))
  ),
  fluidRow(
    column(6, sliderInput("S1", label = "S1", min = 0, max = 1, value = 0.01, step = 0.01)),
    column(6, sliderInput("C1", label = "C1", min = 0, max = 1, value = 0.01, step = 0.01)),
    column(6, sliderInput("S2", label = "S2", min = 0, max = 1, value = 0.00, step = 0.01)),
    column(6, sliderInput("C2", label = "C2", min = 0, max = 1, value = 0.00, step = 0.01)),
    column(6, sliderInput("S3", label = "S3", min = 0, max = 1, value = 0.00, step = 0.01)),
    column(6, sliderInput("C3", label = "C3", min = 0, max = 1, value = 0.00, step = 0.01))
  ),
  div(id = "AddInputs",
      fluidRow(
        column(6, sliderInput("S4", label = "S4", min = 0, max = 1, value = 0.00, step = 0.01)),
        column(6, sliderInput("C4", label = "C4", min = 0, max = 1, value = 0.00, step = 0.01)),
        column(6, sliderInput("S5", label = "S5", min = 0, max = 1, value = 0.00, step = 0.01)),
        column(6, sliderInput("C5", label = "C5", min = 0, max = 1, value = 0.00, step = 0.01)),
        column(6, sliderInput("S6", label = "S6", min = 0, max = 1, value = 0.00, step = 0.01)),
        column(6, sliderInput("C6", label = "C6", min = 0, max = 1, value = 0.00, step = 0.01))
      ),
  ),
  fluidRow(
    column(4, offset = 5, actionButton("go", "Plot!"))
  ),
  fluidRow(
    column(12, h4("")),
    column(12, div(style = "display: flex; justify-content: center; align-items: center;", uiOutput("formula"))),
    column(12, h4("")),
    column(12, plotOutput("plot"))
  )
)

server <- function(input, output, session) {
  observe({
    if(input$toggle_addInputs) {
      show("AddInputs")
    } else {
      hide("AddInputs")
    }
  })

  observe({
    if(input$reset) {
      updateSliderInput(session, "S1", value = 0.01)
      updateSliderInput(session, "C1", value = 0.01)
      updateSliderInput(session, "S2", value = 0)
      updateSliderInput(session, "C2", value = 0)
      updateSliderInput(session, "S3", value = 0)
      updateSliderInput(session, "C3", value = 0)
      updateSliderInput(session, "S4", value = 0)
      updateSliderInput(session, "C4", value = 0)
      updateSliderInput(session, "S5", value = 0)
      updateSliderInput(session, "C5", value = 0)
      updateSliderInput(session, "S6", value = 0)
      updateSliderInput(session, "C6", value = 0)
    } else {
      ""
    }
  })

  sim_data <- eventReactive(input$go, {
    # Headr: Explanation of Simulation Parameters
    # Purpose: Setting up simulation parameters for Fourier series approximation.

    # Parameters for Simulation:
    # - Season: The period of the function.
    # - T: Total time duration for the simulation.
    Season <- 12
    t_val <- 36

    # seq(from, to, length.out = n)
    # evenly space out 1000 "n" in this case to see better
    t_values <- seq(0, t_val - 1, length.out = 1000)



    # Compute simulated values for the Fourier series expression
    # based on user inputs for coefficients S1 to S6 and C1 to C6.
    # Reference: `paste0()`` is a function in R used to concatenate strings without any separator.
    # It takes an arbitrary number of arguments and concatenates them togeteer
    sim_values <- sapply(t_values, function(t) {
      sum_values <- 0
      for (i in 1:(Season/2)) {
        sum_values <- sum_values + (
          input[[paste0("S", i)]] * sin(2 * pi * i * t / Season) +
            input[[paste0("C", i)]] * cos(2 * pi * i * t / Season)
        )
      }
      return(sum_values)
    })

    data.frame(Time = t_values, Value = sim_values)
  })

  output$plot <- renderPlot({
    plot(sim_data()$Time, sim_data()$Value, type = 'l', xlab = 'Time', ylab = 'Value',
         xlim = c(0, 36), xaxt = 'n', col = "#56B4E9")
    axis(1, at = seq(0, 36, by = 3))
    abline(h = 0, col = "#E69F00")

    # text(18, min(sim_data()$Value), function_string, adj = 0)
    title(xlab = 'Time', ylab = 'Value')
  })

  output$formula <- renderUI({
    req(sim_data())
    function_string <- " = "
    for (i in 1:6) {
      S <- input[[paste0("S", i)]]
      C <- input[[paste0("C", i)]]
      if (S != 0 | C != 0) {
        function_string <- paste0(function_string, ifelse(S != 0, paste0(S,"\\cdot \\sin(2 \\cdot \\pi \\cdot", i, " \\cdot \\frac{t}{12})"), ""),
                                  ifelse(S != 0 & C != 0, " + ", ""), ifelse(C != 0, paste0(C, " \\cdot \\cos(2 \\cdot \\pi \\cdot", i, " \\cdot \\frac{t}{12})"), ""))
      }
    }
    tagList(
      withMathJax(paste0("$",function_string,"$")),
      tags$script('renderMathInElement(document.getElementById("formula"), {delimiters: [{left: "$", right: "$", display: false}]});')
    )
  })
}

shinyApp(ui, server)
