library(shiny)
library(shinyjs)  # Add shinyjs library

# Vocabulary Data with Correct Answers (Key)
vocab_data <- data.frame(
  Term = c(
    "A figure with time on the horizontal axis and the value of a random variable on the vertical axis",
    "A systematic change in a time series that does not appear to be periodic",
    "Repeated pattern within each year (or any other fixed time period)",
    "Repeated pattern that does not correspond to some fixed natural period",
    "Observations in which values are related to lagged observations of the same variable",
    "Random trend that does not follow a discernible or predictable pattern",
    "Can be modeled with mathematical functions, facilitating the long-term prediction of the behavior"
  ),
  Answer = c("Time Plot", "Trend", "Seasonal Variation", "Cycle", "Correlated (Serially Dependent) Data", "Stochastic Trend", "Deterministic Trend")
)

# Vocabulary Choices
vocab_choices <- c(
  "-",
  "Cycle",
  "Correlated (Serially Dependent) Data",
  "Deterministic Trend",
  "Seasonal Variation",
  "Stochastic Trend",
  "Time Plot",
  "Trend"
)

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  titlePanel("Vocabulary Matching"),
  fluidRow(
    column(8, style = "margin-top: 25px;",
           textOutput("text1"),
    ),
    column(3,
           selectInput("term_1", "Term", choices = vocab_choices),
    ),
    column(1, style = "margin-top: 25px;",
           hidden(uiOutput("feedback1")),
    ),
  ),
  fluidRow(
    column(8, style = "margin-top: 25px;",
           textOutput("text2"),
    ),
    column(3,
           selectInput("term_2", "Term", choices = vocab_choices),
    ),
    column(1, style = "margin-top: 25px;",
           hidden(uiOutput("feedback2")),
    ),
  ),
  fluidRow(
    column(8, style = "margin-top: 25px;",
           textOutput("text3"),
    ),
    column(3,
           selectInput("term_3", "Term", choices = vocab_choices),
    ),
    column(1, style = "margin-top: 25px;",
           hidden(uiOutput("feedback3")),
    ),
  ),
  fluidRow(
    column(8, style = "margin-top: 25px;",
           textOutput("text4"),
    ),
    column(3,
           selectInput("term_4", "Term", choices = vocab_choices),
    ),
    column(1, style = "margin-top: 25px;",
           hidden(uiOutput("feedback4")),
    ),
  ),
  fluidRow(
    column(8, style = "margin-top: 25px;",
           textOutput("text5"),
    ),
    column(3,
           selectInput("term_5", "Term", choices = vocab_choices),
    ),
    column(1, style = "margin-top: 25px;",
           hidden(uiOutput("feedback5")),
    ),
  ),
  fluidRow(
    column(8, style = "margin-top: 25px;",
           textOutput("text6"),
    ),
    column(3,
           selectInput("term_6", "Term", choices = vocab_choices),
    ),
    column(1, style = "margin-top: 25px;",
           hidden(uiOutput("feedback6")),
    ),
  ),
  fluidRow(
    column(8, style = "margin-top: 25px;",
           textOutput("text7"),
    ),
    column(3,
           selectInput("term_7", "Term", choices = vocab_choices),
    ),
    column(1, style = "margin-top: 25px;",
           hidden(uiOutput("feedback7")),
    ),
  ),
  actionButton("showAnswers", "Show/Hide Answers"),
)

server <- function(input, output) {

  output$text1 <- renderText(vocab_data$Term[1])
  output$text2 <- renderText(vocab_data$Term[2])
  output$text3 <- renderText(vocab_data$Term[3])
  output$text4 <- renderText(vocab_data$Term[4])
  output$text5 <- renderText(vocab_data$Term[5])
  output$text6 <- renderText(vocab_data$Term[6])
  output$text7 <- renderText(vocab_data$Term[7])

  output$feedback1 <- renderUI({
    if (input$term_1 == vocab_data$Answer[1]) {
      icon("check", class = "text-success")  # Green checkmark
    } else {
      icon("times", class = "text-danger")   # Red X
    }
  })

  output$feedback2 <- renderUI({
    if (input$term_2 == vocab_data$Answer[2]) {
      icon("check", class = "text-success")  # Green checkmark
    } else {
      icon("times", class = "text-danger")   # Red X
    }
  })

  output$feedback3 <- renderUI({
    if (input$term_3 == vocab_data$Answer[3]) {
      icon("check", class = "text-success")  # Green checkmark
    } else {
      icon("times", class = "text-danger")   # Red X
    }
  })

  output$feedback4 <- renderUI({
    if (input$term_4 == vocab_data$Answer[4]) {
      icon("check", class = "text-success")  # Green checkmark
    } else {
      icon("times", class = "text-danger")   # Red X
    }
  })

  output$feedback5 <- renderUI({
    if (input$term_5 == vocab_data$Answer[5]) {
      icon("check", class = "text-success")  # Green checkmark
    } else {
      icon("times", class = "text-danger")   # Red X
    }
  })

  output$feedback6 <- renderUI({
    if (input$term_6 == vocab_data$Answer[6]) {
      icon("check", class = "text-success")  # Green checkmark
    } else {
      icon("times", class = "text-danger")   # Red X
    }
  })

  output$feedback7 <- renderUI({
    if (input$term_7 == vocab_data$Answer[7]) {
      icon("check", class = "text-success")  # Green checkmark
    } else {
      icon("times", class = "text-danger")   # Red X
    }
  })

  # feedback <- reactive({
  #    for (i in 1:nrow(vocab_data)) {
  #      term <- paste0("term_", i)
  #      feedback_id <- paste0("feedback_", i)
  #      icon <- if (input[[term]] == vocab_data$Answer[i]) {
  #        icon("check", class = "text-success")  # Green checkmark
  #      } else {
  #        icon("times", class = "text-danger")   # Red X
  #      }
  #      html(feedback_id, icon)
  #      shinyjs::show(feedback_id)
  #    }
  # })
  button <- reactiveVal("show")

  observeEvent(input$showAnswers, {
    if (button() == "show") {
      show("feedback1")
      show("feedback2")
      show("feedback3")
      show("feedback4")
      show("feedback5")
      show("feedback6")
      show("feedback7")
      button("hide")
    } else {
      hide("feedback1")
      hide("feedback2")
      hide("feedback3")
      hide("feedback4")
      hide("feedback5")
      hide("feedback6")
      hide("feedback7")
      button("show")
    }

  #   for (i in 1:nrow(vocab_data)) {
  #     term <- paste0("term_", i)
  #     feedback_id <- paste0("feedback_", i)
  #     icon <- if (input[[term]] == vocab_data$Answer[i]) {
  #       icon("check", class = "text-success")  # Green checkmark
  #     } else {
  #       icon("times", class = "text-danger")   # Red X
  #     }
  #     html(feedback_id, icon)
  #     shinyjs::show(feedback_id)
  #  }
   })
}

shinyApp(ui = ui, server = server)


