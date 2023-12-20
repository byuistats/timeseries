# Install required packages if not already installed
if (!require("shiny")) install.packages("shiny")
if (!require("MASS")) install.packages("MASS")

# Load required libraries
library(shiny)
library(MASS)
library(dplyr)
library(ggplot2)
library(kableExtra)

# Define the UI
ui <- fluidPage(
    titlePanel("Bivariate Normal Distribution Simulator"),

    sidebarLayout(
        sidebarPanel(
            actionButton("go", "Simulate!"),
            sliderInput("n", "Number of Points", min = 1, max = 100, value = 50),
            sliderInput("mu_x", "Mean (x)", min = -10, max = 10, value = 1.25),
            sliderInput("mu_y", "Mean (y)", min = -10, max = 10, value = 2.5),
            sliderInput("sigma_x", "Standard Deviation (x)", min = 0.1, max = 10, value = 1, step =0.05),
            sliderInput("sigma_y", "Standard Deviation (y)", min = 0.1, max = 10, value = 3, step =0.05),
            sliderInput("rho", "Correlation Coefficient", min = -1, max = 1, value = 0.8, step =0.05)
        ),

        mainPanel(
          plotOutput("plot"),
          tableOutput("kable")
        )
    )
)



# Define the server logic
server <- function(input, output, session) {
  #Reactive data input
  sim_data <- eventReactive(input$go,{
    set.seed(Sys.time())

    # Specify means and correlation coefficient
    n <- input$n             # number of points
    mu <- c(input$mu_x,input$mu_y)  # mean vector (mu_x, mu_y)
    sigma_x <- input$sigma_x        # standard deviation x
    sigma_y <- input$sigma_y       # standard deviation y
    rho <- input$rho          # correlation coefficient

    # Define variance-covariance matrix
    sigma <- matrix(
      c(sigma_x^2,
        rho*sigma_x*sigma_y,
        rho*sigma_x*sigma_y,
        sigma_y^2),
      nrow = 2)

    # Simulate bivariate normal data
    mvn_data <- MASS::mvrnorm(n, mu, sigma) %>%
      data.frame() %>%
      rename(x = X1, y = X2)



    cov_dat <- mvn_data %>%
      mutate(i = row_number()) %>%
      #select(i, x, y) %>%
      mutate(
        xx = x - mean(x),
        xx2 = xx^2,
        yy = y - mean(y),
        yy2 = yy^2,
        xy = (x - mean(x)) * (y - mean(y)),
        sign = case_when(
          xy > 0 ~ "positive",
          xy < 0 ~ "negative",
          TRUE ~ "zero")
      )

    cov_dat_summary <- cov_dat %>%
      summarize(
        x = sum(x),
        y = sum(y),
        xx = sum(xx),
        xx2 = sum(xx2),
        yy = sum(yy),
        yy2 = sum(yy2),
        xy = sum(xy)
      ) %>%
      round_df(3) %>%
      mutate(across(everything(), as.character)) %>%
      mutate(
        i = "sum",
        sign = case_when(
          xy > 0 ~ "positive",
          xy < 0 ~ "negative",
          TRUE ~ "zero")
      )

    min_row <- cov_dat %>%
      mutate(
        positive = if_else(xy > 0, 1, 0),
        negative = if_else(xy < 0, 1, 0)
      ) %>%
      mutate(
        pos_sum = cumsum(positive),
        neg_sum = cumsum(negative),
        both = pos_sum > 0 & neg_sum > 0,
        sum_both = cumsum(both)
      ) %>%
      filter(sum_both <= 1) %>%
      nrow()

    ellipsis <- data.frame(
      i = ":",
      x = ":",
      y = ":",
      xx = ":",
      xx2 = ":",
      yy = ":",
      yy2 = ":",
      xy = ":",
      sign = " "
    )

    temp1 <- cov_dat %>%
      round_df(3) %>%
      mutate(across(everything(), as.character))

    temp2 <- temp1 %>%
      # include at least 6 and at most 25 rows in the top part;
      # try to include at least one positive and one negative
      # cross product
      head(min(25,max(6, min_row))) %>%
      bind_rows(ellipsis) %>%
      bind_rows(ellipsis) %>%
      bind_rows(temp1 %>% tail(3)) %>%
      bind_rows(cov_dat_summary) %>%
      mutate(
        xy = cell_spec(xy,
                       color = case_when(
                         sign == "positive" ~ "#56B4E9",
                         sign == "negative" ~ "#E69F00",
                         TRUE ~ "Black"))) %>%
      rename(
        "x-mean(x)" = xx,
        "(x-mean(x))^2" = xx2,
        "y-mean(y)" = yy,
        "(y-mean(y))^2" = yy2,
        "(x-mean(x))(y-mean(y))" = xy
      )# %>%
      #select(-sign)

    sim_data <- sim_data
    return(
      list(
        temp2 <- temp2,
        cov_dat <- cov_dat
      )
    )
  })

  output$text <- renderText("on")
  observeEvent(sim_data(),{
    output$text <- renderText("Final_Tables()[[4]])")
  })
  output$table <- renderTable({
    sim_data()[[1]]
  })

  output$kable <-function() {
    req(sim_data())
    knitr::kable(sim_data()[[1]], format = "html", align='cccccccc', escape = FALSE, width = NA) %>%
      kable_styling(full_width = FALSE)
  }

  output$plot<-renderPlot({
    cov_dat <- sim_data()[[2]]
    rho <- input$rho
    n <- input$n
    ggplot(cov_dat, aes(x = x, y = y, color = sign)) +
      geom_point(data=cov_dat %>% filter(row_number() == 1),
                 pch=21,
                 size=4,
                 colour="black") +
      geom_point() +
      scale_color_manual(values = c("#E69F00", "#56B4E9"),
                         labels = c(expression((x-bar(x))(y-bar(y))<0),
                                    expression((x-bar(x))(y-bar(y))>0))) +
      geom_vline(xintercept = mean(cov_dat$x), color = "#009E73") +
      geom_hline(yintercept = mean(cov_dat$y), color = "#009E73") +
      labs(x="x", y="y") +
      theme_bw() +
      ggtitle(paste0("Simulated Data (n = ",n,", ρ = ",rho,")")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      guides(color = guide_legend(title = "Quadrant", reverse = TRUE)) +
      annotate("text", x = mean(cov_dat$x), y = min(cov_dat$y),
               label = expression(bar(x)), hjust = 0, vjust = 0) +
      annotate("text", y = mean(cov_dat$y), x = min(cov_dat$x),
               label = expression(bar(y)), hjust = 0, vjust = 0)
    })


}


# Run the application
shinyApp(ui, server)

