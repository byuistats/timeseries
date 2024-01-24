# Install required packages if not already installed
if (!require("shiny")) install.packages("shiny")
if (!require("MASS")) install.packages("MASS")

# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(kableExtra)
library(xtable)

# functions

numeric_2_char_df <- function(df, decimals = 3) {
  out_df <- df |>
    as.data.frame() |>
    mutate_if(is.numeric, round, digits=decimals) |>
    mutate(across(everything(), as.character))
  return(out_df)
}

concat_partial_table <- function(df, nrow_head, nrow_tail, decimals = 3) {
  temp_df <- numeric_2_char_df(df, decimals)

  out_df <- head(temp_df, nrow_head) |>
    bind_rows(row_of_vdots(temp_df)) |>
    bind_rows(tail(temp_df, nrow_tail))

  return(out_df)
}

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] <- round(df[,nums], digits = digits)
  (df)
}

row_of_vdots <- function(df) {
  temp_df <- df |>
    # mutate(across(everything(), as.character)) |>
    head(1)

  for (j in 1:ncol(temp_df)) {
    if (names(temp_df[j]) == "sign") {
      temp_df[1,j] = " "
    } else {
      temp_df[1,j] = "⋮"
    }
  } # for

  return(temp_df)
}

# Define the UI
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
    titlePanel("Covariance & Correlation Exploration"),
    fluidRow(
      column(4, sliderInput("n", "Number of Points", min = 10, max = 100, value = 50, step=5)),
      column(4, sliderInput("mu_x", "Mean (x)", min = -10, max = 10, value = 1.25, step=0.25)),
      column(4, sliderInput("mu_y", "Mean (y)", min = -10, max = 10, value = 2.5, step=0.25)),
      column(4, sliderInput("sigma_x", "Standard Deviation (x)", min = 0, max = 10, value = 1, step =0.05)),
      column(4, sliderInput("sigma_y", "Standard Deviation (y)", min = 0, max = 10, value = 3, step =0.05)),
      column(4, sliderInput("rho", "Correlation Coefficient", min = -1, max = 1, value = 0.8, step =0.05))
    ),
    fluidRow(
      column(4, offset = 5,actionButton("go", "Simulate!")),
    ),
    fluidRow(
      column(12, uiOutput("formula0")),
      column(12, uiOutput("formula0.5")),
      column(12, plotOutput("plot")),
      column(12, uiOutput("formula1")),
      column(12, h3("Table 2: Simulated values and computations involving deviations from the mean")),
      column(12, tableOutput("kable")),
      column(12, uiOutput("formula2")),
      column(12, uiOutput("formula2.5")),
      column(12, uiOutput("formula3")),
      column(12, uiOutput("formula3.5")),
      column(12, uiOutput("formula3.55")),
      column(12, uiOutput("formula4"))
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
      mutate(t = row_number()) %>%
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
        t = "sum",
        sign = case_when(
          xy > 0 ~ "positive",
          xy < 0 ~ "negative",
          TRUE ~ "zero")
      )
    ###########################################
    # I was playing around a bit and this code is now in the output section itself. it works here or there ... just me experimenting in shiny
    ###########################################
    # min_row <- cov_dat %>%
    #   mutate(
    #     positive = if_else(xy > 0, 1, 0),
    #     negative = if_else(xy < 0, 1, 0)
    #   ) %>%
    #   mutate(
    #     pos_sum = cumsum(positive),
    #     neg_sum = cumsum(negative),
    #     both = pos_sum > 0 & neg_sum > 0,
    #     sum_both = cumsum(both)
    #   ) %>%
    #   filter(sum_both <= 1) %>%
    #   nrow()



    # cov_table <- cov_dat |>
    #   numeric_2_char_df() |>
    #   bind_rows(cov_dat_summary) |>
    #   mutate(
    #     xy = cell_spec(xy,
    #                    color = case_when(
    #                      xy > 0 ~ "#56B4E9",
    #                      xy == 0 ~ "black",
    #                      xy < 0 ~ "#E69F00"
    #                    )
    #     ),
    #     sign = cell_spec(sign,
    #                      color = case_when(
    #                        sign == "positive" ~ "#56B4E9",
    #                        sign == "negative" ~ "#E69F00"
    #                      )
    #     )
    #   ) |>
    #   rename(
    #     "x_t" = x,
    #     "y_t" = y,
    #     "$$x_t-mean(x)" = xx,
    #     "(x_t-mean(x))^2" = xx2,
    #     "y_t-mean(y)" = yy,
    #     "(y_t-mean(y))^2" = yy2,
    #     "(x_t-mean(x))(y_t-mean(y))" = xy
    #   ) |>
    #   concat_partial_table(min(25,max(6, min_row)), 6)
    # # move summary to bottom
    #cov_table <- rbind(cov_table[-1, ], cov_table[1, ])

    sim_data <- sim_data
    return(
      list(
        cov_sum <- cov_dat_summary,
        cov_dat <- cov_dat,
        mvm_dat <- mvn_data
      )
    )
  })


  output$formula0 <- renderUI({
    req(sim_data())
    tagList(
      "The simulated values are plotted below, with vertical lines drawn at $x = \\bar x$ and $y = \\bar y$. The first simulated point $(t=1)$ is circled.",
      tags$script('renderMathInElement(document.getElementById("formula0"), {delimiters: [{left: "$", right: "$", display: false}]});')
    )
  })

  output$formula0.5 <- renderUI({
    req(sim_data())
    multiline_text<-"If the quantity $(x-\\bar x)(y-\\bar y)$ is greater than zero, the points are colored blue. Otherwise, they are colored orange. <br> -   What color are the points if $(x-\\bar x)$ and $(y-\\bar y)$ have the same sign? <br> -   What color are the points if $(x-\\bar x)$ and $(y-\\bar y)$ have different signs?"
    tagList(
      # Use HTML function to create HTML content
      HTML(paste("<p>", multiline_text, "</p>")),
      tags$script('renderMathInElement(document.getElementById("formula0.5"), {delimiters: [{left: "$", right: "$", display: false}]});')
    )
  })


  output$formula1 <- renderUI({
    mvn_data <- sim_data()[[3]]
    tagList(
      paste0("The resulting table illustrates some of the simulated values. The mean of the $x_t$ values is $\\bar x$ =", round(mean(mvn_data$x), 3),". The mean of the $y_t$ values is $\\bar y =", round(mean(mvn_data$y), 3),"$. We will soon use the values $(x_t-\\bar x)$, $(x_t-\\bar x)^2$, $(y_t-\\bar y)$, $(y_t-\\bar y)^2$, and $(x_t-\\bar x)(y_t-\\bar y)$ For convenience, they are included in the table below."),
      tags$script('renderMathInElement(document.getElementById("formula1"), {delimiters: [{left: "$", right: "$", display: false}]});')
    )
  })

  output$formula2 <- renderUI({
    req(sim_data())
    tagList(
      "To compute the sample covariance, we divide the sum of the $(x - \\bar x)(y - \\bar y)$ values by $n-1$:",
      tags$script('renderMathInElement(document.getElementById("formula2"), {delimiters: [{left: "$", right: "$", display: false}]});')
    )
  })

  output$formula2.5 <- renderUI({
    cov_dat_summary <- sim_data()[[1]]
    cov_dat <- sim_data()[[2]]
    withMathJax(paste0("$$cov(x,y) =\\frac{\\sum\\limits_{t=1}^n (x - \\bar x)(y - \\bar y)}{n-1} =\\frac{", cov_dat_summary$xy ,"}{n - 1} = ", cov(cov_dat$x, cov_dat$y) |> round(3),"$$"))
  })

  output$formula3 <- renderUI({
    req(sim_data())
    multiline_text<-"  You can think of this as an 'average' of the $(x - \\bar x)(y - \\bar y)$ values. The only difference is that we divide by $n-1$ instead of $n$. <br> -   If there are more blue points than orange points, what should the sign of the sample covariance be? Why? <br> -   What does the sample covariance tell us? <br>   <br> The sample covariance is related to the sample standard deviation of $x$ and $y$ and the sample correlation coefficient between $x$ and $y$. <br> The sample standard deviations are:"
    tagList(
      # Use HTML function to create HTML content
      HTML(paste("<p>", multiline_text, "</p>")),
      tags$script('renderMathInElement(document.getElementById("formula3"), {delimiters: [{left: "$", right: "$", display: false}]});')
    )
  })


  output$formula3.5 <- renderUI({
    cov_dat_summary <- sim_data()[[1]]
    cov_dat <- sim_data()[[2]]
    withMathJax(paste0("$$s_x = \\sqrt{ \\frac{\\sum\\limits_{t=1}^n (x - \\bar x)^2}{n-1} }=\\sqrt{\\frac{", sum((cov_dat$x - mean(cov_dat$x))^2) |> round(3),"}{ n-1}}=", sd(cov_dat$x) |> round(3),"$$"))
  })

  output$formula3.55 <- renderUI({
    cov_dat_summary <- sim_data()[[1]]
    cov_dat <- sim_data()[[2]]
    withMathJax(paste0("$$s_y = \\sqrt{ \\frac{\\sum\\limits_{t=1}^n (y - \\bar y)^2}{n-1} }=\\sqrt{\\frac{", sum((cov_dat$y - mean(cov_dat$y))^2) |> round(3),"}{ n-1}}=", sd(cov_dat$y) |> round(3), "$$"))
  })

  output$formula4 <- renderUI({
    cov_dat_summary <- sim_data()[[1]]
    cov_dat <- sim_data()[[2]]
    withMathJax(paste0("The sample correlation coefficient is: $$r = \\frac{\\sum\\limits_{t=1}^n (x - \\bar x)(y - \\bar y)}{\\sqrt{\\sum\\limits_{t=1}^n (x - \\bar x)^2} \\sqrt{\\sum\\limits_{t=1}^n (y - \\bar y)^2}} =\\frac{", sum((cov_dat$x - mean(cov_dat$x))*(cov_dat$y - mean(cov_dat$y))) |> round(3),"}{\\sqrt{", sum((cov_dat$x - mean(cov_dat$x))^2) |> round(3),"} \\sqrt{", sum((cov_dat$y - mean(cov_dat$y))^2) |> round(3),"} }=", cor(cov_dat$x, cov_dat$y) |> round(3),"$$"))
  })







  output$kable <-function() {
    req(sim_data())
    cov_dat <- sim_data()[[1]]
    cov_dat_summary <- numeric_2_char_df(sim_data()[[2]])

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

    cov_table <- cov_dat |>
      bind_rows(cov_dat_summary) |>
      mutate(
        xy = cell_spec(xy,
                       color = case_when(
                         xy > 0 ~ "#56B4E9",
                         xy == 0 ~ "black",
                         xy < 0 ~ "#E69F00"
                       )
        ),
        sign = cell_spec(sign,
                         color = case_when(
                           sign == "positive" ~ "#56B4E9",
                           sign == "negative" ~ "#E69F00"
                         )
        )
      ) |>
      rename(
        "x_t" = x,
        "y_t" = y,
        "x_t-mean(x)" = xx,
        "(x_t-mean(x))^2" = xx2,
        "y_t-mean(y)" = yy,
        "(y_t-mean(y))^2" = yy2,
        "(x_t-mean(x))(y_t-mean(y))" = xy
      ) |>
      concat_partial_table(min(25,max(6, min_row)), 6)
    # move summary to bottom
    cov_table <- rbind(cov_table[-1, ], cov_table[1, ])
    cov_table <- cov_table[c(8, 1:7, 9:ncol(cov_table))]
    rownames(cov_table) <- NULL

    knitr::kable(cov_table, format = "html", align='cccccccc', escape = FALSE, width = NA) %>%
    kable_styling(full_width = FALSE, "striped")

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
      labs(x=expression(bar(x)), y="y") +
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

