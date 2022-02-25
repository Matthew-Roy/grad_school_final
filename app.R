library(shiny)
library(tidyverse)
library(scales)
library(ggpmisc)

ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Lato&display=swap');
      body {
        background-color: #e3e1e1;
        color: #096330;
      }
      h2 {
        font-family: 'Lato', italics;
      }
      .shiny-input-container {
        color: #096330;
      }"))
  ),
  #CSS in shiny sourced from:https://shiny.rstudio.com/articles/css.html
  
  titlePanel("Seattle Battery Electric Vehicle (BEV) Penetration"),

  sidebarPanel(
    position = "right",
    width = 4,
    selectInput(
      inputId = "x",
      label = "Select X variable",
      choices = c(
        "Median Income",
        "Black Population Share"
      )
    ),
    selectInput(
      inputId = "y",
      label = "Select Y variable",
      choices = c(
        "EVs/1000",
        "Nearest Public Charger (Avg. m)"
      )
    )
  ),
  mainPanel(
    plotOutput("plot")
  )
  # mainPanel and sidebar layout sourced from:
  # https://shiny.rstudio.com/articles/layout-guide.html
)

server <- function(input, output) {
  shiny_data <- read_csv("shiny_data.csv")

  scatter_seattle <- function(x, y, x_lab, y_lab, cap) {
    shiny_data %>%
      filter_at(vars(y), any_vars(!is.na(.))) %>%
      ggplot(
        aes_string(x, y)
      ) +
      geom_point(
        alpha = 0.5,
        color = "seagreen4",
        size = 2
      ) +
      geom_smooth(
        method = "lm",
        se = FALSE,
        linetype = "dotted",
        color = "dodgerblue3"
      ) +
      stat_poly_eq(
        formula = y ~ x,
        aes(label = ..rr.label..),
        parse = TRUE
      ) +
      theme(
        panel.background = element_blank(),
        axis.line = element_line(
          colour = "black"
        )
      ) +
      labs(
        title = str_c(y_lab, "vs", x_lab, 
                      "by Zip Code", sep = " "),
        x = x_lab,
        y = y_lab,
        caption = str_c("Data Sources: U.S Census Bureau, ",
                        cap, sep = "")
      )
  }
# Regression coef label source: https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph
  
  select_x <- reactive({
    if (input$x == "Median Income") {
      x <- "med_income"
    }
    else {
      x <- "blck_share"
    }
  })

  select_y <- reactive({
    if (input$y == "EVs/1000") {
      y <- "ev_per_k"
    }
    else {
      y <- "avg_dist"
    }
  })
  
  define_cap <- reactive({
    if(input$y == "EVs/1000"){
      cap <- "State of Washington"
    }
    else{
      cap <- "U.S. Dept. of Energy"
    }
  })

  build_plot <- reactive({
    scatter_seattle(
      x = select_x(),
      y = select_y(),
      x_lab = input$x,
      y_lab = input$y,
      cap = define_cap()
    )
  })
  
  final_plot <- reactive({
    if (input$x == "Black Population Share") {
      build_plot() +
        scale_x_continuous(
          labels = label_percent(
            accuracy = 1
          )
        )
    }
    else {
      build_plot() +
        scale_x_continuous(
          labels = label_dollar(
            accuracy = 1
          )
        )
    }
  })

  output$plot <- renderPlot(
    final_plot()
  )
}


shinyApp(ui = ui, server = server)