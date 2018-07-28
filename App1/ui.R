library(shiny)
library(ggplot2)
library(plotly)
data(diamonds)

shinyUI(navbarPage(
    "Data Explorer",
    tabPanel("Data Overview",
             fluidRow(
                 column(3, align = "center",
                        sliderInput("obs", "Observations to Show", 1, 100, 10),
                        selectInput("variable", "Select Variable to Sort By", names(diamonds)),
                        checkboxInput("asc", "Ascending Order"),
                        hr(),
                        strong(textOutput("var_summary_title")),
                        br(),
                        tableOutput("var_summary"),
                        strong(textOutput("plot_var_title")),
                        plotOutput("plot_var", height = "200px")),
                 column(9, align = "center",
                        tableOutput("data_head"),
                        textOutput("data_head_n"))
             )),
    tabPanel("Exploratory Analysis",
             fluidRow(
                 column(4,
                        selectInput("type", "Type of Plot", c("Scatter Plot", "Box Plot")),
                        selectInput("x", "X-Axis Variable", names(diamonds)),
                        selectInput("y", "Y-Axis Variable", names(diamonds)),
                        checkboxInput("smooth", "Show Regression Line"),
                        selectInput("colour", "Colour Variable", c("None", names(diamonds))),
                        selectInput("facet", "Facet Variable", c("None", names(diamonds)))),
                 column(8, align = "center",
                        plotOutput("plot1"))
             ))
))