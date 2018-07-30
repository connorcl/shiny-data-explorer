library(shiny)
library(ggplot2)
data(diamonds)

shinyUI(navbarPage(
    "Diamonds Analysis",
    tabPanel("Data Overview",
             fluidRow(
                 column(3, align = "center",
                        sliderInput("obs", "Observations to Show", 1, 100, 10),
                        selectInput("variable", "Select Variable to Sort By", names(diamonds)),
                        checkboxInput("asc", "Ascending Order"),
                        hr(),
                        strong(textOutput("var_summary_title")),
                        br(),
                        tableOutput("var_summary")),
                 column(9, align = "left",
                        h4("Diamonds Data"),
                        tableOutput("data_head"),
                        textOutput("data_head_n"),
                        br())
             )),
    tabPanel("Exploratory Graphs",
             fluidRow(
                 column(3,
                        selectInput("type", "Plot Type", c("Scatter Plot", "Box Plot", "Frequency Plot")),
                        hr(),
                        conditionalPanel(
                            condition = "input.type == 'Scatter Plot'",
                            selectInput("x_scatter", "X Axis Variable", names(diamonds)),
                            selectInput("y_scatter", "Y Axis Variable", names(diamonds)),
                            selectInput("color_scatter", "Color Variable", c("None", names(diamonds))),
                            selectInput("facet_scatter", "Faceting Variable", 
                                        c("None", names(diamonds)[!sapply(diamonds[1,], is.numeric)])),
                            sliderInput("alpha_scatter", "Point Opacity", 0.01, 1, 0.5),
                            numericInput("size_scatter", "Point Size", 1, 1, 10, 1),
                            selectInput("smooth_scatter", "Smoothing Line", c("None", "Regression Line", "Smooth Curve"))
                        ),
                        conditionalPanel(
                            condition = "input.type == 'Box Plot'",
                            selectInput("x_box", "X Axis Variable", names(diamonds)),
                            selectInput("y_box", "Y Axis Variable", 
                                        names(diamonds)[sapply(diamonds[1,], is.numeric)]),
                            checkboxInput("rotate_box", "Orientate Plot Horizontally"),
                            radioButtons("dimension3_box", "Show Another Variable Using:", c("None", "Color", "Faceting")),
                            conditionalPanel(
                                condition = "input.dimension3_box == 'Color'",
                                selectInput("color_box", "Color Variable", 
                                            names(diamonds)[!sapply(diamonds[1,], is.numeric)])
                            ),
                            conditionalPanel(
                                condition = "input.dimension3_box == 'Faceting'",
                                selectInput("facet_box", "Faceting Variable",
                                            names(diamonds)[!sapply(diamonds[1,], is.numeric)])
                            )
                        ),
                        conditionalPanel(
                            condition = "input.type == 'Frequency Plot'",
                            radioButtons("freq_subtype", "Plot Subtype", c("Histogram", "Density Plot", "Bar Plot")),
                            conditionalPanel(
                                condition = "input.freq_subtype != 'Bar Plot'",
                                selectInput("freq_x_numeric", "X Axis Variable",
                                            names(diamonds)[sapply(diamonds[1,], is.numeric)])
                            ),
                            conditionalPanel(
                                condition = "input.freq_subtype == 'Bar Plot'",
                                selectInput("freq_x_discrete", "X Axis Variable",
                                            names(diamonds)[!sapply(diamonds[1,], is.numeric)])
                            ),
                            selectInput("freq_color", "Color Variable",
                                        c("None", names(diamonds)[!sapply(diamonds[1,], is.numeric)])),
                            selectInput("freq_facet", "Faceting Variable",
                                        c("None", names(diamonds)[!sapply(diamonds[1,], is.numeric)])),
                            conditionalPanel(
                                condition = "input.freq_subtype == 'Histogram'",
                                sliderInput("freq_bins", "Number of Bins", 2, 100, 10)
                            ),
                            conditionalPanel(
                                condition = "input.freq_subtype != 'Density Plot'",
                                checkboxInput("freq_rotate", "Orientate Plot Horizontally")
                            )
                        )),
                 column(9, align = "center",
                        conditionalPanel(
                            condition = "input.type == 'Scatter Plot'",
                            plotOutput("plot_scatter", height = "500px")
                        ),
                        conditionalPanel(
                            condition = "input.type == 'Box Plot'",
                            plotOutput("plot_box", height = "500px")
                        ),
                        conditionalPanel(
                            condition = "input.type == 'Frequency Plot'",
                            plotOutput("plot_freq", height = "500px")
                        ))
             )),
    tabPanel("Linear Regression",
             fluidRow(
                 column(3,
                        selectInput("depvar", "Select Variable to Predict",
                                    names(diamonds)[sapply(diamonds[1,], is.numeric)]),
                        uiOutput("predictors")),
                 column(9,
                        tabsetPanel(
                            tabPanel("Model Information",
                                     br(),
                                     h4("Adjusted R-Squared"),
                                     textOutput("adj_r_squared_text"),
                                     br(),
                                     h4("Model Coefficients"),
                                     tableOutput("model_coef_table")),
                            tabPanel("Diagnostic Plots",
                                     plotOutput("diag", height = "500px"))
                        ))
             ))
))