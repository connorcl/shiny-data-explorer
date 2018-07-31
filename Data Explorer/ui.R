library(shiny)
library(ggplot2)
data(diamonds)

shinyUI(navbarPage(
    title = "Data Explorer",
    tabPanel(
        title = "Data Overview",
        fluidRow(
            column(
                width = 3,
                h4(actionLink("overview_help", "Help")),
                hr(),
                sliderInput("overview_obs", "Observations to Show", 1, 100, 10),
                selectInput("overview_sort_var",
                            "Select Variable to Sort By",
                            names(diamonds)),
                checkboxInput("overview_sort_asc", "Ascending Order"),
                hr(),
                strong(textOutput("overview_var_summary_title")),
                br(),
                tableOutput("overview_var_summary")),
            column(
                width = 9,
                h4("Dataset: diamonds"),
                tableOutput("overview_data"),
                textOutput("overview_data_caption"),
                br())
        )),
    tabPanel(
        title = "Exploratory Graphs",
        fluidRow(
            column(
                width = 3,
                h4(actionLink("plot_help", "Help")),
                hr(),
                selectInput("plot_type", "Plot Type",
                            c("Scatter Plot", "Box Plot", "Frequency Plot")),
                hr(),
                conditionalPanel(
                    condition = "input.plot_type == 'Scatter Plot'",
                    selectInput("x_scatter", "X Axis Variable", names(diamonds)),
                    selectInput("y_scatter", "Y Axis Variable", names(diamonds)),
                    selectInput("color_scatter", "Color Variable", 
                                c("None", names(diamonds))),
                    selectInput("facet_scatter", "Faceting Variable", 
                                c("None", names(diamonds)[!sapply(diamonds[1,], 
                                                                  is.numeric)])),
                    sliderInput("alpha_scatter", "Point Opacity", 0.01, 1, 1),
                    numericInput("size_scatter", "Point Size", 1, 1, 10, 1),
                    selectInput("smooth_scatter", "Smoothing Line", 
                                c("None", "Regression Line", "Smooth Curve"))
                ),
                conditionalPanel(
                    condition = "input.plot_type == 'Box Plot'",
                    selectInput("x_box", "X Axis Variable", names(diamonds)),
                    selectInput("y_box", "Y Axis Variable", 
                                names(diamonds)[sapply(diamonds[1,], is.numeric)]),
                    checkboxInput("rotate_box", "Orientate Plot Horizontally"),
                    radioButtons("dimension3_box",
                                 "Show Another Variable Using:",
                                 c("None", "Color", "Faceting")),
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
                    condition = "input.plot_type == 'Frequency Plot'",
                    radioButtons("freq_subtype", "Plot Subtype",
                                 c("Histogram","Density Plot", "Bar Plot")),
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
            column(
                width = 9,
                align = "center",
                conditionalPanel(
                    condition = "input.plot_type == 'Scatter Plot'",
                    plotOutput("plot_scatter", height = "500px")
                ),
                conditionalPanel(
                    condition = "input.plot_type == 'Box Plot'",
                    plotOutput("plot_box", height = "500px")
                ),
                conditionalPanel(
                    condition = "input.plot_type == 'Frequency Plot'",
                    plotOutput("plot_freq", height = "500px")
                ))
        )),
    tabPanel(
        title = "Linear Regression",
        fluidRow(
            column(
                width = 3,
                h4(actionLink("regression_help", "Help")),
                hr(),
                selectInput("regression_dep_var", "Select Outcome Variable",
                            names(diamonds)[sapply(diamonds[1,], is.numeric)]),
                uiOutput("predictors")),
            column(
                width = 9,
                tabsetPanel(
                    tabPanel(
                        title = "Model Information",
                        br(),
                        h4("Adjusted R-Squared"),
                        textOutput("adj_r_squared_text"),
                        br(),
                        h4("Model Coefficients"),
                        tableOutput("model_coef_table")),
                    tabPanel(
                        title = "Diagnostic Plots",
                        plotOutput("diagnostic_plots", height = "500px"))
                ))
        ))
))