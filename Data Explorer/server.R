library(shiny)
library(ggplot2)
library(Hmisc)
data(diamonds)

shinyServer(function(input, output) {
    
    ### Data Overview ###
    
    # Help dialog
    observeEvent(input$overview_help, {
        showModal(modalDialog(
            title = "Help - Data Overview",
            paste("This tab gives an overview of the dataset on which the",
                  "analysis is performed."),
            br(),
            br(),
            paste("On the left, you can select how many observations from", 
                  "the dataset to show (up to 100), which variable to sort the", 
                  "dataset by and whether to sort in ascending or descending",
                  "order. Also on the left is a numeric summary of the",
                  "variable by which the dataset is sorted."),
            br(),
            br(),
            paste("On the right, a number of observations from the dataset are",
                  "shown, according to the selected options. This table is",
                  "updated automatically as the options change.")
        ))
    })
    
    # Title of table summarising variable by which dataset is sorted
    output$overview_var_summary_title <- renderText({
        paste("Summary of Variable:", input$overview_sort_var)
    })
    
    # Table summarising variable by which dataset is sorted
    output$overview_var_summary <- renderTable({
        as.matrix(summary(diamonds[[input$overview_sort_var]]))
    }, rownames = T, colnames = F)

    # Data overview table
    output$overview_data <- renderTable({
        ord <- order(as.data.frame(diamonds)[[input$overview_sort_var]])
        if(!input$overview_sort_asc) {
            ord <- rev(ord)
        }
        diamonds[ord,][1:input$overview_obs,]
    }, striped = T)
    
    # Caption of data overview table stating how many observations are
    # shown and how many are in entire dataset
    output$overview_data_caption <- renderText({
        paste("First", as.character(input$overview_obs), "of", 
              as.character(nrow(diamonds)), "observations")
    })
    
    ### Exploratory Graphs ###
    
    # Help dialog
    observeEvent(input$plot_help, {
        showModal(modalDialog(
            title = "Help - Exploratory Graphs",
            paste("This tab allows you to create various graphs to visually",
                  "explore the data."),
            br(),
            br(),
            paste("On the left, you can choose which type of plot to create.",
                  "Once this is selected, you can set other options, for",
                  "instance which variables to display on the axes, whether to",
                  "show further variables using color or faceting, and whether",
                  "to orientate the plot horizontally."),
            br(),
            br(),
            paste("On the right, a graph is displayed according to the",
                  "selected options. It is updated automatically as the",
                  "options change.")
        ))
    })
    
    # Scatter plot
    output$plot_scatter <- renderPlot({
        data <- data.frame(x = diamonds[[input$x_scatter]], y = diamonds[[input$y_scatter]])
        if(input$color_scatter != "None") {
            data$color <- diamonds[[input$color_scatter]]
            aes <- aes(x = x, y = y, color = color)
        } else {
            aes <- aes(x = x, y = y)
        }
        if(input$facet_scatter != "None") {
            data$facet <- diamonds[[input$facet_scatter]]
            facet <- facet_grid(. ~ facet)
        } else {
            facet <- NULL
        }
        if(input$smooth_scatter == "Regression Line") {
            smooth <- geom_smooth(method = "lm")
        } else if(input$smooth_scatter == "Smooth Curve") {
            smooth <- geom_smooth(method = "auto")
        } else {
            smooth <- NULL
        }
        ggplot(data, aes) + 
            smooth +
            geom_point(size = input$size_scatter, alpha = input$alpha_scatter) +
            theme(legend.position = "bottom") +
            xlab(input$x_scatter) +
            ylab(input$y_scatter) +
            labs(color = input$color_scatter) +
            facet
    })
    
    # Box plot
    output$plot_box <- renderPlot({
        data <- data.frame(y = diamonds[[input$y_box]])
        if(is.numeric(diamonds[[input$x_box]])) {
            data$x <- cut2(diamonds[[input$x_box]], g = 6)
        } else {
            data$x <- diamonds[[input$x_box]]
        }
        if(input$dimension3_box == "Color") {
            data$color <- diamonds[[input$color_box]]
            aes <- aes(x = x, y = y, color = color)
        } else {
            aes <- aes(x = x, y = y)
        }
        x_axis_labels_flip <- NULL
        if(input$dimension3_box == "Faceting") {
            data$facet <- diamonds[[input$facet_box]]
            if(!input$rotate_box) {
                x_axis_labels_flip <- theme(axis.text.x = element_text(angle = 90, hjust = 1))
                facet <- facet_grid(. ~ facet)
            } else {
                facet <- facet_grid(facet ~ .)
            }
        } else {
            facet <- NULL
        }
        if(input$rotate_box) {
            coord_flip <- coord_flip()
        } else {
            coord_flip <- NULL
        }
        ggplot(data, aes) +
            geom_boxplot() +
            theme(legend.position = "bottom") +
            xlab(input$x_box) +
            ylab(input$y_box) +
            labs(color = input$color_box) +
            facet +
            coord_flip +
            x_axis_labels_flip
    })
    
    # Frequency plot
    output$plot_freq <- renderPlot({
        geoms <- c("geom_histogram", "geom_density", "geom_bar")
        names(geoms) <- c("Histogram", "Density Plot", "Bar Plot")
        if(input$freq_subtype != "Bar Plot") {
            data <- data.frame(x = diamonds[[input$freq_x_numeric]])
            xlab <- input$freq_x_numeric
        } else {
            data <- data.frame(x = diamonds[[input$freq_x_discrete]])
            xlab <- input$freq_x_discrete
        }
        if(input$freq_subtype == "Density Plot") {
            alpha = 0.2
        } else {
            alpha = 1
        }
        args <- list(alpha = alpha)
        if(input$freq_color != "None") {
            data$color <- diamonds[[input$freq_color]]
            if(input$freq_subtype == "Density Plot") {
                aes <- aes(x = x, color = color)
            } else {
                aes <- aes(x = x, fill = color)
            }
        } else {
            aes <- aes(x = x)
            if(input$freq_subtype == "Density Plot") {
                args$color = "steelblue"
            } else {
                args$fill = "steelblue"
            }
        }
        if(input$freq_facet != "None") {
            data$facet <- diamonds[[input$freq_facet]]
            if(input$freq_rotate) {
                facet <- facet_grid(facet ~ .)
            } else {
                facet <- facet_grid(. ~ facet)
            }
        } else {
            facet <- NULL
        }
        if(input$freq_subtype == "Histogram") {
            args$bins <- input$freq_bins
        }
        if(input$freq_rotate) {
            coord_flip <- coord_flip()
        } else {
            coord_flip <- NULL
        }
        if(input$freq_subtype == "Bar Plot" & !input$freq_rotate) {
            x_axis_labels_flip <- theme(axis.text.x = element_text(angle = 90, hjust = 1))
        } else {
            x_axis_labels_flip <- NULL
        }
        geom <- do.call(geoms[input$freq_subtype], args)
        ggplot(data, aes) + 
            geom +
            xlab(xlab) +
            theme(legend.position = "bottom") +
            labs(fill = input$freq_color, color = input$freq_color) +
            facet +
            coord_flip +
            x_axis_labels_flip
    })
    
    ### Linear Regression ###
    
    # Help dialog
    observeEvent(input$regression_help, {
        showModal(modalDialog(
            title = "Help - Linear Regression",
            paste("This tab allows you to fit a linear regression model to the",
                  "data."),
            br(),
            br(),
            paste("On the left, you can select which variable to model as",
                  "the outcome, as well as which variables to use as predictors.",
                  "If no predictors are selected, only an intercept term is included."),
            br(),
            br(),
            paste("On the right, there are two tabs. In the 'Model Information'",
                  "tab, the adjusted R-Squared of the model is shown along with",
                  "a table of the model's coefficients and their p-values.",
                  "In the 'Diagnostic Plots' tab, four diagnostic plots are",
                  "shown to aid in assessing potential problems with the",
                  "fitted model.")
        ))
    })
    
    # Checkboxes for selecting predictors
    output$predictors <- renderUI({
        available_predictors <- setdiff(names(diamonds), 
                                        input$regression_dep_var)
        checkboxGroupInput("select_predictors", "Select Predictors", 
                           available_predictors)
    })
    
    # Reactive block which fits a linear model with the predictors
    # and dependent variable selected
    model <- reactive({
        data <- data.frame(depvar = diamonds[[input$regression_dep_var]])
        if(length(input$select_predictors) > 0) {
            data[input$select_predictors] <- diamonds[input$select_predictors]
        }
        lm(depvar ~ ., data)
    })
    
    # Adjsuted R-Squared text
    output$adj_r_squared_text <- renderText({
        as.character(round(summary(model())$adj.r.squared, 5))
    })
    
    # Table of model coefficients
    output$model_coef_table <- renderTable({
        summary(model())$coef
    }, rownames = T)
    
    # Diagnostic plots for fitted model
    output$diagnostic_plots <- renderPlot({
        par(mfrow = c(2, 2))
        plot(model(), which = 1:4)
    })
})