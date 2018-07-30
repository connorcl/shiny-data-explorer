library(shiny)
library(ggplot2)
library(Hmisc)
data(diamonds)

shinyServer(function(input, output) {

    output$data_head <- renderTable({
        ord <- order(as.data.frame(diamonds)[[input$variable]])
        if(input$asc == FALSE) {
            ord <- rev(ord)
        }
        diamonds[ord,][1:input$obs,]
    }, striped = T)
    
    output$var_summary <- renderTable({
        as.matrix(summary(diamonds[[input$variable]]))
    }, rownames = T, colnames = F, striped = F)
    
    output$var_summary_title <- renderText({
        paste("Summary of Variable:", input$variable)
    })
    
    output$data_head_n <- renderText({
        paste("First",  as.character(input$obs),  "of", as.character(nrow(diamonds)), "observations")
    })
    
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
            xlab(input$x_scatter) +
            ylab(input$y_scatter) +
            labs(color = input$color_scatter) +
            facet
    })
    
    output$plot_box <- renderPlot({
        data <- data.frame(y = diamonds[[input$y_box]])
        if(class(diamonds[[input$x_box]])[1] == "numeric") {
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
            xlab(input$x_box) +
            ylab(input$y_box) +
            labs(color = input$color_box) +
            facet +
            coord_flip +
            x_axis_labels_flip
    })
    
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
        ggplot(data, aes) + geom + facet + coord_flip + x_axis_labels_flip +
            xlab(xlab) + labs(fill = input$freq_color, color = input$freq_color) + 
            theme(legend.position = "bottom")
    })
    
    output$predictors <- renderUI({
        available_predictors <- setdiff(names(diamonds), input$depvar)
        checkboxGroupInput("select_predictors", "Select Predictors", available_predictors)
    })
    
    model <- reactive({
        ds1 <- data.frame(depvar = diamonds[[input$depvar]])
        if(length(input$select_predictors) > 0) {
            ds1[input$select_predictors] <- diamonds[input$select_predictors]
        }
        lm(depvar ~ ., ds1)
    })
    
    output$adj_r_squared_text <- renderText({
        as.character(round(summary(model())$adj.r.squared, 5))
    })
    
    output$model_coef_table <- renderTable({
        summary(model())$coef
    }, rownames = T)
    
    output$diag <- renderPlot({
        par(mfrow = c(2, 2))
        plot(model(), which = 1:4)
    })
})