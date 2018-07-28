library(shiny)
library(ggplot2)
library(plotly)
library(Hmisc)
data(diamonds)

shinyServer(function(input, output) {

    output$data_head <- renderTable({
        ord <- order(as.data.frame(diamonds)[input$variable])
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
    
    output$plot_var <- renderPlot({
        var <- data.frame(x = diamonds[[input$variable]])
        g <- ggplot(var, aes(x = x)) + 
            xlab("") +
            ylab("")
        if(class(var$x)[1] == "numeric") {
            g + geom_histogram(fill = "steelblue", bins = 15)
        } else {
            g + geom_bar(fill = "darkred") + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
        }
    })
    
    output$plot_var_title <- renderText({
        paste("Distribution of Variable:", input$variable)
    })
    
    output$plot1 <- renderPlot({
        data <- data.frame(x = diamonds[[input$x]], y = diamonds[[input$y]], colour = FALSE)
        if(input$type == "Box Plot" & class(data$x) == "numeric") {
            data$x <- cut2(data$x, g = 6)
        }
        if(input$colour == "None") {
            g <- ggplot(data, aes(x, y))
        } else {
            data$z <- diamonds[[input$colour]]
            g <- ggplot(data, aes(x, y, color = z))
        }
        g <- g + xlab(input$x) + ylab(input$y)
        if(input$smooth & input$type == "Scatter Plot") {
            g <- g + geom_smooth(method = "lm", se = TRUE)
        }
        if(input$type == "Scatter Plot") {
            g + geom_point()
        } else if(input$type == "Box Plot") {
            g + geom_boxplot()
        }
    })
})
