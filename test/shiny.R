

itableplot()

itableplot <- function() {
    require(shiny)
    runApp(list(
        ui = pageWithSidebar(
            headerPanel("Interactive treemap"),
            sidebarPanel(
                selectInput("size", "Size:",
                            list("turnover" = "turnover", 
                                 "employees" = "employees", 
                                 "turnover.prev" = "turnover.prev",
                                 "employees.prev" = "employees.prev"))
            ),
            mainPanel(
                plotOutput("plot")
            )
        ),
        server = function(input, output){
            data(business)
            output$plot <- renderPlot({ treemap(business, 
                                                index=c("NACE1", "NACE2"),
                                                vSize=input$size)})
        }
    ))
}



runApp(list(
    ui = bootstrapPage(
        numericInput('range', 'Range', 1.5),
        plotOutput('plot', clickId=1),
        textOutput("caption")
    ),
    server = function(input, output) {
        output$plot <- renderPlot({ boxplot(decrease ~ treatment, data=OrchardSprays, range=input$range) })
        output$caption <- renderText({ paste("Clicked:", input$clickId$x, ",", input$clickId$y)})
    }
))


