

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
                plotOutput("plot", hoverId="hover"),
                textOutput("caption")
            )
        ),
        server = function(input, output){
            data(business)
            
            tm <- reactive({
                tm <- treemap(business, 
                               index=c("NACE1", "NACE2"),
                               vSize=input$size, vp=vps$plot)
                tm
            })
            
            output$plot <- renderPlot({ 
                plot(1:100)
                vps <- baseViewports()
                
                treemap(business, 
                        index=c("NACE1", "NACE2"),
                        vSize=input$size, vp=vps$plot)
                
            })
            output$caption <- renderText({ 
                tm <- tm()
                paste("Clicked:", input$hover, ",", input$hover$y, tm$vSize)})
        }
    ))
}

data(business)
tab <- treemap(business, 
                                    index=c("NACE1", "NACE2"),
                                    vSize="employees")
locator()



runApp(list(
    ui = bootstrapPage(
        numericInput('range', 'Range', 1.5),
        plotOutput('plot', hoverId="hov"),
        textOutput("caption")
    ),
    server = function(input, output) {
        output$plot <- renderPlot({ boxplot(decrease ~ treatment, data=OrchardSprays, range=input$range) })
        output$caption <- renderText({ paste("Clicked:", input$hov$x, ",", input$hov$y)})
    }
))

require(gridBase)

plot(1:100)
vps <- baseViewports()
treemap(business, 
        index=c("NACE1", "NACE2"),
        vSize="employees", vp=vps$plot)


locator()


par(oma=rep(1, 4), mfrow=c(1, 2), xpd=NA)
plot(1:10)
vps <- baseViewports()
pushViewport(vps$inner)
grid.rect(gp=gpar(lwd=3, col="red"))
pushViewport(vps$figure)
grid.rect(gp=gpar(lwd=3, col="green"))
pushViewport(vps$plot)
grid.rect(gp=gpar(lwd=3, col="blue"))
grid.points(1:10, 10:1)
locator()


