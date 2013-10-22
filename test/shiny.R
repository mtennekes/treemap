


itreemap <- function() {
    require(shiny)
    require(gridBase)
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
                tableOutput("record")
            )
        ),
        server = function(input, output){
            data(business)
            
            getRecord <- reactive({
                x <- input$hover$x
                y <- input$hover$y
                
                x <- (x - .tm$vpCoorX[1]) / (.tm$vpCoorX[2] - .tm$vpCoorX[1])
                y <- (y - .tm$vpCoorY[1]) / (.tm$vpCoorY[2] - .tm$vpCoorY[1])
                
                
                l <- tmLocate(list(x=x, y=y), .tm)
                l[, 1:(ncol(l)-5)]            
            })
            
            
            output$plot <- renderPlot({ 
                par(mar=c(0,0,0,0), xaxs='i', yaxs='i') 
                plot(c(0,1), c(0,1),axes=F, col="white")
                vps <- baseViewports()
                
                .tm <<- treemap(business, 
                        index=c("NACE1", "NACE2"),
                        vSize=input$size, vp=vps$plot)
                
            })
            output$record <- renderTable({
                getRecord()
            })
        }
    ))
}
itreemap()

