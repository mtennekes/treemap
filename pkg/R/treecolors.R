treecolors <- function(height=700) {
    
    runApp(list(
        ui = pageWithSidebar(
            headerPanel("Tree Colors: color schemes for tree structures data", windowTitle="Tree Colors"),
            sidebarPanel(
                wellPanel(
                    sliderInput(inputId = "n",
                                label = "Number of nodes",
                                min = 10, max = 100,
                                step= 5,
                                value = 30),
                    sliderInput(inputId = "d",
                                label = "Tree depth",
                                min = 2, max = 6,
                                step= 1,
                                value = 3)),
                wellPanel(
                    sliderInput(inputId = "Hstart",
                                label = "Hue start",
                                min = 0, max = 360,
                                step= 1,
                                value = 0),
                    sliderInput(inputId = "Hend",
                                label = "Hue end",
                                min = 0, max = 360,
                                step= 1,
                                value = 360),
                    sliderInput(inputId = "Hf",
                                label = "Hue fraction",
                                min = 0, max = 1,
                                step= 0.05,
                                value = 0.75),
                    checkboxInput("Hperm", "Hue permutations", TRUE)),
                wellPanel(
                    sliderInput(inputId = "L",
                                label = "\nLuminance first level value",
                                min = 50, max = 100,
                                step= 1,
                                value = 70),
                    sliderInput(inputId = "Lslope",
                                label = "Luminance slope value",
                                min = -20, max = 20,
                                step= 1,
                                value = -10),
                    sliderInput(inputId = "C",
                                label = "Chroma first level value",
                                min = 50, max = 100,
                                step= 1,
                                value = 60),
                    sliderInput(inputId = "Cslope",
                                label = "Chroma slope value",
                                min = -20, max = 20,
                                step= 1,
                                value = 5))
            ),
            mainPanel(
                tabsetPanel(
                    tabPanel("Graph (radial)", plotOutput("gplot1", height=paste(height, "px", sep=""))),
                    tabPanel("Graph (Fruchterman Reingold)", plotOutput("gplot2", height=paste(height, "px", sep=""))),
                    tabPanel("Treemap", plotOutput("tmplot", height=paste(height, "px", sep="")))))),
        server = function(input, output, session){
            data <- reactive({
                random.hierarchical.data(n=input$n, depth=input$d, value.generator=rnorm, value.generator.args=list(mean=10, sd=3))
            })
            
            HCL.options <- reactive({
                list(hue_start=input$Hstart, hue_end=input$Hend, 
                     hue_spread=input$Hperm, hue_fraction=input$Hf,
                     chroma=input$C, luminance=input$L, 
                     chroma_slope=input$Cslope, luminance_slope=input$Lslope)
            })
            
            output$gplot1 <- renderPlot({
                dat <- data()
                treegraph(dat, index=names(dat)[1:(ncol(dat)-1)], palette.HCL.options=HCL.options(), vertex.size=6, show.labels=TRUE)
                
            })

            output$gplot2 <- renderPlot({
                dat <- data()
                set.seed(1234)
                treegraph(dat, index=names(dat)[1:(ncol(dat)-1)], palette.HCL.options=HCL.options(), vertex.size=6, vertex.layout=igraph::layout.fruchterman.reingold, show.labels=TRUE)
                
            })
            
            output$tmplot <- renderPlot({
                dat <- data()
                
                treemap(dat, index=names(dat)[1:(ncol(dat)-1)], vSize="x", palette.HCL.options=HCL.options(), bg.labels=255)
                
            })
            
            
    }))
}