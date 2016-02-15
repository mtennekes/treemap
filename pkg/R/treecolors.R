#' Interactive tool to experiment with Tree Colors
#' 
#' Tree Colors are color palettes for tree data structures. They are used in \code{\link{treemap}} by default (\code{type="index"}). With this tool, users can experiment with the parameters (in \code{\link{treemap}} stored in \code{palette.HCL.options}). Tree Colors can directly be obtained by \code{\link{treepalette}} with \code{method="HCL"}.
#' @param height height of the plotted treemap in pixels. Tip: decrease this number if the treemap doesn't fit conveniently.
#' @examples
#' \dontrun{
#' treecolors()
#' }
#' @import shiny
#' @importFrom ggplot2 ggplot aes_string geom_bar scale_x_continuous scale_y_continuous scale_fill_manual coord_flip theme_bw theme
#' @export  
treecolors <- function(height=700) {
    
    runApp(list(
        ui = pageWithSidebar(
            headerPanel(p(HTML('Tree Colors <span style="color: #999999">color schemes for tree structured data</span>')), windowTitle="Tree Colors"),
            sidebarPanel(
                    p(strong("Random tree data")),
                    sliderInput(inputId = "n",
                                label = "Number of leaf nodes",
                                min = 10, max = 100,
                                step= 1,
                                value = 30),
                    sliderInput(inputId = "d",
                                label = "Tree depth",
                                min = 2, max = 6,
                                step= 1,
                                value = 3),
                    br(),
                    p(strong("Hue")),
                    sliderInput(inputId = "Hstart",
                                label = "Hue start",
                                min = -360, max = 360,
                                step= 10,
                                value = 0),
                    sliderInput(inputId = "Hend",
                                label = "Hue end", #p(HTML('Hue end <span style="color: #999999">(if Hue end < Hue start, then Hue end + 360 is taken)</span>')),
                                min = -360, max = 360,
                                step= 10,
                                value = 360),
                    #helpText("If Hue end < Hue start, then Hue end + 360 is taken."),
                    sliderInput(inputId = "Hf",
                                label = "Hue fraction",
                                min = 0, max = 1,
                                step= 0.05,
                                value = 0.75),
                    checkboxInput("Hperm", "Hue permutations", TRUE),
                    checkboxInput("Hrev", "Hue reverse", TRUE),
                    br(),
                    p(strong("Luminance")),
                    sliderInput(inputId = "L",
                                label = "\nLuminance first level value",
                                min = 0, max = 100,
                                step= 1,
                                value = 70),
                    sliderInput(inputId = "Lslope",
                                label = "Luminance slope value",
                                min = -20, max = 20,
                                step= 1,
                                value = -10),
                    br(),
                    p(strong("Chroma")),
                                sliderInput(inputId = "C",
                                label = "Chroma first level value",
                                min = 0, max = 100,
                                step= 1,
                                value = 60),
                    sliderInput(inputId = "Cslope",
                                label = "Chroma slope value",
                                min = -20, max = 20,
                                step= 1,
                                value = 5)
            ),
            mainPanel(
                tabsetPanel(
                    tabPanel("Graph (Reingold-Tilford)", plotOutput("gplot1", height=paste(height, "px", sep=""))),
                    tabPanel("Graph (Fruchterman-Reingold)", plotOutput("gplot2", height=paste(height, "px", sep=""))),
                    tabPanel("Treemap", plotOutput("tmplot", height=paste(height, "px", sep=""))),
                    tabPanel("Bar chart", plotOutput("barchart", height=paste(height, "px", sep=""))),
                    tabPanel("Data", dataTableOutput("data"))
                    ))),
        server = function(input, output, session){
            data <- reactive({
                dat <- random.hierarchical.data(n=input$n, depth=input$d, value.generator=rnorm, value.generator.args=list(mean=15, sd=3))
                dat$x[dat$x<0] <- 0
                dat
            })
            
            HCL.options <- reactive({
                huestart <- input$Hstart
                hueend <- input$Hend #ifelse(huestart < input$Hend, input$Hend, input$Hend+360)
                list(hue_start=huestart, hue_end=hueend, 
                     hue_perm=input$Hperm, hue_rev=input$Hrev, hue_fraction=input$Hf,
                     chroma=input$C, luminance=input$L, 
                     chroma_slope=input$Cslope, luminance_slope=input$Lslope)
            })
            
            output$gplot1 <- renderPlot({
                dat <- data()
                treegraph(dat, index=names(dat)[1:(ncol(dat)-1)], palette.HCL.options=HCL.options(), vertex.size=6, show.labels=TRUE)
                
            })

            output$gplot2 <- renderPlot({
                dat <- data()
                random.seed <- sample.int((2^31)-1, 1)
                set.seed(1234) # to fix layout
                treegraph(dat, index=names(dat)[1:(ncol(dat)-1)], palette.HCL.options=HCL.options(), vertex.size=6, vertex.layout="fruchterman.reingold", show.labels=TRUE)
                set.seed(random.seed)
                
            })
            
            output$tmplot <- renderPlot({
                dat <- data()
                for (i in 1:(ncol(dat)-2)) levels(dat[[i]]) <- paste("Category", levels(dat[[i]]))
                treemap(dat, index=names(dat)[1:(ncol(dat)-1)], vSize="x", palette.HCL.options=HCL.options(), bg.labels=255, overlap.labels=.1, title="")
                
            })
            
            output$barchart <- renderPlot({
                dat <- data()
                d <- input$d
                
                # reverse levels
                for (i in 1:d) dat[[i]] <- factor(as.character(dat[[i]]), levels=rev(unique(as.character(dat[[i]]))))
                
                datcolors <- treepalette( dat, index=paste("index", 1:d, sep=""),
                                         palette.HCL.options = HCL.options())
                
                dat$color <- datcolors$HCL.color[match(dat[[d]], datcolors[[d]])]
                
                dat$sp <- addSpace(dat[, 1:d])
                dat$sp <- max(dat$sp) - dat$sp
                
                print(ggplot(dat, aes_string(x="sp", y="x", fill=paste("index", d, sep=""))) +
                    geom_bar(stat="identity") + 
                    scale_x_continuous("", breaks=dat$sp, labels=dat[[d]]) +
                    scale_y_continuous("") +
                    scale_fill_manual(values=rev(dat$color)) + coord_flip() + theme_bw() +
                    theme(legend.position="none"))
            })
            
            output$barchart <- renderPlot({
                dat <- data()
                d <- input$d
                
                # reverse levels
                for (i in 1:d) dat[[i]] <- factor(as.character(dat[[i]]), levels=rev(unique(as.character(dat[[i]]))))
                datcolors <- treepalette( dat, index=paste("index", 1:d, sep=""),
                                          palette.HCL.options = HCL.options())
                dat$color <- datcolors$HCL.color[match(dat[[d]], datcolors[[d]])]
                
                dat$sp <- addSpace(dat[, 1:d])
                dat$sp <- max(dat$sp) - dat$sp
                
                print(ggplot(dat, aes_string(x="sp", y="x", fill=paste("index", d, sep=""))) +
                          geom_bar(stat="identity") + 
                          scale_x_continuous("", breaks=dat$sp, labels=dat[[d]]) +
                          scale_y_continuous("") +
                          scale_fill_manual(values=rev(dat$color)) + coord_flip() + theme_bw() +
                          theme(legend.position="none"))
            }) 
            
            output$data <- renderDataTable({
                dat <- data()
                d <- input$d
                treepalette( dat, index=paste("index", 1:d, sep=""),
                                          palette.HCL.options = HCL.options())
                
            })
                
            
            
    }))
}



addSpace <- function(dat, fact=1.10) {
    d <- ncol(dat)
    dat <- lapply(dat, as.integer) 
    diff <- lapply(dat, function(x)x[-1]!=x[-(length(x))])
    diff <- matrix(unlist(diff), ncol=d)
    steps <- floor(log10(apply(diff, MARGIN=1,FUN=function(x)sum(1, as.numeric(x)*(10^(length(x):1))))))
    cumsum(c(0, fact^steps))
}