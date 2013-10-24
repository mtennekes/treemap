#' Interactieve versie van treemap
#'
#' A treemap is a space-filling visualization of hierarchical structures. This function offers great flexibility to draw treemaps. Required is a data.frame (\code{dtf}) that contains one or more hierarchical index columns given by \code{index}, a column that determines the rectangle area sizes (\code{vSize}), and optionally a column that determines the rectangle colors (\code{vColor}). The way how rectangles are colored is determined by the argument \code{type}.
#' 
#' @param dtf a data.frame. Required.
#' @param index    vector of column names in \code{dtf} that specify the aggregation indices. It could contain only one column name, which results in a treemap without hierarchy. If multiple column names are provided, the first name is the highest aggregation level, the second name the second-highest aggregation level, and so on. Required. 
#' @param vSize name of the column in \code{dtf} that specifies the sizes of the rectangles. Required.
#' @param vColor name of the column that, in combination with \code{type}, determines the colors of the rectangles. The variable can be scaled by the addition of "*<scale factor>" or "/<scale factor>". 
#' @param type type of the treemap. See \code{\link{treemap}}
#' @import data.table
#' @import grid
#' @import gridBase
#' @import shiny
#' @export
itreemap <- function(dtf, 
                     index, 
                     vSize, 
                     vColor=NULL, 
                     type="index") {
    
    obs <- ls(envir=.GlobalEnv)
    if (!length(obs)) stop("No data.frames loaded")
    dfs <- obs[sapply(obs, function(x)inherits(get(x, envir=.GlobalEnv), "data.frame"))]
    if (!length(dfs)) stop("No data.frames loaded")
    
    dfvars <- lapply(dfs, function(x)names(get(x)))
    names(dfvars) <- dfs
    
    dfiscat <- lapply(dfs, function(x)sapply(get(x),function(y)is.factor(y)||is.logical(y)))
    names(dfiscat) <- dfs
    
    dfcat <- lapply(dfiscat, function(x)names(x[x]))
    dfnum <- lapply(dfiscat, function(x)names(x[!x]))
    
    .tm <<- NULL
    .p <<- ""
    .index <<- ""
    .size <<- ""
    .color <<- ""
    .type <<- ""
    
    runApp(list(
        ui = pageWithSidebar(
            headerPanel("Interactive treemap"),
            sidebarPanel(
                uiOutput("df"),
                uiOutput("index"),
                uiOutput("size"),
                uiOutput("color"),
                uiOutput("type")
            ),
            mainPanel(
                tabsetPanel(
                    tabPanel("Treemap", plotOutput("plot", hoverId="hover", clickId="click"),
                        textOutput("summary")),
                    tabPanel("Data", tableOutput("data")))
            )
        ),
        server = function(input, output){
            dataset <- reactive({
                ifelse(is.null(input$df), dfs[1], input$df)
            })
            
            getData <- reactive({
                x <- input$click$x
                y <- input$click$y
                .tm <- get(".tm", .GlobalEnv)

                index <- input$index
                size <- input$size
                color <- input$color
                #type <- input$type

                colnames <- c(index, size, color) #intersect(, names(p))
                if (!is.null(.tm)) {
                    
                    x <- (x - .tm$vpCoorX[1]) / (.tm$vpCoorX[2] - .tm$vpCoorX[1])
                    y <- (y - .tm$vpCoorY[1]) / (.tm$vpCoorY[2] - .tm$vpCoorY[1])
                    
                    l <- tmLocate(list(x=x, y=y), .tm)
                    
                    p <- get(dataset())
                    
                    m <- as.matrix(p[, index])
                    l2 <- as.vector(as.matrix(l[index]))
                    
                    res <- apply(m, MARGIN=1, function(x)all(x==l2))
                    
                    colnames <- intersect(colnames, names(p))
                    dat <- p[res, colnames]
                    
                    dat <- dat[order(dat[[size]], decreasing=TRUE),]
                    
                    if (is.na(l[1,1])) return(dat[1,])
                    
                    return(dat)
                } else {
                    dat <- as.data.frame(as.list(rep(NA, length(colnames))))
                    names(dat) <- colnames
                    return(dat)
                }
            })

            getSummary <- reactive({
                x <- input$hover$x
                y <- input$hover$y
                .tm <- get(".tm", .GlobalEnv)
                
                index <- input$index
                size <- input$size
                color <- input$color
                #type <- input$type
                
                colnames <- c(index, size) #intersect(, names(p))
                if (!is.null(.tm)) {
                    
                    x <- (x - .tm$vpCoorX[1]) / (.tm$vpCoorX[2] - .tm$vpCoorX[1])
                    y <- (y - .tm$vpCoorY[1]) / (.tm$vpCoorY[2] - .tm$vpCoorY[1])
                    
                    l <- tmLocate(list(x=x, y=y), .tm)
                    
                    ind <- paste(as.vector(as.matrix(l[1, 1:length(index)])), collapse="; ")
                    siz <- paste0(size, "= ", format(l[1, length(index)+1]))
                    
                    if (is.na(l[1,1])) return("")
                    
                    return(paste(ind, siz, sep=": "))
                } else {
                    return("")
                }
            })
            
            
            output$df <- renderUI({
                selectInput("df", label="Dataset:", choices=dfs)
            })
            
            
            output$index <- renderUI({
                p <- dataset()
                vars <- dfcat[[p]]
                checkboxGroupInput("index", label="Index variables", vars, selected = NULL)    
            })
            
            
            
            output$size <- renderUI({
                p <- dataset()
                vars <- dfnum[[p]]
                selectInput("size", label="Size variable", choices=vars)
            })

            output$color <- renderUI({
                p <- dataset()
                vars <- dfvars[[p]]
                selectInput("color", label="Color variable", choices=vars)
            })

            output$type <- renderUI({
                p <- dataset()
                vars <- dfvars[[p]]
                selectInput("type", label="Size variable", choices=c("index", "value", "comp", "dens", 
                                                                     "depth", "categorical", "color"))
            })
            
            
            output$plot <- renderPlot({ 
                p <- dataset()
                index <- input$index
                size <- input$size
                color <- input$color
                type <- input$type
                par(mar=c(0,0,0,0), xaxs='i', yaxs='i') 
                plot(c(0,1), c(0,1),axes=F, col="white")
                vps <- baseViewports()

                if (!is.null(p) && !is.null(index) && !is.null(size) && !is.null(color) && !is.null(type)) {
                    #if (p != .p || index != .index || size != .size || color != .color || type != .type) {
                        
                        cat("draw:", p, index, size, color, type, "\n")
                        
                        .tm <<- treemap(get(p), 
                                index=index,
                                vSize=size, 
                                vColor=color,
                                type=type,
                                vp=vps$plot)
                        .p <<- p
                        .index <<- index
                        .size <<- size
                        .color <<- color
                        .type <<- type
                    #}                    
                }
            })
            output$summary <- renderText({
                getSummary()
            })
            output$data <- renderTable({
                getData()
            })
            
        }
    ))
}
#data(business); itreemap()

