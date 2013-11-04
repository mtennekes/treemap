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
itreemap <- function(dtf=NULL, 
                     index=NULL, 
                     vSize=NULL, 
                     vColor=NULL, 
                     type="index") {
    
    obs <- ls(envir=.GlobalEnv)
    if (!length(obs)) stop("No data.frames loaded")
    dfs <- obs[sapply(obs, function(x)inherits(get(x, envir=.GlobalEnv), "data.frame"))]
    if (!length(dfs)) stop("No data.frames loaded")
    
    dfvars <- lapply(dfs, function(x)names(get(x)))
    names(dfvars) <- dfs
    
    dfiscat <- lapply(dfs, function(x)sapply(get(x),function(y)is.factor(y)||is.logical(y)||is.character(y)))
    names(dfiscat) <- dfs
    
    dfcat <- lapply(dfiscat, function(x)if (any(x)) names(x[x]) else "<NA>")
    dfnum <- lapply(dfiscat, function(x)if (any(!x)) names(x[!x]) else "<NA>")
    
    dfiscolor <- lapply(dfs, function(x)sapply(get(x),function(y)!is.numeric(y)&&(class(try(col2rgb(as.character(y)), silent=TRUE))!="try-error")))
    names(dfiscolor) <- dfs
    
    dfcolor <- lapply(dfiscolor, function(x)if (any(x)) names(x[x]) else "<NA>")
    
    dtfname <- if (!missing(dtf)) deparse(substitute(dtf)) else NULL
    
    
    .tm <<- NULL
    .p <<- ""
    .index <<- ""
    .size <<- ""
    .color <<- ""
    .type <<- ""
    .click <- 0
    .count <- 0
    .back <- 0
    
    runApp(list(
        ui = pageWithSidebar(
            headerPanel("Interactive treemap"),
            sidebarPanel(
                uiOutput("df"),
               uiOutput("filter"),
                actionButton("back", "Zoom out"),
                uiOutput("index"),
                conditionalPanel("output.depth > 1",
                    uiOutput("ind1"),
                    uiOutput("ind2")
                ),
                conditionalPanel("output.depth == 3", uiOutput("ind3")),
                conditionalPanel("output.depth == 4", uiOutput("ind4")),
                conditionalPanel("output.depth > 4", uiOutput("ind5")),
                uiOutput("size"),
                uiOutput("type"),
                uiOutput("color")
            ),
            mainPanel(
                #plotOutput("plot",hoverId="hover",height="700px")
                tabsetPanel(
                    tabPanel("Treemap", plotOutput("plot", hoverId="hover", clickId="click", height="700px"),
                        textOutput("summary")),
                    tabPanel("Data", dataTableOutput("data")))
            )
        ),
        server = function(input, output){
            dataset <- reactive({
                ifelse(is.null(input$df), dfs[1], input$df)
            })
            
            
            getHoverID <- reactive({
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
                    if (is.na(l[1,1])) {
                        return(NULL)
                    } else return(as.list(l[1,]))
                } else {
                    return(NULL)
                }
            })

            getClickID <- reactive({
                x <- input$click$x
                y <- input$click$y
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
                    if (is.na(l[1,1])) {
                        return(NULL)
                    } else return(as.list(l[1,]))
                } else {
                    return(NULL)
                }
            })
            
            
            getFilter <- reactive({
                back <- input$back
                cat("back:", back, "\n")
                
                l <- getClickID()
                if (back == .back) {
    
                    if (!is.null(l)) {
                        .click <<- .click + 1
                        filter <- paste0(names(l)[1], " == \"", l[[1]], "\"")
                        return(filter)
                    } else {
                        return("")
                    }
                } else {
                    .click <<- max(0, .click - 1)
                    .back <<- back
                    return("")
                }
            })
            
            getData <- reactive({
                l <- getClickID()
                
                if (!is.null(l)) {
                    index <- input$index
                    
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
                l <- getHoverID()
                
                if (!is.null(l)) {
                    index <- input$index
                    size <- input$size
                    ind <- paste(unlist(l[1:length(index)]), collapse="; ")
                    siz <- paste0(size, "= ", format(l[[length(index)+1]]))
                    return(paste(ind, siz, sep=": "))
                } else {
                    return("")
                }
            })
            
            output$depth <- reactive({
                index <- input$index
                if (is.null(index)) 0 else length(index)
            })
            outputOptions(output, "depth", suspendWhenHidden=FALSE)
            
            output$df <- renderUI({
                selectInput("df", label="Dataset:", choices=dfs, selected=dtfname)
            })
            
            output$filter <- renderUI({
                textInput("filter", label="Filter: ", value=getFilter())
            })
            
            output$index <- renderUI({
                p <- dataset()
                vars <- dfcat[[p]]
                checkboxGroupInput("index", label="Index variables", vars, selected = NULL)    
            })
            

            output$ind1 <- renderUI({
                index <- input$index
                if (!is.null(index)) selectInput("ind1", label="First index variable", choices=index)
            })
            
            output$ind2 <- renderUI({
                index <- input$index
                ind1 <- input$ind1
                if (!is.null(index) && !is.null(ind1)) { 
                    index <- setdiff(index, ind1)
                    if (length(index)) selectInput("ind2", label="Second index variable", choices=index)
                }
            })
            
            output$ind3 <- renderUI({
                index <- input$index
                ind1 <- input$ind1
                ind2 <- input$ind2
                if (!is.null(index) && !is.null(ind1) && !is.null(ind2)) {
                    index <- setdiff(index, ind1)
                    index <- setdiff(index, ind2)
                    if (length(index)) selectInput("ind3", label="Third index variable", choices=index)
                }
            })
            
            output$ind4 <- renderUI({
                index <- input$index
                ind1 <- input$ind1
                ind2 <- input$ind2
                ind3 <- input$ind3
                if (!is.null(index) && !is.null(ind1) && !is.null(ind2) && !is.null(ind3)) {
                    index <- setdiff(index, ind1)
                    index <- setdiff(index, ind2)
                    index <- setdiff(index, ind3)
                    if (length(index)) selectInput("ind4", label="Fourth index variable", choices=index)
                }
            })
            
            output$ind5 <- renderUI({
                index <- input$index
                ind1 <- input$ind1
                ind2 <- input$ind2
                ind3 <- input$ind3
                ind4 <- input$ind4
                if (!is.null(index) && !is.null(ind1) && !is.null(ind2) && !is.null(ind3) && !is.null(ind4)) {
                    index <- setdiff(index, ind1)
                    index <- setdiff(index, ind2)
                    index <- setdiff(index, ind3)
                    index <- setdiff(index, ind4)
                    if (length(index)) selectInput("ind5", label="Fifth index variable", choices=index)
                }
            })
            
            output$size <- renderUI({
                p <- dataset()
                vars <- dfnum[[p]]
                selectInput("size", label="Size variable", choices=vars, selected=vSize)
            })

            output$color <- renderUI({
                p <- dataset()
                
                type <- input$type
                
                if (!is.null(type)) {
                    vars <- if (type=="index") {
                        "<not needed>"
                    } else if (type=="value") {
                        dfnum[[p]]
                    } else if (type=="comp") {
                        dfnum[[p]]
                    } else if (type=="dens") {
                        dfnum[[p]]
                    } else if (type=="depth") {
                        "<not needed>"
                    } else if (type=="categorical") {
                        dfcat[[p]]
                    } else if (type=="color") {
                        dfcolor[[p]]
                    }
                    selectInput("color", label="Color variable", choices=vars, selected=vColor)
                }
            })

            output$type <- renderUI({
                p <- dataset()
                vars <- dfvars[[p]]
                selectInput("type", label="Type", choices=c("index", "value", "comp", "dens", 
                                                                     "depth", "categorical", "color"))
            })
            
            
            output$plot <- renderPlot({ 

                p <- dataset()
                index <- input$index
                
                filter <- input$filter
                if (is.null(filter)) filter <- ""
                
                size <- input$size
                color <- input$color
                type <- input$type
                
                ind1 <- input$ind1
                ind2 <- input$ind2
                ind3 <- input$ind3
                
                .count <<- .count + 1               
                
                nm <- names(input)
                
                indnames <- paste0("ind", 1:length(index))
                inds <- sapply(indnames, function(x)input[[x]])
                
                
                if (!is.null(index)) if (all(indnames %in% nm)) if (setequal(index, inds)) {
                    #cat("yes\n")
                
#                     filter <- input$filter
#                     if (is.null(filter)) filter <- ""
                    
                    depth <- length(index)
                    if (depth>1) {
                        for (d in 1:depth) {
                            ind <- input[[paste0("ind", d)]]
                            if (!is.null(ind)) index[d] <- ind
                        }
                    }
                    
                    
                    cat("plot: ", p, index, size, color, type, filter, .click, "\n")
                    
                            
                    par(mar=c(0,0,0,0), xaxs='i', yaxs='i') 
                    plot(c(0,1), c(0,1),axes=F, col="white")
                    vps <- baseViewports()
                    
                    
                    if (.click!=0) index <- index[-(1:.click)]
                    
                    cat("draw.\n")
                    
                    dat <- get(p)
                    
                    
                    if (filter!="") {
                        selection <- eval(parse(text=filter), dat, parent.frame())
                        dat <- dat[selection,]
                    }
                    
                    .tm <<- treemap(dat, 
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
                }
            })
            output$summary <- renderText({
               getSummary()
            })
            output$data <- renderDataTable({
               getData()
            })
            
        }
    ))
}
#data(business); itreemap()

