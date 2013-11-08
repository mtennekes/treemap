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
    .count <<- 0
    .back <<- 0
    .filters <<- NULL
    .zoom <<- FALSE
    .asp <<- NULL
    .range <<- NA
    .hcl <<- list(tmSetHCLoptions())
    
    runApp(list(
        ui = pageWithSidebar(
            headerPanel("Interactive treemap"),
            sidebarPanel(
                uiOutput("df"),
               #uiOutput("filter"),
                actionButton("back", "Zoom out"),
                uiOutput("ind1"),
                uiOutput("ind2"),
                uiOutput("ind3"),
                uiOutput("ind4"),
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
                cat("getFilter\n")
                l <- getClickID()
                .range <<- NA
                
                if (back == .back) {
                    if (!is.null(l)) if (!(l$x0==0 && l$y0==0 && l$w==1 && l$y==1))  {
                        filter <- as.character(l[[1]])
                        
                        sel <- .tm$tm[[1]] == filter
                        
                        cols <- .tm$tm$color[sel]
                        cols <- substr(cols, 1L, 7L)
                        cols <- hex2RGB(cols)
                        cols <- as(cols, "polarLUV")
                        hues <- cols@coords[,3]
                        hcl <- .hcl[[length(.hcl)]]
                        hcl$hue_start <- min(hues)
                        hcl$hue_end <- max(hues)
                        if (length(l)>8) {
                            hcl$chroma <- hcl$chroma + hcl$chroma_slope
                            hcl$luminance <- hcl$luminance + hcl$luminance_slope
                        }
                        .hcl <<- c(.hcl, list(hcl))
                        
                        x0 <- .tm$tm$x0[sel]
                        x1 <- x0 + .tm$tm$w[sel]
                        y0 <- .tm$tm$y0[sel]
                        y1 <- y0 + .tm$tm$h[sel]
                        
                        w <- max(x1) - min(x0)
                        h <- max(y1) - min(y0)
                        asp <- .tm$aspRatio
                        
                        .asp <<- if (is.null(.asp)) c(asp, asp*(w/h)) else c(.asp, asp*(w/h))
                        cat("setRange\n")
                        .range <<- .tm$range
                        .zoom <<- TRUE
                        .filters <<- unique(c(.filters, filter))
                    }
                } else {
                    if (!is.null(.filters)) if (length(.filters)) {
                        .filters <<- .filters[-(length(.filters))]
                        .hcl <<- .hcl[-(length(.hcl))]
                        .asp <<- .asp[-(length(.asp))]
                        .range <<- .tm$range
                        .zoom <<- TRUE
                    }
                    .back <<- back
                }
                
                .filters
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
            
            
            output$df <- renderUI({
                selectInput("df", label="Dataset:", choices=dfs, selected=dtfname)
            })
            
#             output$filter <- reactive({
#                 getFilter()
#             })
            
            output$index <- renderUI({
                p <- dataset()
                vars <- dfcat[[p]]
                checkboxGroupInput("index", label="Index variables", vars, selected = NULL)    
            })
            

            output$ind1 <- renderUI({
                p <- dataset()
                vars <- dfcat[[p]]
                selectInput("ind1", label="First index variable", choices=vars)
            })
            
            output$ind2 <- renderUI({
                p <- dataset()
                vars <- c("<NA>", dfcat[[p]])
                ind1 <- input$ind1
                if (!is.null(ind1)) { 
                    vars <- setdiff(vars, ind1)
                    selectInput("ind2", label="Second index variable", choices=vars)
                }
            })
            
            output$ind3 <- renderUI({
                p <- dataset()
                vars <- c("<NA>", dfcat[[p]])
                ind1 <- input$ind1
                ind2 <- input$ind2
                if (!is.null(ind1) && !is.null(ind2)) {
                    if (ind2=="<NA>") {
                        vars <- "<NA>"
                    } else {
                        vars <- setdiff(vars, c(ind1, ind2))
                    }
                    selectInput("ind3", label="Third index variable", choices=vars)
                }
            })
            
            output$ind4 <- renderUI({
                p <- dataset()
                vars <- c("<NA>", dfcat[[p]])
                ind1 <- input$ind1
                ind2 <- input$ind2
                ind3 <- input$ind3
                if (!is.null(ind1) && !is.null(ind2) && !is.null(ind3)) {
                    if (ind3=="<NA>") {
                        vars <- "<NA>"
                    } else {
                        vars <- setdiff(vars, c(ind1, ind2, ind3))
                    }
                    selectInput("ind4", label="Fourth index variable", choices=vars)
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
                #p <- dataset()
                #vars <- dfvars[[p]]
                
                selectInput("type", label="Type", choices=c("index", "value", "comp", "dens", 
                                                                     "depth", "categorical", "color"))
                
            })
            
            
            output$plot <- renderPlot({ 

                p <- dataset()
                
                size <- input$size
                color <- input$color
                type <- input$type

                
                ind1 <- input$ind1
                ind2 <- input$ind2
                ind3 <- input$ind3
                ind4 <- input$ind4
                
                if (is.null(size) || is.null(color) || is.null(type) || 
                        is.null(ind1) || is.null(ind2) || is.null(ind3) || is.null(ind4)) return(NULL)
                        
                        
                
                index <- c(ind1, ind2, ind3, ind4)
                index <- index[index!="<NA>"]
                filters <- getFilter()
                
                zoomLevel <- if (is.null(filters)) 0 else length(filters)

               
                
                .count <<- .count + 1               
                cat("DRAW", .count, "\n")

                cat("size:", size, "\n")
                cat("color:", color, "\n")
                cat("type:", type, "\n")
                cat("index:", index, "\n")
                cat("filters:", filters, "\n")
                cat("hcl:", unlist(.hcl), "\n")
                cat("zoomLevel:", zoomLevel, "\n")
                
                
                
                if (anyDuplicated(index) 
                    || (color=="<not needed>" && !(type %in% c("index", "depth")))
                    || ((color %in% dfnum[[p]]) && !(type %in% c("value", "comp", "dens")))
                    || ((color %in% dfcat[[p]]) && !(color %in% dfcolor[[p]]) && (type != "categorical"))
                    || (!(color %in% dfcat[[p]]) && (color %in% dfcolor[[p]]) && (type != "color"))
                    || ((color %in% dfcat[[p]]) && (color %in% dfcolor[[p]]) && !(type %in% c("categorical", "color")))) {
                    cat("wait...\n")
                } else {
                    cat("go!\n")
                    
                    par(mar=c(0,0,0,0), xaxs='i', yaxs='i') 
                    plot(c(0,1), c(0,1),axes=F, col="white")
                    vps <- baseViewports()
                    
                    dat <- get(p)
                    if (zoomLevel>0) {
                        filterString <- paste(paste(index[1:zoomLevel], paste("\"", filters, "\"", sep=""), sep=" == "), collapse=" & ")
                        selection <- eval(parse(text=filterString), dat, parent.frame())
                        dat <- dat[selection,]
                        index <- index[-(1:min(zoomLevel, length(index)-1))]
                        aspRatio <- .asp[length(.asp)]
                    } else {
                        aspRatio <- NA
                    }
                    
                    if (.zoom) {
                        .zoom <<- FALSE
                    } else {
                        .range <<- NA
                    }
                    
                    cat("range", .range, "\n")
                    
#                     if (filter!="") {
#                         selection <- eval(parse(text=filter), dat, parent.frame())
#                         dat <- dat[selection,]
#                     }
                    hcl <- as.list(.hcl[[zoomLevel+1]])
                    require(data.table)
                    .tm <<- treemap(dat, 
                            index=index,
                            vSize=size, 
                            vColor=color,
                            type=type,
                            vp=vps$plot,
                            palette.HCL.options=hcl,
                            aspRatio=aspRatio,
                            range=.range)
                    
                    
                    
                }
                
                
                
#                 
#                 
#                 nm <- names(input)
#                 
#                 
#                 
#                 
#                 indnames <- paste0("ind", 1:length(index))
#                 inds <- sapply(indnames, function(x)input[[x]])
#                 
#                 cat("back:", input$back, "filter", filter, "\n")
#                 
#                 if (!is.null(index)) if (all(indnames %in% nm)) if (setequal(index, inds)) {
#                     cat("yes\n")
#                     
#                     depth <- length(index)
#                     if (depth>1) {
#                         for (d in 1:depth) {
#                             ind <- input[[paste0("ind", d)]]
#                             if (!is.null(ind)) index[d] <- ind
#                         }
#                     }
#                     
#                     
#                     #cat("plot: ", p, index, size, color, type, filter, "\n")
#                     
#                             
#                     par(mar=c(0,0,0,0), xaxs='i', yaxs='i') 
#                     plot(c(0,1), c(0,1),axes=F, col="white")
#                     vps <- baseViewports()
#                     
#                     
#                     zoomLevel <- if (is.null(.filters)) 0 else length(.filters)
# 
#                     if (zoomLevel>0) index <- index[-(1:min(zoomLevel, length(index)-1))]
#                     hue <- as.list(.hue[[zoomLevel+1]])
#                     
#                     
#                     cat("draw.", names(.hue), unlist(.hue), "\n")
#                     
#                     dat <- get(p)
#                     
#                     
#                     if (filter!="") {
#                         selection <- eval(parse(text=filter), dat, parent.frame())
#                         dat <- dat[selection,]
#                     }
#                     
#                     .tm <<- treemap(dat, 
#                             index=index,
#                             vSize=size, 
#                             vColor=color,
#                             type=type,
#                             vp=vps$plot,
#                             palette.HCL.options=hue)
#                     .p <<- p
#                     .index <<- index
#                     .size <<- size
#                     .color <<- color
#                     .type <<- type
#                 } else {
#                     cat("wait\n")
#                 }
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

