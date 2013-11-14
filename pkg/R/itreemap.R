#' Interactieve versie van treemap
#'
#' A treemap is a space-filling visualization of hierarchical structures. This function offers great flexibility to draw treemaps. Required is a data.frame (\code{dtf}) that contains one or more hierarchical index columns given by \code{index}, a column that determines the rectangle area sizes (\code{vSize}), and optionally a column that determines the rectangle colors (\code{vColor}). The way how rectangles are colored is determined by the argument \code{type}.
#' 
#' @param dtf a data.frame (\code{\link{treemap}) If not provided, then all data.frames in the global workspace are given as options in the drop-down list, with the first one loaded.
#' @param height height of the plotted treemap in pixels
#' @import data.table
#' @import grid
#' @import gridBase
#' @import shiny
#' @export
itreemap <- function(dtf=NULL, height=NULL) {
    
    # get data.frame(s)
    obs <- ls(envir=.GlobalEnv)
    if (!length(obs)) stop("No data.frames loaded")
    if (missing(dtf)) {
        dfs <- obs[sapply(obs, function(x)inherits(get(x, envir=.GlobalEnv), "data.frame"))]
        if (!length(dfs)) stop("No data.frames loaded")
    } else {
        dfs <- deparse(substitute(dtf))
        if (!dfs %in% obs) stop(paste(dfs, "is not a data.frame"))
    }
    
    # get variable names
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
    
    # global parameters to be used in the shiny app: not the most elegant solution, but it works
    .tm <<- NULL
    .back <<- 0
    .filters <<- NULL
    .asp <<- NULL
    .range <<- NA
    .hcl <<- list(tmSetHCLoptions())
    .x <<- 0
    .y <<- 0
    .count <<- 0

    .size <<- ""
    .color <<- ""
    .type <<- ""
    .index <<- rep("", 4)
    
    # set plotheight: quick and dirty method
    if (missing(height)) {
        if (Sys.info()[['sysname']]=="Windows") {
            (scr_height <- system("wmic desktopmonitor get screenheight", intern=TRUE))
            scr_height <- as.numeric(scr_height[2])
            height <- scr_height - 375
        } else {
            height <- 800
        }
    }
    
    runApp(list(
        ui = pageWithSidebar(
            headerPanel("", windowTitle="Interactive Treemap"),
            sidebarPanel(
                uiOutput("df"),
               #uiOutput("filter"),
                uiOutput("ind1"),
                uiOutput("ind2"),
                uiOutput("ind3"),
                uiOutput("ind4"),
                uiOutput("size"),
                uiOutput("type"),
                uiOutput("color"),
                checkboxInput("fixscales", "Fix scales", value = TRUE),
                checkboxInput("fixasp", "Fix aspect ratio", value = TRUE),
                actionButton("back", "Zoom out")
            ),
            mainPanel(
                tabsetPanel(
                    tabPanel("Treemap", plotOutput("plot", hoverId="hover", clickId="click", height=paste0(height, "px")),
                             tableOutput("summary")),
                    tabPanel("Data", dataTableOutput("data")),
                    tabPanel("Microdata", dataTableOutput("microdata"))))
        ),
        server = function(input, output, session){
            
            values <- reactiveValues()
            values$update <- FALSE
            
            dataset <- reactive({
                ifelse(is.null(input$df), dfs[1], input$df)
            })
            
            
            getHoverID <- reactive({
                x <- input$hover$x
                y <- input$hover$y
                .tm <- get(".tm", .GlobalEnv)
                
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
                
                if (is.null(x) || is.null(y)) return(NULL)
                if (x==.x && y==.y) return(NULL)
                .x <<- x
                .y <<- y
                
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
                l <- getClickID()
                #.range <<- NA
                
                if (back == .back) {
                    if (!is.null(l)) if (!(l$x0==0 && l$y0==0 && l$w==1 && l$y==1))  {
                        # mouse click on treemap
                        filter <- as.character(l[[1]])
                        
                        # select all rectangles inside clicked rectangle
                        sel <- .tm$tm[[1]] == filter
                        
                        # create hcl options
                        cols <- .tm$tm$color[sel]
                        cols <- substr(cols, 1L, 7L)
                        cols <- hex2RGB(cols)
                        cols <- as(cols, "polarLUV")
                        hues <- cols@coords[,3]
                        hcl <- .hcl[[length(.hcl)]]
                        hcl$hue_start <- min(hues)
                        hcl$hue_end <- max(hues)
                        if (length(l)>10) {
                            hcl$chroma <- hcl$chroma + hcl$chroma_slope
                            hcl$luminance <- hcl$luminance + hcl$luminance_slope
                        }
                        .hcl <<- c(.hcl, list(hcl))
                        
                        # set aspect ratio
                        x0 <- .tm$tm$x0[sel]
                        x1 <- x0 + .tm$tm$w[sel]
                        y0 <- .tm$tm$y0[sel]
                        y1 <- y0 + .tm$tm$h[sel]
                        w <- max(x1) - min(x0)
                        h <- max(y1) - min(y0)
                        asp <- .tm$aspRatio
                        .asp <<- if (is.null(.asp)) c(asp, asp*(w/h)) else c(.asp, asp*(w/h))
                        
                        # get range
                        .range <<- .tm$range
                        
                        # add filter
                        .filters <<- unique(c(.filters, filter))
                    }
                } else {
                    if (!is.null(.filters)) if (length(.filters)) {
                        # click on zoom out button
                        .filters <<- .filters[-(length(.filters))]
                        .hcl <<- .hcl[-(length(.hcl))]
                        .asp <<- .asp[-(length(.asp))]
                        .range <<- .tm$range
                    }
                    .back <<- back
                }
                .filters
            })
            
           getSummary <- reactive({
                l <- getHoverID()
                if (!is.null(l)) {
                    # create summary line on hover                    
                    sizeID <- which(names(l)=="vSize")

                    id <- switch(.type,
                                 comp=sizeID+2,
                                 dens=sizeID+2,
                                 value=sizeID+1,
                                 index=sizeID,
                                 categorical=sizeID+1,
                                 depth=sizeID,
                                 color=sizeID)

                    l <- l[1:id]
                    names(l)[sizeID] <- .size
                    
                    if (!(.type %in% c("index", "depth", "color"))) names(l)[sizeID+1] <- .color
                
                    if (.type=="comp") {
                        names(l)[sizeID+2] <- paste("compared to", .color, "(in %)")
                    } else if (.type=="dens") {
                        names(l)[sizeID+2] <- paste(.color, "per", .size)
                    }                    
                    dt <- as.data.frame(l)
                    row.names(dt) <- ""
                    return(as.data.frame(dt))
                } else {
                    dt <- data.frame('...'="")
                    row.names(dt) <- ""
                    return(dt)
                }
            })
            
            output$df <- renderUI({
                selectInput("df", label="Dataset:", choices=dfs, selected=dtfname)
            })
            
            
            output$index <- renderUI({
                p <- dataset()
                vars <- dfcat[[p]]
                checkboxGroupInput("index", label="Index variables", vars, selected = NULL)    
            })
            

            output$ind1 <- renderUI({
                p <- dataset()
                vars <- dfcat[[p]]
                selectInput("ind1", label="Index variables", choices=vars)
            })
            
            output$ind2 <- renderUI({
                p <- dataset()
                vars <- c("<NA>", dfcat[[p]])
                ind1 <- input$ind1
                if (!is.null(ind1)) { 
                    vars <- setdiff(vars, ind1)
                    selectInput("ind2", label="", choices=vars)
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
                    selectInput("ind3", label="", choices=vars)
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
                    selectInput("ind4", label="", choices=vars)
                }
            })
            
            output$size <- renderUI({
                p <- dataset()
                vars <- dfnum[[p]]
                selectInput("size", label="Size variable", choices=vars)
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
                    selectInput("color", label="Color variable", choices=vars)
                }
            })

            output$type <- renderUI({
                selectInput("type", label="Type", choices=c("index", "value", "comp", "dens", 
                                                                     "depth", "categorical", "color"))
            })
            
            output$plot <- renderPlot({ 

                # get input parameters
                p <- dataset()

                size <- input$size
                color <- input$color
                type <- input$type
                
                ind1 <- input$ind1
                ind2 <- input$ind2
                ind3 <- input$ind3
                ind4 <- input$ind4
                
                asp <- input$fixasp
                scales <- input$fixscales

                # check if all parameters are ready
                if (is.null(size) || is.null(color) || is.null(type) || 
                        is.null(ind1) || is.null(ind2) || is.null(ind3) || is.null(ind4) || is.null(asp) || is.null(scales)) return(NULL)
                
                # create index vector and get filter
                index <- c(ind1, ind2, ind3, ind4)
                
                filters <- getFilter()
                if (all(index==.index) && size ==.size && color==.color && type == .type) {
                    #cat("same variables\n")
                    #return(NULL)
                } else {
                    .range <<- NA
                }
                
                .size <<- size
                .color <<- color
                .type <<- type
                .index <<- index
                
                index <- index[index!="<NA>"]
                
                # determine zoom level
                zoomLevel <- if (is.null(filters)) 0 else length(filters)
                
                # check parameters                
                if (!(anyDuplicated(index) 
                    || (color=="<not needed>" && !(type %in% c("index", "depth")))
                    || ((color %in% dfnum[[p]]) && !(type %in% c("value", "comp", "dens")))
                    || ((color %in% dfcat[[p]]) && !(color %in% dfcolor[[p]]) && (type != "categorical"))
                    || (!(color %in% dfcat[[p]]) && (color %in% dfcolor[[p]]) && (type != "color"))
                    || ((color %in% dfcat[[p]]) && (color %in% dfcolor[[p]]) && !(type %in% c("categorical", "color"))))) {
                    
                    
                    # create empty base R plot to obtain hover and click info
                    par(mar=c(0,0,0,0), xaxs='i', yaxs='i') 
                    plot(c(0,1), c(0,1),axes=F, col="white")
                    vps <- baseViewports()
                    
                    # subset data and get aspect ratio
                    dat <- get(p)
                    if (zoomLevel>0) {
                        filterString <- paste(paste(index[1:zoomLevel], paste("\"", filters, "\"", sep=""), sep=" == "), collapse=" & ")
                        selection <- eval(parse(text=filterString), dat, parent.frame())
                        dat <- dat[selection,]
                        if (length(index)>1) index <- index[-(1:min(zoomLevel, length(index)-1))]
                        aspRatio <- ifelse(asp, .asp[length(.asp)], NA)
                    } else {
                        aspRatio <- NA
                    }
                    
                    # reset range if treemap is changed
                    .count <<- .count + 1
                    #cat("draw", .count, " range", .range,"\n")
                    
                    # get range and hcl info
                    .range <<- if(scales) .range else NA

                    
                    hcl <- if(scales) as.list(.hcl[[zoomLevel+1]]) else .hcl[[1]]
                    
                    #require(data.table)
                    values$update <- TRUE
                    .tm <<- treemap(dat, 
                            index=index,
                            vSize=size, 
                            vColor=color,
                            type=type,
                            vp=vps$plot,
                            palette.HCL.options=hcl,
                            aspRatio=aspRatio,
                            range=.range,
                            title="")
                    values$update <- FALSE
                }
                
            })
            
            output$summary <- renderTable({
                getSummary()
            })

            output$microdata <- renderDataTable({
                # get input parameters (to get attention)
                p <- dataset()
                size <- input$size
                color <- input$color
                type <- input$type
                
                ind1 <- input$ind1
                ind2 <- input$ind2
                ind3 <- input$ind3
                ind4 <- input$ind4
                
                asp <- input$fixasp
                scales <- input$fixscales
                update <- values$update
                
                
                dat <- get(p)
                
                index <- c(ind1, ind2, ind3, ind4)
                filters <- getFilter()
                zoomLevel <- if (is.null(filters)) 0 else length(filters)
                if (zoomLevel>0) {
                    # subset data
                    filterString <- paste(paste(index[1:zoomLevel], paste("\"", filters, "\"", sep=""), sep=" == "), collapse=" & ")
                    selection <- eval(parse(text=filterString), dat, parent.frame())
                    dat <- dat[selection,]
                }
                dat
            })
            
            output$data <- renderDataTable({
                # get input parameters (to get attention)
                p <- dataset()
                
                size <- input$size
                color <- input$color
                type <- input$type
                
                ind1 <- input$ind1
                ind2 <- input$ind2
                ind3 <- input$ind3
                ind4 <- input$ind4
                
                asp <- input$fixasp
                scales <- input$fixscales
                update <- values$update
                tm <- .tm$tm
                
                lvls <- tm$level
                dat <- tm[lvls==max(lvls), 1:(ncol(tm)-6)]

                sizeID <- which(names(dat)=="vSize")
                
                id <- switch(.type,
                             comp=sizeID+2,
                             dens=sizeID+2,
                             value=sizeID+1,
                             index=sizeID,
                             categorical=sizeID+1,
                             depth=sizeID,
                             color=sizeID)
                
                
                dat <- dat[, 1:id]
                names(dat)[sizeID] <- .size
                
                if (!(.type %in% c("index", "depth", "color"))) names(dat)[sizeID+1] <- .color
                
                if (.type=="comp") {
                    names(dat)[sizeID+2] <- paste("compared to", .color, "(in %)")
                } else if (.type=="dens") {
                    names(dat)[sizeID+2] <- paste(.color, "per", .size)
                }                    
                
                dat
            })
            
        }
    ))
}

