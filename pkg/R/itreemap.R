#' Interactive user interface for treemap
#'
#' This function is an interactive user interface for creating treemaps. Interaction is provided for the four main input arguments of (\code{\link{treemap}}) besides the data.frame itself, namely \code{index}, \code{vSize}, \code{vColor} and \code{type}. Zooming in and out is possible. Command line outputs are generated in the console.
#' 
#' @param dtf a data.frame (\code{\link{treemap}}) If not provided, then the first data.frame in the global workspace is loaded.
#' @param index index variables (up to four). See \code{\link{treemap}}.
#' @param vSize name of the variable that determine the rectangle sizes.
#' @param vColor name of the variable that determine the rectangle colors. See \code{\link{treemap}}.
#' @param type treemap type. See \code{\link{treemap}}.
#' @param height height of the plotted treemap in pixels. Tip: decrease this number if the treemap doesn't fit conveniently.
#' @param command.line.output if \code{TRUE}, the command line output of the generated treemaps are provided in the console.
#' @examples
#' \dontrun{
#' data(business)
#' itreemap(business)
#' }
#' @note This interface will no longer be maintained (except for small bugs), since there is a better interactive interface available: \url{https://github.com/d3treeR/d3treeR}.
#' @import data.table
#' @import grid
#' @import gridBase
#' @import shiny
#' @export
itreemap <- function(dtf=NULL, index=NULL, vSize=NULL, vColor=NULL, type=NULL, height=700, command.line.output=TRUE) {
    # get data.frame(s)
    obs <- ls(envir=.GlobalEnv)
    if (!length(obs)) stop("No data.frames loaded")
    dfs <- obs[sapply(obs, function(x)inherits(get(x, envir=.GlobalEnv), "data.frame"))]
    if (!length(dfs)) stop("No data.frames loaded")
    
    # get variable names
    dfvars <- lapply(dfs, function(x)names(get(x, envir=.GlobalEnv)))
    names(dfvars) <- dfs
    
    dfiscat <- lapply(dfs, function(x)sapply(get(x, envir=.GlobalEnv),function(y)is.factor(y)||is.logical(y)||is.character(y)))
    names(dfiscat) <- dfs
    
    dfcat <- lapply(dfiscat, function(x)if (any(x)) names(x[x]) else "<NA>")
    dfnum <- lapply(dfiscat, function(x)if (any(!x)) names(x[!x]) else "<NA>")
    
    ## check input parameters

    if (missing(dtf)) {
        dtfname <- dfs[1]
    } else {
        dtfname <- deparse(substitute(dtf))
        if (!dtfname %in% dfs) stop(paste(dtfname, "is not a data.frame"))
    }
        
    if (missing(index)) {
        indexNames <- c(dfcat[[dtfname]][1], "<NA>", "<NA>", "<NA>")
    } else {
        if (!(all(index %in% dfcat[[dtfname]]))) stop("index variable(s) is(are) not categorical")
        indexNames <- if (length(index) < 4) c(index, rep.int("<NA>", 4-length(index))) else index[1:4]
    }
    
    if (missing(vSize)) {
        vSize <- dfnum[[dtfname]][1]
    } else {
        if (!(vSize %in% dfnum[[dtfname]])) stop("vSize is not numeric")
    }
    
    if (missing(type)) {
        typeName <- "index"
    } else {
        if (!(type %in% c("value", "categorical", "comp", "dens", "index", "depth"))) stop("Invalid type")
        typeName <- type
    }
    
    

    if (missing(vColor)) {
        if (typeName %in% c("value", "comp", "dens")) vColor <- dfnum[[dtfname]][1]
        if (typeName == "categorical") vColor <- dfcat[[dtfname]][1]
    } else {
        if (typeName %in% c("value", "comp", "dens") && (!(vColor %in% dfnum[[dtfname]]))) stop("vColor is not numeric")
        if (typeName == "categorical" && (!(vColor %in% dfcat[[dtfname]]))) stop("vColor is not categorical")
    }
    if (typeName %in% c("index", "depth")) vColor <- "<not needed>"
    
    
    ## administration is kept in this environment (maybe not the most elegant solution)
    e <- environment()
    
    back <- 0
    #filters <- NULL
    #hcl <- list(tmSetHCLoptions())
    x <- 0
    y <- 0
    count <- 0

    size <- ""
    color <- ""
    type <- ""
    index <- rep("", 4)
    
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
                    tabPanel("Treemap", plotOutput("plot", hover="hover", click="click", height=paste(height, "px", sep="")),
                             tableOutput("summary")),
                    tabPanel("Data", dataTableOutput("data")),
                    tabPanel("Microdata", dataTableOutput("microdata"))))
        ),
        server = function(input, output, session){
            
            values <- reactiveValues()
            values$update <- FALSE
            
            dataset <- reactive({
                assign("filters", NULL, envir=e)
                assign("hcl", list(tmSetHCLoptions()), envir=e)
                assign("asp", NULL, envir=e)
                assign("range", NA, envir=e)    
                assign("tm", NULL, envir=e)
                ifelse(is.null(input$df), dfs[1], input$df)
            })
            
            
            getHoverID <- reactive({
                p <- dataset()
                x <- input$hover$x
                y <- input$hover$y
                
                if (!is.null(tm)) {
                    
                    x <- (x - tm$vpCoorX[1]) / (tm$vpCoorX[2] - tm$vpCoorX[1])
                    y <- (y - tm$vpCoorY[1]) / (tm$vpCoorY[2] - tm$vpCoorY[1])
                    l <- tmLocate(list(x=x, y=y), tm)
                    if (is.na(l[1,1])) {
                        return(NULL)
                    } else return(as.list(l[1,]))
                } else {
                    return(NULL)
                }
            })

            getClickID <- reactive({
                p <- dataset()
                x.new <- input$click$x
                y.new <- input$click$y
                if (is.null(x.new) || is.null(y.new)) return(NULL)
                if (x.new==x && y.new==y) return(NULL)
                assign("x", x.new, envir=e)
                assign("y", y.new, envir=e)
                
                if (!is.null(tm)) {
                    x <- (x - tm$vpCoorX[1]) / (tm$vpCoorX[2] - tm$vpCoorX[1])
                    y <- (y - tm$vpCoorY[1]) / (tm$vpCoorY[2] - tm$vpCoorY[1])
                    
                    l <- tmLocate(list(x=x, y=y), tm)
                    if (is.na(l[1,1])) {
                        return(NULL)
                    } else return(as.list(l[1,]))
                } else {
                    return(NULL)
                }
            })
            
            
            getFilter <- reactive({
                p <- dataset()
                back.new <- input$back
                l <- getClickID()
                if (back.new == back) {
                    if (!is.null(l)) if (!(l$x0==0 && l$y0==0 && l$w==1 && l$y==1))  {
                        # mouse click on treemap
                        filter <- as.character(l[[1]])
                        
                        proceed <- is.null(filters)
                        if (!proceed) proceed <- (!length(filters)) || (filter != filters[length(filters)])
                        
                        # select all rectangles inside clicked rectangle
                        if (proceed) {
                            sel <- tm$tm[[1]] == filter
                            #browser()
                            # create hcl options
                            cols <- tm$tm$color[sel]
                            cols <- substr(cols, 1L, 7L)
                            cols <- hex2RGB(cols)
                            cols <- as(cols, "polarLUV")
                            hues <- cols@coords[,3]
                            hcl.last <- hcl[[length(hcl)]]
                            hcl.last$hue_start <- min(hues)
                            hcl.last$hue_end <- max(hues)
                            notDeeper <- all(is.na(tm$tm[sel, 2]))
                            if (length(l)>10 && !notDeeper) {
                                hcl.last$chroma <- hcl.last$chroma + hcl.last$chroma_slope
                                hcl.last$luminance <- hcl.last$luminance + hcl.last$luminance_slope
                            }
                            
                            
                            assign("hcl", c(hcl, list(hcl.last)), envir=e)
                            
                            # set aspect ratio
                            x0 <- tm$tm$x0[sel]
                            x1 <- x0 + tm$tm$w[sel]
                            y0 <- tm$tm$y0[sel]
                            y1 <- y0 + tm$tm$h[sel]
                            w <- max(x1) - min(x0)
                            h <- max(y1) - min(y0)
                            asp.new <- tm$aspRatio
                            assign("asp", if (is.null(asp)) c(asp.new, asp.new*(w/h)) else c(asp, asp.new*(w/h)), envir=e)
                            
                            # get range
                            assign("range", tm$range, envir=e)
                            
                            # add filter
                            assign("filters", c(filters, filter), envir=e)
                        }
                    }
                } else {
                    if (!is.null(filters)) if (length(filters)) {
                        # click on zoom out button
                        assign("filters", filters[-(length(filters))], envir=e)
                        assign("hcl", hcl[-(length(hcl))], envir=e)
                        assign("asp", asp[-(length(asp))], envir=e)
                        assign("range", tm$range, envir=e)
                    }
                    assign("back", back.new, envir=e)
                }
                filters
            })
            
           getSummary <- reactive({
                l <- getHoverID()
                if (!is.null(l)) {
                    # create summary line on hover                    
                    sizeID <- which(names(l)=="vSize")

                    id <- switch(type,
                                 comp=sizeID+2,
                                 dens=sizeID+2,
                                 value=sizeID+1,
                                 index=sizeID,
                                 categorical=sizeID+1,
                                 depth=sizeID,
                                 color=sizeID)

                    l <- l[1:id]
                    names(l)[sizeID] <- size
                    
                    if (!(type %in% c("index", "depth", "color"))) names(l)[sizeID+1] <- color
                
                    if (type=="comp") {
                        names(l)[sizeID+2] <- paste("compared to", color, "(in %)")
                    } else if (type=="dens") {
                        names(l)[sizeID+2] <- paste(color, "per", size)
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
            
            
            output$ind1 <- renderUI({
                p <- dataset()
                vars <- dfcat[[p]]
                selectInput("ind1", label="Index variables", choices=vars, selected=indexNames[1])
            })
            
            output$ind2 <- renderUI({
                p <- dataset()
                vars <- c("<NA>", dfcat[[p]])
                ind1 <- input$ind1
                if (!is.null(ind1)) { 
                    vars <- setdiff(vars, ind1)
                    selectInput("ind2", label="", choices=vars, selected=indexNames[2])
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
                    selectInput("ind3", label="", choices=vars, selected=indexNames[3])
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
                    selectInput("ind4", label="", choices=vars, selected=indexNames[4])
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
                    }
                    selectInput("color", label="Color variable", choices=vars, selected=vColor)
                }
            })

            output$type <- renderUI({
                selectInput("type", label="Type", choices=c("index", "value", "comp", "dens", 
                                                                     "depth", "categorical"), selected=typeName)
            })
            
            output$plot <- renderPlot({ 
                #.tm <- .range <- .count <- .size <- .color <- .type <- .index <- NULL
                
                # get input parameters
                filters <- getFilter()
                p <- dataset()

                size.new <- input$size
                color.new <- input$color
                type.new <- input$type
                
                ind1 <- input$ind1
                ind2 <- input$ind2
                ind3 <- input$ind3
                ind4 <- input$ind4
                
                asp.new <- input$fixasp
                scales <- input$fixscales

                # check if all parameters are ready
                if (is.null(size.new) || is.null(color.new) || is.null(type.new) || 
                        is.null(ind1) || is.null(ind2) || is.null(ind3) || is.null(ind4) || is.null(asp.new) || is.null(scales)) return(NULL)
                
                # create index vector and get filter
                index.new <- c(ind1, ind2, ind3, ind4)
                
                if (all(index.new==index) && size.new ==size && color.new==color && type.new == type) {
                    #cat("same variables\n")
                    #return(NULL)
                } else {
                    assign("range", NA, envir=e)
                }
                
                assign("size", size.new, envir=e)
                assign("color", color.new, envir=e)
                assign("type", type.new, envir=e)
                assign("index", index.new, envir=e)
                
                index.new <- index.new[index.new!="<NA>"]
                
                # determine zoom level
                zoomLevel <- if (is.null(filters)) 0 else length(filters)
                
                # check parameters                
                if (!(anyDuplicated(index.new)) &&
                    ((color.new=="<not needed>" && (type.new %in% c("index", "depth"))) ||
                    ((color.new %in% dfnum[[p]]) && (type.new %in% c("value", "comp", "dens"))) ||
                    ((color.new %in% dfcat[[p]]) && (type.new == "categorical"))) &&
                    all(index.new %in% dfcat[[p]]))
                          {
                    
                    
                    # create empty base R plot to obtain hover and click info
                    par(mar=c(0,0,0,0), xaxs='i', yaxs='i') 
                    plot(c(0,1), c(0,1),axes=F, col="white")
                    vps <- baseViewports()
                    
                    # subset data and get aspect ratio
                    
                    #### TODO: in incomplete trees, the max zoom level is lower
                    #### test: 53 Postal and courier activities
                    
                    dat <- get(p, envir=.GlobalEnv)
                    if (zoomLevel>0) {
                        filterString <- paste(paste(index.new[1:zoomLevel], paste("\"", filters, "\"", sep=""), sep=" == "), collapse=" & ")
                        selection <- eval(parse(text=filterString), dat, parent.frame())
                        dat <- dat[selection,]
                        
                        # determine indices of treemap
                        allNA <- sapply(dat[, index.new], function(x)all(is.na(x)))
                        maxLevel <- ifelse(any(allNA), which(allNA)[1]-1, length(index.new))
                        minLevel <- min(maxLevel, zoomLevel+1, length(index.new))
                        if (length(index.new)>1) index.new <- index.new[(minLevel:maxLevel)]
                        #if (maxLevel==zoomLevel) hcl
                        aspRatio <- ifelse(asp.new, asp[length(asp)], NA)
                    } else {
                        aspRatio <- NA
                    }
                    
                    # reset range if treemap is changed
                    assign("count", count + 1, envir=e)
                    #cat("draw", .count, " range", .range,"\n")
                    
                    # get range and hcl info
                    assign("range", if(scales) range else NA, envir=e)

                    hcl.new <- if(scales) as.list(hcl[[zoomLevel+1]]) else hcl[[1]]
                    
                    #require(data.table)
                    values$update <- TRUE
                    tm <- treemap(dat, 
                            index=index.new,
                            vSize=size.new, 
                            vColor=color.new,
                            type=type.new,
                            vp=vps$plot,
                            palette.HCL.options=hcl.new,
                            aspRatio=aspRatio,
                            range=range,
                            title="")
                    values$update <- FALSE
                    assign("tm", tm, envir=e)
                    tmString <- 
                        paste0("treemap(", 
                               ifelse(zoomLevel==0, p, paste0("subset(", p, ", subset=", filterString, ")")), 
                               ", index=", if(length(index.new)==1) paste0("\"", index.new, "\"") else paste0("c(", paste(paste0("\"", index.new, "\""), collapse=", "), ")"),
                               ", vSize=\"", size.new, "\"", 
                               if (color.new!="<not needed>") paste0(", vColor=\"", color.new, "\""), 
                               ", type=\"", type.new, "\")")
                    if (command.line.output) cat(tmString, "\n")
                }
                
            })
            
            output$summary <- renderTable({
                getSummary()
            })

            output$microdata <- renderDataTable({
                # get input parameters (to get attention)
                filters <- getFilter()
                p <- dataset()
                size <- input$size
                color <- input$color
                type <- input$type
                
                ind1 <- input$ind1
                ind2 <- input$ind2
                ind3 <- input$ind3
                ind4 <- input$ind4
                
                asp.new <- input$fixasp
                scales <- input$fixscales
                update <- values$update
                
                
                dat <- get(p, envir=.GlobalEnv)
                
                index.new <- c(ind1, ind2, ind3, ind4)
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
                
                size.new <- input$size
                color.new <- input$color
                type.new <- input$type
                
                ind1 <- input$ind1
                ind2 <- input$ind2
                ind3 <- input$ind3
                ind4 <- input$ind4
                
                asp <- input$fixasp
                scales <- input$fixscales
                update <- values$update
                tm <- tm$tm
                
                lvls <- tm$level
                dat <- tm[lvls==max(lvls), 1:(ncol(tm)-6)]

                sizeID <- which(names(dat)=="vSize")
                
                id <- switch(type,
                             comp=sizeID+2,
                             dens=sizeID+2,
                             value=sizeID+1,
                             index=sizeID,
                             categorical=sizeID+1,
                             depth=sizeID,
                             color=sizeID)
                
                
                dat <- dat[, 1:id]
                names(dat)[sizeID] <- size
                
                if (!(type %in% c("index", "depth", "color"))) names(dat)[sizeID+1] <- color
                
                if (type=="comp") {
                    names(dat)[sizeID+2] <- paste("compared to", color, "(in %)")
                } else if (type=="dens") {
                    names(dat)[sizeID+2] <- paste(color, "per", size)
                }                    
                
                dat
            })
            
        }
    ))
}

