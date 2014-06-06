library(shiny)

#library(zipcode)
library(maps)
library(RgoogleMaps)
library(sp)
library(TSP)

#data(zipcode)



# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  LatInput <- reactive({
    input$lat
  })
  
  LngInput <- reactive({
    input$lng
  })
  
  MaxPortals <- reactive({
    input$maxPortals
  })
  
  optroute <- reactive({
    # get data
    setwd("/home/larrydag/Restore_500GB/Documents/R-work/Ingress/")
    x <- read.csv("20130613-Portals - Sheet 1.csv")
    
    # convert data
    x$lat <- sapply(x[,"Lat.Lng"], function(i)   strsplit(as.character(i), ",")[[1]][1])
    x$lng <- sapply(x[,"Lat.Lng"], function(i)   strsplit(as.character(i), ",")[[1]][2])
    x$lat <- as.numeric(x$lat)
    x$lng <- as.numeric(x$lng)
    
    # distance matrix
    loc.mtx <- matrix(c(x$lng,x$lat), ncol=2)
    colnames(loc.mtx) <- c("lng","lat")
    d.mtx <- spDists(loc.mtx, longlat=TRUE)
    
    # center location
    center.lat <- LatInput()
    center.lng <- LngInput()
    
    # calc distances from center
    R <- 6373
    lon1 <- center.lng*pi/180
    lon2 <- x$lng*pi/180
    lat1 <- center.lat*pi/180
    lat2 <- x$lat*pi/180
    
    dlon <- lon2 - lon1 
    dlat <- lat2 - lat1
    a <- (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2 
    c <- 2 * atan2( sqrt(a), sqrt(1-a) ) 
    d <- R * c #(where R is the radius of the Earth)
    
    #  km radius index and limit to 15
    #x[d<2,1:2]
    ind <- which(d<1.0)
    ind <- if(length(ind)>MaxPortals()) ind[1:MaxPortals()] else ind
    
    ## optimal tours 
    tsp <- TSP(d.mtx[ind,ind])
    
    methods <- c("nearest_insertion", "cheapest_insertion", "farthest_insertion", "arbitrary_insertion", "nn", "repetitive_nn", "2-opt")
    
    ## calculate tours
    tours <- lapply(methods, FUN = function(m) solve_TSP(tsp, method = m))
    names(tours) <- methods
    tour_lengths <- sapply(tours, FUN = attr, "tour_length") 
    
    opt <- min(which(tour_lengths==min(tour_lengths)))
    opt.len <- tour_lengths[opt]
    
    opt.ind <- ind[as.integer(tours[[opt]])]
    
    opt.locs <- loc.mtx[opt.ind,]
    
    data.frame(Marker=LETTERS[1:length(opt.ind)], Portals=x[opt.ind,1], lng=opt.locs[,"lng"], lat=opt.locs[,"lat"])
  })
  
  #### output:  portalrouteMap ####
  output$portalrouteMap <- renderPlot({
    
    #### Google map background
    mstr <- sapply(1:nrow(optroute()), function(i) paste0("&markers=color:blue|label:",optroute()$Marker[i],"|",optroute()$lat[i],",",optroute()$lng[i]) )
    markerstr <- paste(mstr, collapse="")
    
    center <- c(LatInput(), LngInput())
    MyMap <- GetMap(center=center, zoom=15, size=c(640,640), markers=markerstr, destfile="maptile.png")
    tmp <- PlotOnStaticMap(MyMap)
    
  })
  
  #### output: portalrouteTbl ####
  output$portalrouteTbl <- renderTable({
    optroute()
  })
})  