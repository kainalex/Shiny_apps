#code is inefficient and needs retool. scale is png to reduce load time.

library(shiny)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(readxl)
require(signal)
require(rLakeAnalyzer)
require(data.table)

#global parameters
#Read excel file
X2015Limnology <- read_excel("2015Limnology.xlsx", 
                             sheet = "Calcs", col_types = c("numeric", "numeric", "text", "date", "numeric", 
                                                            "text", "text", "text", "text", "text", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "text", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "blank", "blank", 
                                                            "text", "text", "blank", "numeric", 
                                                            "numeric", "text", "text", "text"))

X2015Limnology$Month <- factor(X2015Limnology$Month, levels=c("May", "Jun", "Jul", "Aug", "Sep", "Oct"))
X2015Limnology$Reach <- factor(X2015Limnology$Reach, levels=c("Lower", "Middle", "Upper", "Sanpoil", "Spokane"))
X2015Limnology <- X2015Limnology[!(X2015Limnology$Month=="May"& X2015Limnology$Location=="SPO-023"),]


ui <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      img(src="http://spokanetribalfisheries.com/wp-content/uploads/2016/05/masthead.jpg", width='200px'), 
      tags$p(),
      h3("Isopleth Generator"),
      tags$p("Spatiotemporally interpolates multiprobe profiles for temperature and dissolved oxygen."),
      sliderInput("limnoyear", "Year", min = min(X2015Limnology$Year), 
                  max = max(X2015Limnology$Year), value = c(max(X2015Limnology$Year)), step=1, sep="", ticks=FALSE
      ),
      uiOutput("locationsub"),
      
      p(em("*Data presented are considered preliminary and may be subject to future revision or qualifiers."))
      
      
    ),
    
    mainPanel(
      h3("Temperature and Dissolved Oxygen Dynamics"),
      p("Temperature profiles are nearly isothermal. Weak stratification patterns occur in the lower reach and Spokane Arm when water residence times are high and flows are reduced."), 
      fluidRow(splitLayout(cellWidths = c("80%", "20%"),
      plotOutput("iso"), 
      #legend
      img(src="http://i.imgur.com/lyafdV8.png")
      

      )
),
p("Temperatures are represented with a color gradient. Dissolved oxygen concentrations are represented with labeled contour lines."),
tags$p("App programmed by", tags$a(href="mailto:alexander.kain@spokanetribe.com", "Alex Kain."))

    )
    
  ))


server <- function(input, output){ 
  
  
  
  output$locationsub <- renderUI({X2015Limnologylist <- subset(X2015Limnology, Year==input$limnoyear)
  selectInput("site", "Location",
              choices=c(
                X2015Limnologylist$Location
              ), selected="")     
  })
  
  output$iso <-renderPlot({
    
    #redefine filled.contour function so that it doesn't include the legend. Legend is currently just a png
    filled.contour3 <-
      function (x = seq(0, 1, length.out = nrow(z)),
                y = seq(0, 1, length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
                ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
                levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
                col = color.palette(length(levels) - 1), plot.title, plot.axes, 
                key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
                axes = TRUE, frame.plot = axes,mar, ...) 
      {
        # modification by Ian Taylor of the filled.contour function
        # to remove the key and facilitate overplotting with contour()
        # further modified by Carey McGilliard and Bridget Ferris
        # to allow multiple plots on one page
        
        if (missing(z)) {
          if (!missing(x)) {
            if (is.list(x)) {
              z <- x$z
              y <- x$y
              x <- x$x
            }
            else {
              z <- x
              x <- seq.int(0, 1, length.out = nrow(z))
            }
          }
          else stop("no 'z' matrix specified")
        }
        else if (is.list(x)) {
          y <- x$y
          x <- x$x
        }
        if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
          stop("increasing 'x' and 'y' values expected")
        # mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
        # on.exit(par(par.orig))
        # w <- (3 + mar.orig[2]) * par("csi") * 2.54
        # par(las = las)
        # mar <- mar.orig
        plot.new()
        # par(mar=mar)
        plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
        if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
          stop("no proper 'z' matrix specified")
        if (!is.double(z)) 
          storage.mode(z) <- "double"
        .filled.contour(as.double(x), as.double(y), z, as.double(levels), 
                        col = col)
        if (missing(plot.axes)) {
          if (axes) {
            title(main = "", xlab = "", ylab = "")
            Axis(x, side = 1)
            Axis(y, side = 2)
          }
        }
        else plot.axes
        if (frame.plot) 
          box()
        if (missing(plot.title)) 
          title(...)
        else plot.title
        invisible()
      }
    
    
    #subset year
    X2015Limnology <- X2015Limnology[ which(X2015Limnology$Location==input$site & X2015Limnology$Year==input$limnoyear),] 
    
    
    #function from TC for cleaning and interpolating water profile data
    heat_mapper <- function(value1,depth1,date,parmname,units,range_z,psize=1,add_below=F){
      depth<-ifelse(is.na(depth1)|depth1=='NA',1,depth1)
      value<-ifelse(is.na(value1)|value1=='NA',NA,value1)
      tmp<-data.frame(value,depth,date)
      tmp<-na.omit(tmp)
      keys <- colnames(tmp)[-1]
      tmp <-as.data.table(tmp)
      tmp<-tmp[,list(AveValue=mean(value)),keys]
      {if(is.numeric(tmp$date)){
        tmp$Date<-as.Date(tmp$date,"1899-12-30")
      }
        else{tmp$Date<-tmp$date}
      }
      z=round(max(depth1))+1
      if(!add_below) {z=z-1}
      profiles<-unique(tmp$Date)
      D<-matrix(1,length(profiles),z+2)
      depth2<-matrix(1,length(profiles),2)
      tmp1<-matrix(1,length(profiles),2)
      xi<-0:z
      for (i in seq(1,length(profiles))) {
        D[i,1]<-profiles[i]
        a<-which(tmp$Date==profiles[i])
        if(length(tmp$AveValue[a])>1) {
          if(depth[a[1]]!=0) {
            depth2<- c(0,tmp$depth[a])
            tmp1<-c(tmp$AveValue[a[1]],tmp$AveValue[a])}
          else {
            depth2<- tmp$depth[a] 
            tmp1<- tmp$AveValue[a]
          }
          yi<-interp1(depth2,tmp1,xi,'linear',extrap=tmp$AveValue[a[length(a)]])
        }
        
        for (jj in seq(1:(z+1))) {
          D[i,jj+1]=yi[jj]
        }
      }
      interp <-data.frame(D)
      colnames(interp)<- c('DateTime',paste("var_",0:z,".0",sep=''))
      
      interp$DateTime<-as.Date(profiles)
      
      #plot
      filled.contour3(
        interp$DateTime,y=0:(ncol(interp)-2),as.matrix(interp[,-1]),ylim=c(ncol(interp)-2,0),
        nlevels=120,color.palette = colorRampPalette(c("violet", 
                                                       "blue", "cyan", "green3", "yellow", "orange", "red"), 
                                                     bias = 1.1, space = "rgb"), ylab = "Depth (m)",
        key.axes=axis(4,cex.axis=psize),zlim=range_z)
      mtext(paste(parmname,units,sep=' '),4,cex=psize)
      
      
      
    }#end function
    
    with(X2015Limnology,heat_mapper(X2015Limnology$`T (oC)`, X2015Limnology$`Depth rounded (m)`, X2015Limnology$Date,"Dissolved Oxygen", "(mg/L)",c(0,30),add_below=T))
    

  

    
    #function
    dissolvedoxygen <- function(value1,depth1,date,parmname,units,range_z,psize=1,add_below=F){
      depth<-ifelse(is.na(depth1)|depth1=='NA',1,depth1)
      value<-ifelse(is.na(value1)|value1=='NA',NA,value1)
      tmp<-data.frame(value,depth,date)
      tmp<-na.omit(tmp)
      keys <- colnames(tmp)[-1]
      tmp <-as.data.table(tmp)
      tmp<-tmp[,list(AveValue=mean(value)),keys]
      {if(is.numeric(tmp$date)){
        tmp$Date<-as.Date(tmp$date,"1899-12-30")
      }
        else{tmp$Date<-tmp$date}
      }
      z=round(max(depth1))+1
      if(!add_below) {z=z-1}
      profiles<-unique(tmp$Date)
      D<-matrix(1,length(profiles),z+2)
      depth2<-matrix(1,length(profiles),2)
      tmp1<-matrix(1,length(profiles),2)
      xi<-0:z
      for (i in seq(1,length(profiles))) {
        D[i,1]<-profiles[i]
        a<-which(tmp$Date==profiles[i])
        if(length(tmp$AveValue[a])>1) {
          if(depth[a[1]]!=0) {
            depth2<- c(0,tmp$depth[a])
            tmp1<-c(tmp$AveValue[a[1]],tmp$AveValue[a])}
          else {
            depth2<- tmp$depth[a] 
            tmp1<- tmp$AveValue[a]
          }
          yi<-interp1(depth2,tmp1,xi,'linear',extrap=tmp$AveValue[a[length(a)]])
        }
        
        for (jj in seq(1:(z+1))) {
          D[i,jj+1]=yi[jj]
        }
      }
      interp <-data.frame(D)
      colnames(interp)<- c('DateTime',paste("var_",0:z,".0",sep=''))
      
      interp$DateTime<-as.Date(profiles)
      
      #plot
      contour(
        interp$DateTime,y=0:(ncol(interp)-2),as.matrix(interp[,-1]),ylim=c(ncol(interp)-2,0),
        nlevels=26, ylab = "Depth (m)",
        labcex=1.2,zlim=range_z,
        add=TRUE)
      mtext(paste(parmname,units,sep=' '),4,cex=psize)
    }
    
    with(X2015Limnology,dissolvedoxygen(X2015Limnology$`D.O. (mg/l)`, X2015Limnology$`Depth rounded (m)`, X2015Limnology$Date,"", "",c(0,13),add_below=T))
  })
}

shinyApp(ui = ui, server = server)
