#WQ and Limnology boxplots

#to do; select variables instead of colnum
#enable traceback for warnings:
#options(warn=2)

#packages
library(shiny)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(readxl)



#Limnology data file
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
                 
#setting order for factors
X2015Limnology$Month <- factor(X2015Limnology$Month, levels=c("May", "Jun", "Jul", "Aug", "Sep", "Oct"))
X2015Limnology$Reach <- factor(X2015Limnology$Reach, levels=c("Lower", "Middle", "Upper", "Sanpoil", "Spokane"))
                

#Water Quality data file. Some data that change types are set as text to avoid errors.
library(readxl)
X2015WQ <- read_excel("2015WQ.xlsx", 
                      sheet = "Calcs", col_types = c("numeric", 
                      "text", "numeric", "text", "text", 
                      "text", "text", "date", "numeric", 
                      "numeric", "text", "text", "numeric", 
                      "numeric", "numeric", "text", "numeric", 
                      "text", "text", "numeric", "numeric", 
                      "numeric", "text", "text", "numeric", 
                      "numeric", "numeric", "text", "numeric", 
                      "numeric", "numeric", "numeric", 
                      "text", "text", "numeric", "numeric", 
                      "numeric", "text", "numeric", "text", 
                      "text", "numeric", "numeric", 
                      "numeric", "text", "text", "numeric", 
                      "numeric", "numeric", "numeric", 
                      "text", "numeric", "text", "numeric", 
                      "numeric"))


#setting order for factors
X2015WQ$Month <- factor(X2015WQ$Month, levels=c("May", "Jun", "Jul", "Aug", "Sep", "Oct"))
X2015WQ$Reach <- factor(X2015WQ$Reach, levels=c("Lower", "Middle", "Upper", "Sanpoil", "Spokane ")) 


#User Interface
ui <- fluidPage(
  titlePanel("Lake Roosevelt Water Quality"),
  sidebarLayout(
    sidebarPanel(

    img(src="http://spokanetribalfisheries.com/wp-content/uploads/2016/05/masthead.jpg", width='200px'),      
    h3("Multiprobe Results"),
    p(a(href="https://www.monitoringresources.org/Document/Method/Details/5487", "Method."), "Samples are composites representative of the photic zone collected with discrete samplers."),
      
   
#Y axis options      
selectInput("const", "Constituent",
            choices=c(
            "Temperature (C)", "Dissolved oxygen (percent)","Dissolved oxygen (mg/L)", "Conductivity (uS/cm)", "Turbidity (NTU)", "TDG (mmHg)", "pH", "ORP (mV)", "TDS (mg/L)"
            )),

#Choose between Reach and Month for X-axis       
selectInput("xaxis", "X-axis",
            selected="",
            choices=c("Location","Month","Location~Month"
            )),

#Year
#Consider adding animationOptions when data set becomes larger
sliderInput("limnoyear", "Year", min = min(X2015Limnology$Year), 
            max = max(X2015Limnology$Year), value = c(max(X2015Limnology$Year)), step=1, sep="", ticks=FALSE
            ),

hr(),

h3("Laboratory Results"),
p(a(href="https://www.monitoringresources.org/Document/Method/Details/5486", "Method."), "Physiochemical profiles were collected with a multiparameter datasonde."),

#User Options
    selectInput("nutrient", "Constituent",
                choices=c(
                  "TSS (mg/L)", "Turbidity (NTU)", "NO3 (mg/L)", "NO2 (mg/L)", "TKN (mg/L)", "NH3 (mg/L)", "TN (mg/L)", "TP (mg/L)", "Ortho-P (mg/L)", "Alkalinity (mg/L)", "DIN:DIP", "TN:TP"
                )),
    selectInput("xaxis2", "X-axis",
                choice=c("Location", "Month", "Location~Month")),

    sliderInput("wqyear", "Year", min = min(X2015WQ$Year), 
            max = max(X2015WQ$Year), value = c(max(X2015WQ$Year)), step=1, sep="", ticks=FALSE),

p(em("*Data presented are considered preliminary and may be subject to future revision or qualifiers."))
    ),

    mainPanel(
      tags$p("Spokane Tribal Fisheries Limnologists  monitor water quality conditions annually. These parameters influence the abundance, growth, and survival of resident fish populations."),
      tags$p(tags$a(href='https://www.monitoringresources.org/Document/Protocol/Details/533', "See how the data were collected.")),
      plotOutput("limnologyplot"),
      tags$hr(),
      plotOutput("wqplot"),
      tags$br(),
      p("Sites are selected from a stratified random sample on an annual basis. Current design (2017) includes 15 sites sampled once a month from May to October."),
      tags$p("App programmed by", tags$a(href="mailto:alexander.kain@spokanetribe.com", "Alex Kain."))
      )
     

  )
)


#Server side
server <- function(input, output) {
    
  output$limnologyplot <-renderPlot({
    #x-axis column selection
    if(input$xaxis=='Month') {v <- 3}
    if(input$xaxis=='Location') {v <- 6}
    if(input$xaxis =="Location~Month") {v <- 3}
    #year subset
    X2015Limnology <- subset(X2015Limnology, Year==input$limnoyear) #reactiveyear
    
    Xax <- {X2015Limnology[,v]}

    #y-axis 
    
    if(input$const=='Temperature (C)') {i <- 16}
    if(input$const=='Dissolved oxygen (percent)') {i <- 17}
    if(input$const=='Dissolved oxygen (mg/L)') {i <- 18}
    if(input$const=='Conductivity (uS/cm)') {i <- 19}
    if(input$const=='Turbidity (NTU)') {i <- 20}
    if(input$const=='TDG (mmHg)') {i <- 21}
    if(input$const=='pH') {i <- 22}
    if(input$const=='ORP (mV)') {i <- 24}
    if(input$const=='TDS (mg/L)') {i <-25}
    Yax <- X2015Limnology[,i] 
    
    
    #boxplots 
    if(input$xaxis=="Location") {box2 <-  ggplot(X2015Limnology, aes(Xax, Yax))+
      geom_boxplot(if(input$xaxis=='Location'){aes(fill = factor(Reach))})+
      theme_economist(base_size=11)+
      theme(axis.text.x = element_text(angle = 30, vjust=0.5))+
      labs(fill="Reach")+
      ylab(input$const)+
      xlab(input$xaxis)+
      ggtitle("Multiprobe")+
      scale_x_discrete()+
      theme(plot.title=element_text(size=9))+
      scale_y_continuous()}
    

    
    if(input$xaxis =="Month"){box2 <-  ggplot(X2015Limnology, aes(Xax, Yax))+
      geom_boxplot(if(input$xaxis=='Location'){aes(fill = factor(Reach))})+
      theme_economist(base_size=11)+
      theme(axis.text.x = element_text(angle = 30, vjust=0.5))+
      labs(fill="Reach")+
      ylab(input$const)+
      xlab(input$xaxis)+
      ggtitle("Multiprobe")+
      scale_x_discrete()+
      theme(plot.title=element_text(size=9))+
      scale_y_continuous() }
    
    
    if(input$xaxis =="Location~Month"){box2 <- ggplot(X2015Limnology, aes(Month, Yax, colour=Reach))+
      geom_boxplot()+ggtitle("Locations by Month")+
      facet_wrap("Location")+
      theme_economist(base_size=11)+
      theme(plot.title=element_text(size=9))+
      scale_y_continuous()+
      ylab(input$nutrient)+
      theme(axis.title.x=element_blank())+
      theme(legend.position="none")}  
    
    box2
    
  })

  #Same process for the water quality dataset
  
  output$wqplot <- renderPlot({
    #x axis ifs
    if(input$xaxis2=='Month') {w <- 11}
    if(input$xaxis2=='Location') {w <- 2}
    if(input$xaxis2=='Location~Month') {w <-2}
    X2015WQ <- {subset(X2015WQ, Year==input$wqyear)} #reactiveyear
    
    Xax2 <- X2015WQ[,w]
    
    #yaxis ifs
    if(input$nutrient=="TSS (mg/L)") {o <- 15}
    if(input$nutrient=="Turbidity (NTU)") {o <- 17}
    if(input$nutrient=='NO3 (mg/L)') {o <- 22}
    if(input$nutrient=='NO2 (mg/L)') {o <- 27}
    if(input$nutrient=='TKN (mg/L)') {o <-32}
    if(input$nutrient=='NH3 (mg/L)') {o <- 37}
    if(input$nutrient=='TN (mg/L)') {o <- 39}
    if(input$nutrient=='TP (mg/L)') {o <-44}
    if(input$nutrient=='Ortho-P (mg/L)') {o <- 50}
    if(input$nutrient=='Alkalinity (mg/L)') {o <- 52}
    if(input$nutrient=='DIN:DIP') {o <- 54}
    if(input$nutrient=='TN:TP') {o <-55}
    Yax2 <- X2015WQ[,o]
      
    #plot
    if(input$xaxis2=="Month"){box1<-
    ggplot(X2015WQ, aes(Xax2, Yax2))+
      geom_boxplot(if(input$xaxis2=='Location'){aes(fill = factor(Reach))})+
      theme_economist(base_family='sans', base_size=11)+
      ggtitle("Laboratory")+scale_x_discrete()+scale_y_continuous()+
      ylab(input$nutrient)+xlab(input$xaxis2)+ labs(fill="Reach")+
      theme(plot.title=element_text(size=9))+
      theme(axis.text.x=element_text(angle =30, vjust=0.5))}
    
    if(input$xaxis2=="Location"){
      box1 <- ggplot(X2015WQ, aes(Xax2, Yax2))+
      geom_boxplot(if(input$xaxis2=='Location'){aes(fill = factor(Reach))})+
      theme_economist(base_family='sans', base_size=11)+
      ggtitle("Laboratory")+scale_x_discrete()+scale_y_continuous()+
      ylab(input$nutrient)+xlab(input$xaxis2)+ labs(fill="Reach")+
      theme(plot.title=element_text(size=9))+
      theme(axis.text.x=element_text(angle =30, vjust=0.5))}
    
    if(input$xaxis2 =="Location~Month"){box1 <-  ggplot(X2015WQ, aes(Month, Yax2, colour=Reach))+
      geom_point(size=3)+ggtitle("Locations by Month")+
      facet_wrap("X2015WQ$`Location Name`")+
      theme_economist(base_size=11)+
      theme(plot.title=element_text(size=9))+
      scale_y_continuous()+
      ylab(input$nutrient)+
      theme(axis.title.x=element_blank())+
      theme(legend.position="none")}  
    
    
    box1
  }) 
         
}

shinyApp(ui = ui, server = server)
