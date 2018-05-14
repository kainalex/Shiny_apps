library(leaflet)
library(shiny)
library(RColorBrewer)
library(htmltools)
library(readxl)

SpatialR <- read_excel("SpatialR.xlsx", 
                        sheet = "Locations", col_types = c("numeric", "numeric", "text", "text", "numeric"))
SpatialR$Reach <- factor(SpatialR$Reach, levels=c("Lower", "Middle", "Upper", "Sanpoil", "Spokane"))


ui <- bootstrapPage(title="LRFEP WQ Sites",
 
  tags$style(type="text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width="100%", height="100%"),
  absolutePanel(bottom = 20, right = 10, draggable=TRUE, fixed=TRUE, 
                sliderInput("spatialyear", "Year", min = min(SpatialR$Year), 
                            max = max(SpatialR$Year), value = c(max(SpatialR$Year)), step=1, sep="", ticks=FALSE), tags$p("App programmed by", tags$a(href="mailto:alexander.kain@spokanetribe.com", "Alex Kain."))
                
                ))



server <- function(input, output, session){ 
  
output$mymap <- renderLeaflet({    
  SpatialR <- subset(SpatialR, Year==input$spatialyear)
  

  pal <- colorFactor(c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0'), SpatialR$Reach)
  
  
  
map <-leaflet(data = SpatialR) %>%
  
  addProviderTiles(providers$Esri.NatGeoWorldMap, group="NatGeoWorldMap")%>%
  addTiles(group = "OSM") %>%
  addProviderTiles(providers$Stamen.Watercolor, group="Watercolor") %>% 
  addProviderTiles(providers$CartoDB.Positron, group="Positron") %>%
  addProviderTiles(providers$Esri.WorldImagery, group="World Imagery")%>% 
  

addCircleMarkers(~long, ~lat,radius = 10,color = ~pal(Reach), stroke = FALSE, 
   fillOpacity = 2, label=SpatialR$`Location Name`, labelOptions = labelOptions(noHide = T,  textsize="9", opacity=0.75)) %>% 
addLegend(position="bottomleft", pal=pal, values=SpatialR$Reach, title="Reach")%>%
  

addLayersControl(
  baseGroups = c("NatGeoWorldMap", "OSM",  "Watercolor", "Positron", "World Imagery"),
  options = layersControlOptions(collapsed = TRUE), position=c("topright")
)

map%>%setView(-118.35, 48.25, zoom=9)%>%
  
  addEasyButton(easyButton(
    icon="fa-crosshairs", title="Locate Me",
    onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%

  addMiniMap(position="topleft")



  })
}

shinyApp(ui, server)

