#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(sp)
require(htmltools)

#data
coord<-read.csv("final.csv",sep=";",dec=".",encoding = "UTF-8")
colnames(coord)[1]<-"Country"
coordinates(coord)<- ~long+lat
proj4string(coord)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

COL<-c("blue","green","lightblue","orange","pink","purple",
       "white","cadetblue","darkblue","black","green","lightgray",
       "lightgreen","beige","red","gray","darkred","blue")
getColor <- function(species,col=COL) {
  col[(as.numeric(species))]
}

ICONS <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(coord$species)
) 
getIconsOK<-function(icons_tot,w)
{
  res<-icons_tot
  res$markerColor<-icons_tot$markerColor[w]
  return(res)
}

species<-coord$species
Country<-coord$Country
sample<-coord$sample
host<-coord$host
method<-coord$method
#host<-coord$
listSpecies<-as.list(levels(species))
names(listSpecies)<-levels(species)
listCountry<-as.list(levels(Country))
names(listCountry)<-levels(Country)
listSample<-as.list(levels(sample))
names(listSample)<-levels(sample)
listHost<-as.list(levels(host))
names(listHost)<-levels(host)
listMethod<-as.list(levels(method))
names(listMethod)<-levels(method)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Leishmania"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("Species",
                           h3("Species"),
                           choices = listSpecies,
                           selected = levels(species)
                           ),
        checkboxGroupInput("Country",
                           h3("Country"),
                           choices = listCountry,
                           selected = levels(Country)
        ),
        checkboxGroupInput("Method",
                           h3("Method"),
                           choices = listMethod,
                           selected = levels(method)
        ),
        checkboxGroupInput("Host",
                           h3("Host"),
                           choices = listHost,
                           selected = levels(host)
        ),
        checkboxGroupInput("Sample",
                           h3("Sample"),
                           choices = listSample,
                           selected = levels(sample)
        )
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("mapspecies", height = 800)
      )
   )
)
# Define server logic required to draw a histogram
server <- function(input, output) {

  WHICH<-eventReactive({input$Species
    input$Country
    input$Sample
    input$Host
    input$Method
    }
    ,{which(coord$species%in%input$Species&
              coord$Country%in%input$Country&
              coord$sample%in%input$Sample&
              coord$host%in%input$Host&
              coord$method%in%input$Method
            )})
  IC<-eventReactive({input$Species
    input$Country
    input$Sample
    input$Host
    input$Method
    },{getIconsOK(ICONS,which(coord$species%in%input$Species&
                                            coord$Country%in%input$Country&
                                            coord$sample%in%input$Sample&
                                            coord$host%in%input$Host&
                                            coord$method%in%input$Method
                                          ))})
    output$mapspecies <- renderLeaflet({
     leaflet(coordinates(coord))%>%
       addTiles()%>%
       addAwesomeMarkers(data=coord[WHICH(),],icon=IC()
                         ,label=~paste(method,host)[WHICH()]
                         )%>%
       addLegend("topleft",color=colorNumeric(COL,1:nlevels(species))(1:nlevels(species)),labels = levels(coord$species))
     
      # generate bins based on input$bins from ui.R
     # x    <- faithful[, 2]
     # bins <- seq(min(x), max(x), length.out = input$bins + 1)
     # 
     # # draw the histogram with the specified number of bins
     # hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
  output$selSpecies<-renderText({input$Species})
}

# Run the application 
shinyApp(ui = ui, server = server)

#leaflet(coordinates(coord))%>%addTiles()%>%addAwesomeMarkers(data=coord,icon=ICONS,label=~as.character(coord$species))%>%addLegend("topleft",color=colorNumeric(COL,1:nlevels(species))(1:nlevels(species)),labels = levels(coord$species))

    
  
