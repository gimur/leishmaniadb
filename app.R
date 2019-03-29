library(shiny)
library(leaflet)
library(sp)

#data
coord<-read.csv("final.csv",sep=",",dec=".",encoding="UTF-8")
colnames(coord)[1]<-"department"
coordinates(coord)<- ~long+lat
proj4string(coord)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Definition global functions
COL<-c("blue","green","lightblue","orange","pink","purple","white","black","darkblue","red")
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

# Definition list of choices for the UI
species<-coord$species
department<-coord$department
sample<-coord$sample
host<-coord$host
method<-coord$method
#host<-coord$
listSpecies<-as.list(levels(species))
names(listSpecies)<-levels(species)
listDepartment<-as.list(levels(department))
names(listDepartment)<-levels(department)
listSample<-as.list(levels(sample))
names(listSample)<-levels(sample)
listHost<-as.list(levels(host))
names(listHost)<-levels(host)
listMethod<-as.list(levels(method))
names(listMethod)<-levels(method)
# Define UI 
ui <- fluidPage(
   
   titlePanel("Leishmania"),
   
   sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("Species",
                           h3("Species"),
                           choices = listSpecies,
                           selected = levels(species)
                           ),
        checkboxGroupInput("Department",
                           h3("Department"),
                           choices = listDepartment,
                           selected = levels(department)
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
      
      mainPanel(
         leafletOutput("mapspecies", height = 800)
      )
   )
)
# Define server logic 
server <- function(input, output) {

  WHICH<-eventReactive({input$Species
    input$Department
    input$Sample
    input$Host
    input$Method
    }
    ,{which(coord$species%in%input$Species&
              coord$department%in%input$Department&
              coord$sample%in%input$Sample&
              coord$host%in%input$Host&
              coord$method%in%input$Method
            )})
  IC<-eventReactive({input$Species
    input$Department
    input$Sample
    input$Host
    input$Method
    },{getIconsOK(ICONS,which(coord$species%in%input$Species&
                                            coord$department%in%input$Department&
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
     
   })
  output$selSpecies<-renderText({input$Species})
}

# Run the application 
shinyApp(ui = ui, server = server)
