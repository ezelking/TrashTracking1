library(dplyr)
library(shiny)

ui <- fluidPage(
  
  ## Page Title
  titlePanel("Trash Corridors"),
  
  sidebarLayout(      
    
    sidebarPanel(
      
      ## Options that effect all diagrams
      helpText("Options for all diagrams:"),
      sliderInput(inputId = "Zoom",
                    label = strong("Zoom"),
                    value = 0,
                  min = 0,
                  max = 100),
      sliderInput(inputId = "lat",
                  label = strong("Latitude"),
                  value = 52,
                  min = 50,
                  max = 54,
                  step = 0.01
      ),
      
      sliderInput(inputId = "lng",
                  label = strong("Longtitude"),
                  value = 5.5,
                  min = 3,
                  max = 8,
                  step = 0.01
      ),
      
      checkboxInput(inputId = "SchoolCorridors",
                    label = "Check for corridors between schools?",
                    value = FALSE),
      selectInput(inputId = "School1",
                  label = "Select First School",
                  choices = schools[,"VESTIGINGSNAAM"]
                  ),
      selectInput(inputId = "School2",
                  label = "Select Second School",
                  choices = schools[,"VESTIGINGSNAAM"]
      ),
      hr()
      
     
    ),
    
    mainPanel(
      
      plotOutput("corridor")
    )
    
  )
)

server <- function(input, output, session) {
  
  ########## Diagram 1 ##########
  output$corridor <- renderPlot({
    plot( trashData$latitude ~ trashData$longitude, ylab="latitude"
          , ylim=c((input$lat +0.1 - ((100 - input$Zoom) *0.02)), (input$lat-0.1 + ((100 - input$Zoom) *0.02)))
          , xlab="longitude", xlim=c((input$lng +0.1 - ((100 - input$Zoom) *0.025)), (input$lng-0.1 + ((100 - input$Zoom) *0.025)))
          , main="trash data", col="red" )
    
    points(schoolCoordinates
           , col="blue")
    
    if (input$SchoolCorridors){
       ## Adds a line between the chosen locations
      segments(schoolCoordinates[which(schools[,"VESTIGINGSNAAM"] == input$School1),"Longtitude"], schoolCoordinates[which(schools[,"VESTIGINGSNAAM"] == input$School1),"Latitude"], schoolCoordinates[which(schools[,"VESTIGINGSNAAM"] == input$School2),"Longtitude"], schoolCoordinates[which(schools[,"VESTIGINGSNAAM"] == input$School2),"Latitude"])
      #lines(schoolCoordinates[which(schools[,"VESTIGINGSNAAM"] == input$School1)],schoolCoordinates[which(schools[,"VESTIGINGSNAAM"] == input$School2)])
      
      # schoolCoordinates[which(trashData[,"longitude"] < schoolCoordinates[which(schools[, "VESTIGINGSNAAM"]== "Openbare Basisschool 't Eenspan"), "Longtitude"] ), "Longtitude"]
      
      # points(#trashData$longitude[schoolCoordinates[which(schools[,"VESTIGINGSNAAM"] == input$School1),"Longtitude"]< trashData$longitude < schoolCoordinates[which(schools[,"VESTIGINGSNAAM"] == input$School2),"Longtitude"]]
      #        #,trashData$latitude[schoolCoordinates[which(schools[,"VESTIGINGSNAAM"] == input$School1),"Latitude"]<trashData$latitude<schoolCoordinates[which(schools[,"VESTIGINGSNAAM"] == input$School2),"Latitude"]]
      #        trashData$longitude[schoolCoordinates[which(schools[, "VESTIGINGSNAAM"] == input$School1), "Longtitude"]]
      #        , col="blue")
       }
  })
}

shinyApp(ui = ui, server = server)


