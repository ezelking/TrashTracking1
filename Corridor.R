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
                  choices = schools[,"VESTIGINGSNAAM"],
                  selected = "Openbare Basisschool 't Eenspan"
                  ),
      selectInput(inputId = "School2",
                  label = "Select Second School",
                  choices = schools[,"VESTIGINGSNAAM"],
                  selected = "Samenwerkingsschool De Schans"
      ),
      hr()
      
     
    ),
    
    mainPanel(
      
      plotOutput("corridor")
    )
    
  )
)

server <- function(input, output, session) {
  trashNearLine <- matrix(nrow = 100, ncol = 1)
  
   # Checks if a point is close to the corridor
  nearLine <- function(point, lineStart, lineEnd) {
    v1 <- lineStart - lineEnd
    v2 <- point - lineStart
    m <- cbind(v1, v2)
    d <- abs(det(m)) / sqrt(sum(v1 * v1))
    return (d < 0.01)
  }

  ########## Diagram 1 ##########
  output$corridor <- renderPlot({
    plot( trashData$latitude ~ trashData$longitude, ylab="latitude"
          , ylim=c((input$lat +0.1 - ((100 - input$Zoom) *0.02)), (input$lat-0.1 + ((100 - input$Zoom) *0.02)))
          , xlab="longitude", xlim=c((input$lng +0.1 - ((100 - input$Zoom) *0.025)), (input$lng-0.1 + ((100 - input$Zoom) *0.025)))
          , main="trash data", col="red" )
    
    points(schoolCoordinates
           , col="blue")
    if (input$SchoolCorridors){
      for (i in 1:nrow(trashData)){
        if (nearLine(c(trashData[i,"latitude"], trashData[i,"longitude"]),c(schoolCoordinates[which(schools[,"VESTIGINGSNAAM"] == input$School1),"Longtitude"], schoolCoordinates[which(schools[,"VESTIGINGSNAAM"] == input$School1),"Latitude"]),c(schoolCoordinates[which(schools[,"VESTIGINGSNAAM"] == input$School2),"Longtitude"], schoolCoordinates[which(schools[,"VESTIGINGSNAAM"] == input$School2),"Latitude"])))
        trashNearLine <- c(trashNearLine, i)
      }
      
      
      
       ## Adds a line between the chosen locations
      segments(schoolCoordinates[which(schools[,"VESTIGINGSNAAM"] == input$School1),"Longtitude"], schoolCoordinates[which(schools[,"VESTIGINGSNAAM"] == input$School1),"Latitude"], schoolCoordinates[which(schools[,"VESTIGINGSNAAM"] == input$School2),"Longtitude"], schoolCoordinates[which(schools[,"VESTIGINGSNAAM"] == input$School2),"Latitude"])

       ## Saves the points that are close to the line
      trashNearLine <- matrix(nrow = 100, ncol = 1)
      j <- 1
      for (i in 1:nrow(trashData)) {
        if (nearLine(
          c(trashData[i, "longitude"], trashData[i, "latitude"]),
          c(schoolCoordinates[which(schools[, "VESTIGINGSNAAM"] == input$School1), "Longtitude"], schoolCoordinates[which(schools[, "VESTIGINGSNAAM"] == input$School1), "Latitude"]),
          c(schoolCoordinates[which(schools[, "VESTIGINGSNAAM"] == input$School2), "Longtitude"], schoolCoordinates[which(schools[, "VESTIGINGSNAAM"] == input$School2), "Latitude"])
        ))
        {
          trashNearLine[j] <- i
          j <- j + 1
        }
      }
      
       ## Colors the points that are close to the line
      points(trashData$longitude[trashNearLine],trashData$latitude[trashNearLine],
             col = "green")
       }
  })
}





shinyApp(ui = ui, server = server)


