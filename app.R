library(raster)
library(pracma)
library(ptw)
library(shiny)
library(shinydashboard)
library(leaflet)

server <- function(input, output) {
  
  
  ras <- stack(list.files("/path/to/your/rasters", full.names=T))
  
  #Get Coordinates for Basemap
  xBase <- (extent(ras)[2] + extent(ras)[1]) / 2
  yBase <- (extent(ras)[4] + extent(ras)[3]) / 2
    
  
  sp <- SpatialPoints(data.frame(xBase ,yBase))
  crs(sp) <- crs(ras)
  sp <- spTransform(sp, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  
  plot(ras[[1]])
  points(sp)

      output$rasPlot <- renderPlot({
        plot(ras[[input$layer]])
      }, height = 400)
      
      
  

  output$rasProj       <- renderText(projection(ras))
  output$rasRes        <- renderText(res(ras))
  output$rasDim        <- renderText(dim(ras))
  output$rasNlayers    <- renderText(nlayers(ras))
  
  
  output$cellnumber <- renderText(round(Coords(),3))
  
  
  output$rasvalue   <- renderText(value())
  
  Coords <- reactive({
    req(input$rasPlot_click$x)
    
    c(input$rasPlot_click$x, input$rasPlot_click$y)
    
    })

  
output$df    <- renderTable({
  
 valdf        <-  data.frame("Layer" = names(ras), "Value" = unlist(value())[1,])
 colnames(valdf) <- c("Name","Value")
 rownames(valdf)  <- 1:nlayers(ras)
 valdf

  
})
  

value  <- eventReactive(input$rasPlot_click$x,{
   extract(ras,cellFromXY(ras,Coords()))
   })


output$Map <- renderLeaflet({
  leaflet() %>% 
    setView(sp@coords[1],sp@coords[2], 8) %>% 
    addProviderTiles("Esri.WorldImagery") 
})


output$TSplot <- renderPlot({
    
  req(input$rasPlot_click$x)
    plot(1:nlayers(ras),value(), type = "l", xlab="Time", ylab="Values")
    #dygraph(value(), main = "Predicted Lung Deaths (UK)")
  
  
      if(input$filterCheckSav) {
        lines(1:nlayers(ras),savgol(value(),5), col = "red")
      }
    
      if(input$filterCheckWhit) {
      lines(1:nlayers(ras),whit2(value(),5), col = "green")
      }
    
    })
}

ui = dashboardPage(
  dashboardHeader(
    title = "R Gis - Raster Time Series Analysis",
    titleWidth = 450
  ),
  dashboardSidebar(
    h4("Filter:"),
    checkboxInput("filterCheckSav", "Savitzky-Golay", value = FALSE),
    checkboxInput("filterCheckWhit", "Whittaker", value = FALSE)),
  
  dashboardBody(
    fluidRow(
      tabBox(
        title = "Interactive Image Analysis", id = "tabset",
        tabPanel("Raster", 
                 plotOutput("rasPlot", click = "rasPlot_click"),
                 sliderInput("layer", "Plot Timestep", min = 1, max = nlayers(ras),1, width="100%")),
        tabPanel("Basemap", leafletOutput("Map", width = "100%"))
      ),
      box(
        title = "Properties", status = "info", solidHeader = TRUE,

        HTML("<b>Layers:</b>"),
        textOutput("rasNlayers"),
        HTML("<b>Resolution:</b>"),
        textOutput("rasRes"),
        HTML("<b>Projection:</b>"),
        textOutput("rasProj"),

        h4("Selected Coordinates"),
        textOutput("cellnumber")
      )
    ),
    fluidRow(
      box(
        title = "Time Series", status = "warning", solidHeader = TRUE, width="100%",
        plotOutput("TSplot")
      )
    ),
    fluidRow(
      box(
        title = "Values", status = "info", solidHeader = TRUE,
        tableOutput("df")
      )
    )
  )
  )
    
  


shinyApp(ui = ui, server = server)
