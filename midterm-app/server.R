server <- function(input, output){
    plotData <- reactive({
        movies %>% 
            filter(Age == input$ageInput) %>% 
            filter(Platform %in% input$platformInput)
    })
    output$MoviePlot <- renderPlot(
        ggplot(plotData(), mapping=aes(x=Rotten.Tomatoes, y=IMDb, color=Platform)) + 
            geom_point() + 
            theme_light() +
            xlab("Rotten Tomatoes Score") +
            ylab("IMDb Score")
    )
    df <- head(demoFreq, 50)
    output$shinytest<-renderHwordcloud({
        hwordcloud(text=df$word, size=df$freq)
    }
    )
    output$map <- renderLeaflet({
        leaflet(locations) %>% 
            addTiles() %>% 
            setView(lng=-78.506750, lat=38.046139, zoom=15) %>% 
            addMarkers(~Long, ~Lat, popup = ~Name)
        
    })
    #define the color pallate for the magnitidue of the earthquake
    pal <- colorNumeric(
        palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
        domain = quakedata$mag)
    
    #define the color of for the depth of the earquakes
    pal2 <- colorFactor(
        palette = c('blue', 'yellow', 'red'),
        domain = quakedata$depth_type
    )
    
    #create the map
    output$mymap <- renderLeaflet({
        leaflet(quakedata) %>% 
            setView(lng = -99, lat = 45, zoom = 2)  %>% #setting the view over ~ center of North America
            addTiles() %>% 
            addCircles(data = quakedata, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~sqrt(mag)*25000, 
                       popup = ~as.character(mag), label = ~as.character(paste0("Magnitude: ", sep = " ", mag)), 
                       color = ~pal(mag), fillOpacity = 0.5) %>% 
            addLegend("bottomright", pal = pal2, values = quakedata$depth_type,
                      title = "Depth Type",
                      opacity = 1)
    })
    
    #next we use the observe function to make the checkboxes dynamic. If you leave this part out you will see that the checkboxes, when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server to update the map when the checkboxes are unchecked.
    observe({
        proxy <- leafletProxy("mymap", data = quakedata)
        proxy %>% clearMarkers() 
        if (input$markers) {
            proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal2(depth_type), fillOpacity = 0.2,
                                       label = ~as.character(paste0("Magnitude: ", sep = " ", mag))) }
        else {
            proxy %>% clearMarkers() 
        }
    })
    
    observe({
        proxy <- leafletProxy("mymap", data = quakedata)
        proxy %>% clearMarkers()
        if (input$heat) {
            proxy %>%  addHeatmap(lng=~longitude, lat=~latitude, intensity = ~mag,
                                  blur =  10, max = 0.05, radius = 15) 
        }
        else{
            proxy %>% clearHeatmap()
        }
        
        
    })
}

shinyApp(ui = ui, server = server)