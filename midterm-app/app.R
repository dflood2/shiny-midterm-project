library(leaflet)
library(leaflet.extras)
library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(hwordcloud)
library(wordcloud2)
movies <- read.csv("MoviesOnStreamingPlatforms_updated.csv")
movies <- movies[,-1]
movies <- movies %>% mutate(Rotten.Tomatoes = as.numeric(gsub("%", "", Rotten.Tomatoes)))
movies$Platform[movies$Netflix == 1 & movies$Hulu == 0 & movies$Prime.Video ==0 & movies$Disney. == 0] <- "Netflix"
movies$Platform[movies$Netflix == 0 & movies$Hulu == 0 & movies$Prime.Video ==1 & movies$Disney. == 0] <- "Prime"
movies$Platform[movies$Netflix == 0 & movies$Hulu == 1 & movies$Prime.Video ==0 & movies$Disney. == 0] <- "Hulu"
movies$Platform[movies$Netflix == 0 & movies$Hulu == 0 & movies$Prime.Video ==0 & movies$Disney. == 1] <- "Disney Plus"
movies <- movies %>% drop_na(Platform)
# View(movies)

age <- c("all", "7+", "13+", "16+", "18+")
genres <- unique(movies$Genres)

locations <- data.frame("Name" = c('JPJ Arena', 'Scott Stadium', 'Trinity'),
                        "Long" = c(-78.506750, -78.513624, -78.500245),
                        "Lat" = c(38.046139, 38.031365, 38.035101)
)

quakedata <- read.csv("earthquake.csv")

#categorize earthquake depth
quakedata$depth_type <- ifelse(quakedata$depth <= 70, "shallow", 
                               ifelse(quakedata$depth <= 300 | quakedata$depth >70, "intermediate", 
                                      ifelse(quakedata$depth > 300, "deep", "other")))

ui <- fluidPage(theme = shinytheme("sandstone"),
                navbarPage(title = "Shiny Package Presentation",
                           tabPanel(title="Package Overview",
                                    img(height = 300, width=450, src="shiny_image.png"),
                                    h1("Background"),
                                    p(code("shiny"), "was created to help users familiar with programming in R 
                to build interactive web-apps.", code("shiny"), "can host standalone apps on 
                a webpage, embed them in R Markdown, or through dashboards. You 
                can embed your analysis findings into the web-apps and users are 
                able to mess around with your data and the results for better 
                understanding and communication of the results."),
                                    p(code("shiny"), "uses reactive programming to display changes in users controls onto 
                the visualizations shown in the app. One of the best aspects of", code("shiny"),
                                      "is the immense capabilities the package has to offer. One can create a simple app 
                showing one panel of graphics or create a whole webpage with multiple panels 
                that involves user interaction.", code("shiny"), "is also well documented with", 
                                      a(href="https://shiny.rstudio.com/tutorial/tutorial", "tutorial videos"),"and blogs 
                and", a(href="https://shiny.rstudio.com/gallery/", "examples of well-made apps"), 
                                      "to help with inspiration on creating your own", code("shiny"), "app!"),
                                    h3("Version History"),
                                    p("First Version: Version 0.2.3 - November 30, 2012"),
                                    p("Current Version: 1.5.0 - June 23, 2020"),
                                    p("There have been many updates to", code("shiny"), "since it was first launched. The
                                      full history of its version updates can be found", 
                                      a(href="https://www.rdocumentation.org/packages/shiny/versions/1.5.0", "here"), "."),
                                    h3("Dependencies"),
                                    p(code("shiny"), "isn't dependent on any other packages, but it is helpful to use", 
                                      code("tidyverse"), "and", code("ggplot"), "to help organize the data being used
                  and to create nice visualizations. There are some add-on features that are used with
                  ", code("shiny"), "such as", code("shinythemes"), "and", code("shinydashboard"),". 
                  Both are used to enhance the visual appearance of your", code("shiny"), "app by adding built-in 
                  themes similar to those used in R Markdown."),
                                    h3("Usage"),
                                    p("There are three main functions used to build a", code("shiny"), "app:"),
                                    p("1. The", code("ui"), "component is used to create the inputs in the app that the 
                  user will be able to interact with once the app is rendered. Within this component,
                  there are many different functions that are referred to as inputs, that can be used.
                  Some of the most common input functions are:", code("actionButton"), ",", code("selectInput"), "and",
                                      code("sliderInput"), "."),
                                    p("There are over 30 different functions used to create widgets that users interact
                  within the app. These", a(href="https://shiny.rstudio.com/reference/shiny/latest/", " UI functions"), 
                                      "usually begin with the name of the widget you want to create and ends with input."),
                                    p("2. The", code("server"), "component is where the inputs that you want to be made
                  reactive are changed and rendered into their respective outputs. This part of the app is treated as a 
                  function that takes in the inputs and outputs. The inputs from the", code("ui"), "are called upon and 
                  functions such as", code("renderPlot"), "or", code("renderText"), "are used to display the visualizations
                  in the app."),
                                    p("3. Finally,", code("shinyApp"), "runs the app taking in the ui and server argument.")
                           ),
                           navbarMenu(title="Usage Examples",
                                      tabPanel(title="Sidebar Example",
                                               titlePanel("Movie Options"),
                                               sidebarLayout(
                                                   sidebarPanel(
                                                       selectInput("platformInput", "Streaming Platform", 
                                                                   choices = c("Netflix", "Hulu", "Prime", "Disney Plus")),
                                                       radioButtons("ageInput", "Age", choices = age)
                                                   ),
                                                   mainPanel(
                                                       plotOutput(outputId = "MoviePlot"),
                                                   )
                                               ),
                                               a(href="https://www.kaggle.com/ruchi798/movies-on-netflix-prime-video-hulu-and-disney", "Source")
                                      ),
                                      tabPanel(title="Interactive Map",
                                               titlePanel("Interactive Maps"),
                                               splitLayout(
                                                   leafletOutput("map"),
                                                   leafletOutput("mymap"),
                                                   mainPanel(checkboxInput("markers", "Depth", FALSE),
                                                             checkboxInput("heat", "Heatmap", FALSE)
                                                   )
                                               ),
                                               p("You can use the leaflet package to create all types of maps, depending on the type of data
                                                 you have. Within the shiny app you design, you can make this apps interactive.  The map on the
                                                 left was built without using any data, but instead using the longitude and latitude of a chosen 
                                                 location.  The map on the right is a more interactive map with input functions based on earthquake 
                                                 data.  The checkboxes (inputs) are dynamic so when you uncheck the box, the corresponding 
                                                 visualization is removed.  You can customize and control a map that has already been rendered using ", code("leafletProxy()")),
                                               p("Source: "), code("https://medium.com/@joyplumeri/how-to-make-interactive-maps-in-r-shiny-brief-tutorial-c2e1ef0447da")
                                      ),
                                      tabPanel(title="Word Cloud",
                                               titlePanel("Demo Word Cloud"),
                                               mainPanel(
                                                   hwordcloudOutput("shinytest", height = "450px")
                                               ),
                                               p("We chose to create this word chart using the library wordcloud2, but Shiny also has the 
                                               capabilities to build a word cloud of this sort from scratch. This shows how Shiny can be 
                                               used to create complex applications all from R."),
                                               p("Using wordcloud2 library to create a word cloud in Shiny from pre-existing dataset."),
                                               code("https://github.com/Lchiffon/wordcloud2")
                                      )
                           ),
                           navbarMenu(title="Similar Packages",
                                      tabPanel(title="Plotly",
                                               img(height=300, width=700, src="plotly_image.png"),
                                               h3("Similarities"),
                                               p("It makes sense to compare the Shiny package with Plotly because embedding Plotly graphics 
                                                 into a Shiny app is a combination that is frequently implemented. Although both packages can 
                                                 be used to produce interactive graphics that can be easily manipulated by the user, there 
                                                 are some key differences that makes using them together in order to optimize the end product 
                                                 extremely valuable. Plotly is a dashboard and cloud server, that can be run on any local server. 
                                                 Shiny, on the other hand, is an application server, meaning that the user codes the app 
                                                 themselves and it needs to run from a Shiny server. Additionally, the Shiny package doesn’t have 
                                                 any dependencies other than an updated version of R, but Plotly relies on the ggplot2 
                                                 package."),
                                               h3("Differences"),
                                               p("Shiny allows the user to access static graphics using other R packages like graphics, ggplot2, 
                                                 and lattice. These packages support a wide range of features and graphics; however, they are limited 
                                                 with regards to interactive graphics. In comparison, Plotly has more aesthetic options and interactive 
                                                 features. Plotly also allows for more control over how the graphics update in response to user input 
                                                 because it is web-based."),
                                      ),
                                      tabPanel(title="Leaflet",
                                               img(height=250, width=700, src="leaflet_image.png"),
                                               h3("Similarities"),
                                               p("It is also only natural to compare the functionalities of the Shiny and Leaflet. Both are user-friendly 
                                                 tools for interactive mapping. However, just like with Plotly, Leaflet is frequently paired with Shiny 
                                                 to optimize the output. Shiny takes longer to render and plot all the points when mapping, so Leaflet is 
                                                 more commonly preferred, and can ultimately be embedded into a Shiny app. Furthermore, Leaflet has a 
                                                 wider range of interactive features like search bars, icons, extensive layering options, and marker methods 
                                                 to manipulate the map widget."),
                                               h3("Differences"),
                                               p("Speaking of widgets, Shiny widgets allow users to switch between datasets, building a new map each time. 
                                                 On the other hand, Leaflet allows the user to directly build the control widget while creating the map, 
                                                 adding every layer the same map (same group). The widget will allow you to switch from one group to another 
                                                 more efficiently. Another Leaflet feature is the “localize me” button, which allows for the map to automatically 
                                                 zoom to the user’s position."),
                                      )
                           ),
                           tabPanel(title="Reflection", 
                                    h1("Reflection"),
                                    p("Using", code("shiny"), "at first may be overwhelming 
            for those that have no web building experience, but", code("shiny"), "has its 
            own webpage and a series of tutorial videos that guide users on how 
            to build an interactive web app using shiny through R. After interacting 
            with some apps, it appears the more complicated the app, the longer it 
            takes to load in the new data and visualize changes to the plots. Some difficulties we 
            faced while building this app is that it is somewhat difficult to debug in", code("shiny"), 
                                      "as it is easy to misplace a comma or parenthesis and the error messages do not 
            direct you to the line of code where the app is not rendering correctly."),
                                    p("Some of the best features", code("shiny"), "has to offer is the user interaction
            with the created plots and how easy it is to implement HTML and CSS into the app without much previous experience.
            Most of us did not have any previous web building experience but found using", code("shiny"), "as a great
            starting point because of its easy set-up through R. The functions are easy to remember and call on and the basic
            template of the", code("ui"), "and", code("server"), "functions help build a well-structured app.", code("shiny"),
                                      "is also great in the way that it encourages you to use packages that you may already know, making the building 
            process easier as there is some familiarity incorporating these packages into the app.")
                           )
                )
)



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
