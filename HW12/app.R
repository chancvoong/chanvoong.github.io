## app.R ##
#install.packages(c("ggplot2", "devtools", "dplyr", "stringr", "rgdal"))
#install.packages(c("maps", "mapdata"))
#devtools::install_github("dkahle/ggmap")

#install.packages(c("maps", "mapproj"))
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(rgdal)
library(shinydashboard)
library(shinyAce)
library(sendmailR)
library(leaflet)
library(mapdata)
library(mapproj)
library(shiny)
library(shinyBS)
library(rgdal)
library(raster)
library(data.table)
library(dplyr)
library(leaflet)

lat <- NULL
lng <- NULL
notes <- NULL


source("helpers.R")


#pull in data before everything else
tmp <- read.csv(file="Libraries2.csv",header=TRUE,sep=",",stringsAsFactors = FALSE)
dimnames(tmp) <- list(rownames(tmp$X, do.NULL = FALSE, prefix = tmp$X),
                      colnames(tmp, do.NULL = FALSE, prefix = "col"))
tmp <- as.data.frame(tmp)
#tmp <- tmp[, -1]

#map philly
philly <- readOGR("censustractsphilly.shp")

#read library data for locations map
locs <- read.csv("Libraries.csv")
locs$loc <- as.character(locs$loc)
d.cru <- data.frame(matrix(list(locs)))

#set lat lng for centering map
longitude <- -75.199356
latitude <- 39.9494666


#set the data to only locations in Philly (though all should be philly)
d <- d.cru$matrix.list.locs..[[1]] %>% filter(City=="Philadelphia")


#Set facility data
fac <- read.csv("Facilities.csv")
fac$Library <- as.character(fac$Library)
dimnames(fac) <- list(rownames(fac$Library, do.NULL = FALSE, prefix = fac$Library),
                      colnames(fac, do.NULL = FALSE, prefix = "col"))
fac <- as.data.frame(fac) 




ui <- dashboardPage(
  dashboardHeader(title = "Free Library of Phila",
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Sales Dept",
                                 message = "Sales are steady this month."
                               ),
                               messageItem(
                                 from = "New User",
                                 message = "How do I register?",
                                 icon = icon("question"),
                                 time = "13:45"
                               ),
                               messageItem(
                                 from = "Support",
                                 message = "The new server is ready.",
                                 icon = icon("life-ring"),
                                 time = "2014-12-01"
                               )
                  ),                
                  
                  dropdownMenu(type = "notifications",
                               notificationItem(
                                 text = "5 new users today",
                                 icon("users")
                               ),
                               notificationItem(
                                 text = "12 items delivered",
                                 icon("truck"),
                                 status = "success"
                               ),
                               notificationItem(
                                 text = "Server load at 86%",
                                 icon = icon("exclamation-triangle"),
                                 status = "warning"
                               )
                  ),
                  
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 90, color = "green",
                                        "Documentation"
                               ),
                               taskItem(value = 17, color = "aqua",
                                        "Project X"
                               ),
                               taskItem(value = 75, color = "yellow",
                                        "Server deployment"
                               ),
                               taskItem(value = 80, color = "red",
                                        "Overall project"
                               )
                  )
                  
                  )
  
  
  ,
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                        label = "Search..."),
      menuItem("About", tabName = "About", icon = icon("info-circle")),
      menuItem("Outline", tabName = "Outline", icon = icon("info-circle")),
      menuItem("Metrics", tabName = "Metrics", icon = icon("bar-chart")),
      menuItem("Library", tabName = "Library", icon = icon("book")),
      menuItem("Maps", tabName = "Maps", icon = icon("map")),
      menuItem("Data", tabName = "Data", icon = icon("address-card-o")),
      menuItem("Model_dev", tabName = "Model_dev", icon = icon("address-card-o")),
      menuItem("Contact", tabName = "Contact", icon = icon("address-card-o")),
      menuItem("Code", tabName = "Code", icon = icon("file-code-o"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "About",
              h2("About"),
                box(width = 10,
                  "This project aims use shiny as an interactive tool for visualizing summer attendance for programs at the Free Library of Philadelphia.",
                  br(),
                  br(),
                  "Data was collected from several sources including internal library data, Census, OpenDataPhilly, and hosted ESRI and user layers from ArcGIS Online.",
                  br(),
                  br(),
                  "Click on dashboard options to use the various tools."

                ),
              a(img(src = "library.jpg"), href="http://www.freelibrary.org")
              
      ),

      #tab content
      tabItem(tabName = "Outline",
              h2("Outline"),
              fluidRow(
                box(
                  title = "Data Collection", width = 10, solidHeader = TRUE, status = "primary",

                  "Library Locations",
                  br(),
                  "Scraped from", a(("Free Library of Philadelphia"),href="https://libwww.freelibrary.org/locations/"),
                  br(),
                  "Click", a(("here"),href="https://github.com/chancvoong/FLP/blob/master/Scrape_Libraries.R"), "for code",
                  br(),
                  br(),
                  "Prek, homecare, childcare sites",
                  br(),
                  "Scraped from", a(("PA Compass"),href="https://www.compass.state.pa.us/Compass.web/ProviderSearch/Home#/SearchResults"),
                  br(),
                  "Location: Philadelphia",
                  br(),
                  "Children's Ages: 0-5",
                  br(),
                  "Click", a(("here"),href="https://github.com/chancvoong/FLP/blob/master/Scrape_HomeCare.R"), "for code",
                  br(),
                  br(),
                  "Summer Reading Data 2014-2016",
                  br(),
                  "Retrieved internally from the Free Library of Philadelphia",
                  br(),
                  br(),
                  "Summer Meal Sites",
                  br(),
                  "Found from a", a(("hosted arcgis online layer"),href="https://upenn.maps.arcgis.com/home/item.html?id=6f6868142a754a6cb0c9f5477d81f17d"),
                  br(),
                  "Hosted by Amory Hillengass at the City of Philadelphia DPH, and former MUSA student", 
                  br(),
                  br(),
                  "Pal Centers",
                  br(),
                  "Found from", a(("OpenDataPhilly"),href="https://www.opendataphilly.org/dataset/police-activity-league-centers"),
                  br(),
                  br(),
                  "Parks and Recreation Out of School Time Programs",
                  br(),
                  "Found from", a(("OpenDataPhilly"),href="https://www.opendataphilly.org/dataset/ppr-out-of-school-time-programs"),
                  br(),
                  br(),
                  "Parks",
                  br(),
                  "Found from", a(("OpenDataPhilly"),href="https://www.opendataphilly.org/dataset/parks-and-recreation-assets/resource/d05b9ea4-7dea-4501-a7fa-b7f90937d747"),
                  br(),
                  br(),
                  "Pools and SprayGrounds",
                  br(),
                  "Found from a", a(("hosted arcgis online layer"),href="https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services/Pools_SprayGrounds/FeatureServer"),
                  br(),
                  "Hosted by Jen Johnson, OEM, Credits to Phila Parks and Rec"                  
                  
                )),
              
              fluidRow(
                box(
                  title = "Data Query", width = 10, solidHeader = TRUE, status = "primary",
                  "Use ArcGIS to geocode locations of libraries then spatial join the library locations to summer reading attendance data 2014-2016",
                  br(),
                  br(),
                  "Use Generate Near Table to calculate distance to each of the following:",
                  br(),
                  "Prek, homecare, childcare sites",
                  br(),
                  "PAL centers",
                  br(),
                  "PPR Out of School Programs",
                  br(),
                  "Parks",
                  br(),
                  "Pools and Spraygrounds",
                  br(),
                  br(),
                  "Save and export the shp files"
              
                ))
      ),
    
      
      
      
      # Second tab content
      tabItem(tabName = "Metrics",
              h2("Metrics"),

              box(width = 10,
                  "Summer Reading Statistics are presented by a bar chart below",
                  br(),
                  br(),
                  "Be sure to select a variable other than X",
                  br(),
                  br(),
                  "14 indicates 2014",
                  br(),
                  "15 indicates 2015",
                  br(),
                  "16 indicates 2016",
                  br(),
                  "Tot indicates total for 2014-2016",
                  br(),
                  br(),
                  "NISV = Number of In-School Visits",
                  br(),
                  "AISV = Attendance In-School Visits",
                  br(),
                  "NISRP = Number of In-School Reading Programs",
                  br(),
                  "AISRP = Attendance In-School Reading Programs",
                  br(),
                  "NPIP = Number of Programs In-Preschools",
                  br(),
                  "APIP = Attendance for Programs In-Preschools",
                  br(),
                  "With O instead of I, Out of School"
                  
              ),
              

              fluidRow(    

                # Generate a row with a sidebar
                sidebarLayout(      
                  
                  # Define the sidebar with one input
                  sidebarPanel(
                    selectInput("Library", "Variables:", 
                                choices=colnames(tmp)),
                    hr(),
                    helpText("Choose a Summer Reading Variable")
                  ),
                  
                  # Create a spot for the barplot
                  mainPanel(
                    plotOutput("LibPlot")  
                  )
                  
                )
              )
            
  
      ),
      
      
      # Third tab content
      tabItem(tabName = "Library",
              h2("Library"),
              shinyUI(fluidPage(
                box(width = 10,
                    "This tool is used to visualize Free Libraries of Philadelphia.",
                    br(),
                    br(),
                    "Select a library from the drop down"
                    
                    
                ),
                
                      tags$style(type="text/css", "html, body {width:100%;height:100%}"),
                      leafletOutput("Map"),
                      absolutePanel(top=200, left=300,
                                    checkboxInput("show_communities", "Show communities", TRUE),
                                    conditionalPanel("input.show_communities == true",
                                                     selectInput("location", "Library", c("", locs$loc), selected=""),
                                                     conditionalPanel("input.location !== null && input.location !== ''",
                                                                      actionButton("button_plot_and_table", "View Plot/Table", class="btn-block"))
                                    )
                      ),
                      bsModal("Plot_and_table", "Plot and Table", "button_plot_and_table", size = "large",
                              plotOutput("TestPlot"),
                              dataTableOutput("TestTable")
                      
                    
                  ),
                  
                 plotOutput("map")
                
              ))
              
              ),
      

  
      
      # Fourth tab content
      tabItem(tabName = "Maps",
              h2("Maps"),
              tabPanel("Map",
                       sidebarLayout(
                         sidebarPanel(
                           box(width = 10,
                               "Use this tool to place your own points for more spatial components."
                           ),
                           
                           textInput("popup", "Add Notes"),
                           downloadButton("download", "Download Lat/Long Data")
                         ),
                         
                         
                         mainPanel(
                           leafletOutput("leaflet")
                         )
                       )
                       
              )
      ),
      
      # Fifth tab content    
      tabItem("Data",
               sidebarLayout(sidebarPanel( 
                 
                 box(width=20,
                     "Upload the", a(("Library.csv dataset"),href="https://www.dropbox.com/s/rmuqu7b7kdykh48/Libraries.csv?dl=0"),
                     br(),
                     br(),
                     "To complete a logistic regression, click Model_Dev on the dashboard.",
                     br(),
                     br(),
                     "The near distances to local programs in relation to attendance would be the most interesting to see."
                     
                 ),
                 
                 
                 
                 fileInput("file","Upload your CSV",multiple = FALSE),
                                           tags$hr(),
                                           h5(helpText("Select the read.table parameters below")),
                                           checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                                           checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                                           radioButtons(inputId = 'sep', label = 'Separator', 
                                                        choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
               ),
               mainPanel(uiOutput("tb1"))
               ) ),

      # Sixth tab content      
      tabItem("Model_dev",
               sidebarLayout(sidebarPanel(
                 uiOutput("model_select"),
                 uiOutput("var1_select"),
                 uiOutput("rest_var_select")),
                 mainPanel( helpText("Your Selected variables"),
                            verbatimTextOutput("other_val_show")))
    ),
      
      
      # Seventh tab content
      tabItem(tabName = "Contact",
              h2("Contact"),
              sidebarPanel(
                textInput("from", "From:", value="from@gmail.com"),
                textInput("to", "To:", value="to@gmail.com"),
                textInput("subject", "Subject:", value=""),
                actionButton("send", "Send mail")
              ),
              
              mainPanel(    
                aceEditor("message", value="write message here")
              )
      ),
      
      #Eighth tab item
      tabItem(tabName = "Code",
               h2("Code"),
               box(
                 width = 10, background = "light-blue",
                 "Source code is from https://shiny.rstudio.com/tutorial and https://github.com/chancvoong/chanvoong.github.io"


      )
      
    )
  )
)
)


server <- function(input, output) {

  

    # Fill in the spot we created for a plot
    output$LibPlot <- renderPlot({
   
      # Render a barplot
      mp <- barplot(tmp[,input$Library], 
              main=input$Library,
              ylab="Count",
              xlab="Libraries"
              )
      
      text(mp, par(las=3), labels = tmp$X, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.5)

      
    })
  

  

  output$leaflet <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      setView(lat = 39.9494666, lng = -75.199356, zoom = 13)
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("upenn-", Sys.Date(), ".csv")
    },
    content = function(temp) {
      write.csv(data.frame(lat = latitude, lng = longitude, notes = notes), temp)
    }
  )
  
  observe({
    
    if(!is.null(input$leaflet_click$lat)){
      leafletProxy("leaflet") %>%
        addMarkers(lat = input$leaflet_click$lat, 
                   lng = input$leaflet_click$lng, 
                   popup = input$popup)
      latitude <<- c(lat, input$leaflet_click$lat)
      longitude <<- c(lng, input$leaflet_click$lng)
      notes <<- c(notes, input$popup)
    }
    
  })
  
  
  shinyServer(function(input, output, session) {
    
    observe({
      if(is.null(input$send) || input$send==0) return(NULL)
      from <- isolate(input$from)
      to <- isolate(input$to)
      subject <- isolate(input$subject)
      msg <- isolate(input$message)
      sendmail(from, to, subject, msg)
    })
    
  })  
  
  
  
data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    
  })  
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  output$tb1 <- renderUI({
    tableOutput("table")
  })
  output$model_select<-renderUI({
    selectInput("modelselect","Select Algo",choices = c("Logistic_reg"="logreg","SVM"="svm"))
  })
  output$var1_select<-renderUI({
    selectInput("ind_var_select","Select Independent Var", choices =as.list(names(data())),multiple = FALSE)
  })
  output$rest_var_select<-renderUI({
    checkboxGroupInput("other_var_select","Select other Var",choices =as.list(names(data())))
  })
  output$other_val_show<-renderPrint({
    input$other_var_select
    input$ind_var_select
    f<-data()
    
    library(caret)
    form <- sprintf("%s~%s",input$ind_var_select,paste0(input$other_var_select,collapse="+"))
    print(form)
    
    logreg <-glm(as.formula(form),data=f)
    print(summary(logreg))
    
  })
  
  
  
#}

# @knitr server03
#server1 <- function(input, output, session) {
  acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=6, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
  
  output$Map <- renderLeaflet({
    leaflet() %>% setView(longitude, latitude, 11) %>% addTiles() %>%
      addCircleMarkers(data=locs, radius=6, color="black", stroke=FALSE, fillOpacity=0.5, group="locations", layerId = ~loc)
  })
  
  
  observe({ # show or hide location markers
    proxy <- leafletProxy("Map")
    if (input$show_communities) {
      proxy %>% showGroup("locations")
    } else {
      updateSelectInput(session, "location", selected="")
      proxy %>% hideGroup("locations") %>% removeMarker(layerId="Selected")
    }
  })
  
  observeEvent(input$Map_marker_click, { # update the map markers and view on map clicks
    p <- input$Map_marker_click
    proxy <- leafletProxy("Map")
    if(p$id=="Selected"){
      proxy %>% removeMarker(layerId="Selected")
    } else {
      proxy %>% setView(lng=p$lng, lat=p$lat, input$Map_zoom) %>% acm_defaults(p$lng, p$lat)
    }
  })
  
  observeEvent(input$Map_marker_click, { # update the location selectInput on map clicks
    p <- input$Map_marker_click
    if(!is.null(p$id)){
      if(is.null(input$location) || input$location!=p$id) updateSelectInput(session, "location", selected=p$id)
    }
  })
  
  observeEvent(input$location, { # update the map markers and view on location selectInput changes
    p <- input$Map_marker_click
    p2 <- subset(locs, loc==input$location)
    proxy <- leafletProxy("Map")
    if(nrow(p2)==0){
      proxy %>% removeMarker(layerId="Selected")
    } else if(length(p$id) && input$location!=p$id){
      proxy %>% setView(lng=p2$lng, lat=p2$lat, input$Map_zoom) %>% acm_defaults(p2$lng, p2$lat)
    } else if(!length(p$id)){
      proxy %>% setView(lng=p2$lng, lat=p2$lat, input$Map_zoom) %>% acm_defaults(p2$lng, p2$lat)
    }
  })
  
  # @knitr server03pointdata
  Data1 <- reactive({ d %>% filter(loc==input$location) })
  
  output$TestPlot <- renderPlot({ ggplot(fac, 
                                         aes(Library, ""), par(las=3), labels = fac$Library, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.5) +
      geom_bar(stat = "identity") 
      #geom_smooth()
    #(par(las=3), labels = fac$Library, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.5)
  })
  
  output$TestTable <- renderDataTable({
    Data1()
  }, options = list(pageLength=5))
  # @knitr server03remainder
  
  
  
}





shinyApp(ui=ui, server=server)

