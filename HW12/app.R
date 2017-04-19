## app.R ##
library(shinydashboard)
library(shinyAce)
library(sendmailR)
library(leaflet)
#install.packages(c("maps", "mapproj"))

library(maps)
library(mapproj)
counties <- readRDS("data/counties.rds")
source("helpers.R")


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
      menuItem("Library", tabName = "Library", icon = icon("book")),
      menuItem("Metrics", tabName = "Metrics", icon = icon("bar-chart")),
      menuItem("Census", tabName = "Census", icon = icon("area-chart")),
      menuItem("Maps", tabName = "Maps", icon = icon("map")),
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
              box(
                width = 10, background = "light-blue",
                "Data was collected from several sources including internal library data, Census, OpenDataPhilly, and hosted ESRI and user layers from ArcGIS Online. Using network analyst, library zones were creating by selecting census block groups that had the shortest distance to each library. Demographics were then measured for each library zone. Since there were several variables to think about, I created an interactive ArcGIS Online Web Application for easy filtering. \n \n 
                
                This shiny app may be a way to display the data without using or paying for ArcGIS Online tools"
              ),
              a(img(src = "freelibrary.jpg"), href="http://www.freelibrary.org")
              
      ),
      
      tabItem(tabName = "Library",
              h2("Library"),
              shinyUI(fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    helpText("Create demographic maps with 
                             information from the 2010 US Census."),
                    
                    selectInput("var2", 
                                label = "Choose a variable to display",
                                choices = c("Percent White", "Percent Black",
                                            "Percent Hispanic", "Percent Asian"),
                                selected = "Percent White"),
                    
                    sliderInput("range2", 
                                label = "Range of interest:",
                                min = 0, max = 100, value = c(0, 100))
                    ),
                  
                  mainPanel(plotOutput("library"))
                )
              ))
      ),
      
      
      
      # Second tab content
      tabItem(tabName = "Metrics",
              h2("Metrics"),
              
              box(
                title = "Histogram", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("plot1", height = 250)
              ),
              
              box(
                title = "Inputs", status = "warning", solidHeader = TRUE,
                "Box content here", br(), "More box content",
                sliderInput("slider", "Slider input:", 1, 100, 50),
                textInput("text", "Text input:")
              ),
              
              
              fluidRow(
                box(title = "Box title", "Box content"),
                box(status = "warning", "Box content")
              ),
              
              fluidRow(
                box(
                  title = "Title 1", width = 4, solidHeader = TRUE, status = "primary",
                  "Box content"
                ),
                box(
                  title = "Title 2", width = 4, solidHeader = TRUE,
                  "Box content"
                ),
                box(
                  title = "Title 1", width = 4, solidHeader = TRUE, status = "warning",
                  "Box content"
                )
              ),
              
              fluidRow(
                box(
                  width = 4, background = "black",
                  "A box with a solid black background"
                ),
                box(
                  title = "Title 5", width = 4, background = "light-blue",
                  "A box with a solid light-blue background"
                ),
                box(
                  title = "Title 6",width = 4, background = "maroon",
                  "A box with a solid maroon background"
                )
              )
      ),
      
      
      # Third tab content
      tabItem(tabName = "Census",
              h2("Census"),
              shinyUI(fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    helpText("Create demographic maps with 
        information from the 2010 US Census."),
                    
                    selectInput("var", 
                                label = "Choose a variable to display",
                                choices = c("Percent White", "Percent Black",
                                            "Percent Hispanic", "Percent Asian"),
                                selected = "Percent White"),
                    
                    sliderInput("range", 
                                label = "Range of interest:",
                                min = 0, max = 100, value = c(0, 100))
                  ),
                  
                  mainPanel(plotOutput("map"))
                )
              ))
              
              ),
      
      # Fourth tab content
      tabItem(tabName = "Maps",
              h2("Maps"),
              tabPanel("Map",
                       sidebarLayout(
                         sidebarPanel(
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
      
      #Sixth tab item
      tabItem(tabName = "Code",
               h2("Code"),
               box(
                 width = 10, background = "light-blue",
                 "Source code is from https://shiny.rstudio.com/tutorial"


      )
      
    )
  )
)
)


server <- function(input, output) {

  output$library <- renderPlot({
    args2 <- switch(input$var2,
                   "Percent White" = list(counties$white, "darkgreen", "% White"),
                   "Percent Black" = list(counties$black, "black", "% Black"),
                   "Percent Hispanic" = list(counties$hispanic, "darkorange", "% Hispanic"),
                   "Percent Asian" = list(counties$asian, "darkviolet", "% Asian"))
    
    args2$min <- input$range2[1]
    args2$max <- input$range2[2]
    
    do.call(percent_map, args2)
  })
  
  output$map <- renderPlot({
    args <- switch(input$var,
                   "Percent White" = list(counties$white, "darkgreen", "% White"),
                   "Percent Black" = list(counties$black, "black", "% Black"),
                   "Percent Hispanic" = list(counties$hispanic, "darkorange", "% Hispanic"),
                   "Percent Asian" = list(counties$asian, "darkviolet", "% Asian"))
    
    args$min <- input$range[1]
    args$max <- input$range[2]
    
    do.call(percent_map, args)
  })
  
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
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
      write.csv(data.frame(lat = lat, lng = lng, notes = notes), temp)
    }
  )
  
  observe({
    
    if(!is.null(input$map_click$lat)){
      leafletProxy("map") %>%
        addMarkers(lat = input$map_click$lat, 
                   lng = input$map_click$lng, 
                   popup = input$popup)
      lat <<- c(lat, input$map_click$lat)
      lng <<- c(lng, input$map_click$lng)
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

   
  
}



shinyApp(ui, server)

