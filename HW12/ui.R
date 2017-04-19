#https://rstudio.github.io/shinydashboard/get_started.html
#install.packages("shinydashboard")

## ui.R ##
library(shinydashboard)
library(shiny)
library(leaflet)
library(shinyAce)
library(sendmailR)

dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)
