
library(leaflet)
library(shinydashboard)

header <- dashboardHeader(title = "Nepal Vis")

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Leaflet", tabName = "leaflet", icon = icon("leaf"))
))

body <- dashboardBody(
  tags$style(type = "text/css", "#leaflet_dangue {height: calc(100vh - 80px) !important;}"),

  tabItem(tabName = "leaflet",
  leafletOutput("leaflet_dangue", height = 800) #make this relative later
  ),
  sliderInput("year_select", "Year", min = 2011, max = 2013, value = 2013, ticks = FALSE, sep = "")
          
)

dashboardPage(header, sidebar, body)