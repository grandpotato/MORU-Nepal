#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(geojsonio)
library(sp)
library(jsonlite)
library(RCurl)
library(data.table)
library(leaflet)
library(shinydashboard)

# REad this in as a leaflet sp object
#https://github.com/leaflet-extras/leaflet-providers
#instructions here https://rstudio.github.io/leaflet/json.html




shinyServer(function(input, output) {
   
  output$leaflet_dangue <- renderLeaflet({
    
    data_url <- "https://raw.githubusercontent.com/opennepal/odp-health/master/Data%20related%20to%20Communicable%20and%20vector%20borne%20diseases%20(2011-14)/data.csv"
   
    dt <- read.csv(text = getURL(data_url), header = TRUE)
    dt <- data.table(dt)
    dt$District.Name <- as.character(dt$District.Name) 
    #dengue only, 2013/14 only ... for now...
    dt <- dt[Sub.Indicator == "Dangue Fever" & Year..AD. == "2013/14"]
    
    
    #original district shape sources
    source_URL_Nepal_District <- "https://raw.githubusercontent.com/mesaugat/geoJSON-Nepal/master/nepal-districts.topojson"
    source_URL_Nepal_District_HQ <- "https://raw.githubusercontent.com/mesaugat/geoJSON-Nepal/master/nepal-district-headquarters.geojson"
    
    nepal_HQ <- readLines(source_URL_Nepal_District_HQ)  %>% paste(collapse = "\n") 
    nepal_districts <- readLines(source_URL_Nepal_District, warn = FALSE) %>% paste(collapse = "\n") %>% fromJSON(simplifyMatrix = FALSE)
    
    #this is a mapping key so I can assign the values properly to the JSON table. I'll only edit the names here.To get the values across. But to do it properly I'd use the more "official" naming convention.
    district_order <- data.table("District.Name" = nepal_districts$objects$nepal$geometries$properties$name, order_num = seq(1:length(nepal_districts$objects$nepal$geometries$properties$name)))
    
    #Rename the districts with varying translated names
   district_order[District.Name == "Udayapur"]$District.Name <- "Udaypur"
   district_order[District.Name == "Terhathum"]$District.Name <- "Teharthum"
   district_order[District.Name == "Sindhupalchok"]$District.Name <- "Sindhupalchowk"
   district_order[District.Name == "Makwanpur"]$District.Name <- "Makawanpur"    
   district_order[District.Name == "Kavrepalanchok"]$District.Name <- "Kavre"
   district_order[District.Name == "Kapilbastu"]$District.Name <- "Kapilvastu"
   district_order[District.Name == "Dolakha"]$District.Name <- "Dolkha"
   district_order[District.Name == "Dhanusa"]$District.Name <- "Dhanusha"
   district_order[District.Name == "Bhaktapur"]$District.Name <- "Bhaktapur "
    
    
    setkey(district_order, District.Name)
    setkey(dt, District.Name)
    
    #Join the spatial data with the incidence data
    district_incidence <- merge(district_order, dt, nomatch = 0)[, c("District.Name", "order_num", "Value")]
    
    #Order it by the origina district list order in the spatial data
    nepal_districts$objects$nepal$geometries$properties$value <- district_incidence[order(order_num)]$Value
    
    #default coloring
    nepal_districts$style = list(
      weight = 1,
      color = "#555555",
      opacity = 1,
      fillOpacity = 0.8
    )
  
  value_est <- nepal_districts$objects$nepal$geometries$properties$value 
    
  value_pal <- colorQuantile("Reds", value_est)    

  nepal_districts$objects$nepal$geometries$properties$value <- lapply(nepal_districts$objects$nepal$geometries$properties$value, function(feat) {
    feat$properties$style <- list(
      fillColor = pal(
        feat$properties$value / max(1, feat$properties$value)
      )
    )
    feat
    }
  )
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldPhysical) %>%
      addProviderTiles(providers$Stamen.TonerHybrid) %>% #Find a better layer so that there was less clutter
      addTopoJSON(nepal_districts) %>% #shading needs to occur according to # infected
      addGeoJSON(nepal_HQ) %>% #how do I add labels and marker information? # how do I add the ID table inforation to the marker information # change this to add markers
      # So this was supposed to add labels but I'm having trouble finding lat longs for the centrum of each district. I wonder if there's a way to find a center of a polygon defined by the json. Then transform and scale that value so it can be plotted on a map. One for later I think.
      # In the meantime I'll just leave the HQ markers in there for demonstration purposes
      
      setView(84.1240, 28.3949,  zoom = 7) %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))

  })
  
})
