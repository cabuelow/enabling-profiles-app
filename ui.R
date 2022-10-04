library(dplyr)
library(sf)
library(leaflet)
library(shiny)

#TODO: add in menu for choosing Enabling profile, or zooming to a country only

# load and wrangle all required components for app
source("load-data.R")

# navigation panel

navbarPage(
  title = div("Enabling profiles for coastal wetland ecosystem conservation", id = 'nav'),
  
  tabPanel('Map',
           #introjsUI(),
           div(class="outer",
               tags$head(
                 includeCSS("styles.css"),
               
               absolutePanel(top = 100, right = 10, draggable = T,
                             selectInput("var", label = "Choose an Enabling profile or Country", 
                                         choices = country, 
                                         selected = 'Globe')
               )),# end absolute panel
               
               leafletOutput("enabling_map", width="100%", height="100%"),
               
               tags$style(".leaflet-control-layers-overlays{color: black}"),
           ), # end div
           tags$div(id="cite",
                    tags$em('This map was developed with support from the 
                            Global Wetlands Project'))
  ), # end tabpanel
) # end nav bar
