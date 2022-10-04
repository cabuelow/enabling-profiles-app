# server

library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(leaflet.extras2)

# server logic

function(input, output, session) {

  output$enabling_map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMapPane('layer1', zIndex = 410) %>%
      addMapPane('layer2', zIndex = 460) %>%
      addCircleMarkers(group = "Case study Theories of Change",
                       data = toc_dat,
                       color = ~pal2(Ecosystem),
                       weight = 1,
                       opacity = 1,
                       fillOpacity = 0.8,
                       radius = 5,
                       popup= pop_up,
                       options = pathOptions(pane = "layer2")) %>%
      addLegend("bottomright",
                #data = toc_dat,
                colors =rgb(t(col2rgb(palset)) / 255),
                #values = ~Ecosystem,
                labels = c('Enabling profile 1', 'Enabling profile 2', 'Enabling profile 3',
                           'Enabling profile 4', 'Enabling profile 5', 'Enabling profile 6'),
                #title = "Enabling profiles",
                opacity = 1) %>% 
      addLegend("bottomright",
                #data = toc_dat,
                colors =rgb(t(col2rgb(c('deeppink4', 'darkorange3'))) / 255),
                #values = ~Ecosystem,
                labels = c('Mangroves', 'Seagrass'),
                #title = "Coastal wetland ecosystem",
                opacity = 1, group = "Case study Theories of Change") %>% 
      addLayersControl(
        overlayGroups = c("Case study Theories of Change"),
        options = layersControlOptions(collapsed = FALSE)) #%>% 
      #hideGroup("Case study Theories of Change")
  }) # end render leaflet
  
  # updated map based on user inputs
  
  observe({
    if(input$var == 'Globe'){
      bounds <- unname(st_bbox(world.clust))
      leafletProxy('enabling_map') %>% 
        addSpinner() %>%
        startSpinner(options = list("lines" = 7, "length" = 20)) %>%
        flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>% 
        clearGroup(c('basepoly', 'countrypoly', 'profilepoly')) %>% 
        addPolygons(
          group = 'basepoly',
          data = world.clust,
          color = ~pal(`Enabling profile`),
          weight = 0.9,
          #opacity = 1,
          popup = pop_up2$popup,
          fillOpacity = 0.8,
          options = pathOptions(pane = "layer1")) %>% stopSpinner()
    }else if(input$var %in% as.character(unique(world.clust$Country))){
      bounds <- unname(st_bbox(st_buffer((world.clust %>% filter(Country == input$var)), 1)))
      leafletProxy('enabling_map') %>% 
        addSpinner() %>%
        startSpinner(options = list("lines" = 7, "length" = 20)) %>%
        clearGroup(c('basepoly', 'countrypoly', 'profilepoly')) %>% 
        addPolygons(
          group = 'countrypoly',
          data = filter(world.clust, Country == input$var),
          color = ~pal(`Enabling profile`),
          weight = 0.9,
          #opacity = 1,
          popup = filter(pop_up2, Country == input$var)$popup,
          fillOpacity = 0.8,
          options = pathOptions(pane = "layer1")) %>% 
        flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>% stopSpinner()
    }else if(input$var %in% c('Enabling profile 1', 'Enabling profile 2', 'Enabling profile 3',
                                  'Enabling profile 4', 'Enabling profile 5', 'Enabling profile 6')){
      profile <- strsplit(input$var, '')[[1]][18]
      bounds <- unname(st_bbox(st_buffer((world.clust %>% filter(`Enabling profile` == profile)), 1)))
      leafletProxy('enabling_map') %>%
        addSpinner() %>%
        startSpinner(options = list("lines" = 7, "length" = 20)) %>%
        flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>% 
        clearGroup(c('basepoly', 'countrypoly', 'profilepoly')) %>% 
        addPolygons(group = 'profilepoly',
                    data = filter(world.clust, `Enabling profile` == profile),
                    color = ~pal(`Enabling profile`),
                    weight = 0.9,
                    #opacity = 1,
                    popup = filter(pop_up2, `Enabling profile` == profile)$popup,
                    fillOpacity = 0.8,
                    options = pathOptions(pane = "layer1")) %>% stopSpinner()
  }
    }) # end observe
  
} #end server