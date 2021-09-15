library(tmap)
library(sf)
library(tidyverse)
library(shiny)
library(shinythemes)
library(RColorBrewer)
library(bslib)
library(scales)
shinyOptions(bslib = TRUE)
thematic::thematic_shiny()

# data

dat <- read.csv('data/master-df_final.csv')
clusters <- read.csv('data/cluster.csv')
clust.name <- read.csv('data/clus-new-order-typ.csv')
inddat <- read.csv('data/residual-ind-vals-LV9-final.csv')
indlab <- read.csv('data/indicator-labels.csv')
World <- st_read('data/UIA_World_Countries_Boundaries/UIA_World_Countries_Boundaries.shp')
World$Country <- recode(World$Country, `Russian Federation` = 'Russia',
                  `Brunei Darussalam` = 'Brunei',
                  Comoros = 'Comores',
                  `Timor-Leste` = 'East Timor',
                  Somalia = 'Federal Republic of Somalia',
                  `Côte d'Ivoire` = 'Ivory Coast',
                  Mauritius = 'Republic of Mauritius')

# rescale indicator data

max <- min(apply(inddat, 2, max))
min <- max(apply(inddat, 2, min))

inddat <- inddat %>% 
    mutate_all(funs(rescale(., to = c(min, max))))

# join country names to indicator data

inddat$ISO_SOV1 <- dat$ISO_SOV1

dtemp <- clusters %>% 
    rename(cluster = Cluster) %>% 
    right_join(inddat, by = 'ISO_SOV1') %>% 
    left_join(clust.name, by = 'cluster') %>% 
    distinct() %>% 
    left_join(select(dat, ISO_SOV1, Country), by = 'ISO_SOV1') %>% 
    rename(Cluster = fine.clust) %>% 
    select(-c(cluster, pal.fine)) %>% 
    pivot_longer(cols = OECD_sc:ramsar.manage, names_to = 'indicator', values_to = 'val') %>% 
    left_join(indlab, by = 'indicator')

# get full spatial df with model results and merge by country 

dat2 <- dat %>% 
    left_join(clusters, by = 'ISO_SOV1') %>% 
    distinct() %>% 
    dplyr::rename(iso_a3 = ISO_SOV1) %>% 
    dplyr::rename(cluster = Cluster) %>% 
    left_join(clust.name, by = 'cluster') %>% 
    dplyr::rename('Enabling profile' = fine.clust)

world.clust <- World %>% 
    left_join(dat2, by = 'Country') %>% 
    filter(!is.na(pal.fine))

# get color palette

pal <- brewer.pal(9, 'YlGnBu')
pal <- pal[c(2,3,5,6,8,9)]

# vector of profile/country names

country <- c('Globe',1:6,sort(as.character(unique(world.clust$Country))))

# Define UI for application

ui <- fluidPage(theme=shinytheme("darkly"),
                
                fluidRow(
                    column(12,
                           h1("Enabling Coastal Wetland Conservation", align = 'center'))),
                fluidRow(
                    column(12,
                           h4("Countries belong to Enabling Profiles with distinct policy, regulatory and engagement mechanisms for conservation", align = 'center'))),
                fluidRow(column(12, align = 'center',
                                conditionalPanel("$('#map').hasClass('recalculating')", 
                                                 tags$div('Loading ... ', style = "font-size:25px;")))),
                fluidRow(
                    column(12, align = 'center', style='padding-top:10px;',
                         selectInput("var", label = h4("Choose a Profile or Country"), 
                                    choices = country, 
                                    selected = 'Globe'))),
                fluidRow(
                    column(6, offset = 1, style='padding-top:15px;',
                           tmapOutput('map')), width = 800,
                    column(5, align = 'center', style='margin-left:-90px;',
                            h4('Relative status of enabling mechanisms (high to low)'),
                    fluidRow(
                        column(12, align = 'center',style='margin-left:-20px;',
                        plotOutput('ind.rank', width = 360, height = 370))))))

# Define server logic

server <- function(input, output) {

    output$map <- renderTmap({
        if(input$var == 'Globe'){
        tmap_mode('view')
        tm_shape(world.clust[,-1]) +
            tm_fill(col = 'Enabling profile', style = 'cat', palette = pal, alpha = 0.7) +
            tm_view(view.legend.position = c('left', 'bottom')) +
            tm_basemap(leaflet::providers$CartoDB.DarkMatter)
        }
        else if(input$var %in% c(1:6)){
        tmap_mode('view')
        pl <- filter(world.clust, `Enabling profile` == input$var)
        tm_shape(pl[,c(2,31)]) +
            tm_fill(col = pal[pl$`Enabling profile`[1]], alpha = 0.7) +
            tm_view(view.legend.position = c('left', 'bottom')) +
            tm_basemap(leaflet::providers$CartoDB.DarkMatter)
        }
        else{
        if(input$var != 'Fiji'){
        tmap_mode('view')
        pl <- filter(world.clust, Country == input$var)
        tm_shape(pl[,c(2,31)]) +
            tm_fill(col = pal[pl$`Enabling profile`], alpha = 0.5) +
            tm_view(view.legend.position = c('left', 'bottom')) +
            tm_basemap(leaflet::providers$CartoDB.DarkMatter)}
            else{
                tmap_mode('view')
                pl <- filter(world.clust, Country == input$var) %>% 
                    st_crop(xmin = 177, ymin = -19.16278, xmax = 179.99999, ymax = -16.15347)
                tm_shape(pl[,c(2,31)]) +
                    tm_fill(col = pal[pl$`Enabling profile`], alpha = 0.5) +
                    tm_view(view.legend.position = c('left', 'bottom') +
                    tm_basemap(leaflet::providers$CartoDB.DarkMatter))
            }
        }
    })
    
    output$ind.rank <- renderPlot({
    if(input$var == 'Globe'){
         p <- dtemp %>% 
            group_by(Category, label) %>% 
            summarise(val = mean(val)) %>% 
            arrange(desc(val))
         p$score <- rescale(p$val)
         p$label <- factor(p$label, levels = paste(p$label))
         ggplot(p) +
             geom_bar(aes(x = forcats::fct_rev(label), y = score, fill = Category), stat = 'identity') +
             geom_text(aes(x = forcats::fct_rev(label), y = score,label = label, hjust = 'left'), size = 5) +
             ylim(0, 3) +
             coord_flip() +
             scale_fill_manual(values = brewer.pal(8, "Accent")[1:3]) +
             theme_void() +
             theme(legend.title = element_blank(), legend.position = 'top', 
                   legend.text = element_text(size = 15, colour = 'white'))
          }
    else if(!input$var %in% c(1:6)){    
        p <- dtemp %>% 
            filter(Country == input$var) %>% 
            arrange(desc(val))
        p$score <- rescale(p$val)
        p$label <- factor(p$label, levels = paste(p$label))
        ggplot(p) +
            geom_bar(aes(x = forcats::fct_rev(label), y = score, fill = Category), stat = 'identity') +
            geom_text(aes(x = forcats::fct_rev(label), y = score,label = label, hjust = 'left'), size = 5) +
            ylim(0, 3) +
            coord_flip() +
            scale_fill_manual(values = brewer.pal(8, "Accent")[1:3]) +
            theme_void() +
            theme(legend.title = element_blank(), legend.position = 'top', 
                  legend.text = element_text(size = 15, colour = 'white'))
        }
        else{
            p <- dtemp %>% 
                filter(Cluster == input$var) %>% 
                group_by(Cluster, indicator, label, Category) %>% 
                summarise(val = median(val)) %>% 
                arrange(desc(val))
            p$score <- rescale(p$val)
            p$label <- factor(p$label, levels = paste(p$label))
            ggplot(p) +
                geom_bar(aes(x = forcats::fct_rev(label), y = score, fill = Category), stat = 'identity') +
                geom_text(aes(x = forcats::fct_rev(label), y = score,label = label, hjust = 'left'), size = 5) +
                ylim(0, 3) +
                coord_flip() +
                scale_fill_manual(values = brewer.pal(8, "Accent")[1:3]) +
                theme_void() +
                theme(legend.title = element_blank(), legend.position = 'top', 
                      legend.text = element_text(size = 15, colour = 'white'))
            }
    })
}

# Run the application 

shinyApp(ui = ui, server = server)
