library(tidyverse)
library(shiny)
library(sf)
library(scales)
library(RColorBrewer)
library(leaflet)
library(rmapshaper)

dat <- read.csv('data/master-df_final.csv')
clusters <- read.csv('data/cluster.csv')
clust.name <- read.csv('data/clus-new-order-typ.csv')
inddat <- read.csv('data/residual-ind-vals-LV9-final.csv')
indlab <- read.csv('data/indicator-labels.csv')
#world_simp <- ms_simplify(World, keep_shapes = T)
#st_write(world_simp, 'data/UIA_World_Countries_Boundaries/UIA_World_Countries_Boundaries_simp.gpkg', overwrite = T, append = F)
World <- st_read('data/UIA_World_Countries_Boundaries/UIA_World_Countries_Boundaries_simp.gpkg')
World$Country <- recode(World$Country, `Russian Federation` = 'Russia',
                        `Brunei Darussalam` = 'Brunei',
                        Comoros = 'Comores',
                        `Timor-Leste` = 'East Timor',
                        Somalia = 'Federal Republic of Somalia',
                        `Côte d'Ivoire` = 'Ivory Coast',
                        Mauritius = 'Republic of Mauritius')
toc_dat <- read.csv('data/Book3.csv') %>% st_as_sf(coords = c('X', 'Y'), crs = 4326) #%>% mutate(Ecosystem = factor(Ecosystem))

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
  mutate('Enabling profile' = fine.clust)

world.clust <- World %>% 
  left_join(dat2, by = 'Country') %>% 
  filter(!is.na(pal.fine))

# colour palette

palset <- brewer.pal(9, 'YlGnBu')
palset <- palset[c(2,3,5,6,8,9)]

pal <- colorFactor( # colour palette for blue forest projects
  palette = palset,
  domain = world.clust$`Enabling profile`)

pal2 <- colorFactor( # colour palette for blue forest projects
  palette = c('deeppink4', 'darkorange3'),
  domain = toc_dat$Ecosystem)

# pop-ups

pop_up <- st_drop_geometry(toc_dat) %>% 
  mutate(popup = paste0("<span style='font-size: 120%'><strong>", Ecosystem ,"</strong></span><br/>",
                        "<strong>", "Enabling profile: ", "</strong>", Enabling.profile, 
                        "<br/>", 
                        "<strong>", "Theory of change: ", "</strong>", Implementation.pathway,
                        "<br/>", 
                        "<strong>", "Case study example: ", "</strong>", Case.study)) %>% 
  pull(popup)

pop_up2 <- st_drop_geometry(world.clust) %>% 
  mutate(popup = paste0("<span style='font-size: 120%'><strong>", Country ,"</strong></span><br/>",
                        "<strong>", "Enabling profile: ", "</strong>", `Enabling profile`)) #%>% 
  #pull(popup)

# vector of profile/country names

country <- c('Globe','Enabling profile 1', 'Enabling profile 2', 'Enabling profile 3',
             'Enabling profile 4', 'Enabling profile 5', 'Enabling profile 6',
             sort(as.character(unique(world.clust$Country))))
