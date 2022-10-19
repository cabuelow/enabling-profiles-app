library(dplyr)
library(sf)
library(leaflet)
library(shiny)
library(rintrojs)

# load and wrangle all required components for app
source("load-data.R")

# navigation panel

navbarPage(
  title = div("Enabling profiles for coastal wetland conservation", id = 'nav'),
  tabPanel('Map',
           introjsUI(),
           div(class="outer",
               tags$head(
                 includeCSS("styles.css")
               ),
               tags$style(".leaflet-control-layers-overlays{color: black}"),
               leafletOutput("enabling_map", width="100%", height="100%"),
           
               absolutePanel(id = 'controls', top = 100, left = 30,
                            class = "panel panel-default",  
                            draggable = T,
                            introBox(data.hint = c('Countries are coloured according to the', "<span style='font-size: 100%'><strong>", 'Enabling profile', "</strong></span>", 'they belong to.',
                                                   "<br/>",
                                     "<br/>",
                                     'Enabling profiles represent distinct', "<span style='font-size: 100%'><strong>", 'socioeconomic and political conditions',"</strong></span>",'for facilitating mangrove, seagrass and saltmarsh conservation.',
                                     "<br/>",
                                     "<br/>", "<span style='font-size: 100%'><strong>",
                                     'Circles are case-studies', "</strong></span>", 'for how Enabling profiles can inform conservation implementation pathways (i.e., Theories of Change).',
                                     "<br/>", "<br/>",'Click on the circles to find out more.',
                                     "<br/>", "<br/>",'Links to references are provided in the', "<span style='font-size: 100%'><strong>", 'Reference', "</strong></span>", 'tab above.')),
                             selectInput("var", label = h5("Choose an Enabling profile or Country"), 
                                         choices = country, 
                                         selected = 'Globe')
               ), # end absolute panel
           ), # end div
           tags$div(id="cite",
                    tags$em('This map was developed with support from the 
                            Global Wetlands Project'))
  ), # end tabpanel
  tabPanel('References',
               tags$div("Brodie, G., Holland, E., N’Yeurt, A. D. R., Soapi, K., & Hills, J. (2020). Seagrasses and seagrass habitats in	Pacific small island developing states: Potential loss of benefits via human disturbance and climate	change. Marine Pollution Bulletin, 160. Available at: ", tags$a("https://doi.org/10.1016/j.marpolbul.2020.111573", href="https://doi.org/10.1016/j.marpolbul.2020.111573", target="_blank", .noWS='outside'), '.'
               ), tags$br(),
               tags$div("de los Santos, C. B., Krause-Jensen, D., Alcoverro, T., Marbà, N., Duarte, C. M., van Katwijk, M. M., Pérez, M., Romero, J., Sánchez-Lizaso, J. L., Roca, G., Jankowska, E., Pérez-Lloréns, J. L., Fournier, J.,	Montefalcone, M., Pergent, G., Ruiz, J. M., Cabaço, S., Cook, K., Wilkes, R. J., … Santos, R. (2019). Recent trend reversal for declining European seagrass meadows. Nature Communications, 10(1). Available at: ", tags$a("https://doi.org/10.1038/s41467-019-11340-4", href="https://doi.org/10.1038/s41467-019-11340-4", target="_blank", .noWS='outside'), '.'
               ),tags$br(),
               tags$div("Duke, N. C., Bell, A. M., Pederson, D. K., Roelfsema, C. M., & Nash, S. B. (2005). Herbicides implicated as the 	cause of severe mangrove dieback in the Mackay region, NE Australia: Consequences for marine plant habitats of the GBR World Heritage Area. Marine Pollution Bulletin, 51(1–4), 308–324. Available at: ", tags$a("https://doi.org/10.1016/j.marpolbul.2004.10.040", href="https://doi.org/10.1016/j.marpolbul.2004.10.040", target="_blank", .noWS='outside'), '.'
               ),tags$br(),
               tags$div("King, J., Alexander, F., & Brodie, J. (2013). Regulation of pesticides in Australia: The Great Barrier Reef as a case 	study for evaluating effectiveness. Agriculture, Ecosystems and Environment, 180, 54–67. Available at: ", tags$a("https://doi.org/10.1016/j.agee.2012.07.001", href="https://doi.org/10.1016/j.agee.2012.07.001", target="_blank", .noWS='outside'), '.'
               ), tags$br(),
               tags$div("Kletou, D., Kleitou, P., Savva, I., Attrill, M. J., Antoniou, C., & Hall-Spencer, J. M. (2018). Seagrass recovery after fish farm relocation in the eastern Mediterranean. Marine Environmental Research, 140, 221–233. Available at: ", tags$a("https://doi.org/10.1016/j.marenvres.2018.06.007", href="https://doi.org/10.1016/j.marenvres.2018.06.007", target="_blank", .noWS='outside'), '.'
               ), tags$br(),
               tags$div("Long, S., Jones, P. J. S., Randriana, Z., & Hadj-Hammou, J. (2021). Governance analysis of a community managed small-scale crab fishery in Madagascar: novel use of an empirical framework. Marine Policy, 127. Available at: ", tags$a("https://doi.org/10.1016/j.marpol.2017.11.022", href="https://doi.org/10.1016/j.marpol.2017.11.022", target="_blank", .noWS='outside'), '.'
               ),tags$br(),
               tags$div("Shilland, R., Grimsditch, G., Ahmed, M., Bandeira, S., Kennedy, H., Potouroglou, M., & Huxham, M. (2021). A question of standards: Adapting carbon and other PES markets to work for community seagrass conservation. Marine Policy, 129. Available at: ", tags$a("https://doi.org/10.1016/j.marpol.2021.10457", href="https://doi.org/10.1016/j.marpol.2021.10457", target="_blank", .noWS='outside'), '.'
               ), tags$br(),
               tags$div("Smith, A. H., & Berkes, F. (1993). Community-based use of mangrove resources in st. lucia. International Journal of 	Environmental Studies, 43(2–3), 123–131. Available at: ", tags$a("https://doi.org/10.1080/00207239308710819", href="https://doi.org/10.1080/00207239308710819", target="_blank", .noWS='outside'), '.'
               ), tags$br(),
               tags$div("Winterwerp, J. C., Albers, T., Anthony, E. J., Friess, D. A., Mancheño, A. G., Moseley, K., Muhari, A., Naipal, S., 	Noordermeer, J., Oost, A., Saengsupavanich, C., Tas, S. A. J., Tonneijck, F. H., Wilms, T., van Bijsterveldt, C., van Eijk, P., van Lavieren, E., & van Wesenbeeck, B. K. (2020). Managing erosion of mangrove-mud coasts with permeable dams – lessons learned. Ecological Engineering, 158. Available at: ", tags$a("https://doi.org/10.1080/00207239308710819", href="https://doi.org/10.1080/00207239308710819", target="_blank", .noWS='outside'), '.'
               ), tags$br(),
               tags$div("WorldFish. (2018). Conservation strategy for dugongs and seagrass habitats in Solomon Islands. Penang, Malaysia: WorldFish. Strategy 2018-22."
               ),
           ) # end tabpanel
  ) # end nav bar
