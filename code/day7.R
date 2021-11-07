pacman::p_load(sf, tidyverse, showtext)
font_add_google(name = "Ubuntu")

#### Get data ------------------------------------------------------------------

load("../data/day7/shape_areas_verdes.rda") #Shape generado 
# con base en el csv de datos abiertos CDMX 
# https://datos.cdmx.gob.mx/dataset/cdmx_areas_verdes_2017
cdmx <- st_read("../data/shape_alcaldias/shape_alcaldias.shp")


#### ---------------------------------------------------------------------------
parques <- 
      shape_areas_verdes %>% 
      filter(categoria %in% c("Parques, arboledas y alamedas",
                              "Plazas y jardines"))
showtext_auto()
map <- 
      ggplot() +
      geom_sf(data = cdmx, aes(geometry = geometry),
              fill = "#f5f5f2") +
      geom_sf(data = parques, aes(geometry = geometry),
              fill = "#606c38", color = "#606c38") +
      labs(
            title = "Parques, arboledas, alamedas,\nplazas y jardines",
            subtitle = "en la CDMX",
            fill = "CI iniciadas",
            caption = "Fuente:https://datos.cdmx.gob.mx/\ntwitter: @AlBRzez") +
      theme_void() +
      theme(
            plot.background = element_rect(fill = "#f5f5f2", color = "#f5f5f2"),
            panel.background = element_rect(fill = "#f5f5f2", color = "#f5f5f2"),
            legend.background = element_rect(fill = "#f5f5f2", color = "#f5f5f2"),
            
            plot.title = element_text(size = 18, hjust = 0.5, color = "#432818", 
                                      margin = margin(b = -0.1, t = 0.4, l = 2, 
                                                      unit = "cm"),
                                      family = "Ubuntu",),
            plot.subtitle = element_text(size = 16, hjust = 0.5, color = "#99582a", 
                                         margin = margin(b = -0.1, t = 0.43, l = 2, 
                                                         unit = "cm"),
                                         family = "Ubuntu",),
            plot.caption = element_text(size = 12, color = "#99582a", 
                                         margin = margin(b = 0.3, r = -99, unit = "cm"),
                                         family = "Ubuntu",)
      ) 

png(file = "../maps/day7.png", width = 600, height = 600)
map
dev.off()   

