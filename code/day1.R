pacman::p_load(sf, tidyverse, showtext)
font_add_google(name = "Ubuntu")

cdmx <- st_read("../data/shape_alcaldias/shape_alcaldias.shp")

centros <- 
      read_csv("../data/day1_servicios-de-atencion-a-violencia-contra-las-mujeres.csv")

geo_centros <- 
      st_as_sf(centros %>% 
                     drop_na(lon), coords = c("lon", "lat"), 
               crs = 4326, agr = "constant")


geo_centros <- 
      geo_centros %>% 
      # filter(id_sede != "FGJ-074") %>% #Está mal la coordenada
      replace_na(list(intervencion_en_crisis = "No"))

puntos_centros <- st_intersection(geo_centros, cdmx)


showtext_auto()
map <- 
      
      ggplot() +
      geom_sf(data = cdmx, aes(geometry = geometry),
              fill = "white", color = "gray30") +
      geom_sf(data = pun, aes(geometry = geometry,
                              color = intervencion_en_crisis),
              size = 2) +
      labs(
            title = "Servicios de atención a violencia\ncontra las mujeres",
            subtitle = "En la CDMX",
            color = "Cuenta con servicio de\nintervención en crisis",
            caption = "Fuente:https://datos.cdmx.gob.mx/\ntwitter: @AlBRzez") +
      scale_color_manual(values = c("#bbb193", "#28536b")) +
      theme_void() + 
      theme(
            legend.position=c(1, .8),
            legend.title = element_text(family = "Ubuntu",
                                        color = "#6f6866", 
                                        size = 12),
            legend.text = element_text(family = "Ubuntu",
                                       color = "#6f6866", 
                                       size = 10, face = "bold"),
            plot.title = element_text(family = "Ubuntu",
                  face = "bold", color = "#38302e",
                  size = 18, hjust = .5),
            plot.subtitle = element_text(family = "Ubuntu",
                                         color = "#38302e",
                                         size = 16,
                                         hjust = .5), 
            plot.caption = element_text(family = "Ubuntu",
                                        color = "#6f6866",
                                        size = 12) 
            ) +
      guides(color = guide_legend(override.aes = list(size = 3)))

png(file = "../maps/day1.png", width = 500, height = 500)
map
dev.off()      
