pacman::p_load(tidyverse, sf, showtext, )
font_add_google(name = "Ubuntu")

path = "../data/day2/"
cdmx <- st_read("../data/shape_alcaldias/shape_alcaldias.shp")

metro <- st_read(paste0(path, "stcmetro_shp/STC_Metro_lineas_utm14n.shp"))
cablebus <- st_read(paste0(path, "ste_cablebus_shp/STE_Cablebus_lineas_utm14n.shp"))

tren_ligero <- st_read(paste0(path, "ste_tren_ligero_shp/STE_TrenLigero_linea_utm14n.shp"))
trolebus <- st_read(paste0(path, "ste_trolebus_shp/STE_Trolebus_lineas_utm14n.shp"))
rtp <- st_read(paste0(path, "rtp_shp/RTP_Rutas.shp"))
metrobus <- st_read(paste0(path, "mb_shp/Metrobus_lineas_rutas_utm14n.shp"))


###############


colores <- c("Cablebus" = "#4ac0df",
             "Metro" = "#e9540d",
             "Concesionado" = "black",
             "Trolebús y\ntren ligero" = "#005aa7",
             "RTP" = "#76b729",
             "Metrobús" = "#ce122d")
showtext_auto()

map <- 
      ggplot() +
      geom_sf(data = cdmx, aes(geometry = geometry),
              fill = "white") +
      # geom_sf(data = consecionado,
      #         aes(geometry = geometry, color = "Metrobús"))
      geom_sf(data = rtp, 
              aes(geometry = geometry, 
                  color = "RTP"),
              alpha = .4, size = .7) +
      geom_sf(data = metro, 
              aes(geometry = geometry, color = "Metro"),
              alpha = .7, size = 1) +
      geom_sf(data = metrobus, 
              aes(geometry = geometry, color = "Metrobús"),
              alpha = .7, size = 1) +
      geom_sf(data = tren_ligero, 
              aes(geometry = geometry, 
                  color = "Trolebús y\ntren ligero"),
              alpha = .7, size = 1) +
      geom_sf(data = trolebus, 
              aes(geometry = geometry, 
                  color = "Trolebús y\ntren ligero"),
              alpha = .7, size = 1) +
      geom_sf(data = cablebus, 
              aes(geometry = geometry, color = "Cablebus"),
              alpha = .7, size = 1) +
      scale_color_manual(values = colores) +
      labs(
            title = "Líneas de transporte público",
            subtitle = "En la CDMX",
            color = "Tipo de transporte",
            caption = "Fuente:https://datos.cdmx.gob.mx/\ntwitter: @AlBRzez"
            ) +
      theme_void() +
      theme(
            legend.position = c(1, .8),
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
      guides(override.aes = list(size = 3,
                                 alpha = 1))


png(file = "../maps/day2.png", width = 500, height = 500)
map
dev.off()  
