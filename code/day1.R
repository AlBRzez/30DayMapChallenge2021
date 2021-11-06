pacman::p_load(sf, tidyverse, showtext, leaflet, htmltools)
font_add_google(name = "Ubuntu")

#### Get data ------------------------------------------------------------------
cdmx <- st_read("../data/shape_alcaldias/shape_alcaldias.shp")

centros <- 
      read_csv("../data/day1_servicios-de-atencion-a-violencia-contra-las-mujeres.csv")

#### ---------------------------------------------------------------------------
geo_centros <- 
      st_as_sf(centros %>% 
                     drop_na(lon), coords = c("lon", "lat"), 
               crs = 4326, agr = "constant")

geo_centros <- 
      geo_centros %>% 
      replace_na(list(intervencion_en_crisis = "No"))

## Conservar puntos que están dentro de la CDMX
puntos_centros <- st_intersection(geo_centros, cdmx)

#### Mapa estatico -------------------------------------------------------------

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
      guides(color = guide_legend(override.aes = list(size = 3)))

png(file = "../maps/day1.png", width = 500, height = 500)
map
dev.off()      


#### Mapa interactivo ----------------------------------------------------------
sedes_intervencion <- 
      geo_centros %>% 
      filter(intervencion_en_crisis == "Sí")

labs_sedes_interv <- 
      sprintf(
            "<strong>%s<br/>%s<br/>%s<br/>%s",
            paste(sedes_intervencion$nombre_de_la_sede),
            paste(sedes_intervencion$alcaldia),
            paste(sedes_intervencion$colonia),
            paste(sedes_intervencion$calle)
      ) %>% 
   lapply(HTML)

sedes_no_intervencion <- 
   geo_centros %>% 
   filter(intervencion_en_crisis == "No")


labs_sedes_no_interv <- 
      sprintf(
         "<strong>%s<br/>%s<br/>%s<br/>%s",
            paste(sedes_no_intervencion$nombre_de_la_sede),
            paste(sedes_no_intervencion$alcaldia),
            paste(sedes_no_intervencion$colonia),
            paste(sedes_no_intervencion$calle)
      ) %>% 
      lapply(HTML)


mapa_int <- 
   leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(-99.136361, 19.32, 10) %>%
      addPolygons(
            data = cdmx,
            color = "black",
            weight = 1.2,
            fillOpacity =  0
      ) %>% 
      addCircleMarkers(
            data = sedes_intervencion,
            color = "#28536b",
            fill = TRUE,
            fillOpacity = 1,
            stroke = F,
            popup = labs_sedes_interv,
            radius = 2,
            group = "Sedes con servicio de intervención en crisis"
      ) %>% 
      addCircleMarkers(
            data = sedes_no_intervencion,
            color = "#bbb193",
            fill = TRUE,
            fillOpacity = 1,
            stroke = F,
            popup = labs_sedes_no_interv,
            radius = 2,
            group = "Sedes sin servicio de intervención en crisis"
      ) %>% 
      addLayersControl(
            overlayGroups = c(
               "Sedes con servicio de intervención en crisis",
               "Sedes sin servicio de intervención en crisis"
            )
      ) %>% 
   addControl(tags$p(tags$style("p {color: black; font-size:30x; width:300px}"),
                     tags$b(paste0("Servicios de atención a violencia\ncontra las mujeres en la CDMX"))),
              position = "bottomleft")  %>% 
   addLegend(
      title = paste0("Cuenta con servicio de", br(), 
                     "intervención en crisis"),
      colors = c("#bbb193", "#28536b"),
      labels = c("No", "Si"))


htmlwidgets::saveWidget(mapa_int, "day1.html")
file.rename("day1.html",
            "../maps/day1.html")
