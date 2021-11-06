pacman::p_load(sf, tidyverse, showtext, lubridate, viridis, ggtext)
font_add_google(name = "Ubuntu")

#### Get data ------------------------------------------------------------------
original <- read_csv("../data/day4_carpetas_completa_septiembre_2021.csv")
# Descargado de https://datos.cdmx.gob.mx/dataset/carpetas-de-investigacion-fgj-de-la-ciudad-de-mexico
# El 3 de noviembre, 2021

alcaldias <- c(
      "ALVARO OBREGON", "AZCAPOTZALCO", "BENITO JUAREZ",
      "COYOACAN", "CUAJIMALPA DE MORELOS", "CUAUHTEMOC", "GUSTAVO A MADERO",
      "IZTACALCO", "IZTAPALAPA", "LA MAGDALENA CONTRERAS",
      "MIGUEL HIDALGO", "MILPA ALTA", "TLAHUAC", "TLALPAN",
      "VENUSTIANO CARRANZA", "XOCHIMILCO"
)

cdmx <- st_read("../data/shape_alcaldias/shape_alcaldias.shp")

##### --------------------------------------------------------------------------
### Transform crs
cdmx <- 
      cdmx %>% 
      st_transform(6372)

## Geo carpetas
ci_geo <- 
      original %>% 
      filter(ao_inicio == 2021 & alcaldia_hechos %in% alcaldias &
                   categoria_delito != "HECHO NO DELICTIVO") %>% 
      drop_na(longitud) %>% 
      st_as_sf(coords = c("longitud", "latitud"), 
               crs = 4326, agr = "constant", remove = FALSE) %>% 
      st_transform(6372) 


## Grid de la CDMX
cdmx_grid <- 
      st_make_grid(cdmx, cellsize = 1000, square = FALSE)

## Conservar solo lo que intersecta

final_grid <- 
      st_as_sf(st_intersection(cdmx_grid, cdmx))

final_grid <- 
      st_as_sf(final_grid) %>% 
      mutate(id = 1:nrow(.))

## Union de grid y ci
grid_join <-
      st_join(final_grid, ci_geo, left = TRUE) %>% 
      mutate(ci = ifelse(!is.na(delito), 1, 0))

## Identificar ci por medida
ci_grid <- 
      grid_join %>% 
      st_drop_geometry() %>% 
      group_by(id) %>% 
      summarise(ci = sum(ci)) %>% 
      replace_na(list(ci = 0))


full_grid <- 
      final_grid %>% 
      left_join(ci_grid) %>% 
      st_transform(crs = 4326)

## Plot
showtext_auto()
map <- 
      ggplot(full_grid) +
      geom_sf(aes(fill = ci), size = 0.1, color = "gray15") +
      scale_fill_viridis(
            option = "B",
            trans = "log",
            na.value = "gray75",
            breaks = c(1, 10, 50, 100, 500, 1000)
      ) +
      labs(
            title = "Carpetas de investigación iniciadas por km<sup>2</sup>",
            subtitle = "en la CDMX, según lugar de hechos",
            fill = "CI iniciadas",
            caption = "Fuente:https://datos.cdmx.gob.mx/\ntwitter: @AlBRzez") +
      theme_void() +
      theme(
            legend.position = c(1, .8),
            legend.title = element_text(family = "Ubuntu",
                                        color = "#6f6866", 
                                        size = 12),
            legend.text = element_text(family = "Ubuntu",
                                       color = "#6f6866", 
                                       size = 8, 
                                       face = "bold"),
            plot.title = element_markdown(family = "Ubuntu",
                                          face = "bold", color = "#38302e",
                                          size = 18, hjust = .5),
            plot.subtitle = element_text(family = "Ubuntu",
                                         color = "#38302e",
                                         size = 16,
                                         hjust = .5), 
            plot.caption = element_text(family = "Ubuntu",
                                        color = "#6f6866",
                                        size = 12) 
      ) 

png(file = "../maps/day4.png", width = 600, height = 600)
map
dev.off()   

