pacman::p_load(sf, tidyverse, showtext, lubridate, ggtext)
font_add_google(name = "Ubuntu")


clues <- readxl::read_xlsx("../data/day6_ESTABLECIMIENTO_SALUD_202109.xlsx")

hosp_cdmx <- 
      clues %>% 
      janitor::clean_names() %>% 
      filter(clave_de_la_entidad == "09")

salud_mental <- 
      hosp_cdmx %>% 
      filter(clave_scian %in% c(624191, 621422, 622211, 623222))

especializados <- 
      hosp_cdmx %>% 
      filter(clave_de_la_institucion == "SSA" & 
                   nombre_de_tipologia %in% c("HOSPITAL ESPECIALIZADO", "HOSPITAL PSIQUIÁTRICO")) %>% 
      drop_na(longitud) %>% 
      st_as_sf(coords = c("longitud", "latitud"), 
               crs = 4326, agr = "constant", remove = FALSE) 

showtext_auto()

map <- 
      ggplot() +
      geom_sf(data = cdmx, aes(geometry = geometry),
              fill = "white", color = "gray30") +
      geom_sf(data = especializados, 
              aes(geometry = geometry),
              color = "#ff4f4f", size = 2) +
      labs(
            title = "Hospitales de especialidad y psiquiátricos",
            subtitle = "En la CDMX",
            caption = "Fuente: Dirección General de Información en Salud (DGIS)\ntwitter: @AlBRzez") +
      theme_void() + 
      theme(

            plot.title = element_text(family = "Ubuntu",
                                      face = "bold", color = "#38302e",
                                      size = 18, hjust = .5),
            plot.subtitle = element_text(family = "Ubuntu",
                                         color = "#38302e",
                                         size = 16,
                                         hjust = .5), 
            plot.caption = element_text(family = "Ubuntu",
                                        color = "#6f6866",
                                        size = 11) 
      ) 

png(file = "../maps/day6.png", width = 500, height = 500)
map
dev.off()      

