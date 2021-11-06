pacman::p_load(sf, tidyverse, showtext, lubridate, viridis)
font_add_google(name = "Ubuntu")

#### Get data ------------------------------------------------------------------
cdmx <- st_read("../data/shape_alcaldias/shape_alcaldias.shp")

cp <- st_read("../data/cp_cdmx/CP_09CDMX_v7.shp")

llamadas <- read_csv("../data/day3/base-integrales-011121.csv",
                     locale = locale(encoding = "latin1"))


#### ---------------------------------------------------------------------------
cp_cl <- 
      cp %>% 
      mutate(cp = as.character(d_cp)) %>% 
      st_transform(4326)

locatel <-
      llamadas %>% 
      janitor::clean_names() %>% 
      filter(servicio == "MÉDICO" & tematica_1 == "PSIQUIATRIA" &
                   ano_alta %in% c(2020, 2021)) %>%   
      mutate(
            cp = str_pad(cp_usuaria, 5, pad = "0")
      )

cp_df <- 
      locatel %>% 
      count(cp) %>% 
      right_join(cp_cl) #%>% 

#### Mapa ----------------------------------------------------------------------
showtext_auto()
map <- 
      ggplot() +
      geom_sf(data = cdmx, aes(geometry = geometry),
              fill = "black", color = "gray60") +
      geom_sf(data = cp_df, 
              aes(geometry = geometry,
                  fill = n), color = "gray60") +
      labs(
            title = "Llamadas a LOCATEL en la CDMX\nPor temas de psiquiatría",
            subtitle = "Años 2020 y 2021",
            fill = "Llamadas",
            caption = "Fuente:https://datos.cdmx.gob.mx/\ntwitter: @AlBRzez\nDivisión por C.P.\nIncluyen llamadas hasta el 31 de octubre 2021") +
      scale_fill_viridis(
            na.value = "black") +
      theme_void() + 
      theme(
            legend.title = element_text(family = "Ubuntu",
                                        color = "gray60", 
                                        # color = "#6f6866", 
                                        size = 12),
            legend.text = element_text(family = "Ubuntu",
                                       color = "gray60", 
                                       # color = "#6f6866", 
                                       size = 10, face = "bold"),
            plot.title = element_text(family = "Ubuntu",
                                      face = "bold", color = "gray60",
                                      size = 18, hjust = .5),
            plot.subtitle = element_text(family = "Ubuntu",
                                         color = "gray60",
                                         size = 16,
                                         hjust = .5), 
            plot.caption = element_text(family = "Ubuntu",
                                        color = "gray60",
                                        size = 10,
                                        margin = margin(b = 0.3, r = -80, unit = "cm")) ,
            legend.position = "top",
            # plot.margin = margin(0, 0, 0, 0, "cm"),
            panel.border = element_blank(),
            plot.background = element_rect(fill = "black", color = NA), 
            legend.background = element_rect(fill = "black", color = NA), 
            panel.background = element_rect(fill = "black", color = NA)
      )

png(file = "../maps/day3.png", width = 600, height = 900)
map
dev.off()      

