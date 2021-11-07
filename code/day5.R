# Tutorial: https://www.r-bloggers.com/2018/11/accessing-openstreetmap-data-with-r/
pacman::p_load(osmdata, tidyverse, sf, ggmap, showtext)

font_add_google(name = "Ubuntu")

#### Get data ------------------------------------------------------------------

q <- getbb("Mexico City") %>% #Cafes en la CDMX
      opq() %>%
      add_osm_feature("amenity", "cafe")

cafe <- osmdata_sf(q)

cdmx_map <- get_map(getbb("Mexico City"), 
                    source = "osm",
                    color = "bw",
                    force = TRUE
)

#### Map -----------------------------------------------------------------------
showtext_auto()
map <- 
      ggmap(cdmx_map) +
      geom_sf(data = cafe$osm_points,
              inherit.aes = FALSE,
              colour = "#7f4f24",
              alpha = .5,
              size = 1) +
      labs(
            title = "Cafés en la Ciudad de México",
            caption = "Según datos registrados en OSM\ntwitter: @AlBRzez") +
      theme_void() +
      theme(
            plot.title = element_text(family = "Ubuntu",
                                          face = "bold", color = "#38302e",
                                          size = 16, hjust = .5),
            plot.caption = element_text(family = "Ubuntu",
                                        color = "#6f6866",
                                        size = 12) 
      ) 

png(file = "../maps/day4.png", width = 500, height = 500)
map
dev.off()   

