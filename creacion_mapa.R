
# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, sf, sp, ggrepel, mapsPERU)

# Importar data -----------------------------------------------------------

## distritos ----
lista_dist <- readxl::read_excel("distritos.xlsx") |> 
              select(COD_DISTRITO = Ubigeo, ID_DISTRITO, CATEGORIA)
dist_vraem <- left_join(lista_dist, map_DIST, by = "COD_DISTRITO")

## regiones ----
region_vraem <- map_REG |> filter(grepl("Apu|Aya|Cus|Hua|Jun", REGION))

## rios ----
dir   <- paste0(getwd(),"/RIOS/") # directorio
files <- list.files(dir, pattern = "\\.shp$", full.names = T)
rios  <- lapply(files, st_read) 
rios  <- as.data.frame(do.call(rbind, rios))

remove(lista_dist, dir, files) # eliminar archivos temporales

# Creación del mapa -------------------------------------------------------

#mapa_vraem <- 
  ggplot() +
  ## mapa de distritos ----
  geom_sf(
    data = dist_vraem, 
    aes(geometry = geometry, fill = CATEGORIA),
    color = "gray60", 
    size = 4
  ) +
  scale_fill_manual(
    values = c("#F2F2F2","#F2EDA7"),
    name = NULL
  ) +
  ## enumerar distritos 
  # geom_text(
  #   data = dist_vraem,
  #   aes(geometry = geometry,
  #       coords_x,
  #       coords_y,
  #       group = NULL,
  #       label = ID_DISTRITO),
  #   size = 3
  # ) +
  ## mapa de regiones ----
  geom_sf(
    data = region_vraem,
    aes(geometry = geometry, group = REGION),
    fill = NA,
    color = "#262626",
    size = 0.5    
  ) +
  ## mapa de rios ----
  geom_sf(
    data = rios |> 
      filter(NOMBRE %in% c("Rio Ene","Rio Mantaro","Rio Apurimac")),
    aes(geometry = geometry, color = NOMBRE), 
  ) +
  scale_color_manual(
    values = c("#72F2EB","#9FC131","#04BF9D"),
    name = "Leyenda"                    
  ) +
  ## establecer tema ----
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", colour = "white"),
    legend.position = c(0,.18),
    legend.box.margin = margin(t=.2, b=.2, r=.3, l=.3,"cm"),
    legend.box.background = element_rect(linetype = 6, size = 0.5, colour = "gray50")
  ) +
  ## cambiar orden de leyenda
  guides(colour = guide_legend(order = 1), 
         fill   = guide_legend(order = 2)) +
  coord_sf(
    ylim = c(-13.54210, -10.66402),
    xlim = c(-75.10485, -73.00506) 
  ) 

# Recortar mapa ----

## determinar límites ----
st_as_sfc(st_bbox(dist_vraem$geometry))

## aplicar límites ----
mapa_vraem +
  coord_sf(
    ylim = c(-13.54210, -10.66402),
    xlim = c(-75.10485, -73.00506) 
  )

# Exportar mapa -----------------------------------------------------------
ggsave("new_mapa_vraem.jpg", width = 8, height = 7, device='tiff', dpi = 700)
