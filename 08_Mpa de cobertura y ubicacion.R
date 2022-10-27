library(sf)
library(raster)
library(ggplot2)
SurAmerica = st_read("01_SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica  ,
                           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Rio_Bagua = st_read("01_SHP/Rio_Bagua.geojson")  %>% st_as_sf()
Rio_Bagu  <- st_transform(Rio_Bagua ,
                           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))



Per           <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Amazonas       <- subset(Per, NAME_1  == "Amazonas")
Amazonas_box = st_as_sfc(st_bbox(Amazonas))

Per_pro        <- getData('GADM', country='Peru', level=2) %>% st_as_sf()
Amazonas_pro      <- subset(Per_pro, NAME_1  == "Amazonas")
Bagua          <- subset(Per_pro, NAME_2  == "Bagua")


library(elevatr)
elev = get_elev_raster(Bagua , z=11)
plot(elev)
Poligo_alt    <- crop(elev, Bagua)                           #
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Bagua)
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope")
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)

colores<- c("#015B26", "#3F8433", "#8BAB56", "#CFCF7D", "#C49450", "#AA5B32", "#872921")
cortes <- c(200, 500,1000,2000,3000,4000,5000, 6500)


Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")


library(rgbif)
Gonatodes_atricucullaris<- occ_search(scientificName="Gonatodes atricucullaris Noble, 1921")
Gonatodes        <- subset(Gonatodes_atricucullaris$data , scientificName == "Gonatodes atricucullaris Noble, 1921")
Gonatodes$image <- "04_PNG/reptil.png"

library(ggrepel)
library(ggforce)
library(grid)
library(png)
library(ggimage)
img <- readPNG("04_PNG/reptil.png", FALSE)
g <- rasterGrob(img, x = unit(0.8, "npc"),y = unit(0.3, "npc"), width = unit(0.2, "npc"))


SurA= ggplot()+

  geom_sf(data = Per , fill="gray", color="gray", size=0.05)+
  geom_sf(data = SurAmeric, fill="white", color="black", size=0.1)+
  geom_sf(data = Amazonas, fill="black", color="black", size=0.01)+
  geom_sf(data = Amazonas_box, fill=NA, color="red", size=0.1)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",

        panel.background = element_rect(fill = "#BFD1FF"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -60, y = 10, hjust = 0, vjust = 1,
           label = "Sur America",size = 3, family="serif", color =
             "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -80, y = -40, hjust = 0, vjust = 1,
           label = "Pacific ocean",size = 3, family="serif", color =
             "black",  fontface="italic", angle=90)+
  annotate(geom = "text", x = -55, y = -50, hjust = 0, vjust = 1,
           label = "Atlantic ocean",size = 3, family="serif", color =
             "black",  fontface="italic")+
  annotate(geom = "text", x = -75, y = -13, hjust = 0, vjust = 1,
           label = "Peru",size = 3, family="serif", color =
             "black",  fontface="italic")

SurA



Macro=ggplot()+
  geom_sf(data = SurAmeric, fill="white", color="black", size=0.01)+
  geom_sf(data = Per , fill="gray90", color="black", size=0.05)+
  geom_sf(data = Amazonas_pro, fill="gray", color="black", size=0.1)+
  geom_sf(data = Bagua, fill="black", color="black", size=0.01, alpha=0.4)+
  coord_sf(xlim = c(-79, -76.8 ), ylim = c(-6.987491 ,-2.984379)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",

        panel.background = element_rect(fill = "#BFD1FF"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -78.7, y = -3 , hjust = 0, vjust = 1,
           label = "Ecuador",size = 3, family="serif", color =
             "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -77, y = -4 , hjust = 0, vjust = 1,
           label = "Loreto",size = 3, family="serif", color =
             "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -78.7, y = -7 , hjust = 0, vjust = 1,angle=90,
           label = "Cajamarca",size = 3, family="serif", color =
             "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -77, y = -6 , hjust = 0, vjust = 1,angle=270,
           label = "San Martin",size = 3, family="serif", color =
             "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -78.5, y = -5 , hjust = 0, vjust = 1,
           label = "BAGUA",size = 3, family="serif", color =
             "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -77.5, y = -3 , hjust = 0, vjust = 1,
           label = "AMAZONAS",size = 3, family="serif", color =
             "black", face = "bold")



library(ggnewscale)
library(cowplot)
library(ggspatial)
Genral =ggplot()+
  geom_sf(data = Bagua, fill=NA, color="black", size=0.01, alpha=0.4)+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6)+
  scale_fill_gradientn(colours = colores ,
                       breaks = cortes ,
                       na.value = 'white',
                       labels = c("[menor a - 270] ","[270 - 499]", "[500 - 999]", "[1000 - 1999]", "[2000 - 2999]",
                                  "[3000 - 3999]", "[4000 - 4999]", "[5000 -6500]"),
                       name='Elevacion \n(msnm)')+
  geom_sf(data = Rio_Bagu ,  size=0.2, color="#a2d2ff")+
  coord_sf(xlim = c(-78.7, -78.1 ), ylim = c(-5.4 ,-4.8)) +
  guides(fill = guide_legend(
    title = " msnm.",
    direction = "horizontal",
    nrow = 1,
    keywidth = 1.75,
    keyheight = 0.5,
    label.position = "bottom",
    title.position = "right",
    override.aes = list(alpha = 1)
  ))+
  theme(legend.position = c(0.25,0.15),

        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        axis.title = element_text(face="bold", color="black"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 16, hjust = 0.5, family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11,  face = "italic", family="serif"),
        plot.caption = element_text(size = 9, family="serif", face = "italic"),

        plot.background = element_rect(fill = "white", color = NA),

        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=7, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"), #ancho de cuadrados de referencia

        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(x = 'Longitud', y = 'Latitud')

legend <- get_legend(Genral)

Genral1= Genral +  theme(legend.position = "nene")+
  geom_image(data = Gonatodes  , aes( x=decimalLongitude, y = decimalLatitude, image = image), size = 0.1)



# Mapa final
library(cowplot)
Final =ggdraw() +
  coord_equal(xlim = c(0, 29), ylim = c(0, 21), expand = FALSE) +
  draw_plot(SurA, width = 8, height = 8,x = 0.0001, y = 12.3)+
  draw_plot(Macro,             width = 8, height = 9,x = 0.0001, y = 3)+
  draw_plot(Genral1,            width =21, height = 21,x = 7, y = 0.001)+

  draw_plot(legend , width = 8, height = 8,x = 11, y = 19)+
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect (color = "white",
                                     fill = NA))



ggsave(plot=Final,"02_Mapas/07_Mapa de elevacon y ubicacion amazonas.png",units = "cm",
       width = 29, #alto
       height = 21, #ancho
       dpi=1500)













