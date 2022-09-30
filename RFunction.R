library(move)
library(tmap)
library(htmlwidgets)
library(leaflet)
library(sf)
library(dplyr)

# tmap_mode("view") ##for interactive plot
# tmap_mode("plot") ## for static mapplot

rFunction <-function(data){
  
  #make sure that there is a location.long and location.lat in data set
  coo <- data.frame(coordinates(data))
  names(coo) <- c("location.long","location.lat")
  data_df <- as.data.frame(data)
  names(data_df) <- make.names(names(data_df),allow_=FALSE)
  data_df <- data.frame(data_df,coo)
  
  ###convert the data.frame using sf package
  trck <-st_as_sf(data_df, coords=c("location.long", "location.lat"), crs= sp::CRS("+init=epsg:4326"))
  
  ###convert the spatial points into linestrings
  tst <- trck %>% 
    group_by(trackId) %>%
    summarise(do_union = FALSE) %>%
    st_cast("LINESTRING")
  #plot(st_geometry(tst))
  
map_out <-  tm_shape(trck)+
  tm_graticules()+
  tm_dots(col = "trackId", palette= "viridis", title= "Individual_ID",scale=1.5)+
    tm_shape(tst)+
  tm_lines( col = "trackId", palette= "viridis", legend.col.show = FALSE)+
  tm_scale_bar(position = c("left", "bottom"), width = 0.5) +
  tm_compass(position = c("left", "top"), size = 4)+
  #tm_add_legend(type = "line", labels = "trackId", size=4)+
  tm_layout(legend.outside = TRUE,
            legend.title.size = 2, legend.width = 5,
            legend.text.size = 1.5)

map_out
tmap_plot <- tmap_leaflet(map_out, mode = "view", show = FALSE) %>%
  addProviderTiles(providers$Esri.WorldStreetMap)

###SAve both the static and interactive map
#tmap_save(map_out, width=10, height = 8, units = "in", dpi=300, file= "Mapplot_output.jpeg")
tmap_save(map_out, width=10, height = 8, units = "in", dpi=300,
           file= paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"Static_plot.jpeg"))

#saveWidget(tmap_plot, file="Movedata_intplot.html")
#saveWidget(tmap_plot, file=paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"Interactive_plot.html"))

return(data)
}
