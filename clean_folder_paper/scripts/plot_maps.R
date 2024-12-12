dpt_by_eje = read.csv("data/metadata/dpts_by_eje.csv")

#Import map
paraguay = sf::st_as_sf(raster::getData('GADM', country='PRY', level=1)) |>
  mutate(NAME_1 = gsub("Presidente","Pte", NAME_1))|>
  mutate(NAME_1 = toupper(iconv(NAME_1,to="ASCII//TRANSLIT"))) |>
  mutate(Eje = dpt_by_eje$Eje[sapply(NAME_1, function(x) which(dpt_by_eje$Department==x))])

p = ggplot()+
  geom_sf(data=paraguay, aes(fill=Eje)) +
  #geom_point(data=meta_weather, aes(x=lon_dec, y=lat_dec), size=3) +
  ggthemes::theme_map()+
  theme(legend.text = element_text(size=20),
        legend.title = element_text(size=25))

pm = paraguay %>% group_by(Eje) %>% summarise() %>%
  ggplot()+
  geom_sf(aes(fill=Eje)) +
  ggthemes::theme_map()+
  theme(legend.text = element_text(size=20),
        legend.title = element_text(size=25))

cowplot::plot_grid(p,pm)
