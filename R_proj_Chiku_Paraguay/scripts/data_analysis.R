
require(pacman)

pacman::p_load(sf,
               raster,
               utils,
               geodata,
               essentials,
               ggplot2,
               dplyr)

rm(list=ls())

theme_set(theme_bw())

cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
                   
dpt_by_eje = read.csv("data/metadata/dpts_by_eje.csv")



#Weather data--------------------------------------
#Get meta data
meta_weather = read.delim("data/raw_data/metadatos_PY_utf8.txt", sep="\t") %>% 
  mutate(lon_dec = gsub(",",".",lon_dec), lat_dec = gsub(",",".",lat_dec)) %>% #Make all decimal separators as .
  mutate(lon_dec =as.numeric(lon_dec),lat_dec = as.numeric(lat_dec)) %>%
  mutate(nivel_adm_1 = gsub("\\?","",nivel_adm_1),
         nivel_adm_1 = gsub("Presidente","Pte",nivel_adm_1))

#Add ejes
meta_weather$eje = dpt_by_eje$Eje[sapply(toupper(iconv(meta_weather$nivel_adm_1,to="ASCII//TRANSLIT")), function(x) which(dpt_by_eje$Department==x))]

#Import map
paraguay = sf::st_as_sf(raster::getData('GADM', country='PRY', level=1)) %>%
  mutate(NAME_1 = gsub("Presidente","Pte", NAME_1))%>%
  mutate(NAME_1 = toupper(iconv(NAME_1,to="ASCII//TRANSLIT"))) %>%
  mutate(Eje = dpt_by_eje$Eje[sapply(NAME_1, function(x) which(dpt_by_eje$Department==x))])

ggplot()+
  geom_sf(data=paraguay, aes(fill=Eje)) +
  #geom_point(data=meta_weather, aes(x=lon_dec, y=lat_dec), size=3) +
  ggthemes::theme_map()+
  theme(legend.text = element_text(size=20),
        legend.title = element_text(size=25))
ggplot()+
  geom_sf(data=paraguay, aes(fill=Eje)) +
  geom_point(data=meta_weather, aes(x=lon_dec, y=lat_dec), size=3) +
  ggthemes::theme_map()

#Get weather data
weather_og = read.csv("data/raw_data/Datos_1961-2023.csv", sep="\t") %>% filter(fecha<"2023-01-01")
weather_23 = read.csv("data/raw_data/weather data update 20230101 to 20230925.csv", sep="\t")
weather_data = rbind(weather_og, weather_23) %>%
  mutate(across(c(tmax,tmin,tmed,prcp), function(x){gsub(",",".",x); as.numeric(x)}),
         fecha=as.Date(fecha),
         dpto = meta_weather$nivel_adm_1[sapply(omm_id, function(x) which(meta_weather$omm_id==x))],
         eje = meta_weather$eje[sapply(omm_id, function(x) which(meta_weather$omm_id==x))],
         lon = meta_weather$lon_dec[sapply(omm_id, function(x) which(meta_weather$omm_id==x))], 
         lat = meta_weather$lat_dec[sapply(omm_id, function(x) which(meta_weather$omm_id==x))])

#Get minimal and maximal temp
weather_extreme_temp = weather_data %>%
  group_by(fecha) %>%
  mutate(tmin = min(tmed, na.rm=T),
         tmax = max(tmed, na.rm=T)) %>%
  ungroup() %>%
  dplyr::select(fecha,tmin,tmax) %>%
  dplyr::distinct()
write.csv(weather_extreme_temp, "data/processed_data/extreme_temp_per_date.csv")

#Group by admin level
weather_by_eje = weather_data %>%
  group_by(fecha, eje) %>%
  mutate(prcp = mean(prcp, na.rm=T),
         tmed = mean(tmed, na.rm=T)) %>%
  ungroup() %>%
  dplyr::select(prcp, tmed, eje, fecha) %>%
  dplyr::distinct()

weather_national = weather_data %>%
  group_by(fecha) %>%
  mutate(prcp = mean(prcp, na.rm=T),
         tmed = mean(tmed, na.rm=T)) %>%
  ungroup() %>%
  dplyr::select(prcp, tmed, fecha) %>%
  dplyr::distinct()

weather_df = rbind(weather_by_eje %>%
                     dplyr::rename(Precipitation=prcp,Temperature=tmed, name=eje) %>%
                     mutate(level="Eje") %>%
                     tidyr::pivot_longer(cols=c(Precipitation, Temperature), names_to="variables", values_to="values"), 
                   weather_national %>%
                     dplyr::rename(Precipitation=prcp,Temperature=tmed) %>%
                     tidyr::pivot_longer(cols=c(Precipitation, Temperature), names_to="variables", values_to="values") %>%
                     mutate(level="National",name="Paraguay"))

#write.csv(weather_df, "data/processed_data/weather_data_1961to2023.csv")

#Sinusoidal fits--------------------
#Daily
#National
fits_coeff_national = c(coef(nls(data = weather_national,
                                 formula = prcp ~ A_prcp + B_prcp*cos(2*pi*as.numeric(fecha)/365)+C_prcp*sin(2*pi*as.numeric(fecha)/365),
                                 start = list(A_prcp=6.48, B_prcp=2.69, C_prcp=3.73))),
                        coef(nls(data = weather_national,
                                 formula = tmed ~ A_tmed + B_tmed*cos(2*pi*as.numeric(fecha)/365)+C_tmed*sin(2*pi*as.numeric(fecha)/365),
                                 start = list(A_tmed=6.48, B_tmed=2.69, C_tmed=3.73))))

#By eje
fits_coeff_eje = rbind(sapply(unique(weather_by_eje$eje), function(x) coef(nls(data = weather_by_eje %>% filter(eje==x),
                                                                               formula = prcp ~ A_prcp + B_prcp*cos(2*pi*as.numeric(fecha)/365)+C_prcp*sin(2*pi*as.numeric(fecha)/365),
                                                                               start = list(A_prcp=6.48, B_prcp=2.69, C_prcp=3.73)))),
                       
                       sapply(unique(weather_by_eje$eje), function(x) coef(nls(data = weather_by_eje %>% filter(eje==x),
                                                                               formula = tmed ~ A_tmed + B_tmed*cos(2*pi*as.numeric(fecha)/365)+C_tmed*sin(2*pi*as.numeric(fecha)/365),
                                                                               start = list(A_tmed=22, B_tmed=2.2, C_tmed=2)))))

fits_coeff = cbind(fits_coeff_eje, National=fits_coeff_national)
#write.csv(fits_coeff, "data/processed_data/coeffs_weather.csv")

#By eje by year (to get coldest and warmest years)
coeffs_weather = fits_coeff_eje
weather_by_eje = weather_by_eje %>% mutate(year = format(fecha, format("%Y")))
fits_coeff_eje_by_year = lapply(unique(weather_by_eje$eje), function(loc){
  
  table = sapply(unique(weather_by_eje$year), function(yr) {
    rbind(tryCatch({
      coef(nls(data = weather_by_eje %>% filter(eje==loc & year==yr),
               formula = prcp ~ A_prcp + B_prcp*cos(2*pi*as.numeric(fecha)/365)+C_prcp*sin(2*pi*as.numeric(fecha)/365),
               start = list(A_prcp=6.48, B_prcp=2.69, C_prcp=3.73)))
    },error=function(e){NA}),
    tryCatch({
      coef(nls(data = weather_by_eje %>% filter(eje==loc & year==yr),
               formula = tmed ~ A_tmed + B_tmed*cos(2*pi*as.numeric(fecha)/365)+C_tmed*sin(2*pi*as.numeric(fecha)/365),
               start = list(A_tmed=22, B_tmed=2.2, C_tmed=2)))
    },error=function(e){NA}))
  })
  rownames(table) = rownames(coeffs_weather)[c(1,4,2,5,3,6)] #Not very elegant
  return(table)
})
names(fits_coeff_eje_by_year) = colnames(coeffs_weather)
#save(fits_coeff_eje_by_year, file="data/processed_data/coeffs_weather_by_year_by_eje.csv")

#Plot temp and prcp evolution
sapply(fits_coeff_eje_by_year, function(x) x["A_tmed",]) %>% 
  as.data.frame(.) %>%
  mutate(year = as.numeric(rownames(.))) %>%
  tidyr::pivot_longer(cols = -year, names_to = "Eje", values_to = "Temperature") %>%
  ggplot()+
  geom_line(aes(x=year, y=Temperature, col=Eje))+
  geom_smooth(aes(x=year, y=Temperature, col=Eje))
sapply(fits_coeff_eje_by_year, function(x) x["A_prcp",]) %>% 
  as.data.frame(.) %>%
  mutate(year = as.numeric(rownames(.))) %>%
  filter(year<2023) %>%
  tidyr::pivot_longer(cols = -year, names_to = "Eje", values_to = "Precipitation") %>%
  ggplot()+
  geom_line(aes(x=year, y=Precipitation, col=Eje))+
  geom_smooth(aes(x=year, y=Precipitation, col=Eje))

#Get min and max temps for each eje (from 1966 oto 2022)
list_coeffs_weather = list(coeffs_med = coeffs_weather[c(1,4,2,5,3,6),],
                           coeffs_max = sapply(fits_coeff_eje_by_year, function(tab) tab[,as.numeric(colnames(tab))>1965 & as.numeric(colnames(tab))<2023][,which.max(tab["A_tmed",as.numeric(colnames(tab))>1965 & as.numeric(colnames(tab))<2023])]),
                           coeffs_min = sapply(fits_coeff_eje_by_year, function(tab) tab[,as.numeric(colnames(tab))>1965 & as.numeric(colnames(tab))<2023][,which.min(tab["A_tmed",as.numeric(colnames(tab))>1965 & as.numeric(colnames(tab))<2023])]))
#save(list_coeffs_weather, file="data/processed_data/coeffs_weather_med_min_max.csv")


#Plot fits
time_vect = as.numeric(unique(weather_by_eje$fecha)) 
fits_prcp = bind_rows(lapply(list_coeffs_weather, function(fits_coeff) as.data.frame(sapply(colnames(fits_coeff), function(eje) fits_coeff["A_prcp",eje] + fits_coeff["B_prcp",eje] * cos(2*pi*time_vect/365)+ fits_coeff["C_prcp",eje]*sin(2*pi*time_vect/365))) %>%
  mutate(fecha=unique(weather_by_eje$fecha), variables = "Precipitaion", cat="fit")), .id = "temp_range")
fits_tmed = bind_rows(lapply(list_coeffs_weather, function(fits_coeff) as.data.frame(sapply(colnames(fits_coeff), function(eje) fits_coeff["A_tmed",eje] + fits_coeff["B_tmed",eje] * cos(2*pi*time_vect/365)+ fits_coeff["C_tmed",eje]*sin(2*pi*time_vect/365))) %>%
  mutate(fecha=unique(weather_by_eje$fecha), variables = "Temperature", cat="fit")), .id = "temp_range")
fits = rbind(fits_prcp, fits_tmed) %>% 
  tidyr::pivot_longer(cols=-c(temp_range, fecha, variables, cat), names_to="eje", values_to="values")

rbind(weather_by_eje %>%
        dplyr::select(-c(year)) %>%
        #mutate(eje=gsub(" ","\\.",eje)) %>%
        dplyr::rename(Precipitaion=prcp,Temperature=tmed) %>%
        tidyr::pivot_longer(cols=c(Precipitaion, Temperature), names_to="variables", values_to="values") %>%
        mutate(cat="data", temp_range="data"), 
      fits) %>%
  filter(fecha>"2020-01-01") %>%
  ggplot()+
  geom_line(aes(x=fecha, y=values, col=temp_range, alpha=cat, size=cat))+
  scale_size_manual(values=c(0.1,1.5))+
  scale_alpha_manual(values=c(0.5,1))+
  scale_color_manual(values=c(cols[c(7,2,3,1)]))+
  facet_grid(variables~eje, scales="free")+
  theme_minimal()

rbind(weather_by_eje %>%
        dplyr::select(-c(year)) %>%
        #mutate(eje=gsub(" ","\\.",eje)) %>%
        dplyr::rename(Precipitaion=prcp,Temperature=tmed) %>%
        tidyr::pivot_longer(cols=c(Precipitaion, Temperature), names_to="variables", values_to="values") %>%
        mutate(cat="data", temp_range="data"), 
      fits) %>%
  filter(fecha>"2022-01-01") %>%
  ggplot()+
  geom_line(aes(x=fecha, y=values, col=temp_range, alpha=cat, size=cat))+
  scale_size_manual(values=c(0.1,1.5))+
  scale_alpha_manual(values=c(0.5,1))+
  scale_color_manual(values=c(cols[c(7,2,3,1)]))+
  facet_grid(variables~eje, scales="free")+
  theme_minimal()

#Weekly
#National
as.numeric(format(weather_national$fecha,"%U"))
weather_national$week_vect = difftime(weather_national$fecha,as.Date("2023-01-01"),units="weeks")
fits_coeff_national_weekly = c(coef(nls(data = weather_national,
                                        formula = prcp ~ A_prcp + B_prcp*cos(2*pi*as.numeric(week_vect)/52)+C_prcp*sin(2*pi*as.numeric(week_vect)/52),
                                        start = list(A_prcp=6.48, B_prcp=2.69, C_prcp=3.73))),
                               coef(nls(data = weather_national,
                                        formula = tmed ~ A_tmed + B_tmed*cos(2*pi*as.numeric(week_vect)/52)+C_tmed*sin(2*pi*as.numeric(week_vect)/52),
                                        start = list(A_tmed=6.48, B_tmed=2.69, C_tmed=3.73))))

#By eje
weather_by_eje$week_vect = difftime(weather_by_eje$fecha,as.Date("2023-01-01"),units="weeks")
fits_coeff_eje_weekly = rbind(sapply(unique(weather_by_eje$eje), function(x) coef(nls(data = weather_by_eje %>% filter(eje==x),
                                                                                      formula = prcp ~ A_prcp + B_prcp*cos(2*pi*as.numeric(week_vect)/52)+C_prcp*sin(2*pi*as.numeric(week_vect)/52),
                                                                                      start = list(A_prcp=6.48, B_prcp=2.69, C_prcp=3.73)))),
                              
                              sapply(unique(weather_by_eje$eje), function(x) coef(nls(data = weather_by_eje %>% filter(eje==x),
                                                                                      formula = tmed ~ A_tmed + B_tmed*cos(2*pi*as.numeric(week_vect)/52)+C_tmed*sin(2*pi*as.numeric(week_vect)/52),
                                                                                      start = list(A_tmed=22, B_tmed=2.2, C_tmed=2)))))

fits_coeff_weekly = cbind(fits_coeff_eje_weekly, National=fits_coeff_national_weekly)
write.csv(fits_coeff_weekly, "data/processed_data/coeffs_weather_weekly.csv")


#Plot fits
time_vect = as.numeric(unique(weather_by_eje$week_vect)) 
fits_prcp = as.data.frame(sapply(colnames(fits_coeff_weekly), function(eje) fits_coeff_weekly["A_prcp",eje] + fits_coeff_weekly["B_prcp",eje] * cos(2*pi*time_vect/52)+ fits_coeff_weekly["C_prcp",eje]*sin(2*pi*time_vect/52))) %>%
  mutate(fecha=unique(weather_by_eje$fecha), week_vect=unique(weather_by_eje$week_vect), variables = "Precipitaion", cat="fit")
fits_tmed =  as.data.frame(sapply(colnames(fits_coeff_weekly), function(eje) fits_coeff_weekly["A_tmed",eje] + fits_coeff_weekly["B_tmed",eje] * cos(2*pi*time_vect/52)+ fits_coeff_weekly["C_tmed",eje]*sin(2*pi*time_vect/52))) %>%
  mutate(fecha=unique(weather_by_eje$fecha),  week_vect=unique(weather_by_eje$week_vect), variables = "Temperature", cat="fit")
fits = rbind(fits_prcp, fits_tmed) %>% 
  tidyr::pivot_longer(cols=c(unique(dpt_by_eje$Eje),National), names_to="eje", values_to="values")


rbind(weather_by_eje %>%
        dplyr::rename(Precipitaion=prcp,Temperature=tmed) %>%
        tidyr::pivot_longer(cols=c(Precipitaion, Temperature), names_to="variables", values_to="values") %>%
        mutate(cat="data"), 
      fits,
      weather_national %>%
        dplyr::rename(Precipitaion=prcp,Temperature=tmed) %>%
        tidyr::pivot_longer(cols=c(Precipitaion, Temperature), names_to="variables", values_to="values") %>%
        mutate(cat="data",eje="National")) %>%
  filter(fecha>"2020-01-01") %>%
  ggplot()+
  geom_line(aes(x=fecha, y=values, col=cat, alpha=cat, size=cat))+
  scale_size_manual(values=c(0.1,1.5))+
  scale_alpha_manual(values=c(0.5,1))+
  scale_color_manual(values=c(cols[c(1,2)]))+
  facet_grid(variables~eje, scales="free")+
  theme_minimal()

rbind(weather_by_eje %>%
        dplyr::rename(Precipitaion=prcp,Temperature=tmed) %>%
        tidyr::pivot_longer(cols=c(Precipitaion, Temperature), names_to="variables", values_to="values") %>%
        mutate(cat="data"), 
      fits,
      weather_national %>%
        dplyr::rename(Precipitaion=prcp,Temperature=tmed) %>%
        tidyr::pivot_longer(cols=c(Precipitaion, Temperature), names_to="variables", values_to="values") %>%
        mutate(cat="data",eje="National")) %>%
  filter(fecha>"2022-01-01" & eje=="National") %>%
  ggplot()+
  geom_line(aes(x=fecha, y=values, col=cat, alpha=cat, size=cat))+
  scale_size_manual(values=c(0.1,1.5))+
  scale_alpha_manual(values=c(0.5,1))+
  scale_color_manual(values=c(cols[c(1,2)]))+
  facet_grid(variables~eje, scales="free")+
  theme_minimal()



#Population data-----------------
pop_data_by_dpt = readxl::read_xlsx("data/raw_data/PROYECCION_POBLACIÓN_ AL 2025/POB_DPTO_PY.xlsx") %>%
  mutate(DEPARTAMENTO = iconv(DEPARTAMENTO,to="ASCII//TRANSLIT")) %>%
  dplyr::rename(Population='AÑO 2023') %>%
  dplyr::select(DEPARTAMENTO, Population) %>%
  dplyr::filter(DEPARTAMENTO != "TOTAL") %>%
  mutate(DEPARTAMENTO = gsub("PTE.","PTE", DEPARTAMENTO))%>%
  mutate(Eje = dpt_by_eje$Eje[sapply(DEPARTAMENTO, function(x) which(dpt_by_eje$Department==x))]) %>%
  mutate(level="Department") %>%
  dplyr::rename(name=DEPARTAMENTO)


pop_data_by_eje = data.frame(level="Eje", name=names(tapply(pop_data_by_dpt$Population,pop_data_by_dpt$Eje,FUN=sum)),Population=tapply(pop_data_by_dpt$Population,pop_data_by_dpt$Eje,FUN=sum))

pop_data_national = data.frame(level="National", name="Paraguay", Population=sum(pop_data_by_eje$Population))

pop_df = rbind(pop_data_by_dpt %>% dplyr::select(-Eje),
               pop_data_by_eje,
               pop_data_national)

write.csv(pop_df, "data/processed_data/pop_data.csv")



