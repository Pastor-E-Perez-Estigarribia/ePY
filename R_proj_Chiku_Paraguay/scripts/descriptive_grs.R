rm(list=ls())

require("pacman")

# 1. Instalar/Cargar paquetes ####
pacman::p_load(tidyverse, # Manejo y tratamiento de datos
               rio,       # Importación y exportación de bases de datos
               abjutils,
               summarytools,
               naniar,     # Limpieza de texto
               shiny,
               ggraptR) # interactive plot


# I. Chikungunya ----


dpt_by_eje = read.csv("data/metadata/dpts_by_eje.csv") %>% 
  dplyr::rename(department_desc = 'Department')


pop_df_by_loc = read.csv("data/processed_data/pop_data.csv", row.names=1) 

#nominal_df = read.csv("data/processed_data/nominal_data_20231106.csv")

nominal_df = read.csv("data/processed_data/nominal_data_2024012430.csv")

#st_options(lang = "es") #Translations
summarytools::view(dfSummary(nominal_df), 
                   footnote = NA, 
                   valid.col = FALSE, 
                   file = paste("output/","summario ARBOVIROSIS 2019-2023.html", sep =""))



# 4. Descritive cases ----


# 5. Case curve ####

# https://vivdas.medium.com/create-heatmap-in-r-using-ggplot2-d4c02dccec28 

theme_set(theme_minimal()+
            theme(legend.position = "top",
              text = element_text(
              family = "sans",
              face = "plain",
              color = "#000000",
              size = 20,
              hjust = 0.5,
              vjust = 0.5
            )))


cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Create data frames per administrative levels

names(nominal_df)


case_df_by_dpt = nominal_df %>%
 dplyr::rename(Departamento_desc = "department_desc") %>% 
  dplyr::select(Departamento_desc, Eje, date, classification, disease)  %>% 
  filter(classification!="DESCARTADO") %>%#Remove discarded cases
  group_by(Departamento_desc, Eje, date, classification, disease) %>% 
  mutate(i_cases = n()) %>%
  distinct() %>% 
  ungroup() %>%
  filter(!is.na(Departamento_desc)) %>% 
  tidyr::pivot_wider(names_from=classification, values_from=i_cases) %>%
  tidyr::replace_na(list(SOSPECHOSO=0, CONFIRMADO=0,PROBABLE=0)) %>%
  mutate(TOTAL=SOSPECHOSO+CONFIRMADO+PROBABLE) %>%
  tidyr::pivot_longer(cols=c(SOSPECHOSO, CONFIRMADO, PROBABLE, TOTAL), names_to="classification", values_to="i_cases") %>%
 dplyr::rename(name=Departamento_desc) %>%
  mutate(level="Department") %>%
  relocate(date, level) 


tapply(case_df_by_dpt$i_cases, list(case_df_by_dpt$classification, case_df_by_dpt$disease), sum)


case_df_by_eje = case_df_by_dpt %>% 
  group_by(Eje, date, classification, disease) %>%
  mutate(i_cases=sum(i_cases)) %>%
  ungroup() %>%
  dplyr::select(-name) %>%
  distinct() %>%
 dplyr::rename(name=Eje) %>%
  mutate(level="Eje") %>%
  relocate(date, level) 

case_df_national = case_df_by_dpt %>% 
  group_by(date, classification, disease) %>%
  mutate(i_cases=sum(i_cases)) %>%
  ungroup() %>%
  dplyr::select(-c(name,Eje)) %>%
  distinct() %>%
  mutate(level="National",name="Paraguay") %>%
  relocate(date, level) 

case_df = rbind(case_df_by_dpt %>% dplyr::select(-Eje), case_df_by_eje, case_df_national)

write.csv(case_df, "data/processed_data/case_data_2019_2023_epiweek_v20240124.csv", row.names = F)

#________

case_df$date = as.Date(case_df$date)

class(case_df$date)

summary(case_df$dat)

case_df$date = as.Date(case_df$date)

case_df_weekly = case_df %>% 
  mutate(date=format(as.Date(date - wday(date)+ 7), format="%Y-%m-%d")) %>%
  group_by(date, level, name, disease, classification) %>%
  summarise(i_cases = sum(i_cases)) %>%
  ungroup() %>%
  
  #Get the time windows to crop the time series once we fill in with zeroes
  group_by(name, level, disease) %>% 
  mutate(t_min=min(date, na.rm = TRUE), t_max=max(date, na.rm = TRUE)) %>%
  ungroup() %>%
  
  #Complete with missing dates
  tidyr::complete(date, tidyr::nesting(name,level), disease, classification, fill=list(i_cases=0)) %>%
  mutate(date = as.Date(date), t_min=as.Date(t_min), t_max=as.Date(t_max)) %>%
  
  #Add tmin and tmax where it got filled in with NAs
  group_by(name, level, disease, classification) %>%
  mutate(t_min=unique(t_min)[1+is.na(unique(t_min))[1]], t_max=unique(t_max)[1+is.na(unique(t_max))[1]]) %>%
  ungroup() %>%
  
  #Crop out when not within the original time windows
  filter(date>=t_min & date<=t_max) %>% dplyr::select(-c(t_min,t_max))  %>% 
  full_join(.,pop_df_by_loc, by = c("name", "level")) %>% 
  mutate(incidence = 10000*i_cases/Population)

summarytools::view(dfSummary(case_df_weekly), 
                   footnote = NA, 
                   valid.col = FALSE, 
                   file = paste("output/","summario series de tiempo ARBOVIROSIS 2019-2023.html", sep =""))


#case_df_weekly %>% filter(classification=="TOTAL" & name=="Centro sur" & disease=="CHIKUNGUNYA") %>% mutate(date=as.Date(date)) %>% arrange(date) %>% print(n=100)
#Plot case data
p_inc_curve = case_df_weekly  %>%
  filter(level!="Department") %>%
  filter(disease=="CHIKUNGUNYA") %>%
 # filter(date >= as.Date('2022-10-01') & date <= as.Date('2023-09-23')) %>% 
  filter(date >= as.Date('2022-10-01')) %>% 
  filter(classification == "TOTAL") %>% 
  mutate(name=factor(name,
                    levels=c("Metropolitano",
                             "Centro est",
                             "Centro sur",
                             "Centro norte",
                             "Chaco",
                             "Paraguay"),
                    labels=c("Metropolitano",
                             "Centro Este",
                             "Centro Sur",
                             "Centro Norte",
                             "Chaco",
                             "Paraguay"))) %>% 
  ggplot()+
  geom_line(aes(x=as.Date(date),y=incidence,col=name, group=name), 
            size=0.7,
            alpha = 0.85) +
#  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(x = "Date (per week)",
       y = "Incidence per 100,000",
       colour = "Sub-region") +
  scale_x_date(date_breaks = "2 month", date_labels =  "%y %W") 

p_inc_curve

# ggsave(
#   "output/Incidence_100000_per_epidemiological_week_v20240124.pdf",
#   plot = p_inc_curve,
#   width = 22.65248,
#   height = 14,
#   units = "cm",
#   dpi = 500,
#   scale=1
# )

# ggsave(
#   "output/Incidence_100000_per_epidemiological_week_v20240124.svg",
#   plot = p_inc_curve,
#   width = 22.65248,
#   height = 14,
#   units = "cm",
#   dpi = 500,
#   scale=1
# )

require(reshape)

p_heatmap_inc = case_df_weekly  %>%
  filter(level!="Department") %>%
  filter(disease=="CHIKUNGUNYA") %>%
  filter(date >= as.Date('2022-09-01') & date <= as.Date('2023-09-30')) %>% 
#  filter(date >= as.Date('2022-10-01')) %>% 
  filter(classification == "TOTAL") %>% 
  mutate(name=factor(name,
                     levels=c(
                              "Chaco",
                              "Centro sur",
                              "Centro est",
                              "Centro norte",
                              "Metropolitano",
                              "Paraguay"),
                     labels=c(
                              "Chaco",
                              "Centro Sur",
                              "Centro Este",
                              "Centro Norte",
                              "Metropolitano",
                              "Paraguay"))) %>% 
  select(name,date, incidence) %>% 
  ggplot(aes(date, name, fill= incidence)) + 
  geom_tile() +
  labs(x = "Date (per week)",
       y = "",
       fill = "Incidence per 10,000")  +
  scale_x_date(date_breaks = "2 month", date_labels =  "%b %y") + 
  scale_fill_distiller(palette = 'Spectral')

p_heatmap_inc


ggsave(
  "output/fig/p_heatmap_inc_v20240124.pdf",
  plot = p_heatmap_inc,
  width = 22.65248,
  height = 14,
  units = "cm",
  dpi = 500,
  scale=1
)

ggsave(
  "output/fig/p_heatmap_inc_v20240124.png",
  plot = p_heatmap_inc,
  width = 22.65248,
  height = 14,
  units = "cm",
  dpi = 500,
  scale=1
)

ggsave(
  "output/fig/p_heatmap_inc_week_v20240124.svg",
  plot = p_heatmap_inc,
  width = 22.65248,
  height = 14,
  units = "cm",
  dpi = 500,
  scale=1
)

#View(case_df_weekly)

case_df_weekly  %>%
  filter(level!="Department") %>%
  filter(disease!="DENGUE,CHIKUNGUNYA" & disease!="ARBOVIROSIS") %>%
 # filter(date >= as.Date('2022-10-01') & date <= as.Date('2023-09-23')) %>% 
  filter(date >= as.Date('2022-10-01')) %>% 
  ggplot()+
  geom_line(aes(x=as.Date(date),y=i_cases,col=classification, group=classification))+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2022-01-01", "2023-01-01"))), col="gray60")+
  facet_grid(name~disease, scales="free_y") +
  #facet_grid(name~disease, scales="free") +
  scale_color_manual(values=c(cols[c(2,3,4)],"black")) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_y_continuous(name="No. casos") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %y") +
  xlab("Epi week")

write.csv(case_df_weekly, "data/processed_data/case_data_2019_2023__v20240124.csv", row.names = F)


names(nominal_df)

#rm(p_epicurva)

# Función labs() para nombrar el título
p_epicurva = nominal_df %>%
  filter(classification!="DESCARTADO") %>%#Remove discarded cases
  filter(disease =="CHIKUNGUNYA") %>%
  filter(sex =="MASCULINO" | sex =="FEMENINO") %>%
  filter(date >= as.Date('2022-09-01') & date <= as.Date('2023-09-30')) %>% 
#  filter(date >= as.Date('2022-10-01')) %>% 
  mutate(Eje=factor(Eje,
                    levels=c("Metropolitano",
                             "Centro est",
                             "Centro sur",
                             "Centro norte",
                             "Chaco"),
                    labels=c("Metropolitano",
                      "Centro Este",
                      "Centro Sur",
                      "Centro Norte",
                      "Chaco"))) %>% 
ggplot() +
  geom_bar(aes(x = as.Date(date_d7se),
                     fill=Eje),
                 stat = "count",
                 alpha = 0.80) +
  labs(x = "Date (per week)",
       y = "Total cases",
       fill = " ") +
   theme(legend.position = "top",
         legend.text=element_text(size=12)) +
  scale_x_date(date_breaks = "2 month", date_labels =  "%b %y") 

p_epicurva


# ggsave(
#   "output/Total_cases_per_epidemiological_week_20231113.pdf",
#   plot = p_epicurva,
#   width = 22.65248,
#   height = 14,
#   units = "cm",
#   dpi = 500,
#   scale=1
# )

# ggsave(
#   "output/Total_cases_per_epidemiological_week_20231113.svg",
#   plot = p_epicurva,
#   width = 22.65248,
#   height = 14,
#   units = "cm",
#   dpi = 500,
#   scale=1
# )


df_inc = case_df_weekly  %>%
  filter(level!="Department") %>%
  filter(disease=="CHIKUNGUNYA") %>%
  filter(date >= as.Date('2022-10-01') & date <= as.Date('2023-09-23')) %>% 
  filter(classification == "TOTAL") %>% 
  mutate(name=factor(name,
                     levels=c("Metropolitano",
                              "Centro est",
                              "Centro sur",
                              "Centro norte",
                              "Chaco",
                              "Paraguay"),
                     labels=c("Metropolitano",
                              "Centro Este",
                              "Centro Sur",
                              "Centro Norte",
                              "Chaco",
                              "Paraguay")))


## 

unique(nominal_df$classification)

require(RColorBrewer)

aux = nominal_df %>%
  filter(classification!="DESCARTADO") %>%#Remove discarded cases
  filter(disease =="CHIKUNGUNYA") %>%
  filter(sex =="MASCULINO" | sex =="FEMENINO") %>%
  filter(date >= as.Date('2022-09-01') & date <= as.Date('2023-09-30')) 

table(aux$classification)/sum(table(aux$classification))*100

# Función labs() para nombrar el título
p_prop_conf = nominal_df %>%
  filter(classification!="DESCARTADO") %>%#Remove discarded cases
  filter(disease =="CHIKUNGUNYA") %>%
  filter(sex =="MASCULINO" | sex =="FEMENINO") %>%
  filter(date >= as.Date('2022-10-01') & date <= as.Date('2023-09-23')) %>% 
  mutate(Eje=factor(Eje,
                     levels=c(
                              "Chaco",
                              "Centro sur",
                              "Centro est",
                              "Centro norte",
                              "Metropolitano"),
                     labels=c(
                              "Chaco",
                              "Centro Sur",
                              "Centro Este",
                              "Centro Norte",
                              "Metropolitano"))) %>% 
  mutate(classification=factor(classification,
                     levels=c("DESCARTADO",
                              "SOSPECHOSO",
                              "PROBABLE",
                              "CONFIRMADO"),
                     labels=c(
                       "DISCARDED",
                       "SUSPICIOUS",
                       "PROBABLE",
                       "CONFIRMED"))) %>% 
  ggplot() +
  geom_bar(aes(x = Eje,
               fill= classification),
           stat = "count",
           position = "fill",
           alpha = 0.80,
           just = 0.5,
           width = 0.5) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "",
       y = "",
       fill = "") + scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top") + coord_flip() 
 
p_prop_conf

# ggsave(
#   "output/p_prop_conf_20231113.pdf",
#   plot = p_prop_conf,
#   width = 22.65248,
#   height = 14,
#   units = "cm",
#   dpi = 500,
#   scale=1
# )
# 
# ggsave(
#   "output/p_prop_conf_20231113.svg",
#   plot = p_prop_conf,
#   width = 22.65248,
#   height = 14,
#   units = "cm",
#   dpi = 500,
#   scale=1
# )
# 
# ?rename

# 6. Seroprevalence ####

sero_10 = import("https://docs.google.com/spreadsheets/d/1dYHa-PfNh3LQXtfZ8Ra4VOX6MIfQYhY1/edit#gid=234901776") %>% 
  dplyr::rename(age = Edad,
                Sex = Sexo,
                Seropositive = Resultado) %>% 
  mutate(Age_group = case_when(
    age >= 0   & age < 10  ~ '00-09',
    age >= 10  & age < 20 ~ '10-19',
    age >= 20  & age < 30 ~ '20-29',
    age >= 30  & age < 40  ~ '30-39',
    age >= 40  & age < 50 ~ '40-49',
    age >= 50  & age < 60  ~ '50-59',
    age >= 60  & age < 70  ~ '60-69',
    age >= 70  & age < 80 ~ '70-79',
    age >= 80  & age < 90  ~ '80-89',
    age >= 90  & age < 100  ~ '90-99',
    age >= 100  & age < 110 ~ '100+'
  )) %>% 
  mutate(Sex = gsub("Femenino","F", Sex))  %>% 
  mutate(Sex = gsub("Masculino","M", Sex))  %>% 
  mutate(Seropositive = gsub("NEGATIVO","NO", Seropositive)) %>% 
  mutate(Seropositive = gsub("POSITIVO","YES", Seropositive))


sero = import("https://docs.google.com/spreadsheets/d/1dYHa-PfNh3LQXtfZ8Ra4VOX6MIfQYhY1/edit#gid=234901776") %>% 
  dplyr::rename(age = Edad,
         Sex = Sexo,
         Seropositive = Resultado) %>% 
  mutate(Age_group = case_when(
           age >= 18  & age < 30 ~ '18-29',
           age >= 30  & age < 40  ~ '30-39',
           age >= 40  & age < 50 ~ '40-49',
           age >= 50  & age < 60  ~ '50-65'
         )) %>% 
  mutate(Sex = gsub("Femenino","F", Sex))  %>% 
  mutate(Sex = gsub("Masculino","M", Sex))  %>% 
  mutate(Seropositive = gsub("NEGATIVO","NO", Seropositive)) %>% 
  mutate(Seropositive = gsub("POSITIVO","YES", Seropositive))

sero$`Fecha Extracción` = as.Date(sero$`Fecha Extracción`)
  
summary(sero$age)


ggplot(sero) +
  geom_bar(aes(x= Sex, fill = Seropositive), position = "fill")

ggplot(sero) +
  geom_bar(aes(x= Age_group, fill = Seropositive), position = "fill") 

summary(sero$age)

sero_df = rbind(
as.data.frame(table(sero$Seropositive)) %>% 
  pivot_wider(names_from = Var1, values_from = Freq) %>% 
  mutate(N = NO + YES,
         Fr = 100*YES/N)  %>% 
  select(YES, N, Fr) %>% 
  dplyr::rename(n_pos = YES, 
         n_tot= N) %>% 
  mutate(level = "Geographic",
         name = "Paraguay",
         date="2023-09-02") %>% 
  select(date, level, name, n_pos, n_tot, Fr), 
as.data.frame(table(sero$Eje,sero$Seropositive)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq) %>% 
  mutate(N = NO + YES,
         Fr = 100*YES/N)  %>% 
  select(Var1, YES, N, Fr) %>% 
  dplyr::rename(name = Var1,
         n_pos =YES, 
         n_tot= N) %>% 
  mutate(level = "Geographic",
         date="2023-09-02") %>% 
  select(date, level, name, n_pos, n_tot, Fr),
as.data.frame(table(sero$Sex,sero$Seropositive)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq) %>% 
  mutate(N = NO + YES,
         Fr = 100*YES/N)  %>% 
  select(Var1, YES, N, Fr) %>% 
  dplyr::rename(name = Var1,
                n_pos =YES, 
                n_tot= N) %>% 
  mutate(level = "Sex",
         date="2023-09-02") %>% 
  select(date, level, name, n_pos, n_tot, Fr),
as.data.frame(table(sero$Age_group,sero$Seropositive)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq) %>% 
  mutate(N = NO + YES,
         Fr = 100*YES/N)  %>% 
  select(Var1, YES, N, Fr) %>% 
  dplyr::rename(name = Var1,
                n_pos =YES, 
                n_tot= N) %>% 
  mutate(level = "Age group",
         date="2023-09-02") %>% 
  select(date, level, name, n_pos, n_tot, Fr)
) %>% 
  mutate(Estimate = Hmisc::binconf(x = n_pos, n=n_tot,alpha=0.05,
                              method="wilson")[,1]*100,
         CI_Lower = Hmisc::binconf(x = n_pos, n=n_tot,alpha=0.05,
                              method="wilson")[,2]*100,
         CI_Upper = Hmisc::binconf(x = n_pos, n=n_tot,alpha=0.05,
                                   method="wilson")[,3]*100) %>% 
  mutate(name_0 = name) %>% 
  mutate(name = paste0(name, " (", "n=",n_tot,")"))

sero_df


sero_df_as = 
  as.data.frame(table(sero_10$Age_group,sero$Sex,sero$Seropositive))%>% 
    pivot_wider(names_from = Var3, values_from = Freq) %>% 
  dplyr::rename(Age_group = Var1,
                Sex = Var2) %>% 
    mutate(N = NO + YES,
           Fr = 100*YES/N)  %>% 
    select(Age_group, Sex, YES, N, Fr) %>% 
    dplyr::rename(n_pos =YES, 
                  n_tot= N) %>% 
    mutate(date="2023-09-02") %>% 
    select(date, Age_group, Sex, n_pos, n_tot, Fr)  %>% 
  mutate(Estimate = Hmisc::binconf(x = n_pos, n=n_tot,alpha=0.05,
                                   method="wilson")[,1]*100,
         CI_Lower = Hmisc::binconf(x = n_pos, n=n_tot,alpha=0.05,
                                   method="wilson")[,2]*100,
         CI_Upper = Hmisc::binconf(x = n_pos, n=n_tot,alpha=0.05,
                                   method="wilson")[,3]*100) 

sero_df_as

p_sero = sero_df %>% 
  mutate(level=factor(level,
                     levels=c("Geographic",
                              "Age group",
                              "Sex"),
                     labels=c("Geographic",
                              "Age group",
                              "Sex"))) %>% 
ggplot(aes(x = name, y = Estimate)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "darkgray")  +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 15)) +
  labs(x = "", y = "Seroprevalence % (.95 CI)") + 
  facet_grid(level~., scales = "free_y", space = "free") +
  coord_flip() 

p_sero


ggsave(
  "output/fig/p_sero_20240228.pdf",
  plot = p_sero,
  width = 22.65248,
  height = 14,
  units = "cm",
  dpi = 500,
  scale=1
)

ggsave(
  "output/fig/p_sero_20240228.svg",
  plot = p_sero,
  width = 22.65248,
  height = 14,
  units = "cm",
  dpi = 500,
  scale=1
)


## Formula interface for tabulated data plus shading and legend:
vcd::mosaic(~Seropositive + Age_group + Sex, data = sero, main = " ", 
       shade = TRUE, legend = T)


write.csv(sero_df, 'output/sero_df.csv')




# 7. cases by sex and age ----

names(pop_df)

pop_df <- import("data/raw_data/Paraguay-2023.csv", encoding = "Latin-1") %>%
  pivot_longer(!c(age_group,age_group_by), 
               names_to = "demographic_level", 
               values_to = "projection")

names(nominal_df)

case_df_by_age_and_sex = nominal_df %>%
  filter(classification!="DESCARTADO") %>%#Remove discarded cases
  filter(disease =="CHIKUNGUNYA") %>%
  filter(sex =="MASCULINO" | sex =="FEMENINO") %>%
  filter(date >= as.Date('2022-09-01') & date <= as.Date('2023-09-30')) %>% 
  group_by(sex,age_group_10) %>% 
  summarise(i_cases = n()) %>% 
  drop_na() %>% 
  full_join(., nominal_df %>%
              filter(classification!="DESCARTADO") %>%#Remove discarded cases
              filter(dead_classification == "CONFIRMADO") %>%#Remove discarded cases
              filter(disease =="CHIKUNGUNYA") %>%
              filter(sex =="MASCULINO" | sex =="FEMENINO") %>%
              filter(date >= as.Date('2022-09-01') & date <= as.Date('2023-09-30')) %>% 
              group_by(sex,age_group_10) %>% 
              summarise(d_cases = n()) %>% 
              drop_na(), 
            by = c("sex","age_group_10")) %>% 
  mutate(sex = gsub("MASCULINO","M", sex)) %>%
  mutate(sex = gsub("FEMENINO","F", sex)) %>% 
  full_join(., pop_df %>% subset(age_group_by == "age_group_10" &
                                   demographic_level != "COUNTRY"),
            by = c("age_group_10"="age_group", "sex"="demographic_level")) %>% 
  mutate(cfr = 100*d_cases/i_cases,
         incidence = 10000*i_cases/projection,
         age_specific_mortality = 10000*d_cases/projection)  %>% 
  replace(is.na(.), 0) %>% 
  dplyr::rename("Age_group"="age_group_10",
                "Sex" = "sex" ) %>% 
  full_join(.,sero_df_as %>% 
              select(Sex,Age_group,Estimate), by = c("Sex","Age_group")) %>% 
  mutate(Estimate_m = mean(sero_df_as$Estimate),
         CI_fm = projection*Estimate_m/100,
         CI_f = projection*Estimate/100,
         CI_0 = projection*0.0185, #0.001,
         TI_m = CI_fm-CI_0,
         TI = CI_f-CI_0,
         IFR= 100*d_cases/TI,
         IFR_m= 100*d_cases/TI_m,
         pD = i_cases/TI_m) %>% 
  mutate(Age_group=factor(Age_group,
                             levels=c("00-09",
                                      "10-19",
                                      "20-29",
                                      "30-39",
                                      "40-49",
                                      "50-59",
                                      "60-69",
                                      "70-79",
                                      "80-89",
                                      "90-99",
                                      "100+"),
                             labels=c(
                               "00-09",
                               "10-19",
                               "20-29",
                               "30-39",
                               "40-49",
                               "50-59",
                               "60-69",
                               "70-79",
                               "80-89",
                               "90-99",
                               "100+"))) 

unique(case_df_by_age_and_sex$age_group_10)

names(sero_df_as)

sero_df_as %>% 
  select(Sex,Age_group,Estimate)

case_df_by_age_and_sex$Sex = as.factor(case_df_by_age_and_sex$Sex)

case_df_by_age_and_sex

#%>% drop_na()

case_df_by_age_and_sex = nominal_df %>%
  filter(classification!="DESCARTADO") %>%#Remove discarded cases
  filter(disease =="CHIKUNGUNYA") %>%
  filter(sex =="MASCULINO" | sex =="FEMENINO") %>%
  filter(date >= as.Date('2022-09-01') & date <= as.Date('2023-09-30')) %>% 
  group_by(sex,age_group_10) %>% 
  summarise(i_cases = n()) %>% 
  drop_na() %>% 
  full_join(., nominal_df %>%
              filter(classification!="DESCARTADO") %>%#Remove discarded cases
              filter(dead_classification == "CONFIRMADO") %>%#Remove discarded cases
              filter(disease =="CHIKUNGUNYA") %>%
              filter(sex =="MASCULINO" | sex =="FEMENINO") %>%
              filter(date >= as.Date('2022-09-01') & date <= as.Date('2023-09-30')) %>% 
              group_by(sex,age_group_10) %>% 
              summarise(d_cases = n()) %>% 
              drop_na(), 
            by = c("sex","age_group_10")) %>% 
  mutate(sex = gsub("MASCULINO","M", sex)) %>%
  mutate(sex = gsub("FEMENINO","F", sex)) %>% 
  full_join(., pop_df %>% subset(age_group_by == "age_group_10" &
                                   demographic_level != "COUNTRY"),
            by = c("age_group_10"="age_group", "sex"="demographic_level")) %>% 
  mutate(cfr = 100*d_cases/i_cases,
         incidence = 10000*i_cases/projection,
         age_specific_mortality = 10000*d_cases/projection)  %>% 
  replace(is.na(.), 0) %>% 
  dplyr::rename("Age_group"="age_group_10",
                "Sex" = "sex" ) %>% 
  full_join(.,sero_df_as %>% 
              select(Sex,Age_group,Estimate), by = c("Sex","Age_group")) %>% 
  mutate(Estimate_m = mean(sero_df_as$Estimate),
         CI_fm = projection*Estimate_m/100,
         CI_f = projection*Estimate/100,
         CI_0 = projection*0.0185, #0.001,
         TI_m = CI_fm-CI_0,
         TI = CI_f-CI_0,
         IFR= 100*d_cases/TI,
         IFR_m= 100*d_cases/TI_m,
         pD = i_cases/TI_m) %>% 
  mutate(Age_group=factor(Age_group,
                          levels=c("00-09",
                                   "10-19",
                                   "20-29",
                                   "30-39",
                                   "40-49",
                                   "50-59",
                                   "60-69",
                                   "70-79",
                                   "80-89",
                                   "90-99",
                                   "100+"),
                          labels=c(
                            "00-09",
                            "10-19",
                            "20-29",
                            "30-39",
                            "40-49",
                            "50-59",
                            "60-69",
                            "70-79",
                            "80-89",
                            "90-99",
                            "100+"))) 
write.csv(case_df_by_age_and_sex, file="data/processed_data/case_df_by_age_and_sex.csv")

pop_df = pop_df %>% filter(age_group_by=="age_group_10") %>% mutate(age_group=ifelse(age_group%in%c("90-99","100+"),"90+",age_group)) #Put 100+ in the 90-99

#DOFGY BELOW
# case_df_by_age_and_sex2 = nominal_df %>%
#   mutate(age_group_10=ifelse(age_group_10%in%c("90-99","100+"),"90+",age_group_10)) %>% #Put 100+ in the 90-99
#   filter(classification!="DESCARTADO") %>%#Remove discarded cases
#   filter(disease =="CHIKUNGUNYA") %>%
#   filter(sex =="MASCULINO" | sex =="FEMENINO") %>%
#   filter(date >= as.Date('2022-09-01') & date <= as.Date('2023-09-30')) %>% 
#   group_by(sex,age_group_10) %>% 
#   summarise(i_cases = n()) %>% 
#   drop_na() %>% 
#   full_join(., nominal_df %>%  mutate(age_group_10=ifelse(age_group_10%in%c("90-99","100+"),"90+",age_group_10)) %>% #Put 100+ in the 90-99
#               filter(classification!="DESCARTADO") %>%#Remove discarded cases
#               filter(dead_classification == "CONFIRMADO") %>%#Remove discarded cases
#               filter(disease =="CHIKUNGUNYA") %>%
#               filter(sex =="MASCULINO" | sex =="FEMENINO") %>%
#               filter(date >= as.Date('2022-09-01') & date <= as.Date('2023-09-30')) %>% 
#               group_by(sex,age_group_10) %>% 
#               summarise(d_cases = n()) %>% 
#               drop_na(), 
#             by = c("sex","age_group_10")) %>% 
#   mutate(sex = gsub("MASCULINO","M", sex)) %>%
#   mutate(sex = gsub("FEMENINO","F", sex)) %>% 
#   full_join(., pop_df %>% subset(age_group_by == "age_group_10" &
#                                    demographic_level != "COUNTRY"),
#             by = c("age_group_10"="age_group", "sex"="demographic_level")) %>% 
#   
#   
#   mutate(cfr = 100*d_cases/i_cases,
#          incidence = 10000*i_cases/projection,
#          age_specific_mortality = 10000*d_cases/projection)  %>% 
#   replace(is.na(.), 0) %>% 
#   
#   
#   dplyr::rename("Age_group"="age_group_10",
#                 "Sex" = "sex" ) %>% 
#   full_join(.,sero_df_as %>% 
#               select(Sex,Age_group,Estimate), by = c("Sex","Age_group")) %>% 
#   mutate(Estimate_m = mean(sero_df_as$Estimate),
#          CI_fm = projection*Estimate_m/100,
#          CI_f = projection*Estimate/100,
#          CI_0 = projection*0.0185, #0.001,
#          TI_m = CI_fm-CI_0,
#          TI = CI_f-CI_0,
#          IFR= 100*d_cases/TI,
#          IFR_m= 100*d_cases/TI_m,
#          pD = i_cases/TI_m) %>% 
#   mutate(Age_group=factor(Age_group,
#                           levels=c("00-09",
#                                    "10-19",
#                                    "20-29",
#                                    "30-39",
#                                    "40-49",
#                                    "50-59",
#                                    "60-69",
#                                    "70-79",
#                                    "80-89",
#                                    "90+"),
#                           labels=c(
#                             "00-09",
#                             "10-19",
#                             "20-29",
#                             "30-39",
#                             "40-49",
#                             "50-59",
#                             "60-69",
#                             "70-79",
#                             "80-89",
#                             "90+"))) 
# 
# write.csv(case_df_by_age_and_sex2, file="data/processed_data/case_df_by_age_and_sex2.csv")

require(ggplot2)
require(patchwork)
library(latex2exp)

case_df_by_age_and_sex = read.csv("data/processed_data/case_df_by_age_and_sex.csv")

theme_set(theme_minimal()+
            theme(axis.title = element_text(size=20),
                  legend.title = element_text(size=15),
                  legend.text = element_text(size=15),
                  strip.text = element_text(size=15),
                  axis.text = element_text(size=15),
                  plot.title = element_text(size=20)))


df_sexIFR = full_join(case_df_by_age_and_sex %>% 
                     subset(Sex != "M") %>% 
                     select(Age_group, IFR_m) %>% 
                     dplyr::rename(F= IFR_m), 
                   case_df_by_age_and_sex %>% 
                     subset(Sex != "F") %>% 
                     select(Age_group, IFR_m) %>% 
                     dplyr::rename(M= IFR_m), by = "Age_group") %>% 
  mutate(IFRm_Ratio =M/F) 



df_sexIFR$IFRm_Ratio[is.infinite(df_sexIFR$IFRm_Ratio)] <- NA

df_sexIFR$IFRm_Ratio[df_sexIFR$IFRm_Ratio==0] <- NA

# plot

df_sexIFR = df_sexIFR  %>% 
  mutate(mycolor = ifelse(IFRm_Ratio>1, "type1", "type2"))



df_sex = full_join(case_df_by_age_and_sex %>% 
                 subset(Sex != "M") %>% 
                 select(Age_group, pD) %>% 
                   dplyr::rename(F= pD), 
                 case_df_by_age_and_sex %>% 
                 subset(Sex != "F") %>% 
                 select(Age_group, pD) %>% 
                   dplyr::rename(M= pD), by = "Age_group") %>% 
  mutate(Pr_Ratio =F/M,
         mutate= mean(Pr_Ratio)) 

# plot

df_sex = df_sex %>% 
  mutate(mycolor = ifelse(Pr_Ratio>1, "type1", "type2"))

p_pD = gg4 +
  inset_element(gg5, left = 0.05, bottom = 0.399, right = 0.48, top = 1) 

p_pD

#inset_element(gg2, left = 0.05, bottom = 0.399, right = 0.4, top = 1)



p_mort = case_df_by_age_and_sex %>% 
  filter(Age_group != "100+") %>% 
  ggplot(aes(y = age_specific_mortality, x = as.factor(Age_group))) + 
  scale_fill_manual(values = c("#FFC107","#1E88E5"))+
  geom_bar(
  aes(fill = as.factor(Sex)),
  stat = "identity",
  position = "dodge",
  alpha = 1
) + guides(fill = "none") +
  xlab("Age group") + ylab("Age-specific mortality 10,000")

p_mort 


p_cfr = ggplot(case_df_by_age_and_sex %>% subset(Age_group != "100+"), aes(y = cfr, x = as.factor(Age_group))) + 
  geom_bar(
    aes(fill = as.factor(Sex)),
    stat = "identity",
    position = "dodge",
    alpha = 0.5
  ) + theme(text = element_text(
    family = "sans",
    face = "plain",
    color = "#000000",
    size = 15,
    hjust = 0.5,
    vjust = 0.5
  )) + guides(fill = guide_legend(title = "Sex")) +
  xlab("Age group") + ylab("CFR (%)")

p_cfr


cowplot::plot_grid(p_inc,p_mort, ncol=2)

ggsave(
  "output/p_inc_&_cfr_by_age_group10_20231113.pdf",
  plot = cowplot::plot_grid(p_inc,p_cfr, ncol=2),
  width = 22.65248*2,
  height = 14,
  units = "cm",
  dpi = 500,
  scale=1
)

ggsave(
  "output/p_inc_&_cfr_by_age_group10_20231113.svg",
  plot = cowplot::plot_grid(p_inc,p_cfr, ncol=2),
  width = 22.65248*2,
  height = 14,
  units = "cm",
  dpi = 500,
  scale=1
)




# 8. Map  ----

dpt_by_eje = read.csv("data/metadata/dpts_by_eje.csv")

case_df_by_subregion = nominal_df %>%
  filter(classification!="DESCARTADO") %>%#Remove discarded cases
  filter(disease =="CHIKUNGUNYA") %>%
  filter(date >= as.Date('2022-10-01') & date <= as.Date('2023-09-23')) %>% 
  group_by(Eje) %>% 
  summarise(i_cases = n()) %>% 
  drop_na() %>% 
  full_join(., nominal_df %>%
              filter(classification!="DESCARTADO") %>%#Remove discarded cases
              filter(dead_classification == "CONFIRMADO") %>%#Remove discarded cases
              filter(disease =="CHIKUNGUNYA") %>%
              filter(date >= as.Date('2022-09-01') & date <= as.Date('2023-09-30')) %>% 
              group_by(Eje) %>% 
              summarise(d_cases = n()), 
            by = "Eje") %>% 
  left_join(., pop_df_by_loc, by = c("Eje"="name")) %>% 
  mutate(cfr = 100*d_cases/i_cases,
         incidence = 10000*i_cases/Population,
         specific_mortality = 10000*d_cases/Population) 

#Import map
paraguay = sf::st_as_sf(raster::getData('GADM', country='PRY', level=1))  %>% 
  mutate(NAME_1 = gsub("Presidente","Pte", NAME_1)) %>% 
  mutate(NAME_1 = toupper(iconv(NAME_1,to="ASCII//TRANSLIT"))) %>% 
  mutate(Eje = dpt_by_eje$Eje[sapply(NAME_1, function(x) which(dpt_by_eje$Department==x))]) %>% 
  left_join(., case_df_by_subregion, by = "Eje")

#paraguay = sf::st_as_sf(raster::getData('GADM', country='PRY', level=1)) 

summary(paraguay$incidence)

library(ggforce)
library(scales)

# https://ggplot2.tidyverse.org/reference/scale_brewer.html

max_inc = round(max(paraguay$incidence),1)

def_breaks = c(max_inc*0.95, max_inc*0.50 , max_inc*0.30) |> round()

names(sero)

theme_set(theme_minimal())
p_inc_map = paraguay %>% group_by(Eje) %>% summarise() %>%
  left_join(., case_df_by_subregion, by = "Eje") %>% 
  ggplot() +
  geom_sf(aes(fill=incidence)) +
  geom_sf_text(aes(label = round(incidence,1)), size = 5) +
  scale_fill_gradient2(high = "#cf3a36",
                       mid = "#e78429",
                       midpoint= log(120),
                       low ="white",
                       trans  = "log",
                       breaks = def_breaks,) +
  ggspatial::annotation_scale(bar_cols=c("gray50","gray80"), line_width=0.1,text_cex=1,pad_x = unit(0.1,"npc"), pad_y = unit(0.2,"npc"), location="tr")+
  ggthemes::theme_map() +theme(legend.position = "none")

p_inc_map

ggsave(
  "output/p_inc_map_gabriel.pdf",
  plot = p_inc_map,
  width = 22.65248,
  height = 14,
  units = "cm",
  dpi = 500,
  scale=1
)

ggsave(
  "output/p_inc_map_gabriel.svg",
  plot = p_inc_map,
  width = 22.65248,
  height = 14,
  units = "cm",
  dpi = 500,
  scale=1
)

# Seroprevalence map ####

max_sero = round(max(sero_df$Estimate, na.rm = T),1)

def_breaks = c(max_sero*0.95, max_sero*0.50 , max_sero*0.30) |> round()

sero2 = sero %>% 
  #mutate(longitude_app=signif(longitude,3), latitude_app=signif(latitude,3)) %>%
  mutate(longitude_app=round(longitude*2)/2, latitude_app=round(latitude*2)/2) %>%
  group_by(longitude_app,latitude_app) %>% 
  summarise(Samples = n(),latitude=mean(latitude),longitude=mean(longitude))

p_sero_map = paraguay %>% group_by(Eje) %>% summarise() %>%
  left_join(., sero_df %>% 
              subset(level == "Geographic" & name_0 != "Paraguay") %>% 
              dplyr::rename(Eje=name_0) %>% 
              select(Eje,Estimate), by = "Eje") %>% 
  #drop_na() %>% 
  ggplot() +
  geom_sf(fill = "gray80",alpha = 1) +
  
  geom_point(data = sero2,
             aes(x=longitude, y=latitude, size = Samples), fill="#e78429", alpha =0.5, pch=21) +
  
  scale_size_area(max_size = 20, breaks=c(1,50,200)) +
  
  geom_sf_text(aes(label = Eje), size = 7) +
  ggthemes::theme_map() +
#  labs(size =paste('Sample size',' by locality', sep = '\n')) +
  labs(size = 'Sample size') +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  ggspatial::annotation_scale(bar_cols=c("gray50","gray80"), line_width=0.1,text_cex=1,pad_x = unit(0.1,"npc"), pad_y = unit(0.2,"npc"), location="tr")+
  theme(legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.position.inside = c(0.15,0.15))  

p_sero_map

ggsave(
  "output/fig/p_sero_map_gabriel.pdf",
  plot = p_sero_map,
  width = 22.65248,
  height = 14,
  units = "cm",
  dpi = 500,
  scale=1
)

ggsave(
  "output/fig/p_sero_map_gabriel.svg",
  plot = p_sero_map,
  width = 22.65248,
  height = 14,
  units = "cm",
  dpi = 500,
  scale=1
)

max_cfr = round(max(paraguay$cfr),2)


ggplot()+
  geom_sf(data=paraguay, aes(fill=cfr), alpha = 0.7) +
  #geom_point(data=meta_weather, aes(x=lon_dec, y=lat_dec), size=3) +
  ggthemes::theme_map()  +
  scale_fill_distiller(name = 'CFR (%)',
                       palette = "Oranges",
                       breaks = c(0.26, 0.16, 0.08)
 ) + 
  #  scale_fill_fermenter(palette = "Spectral")
  theme(legend.text = element_text(size=20),
        legend.title = element_text(size=25))  

p_heatmap_inc

p_prop_conf

cowplot::plot_grid(p_sero_map,
                   p_sero,
                   ncol=1,
                   labels = "AUTO",
                   rel_widths = c(2,1),
                   rel_heights = c(1.5,1.5))

cowplot::plot_grid(gg0,
                   gg1 + theme(legend.position = "none"), 
                   gg4 + theme(legend.position = "none"),
                   ncol=1,
                   labels = "AUTO",
                   rel_widths = c(1,1,1),
                   rel_heights = c(1.3,1,1))

p_seroprevalence = cowplot::plot_grid(cowplot::plot_grid(p_sero_map,
                                                         p_sero,
                                                         ncol=1,
                                                         labels = c('A', 'B'),
                                                         rel_widths = c(2,1),
                                                         rel_heights = c(2,1.5)),
                                      cowplot::plot_grid(gg0,
                                                         gg1 + theme(legend.position = "none"), 
                                                         gg4 + theme(legend.position = "none"),
                                                         ncol=1,
                                                         labels = c('C', 'D', 'E'),
                                                         rel_widths = c(1,1,1),
                                                         rel_heights = c(1.2,1,1)),
                                      ncol=2
                                      #scale = c(1.2,1,1,1)
)


ggsave(
  "output/fig/p_seroprevalence_20240228.pdf",
  plot = p_seroprevalence,
  width = 22.65248*2.5,
  height = 14*3.2,
  units = "cm",
  dpi = 500,
  scale=1
)

ggsave(
  "output/fig/p_seroprevalence_20240228.svg",
  plot = p_seroprevalence,
  width = 22.65248*2.5,
  height = 14*3.2,
  units = "cm",
  scale=1
)

ggsave(
  "output/fig/p_seroprevalence_20240228.png",
  plot = p_seroprevalence,
  width = 22.65248*2.5,
  height = 14*3.2,
  units = "cm",
  dpi = 500,
  scale=1
)


p_supl1 = cowplot::plot_grid(gg2,
                           gg3,
                   ncol=2,labels = "AUTO"#, 
                   #scale = c(1,1,1,1)
                   )

p_supl1 

ggsave(
  "output/fig/p_IFRm-IFR-CFR.pdf",
  plot = p_supl1 ,
  width = 22.65248*2,
  height = 14,
  units = "cm",
  dpi = 500,
  scale=1
)

ggsave(
  "output/fig/p_IFRm-IFR-CFR.svg",
  plot = p_supl1,
  width = 22.65248*2,
  height = 14,
  units = "cm",
  scale=1
)

ggsave(
  "output/fig/p_IFRm-IFR-CFR.png",
  plot = p_supl1 ,
  width = 22.65248*2,
  height = 14,
  units = "cm",
  dpi = 500,
  scale=1
)



p_supl2 = cowplot::plot_grid(
                            gg5,
                            gg6,
                            ncol=2,labels = "AUTO"#, 
                            #scale = c(1,1,1,1)
)

p_supl2 

ggsave(
  "output/fig/ratio.pdf",
  plot = p_supl2 ,
  width = 22.65248*2,
  height = 14,
  units = "cm",
  dpi = 500,
  scale=1
)

ggsave(
  "output/fig/ratio.svg",
  plot = p_supl2,
  width = 22.65248*2,
  height = 14,
  units = "cm",
  scale=1
)

ggsave(
  "output/fig/ratio.png",
  plot = p_supl2 ,
  width = 22.65248*2,
  height = 14,
  units = "cm",
  dpi = 500,
  scale=1
)


p_descriptive = cowplot::plot_grid(p_inc_map,
                                   p_epicurva,
                                   p_inc,p_mort,
                                   ncol=2,labels = "AUTO"#, 
                                   #scale = c(1,1,1,1)
)

p_descriptive 

ggsave(
  "output/fig/p_descriptive_20231113.pdf",
  plot = p_descriptive,
  width = 22.65248*2,
  height = 14*2,
  units = "cm",
  dpi = 500,
  scale=1
)

ggsave(
  "output/fig/p_descriptive_20231113.svg",
  plot = p_descriptive,
  width = 22.65248*2,
  height = 14*2.2,
  units = "cm",
  dpi = 500,
  scale=1
)

ggsave(
  "output/fig/p_descriptive_20231113.png",
  plot = p_descriptive,
  width = 22.65248*2,
  height = 14*2.2,
  units = "cm",
  dpi = 500,
  scale=1
)


#PLOTS HARMONIZED---
p_inc = case_df_by_age_and_sex %>%
  subset(Age_group != "100+") %>% 
  drop_na(incidence) %>% 
  ggplot(aes(y = incidence, x = as.factor(Age_group))) +
  scale_fill_manual(values = c("#94b594","#376795"))+geom_bar(
    aes(fill = as.factor(Sex)),
    stat = "identity",
    position = "dodge",
    alpha = 1
  )  +  guides(fill = guide_legend(title = "Sex")) + 
  xlab("Age group") + ylab("Incidence per 10,000")+
  theme_minimal()+
  theme(axis.title = element_text(size=10),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        strip.text = element_text(size=10),
        axis.text = element_text(size=10),
        plot.title = element_text(size=10))+
  theme(axis.text.x = element_text(angle=90))

incidence_map = p_inc_map +
  inset_element(p_inc, left = 0.05, bottom = 0.05, right = 0.5, top = 0.4) 

p_deaths = p_mort = case_df_by_age_and_sex %>% 
  filter(Age_group != "100+") %>% 
  ggplot(aes(y = d_cases, x = as.factor(Age_group))) + 
  scale_fill_manual(name="Sex",values = c("#94b594","#376795"))+
  geom_bar(
    aes(fill = as.factor(Sex)),
    stat = "identity",
    position = "dodge",
    alpha = 1
  ) + 
  xlab("Age group") + ylab("Deaths reported")

fig_1 = cowplot::plot_grid(incidence_map,p_deaths,nrow=1,labels = "AUTO", label_size = 30)
fig_1
ggsave(fig_1, filename="output/fig/figure_1_descriptive.png", width=15, height=7.5)  


gg0 = case_df_by_age_and_sex %>%
  subset(Age_group != "100+") %>% 
  ggplot(aes(x=Age_group)) + 
  scale_fill_manual(values = c("#94b594","#376795"))+
  geom_bar(aes(y=cfr,fill = Sex),
           stat = "identity",
           position = "dodge",
           alpha = 1)  +
  guides(fill=guide_legend(title=" ")) +
  ylab(expression(paste("CFR"," %"))) +
  xlab("Age group")  

gg0

gg1 = case_df_by_age_and_sex %>%
  subset(Age_group != "100+") %>% 
  ggplot(aes(x=Age_group)) + 
  scale_fill_manual(values = c("#94b594","#376795"))+
  geom_bar(aes(y=IFR_m,fill = Sex),
           stat = "identity",
           position = "dodge",
           alpha = 1)  +
  guides(fill=guide_legend(title=" ")) +
  ylab("IFR (%)") +
  xlab("Age group")  

gg1



gg4 = case_df_by_age_and_sex %>%
  subset(Age_group != "100+") %>% 
  ggplot(aes(x=Age_group)) + 
  scale_fill_manual(values = c("#94b594","#376795"))+
  geom_bar(aes(y=pD*100,fill = Sex),
           stat = "identity",
           position = "dodge",
           alpha = 1)  +
  guides(fill=guide_legend(title=" ")) +
  ylab("Infections detected (%)") +
  xlab("Age group")

gg4

p_props = case_df_by_age_and_sex %>%
  subset(Age_group != "100+") %>%
  tidyr::pivot_longer(cols = c(pD,IFR_m,cfr)) %>% 
  mutate(name=ifelse(name=="pD","Prob. detection",ifelse(name=="IFR_m","IFR (%)","CFR (%)"))) %>%
  mutate(name=factor(name, levels=c("Prob. detection","CFR (%)","IFR (%)"))) %>%
  ggplot(aes(x=Age_group)) + 
  scale_fill_manual(values = c("#94b594","#376795"))+
  geom_bar(aes(y=value,fill = Sex),
           stat = "identity",
           position = "dodge",
           alpha = 1)  +
  facet_wrap(~name, nrow=3, scales = "free_y")+
  guides(fill=guide_legend(title=" ")) +
  ylab("") +
  xlab("Age group")+
  theme(axis.text.x = element_text(angle=90))

p_props
p_sero
p_sero_map


fig_2 = gridExtra::grid.arrange(grobs=list(p_sero_map,p_sero,p_props),
                        layout_matrix=rbind(c(1,3),
                                            c(1,3),
                                            c(2,3)),
                        
                        widths=c(2,1),
                        
                        heights=c(1,1,1))

fig_2

ggsave(fig_2, filename="output/fig/figure_2_sero.png", width=15, height=15)  
ggsave(fig_2, filename="output/fig/figure_2_sero.pdf", width=15, height=15)  




