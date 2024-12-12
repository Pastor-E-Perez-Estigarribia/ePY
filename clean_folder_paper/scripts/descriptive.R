library(tidyverse)
library(dplyr)
library(ggplot2)

rm(list=ls())

case_df_by_age_and_sex = read.csv("data/processed_data/case_df_by_age_and_sex.csv")
(7518+5350)/sum(case_df_by_age_and_sex$i_cases)
(7518+5350)/sum(case_df_by_age_and_sex %>% filter(Age_group!="<1") %>% select(i_cases))


# 1. Instalar/Cargar paquetes ####
pacman::p_load(tidyverse, # Manejo y tratamiento de datos
               rio,       # Importación y exportación de bases de datos
               abjutils,
               summarytools,
               naniar,     # Limpieza de texto
               shiny,
               ggraptR) # interactive plot


# I. Chikungunya ----

pop_df_by_loc = read.csv("data/processed_data/pop_data.csv", row.names=1) 

#nominal_df = read.csv("data/processed_data/nominal_data_20231106.csv")

nominal_df = read.csv("data/processed_data/nominal_data_2024012430.csv") %>%
  mutate(age_group_henrik=as.factor(cut(age,breaks=c(0,1,seq(10,70,by=10),120), right = F,
                                        labels = c("<1","01-09",paste0(seq(10,60,by=10),"-",seq(19,69,by=10)),">70"))))


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

# #Create data frames per administrative levels
# 
# names(nominal_df)
# 
# case_df_by_dpt = nominal_df %>%
#   dplyr::rename(Departamento_desc = "department_desc") %>% 
#   select(Departamento_desc, Eje, date, classification, disease)  %>% 
#   filter(classification!="DESCARTADO") %>%#Remove discarded cases
#   group_by(Departamento_desc, Eje, date, classification, disease) %>% 
#   mutate(i_cases = n()) %>%
#   distinct() %>% 
#   ungroup() %>%
#   filter(!is.na(Departamento_desc)) %>% 
#   tidyr::pivot_wider(names_from=classification, values_from=i_cases) %>%
#   tidyr::replace_na(list(SOSPECHOSO=0, CONFIRMADO=0,PROBABLE=0)) %>%
#   mutate(TOTAL=SOSPECHOSO+CONFIRMADO+PROBABLE) %>%
#   tidyr::pivot_longer(cols=c(SOSPECHOSO, CONFIRMADO, PROBABLE, TOTAL), names_to="classification", values_to="i_cases") %>%
#   dplyr::rename(name=Departamento_desc) %>%
#   mutate(level="Department") %>%
#   relocate(date, level) 
# 
# 
# tapply(case_df_by_dpt$i_cases, list(case_df_by_dpt$classification, case_df_by_dpt$disease), sum)
# 
# 
# case_df_by_eje = case_df_by_dpt %>% 
#   group_by(Eje, date, classification, disease) %>%
#   mutate(i_cases=sum(i_cases)) %>%
#   ungroup() %>%
#   select(-name) %>%
#   distinct() %>%
#   dplyr::rename(name=Eje) %>%
#   mutate(level="Eje") %>%
#   relocate(date, level) 
# 
# case_df_national = case_df_by_dpt %>% 
#   group_by(date, classification, disease) %>%
#   mutate(i_cases=sum(i_cases)) %>%
#   ungroup() %>%
#   select(-c(name,Eje)) %>%
#   distinct() %>%
#   mutate(level="National",name="Paraguay") %>%
#   relocate(date, level) 
# 
# case_df = rbind(case_df_by_dpt %>% select(-Eje), case_df_by_eje, case_df_national)
# 
# #write.csv(case_df, "data/processed_data/case_data_2019_2023_epiweek_v20240124.csv", row.names = F)
# 
# #________
# 
# case_df$date = as.Date(case_df$date)
# 
# class(case_df$date)
# 
# summary(case_df$date)
# 
# case_df$date = as.Date(case_df$date)


# 6. Seroprevalence ####

sero_10 = import("https://docs.google.com/spreadsheets/d/1dYHa-PfNh3LQXtfZ8Ra4VOX6MIfQYhY1/edit#gid=234901776") %>% 
  dplyr::rename(age = Edad,
                Sex = Sexo,
                Seropositive = Resultado) %>% 
  
  mutate(Age_group=as.factor(cut(age,breaks=c(0,1,seq(10,70,by=10),120), right = F,
                                 labels = c("<1","01-09",paste0(seq(10,60,by=10),"-",seq(19,69,by=10)),">70")))) %>%
  
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
  coord_flip() +
  theme(axis.text.x = element_text(angle=0))

p_sero

#write.csv(sero_df, 'output/sero_df.csv')

# 7. cases by sex and age ----
pop_df <- import("data/raw_data/Paraguay-2023.csv", encoding = "Latin-1")
pop_df2 = pop_df %>% filter(age_group_by=="age_group_5")
pop_df2 = data.frame(age_group=c("<1","01-09",paste0(seq(10,60,by=10),"-",seq(19,69,by=10)),">70"),
                     age_group_by="age_group_henrik",
                     COUNTRY = c(0.2*pop_df2$COUNTRY[1],
                                 sum(0.8*pop_df2$COUNTRY[1],pop_df2$COUNTRY[2]),
                                 sapply(1:6 ,function(id) sum(pop_df2$COUNTRY[(2*id+1):(2*id+2)])),
                                 sum(pop_df2$COUNTRY[15:21])),
                     F = c(0.2*pop_df2$F[1],
                           sum(0.8*pop_df2$F[1],pop_df2$F[2]),
                           sapply(1:6 ,function(id) sum(pop_df2$F[(2*id+1):(2*id+2)])),
                           sum(pop_df2$F[15:21])),
                     M = c(0.2*pop_df2$M[1],
                           sum(0.8*pop_df2$M[1],pop_df2$M[2]),
                           sapply(1:6 ,function(id) sum(pop_df2$M[(2*id+1):(2*id+2)])),
                           sum(pop_df2$M[15:21])))

pop_df = rbind(pop_df, pop_df2) %>% 
  pivot_longer(!c(age_group,age_group_by), 
               names_to = "demographic_level", 
               values_to = "projection")
rm(pop_df2)

case_df_by_age_and_sex = nominal_df %>%
  filter(classification!="DESCARTADO") %>%#Remove discarded cases
  filter(disease =="CHIKUNGUNYA") %>%
  filter(sex =="MASCULINO" | sex =="FEMENINO") %>%
  filter(date >= as.Date('2022-09-01') & date <= as.Date('2023-09-30')) %>% 
  group_by(sex,age_group_henrik) %>% 
  summarise(i_cases = n()) %>% 
  drop_na() %>% 
  full_join(., nominal_df %>%
              filter(classification!="DESCARTADO") %>%#Remove discarded cases
              filter(dead_classification == "CONFIRMADO") %>%#Remove discarded cases
              filter(disease =="CHIKUNGUNYA") %>%
              filter(sex =="MASCULINO" | sex =="FEMENINO") %>%
              filter(date >= as.Date('2022-09-01') & date <= as.Date('2023-09-30')) %>% 
              group_by(sex,age_group_henrik) %>% 
              summarise(d_cases = n()) %>% 
              drop_na(), 
            by = c("sex","age_group_henrik")) %>% 
  mutate(sex = gsub("MASCULINO","M", sex)) %>%
  mutate(sex = gsub("FEMENINO","F", sex)) %>%
  
  full_join(., pop_df %>% subset(age_group_by == "age_group_henrik" &
                                   demographic_level != "COUNTRY"),
            by = c("age_group_henrik"="age_group", "sex"="demographic_level")) %>% 
  
  mutate(cfr = 100*d_cases/i_cases,
         incidence = 10000*i_cases/projection,
         age_specific_mortality = 10000*d_cases/projection)  %>% 
  replace(is.na(.), 0) %>% 
  dplyr::rename("Age_group"="age_group_henrik",
                "Sex" = "sex" ) %>% 
  full_join(.,sero_df_as %>% 
              select(Sex,Age_group,Estimate), by = c("Sex","Age_group")) %>% 
  
  mutate(Estimate_m = 100*sum(sero_df_as$n_pos)/sum(sero_df_as$n_tot),
         CI_fm = projection*Estimate_m/100,
         CI_f = projection*Estimate/100,
         CI_0 = projection*0.0185, #0.001,
         TI_m = CI_fm-CI_0,
         TI = CI_f-CI_0,
         IFR= 100*d_cases/TI,
         IFR_m= 100*d_cases/TI_m,
         pD = i_cases/TI_m)

unique(case_df_by_age_and_sex$Age_group)

names(sero_df_as)

sero_df_as %>% 
  select(Sex,Age_group,Estimate)

case_df_by_age_and_sex$Sex = as.factor(case_df_by_age_and_sex$Sex)

case_df_by_age_and_sex

#%>% drop_na()
case_df_by_age_and_sex = case_df_by_age_and_sex %>% 
  mutate(Age_group = factor(Age_group, levels= c("<1","01-09",paste0(seq(10,60,by=10),"-",seq(19,69,by=10)),">70")))

write.csv(case_df_by_age_and_sex, file="data/processed_data/case_df_by_age_and_sex.csv")

pop_df = pop_df %>% filter(age_group_by=="age_group_10") %>% mutate(age_group=ifelse(age_group%in%c("90-99","100+"),"90+",age_group)) #Put 100+ in the 90-99

#Cases by Eje----------------------
pop_by_eje = read.csv("data/processed_data/pop_data.csv") %>% filter(level=="Eje") %>% rename(projection=Population)
case_df_by_eje = nominal_df %>%
  filter(classification!="DESCARTADO") %>%#Remove discarded cases
  filter(disease =="CHIKUNGUNYA") %>%
  filter(sex =="MASCULINO" | sex =="FEMENINO") %>%
  filter(date >= as.Date('2022-09-01') & date <= as.Date('2023-09-30')) %>% 
  group_by(Eje) %>% 
  summarise(i_cases = n()) %>% 
  drop_na() %>% 
  full_join(., nominal_df %>%
              filter(classification!="DESCARTADO") %>%#Remove discarded cases
              filter(dead_classification == "CONFIRMADO") %>%#Remove discarded cases
              filter(disease =="CHIKUNGUNYA") %>%
              filter(sex =="MASCULINO" | sex =="FEMENINO") %>%
              filter(date >= as.Date('2022-09-01') & date <= as.Date('2023-09-30')) %>% 
              group_by(Eje) %>% 
              summarise(d_cases = n()) %>% 
              drop_na(), 
            by = c("Eje")) %>% 
  
  full_join(., pop_by_eje %>% select(projection, name), by = c("Eje"="name"))




#Numbers---------------------------
case_df_by_age_and_sex %>% select(Age_group, d_cases) %>%
  group_by(Age_group) %>% summarise(deaths=sum(d_cases)) %>%
  ungroup() %>%mutate(prop_deaths = deaths/sum(deaths))

case_df_by_age_and_sex %>% select(Sex, d_cases) %>%
  group_by(Sex) %>% summarise(deaths=sum(d_cases))

#Seroprevalence
sero_df %>% filter(name_0=="Paraguay") %>% select(Estimate, CI_Lower, CI_Upper)
sero_df %>% filter(name_0=="Paraguay") %>% select(Estimate, CI_Lower, CI_Upper)-1.85
prop.test(x = c(217*0.986+383*0.02, 123*0.986+278*0.02), n = c(600, 401)) #Statistical test - sex adjusted by sensitivty
prop.test(x = c(217, 123), n = c(600, 401)) #Statistical test - sex

0.321*sum(pop_df %>% filter(demographic_level=="COUNTRY") %>% select(projection))/1e6

#Table
sero_df_gab = sero %>% mutate(age_group_gab=cut(age,seq(0,100,b=10),right = F)) %>%
  group_by(age_group_gab) %>% summarise(n_tot = n(), n_pos = sum(Seropositive=="YES")) %>%
  mutate(Age_group = paste0(seq(10,60,by=10),"-",seq(19,69,by=10)))

#stats overall
stat_overall = lapply(c("m","lo","hi"), function(bound){case_df_by_age_and_sex %>% ungroup() %>% summarise(i_cases=sum(i_cases),
                                                                                             d_cases = sum(d_cases),
                                                                                             projection= sum(projection)) %>%
    mutate(n_pos = sum(sero_df_gab$n_pos), n_tot = sum(sero_df_gab$n_tot)) %>%
    reframe(bound=bound,
            cases=i_cases,
            deaths=d_cases,
            n_pos = n_pos,
            n_tot=n_tot,
            Estimate_m = 100*Hmisc::binconf(x = sum(sero_df_as$n_pos) * 0.986 + (sum(sero_df_as$n_tot)-sum(sero_df_as$n_pos)) * (1- 0.98),
                                            n = sum(sero_df_as$n_tot))[which(c("m","lo","hi")==bound)], #seroprevalence adjusted by kit sens/spec
            seroincidence = Estimate_m-1.85,
            CI_fm = projection*Estimate_m/100, # N seropos post outbresk
            CI_0 = projection*0.0185, #0.001, # N seropos pre outbreak
            n_infections = CI_fm-CI_0, # Seroincidence, N infected during outbreak
            IFR = 100*d_cases/n_infections,
            detection_rate = i_cases/n_infections,
            cfr = d_cases/i_cases*100)}) %>% 
  do.call(what=rbind) %>% mutate(Group="Total") %>% relocate(Group)


stat_overall_formatted = stat_overall %>%   tidyr::pivot_longer(cols = -c(Group,bound)) %>%
  tidyr::pivot_wider(id_cols = c(Group, name), names_from = bound, values_from = value) %>%
  tidyr::pivot_wider(id_cols = c(Group), names_from = name, values_from = c(m, lo, hi), names_sep = "~") %>%
  
  mutate(across(contains("Estimate")|contains("infections")|contains("IFR")|contains("detection")|contains("cfr")|contains("seroincidence"), ~format((signif(.x,3)), big.mark=",", scientific=F))) %>%
  mutate(across(contains("cases")|contains("deaths")|contains("n_tot"), ~format(.x, big.mark=",", scientific=F))) %>%
  mutate(across(everything(), ~as.character(.x))) %>%
  
  tidyr::pivot_longer(cols = -c(Group), names_to = "name", values_to = "values") %>%
  mutate(bound = as.factor(stringr::str_split_i(name,"~",1)),
         name = as.factor(paste0(stringr::str_split_i(name,"~",-1)))) %>%
  tidyr::pivot_wider(id_cols = c(Group, name), names_from = bound, values_from = values) %>%
  mutate(print_out = ifelse(m==lo,m,ifelse(lo<hi,paste0(m," [",lo,"-",hi,"]"),paste0(m," [",hi,"-",lo,"]")))) %>%
  select(-c(m,lo,hi)) %>%
  tidyr::pivot_wider(id_cols = c(Group), names_from = "name", values_from = "print_out") %>%
  select(Group, cases, n_pos, n_tot, deaths, Estimate_m, seroincidence, n_infections, IFR, detection_rate, cfr)


#stats by sex
stat_by_sex = lapply(c("m","lo","hi"), function(bound){case_df_by_age_and_sex %>% group_by(Sex) %>% summarise(i_cases=sum(i_cases),
                                                                                                              d_cases = sum(d_cases),
                                                                                                              projection= sum(projection)) %>%
    
    #left_join(sero_df%>%select(name_0, n_pos, n_tot), by = join_by(Sex==name_0)) %>% #Assume sex-specific seropev
    mutate(n_pos = 340, n_tot=1001) %>% #Assume overall seropev
    
    reframe(bound=bound,
            Sex=Sex,
            cases=i_cases,
            deaths=d_cases,
            n_pos = n_pos,
            n_tot=n_tot,
            Estimate_m = 100*Hmisc::binconf(x = n_pos * 0.986 + (n_tot - n_pos) * (1- 0.98), n = n_tot)[,which(c("m","lo","hi")==bound)], #seroprevalence adjusted by kit sens/spec
            seroincidence = Estimate_m-1.85,
            CI_fm = projection*Estimate_m/100, # N seropos post outbresk
            CI_0 = projection*0.0185, #0.001, # N seropos pre outbreak
            n_infections = CI_fm-CI_0, # Seroincidence, N infected during outbreak
            IFR = 100*d_cases/n_infections,
            detection_rate = i_cases/n_infections,
            cfr = d_cases/i_cases*100)}) %>% 
  do.call(what=rbind) %>%
  arrange(Sex)

stat_by_sex_formatted = stat_by_sex %>%   tidyr::pivot_longer(cols = -c(Sex,bound)) %>%
  tidyr::pivot_wider(id_cols = c(Sex, name), names_from = bound, values_from = value) %>%
  tidyr::pivot_wider(id_cols = c(Sex), names_from = name, values_from = c(m, lo, hi), names_sep = "~") %>%
  
  mutate(across(contains("Estimate")|contains("infections")|contains("IFR")|contains("detection")|contains("cfr")|contains("seroincidence"), ~format((signif(.x,3)), big.mark=",", scientific=F))) %>%
  mutate(across(contains("cases")|contains("deaths")|contains("n_tot"), ~format(.x, big.mark=",", scientific=F))) %>%
  mutate(across(everything(), ~as.character(.x))) %>%
  
  tidyr::pivot_longer(cols = -c(Sex), names_to = "name", values_to = "values") %>%
  mutate(bound = as.factor(stringr::str_split_i(name,"~",1)),
         name = as.factor(paste0(stringr::str_split_i(name,"~",-1)))) %>%
  tidyr::pivot_wider(id_cols = c(Sex, name), names_from = bound, values_from = values) %>%
  mutate(print_out = ifelse(m==lo,m,ifelse(lo<hi,paste0(m," [",lo,"-",hi,"]"),paste0(m," [",hi,"-",lo,"]")))) %>%
  select(-c(m,lo,hi)) %>%
  tidyr::pivot_wider(id_cols = c(Sex), names_from = "name", values_from = "print_out") %>%
  select(Sex, cases, n_pos, n_tot, deaths, Estimate_m, seroincidence, n_infections, IFR, detection_rate, cfr)

#By age
stat_by_age = lapply(c("m","lo","hi"), function(bound){case_df_by_age_and_sex %>% group_by(Age_group) %>% summarise(i_cases=sum(i_cases),
                                                                                                                    d_cases = sum(d_cases),
                                                                                                                    projection= sum(projection)) %>%
    
    #left_join(sero_df_gab%>%select(Age_group, n_pos, n_tot), by = join_by(Age_group==Age_group)) %>% ungroup() %>% #Assume age-specific seroprevalence
    #mutate(n_pos=ifelse(is.na(n_pos),340,n_pos), n_tot=ifelse(is.na(n_tot),1001,n_tot)) %>% #If no samples in a category, assume overall seroprevalence
    mutate(n_pos = 340, n_tot=1001) %>%
    
    reframe(bound=bound,
            cases=i_cases,
            deaths=d_cases,
            n_pos = n_pos,
            n_tot=n_tot,
            Age_group=Age_group,
            projection=projection,
            n_pos = n_pos, n_tot=n_tot,
            Estimate_m = 100*Hmisc::binconf(x = n_pos * 0.986 + (n_tot - n_pos) * (1- 0.98), n = n_tot)[,which(c("m","lo","hi")==bound)], #seroprevalence adjusted by kit sens/spec
            seroincidence = Estimate_m-1.85,
            CI_fm = projection*Estimate_m/100, # N seropos post outbresk
            CI_0 = projection*0.0185, #0.001, # N seropos pre outbreak
            n_infections = CI_fm-CI_0, # Seroincidence, N infected during outbreak
            IFR = 100*d_cases/n_infections,
            detection_rate = i_cases/n_infections,
            cfr = d_cases/i_cases*100)}) %>% 
  do.call(what=rbind) %>%
  arrange(Age_group)

stat_by_age_formatted = stat_by_age %>%   tidyr::pivot_longer(cols = -c(Age_group,bound)) %>%
  tidyr::pivot_wider(id_cols = c(Age_group, name), names_from = bound, values_from = value) %>%
  tidyr::pivot_wider(id_cols = c(Age_group), names_from = name, values_from = c(m, lo, hi), names_sep = "~") %>%
  
  mutate(across(contains("Estimate")|contains("infections")|contains("IFR")|contains("detection")|contains("cfr")|contains("seroincidence"), ~format((signif(.x,3)), big.mark=",", scientific=F))) %>%
  mutate(across(contains("cases")|contains("deaths")|contains("n_tot"), ~format(.x, big.mark=",", scientific=F))) %>%
  mutate(across(everything(), ~as.character(.x))) %>%
  
  tidyr::pivot_longer(cols = -c(Age_group), names_to = "name", values_to = "values") %>%
  mutate(bound = as.factor(stringr::str_split_i(name,"~",1)),
         name = as.factor(paste0(stringr::str_split_i(name,"~",-1)))) %>%
  tidyr::pivot_wider(id_cols = c(Age_group, name), names_from = bound, values_from = values) %>%
  mutate(print_out = ifelse(m==lo,m,ifelse(lo<hi,paste0(m," [",lo,"-",hi,"]"),paste0(m," [",hi,"-",lo,"]")))) %>%
  select(-c(m,lo,hi)) %>%
  tidyr::pivot_wider(id_cols = c(Age_group), names_from = "name", values_from = "print_out") %>%
  select(Age_group,  cases, deaths, n_pos, n_tot, seroincidence, Estimate_m, n_infections, IFR, detection_rate, cfr)

#By age and sex
stat_by_age_and_sex = lapply(c("m","lo","hi"), function(bound){case_df_by_age_and_sex %>% group_by(Age_group, Sex) %>% summarise(i_cases=sum(i_cases),
                                                                                                                    d_cases = sum(d_cases),
                                                                                                                    projection= sum(projection)) %>%
    
    #left_join(sero_df_gab%>%select(Age_group, n_pos, n_tot), by = join_by(Age_group==Age_group)) %>% ungroup() %>% #Assume age-specific seroprevalence
    #mutate(n_pos=ifelse(is.na(n_pos),340,n_pos), n_tot=ifelse(is.na(n_tot),1001,n_tot)) %>% #If no samples in a category, assume overall seroprevalence
    mutate(n_pos = 340, n_tot=1001) %>%
    
    reframe(bound=bound,
            cases=i_cases,
            deaths=d_cases,
            n_pos = n_pos,
            n_tot=n_tot,
            Sex=Sex,
            Age_group=Age_group,
            projection=projection,
            n_pos = n_pos, n_tot=n_tot,
            Estimate_m = 100*Hmisc::binconf(x = n_pos * 0.986 + (n_tot - n_pos) * (1- 0.98), n = n_tot)[,which(c("m","lo","hi")==bound)], #seroprevalence adjusted by kit sens/spec
            seroincidence = Estimate_m-1.85,
            CI_fm = projection*Estimate_m/100, # N seropos post outbresk
            CI_0 = projection*0.0185, #0.001, # N seropos pre outbreak
            n_infections = CI_fm-CI_0, # Seroincidence, N infected during outbreak
            IFR = 100*d_cases/n_infections,
            detection_rate = i_cases/n_infections,
            cfr = d_cases/i_cases*100)}) %>% 
  do.call(what=rbind) %>%
  arrange(Age_group)

stat_by_age_and_sex_formatted = stat_by_age_and_sex %>%   tidyr::pivot_longer(cols = -c(Age_group,Sex,bound)) %>%
  tidyr::pivot_wider(id_cols = c(Age_group,Sex, name), names_from = bound, values_from = value) %>%
  tidyr::pivot_wider(id_cols = c(Age_group,Sex), names_from = name, values_from = c(m, lo, hi), names_sep = "~") %>%
  
  mutate(across(contains("Estimate")|contains("infections")|contains("IFR")|contains("detection")|contains("cfr")|contains("seroincidence"), ~format((signif(.x,3)), big.mark=",", scientific=F))) %>%
  mutate(across(contains("cases")|contains("deaths")|contains("n_tot"), ~format(.x, big.mark=",", scientific=F))) %>%
  mutate(across(everything(), ~as.character(.x))) %>%
  
  tidyr::pivot_longer(cols = -c(Age_group,Sex), names_to = "name", values_to = "values") %>%
  mutate(bound = as.factor(stringr::str_split_i(name,"~",1)),
         name = as.factor(paste0(stringr::str_split_i(name,"~",-1)))) %>%
  tidyr::pivot_wider(id_cols = c(Age_group,Sex, name), names_from = bound, values_from = values) %>%
  mutate(print_out = ifelse(m==lo,m,ifelse(lo<hi,paste0(m," [",lo,"-",hi,"]"),paste0(m," [",hi,"-",lo,"]")))) %>%
  select(-c(m,lo,hi)) %>%
  tidyr::pivot_wider(id_cols = c(Age_group,Sex), names_from = "name", values_from = "print_out") %>%
  select(Age_group,Sex,  cases, deaths, n_pos, n_tot, seroincidence, Estimate_m, n_infections, IFR, detection_rate, cfr)


#By Eje
stat_by_eje = lapply(c("m","lo","hi"), function(bound){case_df_by_eje %>% group_by(Eje) %>% summarise(i_cases=sum(i_cases),
                                                                                                      d_cases = sum(d_cases),
                                                                                                      projection= sum(projection)) %>%
    
    #left_join(sero_df%>%select(name_0, n_pos, n_tot), by = join_by(Eje==name_0)) %>% ungroup() %>% #Assume location specific seropevalence
    #mutate(n_pos=ifelse(is.na(n_pos),340,n_pos), n_tot=ifelse(is.na(n_tot),1001,n_tot)) %>% #If no samples in a category, assume overall seroprevalence
    mutate(n_pos = 340, n_tot=1001) %>%
    
    reframe(bound=bound,
            cases=i_cases,
            deaths=d_cases,
            n_pos = n_pos,
            n_tot=n_tot,
            Eje=Eje,
            projection=projection,
            n_pos = n_pos, n_tot=n_tot,
            Estimate_m = 100*Hmisc::binconf(x = n_pos * 0.986 + (n_tot - n_pos) * (1- 0.98), n = n_tot)[,which(c("m","lo","hi")==bound)], #seroprevalence adjusted by kit sens/spec
            seroincidence = ifelse(Eje=="Metropolitano",Estimate_m-1.85,Estimate_m), #Correct only in eje metropolitano
            CI_fm = projection*Estimate_m/100, # N seropos post outbresk
            CI_0 = ifelse(Eje=="Metropolitano",projection*0.0185,0), #0.001, # N seropos pre outbreak
            n_infections = CI_fm-CI_0, # Seroincidence, N infected during outbreak
            IFR = 100*d_cases/n_infections,
            detection_rate = i_cases/n_infections,
            cfr = d_cases/i_cases*100)}) %>% 
  do.call(what=rbind) %>%
  arrange(Eje)

stat_by_eje_formatted = stat_by_eje %>% tidyr::pivot_longer(cols = -c(Eje,bound)) %>%
  tidyr::pivot_wider(id_cols = c(Eje, name), names_from = bound, values_from = value) %>%
  tidyr::pivot_wider(id_cols = c(Eje), names_from = name, values_from = c(m, lo, hi), names_sep = "~") %>%
  
  mutate(across(contains("Estimate")|contains("infections")|contains("IFR")|contains("detection")|contains("cfr")|contains("seroincidence"), ~format((signif(.x,3)), big.mark=",", scientific=F))) %>%
  mutate(across(contains("cases")|contains("deaths")|contains("n_tot"), ~format(.x, big.mark=",", scientific=F))) %>%
  mutate(across(everything(), ~as.character(.x))) %>%
  
  tidyr::pivot_longer(cols = -c(Eje), names_to = "name", values_to = "values") %>%
  mutate(bound = as.factor(stringr::str_split_i(name,"~",1)),
         name = as.factor(paste0(stringr::str_split_i(name,"~",-1)))) %>%
  tidyr::pivot_wider(id_cols = c(Eje, name), names_from = bound, values_from = values) %>%
  mutate(print_out = ifelse(m==lo,m,ifelse(lo<hi,paste0(m," [",lo,"-",hi,"]"),paste0(m," [",hi,"-",lo,"]")))) %>%
  select(-c(m,lo,hi)) %>%
  tidyr::pivot_wider(id_cols = c(Eje), names_from = "name", values_from = "print_out") %>%
  select(Eje,  cases, deaths, n_pos, n_tot, seroincidence, Estimate_m, n_infections, IFR, detection_rate, cfr)


stat_all = rbind(stat_by_sex_formatted%>%rename(Group=Sex), stat_by_eje_formatted%>%rename(Group=Eje),
                 stat_by_age_formatted%>%rename(Group=Age_group),stat_overall_formatted)
#stat_all = stat_all[c(1:7,15,8:14,16,17),]
stat_all = stat_all %>% relocate(Group, cases, deaths)
stat_all

#write.csv(stat_all, "output/summary_table_paper_assuming_equal_seroprevalence.csv", row.names=F)
#write.csv(stat_all, "output/summary_table_paper_assuming_specific_seroprevalence.csv", row.names=F)

#Stat higher baseline seopositivity
stat_higher_seropos = lapply(c("m","lo","hi"), function(bound){case_df_by_age_and_sex %>% ungroup() %>% summarise(i_cases=sum(i_cases),
                                                                                                           d_cases = sum(d_cases),
                                                                                                           projection= sum(projection)) %>%
    mutate(n_pos = sum(sero_df_gab$n_pos), n_tot = sum(sero_df_gab$n_tot)) %>%
    reframe(bound=bound,
            cases=i_cases,
            deaths=d_cases,
            n_pos = n_pos,
            n_tot=n_tot,
            Estimate_m = 100*Hmisc::binconf(x = n_pos * 0.986 + (n_tot - n_pos) * (1- 0.98), n = n_tot)[,which(c("m","lo","hi")==bound)], #seroprevalence adjusted by kit sens/spec
            seroincidence = Estimate_m-6.87, #Higher baseline immunity
            CI_fm = projection*Estimate_m/100, # N seropos post outbresk
            CI_0 = projection*0.0687, #0.001, # N seropos pre outbreak
            n_infections = CI_fm-CI_0, # Seroincidence, N infected during outbreak
            IFR = 100*d_cases/n_infections,
            detection_rate = i_cases/n_infections,
            cfr = d_cases/i_cases*100)}) %>% 
  do.call(what=rbind) %>% mutate(Group="Total") %>% relocate(Group)

stat_higher_seropos_formatted = stat_higher_seropos %>%   tidyr::pivot_longer(cols = -c(Group,bound)) %>%
  tidyr::pivot_wider(id_cols = c(Group, name), names_from = bound, values_from = value) %>%
  tidyr::pivot_wider(id_cols = c(Group), names_from = name, values_from = c(m, lo, hi), names_sep = "~") %>%
  
  mutate(across(contains("Estimate")|contains("infections")|contains("IFR")|contains("detection")|contains("cfr")|contains("seroincidence"), ~format((signif(.x,3)), big.mark=",", scientific=F))) %>%
  mutate(across(contains("cases")|contains("deaths")|contains("n_tot"), ~format(.x, big.mark=",", scientific=F))) %>%
  mutate(across(everything(), ~as.character(.x))) %>%
  
  tidyr::pivot_longer(cols = -c(Group), names_to = "name", values_to = "values") %>%
  mutate(bound = as.factor(stringr::str_split_i(name,"~",1)),
         name = as.factor(paste0(stringr::str_split_i(name,"~",-1)))) %>%
  tidyr::pivot_wider(id_cols = c(Group, name), names_from = bound, values_from = values) %>%
  mutate(print_out = ifelse(m==lo,m,ifelse(lo<hi,paste0(m," [",lo,"-",hi,"]"),paste0(m," [",hi,"-",lo,"]")))) %>%
  select(-c(m,lo,hi)) %>%
  tidyr::pivot_wider(id_cols = c(Group), names_from = "name", values_from = "print_out") %>%
  select(Group, cases, n_pos, n_tot, deaths, Estimate_m, seroincidence, n_infections, IFR, detection_rate, cfr)

summary_higherprev =rbind(stat_overall_formatted,stat_higher_seropos_formatted %>% mutate(Group = "Higher seroprevalence"))
write.csv(summary_higherprev, "output/table_higher_seroprevalence.csv")

#Plots-------------------------------------------------
require(ggplot2)
require(patchwork)
library(latex2exp)

case_df_by_age_and_sex = read.csv("data/processed_data/case_df_by_age_and_sex.csv") %>%
  mutate(Age_group = factor(Age_group, levels= c("<1","01-09",paste0(seq(10,60,by=10),"-",seq(19,69,by=10)),">70")))

theme_set(theme_minimal()+
            theme(axis.title = element_text(size=20),
                  legend.title = element_text(size=15),
                  legend.text = element_text(size=15),
                  strip.text = element_text(size=15),
                  axis.text = element_text(size=15),
                  plot.title = element_text(size=20)))

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
paraguay = sf::st_as_sf(geodata::gadm(country="PRY", level = 1, path="data/metadata/")) %>%
  mutate(NAME_1 = gsub("Presidente","Pte", NAME_1)) %>% 
  mutate(NAME_1 = toupper(iconv(NAME_1,to="ASCII//TRANSLIT"))) %>% 
  mutate(Eje = dpt_by_eje$Eje[sapply(NAME_1, function(x) which(dpt_by_eje$Department==x))]) %>% 
  left_join(., case_df_by_subregion, by = "Eje")

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
  #geom_sf_text(aes(label = round(incidence,1)), size = 5) +
  scale_fill_gradient2(high = "#cf3a36",
                       mid = "#e78429",
                       midpoint= log(160),
                       low ="white",
                       #limits=c(1,350),
                       breaks = c(0,10,100,200,300,400,500),
                       trans  = "log",
                       name="Incidence\n(per 10,000)") +
  ggspatial::annotation_scale(bar_cols=c("gray50","gray80"), line_width=0.1,text_cex=1,pad_x = unit(0.1,"npc"), pad_y = unit(0.2,"npc"), location="tr")+
  ggthemes::theme_map()+
  geom_sf_text(aes(label = Eje), size = 5) +
  theme(legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.position.inside = c(0.15,0.15))  

p_inc_map

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

#PLOTS HARMONIZED---
theme_set(theme_minimal()+
            theme(axis.title = element_text(size=20),
                  legend.title = element_text(size=20),
                  legend.text = element_text(size=15),
                  strip.text = element_text(size=15),
                  axis.text = element_text(size=15),
                  plot.title = element_text(size=20))+
            theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)))

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
  xlab("Age group") + ylab("Incidence per 10,000")

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

#Inset
#fig_1 = cowplot::plot_grid(incidence_map,p_deaths,nrow=1,labels = "AUTO", label_size = 30)
#fig_1

#No inset
fig_1 = gridExtra::grid.arrange(grobs=list(p_inc_map,p_inc,p_deaths),
                                layout_matrix=rbind(c(1,2),
                                                    c(1,3)),
                                
                                widths=c(1.5,1),
                                heights=c(1,1))
# 
# ggsave(fig_1, filename="output/fig/figure_1_descriptive.png", width=15, height=7.5)  
# ggsave(fig_1, filename="output/fig/figure_1_descriptive.pdf", width=15, height=7.5)  


gg0 = stat_by_age_and_sex %>%
  tidyr::pivot_wider(id_cols = c(Age_group, Sex), names_from = bound, values_from = cfr) %>%
  subset(Age_group != "100+") %>% 
  ggplot(aes(x=Age_group)) + 
  scale_fill_manual(values = c("#94b594","#376795"))+
  geom_bar(aes(y=m,fill = Sex),
           stat = "identity",
           position = "dodge",
           alpha = 1)  +
  #geom_errorbar(aes(ymin=lo, ymax=hi))+
  guides(fill=guide_legend(title=" ")) +
  ylab(expression(paste("CFR"," %"))) +
  xlab("Age group")  

gg0

gg1 = stat_by_age_and_sex %>%
  tidyr::pivot_wider(id_cols = c(Age_group, Sex), names_from = bound, values_from = IFR) %>%
  subset(Age_group != "100+") %>% 
  ggplot(aes(x=Age_group, fill = Sex)) + 
  scale_fill_manual(values = c("#94b594","#376795"))+
  geom_bar(aes(y=m),
           stat = "identity",
           position = position_dodge(),
           alpha = 1)  +
  geom_errorbar(aes(ymin=lo, ymax=hi), position=position_dodge(width = 0.9), col="gray30", linewidth=1, width=0.6)+
  guides(fill=guide_legend(title=" ")) +
  ylab("IFR (%)") +
  xlab("Age group")  

gg1

gg4 = stat_by_age_and_sex %>%
  tidyr::pivot_wider(id_cols = c(Age_group, Sex), names_from = bound, values_from = detection_rate) %>%
  subset(Age_group != "100+") %>% 
  ggplot(aes(x=Age_group, fill=Sex)) + 
  scale_fill_manual(values = c("#94b594","#376795"))+
  geom_bar(aes(y=m*100,fill = Sex),
           stat = "identity",
           position = "dodge",
           alpha = 1)  +
  geom_errorbar(aes(ymin=lo*100, ymax=hi*100), position=position_dodge(width = 0.9), col="gray30", linewidth=1, width=0.6)+
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

#Option1
# fig_2 = gridExtra::grid.arrange(grobs=list(p_sero_map,p_sero,p_props),
#                                 layout_matrix=rbind(c(1,3),
#                                                     c(1,3),
#                                                     c(2,3)),
#                                 
#                                 widths=c(2,1),
#                                 
#                                 heights=c(1,1,1))

#Option2
fig_2 = gridExtra::grid.arrange(grobs=list(p_sero_map,p_sero,gg0,gg1,gg4),
                                layout_matrix=rbind(c(1,3),
                                                    c(1,4),
                                                    c(2,5)),
                                
                                widths=c(2,1),
                                
                                heights=c(1,1,1))


fig_2

#ggsave(fig_2, filename="output/fig/figure_2_sero.png", width=15, height=15)  
#ggsave(fig_2, filename="output/fig/figure_2_sero.pdf", width=15, height=15)  

