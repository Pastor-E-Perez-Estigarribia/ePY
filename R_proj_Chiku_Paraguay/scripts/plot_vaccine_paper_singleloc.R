library(dplyr)
library(ggplot2)

rm(list=ls())

#Load data-----------------------
load("output/list_models_paper_singleloc_smoothed.Rdata")
case_df = read.csv("data/processed_data/case_data_upto16sep_23_weekly.csv") %>% filter(date>"2022-09-01")
pop_paraguay = read.csv("data/processed_data/pop_data.csv", row.names=1) %>%filter(level=="National") %>% select(Population) %>% as.numeric()
input_case = case_df %>% 
  filter(classification=="TOTAL" & name=="Paraguay" & disease == "CHIKUNGUNYA") %>% 
  mutate(date=as.Date(date)) %>% 
  arrange(date) %>%
  mutate(n_pos = NA, n_tot=NA, prop_pos=NA, prop_lo=NA, prop_hi=NA)
sero_df = data.frame(date="2023-08-12",
                     week_n = 428,
                     level = "National",
                     name= "Paraguay",
                     n_pos = 340,
                     n_tot = 1001)
idx_date_sero = sapply(sero_df$date, function(date) which(input_case$date==date))
input_case[idx_date_sero,c("n_pos","n_tot","prop_pos","prop_lo","prop_hi")] = cbind(sero_df[,c("n_pos","n_tot")],Hmisc::binconf(x = sero_df$n_pos, n=sero_df$n_tot))

pop_by_age = readxl::read_xlsx("data/raw_data/PROYECCION_POBLACIÓN_ AL 2025/POB_EDAD_SEXO_PAIS.xlsx") %>%
  filter(AÑO == 2020 & SEXO=="Masculino") %>% select(TOTAL, GRUPO_DE_EDAD)
pop_by_age_10 = c(sapply(1:9, function(i) sum(pop_by_age$TOTAL[(2*i-1):(2*i)])),sum(pop_by_age$TOTAL[19:21]))

estimated_pars = read.csv("data/processed_data/estimated_parameters.csv")
case_by_age_and_sex = read.csv("data/processed_data/case_df_by_age_and_sex.csv")
case_by_age_and_sex = case_by_age_and_sex %>% mutate(Age_group=ifelse(Age_group=="100+","90-99",Age_group)) %>% #Put 100+ in the 90-99
  group_by(Age_group) %>% 
  summarise(cases = sum(i_cases), deaths=sum(d_cases)) %>%
  ungroup() %>%
  mutate(pop = pop_by_age_10,
         infections = sum(df_fits$mean_i_infections)*pop/sum(pop)) %>%
  mutate(cfr = deaths/cases,
         icr = cases/infections,
         ifr = deaths/infections)

#Outbreak characteristics--------------
#First infection and Rt
df_fits %>% select(date, mean_rt)
df_fits %>% filter(date>"2022-10-01" & date<"2022-12-31") %>% summarise(mean=mean(mean_rt), lo=mean(lower_rt), hi=mean(upper_rt))

#Seroprev levels
df_fits %>% 
  filter(date %in% c(as.Date("2022-11-12")+lubridate::weeks(5*4),"2023-08-12")) %>%
  select(lower_i_susceptible, upper_i_susceptible, mean_i_susceptible) %>%
  mutate(across(everything(), ~100-(.x/pop_paraguay*100)))

#Get vaccination campaign--------------------------
#Get trigger date
#Parameters
get_vaccine_impact = function(coverage_lvl, case_trigger){
  # coverage_lvl = 40
  # case_trigger = 0
  
  df_fits = df_fits %>% arrange(date) %>%
    mutate(roll_sum = zoo::rollsum(mean_i_cases, k=4, fill=0, align = "right")) %>%
    mutate(trigger_reactive = cumsum(date>="2022-10-01")>=1, #Base case is start 1st october
           trigger_proactive= T)
  #Add delay
  if(case_trigger>=0) {df_fits = df_fits %>% mutate(trigger_reactive = c(rep(F,case_trigger),trigger_reactive)[1:n()])}
  if(case_trigger<0) {df_fits = df_fits %>% mutate(trigger_reactive = c(trigger_reactive[(-1):case_trigger],rep(T,-case_trigger))[1:n()])}
  
  #Add vaccination ramp up
  get_prop_vac = function(trigger_vect, target_cov, date_vector, time_to_coverage = 12){
    if(sum(trigger_vect)==0){
      return(rep(0, length(date_vector)))
    }else{
      start_date = which(trigger_vect)[1]
      Vacc_vect = rep(0, length(date_vector))
      daily_rate = target_cov/time_to_coverage
      Vacc_vect[start_date:(start_date+ceiling(time_to_coverage)-1)] = cumsum(rep(daily_rate, ceiling(time_to_coverage)))
      Vacc_vect[(start_date+ceiling(time_to_coverage)-1):length(Vacc_vect)] = target_cov
      return(Vacc_vect[1:length(date_vector)])
    } #No outbreak detected
  }
  
  #Prop vaccinated
  df_fits = df_fits %>% 
    mutate(prop_vacc = sapply(coverage_lvl, function(cov){get_prop_vac(trigger_vect = trigger_reactive, target_cov = cov, date_vector = date)}) %>% 
             as.data.frame() %>% rename_all(~paste0("reac_cov"))) %>%
    bind_cols(.$prop_vacc) %>%
    select(-prop_vacc)
  df_fits[,paste0("proac_cov")] = coverage_lvl
  
  #Prop protected (immunogenicity delay)
  df_fits = df_fits %>% mutate(across(.cols = contains("reac_cov"),
                                      .fns = ~ c(rep(0,2),.x)[1:length(.x)],
                                      .names = "protected_{.col}"))
  df_fits = df_fits %>% mutate(across(.cols = contains("proac_cov"),
                                      .fns = ~ .x,
                                      .names = "protected_{.col}"))
  
  #Deterministic
  # prop_over12 = (sum(pop_by_age$TOTAL) - sum(pop_by_age$TOTAL[1:2]) -0.5*pop_by_age$TOTAL[3])/sum(pop_by_age$TOTAL)
  # 
  # ve = 0.75
  # df_fits = df_fits %>% mutate(across(.cols=contains("protected"),
  #                                     .fns=list(count_unvac = ~ mean_i_infections * (1-.x/100) * prop_over12 + mean_i_infections *  (1-prop_over12),
  #                                               count_vac = ~ (mean_i_infections * .x/100) * (1-ve) * prop_over12,
  #                                               count_tot = ~ (mean_i_infections * (1-.x/100) * prop_over12 + mean_i_infections *  (1-prop_over12)) +
  #                                                 ((mean_i_infections * .x/100) * (1-ve) * prop_over12)),
  #                                     .names = "{.fn}_{.col}"))
  # names(df_fits) = gsub(pattern = "_protected", replacement = "", x = names(df_fits))
  
  #Infections by age
  inf_by_age = as.data.frame(sapply(case_by_age_and_sex$pop, function(pop_a) df_fits$mean_i_infections*pop_a/sum(case_by_age_and_sex$pop))) %>%
    `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
    mutate(date=df_fits$date) %>%
    relocate(date)
  
  inf_by_age_lo = as.data.frame(sapply(case_by_age_and_sex$pop, function(pop_a) df_fits$lower_i_infections*pop_a/sum(case_by_age_and_sex$pop))) %>%
    `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
    mutate(date=df_fits$date) %>%
    relocate(date)
  
  inf_by_age_hi = as.data.frame(sapply(case_by_age_and_sex$pop, function(pop_a) df_fits$upper_i_infections*pop_a/sum(case_by_age_and_sex$pop))) %>%
    `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
    mutate(date=df_fits$date) %>%
    relocate(date)
  
  ve = 0.75
  prop_below_12_within10to19 = (2/5*pop_by_age$TOTAL[3])/sum(pop_by_age$TOTAL[3:4])
  
  inf_by_age_vac = inf_by_age
  inf_by_age_vac[,3] = prop_below_12_within10to19*inf_by_age_vac[,3]+ #Below 12yo are unvaccinated
    (1-prop_below_12_within10to19) * inf_by_age_vac[,3] * (df_fits$protected_reac_cov/100*(1-ve) + (1-df_fits$protected_reac_cov/100)) #Above 12y are vaccinated
  inf_by_age_vac[,4:11] = sapply(4:11, function(col) (inf_by_age_vac[,col]) * (df_fits$protected_reac_cov/100*(1-ve) + (1-df_fits$protected_reac_cov/100)))
  
  inf_by_age_vac_lo = inf_by_age_lo
  inf_by_age_vac_lo[,3] = prop_below_12_within10to19*inf_by_age_vac_lo[,3]+ #Below 12yo are unvaccinated
    (1-prop_below_12_within10to19) * inf_by_age_vac_lo[,3] * (df_fits$protected_reac_cov/100*(1-ve) + (1-df_fits$protected_reac_cov/100)) #Above 12y are vaccinated
  inf_by_age_vac_lo[,4:11] = sapply(4:11, function(col) (inf_by_age_vac_lo[,col]) * (df_fits$protected_reac_cov/100*(1-ve) + (1-df_fits$protected_reac_cov/100)))
  
  inf_by_age_vac_hi = inf_by_age_hi
  inf_by_age_vac_hi[,3] = prop_below_12_within10to19*inf_by_age_vac_hi[,3]+ #Below 12yo are unvaccinated
    (1-prop_below_12_within10to19) * inf_by_age_vac_hi[,3] * (df_fits$protected_reac_cov/100*(1-ve) + (1-df_fits$protected_reac_cov/100)) #Above 12y are vaccinated
  inf_by_age_vac_hi[,4:11] = sapply(4:11, function(col) (inf_by_age_vac_hi[,col]) * (df_fits$protected_reac_cov/100*(1-ve) + (1-df_fits$protected_reac_cov/100)))
  
  #Cases by age
  case_by_age = sapply(1:nrow(case_by_age_and_sex), function(age) inf_by_age[,1+age]*case_by_age_and_sex$icr[age]) %>%
    as.data.frame() %>%
    `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
    mutate(date=df_fits$date) %>%
    relocate(date)
  case_by_age_lo = sapply(1:nrow(case_by_age_and_sex), function(age) inf_by_age_lo[,1+age]*case_by_age_and_sex$icr[age]) %>%
    as.data.frame() %>%
    `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
    mutate(date=df_fits$date) %>%
    relocate(date)
  case_by_age_hi = sapply(1:nrow(case_by_age_and_sex), function(age) inf_by_age_hi[,1+age]*case_by_age_and_sex$icr[age]) %>%
    as.data.frame() %>%
    `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
    mutate(date=df_fits$date) %>%
    relocate(date)
  
  case_by_age_vac = sapply(1:nrow(case_by_age_and_sex), function(age) inf_by_age_vac[,1+age]*case_by_age_and_sex$icr[age]) %>%
    as.data.frame() %>%
    `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
    mutate(date=df_fits$date) %>%
    relocate(date)
  case_by_age_vac_lo = sapply(1:nrow(case_by_age_and_sex), function(age) inf_by_age_vac_lo[,1+age]*case_by_age_and_sex$icr[age]) %>%
    as.data.frame() %>%
    `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
    mutate(date=df_fits$date) %>%
    relocate(date)
  case_by_age_vac_hi = sapply(1:nrow(case_by_age_and_sex), function(age) inf_by_age_vac_hi[,1+age]*case_by_age_and_sex$icr[age]) %>%
    as.data.frame() %>%
    `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
    mutate(date=df_fits$date) %>%
    relocate(date)
  
  #Deaths by age
  deaths_by_age = sapply(1:nrow(case_by_age_and_sex), function(age) inf_by_age[,1+age]*case_by_age_and_sex$ifr[age]) %>%
    as.data.frame() %>%
    `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
    mutate(date=df_fits$date) %>%
    relocate(date)
  deaths_by_age_lo = sapply(1:nrow(case_by_age_and_sex), function(age) inf_by_age_lo[,1+age]*case_by_age_and_sex$ifr[age]) %>%
    as.data.frame() %>%
    `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
    mutate(date=df_fits$date) %>%
    relocate(date)
  deaths_by_age_hi = sapply(1:nrow(case_by_age_and_sex), function(age) inf_by_age_hi[,1+age]*case_by_age_and_sex$ifr[age]) %>%
    as.data.frame() %>%
    `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
    mutate(date=df_fits$date) %>%
    relocate(date)
  
  deaths_by_age_vac = sapply(1:nrow(case_by_age_and_sex), function(age) inf_by_age_vac[,1+age]*case_by_age_and_sex$ifr[age]) %>%
    as.data.frame() %>%
    `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
    mutate(date=df_fits$date) %>%
    relocate(date)
  deaths_by_age_vac_lo = sapply(1:nrow(case_by_age_and_sex), function(age) inf_by_age_vac_lo[,1+age]*case_by_age_and_sex$ifr[age]) %>%
    as.data.frame() %>%
    `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
    mutate(date=df_fits$date) %>%
    relocate(date)
  deaths_by_age_vac_hi = sapply(1:nrow(case_by_age_and_sex), function(age) inf_by_age_vac_hi[,1+age]*case_by_age_and_sex$ifr[age]) %>%
    as.data.frame() %>%
    `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
    mutate(date=df_fits$date) %>%
    relocate(date)
  
  
  #Nb averted
  ndoses = coverage_lvl/100*(sum(pop_by_age$TOTAL[4:21])+3/5*pop_by_age$TOTAL[3])
  
  tot_i = sum(rowSums(inf_by_age[,-1]))
  tot_i_v =  sum(rowSums(inf_by_age_vac[,-1]))
  i_averted = tot_i-tot_i_v
  
  tot_c = sum(rowSums(case_by_age[,-1]))
  tot_c_v =  sum(rowSums(case_by_age_vac[,-1]))
  c_averted = tot_c-tot_c_v
  
  tot_d = sum(rowSums(deaths_by_age[,-1]))
  tot_d_v =  sum(rowSums(deaths_by_age_vac[,-1]))
  d_averted = tot_d-tot_d_v
  
  tot_i_lo = sum(rowSums(inf_by_age_lo[,-1]))
  tot_i_v_lo =  sum(rowSums(inf_by_age_vac_lo[,-1]))
  tot_c_lo = sum(rowSums(case_by_age_lo[,-1]))
  tot_c_v_lo =  sum(rowSums(case_by_age_vac_lo[,-1]))
  tot_d_lo = sum(rowSums(deaths_by_age_lo[,-1]))
  tot_d_v_lo =  sum(rowSums(deaths_by_age_vac_lo[,-1]))
  
  tot_i_hi = sum(rowSums(inf_by_age_hi[,-1]))
  tot_i_v_hi =  sum(rowSums(inf_by_age_vac_hi[,-1]))
  tot_c_hi = sum(rowSums(case_by_age_hi[,-1]))
  tot_c_v_hi =  sum(rowSums(case_by_age_vac_hi[,-1]))
  tot_d_hi = sum(rowSums(deaths_by_age_hi[,-1]))
  tot_d_v_hi =  sum(rowSums(deaths_by_age_vac_hi[,-1]))
  
  out = data.frame(coverage = coverage_lvl,
                   trigger = case_trigger,
                   doses = ndoses,
                   
                   infections_control = tot_i,
                   cases_control = tot_c,
                   deaths_control = tot_d,
                   
                   infections = tot_i_v,
                   cases = tot_c_v,
                   deaths = tot_d_v,
                   
                   infections_control_lo = tot_i_lo,
                   cases_control_lo = tot_c_lo,
                   deaths_control_lo = tot_d_lo,
                   
                   infections_lo = tot_i_v_lo,
                   cases_lo = tot_c_v_lo,
                   deaths_lo = tot_d_v_lo,
                   
                   infections_control_hi = tot_i_hi,
                   cases_control_hi = tot_c_hi,
                   deaths_control_hi = tot_d_hi,
                   
                   infections_hi = tot_i_v_hi,
                   cases_hi = tot_c_v_hi,
                   deaths_hi = tot_d_v_hi
                   
  )
  
  return(out)
}


#Plot figure 4
df_plot = lapply(seq(-1,14,by=1), function(trigger){
  lapply(seq(0,100,by=5), function(cov) 
    get_vaccine_impact(coverage_lvl = cov, case_trigger = trigger))%>%do.call(what=rbind)
}) %>% 
  do.call(what=rbind)

base_cov = 40
base_delay = 0

i_av = df_plot %>% filter(trigger==base_delay, coverage==base_cov) %>% reframe(infections_averted = infections_control-infections) %>% as.numeric()
c_av = df_plot %>% filter(trigger==base_delay, coverage==base_cov) %>% reframe(cases_averted = cases_control-cases) %>% as.numeric()
d_av = df_plot %>% filter(trigger==base_delay, coverage==base_cov) %>% reframe(deaths_averted = deaths_control-deaths) %>% as.numeric()

#Get cumulative numbers since outbreak start
df_oct = df_fits %>% filter(date>="2022-10-01")
inf_by_age = as.data.frame(sapply(case_by_age_and_sex$pop, function(pop_a) df_oct$mean_i_infections*pop_a/sum(case_by_age_and_sex$pop))) %>%
  `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
  mutate(date=df_oct$date) %>%
  relocate(date)
inf_by_age_lo = as.data.frame(sapply(case_by_age_and_sex$pop, function(pop_a) df_oct$lower_i_infections*pop_a/sum(case_by_age_and_sex$pop))) %>%
  `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
  mutate(date=df_oct$date) %>%
  relocate(date)
inf_by_age_hi = as.data.frame(sapply(case_by_age_and_sex$pop, function(pop_a) df_oct$upper_i_infections*pop_a/sum(case_by_age_and_sex$pop))) %>%
  `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
  mutate(date=df_oct$date) %>%
  relocate(date)
case_by_age = sapply(1:nrow(case_by_age_and_sex), function(age) inf_by_age[,1+age]*case_by_age_and_sex$icr[age]) %>%
  as.data.frame() %>%
  `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
  mutate(date=df_oct$date) %>%
  relocate(date)
case_by_age_lo = sapply(1:nrow(case_by_age_and_sex), function(age) inf_by_age_lo[,1+age]*case_by_age_and_sex$icr[age]) %>%
  as.data.frame() %>%
  `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
  mutate(date=df_oct$date) %>%
  relocate(date)
case_by_age_hi = sapply(1:nrow(case_by_age_and_sex), function(age) inf_by_age_hi[,1+age]*case_by_age_and_sex$icr[age]) %>%
  as.data.frame() %>%
  `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
  mutate(date=df_oct$date) %>%
  relocate(date)
deaths_by_age = sapply(1:nrow(case_by_age_and_sex), function(age) inf_by_age[,1+age]*case_by_age_and_sex$ifr[age]) %>%
  as.data.frame() %>%
  `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
  mutate(date=df_oct$date) %>%
  relocate(date)
deaths_by_age_lo = sapply(1:nrow(case_by_age_and_sex), function(age) inf_by_age_lo[,1+age]*case_by_age_and_sex$ifr[age]) %>%
  as.data.frame() %>%
  `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
  mutate(date=df_oct$date) %>%
  relocate(date)
deaths_by_age_hi = sapply(1:nrow(case_by_age_and_sex), function(age) inf_by_age_hi[,1+age]*case_by_age_and_sex$ifr[age]) %>%
  as.data.frame() %>%
  `colnames<-`(paste0(case_by_age_and_sex$Age_group)) %>%
  mutate(date=df_oct$date) %>%
  relocate(date)

df_cum = data.frame(date = df_oct$date[c(1,1:15)],
                    delay=(-1):14,
                    cum_inf = cumsum(rowSums(inf_by_age[,-1]))[c(1,1:15)],
                    cum_case = cumsum(rowSums(case_by_age[,-1]))[c(1,1:15)],
                    cum_death = cumsum(rowSums(deaths_by_age[,-1]))[c(1,1:15)],
                    
                    cum_inf_lo = cumsum(rowSums(inf_by_age_lo[,-1]))[c(1,1:15)],
                    cum_case_lo = cumsum(rowSums(case_by_age_lo[,-1]))[c(1,1:15)],
                    cum_death_lo = cumsum(rowSums(deaths_by_age_lo[,-1]))[c(1,1:15)],
                    
                    cum_inf_hi = cumsum(rowSums(inf_by_age_hi[,-1]))[c(1,1:15)],
                    cum_case_hi = cumsum(rowSums(case_by_age_hi[,-1]))[c(1,1:15)],
                    cum_death_hi = cumsum(rowSums(deaths_by_age_hi[,-1]))[c(1,1:15)])


p11 = df_plot %>% 
  reframe(coverage = coverage,
          trigger = trigger,
          infections_averted = (infections_control-infections)/infections_control,
          cases_averted = (cases_control-cases)/cases_control,
          deaths_averted = (deaths_control-deaths)/deaths_control) %>%
  ggplot(aes(x=coverage,y=trigger,fill=100*cases_averted))+
  geom_tile()+
  scale_fill_gradient2(name="% Cases\naverted",
                       low = "#D81B60",
                       mid = "#FFC107",
                       midpoint=30,
                       high="#1E88E5")+
  theme_minimal()+
  geom_hline(yintercept = base_delay, linetype="dashed")+
  geom_vline(xintercept = base_cov, linetype="dashed")+
  xlab("Coverage (%)\nDoses used (million)")+ylab("Campaign delay (weeks)")+
  scale_x_continuous(labels=~paste(.,
                                   signif(.*(df_plot$doses/df_plot$coverage)[2]/1e6,2),
                                   sep="\n"))
p11

#Plot alternative
p12 = df_plot %>% 
  filter(trigger==base_delay) %>%
  ggplot(aes(x=coverage, y=cases_control-cases,
             ymin=cases_control_lo-cases_lo,
             ymax=cases_control_hi-cases_hi)) +
  geom_line(col="#1E88E5")+geom_ribbon(fill="#1E88E5" ,alpha=0.3)+
  theme_minimal()+
  xlab(NULL)+ylab("Cases averted\n(% averted)")+
  expand_limits(y=c(0,85000))+
  geom_hline(yintercept = c_av, linetype="dashed")+
  geom_vline(xintercept = base_cov, linetype="dashed")+
  scale_y_continuous(labels=~paste(formatC(.,format="d",big.mark = ","),paste0("(",signif(./sum(rowSums(case_by_age[,-1]))*100,2),"%)"),sep="\n"))+
  scale_x_continuous(labels=NULL)
#scale_y_continuous(sec.axis = sec_axis(~./sum(rowSums(case_by_age[,-1]))*100, name="Proportion\naverted (%)"))
p12
p13 = df_plot %>% 
  filter(coverage==base_cov) %>%
  ggplot() +
  
  geom_hline(yintercept = c_av, linetype="dashed")+
  geom_vline(xintercept = base_delay, linetype="dashed") +
  
  #geom_line(data=df_cum, aes(x=delay, y=1e4*log(cum_case/cum_case_lo[1])), col="#D81B60", linetype="dashed")+
  #eom_ribbon(data=df_cum, aes(x=delay, ymin=1e4*log(cum_case_lo/cum_case_lo[1]), ymax=1e4*log(cum_case_hi/cum_case_lo[1])), fill="#D81B60", alpha=0.3)+
  
  geom_line(aes(x=trigger, y=cases_control-cases),
            col="#1E88E5")+
  geom_ribbon(aes(x=trigger, y=cases_control-cases,
                  ymin=cases_control_lo-cases_lo,
                  ymax=cases_control_hi-cases_hi),
              fill="#1E88E5" ,alpha=0.3)+
  theme_minimal()+
  xlab(NULL)+ylab(NULL)+
  expand_limits(y=c(0,85000))+
  scale_y_continuous(labels=NULL)+
  # scale_y_continuous(labels=NULL, sec.axis = sec_axis(~exp(./1e4)*df_cum$cum_case_lo[1], labels=scales::label_comma(), breaks=c(1,10,100,1000,10000),
  #                                                                      name="Cases reported before\ncampaign start"))+
  scale_x_continuous(labels=NULL)
p13

p21 = df_plot %>% 
  reframe(coverage = coverage,
          trigger = trigger,
          infections_averted = (infections_control-infections)/infections_control,
          cases_averted = (cases_control-cases)/cases_control,
          deaths_averted = (deaths_control-deaths)/deaths_control) %>%
  ggplot(aes(x=coverage,y=trigger,fill=100*infections_averted))+
  geom_tile()+
  scale_fill_gradient2(name="% Infections\naverted",
                       low = "#D81B60",
                       mid = "#FFC107",
                       midpoint=30,
                       high="#1E88E5")+
  theme_minimal()+
  geom_hline(yintercept = base_delay, linetype="dashed")+
  geom_vline(xintercept = base_cov, linetype="dashed")+
  xlab("Coverage (%)")+ylab("Campaign delay (weeks)")
p21

#Plot alternative
p22 = df_plot %>% 
  filter(trigger==base_delay) %>%
  ggplot(aes(x=coverage, y=infections_control-infections,
             ymin=infections_control_lo-infections_lo,
             ymax=infections_control_hi-infections_hi)) +
  geom_line(col="#1E88E5")+geom_ribbon(fill="#1E88E5" ,alpha=0.3)+
  theme_minimal()+
  xlab(NULL)+ylab("Infections averted\n(% averted)")+
  expand_limits(y=c(0,1510000))+
  geom_hline(yintercept = i_av, linetype="dashed")+
  geom_vline(xintercept = base_cov, linetype="dashed")+
  scale_y_continuous(labels=~paste(formatC(.,format="d",big.mark=","),paste0("(",signif(./sum(rowSums(inf_by_age[,-1]))*100,2),"%)"),sep="\n"))+
  scale_x_continuous(labels=NULL)
#scale_y_continuous(labels=scales::label_scientific(),sec.axis = sec_axis(~./sum(rowSums(inf_by_age[,-1]))*100, name="Proportion\naverted (%)"))
p22

p23 = df_plot %>% 
  filter(coverage==base_cov) %>%
  ggplot() +
  geom_hline(yintercept = i_av, linetype="dashed")+
  geom_vline(xintercept = base_delay, linetype="dashed")+
  
  #geom_line(data=df_cum, aes(x=delay, y=1.5e5*log(cum_inf/cum_inf_lo[1])), col="#D81B60", linetype="dashed")+
  #geom_ribbon(data=df_cum, aes(x=delay, ymin=1.5e5*log(cum_inf_lo/cum_inf_lo[1]), ymax=1.5e5*log(cum_inf_hi/cum_inf_lo[1])), fill="#D81B60", alpha=0.3)+
  
  geom_line(aes(x=trigger, y=infections_control-infections),
            col="#1E88E5")+
  geom_ribbon(aes(x=trigger, y=infections_control-infections,
                  ymin=infections_control_lo-infections_lo,
                  ymax=infections_control_hi-infections_hi),
              fill="#1E88E5" ,alpha=0.3)+
  theme_minimal()+
  xlab(NULL)+ylab(NULL)+
  expand_limits(y=c(0,1510000))+
  scale_y_continuous(labels=NULL)+
  # scale_y_continuous(labels=NULL, sec.axis = sec_axis(~exp(./1.5e5)*df_cum$cum_inf_lo[1], labels=scales::label_scientific(), breaks=c(10^(0:7)),
  #                                                                           name="Infections before\ncampaign start"))+
  scale_x_continuous(labels=NULL)
p23

p31 = df_plot %>% 
  reframe(coverage = coverage,
          trigger = trigger,
          infections_averted = (infections_control-infections)/infections_control,
          cases_averted = (cases_control-cases)/cases_control,
          deaths_averted = (deaths_control-deaths)/deaths_control) %>%
  ggplot(aes(x=coverage,y=trigger,fill=100*deaths_averted))+
  geom_tile()+
  scale_fill_gradient2(name="% Deaths\naverted",
                       low = "#D81B60",
                       mid = "#FFC107",
                       midpoint=30,
                       high="#1E88E5")+
  theme_minimal()+
  geom_hline(yintercept = base_delay, linetype="dashed")+
  geom_vline(xintercept = base_cov, linetype="dashed")+
  xlab("Coverage (%)")+ylab("Campaign delay (weeks)")
p31

#Plot alternative
p32 = df_plot %>% 
  filter(trigger==base_delay) %>%
  ggplot(aes(x=coverage, y=deaths_control-deaths,
             ymin=deaths_control_lo-deaths_lo,
             ymax=deaths_control_hi-deaths_hi)) +
  geom_line(col="#1E88E5")+geom_ribbon(fill="#1E88E5" ,alpha=0.3)+
  theme_minimal()+
  expand_limits(y=c(0,210))+
  xlab("Coverage (%)\nDoses used (million)")+ylab("Deaths averted\n(% averted)")+
  geom_hline(yintercept = d_av, linetype="dashed")+
  geom_vline(xintercept = base_cov, linetype="dashed")+
  scale_y_continuous(labels=~paste(.,paste0("(",signif(./sum(rowSums(deaths_by_age[,-1]))*100,2),"%)"),sep="\n"))+
  #scale_y_continuous(sec.axis = sec_axis(~./sum(rowSums(deaths_by_age[,-1]))*100, name="Proportion\naverted (%)"))+
  scale_x_continuous(labels=~paste(.,
                                   signif(.*(df_plot$doses/df_plot$coverage)[2]/1e6,2),
                                   sep="\n"))
p32

p33 = df_plot %>% 
  filter(coverage==base_cov) %>%
  ggplot() +
  geom_hline(yintercept = d_av, linetype="dashed")+
  geom_vline(xintercept = base_delay, linetype="dashed")+
  #geom_line(data=df_cum, aes(x=delay, y=8*round(cum_death)), col="#D81B60", linetype="dashed")+
  #geom_ribbon(data=df_cum, aes(x=delay, ymin=8*round(cum_death_lo), ymax=8*round(cum_death_hi)), alpha=0.3, fill="#D81B60")+
  geom_line(aes(x=trigger, y=deaths_control-deaths),
            col="#1E88E5")+
  geom_ribbon(aes(x=trigger, y=deaths_control-deaths,
                  ymin=deaths_control_lo-deaths_lo,
                  ymax=deaths_control_hi-deaths_hi),
              fill="#1E88E5" ,alpha=0.3)+
  theme_minimal()+
  xlab("Campaign delay (weeks)\n")+ylab(NULL)+
  expand_limits(y=c(0,210))+
  scale_x_continuous(labels=~paste(.,NULL,sep="\n"))+
  scale_y_continuous(labels=NULL)
#scale_y_continuous(labels=NULL, sec.axis = sec_axis(~./8, name="Deaths before\ncampaign start",breaks=c(0,10,20)))
p33

pvoid=ggplot()+theme_minimal()
pcust = gridExtra::grid.arrange(grobs=list(p11, p22,p23,p12,p13, p32,p33),
                                layout_matrix=rbind(c(1,2,3),
                                                    c(1,4,5),
                                                    c(1,6,7)),
                                
                                widths=c(2,1.2,1),
                                
                                heights=c(1,1,1.4))

ggsave(pcust,filename = "output/fig/figure4_vaccine_impact.pdf", width=11*0.9*1.3, height=5.05*1.3)
ggsave(pcust,filename = "output/fig/figure4_vaccine_impact.png", width=11*0.9*1.3, height=5.05*1.3)

#Get numbers
df_fits %>% filter(date<"2022-10-01") %>% select(-c(date,model)) %>% colSums()
df_out = df_plot %>% filter((coverage%in%c(20,40) & trigger==0)|(coverage==40 & trigger%in%c(13,-1)))

df_out

df_out %>% reframe(coverage = coverage,
                   trigger = trigger,
                   
                   infections_averted = infections_control-infections,
                   infections_averted_lo = infections_control_lo-infections_lo,
                   infections_averted_hi = infections_control_hi-infections_hi,
                   
                   cases_averted = cases_control-cases,
                   cases_averted_lo = cases_control_lo-cases_lo,
                   cases_averted_hi = cases_control_hi-cases_hi,
                   
                   deaths_averted = deaths_control-deaths,
                   deaths_averted_lo = deaths_control_lo-deaths_lo,
                   deaths_averted_hi = deaths_control_hi-deaths_hi)

df_out %>% reframe(coverage = coverage,
                   trigger = trigger,
                   infections_averted = (infections_control-infections)/doses*1e4,
                   cases_averted = (cases_control-cases)/doses*1e4,
                   deaths_averted = (deaths_control-deaths)/doses*1e4)

df_fits %>% filter(date<as.Date("2022-10-05")) %>% select(mean_i_infections) %>% sum()
input_case %>% filter(date<as.Date("2022-10-05")) %>% select(i_cases) %>% sum()


pop_by_sex = readxl::read_xlsx("data/raw_data/PROYECCION_POBLACIÓN_ AL 2025/POB_EDAD_SEXO_PAIS.xlsx") %>%
  group_by(SEXO) %>% summarise(pop = sum(POB_SEXO)) %>% mutate(SEXO=ifelse(SEXO=="Femenino","F","M"))
colnames(pop_by_sex) = c("Sex","pop")

case_by_age_and_sex = read.csv("data/processed_data/case_df_by_age_and_sex.csv")

case_by_sex = case_by_age_and_sex %>% 
  group_by(Sex) %>% 
  summarise(cases = sum(i_cases), deaths=sum(d_cases)) %>%
  ungroup() %>%
  inner_join(pop_by_sex, by="Sex") %>%
  mutate(infections = sum(df_fits$mean_i_infections)*pop/sum(pop)) %>%
  mutate(case_incidence = cases/pop,
         cfr = deaths/cases,
         icr = cases/infections,
         ifr = deaths/infections)

colSums(case_by_sex %>% select(cases))
case_by_sex$case_incidence[1]/case_by_sex$case_incidence[2]

case_df = read.csv("data/processed_data/case_data_upto16sep_23_weekly.csv") %>% 
  filter(date>"2022-09-01")

pop_by_dpt = read.csv("data/processed_data/pop_data.csv") %>% filter(level=="Department")

case_df %>%  filter(classification=="TOTAL" & level=="Department" & disease == "CHIKUNGUNYA") %>% 
  mutate(date=as.Date(date)) %>% ungroup() %>% group_by(name) %>%
  summarise(cases = sum(i_cases)) %>%
  inner_join(pop_by_dpt, by="name") %>%
  mutate(incidence = cases/Population*1e4) %>%
  arrange(incidence)

pop_by_eje =  read.csv("data/processed_data/pop_data.csv") %>% filter(level=="Eje")

case_by_eje = case_df %>%  filter(classification=="TOTAL" & level=="Eje" & disease == "CHIKUNGUNYA") %>% 
  mutate(date=as.Date(date)) %>% ungroup() %>% group_by(name) %>%
  summarise(cases = sum(i_cases)) %>%
  inner_join(pop_by_eje, by="name") %>%
  mutate(incidence = cases/Population*1e4) %>%
  arrange(incidence)

case_by_eje

sero_df = read.csv("data/processed_data/seroprevalence_by_eje.csv")
sero_by_eje = sero_df %>% mutate(seropositivity = n_pos/n_tot) %>% arrange(seropositivity)
sero_by_eje
sum(sero_df$n_pos)
sum(sero_df$n_tot)

case_by_eje = case_by_eje %>% 
  inner_join(sero_by_eje, by="name") %>%
  mutate(attack_rate = ifelse(name=="Metropolitano", seropositivity-0.05, seropositivity)) %>%
  mutate(infections=attack_rate*Population,
         reporting = cases/infections)

sum(case_by_eje$infections)/sum(case_by_eje$Population)*100
sum(case_by_eje$cases)/sum(case_by_eje$infections)*100

case_by_age2 = case_by_age_and_sex %>% 
  group_by(Age_group) %>% 
  summarise(cases = sum(i_cases), deaths=sum(d_cases)) %>%
  ungroup() %>%
  mutate(pop=pop_by_age_10,
         infections = sum(df_fits$mean_i_infections)*pop/sum(pop)) %>%
  mutate(case_incidence = cases/pop,
         cfr = deaths/cases,
         icr = cases/infections,
         ifr = deaths/infections)
case_by_age2
sum(case_by_age2$cases[7:10])/sum(case_by_age2$infections[7:10])


