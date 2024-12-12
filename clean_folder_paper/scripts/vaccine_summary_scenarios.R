library(dplyr)
library(ggplot2)

rm(list=ls())

load("output/vaccine_estimates_higher_seropevalence.Rdata")
df_1 = df_plot %>% mutate(scenario="high_serop")
load("output/vaccine_estimates_basecase.Rdata")
df_2 = df_plot %>% mutate(scenario="base_case")
load("output/vaccine_estimates_ve98.Rdata")
df_3 = df_plot %>% mutate(scenario="ve_98")
df_vaccine = rbind(df_1, df_2, df_3)
rm(df_plot, df_1, df_2, df_3)

#Add infection blocking scenario----------------
load("data/processed_data/vaccine_estimates_infection_blocking.Rdata")
df_4 = df_plot %>% mutate(scenario="infection_blocking")
rm(df_plot)

df_vaccine = df_vaccine %>% 
  mutate(infections_averted = infections_control - infections,
         cases_averted = cases_control - cases,
         deaths_averted = deaths_control - deaths,
         
         infections_averted_lo = infections_control_lo - infections_lo,
         cases_averted_lo = cases_control_lo - cases_lo,
         deaths_averted_lo = deaths_control_lo - deaths_lo,
         
         infections_averted_hi = infections_control_hi - infections_hi,
         cases_averted_hi = cases_control_hi - cases_hi,
         deaths_averted_hi = deaths_control_hi - deaths_hi)

df_vaccine = rbind(df_vaccine[,intersect(colnames(df_4), colnames(df_vaccine))],
                   df_4[,intersect(colnames(df_4), colnames(df_vaccine))]) %>%
  filter(trigger==0) %>%
  filter((coverage==40) | (scenario=="base_case" & coverage==20)) %>%
  relocate(scenario)



tab_vaccine = df_vaccine %>% 
  mutate(chronic_averted=0.5*cases_averted, chronic_averted_lo=0.5*cases_averted_lo, chronic_averted_hi=0.5*cases_averted_hi) %>%
  mutate(across(where(is.numeric), ~format(signif(round(.x),3), big.mark=",", scientific=F))) %>%
  reframe(scenario=paste0(scenario,"_",coverage),
          # coverage = coverage,
          # eff_disease = c(0.75,0.75,0.75,0.75,0.98),
          # eff_infection = c(0,0,0,0.75,0),
          doses_used = doses,
          infections_averted = paste0(infections_averted," (95%CI: ",infections_averted_lo,"-",infections_averted_hi,")"),
          acute_cases_averted = paste0(cases_averted," (95%CI: ",cases_averted_lo,"-",cases_averted_hi,")"),
          chronic_cases_averted = paste0(chronic_averted," (95%CI: ",chronic_averted_lo,"-",chronic_averted_hi,")"),
          deaths_averted = paste0(deaths_averted," (95%CI: ",deaths_averted_lo,"-",deaths_averted_hi,")")) %>%
  arrange(scenario)

tab_vaccine = tab_vaccine[c(2,1,3,5,4),]
tab_vaccine
write.csv(tab_vaccine, file = "output/summary_vccine_impact.csv")
















