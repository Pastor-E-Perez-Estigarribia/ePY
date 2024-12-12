library(dplyr)
library(ggplot2)

rm(list=ls())

load("output/list_models_paper_singleloc_smoothed.Rdata")
pop_paraguay = read.csv("data/processed_data/pop_data.csv", row.names=1) %>%filter(level=="National") %>% select(Population) %>% as.numeric()

#Get weather data
weather_data = read.csv("data/processed_data/weather_data_1961to2023.csv") %>%
  filter(level=="National") %>%
  filter(variables=="Temperature") %>%
  mutate(fecha=as.Date(fecha)) %>%
  arrange(fecha) %>%
  filter(fecha<"2023-10-01") %>%
  filter(fecha>="2021-10-01")

weather_data = weather_data %>%
  mutate(avg_temp = zoo::rollmean(values,k=30,fill=NA))%>%
  filter(fecha>="2022-10-01")

extreme_temp = read.csv("data/processed_data/extreme_temp_per_date.csv") %>%
  filter(fecha<"2023-10-01") %>%
  filter(fecha>="2021-10-01") %>%
  mutate(tmin = zoo::rollmean(tmin,k=30,fill=NA, na.rm=T),
         tmax = zoo::rollmean(tmax,k=30,fill=NA, na.rm=T))%>%
  filter(fecha>="2022-10-01")

#Curve of daily cases
p_case = df_fits%>%filter(date!="1900-01-01") %>%
  filter(date<"2023-10-01") %>% #Focus no first peak
  filter(date>="2022-10-01") %>%
  ggplot() +
  geom_ribbon(aes(x = date, ymin = lower_i_cases, ymax = upper_i_cases, fill=model),
              alpha = 0.3) +
  geom_line(aes(x = date, y = mean_i_cases, color=model)) +
  geom_point(data = input_case %>% filter(date>="2022-10-01"), aes(x = date, y = i_cases_raw)) +
  #geom_point(data = input_case, aes(x = date, y = i_cases), col=2) +
  #geom_line(data=scaling_vect, aes(x = time, y = scaling)) +
  xlab("") + ylab("Weekly N. of Cases")+
  #facet_grid(~prop_immune,labeller=label_both)+
  scale_x_date(date_breaks="3 months", date_labels="%b %y")+
  ggtitle("Cases")+
  scale_y_continuous(labels = scales::comma)+
  theme_minimal()+
  theme(legend.position = "none")

p_infections = df_fits%>%filter(date!="1900-01-01") %>% mutate(mean_i_infections = mean_i_infections,
                                                               lower_i_infections = lower_i_infections,
                                                               upper_i_infections = upper_i_infections) %>%
  filter(date<"2023-10-01") %>% #Focus no first peak
  filter(date>="2022-10-01") %>%
  ggplot() +
  geom_ribbon(aes(x = date, ymin = lower_i_infections, ymax = upper_i_infections, fill=model),
              alpha = 0.3) +
  geom_line(aes(x = date, y = mean_i_infections, color=model)) +
  xlab("") + ylab("Weekly N. of Infections")+
  #facet_grid(~prop_immune,labeller=label_both)+
  scale_x_date(date_breaks="3 months", date_labels="%b %y")+
  ggtitle("Infections")+
  scale_y_continuous(labels = scales::comma)+
  theme_minimal()+
  theme(legend.position = "none")

p_infections_cum = df_fits%>%filter(date!="1900-01-01") %>% mutate(mean_i_infections = mean_i_infections,
                                                                   lower_i_infections = lower_i_infections,
                                                                   upper_i_infections = upper_i_infections) %>%
  filter(date<"2023-10-01") %>% #Focus no first peak
  filter(date>="2022-10-01") %>%
  ggplot() +
  geom_ribbon(aes(x = date, ymin = lower_i_infections_cum, ymax = upper_i_infections_cum, fill=model),
              alpha = 0.3) +
  geom_line(aes(x = date, y = mean_i_infections_cum, color=model)) +
  xlab("") + ylab("Weekly N. of Infections")+
  #facet_grid(~prop_immune,labeller=label_both)+
  scale_x_date(date_breaks="3 months", date_labels="%b %y")+
  ggtitle("Infections")+
  theme_minimal()+
  theme(legend.position = "none")

p_immunity = df_fits%>%filter(date!="1900-01-01") %>%
  filter(date<"2023-10-01") %>% #Focus no first peak
  filter(date>="2022-10-01") %>%
  ggplot() +
  geom_ribbon(aes(x = date, ymin = 1-lower_i_susceptible/pop_paraguay, ymax = 1-upper_i_susceptible/pop_paraguay, fill=model),
              alpha = 0.3) +
  geom_line(aes(x = date, y = 1-mean_i_susceptible/pop_paraguay, color=model)) +
  xlab("") + ylab("Immunity")+
  #facet_grid(~prop_immune,labeller=label_both) +
  geom_pointrange(data = input_case %>% filter(date>="2022-10-01"), aes(x = date, y = prop_pos, ymin=prop_lo, ymax=prop_hi)) +
  expand_limits(y=c(0,1))+
  scale_x_date(date_breaks="3 months", date_labels="%b %y")+
  ggtitle("Immunity")+
  theme_minimal()+
  theme(legend.position = "none")

p_rt = df_fits%>%filter(date!="1900-01-01") %>%
  filter(date<"2023-10-01") %>% #Focus no first peak
  filter(date>="2022-10-01") %>%
  ggplot() +
  geom_hline(yintercept = 1, linetype="dashed", col=1) +
  geom_ribbon(data=extreme_temp, aes(x=as.Date(fecha), ymin=(tmin-10)/10, ymax=(tmax-10)/10), fill="#FFC107", alpha=0.3)+
  geom_line(data=weather_data, aes(x=as.Date(fecha), y=(avg_temp-10)/10), col="#FFC107")+
  geom_ribbon(aes(x = date, ymin =lower_rt, ymax = upper_rt), fill="#1E88E5", alpha=0.3) +
  geom_line(aes(x = date, y = mean_rt), color="#1E88E5") +
  xlab("") + ylab("R eff")+
  #facet_grid(~prop_immune,labeller=label_both) +
  expand_limits(y=c(0,1))+
  scale_x_date(date_breaks="3 months", date_labels="%b %y")+
  ggtitle("Effective Reproductive Number")+
  scale_y_continuous( sec.axis = sec_axis(~.*10+10, name="Temperature (°C)"))+
  theme_minimal()+
  theme(legend.position = "none")
p_rt

p_beta = df_fits%>%filter(date!="1900-01-01") %>%
  filter(date<"2023-10-01") %>% #Focus no first peak
  filter(date>="2022-10-01") %>%
  ggplot() +
  geom_ribbon(aes(x = date, ymin =lower_beta, ymax = upper_beta), fill=rgb(0,1,0,0.2)) +
  geom_line(aes(x = date, y = mean_beta), color=rgb(0,1,0)) +
  xlab("") + ylab("Beta")+
  #facet_grid(~prop_immune,labeller=label_both) +
  expand_limits(y=c(0,1))+
  theme(legend.position = "none")+
  #scale_x_date(breaks=as.Date(c("2023-01-01","2024-01-01")), date_labels="%Y")+
  ggtitle("Beta")+
  theme_minimal()

p = cowplot::plot_grid(p_case,p_infections, p_immunity,p_rt,ncol=2)
p
ggsave(p,filename = "output/fig/figure3_model_fit.pdf", width=7, height=7)
ggsave(p,filename = "output/fig/figure3_model_fit.png", width=7, height=7)


#Some numbers
df_fits[which.max(df_fits$mean_rt),grepl("rt",colnames(df_fits))] #Max Rt
df_fits %>% select(date, mean_rt)
df_fits %>% filter(date<"2022-12-30" & date>"2022-09-30") %>%summarise(mean_rt = mean(mean_rt),
                                                                       lo_rt = mean(lower_rt),
                                                                       hi_rt = mean(upper_rt))

df_fits = df_fits %>% mutate(cum_prop = cumsum(mean_i_infections)/sum(mean_i_infections)) #Weeks within xx% infection
difftime(df_fits$date[which(df_fits$cum_prop>0.75)[1]],df_fits$date[1],units="weeks")
difftime(df_fits$date[which(df_fits$cum_prop>0.95)[1]],df_fits$date[1],units="weeks")
difftime(df_fits$date[which(df_fits$cum_prop>0.95)[1]],df_fits$date[1],units="weeks")

weather_weekly = weather_data %>% mutate(date_week = lubridate::floor_date(fecha,"weeks")) %>% #Correaltion tempersture and Rt
  group_by(date_week) %>% summarise(temp_weekly=mean(avg_temp))
cor(df_fits$mean_rt[4:53] , weather_weekly$temp_weekly[1:50], method="pearson")

p_case_early = df_fits%>%filter(date!="1900-01-01") %>%
  filter(date<"2022-12-01") %>% #Focus no first peak
  ggplot() +
  geom_ribbon(aes(x = date, ymin = lower_i_cases, ymax = upper_i_cases, fill=model),
              alpha = 0.3) +
  geom_line(aes(x = date , y = mean_i_cases, color=model)) +
  geom_point(data = input_case  %>% filter(date<"2022-12-01" & date>"2022-01-01"), aes(x = date, y = i_cases_raw)) +
  geom_point(data = input_case  %>% filter(date<"2022-12-01" & date>"2022-01-01"), aes(x = date, y = i_cases), col=2) +
  #geom_line(data=scaling_vect, aes(x = time, y = scaling)) +
  xlab("") + ylab("Weekly N. of Cases")+
  #facet_grid(~prop_immune,labeller=label_both)+
  theme(legend.position = "none")+
  scale_x_date(date_breaks="3 months", date_labels="%b %y")+
  ggtitle("Cases")

p_infections_tot_early = df_fits%>%filter(date!="1900-01-01") %>% mutate(mean_i_infections = mean_i_infections,
                                                                         lower_i_infections = lower_i_infections,
                                                                         upper_i_infections = upper_i_infections) %>%
  filter(date<"2022-12-01") %>% #Focus no first peak
  ggplot() +
  geom_ribbon(aes(x = date, ymin = lower_i_infections_cum, ymax = upper_i_infections_cum, fill=model),
              alpha = 0.3) +
  geom_line(aes(x = date, y = mean_i_infections_cum, color=model)) +
  xlab("") + ylab("Total nb of Infections")+
  #facet_grid(~prop_immune,labeller=label_both)+
  theme(legend.position = "none")+
  scale_x_date(date_breaks="1 month", date_labels="%b %y")+
  ggtitle("Infections")

cowplot::plot_grid(p_case_early,p_infections_tot_early,ncol=2)
