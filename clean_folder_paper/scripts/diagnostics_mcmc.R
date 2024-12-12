rm(list=ls())

load("output/list_models_paper_singleloc_smoothed_temp.Rdata")

parm_set = 1:4
mcmc_steps <- 25000
mcmc_adaptive_steps <- 10000
thinning <- seq(mcmc_adaptive_steps + 1, mcmc_steps, by = 10) #10)

c_names <- c("prop_i0","Reporting","nb_shape", paste0("Transmission rate - week ",str_pad(as.character(1:(100-4)),2,pad="0")))
chain <- lapply(parm_set, function(chain_id) list_models$output[[chain_id]]$params%>% as.data.frame() %>% mutate(chain_id=chain_id, iter=1:n()) %>% relocate(chain_id, iter)) %>%
  do.call(what=rbind)
colnames(chain)[-c(1,2)] = c_names[1:(-2+ncol(chain))]

#Get posteriors------------
df_posterior = chain %>% filter(iter %in% thinning) %>%
  select(-c(nb_shape,prop_i0)) %>%
  tidyr::pivot_longer(cols = -c(iter, chain_id), names_to = "par", values_to = "value") %>%
  group_by(par) %>%
  summarise(med=median(value), lo=quantile(value,0.025), hi=quantile(value,0.975))



#Get diagnostics------------------
chain_coda = lapply(parm_set, function(ch_id){
  ch = chain %>% filter(chain_id==ch_id) %>% select(-c(chain_id,iter,nb_shape,prop_i0))
  coda::mcmc(data = ch)
})

chain_coda = coda::mcmc.list(chain_coda)
eff_df = coda::effectiveSize(chain_coda)
rhat_df = coda::gelman.diag(chain_coda)
#coda::gelman.plot(chain_coda)

#Join data frames
summary_mcmc = df_posterior %>% reframe(parameter=par,
                         prior=c("Uniform [0,1]", rep("Uniform [0,10]",55)),
                        posterior = paste0(signif(med,3), " (95%CI: ",signif(lo,3),"-",signif(hi,3),")"),
                        ess = signif(eff_df,3), r_hat=signif(rhat_df$psrf[,2],3))

write.csv(summary_mcmc, file="output/table_mcmc.csv", row.names=F)








