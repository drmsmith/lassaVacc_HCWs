#####################################
### SUMMARISE MAIN BURDEN REUSLTS ###
#####################################


### annual outcomes
df_vacc_outcomesAverted_summarised_nuisance_time_cumul%>%
  filter(outcome %in% c("cases", "snhl", "deaths", "dalys"),
         outbreakSize == vec_labels_outbreakSize[1],
         years == 1,
         VE == 0,
         prob_vacc == 0,
         turnover == vec_labels_turnover[1])

### vaccine impact over 10 years (40% uptake)
df_vacc_outcomesAverted_summarised_nuisance_time_cumul%>%
  filter(outcome %in% c("casesAverted", "snhlAverted", "deathsAverted", "dalysAverted"),
         outbreakSize == vec_labels_outbreakSize[1],
         years == 10,
         VE == "0.7",
         prob_vacc == 0.4,
         turnover == vec_labels_turnover[2])

### vaccine impact over 10 years (80% uptake)
df_vacc_outcomesAverted_summarised_nuisance_time_cumul%>%
  filter(outcome %in% c("casesAverted", "snhlAverted", "deathsAverted", "dalysAverted"),
         outbreakSize == vec_labels_outbreakSize[1],
         years == 10,
         VE == "0.7",
         prob_vacc == 0.8,
         turnover == vec_labels_turnover[2])

### vaccine impact over 10 years (80% uptake) and lower attrition
df_vacc_outcomesAverted_summarised_nuisance_time_cumul%>%
  filter(outcome %in% c("casesAverted", "snhlAverted", "deathsAverted", "dalysAverted"),
         outbreakSize == vec_labels_outbreakSize[1],
         years == 10,
         VE == "0.7",
         prob_vacc == 0.8,
         turnover == vec_labels_turnover[1])


### vaccine efficiency over 10 years
df_vacc_outcomesAverted_summarised_nuisance_time_cumul%>%
  filter(outcome %in% c("casesAvertedPerDose", "snhlAvertedPerDose", "deathsAvertedPerDose", "dalysAvertedPerDose"),
         outbreakSize == vec_labels_outbreakSize[1],
         years == 10,
         VE == "0.7",
         prob_vacc == 0.8,
         turnover == vec_labels_turnover[2])%>%
  mutate(mean = mean*1000,
         lower = lower*1000,
         upper = upper*1000)


df_vacc_outcomesAverted_summarised%>%
  filter(outcome %in% c("casesAverted", "snhlAverted", "deathsAverted", "dalysAverted"),
         outbreakSize == vec_labels_outbreakSize[4],
         VE == "0.7",
         prob_vacc == 0.8)

###########################################
### (9) PLOTS OF VACCINE IMPACT RESULTS ###
###########################################

save_plots = F



###########################################################################################
### (10) CONTOUR PLOTS OF OUTCOMES AVERTED AS A FUNCTION OF VACCINE EFFICACY AND UPTAKE ###
###########################################################################################

### DEFINE CONTOURS, DEPENDS ON APPROACH
if(which_approach %in% c("approach1", "approach2")){
  breaks_cases = c(0.3, 1, 3, 6, 10, 13)
  breaks_snhl = c(0.1, 0.5, 1, 2, 3)
  breaks_deaths = c(0.1, 0.5, 1, 2, 3)
  breaks_dalys = c(1, 10, 20, 50, 90)
}

if(which_approach %in% c("approach3")){
  breaks_cases = c(1, 5, 15, 25, 50)
  breaks_snhl = c(1, 2, 3, 5, 10)
  breaks_deaths = c(1, 2, 5, 8, 13)
  breaks_dalys = c(50, 100, 200, 300, 400)
}

### CASES ###
p_cases_averted_contours = df_vacc_outcomesAverted_summarised%>%
  filter(outcome == "casesAverted",
         outbreakSize != vec_labels_outbreakSize[1])%>%
  ggplot(., aes(x = prob_vacc*100, y = VE*100, fill = mean, z = mean))+
  geom_tile()+
  facet_wrap(facets = vars(outbreakSize), nrow = 3)+
  scale_fill_gradientn("Mean cases\naverted annually", colours= vec_cols_cases)+
  geom_contour(breaks = breaks_cases, colour = "black")+
  metR::geom_text_contour(aes(z = mean), stroke = 0.3, stroke.colour = "white", breaks = breaks_cases, skip = 0)+
  theme_bw()+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  xlab("HCW vaccine coverage (%)")+ylab("Vaccine efficacy (%)")
#geom_dl(aes(label=..level..), method="bottom.pieces", stat="contour",breaks = c(0.4, 1, 4, 10, 15))

if(save_plots){
  ggsave(p_cases_averted_contours, file = paste0("p_casesAverted_contours_", which_approach, ".png"), width = 7.5, height = 18, units = "cm")  
  
}

### SNHL ###
p_snhl_averted_contours = df_vacc_outcomesAverted_summarised%>%
  filter(outcome == "snhlAverted",
         outbreakSize != vec_labels_outbreakSize[1])%>%
  ggplot(., aes(x = prob_vacc*100, y = VE*100, fill = mean, z = mean))+
  geom_tile()+
  facet_wrap(facets = vars(outbreakSize), nrow = 3)+
  scale_fill_gradientn("Mean hearing loss\naverted annually", colours= vec_cols_snhl)+
  geom_contour(breaks = breaks_snhl, colour = "black")+
  metR::geom_text_contour(aes(z = mean), stroke = 0.3, stroke.colour = "white", breaks = breaks_snhl, skip = 0)+
  theme_bw()+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  xlab("HCW vaccine coverage (%)")+ylab("Vaccine efficacy (%)")

if(save_plots){
  ggsave(p_snhl_averted_contours, file = paste0("p_snhlAverted_contours_", which_approach, ".png"), width = 7.5, height = 18, units = "cm")  
}


### DEATHS ###
p_deaths_averted_contours = df_vacc_outcomesAverted_summarised%>%
  filter(outcome == "deathsAverted",
         outbreakSize != vec_labels_outbreakSize[1])%>%
  ggplot(., aes(x = prob_vacc*100, y = VE*100, fill = mean, z = mean))+
  geom_tile()+
  facet_wrap(facets = vars(outbreakSize), nrow = 3)+
  scale_fill_gradientn("Mean deaths\naverted annually", colours= vec_cols_deaths)+
  geom_contour(breaks = breaks_deaths, colour = "black")+
  metR::geom_text_contour(aes(z = mean), stroke = 0.3, stroke.colour = "white", breaks = breaks_deaths, skip = 0)+
  theme_bw()+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  xlab("HCW vaccine coverage (%)")+ylab("Vaccine efficacy (%)")

if(save_plots){
  ggsave(p_deaths_averted_contours, file = paste0("p_deathsAverted_contours_", which_approach, ".png"), width = 7.5, height = 18, units = "cm")  
  
}


### DALYs ###
p_dalys_averted_contours = df_vacc_outcomesAverted_summarised%>%
  filter(outcome == "dalysAverted",
         outbreakSize != vec_labels_outbreakSize[1])%>%
  ggplot(., aes(x = prob_vacc*100, y = VE*100, fill = mean, z = mean))+
  geom_tile()+
  facet_wrap(facets = vars(outbreakSize), nrow = 3)+
  scale_fill_gradientn("Mean DALYs\naverted annually", colours= vec_cols_dalys)+
  geom_contour(breaks = breaks_dalys, colour = "black")+
  metR::geom_text_contour(aes(z = mean), stroke = 0.3, stroke.colour = "white", breaks = breaks_dalys, skip = 0)+
  theme_bw()+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  xlab("HCW vaccine coverage (%)")+ylab("Vaccine efficacy (%)") 

if(save_plots){
  ggsave(p_dalys_averted_contours, file = paste0("p_dalysAverted_contours_", which_approach, ".png"), width = 7.5, height = 18, units = "cm")  
  
}

### DALYs with base case outline
p_dalys_averted_contours_basecase = p_dalys_averted_contours+ 
  geom_point(data = df_vacc_outcomesAverted_summarised%>%
               filter(outcome == "dalysAverted",
                      outbreakSize == vec_labels_outbreakSize[3],
                      VE == "0.7", prob_vacc == 0.8),
             fill = NA, color = "red", size = 3, shape = 22, stroke = 2)+
  geom_text(data = data.frame(prob_vacc = 0.725, VE = 0.7, mean = 1, outbreakSize = vec_labels_outbreakSize[3]), 
            label = "Base\ncase", hjust = 1, colour = "red", size = 3, lineheight = .75)




###############################################################
### (11) PLOTS OF OUTCOMES AVERTED AS FUNCTION OF ATTRITION ###
###      DECLINING IMPACT THROUGH TIME WITH HCW TURNOVER    ###
###############################################################

##########################
### (11) CASES AVERTED ###
##########################

### DEFINE RISK LEVELS, DEPENDS ON APPROACH
if(which_approach %in% c("approach1", "approach2")){
  risk_levels = c(0.05, 0.1, 0.25)
  risk_labels = c("5% annual outbreak risk", "10% annual outbreak risk", "25% annual outbreak risk")
}

if(which_approach %in% c("approach3", "approach4")){
  risk_levels = c(1)
  risk_labels = "Stable baseline outbreak risk"
}

### ANNUAL
p_casesAverted_annual = df_vacc_outcomesAverted_summarised_nuisance_time%>%
  mutate(VE = factor(VE),
         years = factor(years))%>%
  filter(VE == 0.7,
         outcome == "casesAverted",
         prob_vacc == 0.8,
         outbreakSize %in% vec_labels_outbreakSize[c(3)],
         risk %in% risk_levels,
         turnover %in% c("5% annual turnover", "15% annual turnover", "50% annual turnover"))%>%
  mutate(risk = factor(risk, labels = risk_labels),
         turnover = factor(turnover,
                           levels = c("5% annual turnover", "15% annual turnover", "50% annual turnover"), 
                           labels = c("5%", "15%", "50%")))%>%
  ggplot(., aes(x = years, y = mean, ymin = lower, ymax = upper, colour = turnover, fill = turnover))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Years since vaccination campaign")+ylab("Cases averted annually")+
  facet_wrap(facets = vars(risk), nrow = 3)+
  scale_fill_manual("Annual healthcare worker turnover", values = vec_cols_turnover)

if(save_plots){
  ggsave(p_casesAverted_annual, file = paste0("p_casesAverted_annual_", which_approach, ".png"),
         width = 12, height = 18, units = "cm")
}


### CUMULATIVE
p_casesAverted_cumul = df_vacc_outcomesAverted_summarised_nuisance_time_cumul%>%
  mutate(VE = factor(VE),
         years = factor(years))%>%
  filter(VE == 0.7,
         outcome == "casesAverted",
         prob_vacc == 0.8,
         outbreakSize %in% vec_labels_outbreakSize[c(3)],
         risk %in% risk_levels,
         turnover %in% c("5% annual turnover", "15% annual turnover", "50% annual turnover"),
         years %in% seq(2, 10, by = 2))%>%
  mutate(risk = factor(risk, labels = risk_labels),
         turnover = factor(turnover,
                           levels = c("5% annual turnover", "15% annual turnover", "50% annual turnover"), 
                           labels = c("5%", "15%", "50%")))%>%
  ggplot(., aes(x = years, y = mean, ymin = lower, ymax = upper, colour = turnover, fill = turnover))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Years since vaccination campaign")+ylab("Cumulative cases averted")+
  facet_wrap(facets = vars(risk), nrow = 3)+
  scale_fill_manual("Annual healthcare worker turnover", values = vec_cols_turnover)

if(save_plots){
  ggsave(p_casesAverted_cumul, file = paste0("p_casesAverted_cumul_", which_approach, ".png"),
         width = 12, height = 18, units = "cm")
}


###########################
### (11) DEATHS AVERTED ###
###########################

### ANNUAL
p_deathsAverted_annual = df_vacc_outcomesAverted_summarised_nuisance_time%>%
  mutate(VE = factor(VE),
         years = factor(years))%>%
  filter(VE == 0.7,
         outcome == "deathsAverted",
         prob_vacc == 0.8,
         outbreakSize %in% vec_labels_outbreakSize[c(3)],
         risk %in% risk_levels,
         turnover %in% c("5% annual turnover", "15% annual turnover", "50% annual turnover"))%>%
  mutate(risk = factor(risk, labels = risk_labels),
         turnover = factor(turnover,
                           levels = c("5% annual turnover", "15% annual turnover", "50% annual turnover"), 
                           labels = c("5%", "15%", "50%")))%>%
  ggplot(., aes(x = years, y = mean, ymin = lower, ymax = upper, colour = turnover, fill = turnover))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Years since vaccination campaign")+ylab("Deaths averted annually")+
  facet_wrap(facets = vars(risk), nrow = 3)+
  scale_fill_manual("Annual healthcare worker turnover", values = vec_cols_turnover)

if(save_plots){
  ggsave(p_deathsAverted_annual, file = paste0("p_deathsAverted_annual_", which_approach, ".png"),
         width = 12, height = 18, units = "cm")
}


### CUMULATIVE
p_deathsAverted_cumul = df_vacc_outcomesAverted_summarised_nuisance_time_cumul%>%
  mutate(VE = factor(VE),
         years = factor(years))%>%
  filter(VE == 0.7,
         outcome == "deathsAverted",
         prob_vacc == 0.8,
         outbreakSize %in% vec_labels_outbreakSize[c(3)],
         risk %in% risk_levels,
         turnover %in% c("5% annual turnover", "15% annual turnover", "50% annual turnover"),
         years %in% seq(2, 10, by = 2))%>%
  mutate(risk = factor(risk, labels = risk_labels),
         turnover = factor(turnover,
                           levels = c("5% annual turnover", "15% annual turnover", "50% annual turnover"), 
                           labels = c("5%", "15%", "50%")))%>%
  ggplot(., aes(x = years, y = mean, ymin = lower, ymax = upper, colour = turnover, fill = turnover))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Years since vaccination campaign")+ylab("Cumulative deaths averted")+
  facet_wrap(facets = vars(risk), nrow = 3)+
  scale_fill_manual("Annual healthcare worker turnover", values = vec_cols_turnover)

if(save_plots){
  ggsave(p_deathsAverted_cumul, file = paste0("p_deathsAverted_cumul_", which_approach, ".png"),
         width = 12, height = 18, units = "cm")
}


##########################
### (11) DALYS AVERTED ###
##########################

### ANNUAL
p_dalysAverted_annual = df_vacc_outcomesAverted_summarised_nuisance_time%>%
  mutate(VE = factor(VE),
         years = factor(years))%>%
  filter(VE == 0.7,
         outcome == "dalysAverted",
         prob_vacc == 0.8,
         outbreakSize %in% vec_labels_outbreakSize[c(3)],
         risk %in% risk_levels,
         turnover %in% c("5% annual turnover", "15% annual turnover", "50% annual turnover"))%>%
  mutate(risk = factor(risk, labels = risk_labels),
         turnover = factor(turnover,
                           levels = c("5% annual turnover", "15% annual turnover", "50% annual turnover"), 
                           labels = c("5%", "15%", "50%")))%>%
  ggplot(., aes(x = years, y = mean, ymin = lower, ymax = upper, colour = turnover, fill = turnover))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Years since vaccination campaign")+ylab("DALYs averted annually")+
  facet_wrap(facets = vars(risk), nrow = 3)+
  scale_fill_manual("Annual healthcare worker turnover", values = vec_cols_turnover)

if(save_plots){
  ggsave(p_dalysAverted_annual, file = paste0("p_dalysAverted_annual_", which_approach, ".png"),
         width = 12, height = 18, units = "cm")
}


### CUMULATIVE
p_dalysAverted_cumul = df_vacc_outcomesAverted_summarised_nuisance_time_cumul%>%
  mutate(VE = factor(VE),
         years = factor(years))%>%
  filter(VE == 0.7,
         outcome == "dalysAverted",
         prob_vacc == 0.8,
         outbreakSize %in% vec_labels_outbreakSize[c(3)],
         risk %in% risk_levels,
         turnover %in% c("5% annual turnover", "15% annual turnover", "50% annual turnover"),
         years %in% seq(2, 10, by = 2))%>%
  mutate(risk = factor(risk, labels = risk_labels),
         turnover = factor(turnover,
                           levels = c("5% annual turnover", "15% annual turnover", "50% annual turnover"), 
                           labels = c("5%", "15%", "50%")))%>%
  ggplot(., aes(x = years, y = mean, ymin = lower, ymax = upper, colour = turnover, fill = turnover))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Years since vaccination campaign")+ylab("Cumulative DALYs averted")+
  facet_wrap(facets = vars(risk), nrow = 3)+
  scale_fill_manual("Annual healthcare worker turnover", values = vec_cols_turnover)

if(save_plots){
  ggsave(p_dalysAverted_cumul, file = paste0("p_dalysAverted_cumul_", which_approach, ".png"),
         width = 12, height = 18, units = "cm")
}



#################################################################
### (12) PLOTS OF OUTCOMES AVERTED AS FUNCTION OF VACC UPTAKE ###
###      DECLINING IMPACT THROUGH TIME WITH HCW TURNOVER      ###
#################################################################

##############
### DEATHS ###
##############

### ANNUAL
p_deathsAverted_annual_uptake = df_vacc_outcomesAverted_summarised_nuisance_time%>%
  mutate(VE = factor(VE),
         years = factor(years),
         prob_vacc = factor(prob_vacc))%>%
  filter(VE == 0.7,
         outcome == "deathsAverted",
         prob_vacc %in% c(0.4, 0.6, 0.8),
         outbreakSize %in% vec_labels_outbreakSize[c(3)],
         risk == 1,
         turnover %in% c("15% annual turnover"),
         years %in% seq(2, 10, by = 2))%>%
  mutate(prob_vacc = factor(prob_vacc, levels = c(0.4, 0.6, 0.8), labels = c("40%", "60%", "80%")))%>%
  ggplot(., aes(x = years, y = mean, ymin = lower, ymax = upper, fill = prob_vacc))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Years since vaccination campaign")+ylab("Deaths averted annually")+
  scale_fill_manual("Percentage of healthcare\nworkers vaccinated", values = vec_cols_turnover)

if(save_plots){
  ggsave(p_deathsAverted_annual_uptake, file = paste0("p_deathsAverted_annual_uptake_", which_approach, ".png"),
         width = 12, height = 18, units = "cm")
}


### CUMULATIVE OVER TEN YEARS
p_deathsAverted_cumul_uptake = df_vacc_outcomesAverted_summarised_nuisance_time_cumul%>%
  mutate(VE = factor(VE),
         years = factor(years),
         prob_vacc = factor(prob_vacc))%>%
  filter(VE == 0.7,
         outcome == "deathsAverted",
         prob_vacc %in% c(0.4, 0.6, 0.8),
         outbreakSize %in% vec_labels_outbreakSize[c(1)],
         risk == 1,
         turnover %in% c("15% annual turnover"),
         years %in% seq(2, 10, by = 2))%>%
  mutate(prob_vacc = factor(prob_vacc, levels = c(0.4, 0.6, 0.8), labels = c("40%", "60%", "80%")))%>%
  ggplot(., aes(x = years, y = mean, ymin = lower, ymax = upper, fill = prob_vacc))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Years since vaccination campaign")+ylab("Cumulative deaths averted")+
  scale_fill_manual("Percentage of healthcare\nworkers vaccinated", values = vec_cols_turnover)+
  scale_y_continuous(labels = comma)

if(save_plots){
  ggsave(p_deathsAverted_cumul_uptake, file = paste0("p_deathsAverted_cumul_uptake_", which_approach, ".png"),
         width = 12, height = 18, units = "cm")
}


### CUMULATIVE DEATHS AVERTED OVER TEN YEARS BY ATTRITION
p_deathsAverted_cumul_attrition = df_vacc_outcomesAverted_summarised_nuisance_time_cumul%>%
  mutate(VE = factor(VE),
         years = factor(years),
         prob_vacc = factor(prob_vacc))%>%
  filter(VE == 0.7,
         outcome == "deathsAverted",
         prob_vacc %in% c(0.8),
         outbreakSize %in% vec_labels_outbreakSize[c(1)],
         risk == 1,
         turnover %in% c("5% annual turnover", "15% annual turnover", "25% annual turnover"),
         years %in% seq(1, 10, by = 1))%>%
  mutate(turnover = factor(turnover, 
                           levels = c("5% annual turnover", "15% annual turnover", "25% annual turnover"), 
                           labels = c("5%", "15%", "25%")))%>%
  ggplot(., aes(x = years, y = mean, ymin = lower, ymax = upper, fill = turnover))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right")+
  xlab("Years since vaccination campaign")+ylab("Cumulative deaths averted")+
  scale_fill_manual("Annual HCW\nattrition rate", values = vec_cols_attrition)+
  scale_y_continuous(labels = comma)

### CUMULATIVE DEATHS AVERTED OVER TEN YEARS BY VE
p_deathsAverted_cumul_VE = df_vacc_outcomesAverted_summarised_nuisance_time_cumul%>%
  mutate(VE = factor(VE),
         years = factor(years),
         prob_vacc = factor(prob_vacc))%>%
  filter(VE %in% c(0.5, 0.7, 0.9),
         outcome == "deathsAverted",
         prob_vacc %in% c(0.8),
         outbreakSize %in% vec_labels_outbreakSize[c(1)],
         risk == 1,
         turnover == "15% annual turnover",
         years %in% seq(1, 10, by = 1))%>%
  mutate(VE = factor(VE, 
                           levels = c(0.5, 0.7, 0.9), 
                           labels = c("50%", "70%", "90%")))%>%
  ggplot(., aes(x = years, y = mean, ymin = lower, ymax = upper, fill = VE))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right")+
  xlab("Years since vaccination campaign")+ylab("Cumulative deaths averted")+
  scale_fill_manual("Vaccine efficacy", values = rev(vec_cols_VE))+
  scale_y_continuous(labels = comma)

p_deathsAverted_cumul_SA = plot_grid(p_deathsAverted_cumul_attrition,
          p_deathsAverted_cumul_VE,
          nrow = 2,
          labels = c("A", "B"),
          align = "v",
          axis = "lr")

if(save_plots){
  ggsave(p_deathsAverted_cumul_SA, file = paste0("p_deathsAverted_cumul_SA_", which_approach, ".png"),
         width = 18, height = 18, units = "cm")
}

#############
### DALYS ###
#############

### ANNUAL
p_dalysAverted_annual_uptake = df_vacc_outcomesAverted_summarised_nuisance_time%>%
  mutate(VE = factor(VE),
         years = factor(years),
         prob_vacc = factor(prob_vacc))%>%
  filter(VE == 0.7,
         outcome == "dalysAverted",
         prob_vacc %in% c(0.4, 0.6, 0.8),
         outbreakSize %in% vec_labels_outbreakSize[c(3)],
         risk == 1,
         turnover %in% c("15% annual turnover"),
         years %in% seq(2, 10, by = 2))%>%
  mutate(prob_vacc = factor(prob_vacc, levels = c(0.4, 0.6, 0.8), labels = c("40%", "60%", "80%")))%>%
  ggplot(., aes(x = years, y = mean, ymin = lower, ymax = upper, fill = prob_vacc))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Years since vaccination campaign")+ylab("DALYs averted annually")+
  scale_fill_manual("Percentage of healthcare\nworkers vaccinated", values = vec_cols_turnover)

if(save_plots){
  ggsave(p_dalysAverted_annual_uptake, file = paste0("p_dalysAverted_annual_uptake_", which_approach, ".png"),
         width = 12, height = 18, units = "cm")
}


### CUMULATIVE OVER TEN YEARS
p_dalysAverted_cumul_uptake = df_vacc_outcomesAverted_summarised_nuisance_time_cumul%>%
  mutate(VE = factor(VE),
         years = factor(years),
         prob_vacc = factor(prob_vacc))%>%
  filter(VE == 0.7,
         outcome == "dalysAverted",
         prob_vacc %in% c(0.4, 0.6, 0.8),
         outbreakSize %in% vec_labels_outbreakSize[c(1)],
         risk == 1,
         turnover %in% c("15% annual turnover"),
         years %in% seq(2, 10, by = 2))%>%
  mutate(prob_vacc = factor(prob_vacc, levels = c(0.4, 0.6, 0.8), labels = c("40%", "60%", "80%")))%>%
  ggplot(., aes(x = years, y = mean, ymin = lower, ymax = upper, fill = prob_vacc))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Years since vaccination campaign")+ylab("Cumulative DALYs averted")+
  scale_fill_manual("Percentage of healthcare\nworkers vaccinated", values = vec_cols_turnover)+
  scale_y_continuous(labels = comma)

if(save_plots){
  ggsave(p_dalysAverted_cumul_uptake, file = paste0("p_dalysAverted_cumul_uptake_", which_approach, ".png"),
         width = 12, height = 18, units = "cm")
}





#############################################################
### (13) CHARACTERISTIC HOSPITAL OUTBREAKS ONLY:          ###
### CONTOUR PLOTS OF CUMULATIVE OUTCOMES AVERTED PER DOSE ###
#############################################################

if(which_approach %in% c("approach1", "approach2")){
  
  ##################
  ### (13) CASES ###
  ##################
  
  p_casesAvertedPerDose_byRisk = df_vacc_outcomesAverted_summarised_nuisance%>%
    mutate(VE = factor(VE))%>%
    filter(VE %in% vec_levels_VE,
           risk %in% c(0, 0.01, 0.02, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5),
           outcome == "casesAvertedPerDose",
           prob_vacc == 0.8,
           outbreakSize == vec_labels_outbreakSize[3],
           turnover == "15% annual turnover")%>%
    mutate(VE = factor(VE, 
                       levels = vec_levels_VE,
                       labels = vec_labels_VE),
           risk = factor(risk,
                         levels = c(0, 0.01, 0.02, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5),
                         labels = paste0(c(0, 0.01, 0.02, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)*100, "%")))%>%
    ggplot(., aes(x = VE, y = risk, fill = mean*1000, label = label))+
    geom_tile()+
    theme_classic()+
    xlab("Vaccine efficacy")+ylab("Annual outbreak risk")+
    scale_fill_gradientn("Mean cases\naverted per\n1,000 doses\n(cumulative)", colours = vec_cols_dalysPerDose)+
    geom_text()+
    theme(legend.position = "bottom")
  
  if(save_plots){
    ggsave(p_casesAvertedPerDose_byRisk, file = paste0("p_casesAvertedPerDose_byRisk_", which_approach, ".png"),
           width = 12, height = 18, units = "cm")
  }
  
  
  ###################
  ### (13) DEATHS ###
  ###################
  
  p_deathsAvertedPerDose_byRisk = df_vacc_outcomesAverted_summarised_nuisance%>%
    mutate(VE = factor(VE))%>%
    filter(VE %in% vec_levels_VE,
           risk %in% c(0, 0.01, 0.02, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5),
           outcome == "deathsAvertedPerDose",
           prob_vacc == 0.8,
           outbreakSize == vec_labels_outbreakSize[3],
           turnover == "15% annual turnover")%>%
    mutate(VE = factor(VE, 
                       levels = vec_levels_VE,
                       labels = vec_labels_VE),
           risk = factor(risk,
                         levels = c(0, 0.01, 0.02, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5),
                         labels = paste0(c(0, 0.01, 0.02, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)*100, "%")))%>%
    ggplot(., aes(x = VE, y = risk, fill = mean*1000, label = label))+
    geom_tile()+
    theme_classic()+
    xlab("Vaccine efficacy")+ylab("Annual outbreak risk")+
    scale_fill_gradientn("Mean deaths\naverted per\n1,000 doses\n(cumulative)", colours = vec_cols_dalysPerDose)+
    geom_text()+
    theme(legend.position = "bottom")
  
  if(save_plots){
    ggsave(p_deathsAvertedPerDose_byRisk, file = paste0("p_deathsAvertedPerDose_byRisk_", which_approach, ".png"),
           width = 12, height = 18, units = "cm")
  }
  
  
  ##################
  ### (13) DALYS ###
  ##################
  
  p_dalysAvertedPerDose_byRisk = df_vacc_outcomesAverted_summarised_nuisance%>%
    mutate(VE = factor(VE))%>%
    filter(VE %in% vec_levels_VE,
           risk %in% c(0, 0.01, 0.02, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5),
           outcome == "dalysAvertedPerDose",
           prob_vacc == 0.8,
           outbreakSize == vec_labels_outbreakSize[3],
           turnover == "15% annual turnover")%>%
    mutate(VE = factor(VE, 
                       levels = vec_levels_VE,
                       labels = vec_labels_VE),
           risk = factor(risk,
                         levels = c(0, 0.01, 0.02, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5),
                         labels = paste0(c(0, 0.01, 0.02, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)*100, "%")))%>%
    ggplot(., aes(x = VE, y = risk, fill = mean*1000, label = label))+
    geom_tile()+
    theme_classic()+
    xlab("Vaccine efficacy")+ylab("Annual outbreak risk")+
    scale_fill_gradientn("Mean DALYs\naverted per\n1,000 doses\n(cumulative)", colours = vec_cols_dalysPerDose)+
    geom_text()+
    theme(legend.position = "bottom")
  
  if(save_plots){
    ggsave(p_dalysAvertedPerDose_byRisk, file = paste0("p_dalysAvertedPerDose_byRisk_", which_approach, ".png"),
           width = 12, height = 18, units = "cm")
  }
  
  
}



#############################################################
### (13) NATIONAL LEVEL OUTBREAKS ONLY:                   ###
### CONTOUR PLOTS OF CUMULATIVE OUTCOMES AVERTED PER DOSE ###
### COMPARED TO PREVIOUS RISK-TARGETED RESULTS            ###
#############################################################

if(which_approach %in% c("approach3", "approach4")){
  
  
  ##########################################################################
  ### (XX) HEAT MAP OF VACCINE IMPACT PER DOSE RELATIVE TO PREVIOUS WORK ###
  ##########################################################################
  
  #############
  ### CASES ###
  #############
  
  ### combine risk targeted data and HCW data
  df_vacc_casesAverted_risk_HCWs = df_vacc_outcomesAverted_summarised_nuisance%>%
    ungroup()%>%
    mutate(VE = factor(VE),
           strategy = "Healthcare workers",
           mean = mean * 1000,
           lower = lower * 1000,
           upper = upper * 1000)%>%
    filter(VE %in% vec_levels_VE,
           outcome == "casesAvertedPerDose",
           prob_vacc == 0.8,
           outbreakSize == vec_labels_outbreakSize[1],
           turnover == "15% annual turnover")%>%
    dplyr::select(VE, mean, lower, upper, label, context, strategy, outcome)%>%
    bind_rows(., df_vacc_RiskTargeted_cases%>%
                mutate(strategy = factor(strategy,
                                         levels = vec_levels_strategy,
                                         labels = vec_labels_strategy),
                       VE = factor(VE))
    )
  
  p_casesAvertedPerDose_byRisk = df_vacc_casesAverted_risk_HCWs%>%
    mutate(VE = factor(VE,
                       levels = vec_levels_VE,
                       labels = vec_labels_VE),
    )%>%
    ggplot(., aes(x = VE, y = strategy, fill = mean, label = label))+
    geom_tile()+
    theme_classic()+
    xlab("Vaccine efficacy")+ylab("Vaccine target group\n")+
    scale_fill_gradientn("Cumulative cases\naverted per\n1,000 doses", 
                         colours = vec_cols_cases, limits = c(0, 1.15))+
    geom_text()+
    theme(legend.position = "right")+
    facet_grid(rows = vars(context), scale = "free_y", space = "free_y")+
    theme(strip.background = element_blank(),
          strip.text = element_blank())
  
  
  ##############
  ### DEATHS ###
  ##############
  
  ### combine risk targeted data and HCW data
  df_vacc_deathsAverted_risk_HCWs = df_vacc_outcomesAverted_summarised_nuisance%>%
    ungroup()%>%
    mutate(VE = factor(VE),
           strategy = "Healthcare workers",
           mean = mean * 1000,
           lower = lower * 1000,
           upper = upper * 1000)%>%
    filter(VE %in% vec_levels_VE,
           outcome == "deathsAvertedPerDose",
           prob_vacc == 0.8,
           outbreakSize == vec_labels_outbreakSize[1],
           turnover == "15% annual turnover")%>%
    dplyr::select(VE, mean, lower, upper, label, context, strategy, outcome)%>%
    bind_rows(., df_vacc_RiskTargeted_deaths%>%
                mutate(strategy = factor(strategy,
                                         levels = vec_levels_strategy,
                                         labels = vec_labels_strategy),
                       VE = factor(VE))
    )
  
  p_deathsAvertedPerDose_byRisk = df_vacc_deathsAverted_risk_HCWs%>%
    mutate(VE = factor(VE,
                       levels = vec_levels_VE,
                       labels = vec_labels_VE),
    )%>%
    ggplot(., aes(x = VE, y = strategy, fill = mean, label = label))+
    geom_tile()+
    theme_classic()+
    xlab("Vaccine efficacy")+ylab("Vaccine target group\n")+
    scale_fill_gradientn("Cumulative deaths\naverted per\n1,000 doses", 
                         colours = vec_cols_deaths, limits = c(0, 0.38))+
    geom_text()+
    theme(legend.position = "right")+
    facet_grid(rows = vars(context), scale = "free_y", space = "free_y")+
    theme(strip.background = element_blank(),
          strip.text = element_blank())
  
  
  ################################
  ### COMBINE CASES AND DEATHS ###
  ################################
  
  p_grid_casesAvertedPerDose_deathsAvertedPerDose = plot_grid(
    p_casesAvertedPerDose_byRisk,
    p_deathsAvertedPerDose_byRisk,
    nrow = 2,
    align = "hv",
    axis = "tblr",
    labels = c("A", "B")
  )
  
  if(save_plots){
    ggsave(p_grid_casesAvertedPerDose_deathsAvertedPerDose, file = paste0("p_grid_casesAvertedPerDose_deathsAvertedPerDose_", which_approach, ".png"),
           width = 18, height = 18, units = "cm")
  }
  
  #############
  ### DALYS ###
  #############
  
  ### combine risk targeted data and HCW data
  df_vacc_dalysAverted_risk_HCWs = df_vacc_outcomesAverted_summarised_nuisance%>%
    ungroup()%>%
    mutate(VE = factor(VE),
           strategy = "Healthcare workers",
           mean = mean * 1000,
           lower = lower * 1000,
           upper = upper * 1000)%>%
    filter(VE %in% vec_levels_VE,
           outcome == "dalysAvertedPerDose",
           prob_vacc == 0.8,
           outbreakSize == vec_labels_outbreakSize[1],
           turnover == "15% annual turnover")%>%
    dplyr::select(VE, mean, lower, upper, label, context, strategy, outcome)%>%
    bind_rows(., df_vacc_RiskTargeted_dalys%>%
                mutate(strategy = factor(strategy,
                                         levels = vec_levels_strategy,
                                         labels = vec_labels_strategy),
                       VE = factor(VE))
    )
  
  p_dalysAvertedPerDose_byRisk = df_vacc_dalysAverted_risk_HCWs%>%
    mutate(VE = factor(VE,
                       levels = vec_levels_VE,
                       labels = vec_labels_VE),
           )%>%
    ggplot(., aes(x = VE, y = strategy, fill = mean, label = label))+
    geom_tile()+
    theme_classic()+
    xlab("Vaccine efficacy")+ylab("Vaccine target group\n")+
    scale_fill_gradientn("Cumulative DALYs averted\nper 1,000 doses", 
                         colours = vec_cols_dalysPerDose, limits = c(0, 10))+
    geom_text()+
    theme(legend.position = "bottom")+
    facet_grid(rows = vars(context), scale = "free_y", space = "free_y")+
    theme(strip.background = element_blank(),
          strip.text = element_blank())
  
  if(save_plots){
    ggsave(p_dalysAvertedPerDose_byRisk, file = paste0("p_dalysAvertedPerDose_byRisk_", which_approach, ".png"),
           width = 12, height = 18, units = "cm")
  }
  
  
  
  ### SENSITIVITY ANALYSIS: HOW EFFICIENCY OUTCOMES VARY WITH VE AND ATTRITION
  df_vacc_outcomesAverted_SA = df_vacc_outcomesAverted_summarised_nuisance%>%
    mutate(VE = factor(VE))%>%
    filter(outcome == "dalysAvertedPerDose",
           prob_vacc == 0.8,
           outbreakSize == vec_labels_outbreakSize[1],
           VE %in% seq(0.5,1,by=0.1))%>%
    mutate(turnover = factor(turnover,
                             levels = c("5% annual turnover", 
                                        "10% annual turnover", 
                                        "15% annual turnover",
                                        "25% annual turnover"),
                             labels = c("5%", "10%", "15%", "25%")),
           VE = factor(VE, 
                       levels = seq(0.5,1,by=0.1),
                       labels = paste0(seq(50,100,by=10), "%")))
  
  p_dalysAvertedPerDose_SA_attrition = df_vacc_outcomesAverted_SA%>%
    ggplot(., aes(x = VE, y = turnover, fill = mean*1000, label = label))+
    geom_tile()+
    theme_classic()+
    xlab("Vaccine efficacy")+ylab("Annual HCW attrition rate\n")+
    scale_fill_gradientn("Cumulative DALYs averted per 1,000 doses", 
                         colours = vec_cols_dalysPerDose, limits = c(0, 17))+
    geom_text()+
    theme(legend.position = "bottom")+
    facet_grid(rows = vars(context), scale = "free_y", space = "free_y")+
    theme(strip.background = element_blank(),
          strip.text = element_blank())
  
  
  if(save_plots){
    ggsave(p_dalysAvertedPerDose_SA_attrition, file = paste0("p_dalysAvertedPerDose_SA_attrition_", which_approach, ".png"),
           width = 16, height = 10, units = "cm")
  }
  
  
}


#############################
### (14) FINAL PLOT GRIDS ###
#############################

##############################
### (14) DATA / PARAMETERS ###
##############################

# p_grid_params_fits = plot_grid(p_popSize_fit,
#                                p_outbreaksize_fit,
#                                p_ages_deaths_fit,
#                                ncol = 3,
#                                align = "h",
#                                axis = "tb",
#                                labels = c("A", "B", "C"))
# 
# if(save_plots){
#   ggsave(p_grid_params_fits, file = paste0("p_grid_params_fits_", which_approach, ".png"),
#          width = 18, height = 12, units = "cm")
# }


#####################
### (14) CONTOURS ###
#####################

# with DALYs
p_grid_contours_cases_snhl_deaths_dalys = plot_grid(p_cases_averted_contours,
                                                    p_snhl_averted_contours,
                                                    p_deaths_averted_contours,
                                                    p_dalys_averted_contours,
                                                    ncol = 4, labels = c("A", "B", "C", "D"),
                                                    align = "h", axis = "tb")

if(save_plots){
  ggsave(p_grid_contours_cases_snhl_deaths_dalys, file = paste0("p_grid_contours_cases_snhl_deaths_dalys_", which_approach, ".png"),
         width = 24, height = 18, units = "cm")
}



# without DALYs
p_grid_contours_cases_snhl_deaths = plot_grid(p_cases_averted_contours,
                                              p_snhl_averted_contours,
                                              p_deaths_averted_contours,
                                              ncol = 3, labels = c("A", "B", "C"),
                                              align = "h", axis = "tb")

if(save_plots){
  ggsave(p_grid_contours_cases_snhl_deaths, file = paste0("p_grid_contours_cases_snhl_deaths_", which_approach, ".png"),
         width = 20, height = 20, units = "cm")
}



##########################
### (14) MIXED FIGURES ###
##########################

### CHARACTERISTIC HOSPITAL
if(which_approach %in% c("approach1", "approach2")){
  ### DALYs
  p_grid_final_dalys = plot_grid(p_dalys_averted_contours_basecase, 
                                 p_dalysAverted_cumul,
                                 p_dalysAvertedPerDose_byRisk,
                                 ncol = 3,
                                 labels = c("A", "B", "C"),
                                 align = "h",
                                 axis = "tb",
                                 rel_widths = c(0.5,1, 0.75)
  )
  
  if(save_plots){
    ggsave(p_grid_final_dalys, file = paste0("p_grid_final_dalys_", which_approach, ".png"),
           width = 28, height = 18, units = "cm")
  }
}


### NATIONAL ASSESSMENT
if(which_approach %in% c("approach3", "approach4")){
  
  ### weekly cases, cumulative dalys averted, dalysAvertedPerDose 
  p_grid_final1 = plot_grid(p_cases_monthly_byState,
                                 plot_grid(p_dalysAverted_cumul_uptake,
                                           p_dalysAvertedPerDose_byRisk,
                                           ncol = 2,
                                           labels = c("B", "C"),
                                           align = "h",
                                           axis = "tb",
                                           rel_widths = c(1, 1)),
                                 nrow = 2, labels = c("A", ""),
                                 rel_heights = c(1, 1)
  )
  
  ### weekly cases, cumulative deaths averted, dalysAvertedPerDose 
  p_grid_final2 = plot_grid(p_cases_monthly_byState,
                            plot_grid(p_deathsAverted_cumul_uptake,
                                      p_dalysAvertedPerDose_byRisk,
                                      ncol = 2,
                                      labels = c("B", "C"),
                                      align = "h",
                                      axis = "tb",
                                      rel_widths = c(1, 1)),
                            nrow = 2, labels = c("A", ""),
                            rel_heights = c(1, 1)
  )
  
  ### weekly cases, annual DALYs averted, dalysAvertedPerDose 
  p_grid_final3 = plot_grid(p_cases_monthly_byState,
                            plot_grid(p_dalysAverted_annual_uptake,
                                      p_dalysAvertedPerDose_byRisk,
                                      ncol = 2,
                                      labels = c("B", "C"),
                                      align = "h",
                                      axis = "tb",
                                      rel_widths = c(1, 1)),
                            nrow = 2, labels = c("A", ""),
                            rel_heights = c(1, 1)
  )
  
  ### weekly cases, annual deaths averted, dalysAvertedPerDose 
  p_grid_final4 = plot_grid(p_cases_monthly_byState,
                            plot_grid(p_deathsAverted_annual_uptake,
                                      p_dalysAvertedPerDose_byRisk,
                                      ncol = 2,
                                      labels = c("B", "C"),
                                      align = "h",
                                      axis = "tb",
                                      rel_widths = c(1, 1)),
                            nrow = 2, labels = c("A", ""),
                            rel_heights = c(1, 1)
  )
  
  ### weekly cases by (non-)endemic, annual deaths averted, dalysAvertedPerDose 
  p_grid_final5 = plot_grid(p_cases_weekly_byEndemic,
                            plot_grid(p_deathsAverted_annual_uptake,
                                      p_dalysAvertedPerDose_byRisk,
                                      ncol = 2,
                                      labels = c("B", "C"),
                                      align = "h",
                                      axis = "tb",
                                      rel_widths = c(1, 1)),
                            nrow = 2, labels = c("A", ""),
                            rel_heights = c(1, 1)
  )
  
  if(save_plots){
    ggsave(p_grid_final1, file = paste0("p_grid_final1_", which_approach, ".png"),
           width = 26, height = 22, units = "cm")
    ggsave(p_grid_final2, file = paste0("p_grid_final2_", which_approach, ".png"),
           width = 26, height = 22, units = "cm")
    ggsave(p_grid_final3, file = paste0("p_grid_final3_", which_approach, ".png"),
           width = 26, height = 22, units = "cm")
    ggsave(p_grid_final4, file = paste0("p_grid_final4_", which_approach, ".png"),
           width = 26, height = 22, units = "cm")
    ggsave(p_grid_final5, file = paste0("p_grid_final5_", which_approach, ".png"),
           width = 26, height = 22, units = "cm")
  }
}



