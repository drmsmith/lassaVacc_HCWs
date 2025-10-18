####################################################
### GENERATE ANNUAL CASES AND POPULATION AT RISK ###
####################################################

### in particular:
### popSize
### outbreakSize

filepath = this.path::here()
setwd(filepath)
source("housekeeping.R")

save_plots = F

###########################################################################
### APPROACH 3: HCWs IN ENDEMIC STATES:                                 ###
### USE CASE DATA FROM SYSTEMATIC REVIEW AND TAKE ANNUAL CASES DIRECTLY ###
###########################################################################

####################################
### (1) HCW CASES FROM NCDC DATA ###
####################################

### take weekly reported cases from NCDC situation reports

### STATES WEEKLY ###
df_hcw_cases_ncdc_state = read_excel("NCDC data totals and HCW by week and annual.xlsx", 
                               sheet = 2, skip = 2 )%>%
  filter(!is.na(Year))%>%
  filter(!Year %in% c(2025))%>%
  pivot_longer(-c(Year, `Epi Week`, Date, total, Cumulativ), names_to = "State", values_to = "Cases")%>%
  mutate(Cases = ifelse(is.na(Cases), 0, Cases))%>%
  mutate(endemicState = ifelse(State %in% vec_endemicStates, "endemic", "non-endemic"))%>%
  mutate(Date_clean = as.Date(paste(Year, `Epi Week`, 1, sep="-"), "%Y-%U-%u"))

test = df_hcw_cases_ncdc_state[which(df_hcw_cases_ncdc_state$Cases>0),]
vec_states_with_cases = unique(test$State)

vec_nonEndemicStates = setdiff(vec_states_with_cases, vec_endemicStates)

### STATES WEEKLY, GROUP NON-ENDEMIC
df_hcw_cases_ncdc_state = df_hcw_cases_ncdc_state%>%
  mutate(State2 = ifelse(State %in% vec_nonEndemicStates, 
                         "non-endemic states", State))%>%
  group_by(Year, `Epi Week`, Date_clean, State2, endemicState)%>%
  summarise(Cases = sum(Cases))%>%
  mutate(State2 = factor(State2,
                         levels = c(vec_endemicStates, "non-endemic states")))
  
### NATIONAL WEEKLY ###
df_hcw_cases_ncdc_national = df_hcw_cases_ncdc_state%>%
  group_by(Year, `Epi Week`, Date_clean)%>%
  summarise(Cases = sum(Cases))


### STATE ANNUAL ###
df_hcw_cases_ncdc_state_annual = read_excel("NCDC data totals and HCW by week and annual.xlsx", 
           sheet = 1, skip = 2 )%>%
  filter(Year != 2017,
         Outcome == "HCW")%>%
  pivot_longer(-c(Year, `Outcome`), names_to = "State", values_to = "Cases")%>%
  mutate(Cases = ifelse(is.na(Cases), 0, Cases))%>%
  mutate(endemicState = ifelse(State %in% vec_endemicStates, "endemic", "non-endemic"))%>%
  filter(State != "Total")%>%
  mutate(State2 = ifelse(State %in% vec_endemicStates, 
                         State, "non-endemic states"))%>%
  mutate(Year = factor(Year),
         State2 = factor(State2,
                         levels = c(vec_endemicStates, "non-endemic states")))

### NATIONAL ANNUAL ###
df_hcw_cases_ncdc_national_annual = df_hcw_cases_ncdc_state_annual%>%
  group_by(Year)%>%
  summarise(Cases = sum(Cases))%>%
  mutate(Year = factor(Year))

### NATIONAL ANNUAL ENDEMIC ###
df_hcw_cases_ncdc_national_annual_endemic = df_hcw_cases_ncdc_state_annual%>%
  group_by(Year, endemicState)%>%
  summarise(Cases = sum(Cases))%>%
  filter(endemicState == "endemic")

#########################################################################################
### TABLE OF ANNUAL CASES IN INDIVIDUAL ENDEMIC STATES AND GROUPED NON-ENDEMIC STATES ###
#########################################################################################

t_cases_state_annual = df_hcw_cases_ncdc_state_annual%>%
  group_by(Year, State2)%>%
  summarise(Cases = sum(Cases))%>%
  pivot_wider(names_from = Year, values_from = Cases)

if(save_plots){
  write.csv(t_cases_state_annual, file = "t_cases_state_annual.csv")
}

#######################
### PLOT EPI CURVES ###
#######################

### Plot weekly cases by state
p_cases_weekly_byState = ggplot(df_hcw_cases_ncdc_state%>%filter(State2 %in% c(vec_endemicStates, "non-endemic states")), 
       aes(x = Date_clean, fill = State2, y = Cases))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.1)+
  theme_classic()+
  xlab("Date")+
  ylab("Weekly Lassa fever cases reported\nin Nigerian healthcare workers")+
  scale_fill_manual("State", values = cols_states)+
  guides(fill = guide_legend(ncol = 4))+
  theme(legend.position = c(0.25,0.75))

### Plot weekly cases by "endemic" vs. "non-endemic" state
p_cases_weekly_byEndemic = ggplot(df_hcw_cases_ncdc_state%>%
         group_by(Year, `Epi Week`, Date_clean, endemicState)%>%
         summarise(Cases = sum(Cases)), 
       aes(x = Date_clean, fill = endemicState, y = Cases))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.1)+
  theme_classic()+
  xlab("Date")+
  ylab("Weekly Lassa fever cases reported\nin Nigerian healthcare workers")+
  scale_fill_manual("State", values = c("#1c5b5a", "#56ebd3"))+
  theme(legend.position = c(0.25,0.75))

### Plot monthly cases by state

p_cases_monthly_byState = df_hcw_cases_ncdc_state%>%filter(State2 %in% c(vec_endemicStates, "non-endemic states"))%>%
  mutate(Date_clean_monthly = format(as.Date(Date_clean), "%Y-%m"))%>%
  group_by(Year, State2, endemicState, Date_clean_monthly)%>%
  summarise(Cases = sum(Cases))%>%
  mutate(Date_clean_monthly = as.yearmon(Date_clean_monthly))%>%
  ggplot(., aes(x = Date_clean_monthly, fill = State2, y = Cases))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.1)+
  theme_classic()+
  xlab("Date")+
  ylab("Monthly Lassa fever cases reported\nin Nigerian healthcare workers")+
  scale_fill_manual("State", values = cols_states)+
  guides(fill = guide_legend(ncol = 4))+
  theme(legend.position = c(0.25,0.85))
                                   


###########################################
### (2) BOOTSTRAP MEAN OF OUTBREAK SIZE ###
###########################################

vec_outbreakSize = c(df_hcw_cases_ncdc_national_annual_endemic$Cases)

set.seed(20250228)  # For reproducibility
vec_outbreakSize_boot_sim = replicate(10000, mean(sample(vec_outbreakSize, replace = T)))

### combine hospital size data
df_outbreakSize_data_sim = data.frame(data = "Annual\nLassa fever\ncases", outbreakSize = vec_outbreakSize)%>%
  bind_rows(., data.frame(data = "Simulated", outbreakSize = vec_outbreakSize_boot_sim))

### Plot outbreak size data
p_outbreaksize_fit = df_outbreakSize_data_sim%>%
  #filter(data != "Simulated")%>%
  ggplot(., aes(x = data, y = outbreakSize, shape = data, fill = data))+
  geom_hline(yintercept = 0, colour = "grey")+
  #geom_boxplot(alpha = 0.5, width = 0.5)+
  geom_violin(alpha = 0.3, colour = NA, adjust = 3)+
  geom_dotplot(dotsize = 0.5, binaxis = "y", stackdir = "center")+
  #geom_point(size = 3, position = position_jitter(width = 0.3))+
  theme_classic()+
  scale_shape_manual("Data source", values = c(21, NA))+
  scale_fill_manual("Data source", values = c("red", "blue"))+
  ylab("Outbreak size")+
  xlab("")+ 
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.9,
               colour = "black")+
  theme(legend.position = "none")
p_outbreaksize_fit


#############=################
### (3) POPULATION AT RISK ###
##############################

vec_popSize = sum(df_population_states$N)*vec_workforce

###############################################
### (4) FINAL PARAMETER SET FOR MODEL INPUT ###
###############################################

### Update parameters to include random draws of outbreak size and hospital size
df_params_sim = df_params

set.seed(20240228)
### outbreak sizes from distribution of boostrapped mean annual HCW cases in endemic states
df_params_sim$outbreakSize = sample(vec_outbreakSize_boot_sim, 500)

### hospital sizes from gamma distribution fit to reported hospitals from Femi and Sylvanus
df_params_sim$popSize = vec_popSize
