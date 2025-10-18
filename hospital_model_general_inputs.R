###########################
### PRELIM GENERAL DATA ###
###########################


filepath = this.path::here()
setwd(filepath)
source("housekeeping.R")

### Load in and run data analysis for data common to several of the proposed approaches
save_plots = F

#############################################################
### LOAD MONTE CARLO PARAMETERS FROM LASSA EXTENSION WORK ###
#############################################################

df_params = loadRData("params_montecarlo.Rdata")

######################################
### LOAD LASSA IN HCWS REVIEW DATA ###
######################################

df_hcw_cases_litReview = read_excel("review_data_extraction.xlsx", 
                                     sheet = 1)

##########################################
### LASSA FEVER HOSPITAL STAFF NUMBERS ###
##########################################

### Hospital 1
df_femi = data.frame(hospital = 1,
                     ward = c("Lassa",
                              "Renal",
                              "Medical",
                              "A&E",
                              "Surgical",
                              "ICU",
                              "Orthopaedic",
                              "Plastic",
                              "Paed",
                              "Neonatal",
                              "CHER",
                              "Psych",
                              "OBGYN",
                              "Labour",
                              "Theatre",
                              "Opthalm",
                              "ENT",
                              "Laboratory"),
                     N_HCW = c(40,
                               20,
                               40,
                               40,
                               60,
                               20,
                               40,
                               20,
                               30,
                               20,
                               20,
                               20,
                               40,
                               20,
                               30,
                               15,
                               15,
                               150))


### Hospital 2
df_sylvanus = data.frame(hospital = 2,
                         ward = c("Lassa",
                                  "A&E",
                                  "Theatre",
                                  "ENT",
                                  "Labour",
                                  "Paed_emerg",
                                  "Paed",
                                  "Medical_male",
                                  "Medical_female",
                                  "Newborn",
                                  "Obstetric",
                                  "Psych",
                                  "Orthopaedic",
                                  "Plastic",
                                  "Lab_viral",
                                  "Lab_haem",
                                  "Lab_chem",
                                  "Lab_micro",
                                  "Histopathology",
                                  "Ophthalm",
                                  "Doctors_other"
                         ),
                         N_HCW = c(32,
                                   38,
                                   34,
                                   18,
                                   32,
                                   28,
                                   28,
                                   32,
                                   30,
                                   24,
                                   28,
                                   30,
                                   30,
                                   24,
                                   32,
                                   28,
                                   24,
                                   24,
                                   20,
                                   19,
                                   250))


### hospital sizes
vec_hospital_sizes = c(sum(df_femi$N_HCW), sum(df_sylvanus$N_HCW)) 
vec_hospital_sizes_sim = runif(n_draws_stoch, 500, 1000)

### PLOT WARD STRUCTURES ###

p_hospital1 = df_femi%>%
  ggplot(., aes(x = ward, y = N_HCW))+
  geom_bar(stat = "identity", colour = "black", fill = "grey")+
  ggtitle("Hospital 1")+
  theme_classic()+
  xlab("")+ylab("Number of HCWs")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

p_hospital2 = df_sylvanus%>%
  filter(ward != "Doctors_other")%>%
  ggplot(., aes(x = ward, y = N_HCW))+
  geom_bar(stat = "identity", colour = "black", fill = "grey")+
  ggtitle("Hospital 2")+
  theme_classic()+
  xlab("")+ylab("Number of HCWs")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

p_hospitals = plot_grid(p_hospital1, p_hospital2, nrow = 2, align = "hv", axis = "tblr")
p_hospitals

if(save_plots){
  ggsave(p_hospitals, file = "p_hospitals.png", width = 12, height = 12, units = "cm")  
  
}

### combine hospital wards for hospital total HCW numbers
df_hospitals = df_femi%>%
  group_by(hospital)%>%
  summarise(N_HCW = sum(N_HCW))%>%
  bind_rows(., df_sylvanus%>%
              group_by(hospital)%>%
              summarise(N_HCW = sum(N_HCW)))


########################################
### HCW POPULATION IN ENDEMIC STATES ###
########################################

#### 2025 estimates of population size in each state

df_population_states = loadRData("df_population.Rdata")%>%
  filter(Year == 2025, Country == "Nigeria", Sex %in% c("Female", "Male"))%>%
  group_by(Country, GID_0, Region, GID_1)%>%
  summarise(N = sum(UN_scaled_subnational))

sum(df_population_states$N)

### DRAWS OF WORKFORCE

# Min estimate: GBD 2019 estimate, 17.9 HCWs/10,000
# https://www.thelancet.com/action/showPdf?pii=S0140-6736%2822%2900532-3
workforce_min = 17.9/10000

# Mid estimate: skilled health professionals in Nigeria estimated at 18.25/10,000
# https://apps.who.int/gho/data/view.main.HWF10v
workforce_mid = 18.25/10000

# Max estimate: 2022 National Health Workforce Accounts Database, 20.33/10,000
# https://apps.who.int/nhwaportal/
workforce_max = 20.33/10000

set.seed(20250228)  # For reproducibility
vec_workforce = runif(n_draws_stoch, workforce_min, workforce_max)

############################
### LIFE EXPECTANCY DATA ###
############################

### Load Nigeria life expectancy data for 2025
df_un_le_nigeria_2025 = read.csv("df_un_le_nigeria_2025.csv")%>%
  dplyr::select(-X)%>%
  pivot_wider(id_cols = c(Country, GID_0, Year, Age), names_from = Sex, values_from = LifeExpectancy)%>%
  mutate(LifeExpectancy = (`M`+`F`)/2)%>%
  dplyr::select(-c(`M`,`F`))


##########################
### (3) AGES OF DEATHS ###
##########################

### From Dr. Okogbenin
vec_ages_of_deaths = c(30, 36, 38, 42, 48)

fit_ages_gamma = fitdistr(vec_ages_of_deaths, "Gamma")

set.seed(20250228)  # For reproducibility
vec_ages_gamma_sim = rgamma(n_draws_stoch, fit_ages_gamma$estimate[1], fit_ages_gamma$estimate[2])

### combine age data
df_ages_data_sim = data.frame(data = "NCDC", ages = vec_ages_of_deaths)%>%
  bind_rows(., data.frame(data = "Simulated", ages = vec_ages_gamma_sim))

### summarise age data
df_ages_data_sim_summary = df_ages_data_sim%>%
  group_by(data)%>%
  summarise(mean=mean(ages),
            median = median(ages))

### Plot age data
p_ages_deaths_fit = df_ages_data_sim%>%
  ggplot(., aes(x = data, y = ages, shape = data, fill = data))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_violin(alpha = 0.3, colour = NA)+
  geom_point(size = 3)+
  theme_classic()+
  scale_shape_manual("Data source", values = c(21, NA))+
  scale_fill_manual("Data source", values = c("red", "blue"))+
  ylab("HCW age upon Lassa fever death")+
  xlab("")+ 
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.9,
               colour = "black")+
  theme(legend.position = "none")+
  ylim(0, NA)
p_ages_deaths_fit

if(save_plots){
  ggsave(p_ages_deaths_fit, file = "p_ages_deaths_fit.png", width = 8, height = 10, units = "cm")  
  
}


##########################
### CASE FATALITY RATE ###
##########################

####################
### PREPARE DATA ###
####################

### UPDATE: conduct meta-analysis on reported HCW cases and deaths from systematic review
df_hcw_cases_litReview_included = df_hcw_cases_litReview%>%
  # remove studies with no HCW deaths
  filter(`# of HCW deaths` != "na")%>%
  # remove study 22, which does not report # of HCW cases
  filter(Number != 22)%>%
  # remove study 25, which does not clearly report # of HCW deaths
  filter(Number != 25)%>%
  # remove study 379, which describes the same outbreak as study 378
  filter(Number != 379)%>%
  # remove study 133, which describes cases in Nigeria 2018 also covered in study 244
  filter(Number != 133)%>%
  # remove study 184, which does not report specific number of deaths
  filter(Number != 184)%>%
  # remove study 348, which describes the same outbreak as study 364
  filter(Number != 348)%>%
  # remove study 169, which covers same period as outbreak in study 186 
  filter(Number != 169)%>%
  # remove studies 22 and 114 and 469, which are covered in national surveillance data in study 244
  filter(Number != 22, Number != 114, Number != 469)

### CFR1: include all studies passing inclusion
df_hcw_cases_litReview_CFR1 = df_hcw_cases_litReview_included%>%
  mutate(author_year = paste(`First author`, `Year of pub`),
         event = `# of HCW deaths`,
         n = `# of HCW cases (total)`)%>%
  dplyr::select(author_year, event, n)%>%
  mutate(event = as.numeric(event),
         n = as.numeric(n))

### CFR2: include only studies after 2000
df_hcw_cases_litReview_CFR2 = df_hcw_cases_litReview_included%>%
  # limit to outbreaks since 2010 (corresponds to publish date >2010)
  filter(`Year of pub` > 2000)%>%
  mutate(author_year = paste(`First author`, `Year of pub`),
         event = `# of HCW deaths`,
         n = `# of HCW cases (total)`)%>%
  dplyr::select(author_year, event, n)%>%
  mutate(event = as.numeric(event),
         n = as.numeric(n))


### CFR3: include only studies in Nigeria
df_hcw_cases_litReview_CFR3 = df_hcw_cases_litReview_included%>%
  # limit to outbreaks since 2010 (corresponds to publish date >2010)
  filter(`Outbreak country` == "Nigeria")%>%
  mutate(author_year = paste(`First author`, `Year of pub`),
         event = `# of HCW deaths`,
         n = `# of HCW cases (total)`)%>%
  dplyr::select(author_year, event, n)%>%
  mutate(event = as.numeric(event),
         n = as.numeric(n))


####################################
### MEAT-ANALYSIS 1: ALL STUDIES ###
####################################


meta_CFR <- metaprop(
  event = event,    # Number of events
  n = n,             # Sample sizes
  data = df_hcw_cases_litReview_CFR1,       # Dataset
  sm = "PLOGIT",        # Freeman-Tukey transformation
  method = "Inverse",   # Random-effects model
  studlab = author_year    # Study labels
)

summary_meta_CFR = summary(meta_CFR)

if(save_plots){
  png(file = "p_CFR_MA_allStudies.png",
      width = 800, height = 300, units = "px")
  forest(meta_CFR, xlab = "Proportion", leftlabs = c("Study", "Events", "Total"),
         random = T, common = F)
  dev.off()
}

### Extract pooled proportion and 95% CI
logit_pooled_prop <- meta_CFR$TE.random
logit_ci_lower <- meta_CFR$lower.random
logit_ci_upper <- meta_CFR$upper.random

### Back-transform from logit scale to proportion scale using the inverse logit function
p_cfr_total_mean <- 1 / (1 + exp(-logit_pooled_prop))
p_cfr_total_l <- 1 / (1 + exp(-logit_ci_lower))
p_cfr_total_u <- 1 / (1 + exp(-logit_ci_upper))


### METHOD OF MOMENTS
p_cfr_total_std_dev <- (p_cfr_total_u - p_cfr_total_l) / (2 * 1.96)
p_cfr_total_variance_mom <- p_cfr_total_std_dev^2


# Method of moments estimates for alpha and beta
p_cfr_total_alpha_mom <- p_cfr_total_mean * ((p_cfr_total_mean * (1 - p_cfr_total_mean) / p_cfr_total_variance_mom) - 1)
p_cfr_total_beta_mom <- (1 - p_cfr_total_mean) * ((p_cfr_total_mean * (1 - p_cfr_total_mean) / p_cfr_total_variance_mom) - 1)

# Draw samples from the Beta distribution
set.seed(20250228)  # For reproducibility
parvec_p_cfr_total_beta <- rbeta(n_draws_stoch, shape1 = p_cfr_total_alpha_mom, shape2 = p_cfr_total_beta_mom)


# Check quantiles to see how well they align
mean(parvec_p_cfr_total_beta)
quantile(parvec_p_cfr_total_beta, probs=c(0.025, 0.5, 0.975))




####################################
### MEAT-ANALYSIS 2: ALL STUDIES ###
####################################


###############################################
### ASSIGN FINAL CFR TO PARAMETER DATAFRAME ###
###############################################

df_params$CFR_HCW = parvec_p_cfr_total_beta


####################
### AGE AT DEATH ###
####################

### average age of HCW fatality
set.seed(20250228)  # For reproducibility
vec_ages_gamma_sample = rgamma(n_draws_stoch, fit_ages_gamma$estimate[1], fit_ages_gamma$estimate[2])
df_params$Age = round(vec_ages_gamma_sample)



########################################################
### (8) ENTER DATA FROM PREVIOUS WORK FOR COMPARISON ###
########################################################

### VACCINE IMPACT FROM RISK TARGETED WORK ###
vec_levels_strategy = c("children","wcba","adults","elderly","all")
vec_labels_strategy = c("Children\n(2-14)", "WCBA\n(15-49)", "Adolescents and adults\n(15-49)", "Older adults\n(50+)", "All\n(2+)")

#############
### DALYS ###
#############

df_vacc_RiskTargeted_dalys_VE50 = data.frame(outcome = "dalysAvertedPerDose",
                                             strategy = c("children","wcba","adults","elderly","all"),
                                             mean = c(94.3, 205, 184, 136, 146)/100,
                                             lower = c(42.1, 113, 105, 81.9, 83.9)/100,
                                             upper = c(189, 308, 275, 204, 233)/100,
                                             VE = 0.5)
df_vacc_RiskTargeted_dalys_VE70 = data.frame(outcome = "dalysAvertedPerDose",
                                             strategy = c("children","wcba","adults","elderly","all"),
                                             mean = c(132, 288, 258, 191, 205)/100,
                                             lower = c(58.9, 158, 148, 115, 117)/100,
                                             upper = c(264, 431, 385, 286, 326)/100,
                                             VE = 0.7)
df_vacc_RiskTargeted_dalys_VE90 = data.frame(outcome = "dalysAvertedPerDose",
                                             strategy = c("children","wcba","adults","elderly","all"),
                                             mean = c(170, 370, 331, 245, 263)/100,
                                             lower = c(75.7, 203, 190, 147, 151)/100,
                                             upper = c(340, 555, 496, 367, 419)/100,
                                             VE = 0.9)
df_vacc_RiskTargeted_dalys = bind_rows(df_vacc_RiskTargeted_dalys_VE50,
                                       df_vacc_RiskTargeted_dalys_VE70,
                                       df_vacc_RiskTargeted_dalys_VE90)%>%
  mutate(context = "Community-targeted vaccination")%>%
  mutate(label = paste0(round(signif(mean,3),2),"\n(",round(signif(lower,3),2)," - ",round(signif(upper,3),2),")"))

#############
### CASES ###
#############

### NB: these are saved as "hospitalisations" in the previous work but track to "cases" here
### as all cases in present work are hospitalisations

df_vacc_RiskTargeted_cases_VE50 = data.frame(outcome = "casesAvertedPerDose",
                                             strategy = c("children","wcba","adults","elderly","all"),
                                             mean = c(16.3, 35.2, 39.4, 36.7, 30.7)/100,
                                             lower = c(10.9, 24.0, 26.3, 24.8, 20.7)/100,
                                             upper = c(22.1, 47.8, 53.2, 50.4, 41.4)/100,
                                             VE = 0.5)
df_vacc_RiskTargeted_cases_VE70 = data.frame(outcome = "casesAvertedPerDose",
                                             strategy = c("children","wcba","adults","elderly","all"),
                                             mean = c(22.8, 49.3, 55, 51.3, 43)/100,
                                             lower = c(15.2, 33.6, 36.9, 34.8, 29)/100,
                                             upper = c(30.9, 66.9, 74.5, 70.6, 58)/100,
                                             VE = 0.7)
df_vacc_RiskTargeted_cases_VE90 = data.frame(outcome = "casesAvertedPerDose",
                                             strategy = c("children","wcba","adults","elderly","all"),
                                             mean = c(29.4, 63.3, 70.7, 66, 55.2)/100,
                                             lower = c(19.5, 43.2, 47.4, 44.7, 37.2)/100,
                                             upper = c(39.8, 86, 95.8, 90.8, 74.5)/100,
                                             VE = 0.9)
df_vacc_RiskTargeted_cases = bind_rows(df_vacc_RiskTargeted_cases_VE50,
                                       df_vacc_RiskTargeted_cases_VE70,
                                       df_vacc_RiskTargeted_cases_VE90)%>%
  mutate(context = "Community-targeted vaccination")%>%
  mutate(label = paste0(round(signif(mean,3),2),"\n(",round(signif(lower,3),2)," - ",round(signif(upper,3),2),")"))

##############
### DEATHS ###
##############

df_vacc_RiskTargeted_deaths_VE50 = data.frame(outcome = "deathsAvertedPerDose",
                                             strategy = c("children","wcba","adults","elderly","all"),
                                             mean = c(1.06, 4.8, 4.72, 9.48, 3.93)/100,
                                             lower = c(0.6, 2.89, 2.94, 5.69, 2.52)/100,
                                             upper = c(1.69, 7.01, 6.96, 14.1, 5.84)/100,
                                             VE = 0.5)
df_vacc_RiskTargeted_deaths_VE70 = data.frame(outcome = "deathsAvertedPerDose",
                                             strategy = c("children","wcba","adults","elderly","all"),
                                             mean = c(1.49, 6.72, 6.61, 13.3, 5.51)/100,
                                             lower = c(0.84, 4.05, 4.12, 7.96, 3.52)/100,
                                             upper = c(2.37, 9.81, 9.75, 19.8, 8.17)/100,
                                             VE = 0.7)
df_vacc_RiskTargeted_deaths_VE90 = data.frame(outcome = "deathsAvertedPerDose",
                                             strategy = c("children","wcba","adults","elderly","all"),
                                             mean = c(1.91, 8.64, 8.5, 17.1, 7.08)/100,
                                             lower = c(1.08, 5.21, 5.3, 10.2, 4.53)/100,
                                             upper = c(3.05, 12.6, 12.5, 25.5, 10.5)/100,
                                             VE = 0.9)
df_vacc_RiskTargeted_deaths = bind_rows(df_vacc_RiskTargeted_deaths_VE50,
                                       df_vacc_RiskTargeted_deaths_VE70,
                                       df_vacc_RiskTargeted_deaths_VE90)%>%
  mutate(context = "Community-targeted vaccination")%>%
  mutate(label = paste0(round(signif(mean,3),2),"\n(",round(signif(lower,3),2)," - ",round(signif(upper,3),2),")"))
