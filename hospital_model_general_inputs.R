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

df_params = loadRData("data/params_montecarlo.Rdata")

######################################
### LOAD LASSA IN HCWS REVIEW DATA ###
######################################

df_hcw_cases_litReview = read_excel("data/review_data_extraction.xlsx", 
                                     sheet = 1)


########################################
### HCW POPULATION IN ENDEMIC STATES ###
########################################

#### 2025 estimates of population size in each state
df_population_states = loadRData("data/df_population.Rdata")%>%
  filter(Year == 2025, Country == "Nigeria", Sex %in% c("Female", "Male"))%>%
  group_by(Country, GID_0, Region, GID_1)%>%
  summarise(N = sum(UN_scaled_subnational))

sum(df_population_states$N)


#### 2025-2034 (10 year) estimates of population size in each state, stratified by age, sex and preg/non-preg
df_population_states_age_sex = loadRData("data/df_population.Rdata")%>%
  filter(Year %in% c(2025:2034), Country == "Nigeria", Sex != c("Female"))%>%
  group_by(Country, GID_0, Region, GID_1)

sum(df_population_states_age_sex$UN_scaled_subnational)


### DRAWS OF WORKFORCE

# Min estimate: GBD 2019 estimate, 17.9 HCWs/10,000 population
# https://www.thelancet.com/action/showPdf?pii=S0140-6736%2822%2900532-3
workforce_min = 17.9/10000

# Mid estimate: skilled health professionals in Nigeria estimated at 18.25/10,000 population
# https://apps.who.int/gho/data/view.main.HWF10v
workforce_mid = 18.25/10000

# Max estimate: 2022 National Health Workforce Accounts Database, 20.33/10,000 population
# https://apps.who.int/nhwaportal/
workforce_max = 20.33/10000

set.seed(20250228)  # For reproducibility
vec_workforce = runif(n_draws_stoch, workforce_min, workforce_max)


################################
### AGE/SEX OF HCW WORKFORCE ###
################################

df_nigeria_hcw_dist_age_sex = read.csv("data/HCWs_nigeria_age_sex.csv", sep = ";")

############################
### LIFE EXPECTANCY DATA ###
############################

# ### Load Nigeria life expectancy data for 2025
# df_un_le_nigeria_2025 = read.csv("data/df_un_le_nigeria_2025.csv")%>%
#   dplyr::select(-X)%>%
#   pivot_wider(id_cols = c(Country, GID_0, Year, Age), names_from = Sex, values_from = LifeExpectancy)%>%
#   mutate(LifeExpectancy = (`M`+`F`)/2)%>%
#   dplyr::select(-c(`M`,`F`))


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
#df_params$Age = round(vec_ages_gamma_sample)



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



########################################
### APPROACH: HCWs IN ENDEMIC STATES ###
### USE CASE DATA FROM NCDC DIRECTLY ###
########################################

####################################
### (1) HCW CASES FROM NCDC DATA ###
####################################

### take weekly reported cases from NCDC situation reports

### STATES WEEKLY ###
df_hcw_cases_ncdc_state = read_excel("data/NCDC data totals and HCW by week and annual.xlsx", 
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
df_hcw_cases_ncdc_state_annual = read_excel("data/NCDC data totals and HCW by week and annual.xlsx", 
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





###########################################
### (2) BOOTSTRAP MEAN OF OUTBREAK SIZE ###
###########################################

vec_outbreakSize = c(df_hcw_cases_ncdc_national_annual_endemic$Cases)

set.seed(20250228)  # For reproducibility
vec_outbreakSize_boot_sim = replicate(10000, mean(sample(vec_outbreakSize, replace = T)))

### combine hospital size data
df_outbreakSize_data_sim = data.frame(data = "Annual\nLassa fever\ncases", outbreakSize = vec_outbreakSize)%>%
  bind_rows(., data.frame(data = "Simulated", outbreakSize = vec_outbreakSize_boot_sim))



##############################
### (3) POPULATION AT RISK ###
##############################

### ORIGINAL ANALYSIS ###
### Total workforce, no age or sex stratification
vec_popSize = sum(df_population_states$N)*vec_workforce


### UPDATED ANALYSIS ###
### Workforce stratified by year, age and sex

if(length(vec_workforce) != n_draws_stoch){warning("misalignment in stoch draw number")}

### total population size by year
df_popSize_annual = df_population_states_age_sex%>%
  group_by(Country, GID_0, Region, GID_1, Year)%>%
  summarise(N = sum(UN_scaled_subnational))

### multiply workforce populations by population total for each year (assuming stable proportion through time) to get total HCWs
df_popSize_annual_hcws = df_popSize_annual%>%
  cross_join(., data.frame(hcw_prop = vec_workforce,
                           n_draw = 1:n_draws_stoch))%>%
  mutate(N_hcws = hcw_prop * N)%>%
  dplyr::select(-c(N, hcw_prop))

### Nigeria-wide HCW workforce
df_popSize_annual_hcws_nigeria = df_popSize_annual_hcws%>%
  group_by(Country, GID_0, Year, n_draw)%>%
  summarise(N_hcws = sum(N_hcws))
  

### Distribute HCWs by age and sex following https://link.springer.com/article/10.1186/s12960-022-00706-3
## ASSUMPTION: same pregnancy proportion in female HCWs as in general population

### in HCW age-sex data, spread HCWs out to all age-number combinations and determine
### the proportion of all HCWs in each age-sex group
df_states_hcw_age_sex_props_allage = df_nigeria_hcw_dist_age_sex%>%
  rowwise() %>%
  mutate(age = list(age_min:age_max)) %>%
  unnest(age) %>%
  mutate(
    n_years = age_max - age_min + 1,
    hcw = hcw / n_years
  )%>%
  mutate(hcw_prop = hcw/sum(hcw))%>%
  dplyr::select(-c(n_years, hcw))

### Extract proportion pregnant for each female age group, limited to included HCW ages (15 to 65)
df_prop_preg = df_population_states_age_sex%>%
  filter(Year == 2025, Sex %in% c("Male", "Preg_Female"))%>%
  dplyr::select(GID_1, Age, Sex, PregProp)%>%
  filter(Age %in% 15:65)%>%
  mutate(Sex = case_when(Sex == "Preg_Female" ~ "Female",
                         T ~ Sex))

# full join total workforce numbers for each year-state by this age-sex distribution,
df_states_n_hcw_age_sex_all = cross_join(df_popSize_annual_hcws, df_states_hcw_age_sex_props_allage)%>%
  rename(Age = age,
         Sex = sex)%>%
  mutate(N_hcw_age_sex = N_hcws * hcw_prop)%>%
  left_join(., df_prop_preg, by = c("Country", "GID_0", "Region", "GID_1", "Age", "Sex"))

# split pregnant female and non-pregnant female and account for person-time pregnant
# So 40/52 weeks pregnant
df_states_n_hcw_age_sex_female_Preg = df_states_n_hcw_age_sex_all%>%
  filter(Sex == "Female")%>%
  mutate(N_hcw_age_sex = N_hcw_age_sex * PregProp*40/52)%>%
  mutate(Sex = "Female_Preg")

df_states_n_hcw_age_sex_female_NotPreg = df_states_n_hcw_age_sex_all%>%
  filter(Sex == "Female")%>%
  mutate(N_hcw_age_sex = N_hcw_age_sex * (1-(PregProp*40/52)))%>%
  mutate(Sex = "Female_NotPreg")

#########################################################
### FINAL POPULATION AT RISK BY AGE AND SEX OVER TIME ###
#########################################################

df_states_n_hcw_age_sex = df_states_n_hcw_age_sex_all%>%
  filter(Sex == "Male")%>%
  bind_rows(., df_states_n_hcw_age_sex_female_Preg)%>%
  bind_rows(., df_states_n_hcw_age_sex_female_NotPreg)%>%
  dplyr::select(-c(PregProp, hcw_prop, N_hcws))

### Test that the number of HCWs, after stratification, matches the overall proportion times population size in each state
test1 = df_states_n_hcw_age_sex%>%group_by(Country, Region, GID_1, Year, n_draw)%>%
  summarise(N_hcw = sum(N_hcw_age_sex))
test2 = df_popSize_annual_hcws

# test1%>%
#   group_by(Year, n_draw)%>%
#   summarise(N_hcw = sum(N_hcw))


################################
### MILD INFECTION INCIDENCE ###
################################

### load zoonosis incidence by age (note that it is identical by sex so can exclude sex) 
df_zoonosis_incidence = loadRData("data/df_incidence_2019_allAges.Rdata")%>%
  filter(n_draw %in% c(1:n_draws_stoch),
         GID_1 %in% vec_endemic_GID_1,
         Sex == "Female")%>%
  dplyr::select(-c(Sex, propAg_Enable, N_infection, UN_scaled_subnational, N_infections_scaled, Year))

### summarise incidence by age group
df_zoonosis_incidence_summarise = df_zoonosis_incidence%>%
  filter(Country == "Nigeria", Age > 14)%>%
  group_by(Country, GID_0, Region, GID_1, ag)%>%
  summarise(mean = mean(incidence),
            lower = quantile(incidence, 0.025),
            upper = quantile(incidence, 0.975))%>%
  mutate(label = paste0(round(mean*100, 3), " (", round(lower*100, 3), ", ", round(upper*100, 3), ")"))%>%
  dplyr::select(-c(mean, lower, upper))%>%
  pivot_wider(names_from = ag, values_from = label)


### apply zoonosis incidence by age and sex to population projections, including pregnant
df_states_n_hcw_age_sex_infections = df_states_n_hcw_age_sex%>%
  left_join(., df_zoonosis_incidence, by = c("Country", "GID_0", "Region", "GID_1", "n_draw", "Age"))%>%
  mutate(N_infections = incidence * N_hcw_age_sex)
# check how many infections by age and sex
temp = df_states_n_hcw_age_sex_infections%>%
  group_by(Year, age_group, Sex, n_draw)%>%
  summarise(N_infections = sum(N_infections))

temp2 = df_states_n_hcw_age_sex_infections%>%
  group_by(Year, n_draw)%>%
  summarise(N_infections = sum(N_infections))

### summarise for whole country
df_nigeria_n_hcw_age_sex_infections = df_states_n_hcw_age_sex_infections%>%
  group_by(Country, GID_0, Year, n_draw, age_group, ag, Sex, Age)%>%
  summarise(N_hcw_age_sex = sum(N_hcw_age_sex),
            N_infections = sum(N_infections))

### summarise all infections (ignore age and sex)
df_nigeria_n_hcw_infections = df_states_n_hcw_age_sex_infections%>%
  group_by(Country, GID_0, Year, n_draw)%>%
  summarise(N_hcw = sum(N_hcw_age_sex),
            N_infections = sum(N_infections))

### summarise annual HCWs and infections by age group
df_nigeria_n_hcw_age_infections_summarise = df_nigeria_n_hcw_age_sex_infections%>%
  group_by(Year, age_group, n_draw)%>%
  summarise(N_infections = sum(N_infections), N_hcw_age_sex = sum(N_hcw_age_sex))%>%
  group_by(Year, age_group)%>%
  summarise(N_hcw_age_mean = mean(N_hcw_age_sex),
            N_hcw_age_lower = quantile(N_hcw_age_sex, 0.025),
            N_hcw_age_upper = quantile(N_hcw_age_sex, 0.975),
            N_infections_mean = mean(N_infections),
            N_infections_lower = quantile(N_infections, 0.025),
            N_infections_upper = quantile(N_infections, 0.975))

### summarise annual HCWs and infections by age group AND SEX
df_nigeria_n_hcw_age_sex_infections_summarise = df_nigeria_n_hcw_age_sex_infections%>%
  group_by(Year, Sex, age_group, n_draw)%>%
  summarise(N_infections = sum(N_infections), N_hcw_age_sex = sum(N_hcw_age_sex))%>%
  group_by(Year, Sex, age_group)%>%
  summarise(N_hcw_age_sex_mean = mean(N_hcw_age_sex),
            N_hcw_age_sex_lower = quantile(N_hcw_age_sex, 0.025),
            N_hcw_age_sex_upper = quantile(N_hcw_age_sex, 0.975),
            N_infections_mean = mean(N_infections),
            N_infections_lower = quantile(N_infections, 0.025),
            N_infections_upper = quantile(N_infections, 0.975))


###################################################################################
### Add age-specific life expectancy to demographic data for years of life lost ###
###################################################################################

df_life_exp_age_sex_year_prelim = loadRData("Data/df_life_exp_age_sex_year.Rdata")%>%
  # age and year in this df are character, so update
  dplyr::select(-c(Age))%>%
  rename(Age = Age_num)%>%
  mutate(Year = as.numeric(Year))
  
### Specific life expectancies by sex in Nigeria
df_life_exp_age_sex_year_male = df_life_exp_age_sex_year_prelim%>%
  filter(Country == "Nigeria", Age %in% 15:65, Sex == "Male")
df_life_exp_age_sex_year_female_preg = df_life_exp_age_sex_year_prelim%>%
  filter(Country == "Nigeria", Age %in% 15:65, Sex == "Female")%>%
  mutate(Sex = "Female_Preg")
df_life_exp_age_sex_year_female_notpreg = df_life_exp_age_sex_year_prelim%>%
  filter(Country == "Nigeria", Age %in% 15:65, Sex == "Female")%>%
  mutate(Sex = "Female_NotPreg")

### Extract infant life expectancy for neonatal death and foetal loss DALYs
lifeExpInfant = df_life_exp_age_sex_year_prelim%>%
  filter(Country == "Nigeria", Age == 0)%>%
  group_by(Year)%>%
  summarise(LifeExpectancyNeonate = mean(life_exp_at_age_x))

### Combine life expectancies and include neonatal life expectancy as additional parameter
df_life_exp_age_sex_year = bind_rows(df_life_exp_age_sex_year_male,
                                     df_life_exp_age_sex_year_female_preg,
                                     df_life_exp_age_sex_year_female_notpreg)


########################################################################################
### COMBINE LIFE EXPECTANCY WITH VACCINE COVERAGE AND WANING, SIMPLIFY TO AGE GROUPS ###
########################################################################################

### Combine life expectancy with demographic and infection data, and take weighted averages across age groups ###

df_nigeria_n_hcw_age_sex_infections_life = df_nigeria_n_hcw_age_sex_infections%>%
  left_join(., df_life_exp_age_sex_year,
            by = c("Country", "GID_0", "Year", "Age", "Sex"))%>%
  group_by(Country, GID_0, Year, n_draw, age_group, Sex)%>%
  summarise(N_hcw_age_sex_sum = sum(N_hcw_age_sex),
            N_infections_sum = sum(N_infections),
            life_exp_at_age_x = weighted.mean(life_exp_at_age_x, N_infections, na.rm = T))%>%
  rename(N_hcw_age_sex = N_hcw_age_sex_sum,
         N_infections = N_infections_sum,
         LifeExpectancy = life_exp_at_age_x)%>%
  # remove rows with no infections (pregnant women at ages where no pregnancy happens)
  filter(N_infections>0)%>%
  left_join(., lifeExpInfant, by = "Year")


#######################################
### ADD VACCINE COVERAGE AND WANING ###
#######################################

#################
### CAMPAIGNS ###
#################

### 1 year campaign
df_campaign1_cov = data.frame(cov_vacc = seq(0.2, 1, by = 0.2))

#############
### DOSES ###
#############

df_doses = df_nigeria_n_hcw_infections%>%
  cross_join(., df_campaign1_cov)%>%
  mutate(doses = case_when(Year == 2025 ~ N_hcw * cov_vacc * 1.1,
                           T ~ 0))%>%
  filter(Year == 2025)


###################
### PROP IMMUNE ###
###################

vec_turnover = seq(0, 0.25, by = 0.05)

df_turnover = crossing(df_campaign1_cov, turnover = vec_turnover, Year = 2025:2034) 

# Build immunity lookup by year
df_immunity <- df_turnover %>%
  mutate(
    years_since_vax = Year - min(Year),
    prop_immune = cov_vacc * (1 - turnover)^years_since_vax
  )%>%
  dplyr::select(-c('years_since_vax'))



###############################################
### (4) FINAL PARAMETER SET FOR MODEL INPUT ###
###############################################

set.seed(20240228)

### outbreak sizes from distribution of boostrapped mean annual HCW cases in endemic states
df_outbreakSize = data.frame(n_draw = 1:n_draws_stoch,
                             outbreakSize = sample(vec_outbreakSize_boot_sim, n_draws_stoch))

### calculate the prob_severe that reproduce outbreak sizes in each sample in 2025
df_outbreakSize_riskSevere = df_nigeria_n_hcw_infections%>%
  filter(Year == 2025)%>%
  left_join(., df_outbreakSize, by = "n_draw")%>%
  ungroup()%>%
  mutate(p_severe_hcw = outbreakSize / N_infections)%>%
  # manually insert small, large and mean outbreak sizes for the alternative analysis
  mutate(p_severe_hcw_smallOutbreak = min(vec_outbreakSize) / N_infections,
         p_severe_hcw_meanOutbreak = mean(vec_outbreakSize) / N_infections,
         p_severe_hcw_largeOutbreak = max(vec_outbreakSize) / N_infections)%>%
  dplyr::select(-c(Country, GID_0, Year, N_hcw, N_infections))


################
### DF MODEL ###
################

### Vaccinated cohort in first year
df_vaccinees = df_nigeria_n_hcw_age_sex_infections_life%>%
  ungroup()%>%
  dplyr::select(-c("Country", "GID_0"))%>%
  cross_join(., df_campaign1_cov)%>%
  filter(Year == 2025)%>%
  dplyr::select(-c(N_infections, LifeExpectancy, LifeExpectancyNeonate))%>%
  mutate(N_hcw_yr1 = N_hcw_age_sex)%>%
  dplyr::select(-c(N_hcw_age_sex, Year))%>%
  crossing(Year = 2025:2034)

### Combine demographic data with life expectancy data and parameter data in final df_model ###
df_model = df_nigeria_n_hcw_age_sex_infections_life%>%
  left_join(., df_vaccinees, by = c("Year", "age_group", "Sex", "n_draw"))%>%
  left_join(., df_immunity, by = c("Year", "cov_vacc"), relationship = "many-to-many")%>%
  ### calculate in each year and group
  # 1. the proportion of HCWs infected
  # 2. apply this to the proportion of vaccinated HCWs that are infected
  mutate(N_hcw_vacc = N_hcw_yr1 * prop_immune,
         prop_hcw_infected = N_infections / N_hcw_age_sex,
         N_infections_vacc = N_hcw_vacc * prop_hcw_infected)%>%
  left_join(., df_params, by = 'n_draw')%>%
  left_join(., df_outbreakSize_riskSevere, by = "n_draw")%>%
  mutate(cov_vacc = factor(cov_vacc), turnover = factor(turnover))

# ### Simplified without age or sex
# df_model_aggr = df_nigeria_n_hcw_infections%>%
#   left_join(., df_params, by = 'n_draw')%>%
#   left_join(., df_outbreakSize_riskSevere, by = "n_draw")%>%
#   left_join(., df_immunity, by = "Year", relationship = "many-to-many")


#####################
### DOSE FORECAST ###
#####################

### Determine dose requirements to maintain a given coverage level each year, given population growth and attrition

### Combine annual HCW counts with target vaccine coverage and turnover rates
df_hcw_by_year = df_nigeria_n_hcw_infections%>%
  dplyr::select(-c(N_infections))%>%
  left_join(., df_immunity%>%dplyr::select(Year, cov_vacc, turnover),
            by = "Year", relationship = "many-to-many")

vec_years_forecast = 2026:2034

list_dose_forecast = list()
qounter_forecast = 0

for(year_i in vec_years_forecast){
  
  qounter_forecast = qounter_forecast + 1
  
  year_current = year_i
  ### Extract numbers for 2025
  
  year_previous = year_current - 1
  
  # how much attrition occurred among vaccinated HCWs last year
  df_hcw_previous_yr = df_hcw_by_year%>%filter(Year == year_previous)%>%
    mutate(N_vacc = N_hcw * cov_vacc ,
           N_doses = N_vacc * 1.1)%>%
    mutate(N_attrition = N_vacc*turnover)
  
  ### how many HCWs are still vaccinated after accounting for attrition
  df_hcw_previous_yr_carryOver = df_hcw_previous_yr%>%
    ungroup()%>%
    mutate(N_still_vacc = N_vacc - N_attrition)%>%
    mutate(Year = year_current)%>%
    dplyr::select(Year, n_draw, cov_vacc, turnover, N_still_vacc)
  
  ### what is the difference between current coverage target and number still vaccinated
  df_hcw_current_yr = df_hcw_by_year%>%filter(Year == year_current)%>%
    mutate(N_target_vacc = N_hcw * cov_vacc)%>%
    left_join(., df_hcw_previous_yr_carryOver, by = c("Year", "n_draw", "cov_vacc", "turnover"))%>%
    mutate(N_vacc_gap = N_target_vacc - N_still_vacc,
           N_dose_gap = N_vacc_gap * 1.1)
  
  list_dose_forecast[[qounter_forecast]] = df_hcw_current_yr
}


### combine annual forecasts
df_dose_forecast = do.call(rbind, list_dose_forecast)


###################
### DIAGNOSTICS ###
###################

### check that multiplying p_severe_hcw by the total number of HCW infections reproduces the simulated outbreak size
df_model_check = df_model%>%
  filter(Year == 2025, cov_vacc == 0.4, turnover == 0.1)%>%
  mutate(outbreakSizeCheck = N_infections * p_severe_hcw)%>%
  group_by(n_draw)%>%
  summarise(outbreakSizeCheck = sum(outbreakSizeCheck))%>%
  left_join(., df_outbreakSize, by = "n_draw")%>%
  mutate(match = ifelse(abs(outbreakSize - outbreakSizeCheck)<0.0001, T, F))
sum(df_model_check$match)

### check change in outbreakSize over time
p_check_outbreakSize_overTime = df_model%>%
  filter(cov_vacc == 0.4, turnover == 0.1)%>%
  mutate(outbreakSizeCheck = N_infections * p_severe_hcw)%>%
  group_by(Year, n_draw)%>%
  summarise(outbreakSizeCheck = sum(outbreakSizeCheck))%>%
  mutate(Year = factor(Year))%>%
  ggplot(., aes(x = Year, y = outbreakSizeCheck))+
  geom_boxplot()


### evaluate change in vaccinated cohort over time
df_test_vacc_cohort_noTurnover = df_model%>%
  filter(cov_vacc == 0.6, turnover %in% c(0, 0.15))%>%
  group_by(Year, n_draw, cov_vacc, turnover)%>%
  summarise(N_hcw_age_sex  = sum(N_hcw_age_sex),
            N_infections  = sum(N_infections),
            N_hcw_yr1 = sum(N_hcw_yr1),
            N_hcw_vacc = sum(N_hcw_vacc),
            N_infections_vacc = sum(N_infections_vacc))

df_test_vacc_cohort_noTurnover_long = df_test_vacc_cohort_noTurnover%>%
  pivot_longer(-c(Year, n_draw, cov_vacc, turnover),
               names_to = "measure", values_to = "value")%>%
  group_by(Year, cov_vacc, turnover, measure)%>%
  summarise(mean = mean(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975))

