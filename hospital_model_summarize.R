###########################################
### SUMMARIZE RESULTS OF HOSPITAL MODEL ###
###########################################

filepath = this.path::here()
setwd(filepath)
source("housekeeping.R")

save_plots = F

#######################
### LOAD INPUT DATA ###
#######################

source("hospital_model_general_inputs.R")

#########################
### LOAD OUTCOME DATA ###
#########################

df_vaccOutcomes_annual = loadRData("outputs/df_vaccOutcomes_annual.Rdata")
df_vaccOutcomes_10yr = loadRData("outputs/df_vaccOutcomes_10yr.Rdata")
df_vaccOutcomes_5yr = loadRData("outputs/df_vaccOutcomes_5yr.Rdata")
df_vaccOutcomes_2yr = loadRData("outputs/df_vaccOutcomes_2yr.Rdata")

############################
### PREPARE DATA FORMATS ###
############################

##################
### CUMULATIVE ###
##################

### Cumulative results over time ###
cols_meta = names(df_vaccOutcomes_annual)[1:14]
cols_outcomes = setdiff(names(df_vaccOutcomes_annual), cols_meta)

df_vaccOutcomes_annual_cumul = df_vaccOutcomes_annual%>%
  arrange(Country, GID_0, cov_vacc, turnover, VE, n_draw, Year)%>%
  group_by(Country, GID_0, cov_vacc, turnover, VE, n_draw) %>%
  mutate(across(all_of(cols_outcomes), cumsum)) %>%
  ungroup()

##################
### EFFICIENCY ###
##################

### Results efficiency (outcomes at 5 or 10 years divided by number of doses) ###
df_vaccOutcomes_10yr_perDose = df_vaccOutcomes_10yr%>%
  left_join(., df_doses%>%
              ungroup()%>%
              dplyr::select(cov_vacc, n_draw, doses)%>%
              mutate(cov_vacc = factor(cov_vacc)),
            by = c("cov_vacc", "n_draw"))%>%
  dplyr::select(-c(Country, GID_0, Year, N_hcw_age_sex, N_infections, LifeExpectancyNeonate, N_hcw_yr1, N_hcw_vacc, prop_hcw_infected, N_infections_vacc))%>%
  pivot_longer(-c(n_draw, cov_vacc, turnover, VE, doses),
               names_to = "measure", values_to = "value")%>%
  mutate(efficiency = value/doses*1000)

df_vaccOutcomes_5yr_perDose = df_vaccOutcomes_5yr%>%
  left_join(., df_doses%>%
              ungroup()%>%
              dplyr::select(cov_vacc, n_draw, doses)%>%
              mutate(cov_vacc = factor(cov_vacc)),
            by = c("cov_vacc", "n_draw"))%>%
  dplyr::select(-c(Country, GID_0, Year, N_hcw_age_sex, N_infections, LifeExpectancyNeonate, N_hcw_yr1, N_hcw_vacc, prop_hcw_infected, N_infections_vacc))%>%
  pivot_longer(-c(n_draw, cov_vacc, turnover, VE, doses),
               names_to = "measure", values_to = "value")%>%
  mutate(efficiency = value/doses*1000)

df_vaccOutcomes_2yr_perDose = df_vaccOutcomes_2yr%>%
  left_join(., df_doses%>%
              ungroup()%>%
              dplyr::select(cov_vacc, n_draw, doses)%>%
              mutate(cov_vacc = factor(cov_vacc)),
            by = c("cov_vacc", "n_draw"))%>%
  dplyr::select(-c(Country, GID_0, Year, N_hcw_age_sex, N_infections, LifeExpectancyNeonate, N_hcw_yr1, N_hcw_vacc, prop_hcw_infected, N_infections_vacc))%>%
  pivot_longer(-c(n_draw, cov_vacc, turnover, VE, doses),
               names_to = "measure", values_to = "value")%>%
  mutate(efficiency = value/doses*1000)


######################
### SUMMARIZE DATA ###
######################

### Annual ###
df_vaccOutcomes_annual_summarised = df_vaccOutcomes_annual%>%
  dplyr::select(-c(Country, GID_0, N_hcw_age_sex, N_infections, LifeExpectancyNeonate, N_hcw_yr1, N_hcw_vacc, prop_hcw_infected, N_infections_vacc))%>%
  pivot_longer(-c(Year, cov_vacc, turnover, VE, n_draw),
               names_to = "outcome", values_to = "value")%>%
  ungroup()%>%
  group_by(Year, cov_vacc, turnover, VE, outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975))

### Annual cumulative ###
df_vaccOutcomes_annual_cumul_summarised = df_vaccOutcomes_annual_cumul%>%
  dplyr::select(-c(Country, GID_0, N_hcw_age_sex, N_infections, LifeExpectancyNeonate, N_hcw_yr1, N_hcw_vacc, prop_hcw_infected, N_infections_vacc))%>%
  pivot_longer(-c(Year, cov_vacc, turnover, VE, n_draw),
               names_to = "outcome", values_to = "value")%>%
  ungroup()%>%
  group_by(Year, cov_vacc, turnover, VE, outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975))

### 10 year totals ###
df_vaccOutcomes_10yr_summarised = df_vaccOutcomes_10yr%>%
  dplyr::select(-c(Country, GID_0, N_hcw_age_sex, N_infections, LifeExpectancyNeonate, N_hcw_yr1, N_hcw_vacc, prop_hcw_infected, N_infections_vacc))%>%
  pivot_longer(-c(Year, cov_vacc, turnover, VE, n_draw),
               names_to = "outcome", values_to = "value")%>%
  ungroup()%>%
  group_by(Year, cov_vacc, turnover, VE, outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975))

### 10 year efficiency ###
df_vaccOutcomes_10yr_perDose_summarised = df_vaccOutcomes_10yr_perDose%>%
  group_by(cov_vacc, turnover, VE, measure)%>%
  summarise(mean = mean(efficiency),
            median = median(efficiency),
            lower = quantile(efficiency, 0.025),
            upper = quantile(efficiency, 0.975))

### 5 year totals ###
df_vaccOutcomes_5yr_summarised = df_vaccOutcomes_5yr%>%
  dplyr::select(-c(Country, GID_0, N_hcw_age_sex, N_infections, LifeExpectancyNeonate, N_hcw_yr1, N_hcw_vacc, prop_hcw_infected, N_infections_vacc))%>%
  pivot_longer(-c(Year, cov_vacc, turnover, VE, n_draw),
               names_to = "outcome", values_to = "value")%>%
  ungroup()%>%
  group_by(Year, cov_vacc, turnover, VE, outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975))

### 5 year efficiency ###
df_vaccOutcomes_5yr_perDose_summarised = df_vaccOutcomes_5yr_perDose%>%
  group_by(cov_vacc, turnover, VE, measure)%>%
  summarise(mean = mean(efficiency),
            median = median(efficiency),
            lower = quantile(efficiency, 0.025),
            upper = quantile(efficiency, 0.975))

### 2 year totals ###
df_vaccOutcomes_2yr_summarised = df_vaccOutcomes_2yr%>%
  dplyr::select(-c(Country, GID_0, N_hcw_age_sex, N_infections, LifeExpectancyNeonate, N_hcw_yr1, N_hcw_vacc, prop_hcw_infected, N_infections_vacc))%>%
  pivot_longer(-c(Year, cov_vacc, turnover, VE, n_draw),
               names_to = "outcome", values_to = "value")%>%
  ungroup()%>%
  group_by(Year, cov_vacc, turnover, VE, outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975))

### 2 year efficiency ###
df_vaccOutcomes_2yr_perDose_summarised = df_vaccOutcomes_2yr_perDose%>%
  group_by(cov_vacc, turnover, VE, measure)%>%
  summarise(mean = mean(efficiency),
            median = median(efficiency),
            lower = quantile(efficiency, 0.025),
            upper = quantile(efficiency, 0.975))


#########################################################
### MANUAL MODEL CHECK WITH ALTERNATIVE DISTRIBUTIONS ###
#########################################################

if(manual_testing){
  VE_i = 0.7
  cov_vacc_i = 0.6
  turnover_i = 0.15
  
  df_model_cov_turnover = df_model_lognormal%>%
    filter(cov_vacc == cov_vacc_i, turnover == turnover_i)
  
  if(nrow(df_model_cov_turnover) < 1){stop("data not filtering correctly")}
  
  ### MALE
  df_vacc_randOutbreak_male_lognormal = df_model_cov_turnover%>%
    filter(Sex == "Male")%>%
    # mild cases expected without vaccine
    mutate(mild = N_infections * prob_symptoms)%>%
    # cases expected without vaccine:
    mutate(cases = N_infections * p_severe_hcw)%>%
    # deaths expected without vaccine:
    mutate(deaths = cases * CFR_HCW)%>%
    # snhl expected without vaccine (main analysis: all individuals; sensitivity analysis: only hospitalised)
    mutate(snhl_sens1 = (cases - deaths)*prob_snhl)%>%
    mutate(snhl = mild * prob_snhl + (cases - deaths)*prob_snhl)%>%
    # foetal loss and neonatal death expected without vaccine
    mutate(fl = 0,
           nnd = 0)%>%
    # years of life lost without vaccine
    mutate(YLL_death = deaths * LifeExpectancy,
           YLL_nnd = nnd * LifeExpectancyNeonate,
           YLL_fl = fl * LifeExpectancyNeonate)%>%
    # add prob vaccinated and vaccine efficacy parameters from loop
    mutate(VE = VE_i)%>%
    # number of mild cases averted in vaccinated people:
    mutate(mildAverted = N_infections_vacc * prob_symptoms * VE)%>%
    # number of cases averted in vaccinated people:
    mutate(casesAverted = N_infections_vacc * p_severe_hcw * VE)%>%
    # deaths averted among cases averted:
    mutate(deathsAverted = casesAverted * CFR_HCW)%>%
    # snhl averted among survivors averted:
    mutate(snhlAverted_sens1 = (casesAverted - deathsAverted) * prob_snhl)%>%
    mutate(snhlAverted = mildAverted*prob_snhl + (casesAverted - deathsAverted)*prob_snhl)%>%
    # foetal loss and neonatal death averted
    mutate(flAverted = 0,
           nndAverted = 0)%>%
    # years of life lost without vaccine
    mutate(YLL_deathAverted = deathsAverted * LifeExpectancy,
           YLL_nndAverted = nndAverted * LifeExpectancyNeonate,
           YLL_flAverted = flAverted * LifeExpectancyNeonate)%>%
    # mild cases remaining after vaccinating:
    mutate(mildRemaining = mild - mildAverted)%>%
    # cases remaining after vaccinating:
    mutate(casesRemaining = cases - casesAverted)%>%
    # deaths remaining after vaccinating:
    mutate(deathsRemaining = casesRemaining * CFR_HCW)%>%
    # snhl remaining after vaccinating:
    mutate(snhlRemaining_sens1 = (casesRemaining - deathsRemaining) * prob_snhl)%>%
    mutate(snhlRemaining = mildRemaining * prob_snhl + (casesRemaining - deathsRemaining) * prob_snhl)%>%
    # foetal loss and neonatal death remaining after vaccinating:
    mutate(flRemaining = 0,
           nndRemaining = 0)%>%
    # YLL remaining after vaccinating
    mutate(YLL_deathRemaining = deathsRemaining * LifeExpectancy,
           YLL_nndRemaining = nndRemaining * LifeExpectancyNeonate,
           YLL_flRemaining = flRemaining * LifeExpectancyNeonate)
  
  ### FEMALE NOT PREG
  df_vacc_randOutbreak_female_notpreg_lognormal = df_model_cov_turnover%>%
    filter(Sex == "Female_NotPreg")%>%
    # mild cases expected without vaccine
    mutate(mild = N_infections * prob_symptoms)%>%
    # cases expected without vaccine:
    mutate(cases = N_infections * p_severe_hcw)%>%
    # deaths expected without vaccine:
    mutate(deaths = cases * CFR_HCW)%>%
    # snhl expected without vaccine (main analysis: all individuals; sensitivity analysis: only hospitalised)
    mutate(snhl_sens1 = (cases - deaths)*prob_snhl)%>%
    mutate(snhl = mild * prob_snhl + (cases - deaths)*prob_snhl)%>%
    # foetal loss and neonatal death expected without vaccine
    mutate(fl = 0,
           nnd = 0)%>%
    # years of life lost without vaccine
    mutate(YLL_death = deaths * LifeExpectancy,
           YLL_nnd = nnd * LifeExpectancyNeonate,
           YLL_fl = fl * LifeExpectancyNeonate)%>%
    # add prob vaccinated and vaccine efficacy parameters from loop
    mutate(VE = VE_i)%>%
    # number of mild cases averted in vaccinated people:
    mutate(mildAverted = N_infections_vacc * prob_symptoms * VE)%>%
    # number of cases averted in vaccinated people:
    mutate(casesAverted = N_infections_vacc * p_severe_hcw * VE)%>%
    # deaths averted among cases averted:
    mutate(deathsAverted = casesAverted * CFR_HCW)%>%
    # snhl averted among survivors averted:
    mutate(snhlAverted_sens1 = (casesAverted - deathsAverted) * prob_snhl)%>%
    mutate(snhlAverted = mildAverted*prob_snhl + (casesAverted - deathsAverted)*prob_snhl)%>%
    # foetal loss and neonatal death averted
    mutate(flAverted = 0,
           nndAverted = 0)%>%
    # years of life lost without vaccine
    mutate(YLL_deathAverted = deathsAverted * LifeExpectancy,
           YLL_nndAverted = nndAverted * LifeExpectancyNeonate,
           YLL_flAverted = flAverted * LifeExpectancyNeonate)%>%
    # mild cases remaining after vaccinating:
    mutate(mildRemaining = mild - mildAverted)%>%
    # cases remaining after vaccinating:
    mutate(casesRemaining = cases - casesAverted)%>%
    # deaths remaining after vaccinating:
    mutate(deathsRemaining = casesRemaining * CFR_HCW)%>%
    # snhl remaining after vaccinating:
    mutate(snhlRemaining_sens1 = (casesRemaining - deathsRemaining) * prob_snhl)%>%
    mutate(snhlRemaining = mildRemaining * prob_snhl + (casesRemaining - deathsRemaining) * prob_snhl)%>%
    # foetal loss and neonatal death remaining after vaccinating:
    mutate(flRemaining = 0,
           nndRemaining = 0)%>%
    # YLL remaining after vaccinating
    mutate(YLL_deathRemaining = deathsRemaining * LifeExpectancy,
           YLL_nndRemaining = nndRemaining * LifeExpectancyNeonate,
           YLL_flRemaining = flRemaining * LifeExpectancyNeonate)
  
  ### FEMALE PREG
  df_vacc_randOutbreak_female_preg_lognormal = df_model_cov_turnover%>%
    filter(Sex == "Female_Preg")%>%
    # mild cases expected without vaccine
    mutate(mild = N_infections * prob_symptoms)%>%
    # cases expected without vaccine:
    mutate(cases = N_infections * p_severe_hcw)%>%
    # deaths expected without vaccine:
    mutate(deaths = cases * CFR_HCW)%>%
    # snhl expected without vaccine (main analysis: all individuals; sensitivity analysis: only hospitalised)
    mutate(snhl_sens1 = (cases - deaths)*prob_snhl)%>%
    mutate(snhl = mild * prob_snhl + (cases - deaths)*prob_snhl)%>%
    # foetal loss and neonatal death expected without vaccine
    mutate(fl = prob_fl_lassa * cases,
           nnd = prob_nnd_lassa * cases)%>%
    # years of life lost without vaccine
    mutate(YLL_death = deaths * LifeExpectancy,
           YLL_nnd = nnd * LifeExpectancyNeonate,
           YLL_fl = fl * LifeExpectancyNeonate)%>%
    # add prob vaccinated and vaccine efficacy parameters from loop
    mutate(VE = VE_i)%>%
    # number of mild cases averted in vaccinated people:
    mutate(mildAverted = N_infections_vacc * prob_symptoms * VE)%>%
    # number of cases averted in vaccinated people:
    mutate(casesAverted = N_infections_vacc * p_severe_hcw * VE)%>%
    # deaths averted among cases averted:
    mutate(deathsAverted = casesAverted * CFR_HCW)%>%
    # snhl averted among survivors averted:
    mutate(snhlAverted_sens1 = (casesAverted - deathsAverted) * prob_snhl)%>%
    mutate(snhlAverted = mildAverted*prob_snhl + (casesAverted - deathsAverted)*prob_snhl)%>%
    # foetal loss and neonatal death averted
    mutate(flAverted = prob_fl_lassa * casesAverted,
           nndAverted = prob_nnd_lassa * casesAverted)%>%
    # years of life lost without vaccine
    mutate(YLL_deathAverted = deathsAverted * LifeExpectancy,
           YLL_nndAverted = nndAverted * LifeExpectancyNeonate,
           YLL_flAverted = flAverted * LifeExpectancyNeonate)%>%
    # mild cases remaining after vaccinating:
    mutate(mildRemaining = mild - mildAverted)%>%
    # cases remaining after vaccinating:
    mutate(casesRemaining = cases - casesAverted)%>%
    # deaths remaining after vaccinating:
    mutate(deathsRemaining = casesRemaining * CFR_HCW)%>%
    # snhl remaining after vaccinating:
    mutate(snhlRemaining_sens1 = (casesRemaining - deathsRemaining) * prob_snhl)%>%
    mutate(snhlRemaining = mildRemaining * prob_snhl + (casesRemaining - deathsRemaining) * prob_snhl)%>%
    # foetal loss and neonatal death remaining after vaccinating:
    mutate(flRemaining = 0,
           nndRemaining = 0)%>%
    # YLL remaining after vaccinating
    mutate(YLL_deathRemaining = deathsRemaining * LifeExpectancy,
           YLL_nndRemaining = nndRemaining * LifeExpectancyNeonate,
           YLL_flRemaining = flRemaining * LifeExpectancyNeonate)
  
  ### COMBINE SEXES AND CALCULATE DALYs
  df_vacc_randOutbreak_age_sex_lognormal = bind_rows(df_vacc_randOutbreak_male_lognormal,
                                           df_vacc_randOutbreak_female_notpreg_lognormal,
                                           df_vacc_randOutbreak_female_preg_lognormal)%>%
    mutate(dalys_mild = mild*disutility_fever*dur_fever/365,
           dalys_acute_survived = (cases - deaths)*((dur_ill_prehosp+dur_hosp_survived)/365)*disutility_hospital,
           dalys_acute_died = deaths*((dur_ill_prehosp+dur_hosp_died)/365)*disutility_hospital,
           dalys_snhl = snhl*dur_snhl*disutility_snhl,
           dalys_snhl_sens1 = snhl_sens1*dur_snhl*disutility_snhl,
           dalys_death = YLL_death,
           dalys_nnd = YLL_nnd,
           dalys_fl = YLL_fl)%>%
    mutate(dalys = dalys_mild + dalys_acute_survived + dalys_acute_died + dalys_snhl + dalys_death + dalys_nnd,
           # sensitivity analysis 1: assume post-acute hearing loss only in hospitalised survivors
           dalys_sens1 = dalys_mild + dalys_acute_survived + dalys_acute_died + dalys_snhl_sens1 + dalys_death + dalys_nnd,
           # sensitivity analysis 2: include foetal loss DALYs
           dalys_sens2 = dalys_mild + dalys_acute_survived + dalys_acute_died + dalys_snhl + dalys_death + dalys_nnd + dalys_fl)%>%
    mutate(dalysAverted_mild = mildAverted*disutility_fever*dur_fever/365,
           dalysAverted_acute_survived = (casesAverted - deathsAverted)*((dur_ill_prehosp+dur_hosp_survived)/365)*disutility_hospital,
           dalysAverted_acute_died = deathsAverted*((dur_ill_prehosp+dur_hosp_died)/365)*disutility_hospital,
           dalysAverted_snhl = snhlAverted*dur_snhl*disutility_snhl,
           dalysAverted_snhl_sens1 = snhlAverted_sens1*dur_snhl*disutility_snhl,
           dalysAverted_death = YLL_deathAverted,
           dalysAverted_nnd = YLL_nndAverted,
           dalysAverted_fl = YLL_flAverted)%>%
    mutate(dalysAverted = dalysAverted_mild + dalysAverted_acute_survived + dalysAverted_acute_died + dalysAverted_snhl + dalysAverted_death + dalysAverted_nnd,
           dalysAverted_sens1 = dalysAverted_mild + dalysAverted_acute_survived + dalysAverted_acute_died + dalysAverted_snhl_sens1 + dalysAverted_death + dalysAverted_nnd,
           dalysAverted_sens2 = dalysAverted_mild + dalysAverted_acute_survived + dalysAverted_acute_died + dalysAverted_snhl + dalysAverted_death + dalysAverted_nnd + dalysAverted_fl)%>%
    mutate(dalysRemaining = dalys - dalysAverted)%>%
    mutate(dalysRemaining_sens1 = dalys_sens1 - dalysAverted_sens1,
           dalysRemaining_sens2 = dalys_sens2 - dalysAverted_sens2)
  
  ### ADD UP ANNUAL OUTCOMES ACROSS ALL AGE/SEX GROUPS
  df_vacc_randOutbreak_annual_lognormal = df_vacc_randOutbreak_age_sex_lognormal%>%
    dplyr::select(-c("prop_immune"))%>%
    dplyr::select(-contains("prob_"))%>%
    dplyr::select(-contains("dur_"))%>%
    dplyr::select(-contains("disutility_"))%>%
    dplyr::select(-contains("DALYperpatient_"))%>%
    dplyr::select(-contains("p_severe"))%>%
    dplyr::select(-c("CFR_HCW", "outbreakSize", "LifeExpectancy"))%>%
    group_by(Country, GID_0, Year, n_draw, cov_vacc, turnover, VE)%>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")
  
  ### ADD UP TOTAL OUTCOMES (10 YEARS) ACROSS ALL AGE/SEX GROUPS
  df_vacc_randOutbreak_10yr_lognormal = df_vacc_randOutbreak_age_sex_lognormal%>%
    dplyr::select(-c("prop_immune"))%>%
    dplyr::select(-contains("prob_"))%>%
    dplyr::select(-contains("dur_"))%>%
    dplyr::select(-contains("disutility_"))%>%
    dplyr::select(-contains("DALYperpatient_"))%>%
    dplyr::select(-contains("p_severe"))%>%
    dplyr::select(-c("CFR_HCW", "outbreakSize", "LifeExpectancy"))%>%
    group_by(Country, GID_0, n_draw, cov_vacc, turnover, VE)%>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")
  
  ### ADD UP TOTAL OUTCOMES (5 YEARS) ACROSS ALL AGE/SEX GROUPS
  df_vacc_randOutbreak_5yr_lognormal = df_vacc_randOutbreak_age_sex_lognormal%>%
    filter(Year %in% 2025:2029)%>%
    dplyr::select(-contains("prob_"))%>%
    dplyr::select(-c("prop_immune"))%>%
    dplyr::select(-contains("dur_"))%>%
    dplyr::select(-contains("disutility_"))%>%
    dplyr::select(-contains("DALYperpatient_"))%>%
    dplyr::select(-contains("p_severe"))%>%
    dplyr::select(-c("CFR_HCW", "outbreakSize", "LifeExpectancy"))%>%
    group_by(Country, GID_0, n_draw, cov_vacc, turnover, VE)%>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")
  
  
  ### SUMMARISE OUTPUTS
  ### Combine rows for final vaccine outcome results
  df_vaccOutcomes_annual_lognormal = df_vacc_randOutbreak_annual_lognormal
  df_vaccOutcomes_10yr_lognormal = df_vacc_randOutbreak_10yr_lognormal
  df_vaccOutcomes_5yr_lognormal = df_vacc_randOutbreak_5yr_lognormal
  
  # Burden
  df_vaccOutcomes_10yr_summarised_lognormal = df_vaccOutcomes_10yr_lognormal%>%
    dplyr::select(-c(Country, GID_0, N_hcw_age_sex, N_infections, LifeExpectancyNeonate, N_hcw_yr1, N_hcw_vacc, prop_hcw_infected, N_infections_vacc))%>%
    pivot_longer(-c(Year, cov_vacc, turnover, VE, n_draw),
                 names_to = "outcome", values_to = "value")%>%
    ungroup()%>%
    group_by(Year, cov_vacc, turnover, VE, outcome)%>%
    summarise(mean = mean(value),
              median = median(value),
              lower = quantile(value, 0.025),
              upper = quantile(value, 0.975))
  
  
  ### COMPARE OUTCOMES
  testA_normal = df_vaccOutcomes_10yr_summarised%>%
    filter(VE == 0.7,
           cov_vacc == 0.6,
           turnover == 0.15)
  
  testB_lognormal = df_vaccOutcomes_10yr_summarised_lognormal%>%
    filter(VE == 0.7,
           cov_vacc == 0.6,
           turnover == 0.15)
}

# df_vaccOutcomes_annual_summarised%>%filter(outcome == "cases")%>%View()

