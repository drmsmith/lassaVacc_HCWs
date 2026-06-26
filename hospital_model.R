######################
### HOSPITAL MODEL ###
######################

filepath = this.path::here()
setwd(filepath)
source("housekeeping.R")

save_plots = F


##############################
### LOAD IN GENERAL INPUTS ###
##############################

source("hospital_model_general_inputs.R")


################################################
### LOOP THROUGH VACCINE IMPACTS ON OUTCOMES ###
################################################

manual_testing = F

# extract included turnover rates and vaccine coverage rates
vec_turnover = unique(df_model$turnover)
vec_cov_vacc = unique(df_model$cov_vacc)

# define VE values
vec_VE = c(0.5, 0.7, 0.9)

# save final results for annual, at 2 years, 5 years and at 10 years
list_vacc_randOutbreak_annual = list()
list_vacc_randOutbreak_2yr = list()
list_vacc_randOutbreak_5yr = list()
list_vacc_randOutbreak_10yr = list()

# initialize loop
list_counter = 0
set.seed(20240228)

for(turnover_i in vec_turnover){
  for(cov_vacc_i in vec_cov_vacc){
    for(VE_i in vec_VE){
      
      list_counter = list_counter + 1
      
      print(paste0("looping scenario with turnover = ", turnover_i, " and vaccine with coverage = ", cov_vacc_i, " and VE = ", VE_i))
      
      ########################
      ### OUTCOMES AVERTED ###
      ########################
      ### Number of cases happening in vaccinated people given different vaccine uptake proportions
      ### use the parameter dataframe as a scaffold for outcomes
      
      ### RAND OUTBREAK ###
      # separate by sex, because pinch_point from case_when when applying neonatal risk specifically to pregnant women
      
      df_model_cov_turnover = df_model%>%
        filter(cov_vacc == cov_vacc_i, turnover == turnover_i)
      
      if(nrow(df_model_cov_turnover) < 1){stop("data not filtering correctly")}
      
      ### MALE
      df_vacc_randOutbreak_male = df_model_cov_turnover%>%
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
      df_vacc_randOutbreak_female_notpreg = df_model_cov_turnover%>%
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
      df_vacc_randOutbreak_female_preg = df_model_cov_turnover%>%
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
      df_vacc_randOutbreak_age_sex = bind_rows(df_vacc_randOutbreak_male,
                                               df_vacc_randOutbreak_female_notpreg,
                                               df_vacc_randOutbreak_female_preg)%>%
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
      df_vacc_randOutbreak_annual = df_vacc_randOutbreak_age_sex%>%
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
      df_vacc_randOutbreak_10yr = df_vacc_randOutbreak_age_sex%>%
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
      df_vacc_randOutbreak_5yr = df_vacc_randOutbreak_age_sex%>%
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
      
      ### ADD UP TOTAL OUTCOMES (2 YEARS) ACROSS ALL AGE/SEX GROUPS
      df_vacc_randOutbreak_2yr = df_vacc_randOutbreak_age_sex%>%
        filter(Year %in% 2025:2026)%>%
        dplyr::select(-contains("prob_"))%>%
        dplyr::select(-c("prop_immune"))%>%
        dplyr::select(-contains("dur_"))%>%
        dplyr::select(-contains("disutility_"))%>%
        dplyr::select(-contains("DALYperpatient_"))%>%
        dplyr::select(-contains("p_severe"))%>%
        dplyr::select(-c("CFR_HCW", "outbreakSize", "LifeExpectancy"))%>%
        group_by(Country, GID_0, n_draw, cov_vacc, turnover, VE)%>%
        summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")
      
      ### Conduct some tests to check results are as expected
      if(manual_testing){
        ### Tests for each outcome
        df_vacc_randOutbreak%>%
          group_by(Year, n_draw)%>%
          summarise(mild = sum(mild), mildAverted = sum(mildAverted), mildRemaining = sum(mildRemaining))%>%
          mutate(testRemaining = mild - mildAverted)
        
        df_vacc_randOutbreak%>%
          group_by(Year, n_draw)%>%
          summarise(cases = sum(cases), casesAverted = sum(casesAverted), casesRemaining = sum(casesRemaining))%>%
          mutate(testRemaining = cases - casesAverted)
        
        df_vacc_randOutbreak%>%
          group_by(Year, n_draw)%>%
          summarise(deaths = sum(deaths), deathsAverted = sum(deathsAverted), deathsRemaining = sum(deathsRemaining))%>%
          mutate(testRemaining = deaths - deathsAverted)
        
        df_vacc_randOutbreak%>%
          group_by(Year, n_draw)%>%
          summarise(snhl = sum(snhl), snhlAverted = sum(snhlAverted), snhlRemaining = sum(snhlRemaining))%>%
          mutate(testRemaining = snhl - snhlAverted)
        
        df_vacc_randOutbreak%>%
          group_by(Year, n_draw)%>%
          summarise(snhl_sens1 = sum(snhl_sens1), snhlAverted_sens1 = sum(snhlAverted_sens1), snhlRemaining_sens1 = sum(snhlRemaining_sens1))%>%
          mutate(testRemaining = snhl_sens1 - snhlAverted_sens1)
        
        df_vacc_randOutbreak%>%
          group_by(Year, n_draw)%>%
          summarise(fl = sum(fl), flAverted = sum(flAverted), flRemaining = sum(flRemaining))%>%
          mutate(testRemaining = fl - flAverted)
        
        df_vacc_randOutbreak%>%
          group_by(Year, n_draw)%>%
          summarise(nnd = sum(nnd), nndAverted = sum(nndAverted), nndRemaining = sum(nndRemaining))%>%
          mutate(testRemaining = nnd - nndAverted)
        
        df_vacc_randOutbreak%>%
          group_by(Year, n_draw)%>%
          summarise(YLL = sum(YLL), YLLAverted = sum(YLLAverted), YLLRemaining = sum(YLLRemaining))%>%
          mutate(testRemaining = YLL - YLLAverted)
      }
      
      
      ### check that sum of averted + remaining = total
      # RAND
      stopifnot(sum(df_vacc_randOutbreak_annual$cases) - sum(df_vacc_randOutbreak_annual$casesAverted) - sum(df_vacc_randOutbreak_annual$casesRemaining) < 0.0001)
      stopifnot(sum(df_vacc_randOutbreak_annual$deaths) - sum(df_vacc_randOutbreak_annual$deathsAverted) - sum(df_vacc_randOutbreak_annual$deathsRemaining) < 0.0001)
      stopifnot(sum(df_vacc_randOutbreak_annual$snhl) - sum(df_vacc_randOutbreak_annual$snhlAverted) - sum(df_vacc_randOutbreak_annual$snhlRemaining) < 0.0001)
      stopifnot(sum(df_vacc_randOutbreak_annual$dalys) - sum(df_vacc_randOutbreak_annual$dalysAverted) - sum(df_vacc_randOutbreak_annual$dalysRemaining) < 0.0001)
      stopifnot(sum(df_vacc_randOutbreak_annual$dalys_sens1) - sum(df_vacc_randOutbreak_annual$dalysAverted_sens1) - sum(df_vacc_randOutbreak_annual$dalysRemaining_sens1) < 0.0001)
      stopifnot(sum(df_vacc_randOutbreak_annual$dalys_sens2) - sum(df_vacc_randOutbreak_annual$dalysAverted_sens2) - sum(df_vacc_randOutbreak_annual$dalysRemaining_sens2) < 0.0001)
      
      ### SAVE RESULTS IN LIST
      list_vacc_randOutbreak_annual[[list_counter]] = df_vacc_randOutbreak_annual
      list_vacc_randOutbreak_10yr[[list_counter]] = df_vacc_randOutbreak_10yr
      list_vacc_randOutbreak_5yr[[list_counter]] = df_vacc_randOutbreak_5yr
      list_vacc_randOutbreak_2yr[[list_counter]] = df_vacc_randOutbreak_2yr
    }
  }
}


### Combine rows for final vaccine outcome results
df_vaccOutcomes_annual = do.call(rbind, list_vacc_randOutbreak_annual)
df_vaccOutcomes_10yr = do.call(rbind, list_vacc_randOutbreak_10yr)
df_vaccOutcomes_5yr = do.call(rbind, list_vacc_randOutbreak_5yr)
df_vaccOutcomes_2yr = do.call(rbind, list_vacc_randOutbreak_2yr)


### Save outputs
save(df_vaccOutcomes_annual, file = "df_vaccOutcomes_annual.Rdata")
save(df_vaccOutcomes_10yr, file = "df_vaccOutcomes_10yr.Rdata")
save(df_vaccOutcomes_5yr, file = "df_vaccOutcomes_5yr.Rdata")
save(df_vaccOutcomes_2yr, file = "df_vaccOutcomes_2yr.Rdata")
