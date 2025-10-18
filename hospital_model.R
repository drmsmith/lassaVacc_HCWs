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

#######################################
### SET WHICH APPROACH IS BEING RUN ###
#######################################

#which_approach = "approach1"
#which_approach = "approach2"
which_approach = "approach3"
#which_approach = "approach4"

print(paste0("RUNNING ", which_approach))

########################################
### RUN THE INPUTS FOR THAT APPROACH ###
########################################

source(paste0("hospital_model_inputs_", which_approach, ".R"))

##########################################################
### (4) DETERMINISTIC MODEL WITH PARAMETER UNCERTAINTY ###
##########################################################

### Define small, mean and large outbreak sizes depending on approach

if(which_approach %in% c("approach1", "approach2")){
  smallOutbreakSize = 1
  meanOutbreakSize = 4.2
  largeOutbreakSize = 15
  vec_labels_outbreakSize = c("Random outbreak", 
                              "Small outbreak (N=1 case)", 
                              "Mean outbreak (N=4.2 cases)", 
                              "Large outbreak (N=15 cases)")
}

if(which_approach %in% c("approach3")){
  smallOutbreakSize = min(vec_outbreakSize)
  meanOutbreakSize = round(mean(vec_outbreakSize), 1)
  largeOutbreakSize = max(vec_outbreakSize)
  vec_labels_outbreakSize = c("Random outbreak year", 
                              paste0("Small outbreak year (N=", smallOutbreakSize," cases)"), 
                              paste0("Mean outbreak year (N=", meanOutbreakSize," cases)"), 
                              paste0("Large outbreak year (N=", largeOutbreakSize," cases)"))
}


####################################################
### (4) LOOP THROUGH VACCINE IMPACTS ON OUTCOMES ###
####################################################

vec_prob_vacc_full = seq(0, 1, by=0.05)
vec_VE_full = seq(0, 1, by = 0.05)

list_vacc_randOutbreakSizes = list()
list_vacc_smallOutbreakSizes = list()
list_vacc_meanOutbreakSizes = list()
list_vacc_largeOutbreakSizes = list()
list_counter = 0

set.seed(20240228)

for(prob_vacc_i in vec_prob_vacc_full){
  
  for(VE_i in vec_VE_full){
    
    list_counter = list_counter + 1
    
    print(paste0("looping through vaccine with coverage = ", prob_vacc_i*100, "% and VE = ", VE_i*100, "%"))
    
    ########################
    ### OUTCOMES AVERTED ###
    ########################
    ### Number of cases happening in vaccinated people given different vaccine uptake proportions
    ### use the parameter dataframe as a scaffold for outcomes
    
    ### RAND OUTBREAK
    df_vacc_randOutbreak = df_params_sim%>%
      # cases expected without vaccine:
      mutate(cases = outbreakSize)%>%
      # deaths expected without vaccine:
      mutate(deaths = cases * CFR_HCW)%>%
      # snhl expected without vaccine
      mutate(snhl = (cases - deaths)*prob_snhl)%>%
      # add prob vaccinated and vaccine efficacy parameters from loop
      mutate(prob_vacc = prob_vacc_i,
             VE = VE_i)%>%
      # number of cases in vaccinated people:
      mutate(casesVacc = cases*prob_vacc)%>%
      # number of cases averted in vaccinated people:
      mutate(casesAverted = casesVacc * VE)%>%
      # deaths averted among cases averted:
      mutate(deathsAverted = casesAverted * CFR_HCW)%>%
      # snhl averted among survivors averted:
      mutate(snhlAverted = (casesAverted - deathsAverted) * prob_snhl)%>%
      # cases remaining after vaccinating:
      mutate(casesRemaining = cases - casesAverted)%>%
      # deaths remaining after vaccinating:
      mutate(deathsRemaining = casesRemaining * CFR_HCW)%>%
      # snhl remaining after vaccinating:
      mutate(snhlRemaining = (casesRemaining - deathsRemaining) * prob_snhl)%>%
      # number of vaccine doses
      mutate(doses = prob_vacc * popSize * 1.1)%>%
      # outcomes averted per dose of vaccine
      mutate(casesAvertedPerDose = casesAverted/doses,
             snhlAvertedPerDose = snhlAverted/doses,
             deathsAvertedPerDose = deathsAverted/doses)
    
    ### SMALL OUTBREAK
    df_vacc_smallOutbreak = df_params_sim%>%
      mutate(cases = smallOutbreakSize)%>%
      mutate(deaths = cases * CFR_HCW)%>%
      mutate(snhl = (cases - deaths)*prob_snhl)%>%
      mutate(prob_vacc = prob_vacc_i,
             VE = VE_i)%>%
      mutate(casesVacc = cases*prob_vacc)%>%
      mutate(casesAverted = casesVacc * VE)%>%
      mutate(deathsAverted = casesAverted * CFR_HCW)%>%
      mutate(snhlAverted = (casesAverted - deathsAverted) * prob_snhl)%>%
      mutate(casesRemaining = cases - casesAverted)%>%
      mutate(deathsRemaining = casesRemaining * CFR_HCW)%>%
      mutate(snhlRemaining = (casesRemaining - deathsRemaining) * prob_snhl)%>%
      mutate(doses = prob_vacc * popSize * 1.1)%>%
      mutate(casesAvertedPerDose = casesAverted/doses,
             snhlAvertedPerDose = snhlAverted/doses,
             deathsAvertedPerDose = deathsAverted/doses)
    
    ### MEAN OUTBREAK
    df_vacc_meanOutbreak = df_params_sim%>%
      mutate(cases = meanOutbreakSize)%>%
      mutate(deaths = cases * CFR_HCW)%>%
      mutate(snhl = (cases - deaths)*prob_snhl)%>%
      mutate(prob_vacc = prob_vacc_i,
             VE = VE_i)%>%
      mutate(casesVacc = cases*prob_vacc)%>%
      mutate(casesAverted = casesVacc * VE)%>%
      mutate(deathsAverted = casesAverted * CFR_HCW)%>%
      mutate(snhlAverted = (casesAverted - deathsAverted) * prob_snhl)%>%
      mutate(casesRemaining = cases - casesAverted)%>%
      mutate(deathsRemaining = casesRemaining * CFR_HCW)%>%
      mutate(snhlRemaining = (casesRemaining - deathsRemaining) * prob_snhl)%>%
      mutate(doses = prob_vacc * popSize * 1.1)%>%
      mutate(casesAvertedPerDose = casesAverted/doses,
             snhlAvertedPerDose = snhlAverted/doses,
             deathsAvertedPerDose = deathsAverted/doses)
    
    ### LARGE OUTBREAK
    df_vacc_largeOutbreak = df_params_sim%>%
      mutate(cases = largeOutbreakSize)%>%
      mutate(deaths = cases * CFR_HCW)%>%
      mutate(snhl = (cases - deaths)*prob_snhl)%>%
      mutate(prob_vacc = prob_vacc_i,
             VE = VE_i)%>%
      mutate(casesVacc = cases*prob_vacc)%>%
      mutate(casesAverted = casesVacc * VE)%>%
      mutate(deathsAverted = casesAverted * CFR_HCW)%>%
      mutate(snhlAverted = (casesAverted - deathsAverted) * prob_snhl)%>%
      mutate(casesRemaining = cases - casesAverted)%>%
      mutate(deathsRemaining = casesRemaining * CFR_HCW)%>%
      mutate(snhlRemaining = (casesRemaining - deathsRemaining) * prob_snhl)%>%
      mutate(doses = prob_vacc * popSize * 1.1)%>%
      mutate(casesAvertedPerDose = casesAverted/doses,
             snhlAvertedPerDose = snhlAverted/doses,
             deathsAvertedPerDose = deathsAverted/doses)
    
    ### check that sum of averted + remaining = total
    # RAND
    stopifnot(sum(df_vacc_randOutbreak$cases) - sum(df_vacc_randOutbreak$casesAverted) - sum(df_vacc_randOutbreak$casesRemaining) < 0.0001)
    stopifnot(sum(df_vacc_randOutbreak$deaths) - sum(df_vacc_randOutbreak$deathsAverted) - sum(df_vacc_randOutbreak$deathsRemaining) < 0.0001)
    stopifnot(sum(df_vacc_randOutbreak$snhl) - sum(df_vacc_randOutbreak$snhlAverted) - sum(df_vacc_randOutbreak$snhlRemaining) < 0.0001)
    #SMALL
    stopifnot(sum(df_vacc_smallOutbreak$cases) - sum(df_vacc_smallOutbreak$casesAverted) - sum(df_vacc_smallOutbreak$casesRemaining) < 0.0001)
    stopifnot(sum(df_vacc_smallOutbreak$deaths) - sum(df_vacc_smallOutbreak$deathsAverted) - sum(df_vacc_smallOutbreak$deathsRemaining) < 0.0001)
    stopifnot(sum(df_vacc_smallOutbreak$snhl) - sum(df_vacc_smallOutbreak$snhlAverted) - sum(df_vacc_smallOutbreak$snhlRemaining) < 0.0001)
    #MEAN
    stopifnot(sum(df_vacc_meanOutbreak$cases) - sum(df_vacc_meanOutbreak$casesAverted) - sum(df_vacc_meanOutbreak$casesRemaining) < 0.0001)
    stopifnot(sum(df_vacc_meanOutbreak$deaths) - sum(df_vacc_meanOutbreak$deathsAverted) - sum(df_vacc_meanOutbreak$deathsRemaining) < 0.0001)
    stopifnot(sum(df_vacc_meanOutbreak$snhl) - sum(df_vacc_meanOutbreak$snhlAverted) - sum(df_vacc_meanOutbreak$snhlRemaining) < 0.0001)
    #LARGE
    stopifnot(sum(df_vacc_largeOutbreak$cases) - sum(df_vacc_largeOutbreak$casesAverted) - sum(df_vacc_largeOutbreak$casesRemaining) < 0.0001)
    stopifnot(sum(df_vacc_largeOutbreak$deaths) - sum(df_vacc_largeOutbreak$deathsAverted) - sum(df_vacc_largeOutbreak$deathsRemaining) < 0.0001)
    stopifnot(sum(df_vacc_largeOutbreak$snhl) - sum(df_vacc_largeOutbreak$snhlAverted) - sum(df_vacc_largeOutbreak$snhlRemaining) < 0.0001)
    
    ### SAVE RESULTS IN LIST
    list_vacc_randOutbreakSizes[[list_counter]] = df_vacc_randOutbreak
    list_vacc_smallOutbreakSizes[[list_counter]] = df_vacc_smallOutbreak
    list_vacc_meanOutbreakSizes[[list_counter]] = df_vacc_meanOutbreak
    list_vacc_largeOutbreakSizes[[list_counter]] = df_vacc_largeOutbreak}
}

### combine rows
df_vacc_randOutbreakSizes = do.call(rbind, list_vacc_randOutbreakSizes)%>%
  mutate(outbreakSize = "random")
df_vacc_smallOutbreakSizes = do.call(rbind, list_vacc_smallOutbreakSizes)%>%
  mutate(outbreakSize = "small")
df_vacc_meanOutbreakSizes = do.call(rbind, list_vacc_meanOutbreakSizes)%>%
  mutate(outbreakSize = "mean")
df_vacc_largeOutbreakSizes = do.call(rbind, list_vacc_largeOutbreakSizes)%>%
  mutate(outbreakSize = "large")

df_vacc_variedOutbreaks = bind_rows(df_vacc_randOutbreakSizes,
                                    df_vacc_smallOutbreakSizes,
                                    df_vacc_meanOutbreakSizes,
                                    df_vacc_largeOutbreakSizes)

# replace NaN with 0
df_vacc_variedOutbreaks[is.na(df_vacc_variedOutbreaks)] <- 0


#################################################
### (5) SIMULATE YLL GAINED PER DEATH AVERTED ###
#################################################

### Merge life expectancy estimates and calculate YLLs averted for each death averted
df_vacc_YLL = df_vacc_variedOutbreaks%>%
  left_join(df_un_le_nigeria_2025, by = c("Age"), relationship = "many-to-many")%>%
  mutate(YLL = LifeExpectancy*deaths,
         YLLaverted = LifeExpectancy*deathsAverted)%>%
  dplyr::select(prob_vacc, VE, n_draw, outbreakSize, Age, LifeExpectancy, YLL, YLLaverted)

### Summarise total YLL for each simulations
df_vacc_YLL_sum = df_vacc_YLL%>%
  group_by(prob_vacc, VE, n_draw, outbreakSize)%>%
  summarise(YLL = sum(YLL),
            YLLaverted = sum(YLLaverted))

### Merge original dataset with the YLLs, and add zeroes for rows where no YLLs averted (no deaths averted)
df_vacc_YLL = df_vacc_variedOutbreaks%>%
  left_join(., df_vacc_YLL_sum, by = c("prob_vacc", "VE", "n_draw", "outbreakSize"))%>%
  mutate(YLLaverted = ifelse(is.na(YLLaverted), 0, YLLaverted))

##########################################
### (6) SIMULATE DALYS AVERTED PER SIM ###
##########################################

### merge disutility weights to dataset by sim number
df_vacc_DALYs = df_vacc_YLL%>%
  mutate(dalys_acute_survived = (cases - deaths)*((dur_ill_prehosp+dur_hosp_died)/365)*disutility_hospital,
         dalys_acute_died = deaths*((dur_ill_prehosp+dur_hosp_died)/365)*disutility_hospital,
         dalys_snhl = snhl*dur_snhl*disutility_snhl,
         dalys_death = YLL)%>%
  mutate(dalys = dalys_acute_survived + dalys_acute_died + dalys_snhl + dalys_death)%>%
  mutate(dalysAverted_acute_survived = (casesAverted - deathsAverted)*((dur_ill_prehosp+dur_hosp_died)/365)*disutility_hospital,
         dalysAverted_acute_died = deathsAverted*((dur_ill_prehosp+dur_hosp_died)/365)*disutility_hospital,
         dalysAverted_snhl = snhlAverted*dur_snhl*disutility_snhl,
         dalysAverted_death = YLLaverted)%>%
  mutate(dalysAverted = dalysAverted_acute_survived + dalysAverted_acute_died + dalysAverted_snhl + dalysAverted_death)%>%
  mutate(dalysAvertedPerDose = dalysAverted/doses)


# replace NaN with 0
df_vacc_DALYs[which(is.nan(df_vacc_DALYs$dalysAvertedPerDose)),"dalysAvertedPerDose"] <- 0

############################################
### (7) SUMMARISE VACCINE IMPACT RESULTS ###
############################################

### update risk nuisance parameters specific to different approaches

if(which_approach %in% c("approach1", "approach2")){
  vec_risk = seq(0, 0.5, by = c(0.05))
}

if(which_approach %in% c("approach3")){
  vec_risk = 1
}


### SUMMARISE OUTCOMES AVERTED: PER OUTBREAK ###
df_vacc_outcomesAverted_summarised = df_vacc_DALYs%>%
  dplyr::select(outbreakSize, n_draw, prob_vacc, VE, 
                cases, deaths, snhl, dalys,
                casesAverted, deathsAverted, snhlAverted,
                casesAvertedPerDose, snhlAvertedPerDose, deathsAvertedPerDose, YLLaverted,
                #dalysAverted_acute_survived, dalysAverted_acute_died, dalysAverted_snhl, dalysAverted_death,
                dalysAverted, dalysAvertedPerDose)%>%
  mutate(casesRemaining = cases - casesAverted,
         deathsRemaining = deaths - deathsAverted,
         snhlRemaining = snhl - snhlAverted,
         dalysRemaining = dalys - dalysAverted)%>%
  pivot_longer(-c(n_draw, prob_vacc, VE, outbreakSize), names_to = "outcome", values_to = "value")%>%
  group_by(outbreakSize, prob_vacc, VE, outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975))%>%
  mutate(outbreakSize = factor(outbreakSize, 
                               levels = c("random", "small", "mean", "large"),
                               labels = vec_labels_outbreakSize))%>%
  # label is for DALYS per 1,000 doses only, hence multiplying by 1,000
  mutate(label = paste0(round(signif(mean*1000,3),2),"\n(",round(signif(mean*1000,3),2)," - ",round(signif(upper*1000,3),2),")"))

### SUMMARISE OUTCOMES AVERTED: CUMULATIVE (10 YEARS) ###
### add nuisance multipliers (annual outbreak probability, HCW turnover) and calculate cumulative impact
df_timelines = data.frame(turnover = "5% annual turnover", vaccinee_time = sum(0.95^(0:9)))%>%
  bind_rows(., data.frame(turnover = "10% annual turnover", vaccinee_time = sum(0.9^(0:9))))%>%
  bind_rows(., data.frame(turnover = "15% annual turnover", vaccinee_time = sum(0.85^(0:9))))%>%
  bind_rows(., data.frame(turnover = "25% annual turnover", vaccinee_time = sum(0.75^(0:9))))%>%
  cross_join(., data.frame(risk = vec_risk))

### combine per-outbreak risk with cumulative risk-time multipliers
df_vacc_outcomesAverted_summarised_nuisance = df_vacc_outcomesAverted_summarised%>%
  cross_join(., df_timelines)%>%
  mutate(mean = mean * risk * vaccinee_time,
         median = median * risk * vaccinee_time,
         lower = lower * risk * vaccinee_time,
         upper = upper * risk * vaccinee_time)%>%
  mutate(context = "Healthcare facility-targeted vaccination")%>%
  mutate(label = paste0(round(signif(mean*1000,3),2),"\n(",round(signif(lower*1000,3),2)," - ",round(signif(upper*1000,3),2),")"))

### SUMMARISE OUTCOMES AVERTED: ANNUAL FOR 10 YEARS ###
### add nuisance multipliers (annual outbreak probability, HCW turnover) and calculate annual impact
df_timelines2 = data.frame(turnover = "2% annual turnover", years = 1:10, vaccinee_time = 0.98^(0:9))%>%
  bind_rows(., data.frame(turnover = "5% annual turnover", years = 1:10, vaccinee_time = 0.95^(0:9)))%>%
  bind_rows(., data.frame(turnover = "10% annual turnover", years = 1:10, vaccinee_time = 0.9^(0:9)))%>%
  bind_rows(., data.frame(turnover = "15% annual turnover", years = 1:10, vaccinee_time = 0.85^(0:9)))%>%
  bind_rows(., data.frame(turnover = "25% annual turnover", years = 1:10, vaccinee_time = 0.75^(0:9)))%>%
  bind_rows(., data.frame(turnover = "50% annual turnover", years = 1:10, vaccinee_time = 0.5^(0:9)))%>%
  cross_join(., data.frame(risk = vec_risk))

### combine per-outbreak risk with annual risk-time multipliers
df_vacc_outcomesAverted_summarised_nuisance_time = df_vacc_outcomesAverted_summarised%>%
  cross_join(., df_timelines2)%>%
  mutate(mean = mean * risk * vaccinee_time,
         median = median * risk * vaccinee_time,
         lower = lower * risk * vaccinee_time,
         upper = upper * risk * vaccinee_time)%>%
  mutate(context = "Healthcare facility-targeted vaccination")%>%
  mutate(label = paste0(round(signif(mean*1000,3),2),"\n(",round(signif(lower*1000,3),2)," - ",round(signif(upper*1000,3),2),")"))


### SUMMARISE OUTCOMES AVERTED: ANNUAL CUMULATIVE (10 YEARS) ###
### add nuisance multipliers (annual outbreak probability, HCW turnover) and calculate cumulative annual impact
df_timelines2cumul = data.frame(turnover = "2% annual turnover", years = 1:10, vaccinee_time = cumsum(0.98^(0:9)))%>%
  bind_rows(., data.frame(turnover = "5% annual turnover", years = 1:10, vaccinee_time = cumsum(0.95^(0:9))))%>%
  bind_rows(., data.frame(turnover = "10% annual turnover", years = 1:10, vaccinee_time = cumsum(0.9^(0:9))))%>%
  bind_rows(., data.frame(turnover = "15% annual turnover", years = 1:10, vaccinee_time = cumsum(0.85^(0:9))))%>%
  bind_rows(., data.frame(turnover = "25% annual turnover", years = 1:10, vaccinee_time = cumsum(0.75^(0:9))))%>%
  bind_rows(., data.frame(turnover = "50% annual turnover", years = 1:10, vaccinee_time = cumsum(0.5^(0:9))))%>%
  cross_join(., data.frame(risk = vec_risk))

### combine per-outbreak risk with cumulative annual risk-time multipliers
df_vacc_outcomesAverted_summarised_nuisance_time_cumul = df_vacc_outcomesAverted_summarised%>%
  cross_join(., df_timelines2cumul)%>%
  mutate(mean = mean * risk * vaccinee_time,
         median = median * risk * vaccinee_time,
         lower = lower * risk * vaccinee_time,
         upper = upper * risk * vaccinee_time)%>%
  mutate(context = "Healthcare facility-targeted vaccination")%>%
  mutate(label = paste0(round(signif(mean*1000,3),2),"\n(",round(signif(lower*1000,3),2)," - ",round(signif(upper*1000,3),2),")"))







