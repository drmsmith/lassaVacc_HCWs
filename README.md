# lassaVacc_HCWs
Model evaluating health impacts of vaccinating Nigerian healthcare workers against Lassa fever. Composed of 5 scripts and 5 data input files. 

This code was developed, tested and run in R v.4.5.1. All packages required to run the code are listed in housekeeping.R. Total run time on a normal desktop computer is <2 minutes. 

Running this code reproduces all quantitative results and figures in the manuscript.

### (1) housekeeping.R
Base functions and labels

### (2) hospital_model_general_inputs.R
General model inputs; this loads data from a literature review of Lassa fever CFR in HCWs (review_data_extraction.xlsx), state-level population estimates (df_population.Rdata), life expectancy data (df_un_le_nigeria_2025.csv) and decision tree parameters (params_montecarlo.Rdata)

### (3) hospital_model_inputs_approach3.R
Additional inputs specific to approach3 (the final approach considered) including NCDC data (NCDC data totals and HCW by week and annual.xlsx)

### (4) hospital_model.R
The model, simulates outcomes and vaccine impact

### (5) hospital_model_plots.R
Data visualisations
