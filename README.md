### lassaVacc_HCWs
Model evaluating health impacts of vaccinating Nigerian healthcare workers against Lassa fever. Composed of 5 scripts and 3 data input files.

# (1) housekeeping.R
Base functions and labels

# (2) hospital_model_general_inputs.R
General model inputs; this loads data from a literature review of Lassa fever CFR in HCWs (review_data_extraction.xlsx), state-level population estimates (df_population.Rdata), and life expectancy data (df_un_le_nigeria_2025.csv) 

# (3) hospital_model_inputs_approach3.R
Additional inputs specific to approach3 (the final approach considered)

# (4) hospital_model.R
The model, simulates outcomes and vaccine impact

# (5) hospital_model_plots.R
Data visualisations
