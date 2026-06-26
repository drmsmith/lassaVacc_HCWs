########################
### PLOTS AND TABLES ###
########################

filepath = this.path::here()
setwd(filepath)
source("housekeeping.R")

save_plots = F
source("hospital_model_summarize.R")


##############################
##############################
### EXCEL OF MAIN OUTCOMES ###
##############################
##############################

df_excel_vaccOutcomes_summarised = bind_rows(df_vaccOutcomes_2yr_summarised%>%
                                               mutate(horizon = "2 years"),
                                             df_vaccOutcomes_5yr_summarised%>%
                                               mutate(horizon = "5 years"),
                                             df_vaccOutcomes_10yr_summarised%>%
                                               mutate(horizon = "10 years"))

if(save_plots){
  write.xlsx(df_excel_vaccOutcomes_summarised, 'df_excel_vaccOutcomes_summarised.xlsx')
}

###################################
###################################
### DESCRIPTIVE AND INPUT PLOTS ###
###################################
###################################

### Distribution of deaths from Irrua

### Plot age data ###
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


### HCW CFR ###

if(save_plots){
  png(file = "p_CFR_MA_allStudies.png",
      width = 800, height = 500, units = "px",
      pointsize = 12)
  forest(meta_CFR, xlab = "Proportion", leftlabs = c("Study", "Events", "Total"),
         random = T, common = F)
  dev.off()
}

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
  ylab("Weekly Lassa fever cases reported\nin Nigerian HCWs")+
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
  ylab("Weekly Lassa fever cases reported\nin Nigerian HCWs")+
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
  ylab("Monthly Lassa fever cases reported\nin Nigerian HCWs")+
  scale_fill_manual("State", values = cols_states)+
  guides(fill = guide_legend(ncol = 4))+
  theme(legend.position = c(0.25,0.85))

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


################################
### POPULATION AT RISK PLOTS ###
################################

### Change over time by age and sex
p_n_hcw_over_time_age_sex = df_states_n_hcw_age_sex%>%
  group_by(Year, age_group, Sex, n_draw)%>%
  summarise(N_hcw_age_sex= sum(N_hcw_age_sex))%>%
  group_by(Year, age_group, Sex)%>%
  summarise(mean = mean(N_hcw_age_sex),
            lower = quantile(N_hcw_age_sex, 0.025),
            upper = quantile(N_hcw_age_sex, 0.975))%>%
  mutate(Sex = factor(Sex, 
                      levels = c("Male", "Female_Preg", "Female_NotPreg"), 
                      labels = c("Male", "Female (pregnant)", "Female (not pregnant)")),
         Year = make_date(Year))%>%
  ggplot(., aes(x = Year, y = mean, ymax = upper, ymin = lower, colour = age_group, group = age_group))+
  geom_hline(yintercept = 0, colour = "darkgrey")+
  geom_point(shape = 1)+
  geom_errorbar(width = 0.2)+
  theme_bw()+
  facet_wrap(facets = vars(Sex))+
  ylab("Total HCW person-years")+xlab("Year")+
  scale_y_continuous(labels = comma)+
  scale_colour_manual("Age group", values = vec_cols_age)+
  scale_fill_manual("Age group", values = vec_cols_age)+
  guides(colour = guide_legend(title = "Age group"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

### Change over time by age
p_n_hcw_over_time_age = df_states_n_hcw_age_sex%>%
  group_by(Year, age_group, n_draw)%>%
  summarise(N_hcw_age_sex= sum(N_hcw_age_sex))%>%
  group_by(Year, age_group)%>%
  summarise(mean = mean(N_hcw_age_sex),
            lower = quantile(N_hcw_age_sex, 0.025),
            upper = quantile(N_hcw_age_sex, 0.975))%>%
  mutate(Year = make_date(Year))%>%
  ggplot(., aes(x = Year, y = mean, ymax = upper, ymin = lower, 
                colour = age_group, fill = age_group, group = age_group))+
  geom_hline(yintercept = 0, colour = "darkgrey")+
  geom_ribbon(alpha = 0.2, colour = NA)+
  geom_line()+
  geom_point(shape = 1)+
  theme_bw()+
  ylab("Total HCW person-years")+xlab("Year")+
  scale_y_continuous(labels = comma)+
  scale_colour_manual("Age group", values = vec_cols_age)+
  scale_fill_manual("Age group", values = vec_cols_age)+
  guides(colour = guide_legend(title = "Age group"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


### Total in 2025 by state, age and sex
p_n_hcw_2025_age_sex_state = df_states_n_hcw_age_sex%>%
  filter(Year == 2025)%>%
  group_by(Country, GID_0, Region, GID_1, age_group, Sex, n_draw)%>%
  summarise(N_hcw_age_sex= sum(N_hcw_age_sex))%>%
  group_by(Country, GID_0, Region, GID_1, age_group, Sex)%>%
  summarise(mean = mean(N_hcw_age_sex),
            lower = quantile(N_hcw_age_sex, 0.025),
            upper = quantile(N_hcw_age_sex, 0.975))%>%
  mutate(Sex = factor(Sex, 
                      levels = c("Male", "Female_Preg", "Female_NotPreg"), 
                      labels = c("Male", "Female (pregnant)", "Female (not pregnant)")))%>%
  ggplot(., aes(x = age_group, y = mean, ymax = upper, ymin = lower, colour = Sex, group = Sex))+
  geom_hline(yintercept = 0, colour = "darkgrey")+
  geom_point(shape = 1)+
  geom_errorbar(width = 0.2)+
  #geom_line()+
  theme_bw()+
  facet_wrap(facets = vars(Region))+
  ylab("Total HCW person-years (2025)")+xlab("Age group")+
  scale_y_continuous(labels = comma)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


p_grid_nhcw = plot_grid(p_n_hcw_2025_age_sex_state,
                        p_n_hcw_over_time_age,
                        nrow = 2, align = "v", axis = "lr", labels = c("A", "B"),
                        rel_heights = c(1,0.5))



### MAP OF HCW POPULATION SIZE (2025) ###

### Download Nigeria state boundaries (GADM level 1) 
nga_sf <- gadm("NGA", level = 1, path = tempdir()) %>%
  st_as_sf()

### Define data to be mapped ###

## Total number of HCWs
df_states_n_hcw_age_sex%>%
  filter(Year %in% c(2025,2034))%>%
  group_by(Year, Country, GID_0, n_draw)%>%
  summarise(N_hcw_age_sex= sum(N_hcw_age_sex))%>%
  group_by(Year, Country, GID_0)%>%
  summarise(mean = mean(N_hcw_age_sex),
            lower = quantile(N_hcw_age_sex, 0.025),
            upper = quantile(N_hcw_age_sex, 0.975))

##Total numbers of HCWs by sex
df_states_n_hcw_age_sex%>%
  filter(Year == 2025)%>%
  group_by(Country, GID_0, n_draw, Sex)%>%
  summarise(N_hcw_age_sex= sum(N_hcw_age_sex))%>%
  group_by(Country, GID_0, Sex)%>%
  summarise(mean = mean(N_hcw_age_sex),
            lower = quantile(N_hcw_age_sex, 0.025),
            upper = quantile(N_hcw_age_sex, 0.975))

## Number of HCWs by state
df_nigeria_hcws_mapping <- df_states_n_hcw_age_sex%>%
  filter(Year == 2025)%>%
  group_by(Country, GID_0, Region, GID_1, n_draw)%>%
  summarise(N_hcw_age_sex= sum(N_hcw_age_sex))%>%
  group_by(Country, GID_0, Region, GID_1)%>%
  summarise(mean = mean(N_hcw_age_sex),
            lower = quantile(N_hcw_age_sex, 0.025),
            upper = quantile(N_hcw_age_sex, 0.975))


## Incidence total
df_nigeria_infections_incidence_total <- df_states_n_hcw_age_sex_infections%>%
  filter(Year %in% c(2025,2034))%>%
  group_by(Year, Country, GID_0, n_draw)%>%
  summarise(N_infections= sum(N_infections))%>%
  group_by(Year, Country, GID_0, )%>%
  summarise(mean = mean(N_infections),
            lower = quantile(N_infections, 0.025),
            upper = quantile(N_infections, 0.975))

## Incidence by state
df_nigeria_infections_incidence_mapping <- df_states_n_hcw_age_sex_infections%>%
  filter(Year == 2025)%>%
  group_by(Country, GID_0, Region, GID_1, n_draw)%>%
  summarise(N_infections= sum(N_infections))%>%
  group_by(Country, GID_0, Region, GID_1)%>%
  summarise(mean = mean(N_infections),
            lower = quantile(N_infections, 0.025),
            upper = quantile(N_infections, 0.975))

### Join dataframes to the shapefile
nga_map_nhcw <- nga_sf %>%
  left_join(df_nigeria_hcws_mapping, by = c("GID_1" = "GID_1"))

nga_map_ninfections <- nga_sf %>%
  left_join(df_nigeria_infections_incidence_mapping, by = c("GID_1" = "GID_1"))

### Compute label positions (centroid of each state)
nga_labels_nhcw <- nga_map_nhcw %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  ) %>%
  st_drop_geometry()


### PLOT NUMBER OF HEATLHCARE WORKERS ###
p_map_nigeria_nhcws = ggplot() +
  # Grey fill for ALL states (full Nigeria outline)
  geom_sf(data = nga_map_nhcw, fill = "grey85", colour = "white", linewidth = 0.3) +
  # Choropleth fill only for states with data (NA states stay grey)
  geom_sf(
    data = nga_map_nhcw %>% filter(!is.na(mean)),
    aes(fill = mean),
    colour = "white",
    linewidth = 0.3
  ) +
  # Colour scale
  scale_fill_gradientn(
    colours = vec_cols_hcws,
    #option = "plasma",
    name = "Mean estimated\nnumber of HCWs\n(2025)",
    na.value = "grey85",
    labels = scales::comma
  ) +
  # State name labels (only for states with data; remove filter() to label all)
  geom_text(
    data = nga_labels_nhcw %>% filter(!is.na(mean)),
    aes(x = lon, y = lat, label = NAME_1),
    size = 2.5,
    colour = "black"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey40"),
    plot.caption  = element_text(size = 8, colour = "grey50"),
    legend.position = "right",
    legend.title  = element_text(size = 9),
    plot.margin   = margin(10, 10, 10, 10)
  )


### PLOT INFECTION INCIDENCE ###
p_map_nigeria_ninfections = ggplot() +
  # Grey fill for ALL states (full Nigeria outline)
  geom_sf(data = nga_map_ninfections, fill = "grey85", colour = "white", linewidth = 0.3) +
  # Choropleth fill only for states with data (NA states stay grey)
  geom_sf(
    data = nga_map_ninfections %>% filter(!is.na(mean)),
    aes(fill = mean),
    colour = "white",
    linewidth = 0.3
  ) +
  # Colour scale
  scale_fill_gradientn(
    colours = vec_cols_infections,
    #option = "plasma",
    name = "Mean estimated\nnumber of LASV\ninfections in\nHCWs (2025)",
    na.value = "grey85",
    labels = scales::comma
  ) +
  # State name labels (only for states with data; remove filter() to label all)
  geom_text(
    data = nga_labels_nhcw %>% filter(!is.na(mean)),
    aes(x = lon, y = lat, label = NAME_1),
    size = 2.5,
    colour = "black"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey40"),
    plot.caption  = element_text(size = 8, colour = "grey50"),
    legend.position = "right",
    legend.title  = element_text(size = 9),
    plot.margin   = margin(10, 10, 10, 10)
  )



if(save_plots){
  ggsave(p_n_hcw_over_time_age, file = "p_n_hcw_over_time_age.png", width = 10, height = 10, units = "cm")
  ggsave(p_n_hcw_over_time_age+
           theme(axis.text = element_text(size = 14),
                 axis.title = element_text(size = 18),
                 legend.text = element_text(size = 14),
                 legend.title = element_text(size = 18)), file = "p_n_hcw_over_time_age_bigFont.png",
         width = 15, height = 11, units = "cm")
  ggsave(p_n_hcw_over_time_age_sex, file = "p_n_hcw_over_time_age_sex.png", width = 20, height = 10, units = "cm")
  ggsave(p_n_hcw_2025_age_sex_state, file = "p_n_hcw_2025_age_sex_state.png", width = 20, height = 15, units = "cm")
  ggsave(p_grid_nhcw, file = "p_grid_nhcw.png", width = 14, height = 20, units = "cm")
  ggsave(p_map_nigeria_nhcws, file = "p_map_nigeria_nhcws.png", width = 15, height = 10, units = "cm")
  ggsave(p_map_nigeria_ninfections, file = "p_map_nigeria_ninfections.png", width = 15, height = 10, units = "cm")
  
}


#####################################################
### TABLE OF AGE-SPECIFIC LASV INCIDENCE BY STATE ###
#####################################################


t_zoonosis_incidence_summarise = df_zoonosis_incidence_summarise%>%
  ungroup()%>%
  dplyr::select(-c(Country, GID_0, GID_1))%>%
  flextable()%>%
  fontsize(size = 9)%>%
  theme_vanilla()%>%
  autofit()

doc_zoonosis_incidence_summarise = read_docx()%>%
  body_add_flextable(t_zoonosis_incidence_summarise)

if(save_plots){
  print(doc_zoonosis_incidence_summarise, target = paste0("t_zoonosis_incidence_summarise.docx"))
}

#######################
### INFECTION PLOTS ###
#######################

### Visualize LASV infections over time by age group
p_n_LASV_over_time = df_nigeria_n_hcw_age_infections_summarise%>%
  mutate(Year = make_date(Year))%>%
  ggplot(., aes(x = Year, y = N_infections_mean, ymin = N_infections_lower, ymax = N_infections_upper,
                colour = age_group, fill = age_group))+
  geom_hline(yintercept = 0, colour = "darkgrey")+
  geom_point(shape = 1)+
  geom_ribbon(colour = NA, alpha = 0.2)+
  geom_line()+
  ylab("LASV infections")+xlab("Year")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_colour_manual("Age group", values = vec_cols_age)+
  scale_fill_manual("Age group", values = vec_cols_age)

### Visualize HCWs in 2025 by age group
p_n_hcw_2025_age_sex = df_nigeria_n_hcw_age_sex_infections_summarise%>%
  mutate(Sex = factor(Sex,
                      levels = c("Female_NotPreg", "Female_Preg", "Male"),
                      labels = c("F (not pregnant)", "F (pregnant)", "M")))%>%
  filter(Year == 2025)%>%
  ggplot(., aes(x = age_group, y = N_hcw_age_sex_mean, ymin = N_hcw_age_sex_lower, ymax = N_hcw_age_sex_upper,
                colour = Sex, fill = Sex, group = Sex))+
  geom_hline(yintercept = 0, colour = "darkgrey")+
  geom_point(shape = 1)+
  geom_ribbon(colour = NA, alpha = 0.2)+
  geom_line()+
  ylab("Healthcare workers in endemic states (2025)")+xlab("Age group")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_y_continuous(labels = comma)

### Visualize LASV infections in 2025 by age group
p_n_LASV_2025_age_sex = df_nigeria_n_hcw_age_sex_infections_summarise%>%
  mutate(Sex = factor(Sex,
                      levels = c("Female_NotPreg", "Female_Preg", "Male"),
                      labels = c("F (not pregnant)", "F (pregnant)", "M")))%>%
  filter(Year == 2025)%>%
  ggplot(., aes(x = age_group, y = N_infections_mean, ymin = N_infections_lower, ymax = N_infections_upper,
                colour = Sex, fill = Sex, group = Sex))+
  geom_hline(yintercept = 0, colour = "darkgrey")+
  geom_point(shape = 1)+
  geom_ribbon(colour = NA, alpha = 0.2)+
  geom_line()+
  ylab("LASV infections (2025)")+xlab("Age group")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_n_LASV_grid_time_age_sex = plot_grid(p_n_LASV_2025_age_sex+theme(legend.position = "bottom"),
                                       p_n_LASV_over_time+theme(legend.position = "bottom"),
                                       ncol = 2, align = "h", axis = "tb", labels = c("A", "B"))

if(save_plots){
  ggsave(p_n_hcw_2025_age_sex, file = "p_n_hcw_2025_age_sex.png", width = 10, height = 8, units = "cm")
  ggsave(p_n_hcw_2025_age_sex+
           theme(axis.text = element_text(size = 14),
                 axis.title = element_text(size = 18),
                 legend.text = element_text(size = 14),
                 legend.title = element_text(size = 18)),
         file = "p_n_hcw_2025_age_sex_bigFont.png", width = 20, height = 15, units = "cm")
  ggsave(p_n_LASV_over_time, file = "p_n_LASV_over_time.png", width = 10, height = 8, units = "cm")
  ggsave(p_n_LASV_2025_age_sex, file = "p_n_LASV_2025_age_sex.png", width = 12, height = 8, units = "cm")
  ggsave(p_n_LASV_grid_time_age_sex, file = "p_n_LASV_grid_time_age_sex.png", width = 20, height = 10, units = "cm")
}


##########################
### HCW turnover plots ###
##########################

p_n_LASV_over_time_vaccstaus_turnover = df_test_vacc_cohort_noTurnover_long%>%
  filter(measure %in% c("N_infections", "N_infections_vacc"))%>%
  mutate(Group = case_when(measure == "N_infections" & turnover == 0 ~ "All HCWs",
                           measure == "N_infections_vacc" & turnover == 0 ~ "Vaccinated HCWs\n(no staff turnover)",
                           measure == "N_infections_vacc" & turnover == 0.15 ~ "Vaccinated HCWs\n(15% annual turnover)",
                           T ~ "0"))%>%
  filter(Group != 0)%>%
  mutate(Year = make_date(Year))%>%
  ggplot(., aes(x = Year, y = mean, ymax = upper, ymin = lower, fill = Group, colour = Group))+
  geom_hline(yintercept = 0, colour = "darkgrey")+
  geom_point()+
  geom_line()+
  geom_ribbon(colour = NA, alpha = 0.2)+
  theme_classic()+
  ylim(0, 700)+ylab("LASV infections")

if(save_plots){
  ggsave(p_n_LASV_over_time_vaccstaus_turnover,
         file = "p_n_LASV_over_time_vaccstaus_turnover.png",
         width = 12, height = 8, units = "cm") 
}



######################
### COMBINED PLOTS ###
######################

### FIGURE: MAPS OF HCWS AND INFECTIONS WITH CASE SERIES

p_grid_maps_case_series = plot_grid(
  plot_grid(p_map_nigeria_nhcws,
          p_map_nigeria_ninfections,
          ncol = 2,
          align = "h",
          axis = "bt",
          labels = c("A", "B")),
  p_cases_monthly_byState,
  nrow = 2,
  rel_heights = c(0.7,1),
  labels = c("", "C"))

if(save_plots){
  ggsave(p_grid_maps_case_series,
         file = "p_grid_maps_case_series.png",
         width = 24, height = 18, units = "cm") 
  
  ggsave(p_grid_maps_case_series,
         file = "p_grid_maps_case_series.pdf",
         width = 24, height = 18, units = "cm") 
}



#####################
#####################
### MODEL RESULTS ###
#####################
#####################

#######################
#######################
### Evaluate burden ###
#######################
#######################

### Test scripts


df_vaccOutcomes_annual_summarised%>%
  filter(Year == 2034, outcome %in% c("mild", "cases", "deaths", "snhl", "nnd", "fl", "dalys"))


######################################################################
### TABLES OF TOTAL BURDEN AND BURDEN PER 100,000 HCW PERSON-YEARS ###
######################################################################

### TOTAL BURDEN ###
### Total infections over 10 years
df_burden_infections_cumul = df_nigeria_n_hcw_infections%>%
  group_by(n_draw)%>%
  summarise(N_infections = sum(N_infections))%>%
  ungroup()%>%
  summarise(mean = mean(N_infections),
            median = median(N_infections),
            lower = quantile(N_infections, 0.025),
            upper = quantile(N_infections, 0.975))%>%
  mutate(outcome = "N_infections")

# clean (label only)
df_burden_infections_cumul_clean = df_burden_infections_cumul%>%
  summarise(
    across(
      where(is.numeric),
      ~ ifelse(.x < 10,
               round(.x, 1),
               round(.x, 0))
    )
  )%>%
  mutate(outcome = "infection",
         label = paste0(mean, " (", lower, ", ", upper, ")"))%>%
  dplyr::select(outcome, label)
  

### Total burden over 10 years
df_burden_mainoutcomes_cumul = df_vaccOutcomes_10yr_summarised%>%
  filter(cov_vacc == 0.2,
         turnover == 0,
         VE == 0.5,
         outcome %in% vec_main_outcomes)%>%
  ungroup()%>%
  dplyr::select(-c(Year, cov_vacc, turnover, VE))

# clean (label only)
df_burden_mainoutcomes_cumul_clean = df_burden_mainoutcomes_cumul%>%
  group_by(outcome)%>%
  summarise(
    across(
      where(is.numeric),
      ~ ifelse(.x < 10,
               round(.x, 1),
               round(.x, 0))
    ),
    .groups = "drop"
  )%>%
  mutate(label = paste0(mean, " (", lower, ", ", upper, ")"))%>%
  dplyr::select(outcome, label)

### Total burden (infections + outcomes) presented cleanly
df_burden_cumul = bind_rows(df_burden_infections_cumul_clean,
                            df_burden_mainoutcomes_cumul_clean)


### PERSON-TIME BURDEN ###

### Infections per person-time
df_burden_infections_personyears = df_nigeria_n_hcw_infections%>%
  filter(Year == 2025)%>%
  mutate(infections = N_infections/N_hcw*100000)%>%
  ungroup()%>%
  summarise(mean = mean(infections),
            median = median(infections),
            lower = quantile(infections, 0.025),
            upper = quantile(infections, 0.975))

# clean (label only)
df_burden_infections_personyears_clean = df_burden_infections_personyears%>%
  mutate(outcome = "infection",
         label = paste0(round(mean), " (", round(lower), ", ", round(upper), ")"))%>%
  dplyr::select(outcome, label)

### Burden per person-time
df_burden_mainoutcomes_personyears = df_vaccOutcomes_10yr%>%
  filter(cov_vacc == 0.2, turnover == 0, VE == 0.5)%>%
  ungroup()%>%
  mutate(mild = mild/N_hcw_age_sex * 100000,
         cases = cases/N_hcw_age_sex * 100000,
         deaths = deaths/N_hcw_age_sex * 100000,
         snhl = snhl/N_hcw_age_sex * 100000,
         nnd = nnd/N_hcw_age_sex * 100000,
         fl = fl/N_hcw_age_sex * 100000,
         dalys = dalys/N_hcw_age_sex * 100000)%>%
  dplyr::select(n_draw,
                all_of(vec_main_outcomes))%>%
  pivot_longer(-c(n_draw),
               names_to = "outcome", values_to = "value")%>%
  ungroup()%>%
  group_by(outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975))

# clean (label only)
df_burden_mainoutcomes_personyears_clean = df_burden_mainoutcomes_personyears%>%
  group_by(outcome)%>%
  summarise(
    across(
      where(is.numeric),
      ~ ifelse(.x < 10,
               round(.x, 1),
               round(.x, 0))
    ),
    .groups = "drop"
  )%>%
  mutate(label = paste0(mean, " (", lower, ", ", upper, ")"))%>%
  dplyr::select(outcome, label)

### Total burden (infections + outcomes)
df_burden_personyears = bind_rows(df_burden_infections_personyears_clean,
                                  df_burden_mainoutcomes_personyears_clean)
  


### COMBINE TOTAL BURDEN AND PERSON-TIME BURDEN

t_burden_total_persontime = left_join(df_burden_cumul%>%
                                         rename("Cumulative (2025-2034)" = label), 
                                       df_burden_personyears%>%
                                         rename("Per 100,000 PY" = label))

### EXPORT TABLE

t_burden_total_persontime_formatted = t_burden_total_persontime%>%
  flextable()%>%
  fontsize(size = 11)%>%
  theme_vanilla()%>%
  autofit()

doc_burden_total_persontime_formatted = read_docx()%>%
  body_add_flextable(t_burden_total_persontime_formatted)

if(save_plots){
  print(doc_burden_total_persontime_formatted, target = paste0("t_burden_total_persontime_formatted.docx"))
}



######################
### DALY BREAKDOWN ###
######################

### filter for arbitrary cov_vacc, turnover and VE (the same burden outcomes across each)

### DALYs main analysis
df_daly_breakdown1 = df_vaccOutcomes_10yr_summarised%>%
  filter(cov_vacc == 0.2,
         turnover == 0,
         VE == 0.5)%>%
  # filter outcomes of interest
  filter(outcome %in% c("dalys_acute_survived", "dalys_acute_died", "dalys_death", "dalys_mild", "dalys_nnd", "dalys_snhl", "dalys"))%>%
  mutate(analysis = "Base case")

### DALYs sensitivity analysis 1 (SNHL only after hospital)
df_daly_breakdown2 = df_vaccOutcomes_10yr_summarised%>%
  filter(cov_vacc == 0.2,
         turnover == 0,
         VE == 0.5)%>%
  # filter outcomes of interest
  filter(outcome %in% c("dalys_acute_survived", "dalys_acute_died", "dalys_death", "dalys_mild", "dalys_nnd", "dalys_snhl_sens1", "dalys_sens1"))%>%
  mutate(analysis = "Limit SNHL only to\nsurvivors of severe disease")%>%
  mutate(outcome = case_when(outcome == "dalys_snhl_sens1" ~ "dalys_snhl",
                             outcome == "dalys_sens1" ~ "dalys",
                             T ~ outcome))

### DALYs sensitivity analysis 2 (include foetal loss DALYs)
df_daly_breakdown3 = df_vaccOutcomes_10yr_summarised%>%
  filter(cov_vacc == 0.2,
         turnover == 0,
         VE == 0.5)%>%
  # filter outcomes of interest
  filter(outcome %in% c("dalys_acute_survived", "dalys_acute_died", "dalys_death", "dalys_mild", "dalys_nnd", "dalys_fl", "dalys_snhl", "dalys_sens2"))%>%
  mutate(analysis = "Include DALYs due\nto foetal loss")%>%
  mutate(outcome = case_when(outcome == "dalys_sens2" ~ "dalys",
                             T ~ outcome))

df_daly_breakdown_prelim = bind_rows(df_daly_breakdown1, df_daly_breakdown2, df_daly_breakdown3)%>%
  dplyr::select(-c(median, lower, upper))

df_daly_breakdown_acute = df_daly_breakdown_prelim%>%
  filter(outcome %in% c("dalys_mild", "dalys_acute_died", "dalys_acute_survived"))%>%
  group_by(Year, cov_vacc, turnover, VE, analysis)%>%
  summarise(mean = sum(mean))%>%
  mutate(outcome = "dalys_acute")%>%
  ungroup()

df_daly_breakdown = df_daly_breakdown_prelim%>%
  filter(!outcome %in% c("dalys_mild", "dalys_acute_died", "dalys_acute_survived"))%>%
  bind_rows(., df_daly_breakdown_acute)%>%
  mutate(DALY = factor(outcome,
                       levels = c("dalys_acute",
                                  "dalys_snhl",
                                  "dalys_death",
                                  "dalys_nnd",
                                  "dalys_fl",
                                  "dalys"),
                       labels = c("Acute disease",
                                  "SNHL",
                                  "HCW death",
                                  "Neonatal death",
                                  "Foetal loss",
                                  "All")))

p_daly_breakdown = df_daly_breakdown%>%
  filter(!outcome %in% c("dalys"))%>%
  ggplot(., aes(x = analysis, y = mean, fill = DALY))+
  geom_bar(position = 'stack', stat = "identity", colour = "black", linewidth = 0.5)+
  theme_classic()+
  scale_y_continuous(labels = comma)+
  xlab("")+
  ylab("Mean cumulative DALYs (2025-2034)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

if(save_plots){
  ggsave(p_daly_breakdown, file = paste0("p_daly_breakdown.png"),
         width = 10, height = 14, units = "cm")
}

##########################
### (10) DOSE FORECAST ###
##########################

### DOSES IN 2025
p_doses_2025 = df_doses%>%
  filter(cov_vacc > 0.3 & cov_vacc < 0.9)%>%
  group_by(cov_vacc)%>%
  summarise(mean = mean(doses),
            lower = quantile(doses, 0.025),
            upper = quantile(doses, 0.975))%>%
  mutate(cov_vacc_clean = factor(cov_vacc, 
                                 levels = c(0.2, 0.4, 0.6, 0.8, 1),
                                 labels = c("20%", "40%", "60%", "80%", "100%")))%>%
  ggplot(., aes(x = cov_vacc_clean, y = mean, ymin = lower, ymax = upper, fill = cov_vacc_clean))+
  geom_hline(yintercept = 0, colour = "darkgrey")+
  geom_bar(stat = "identity", colour = "black")+
  geom_errorbar(width = 0.2)+
  theme_classic()+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))+
  ylab("Doses (2025)")+xlab("Population coverage\ntarget in 2025")+
  scale_fill_manual(values = vec_cols_coverage[c(2,4,5)])+
  theme(legend.position = "none")


### DOSES 2026 TO 2034
p_doses_forecast = df_dose_forecast%>%
  filter(cov_vacc > 0.3 & cov_vacc < 0.9)%>%
  group_by(Year, cov_vacc, turnover)%>%
  summarise(mean = mean(N_dose_gap),
            lower = quantile(N_dose_gap, 0.025),
            upper = quantile(N_dose_gap, 0.975))%>%
  mutate(turnover = factor(turnover))%>%
  filter(turnover %in% c(0.05, 0.15, 0.25))%>%
  mutate(turnover = factor(turnover,
                           levels = c(0.05, 0.15, 0.25),
                           labels = c("5% annual attrition rate",
                                      "15% annual attrition rate",
                                      "25% annual attrition rate")),
         cov_vacc = factor(cov_vacc, 
                           levels = c(0.2, 0.4, 0.6, 0.8, 1),
                           labels = c("20%", "40%", "60%", "80%", "100%")))%>%
  ggplot(., aes(x = Year, y = mean, ymax = upper, ymin = lower, colour = cov_vacc, fill = cov_vacc))+
  geom_hline(yintercept = 0, colour = "darkgrey")+
  geom_ribbon(alpha = 0.25, colour = NA)+
  geom_point()+
  geom_line()+
  facet_wrap(facets = vars(turnover), nrow = 3)+
  theme_bw()+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))+
  ylab("Annual doses to maintain coverage target")+xlab("Year")+
  scale_fill_manual("Coverage\ntarget", values = vec_cols_coverage[c(2,4,5)])+
  scale_colour_manual("Coverage\ntarget", values = vec_cols_coverage[c(2,4,5)])+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p_grid_doses_2025_forecast = plot_grid(p_doses_2025, p_doses_forecast, ncol = 2, rel_widths = c(0.65, 1),
          labels = c("A", "B"), align = "h", axis = "tb")

if(save_plots){
  ggsave(p_grid_doses_2025_forecast, file = paste0("p_grid_doses_2025_forecast.png"),
         width = 14, height = 12, units = "cm")
  
  ggsave(p_grid_doses_2025_forecast, file = paste0("p_grid_doses_2025_forecast.pdf"),
         width = 14, height = 12, units = "cm")
}

##########################
### (10) CASES AVERTED ###
##########################

##############
### ANNUAL ###
##############

### CASES ANNUAL BY ATTRITION
p_casesAverted_annual_attr = df_vaccOutcomes_annual_summarised%>%
  mutate(VE = factor(VE),
         Year = factor(Year))%>%
  filter(VE == 0.7,
         outcome == "casesAverted",
         cov_vacc == 0.6,
         turnover %in% c(0.05, 0.15, 0.25))%>%
  mutate(turnover = factor(turnover,
                           levels = c(0.05, 0.15, 0.25), 
                           labels = c("5%", "15%", "25%")))%>%
  ggplot(., aes(x = Year, y = mean, ymin = lower, ymax = upper, colour = turnover, fill = turnover))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Year")+ylab("Cases averted annually")+
  scale_fill_manual("Annual HCW\nattrition rate", values = vec_cols_attrition)

if(save_plots){
  ggsave(p_casesAverted_annual_attr, file = paste0("p_casesAverted_annual_attr.png"),
         width = 12, height = 8, units = "cm")
}

#############
### CUMUL ###
#############

### CASES CUMULATIVE BY ATTRITION
p_casesAverted_cumul_attr = df_vaccOutcomes_annual_cumul_summarised%>%
  mutate(VE = factor(VE),
         Year = factor(Year))%>%
  filter(VE == 0.7,
         outcome == "casesAverted",
         cov_vacc == 0.6,
         turnover %in% c(0.05, 0.15, 0.25))%>%
  mutate(turnover = factor(turnover,
                           levels = c(0.05, 0.15, 0.25), 
                           labels = c("5%", "15%", "25%")))%>%
  ggplot(., aes(x = Year, y = mean, ymin = lower, ymax = upper, colour = turnover, fill = turnover))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Year")+ylab("Severe cases averted (cumulative)")+
  scale_fill_manual("Annual HCW\nattrition rate", values = vec_cols_attrition)

if(save_plots){
  ggsave(p_casesAverted_cumul_attr, file = paste0("p_casesAverted_cumul_attr.png"),
         width = 12, height = 8, units = "cm")
}

### CASES CUMULATIVE BY COVERAGE
p_casesAverted_cumul_cov = df_vaccOutcomes_annual_cumul_summarised%>%
  mutate(VE = factor(VE),
         Year = factor(Year))%>%
  filter(outcome == "casesAverted",
         turnover %in% c(0.15),
         VE == 0.7,
         cov_vacc %in% c(0.4, 0.6, 0.8))%>%
  mutate(cov_vacc = factor(cov_vacc, 
                           levels = c(0.4, 0.6, 0.8), 
                           labels = c("40%", "60%", "80%")))%>%
  ggplot(., aes(x = Year, y = mean, ymin = lower, ymax = upper, colour = cov_vacc, fill = cov_vacc))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Year")+ylab("Severe cases averted (cumulative)")+
  scale_fill_manual("Percentage of HCWs\nvaccinated in 2025", values = vec_cols_turnover)

if(save_plots){
  ggsave(p_casesAverted_cumul_cov, file = paste0("p_casesAverted_cumul_cov.png"),
         width = 12, height = 8, units = "cm")
}

### CASES CUMULATIVE BY VE
p_casesAverted_cumul_VE = df_vaccOutcomes_annual_cumul_summarised%>%
  mutate(VE = factor(VE),
         Year = factor(Year))%>%
  filter(outcome == "casesAverted",
         cov_vacc == 0.6,
         turnover %in% c(0.15))%>%
  mutate(VE = factor(VE, 
                     levels = c(0.5, 0.7, 0.9), 
                     labels = c("50%", "70%", "90%")))%>%
  ggplot(., aes(x = Year, y = mean, ymin = lower, ymax = upper, colour = VE, fill = VE))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Year")+ylab("Severe cases averted (cumulative)")+
  scale_fill_manual("Vaccine efficacy", values = rev(vec_cols_VE))


if(save_plots){
  ggsave(p_casesAverted_cumul_VE, file = paste0("p_casesAverted_cumul_VE.png"),
         width = 12, height = 8, units = "cm")
}

### COMBINE THESE THREE SENSITIVITY ANALYSES


#########################
### (10) SNHL AVERTED ###
#########################

### SNHL CUMULATIVE BY COVERAGE
p_snhlAverted_cumul_cov = df_vaccOutcomes_annual_cumul_summarised%>%
  mutate(VE = factor(VE),
         Year = factor(Year))%>%
  filter(outcome == "snhlAverted",
         turnover %in% c(0.15),
         VE == 0.7,
         cov_vacc %in% c(0.4, 0.6, 0.8))%>%
  mutate(cov_vacc = factor(cov_vacc, 
                           levels = c(0.4, 0.6, 0.8), 
                           labels = c("40%", "60%", "80%")))%>%
  ggplot(., aes(x = Year, y = mean, ymin = lower, ymax = upper, colour = cov_vacc, fill = cov_vacc))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Year")+ylab("Cumulative SNHL averted")+
  scale_fill_manual("Percentage of HCWs\nvaccinated in 2025", values = vec_cols_turnover)

if(save_plots){
  ggsave(p_snhlAverted_cumul_cov, file = paste0("p_snhlAverted_cumul_cov.png"),
         width = 12, height = 8, units = "cm")
}


###########################
### (11) DEATHS AVERTED ###
###########################

### ANNUAL BY ATTRITION
p_deathsAverted_annual_attr = df_vaccOutcomes_annual_summarised%>%
  mutate(VE = factor(VE),
         Year = factor(Year))%>%
  filter(VE == 0.7,
         outcome == "deathsAverted",
         cov_vacc == 0.6,
         turnover %in% c(0.05, 0.15, 0.25))%>%
  mutate(turnover = factor(turnover,
                           levels = c(0.05, 0.15, 0.25), 
                           labels = c("5%", "15%", "25%")))%>%
  ggplot(., aes(x = Year, y = mean, ymin = lower, ymax = upper, colour = turnover, fill = turnover))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Year")+ylab("Deaths averted annually")+
  scale_fill_manual("Annual HCW\nattrition rate", values = vec_cols_attrition)

if(save_plots){
  ggsave(p_deathsAverted_annual_attr, file = paste0("p_deathsAverted_annual_attr.png"),
         width = 12, height = 8, units = "cm")
}


### CUMULATIVE BY ATTRITION
p_deathsAverted_cumul_attr = df_vaccOutcomes_annual_cumul_summarised%>%
  mutate(VE = factor(VE),
         Year = factor(Year))%>%
  filter(VE == 0.7,
         outcome == "deathsAverted",
         cov_vacc == 0.6,
         turnover %in% c(0.05, 0.15, 0.25))%>%
  mutate(turnover = factor(turnover,
                           levels = c(0.05, 0.15, 0.25), 
                           labels = c("5%", "15%", "25%")))%>%
  ggplot(., aes(x = Year, y = mean, ymin = lower, ymax = upper, colour = turnover, fill = turnover))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Year")+ylab("Cumulative HCW deaths averted")+
  scale_fill_manual("Annual HCW attrition rate", values = vec_cols_attrition)

if(save_plots){
  ggsave(p_deathsAverted_cumul_attr, file = paste0("p_deathsAverted_cumul_attr.png"),
         width = 12, height = 10, units = "cm")
}


### CUMULATIVE BY VE
p_deathsAverted_cumul_VE = df_vaccOutcomes_annual_cumul_summarised%>%
  mutate(VE = factor(VE),
         Year = factor(Year))%>%
  filter(outcome == "deathsAverted",
         cov_vacc == 0.6,
         turnover %in% c(0.15))%>%
  mutate(VE = factor(VE, 
                     levels = c(0.5, 0.7, 0.9), 
                     labels = c("50%", "70%", "90%")))%>%
  ggplot(., aes(x = Year, y = mean, ymin = lower, ymax = upper, colour = VE, fill = VE))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Year")+ylab("Cumulative HCW deaths averted")+
  scale_fill_manual("Vaccine efficacy", values = rev(vec_cols_VE))

if(save_plots){
  ggsave(p_deathsAverted_cumul_VE, file = paste0("p_deathsAverted_cumul_VE.png"),
         width = 12, height = 10, units = "cm")
}

### ANNUAL BY VACC COVERAGE
p_deathsAverted_annual_cov = df_vaccOutcomes_annual_summarised%>%
  mutate(VE = factor(VE),
         Year = factor(Year))%>%
  filter(outcome == "deathsAverted",
         turnover %in% c(0.15),
         VE == 0.7,
         cov_vacc %in% c(0.4, 0.6, 0.8))%>%
  mutate(cov_vacc = factor(cov_vacc, 
                           levels = c(0.4, 0.6, 0.8), 
                           labels = c("40%", "60%", "80%")))%>%
  ggplot(., aes(x = Year, y = mean, ymin = lower, ymax = upper, colour = cov_vacc, fill = cov_vacc))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Year")+ylab("Annual HCW deaths averted")+
  scale_fill_manual("Percentage of HCWs\nvaccinated in 2025", values = vec_cols_turnover)

if(save_plots){
  ggsave(p_deathsAverted_annual_cov, file = paste0("p_deathsAverted_annual_cov.png"),
         width = 12, height = 10, units = "cm")
}


### CUMULATIVE BY VACC COVERAGE
p_deathsAverted_cumul_cov = df_vaccOutcomes_annual_cumul_summarised%>%
  mutate(VE = factor(VE),
         Year = factor(Year))%>%
  filter(outcome == "deathsAverted",
         turnover %in% c(0.15),
         VE == 0.7,
         cov_vacc %in% c(0.4, 0.6, 0.8),
         #Year %in% seq(2026, 2034, by = 2)
         )%>%
  mutate(cov_vacc = factor(cov_vacc, 
                     levels = c(0.4, 0.6, 0.8), 
                     labels = c("40%", "60%", "80%")))%>%
  ggplot(., aes(x = Year, y = mean, ymin = lower, ymax = upper, colour = cov_vacc, fill = cov_vacc))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Year")+ylab("Cumulative HCW deaths averted")+
  scale_fill_manual("Percentage of HCWs\n vaccinated in 2025", values = vec_cols_turnover)

if(save_plots){
  ggsave(p_deathsAverted_cumul_cov, file = paste0("p_deathsAverted_cumul_cov.png"),
         width = 12, height = 10, units = "cm")
  
  ggsave(p_deathsAverted_cumul_cov, file = paste0("p_deathsAverted_cumul_cov.pdf"),
         width = 12, height = 10, units = "cm")
}


##########################
### (11) DALYS AVERTED ###
##########################

### ANNUAL BY ATTRITION
p_dalysAverted_annual_attr = df_vaccOutcomes_annual_summarised%>%
  mutate(VE = factor(VE),
         Year = factor(Year))%>%
  filter(VE == 0.7,
         outcome == "dalysAverted",
         cov_vacc == 0.6,
         turnover %in% c(0.05, 0.15, 0.25))%>%
  mutate(turnover = factor(turnover,
                           levels = c(0.05, 0.15, 0.25), 
                           labels = c("5%", "15%", "25%")))%>%
  ggplot(., aes(x = Year, y = mean, ymin = lower, ymax = upper, colour = turnover, fill = turnover))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Year")+ylab("DALYs averted annually")+
  scale_fill_manual("Annual HCW\nattrition rate", values = vec_cols_attrition)

if(save_plots){
  ggsave(p_dalysAverted_annual_attr, file = paste0("p_dalysAverted_annual_attr.png"),
         width = 12, height = 10, units = "cm")
}


### CUMULATIVE BY ATTRITION
p_dalysAverted_cumul_attr = df_vaccOutcomes_annual_cumul_summarised%>%
  mutate(VE = factor(VE),
         Year = factor(Year))%>%
  filter(VE == 0.7,
         outcome == "dalysAverted",
         cov_vacc == 0.6,
         turnover %in% c(0.05, 0.15, 0.25))%>%
  mutate(turnover = factor(turnover,
                           levels = c(0.05, 0.15, 0.25), 
                           labels = c("5%", "15%", "25%")))%>%
  ggplot(., aes(x = Year, y = mean, ymin = lower, ymax = upper, colour = turnover, fill = turnover))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9), linewidth = 0.2)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  xlab("Year")+ylab("Cumulative DALYs averted")+
  scale_fill_manual("Annual HCW\nattrition rate", values = vec_cols_attrition)

if(save_plots){
  ggsave(p_dalysAverted_cumul_attr, file = paste0("p_dalysAverted_cumul_attr.png"),
         width = 12, height = 10, units = "cm")
}




##########################################
### SLIMMED DOWN PLOTS FOR MAIN FIGURE ###
##########################################

### GRID OF CASES AND SNHL AVERTED BY COVERAGE
p_cases_and_snhlAverted_cumul_SA = plot_grid(p_casesAverted_cumul_cov+
                                       theme(legend.position = "right"),
                                     p_snhlAverted_cumul_cov+
                                       theme(legend.position = "right"),
                                     nrow = 2,
                                     labels = c("A", "B"),
                                     align = "v",
                                     axis = "lr")

if(save_plots){
  ggsave(p_cases_and_snhlAverted_cumul_SA, file = paste0("p_cases_and_snhlAverted_cumul_SA.png"),
         width = 18, height = 17, units = "cm")
}

### GRID OF DEATHS AVERTED BY ATTRITION AND VE
p_deathsAverted_cumul_SA = plot_grid(p_deathsAverted_cumul_attr+
                                       theme(legend.position = "right")+
                                       guides(fill=guide_legend(title="Annual HCW\nattrition rate")),
                                     p_deathsAverted_cumul_VE+
                                       theme(legend.position = "right"),
                                     nrow = 2,
                                     labels = c("A", "B"),
                                     align = "v",
                                     axis = "lr")

if(save_plots){
  ggsave(p_deathsAverted_cumul_SA, file = paste0("p_deathsAverted_cumul_SA.png"),
         width = 18, height = 17, units = "cm")
}

#######################################
### CUMULATIVE ALL OUTCOMES AVERTED ###
#######################################
p_outcomesAverted_cumul = df_vaccOutcomes_annual_cumul_summarised%>%
  mutate(VE = factor(VE),
         Year = factor(Year))%>%
  filter(VE == 0.7,
         outcome %in% c("casesAverted", "snhlAverted", "deathsAverted", "dalysAverted"),
         cov_vacc == 0.6,
         turnover %in% c(0.15))%>%
  mutate(outcome = factor(outcome,
                           levels = c("casesAverted", "snhlAverted", "deathsAverted", "dalysAverted"), 
                           labels = c("Severe Lassa fever", "SNHL", "HCW deaths", "DALYs")))%>%
  ggplot(., aes(x = Year, y = mean, ymin = lower, ymax = upper, colour = outcome, fill = outcome))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = 0.9),
           linewidth = 0.2, alpha = 0.7)+
  geom_errorbar(colour = "black", position = position_dodge(width = 0.9), width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Year")+ylab("Total burden averted due to vaccination (cumulative)")+
  facet_wrap(facet = vars(outcome), scale = "free")+
  scale_fill_manual("Health outcome", values = vec_cols_outcomes)

if(save_plots){
  ggsave(p_outcomesAverted_cumul, file = paste0("p_outcomesAverted_cumul.png"),
         width = 15, height = 13, units = "cm")
}


###################################
### CUMULATIVE ALL SAs COMBINED ###
###################################

### CASES ###
### CUMUL CASE COV
p_SA_cumul_case_cov = df_vaccOutcomes_10yr_summarised%>%
  mutate(VE = factor(VE))%>%
  filter(outcome == "casesAverted",
         turnover %in% c(0.15),
         VE == 0.7,
         cov_vacc %in% c(0.4, 0.6, 0.8)
  )%>%
  mutate(cov_vacc = factor(cov_vacc, 
                           levels = c(0.4, 0.6, 0.8), 
                           labels = c("40%", "60%", "80%")))%>%
  ggplot(., aes(x = cov_vacc, y = mean, ymin = lower, ymax = upper, colour = cov_vacc, fill = cov_vacc))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.2)+
  geom_errorbar(colour = "black", width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.direction = "vertical")+
  ylab("Severe LF")+
  scale_fill_manual("Vaccine\ncoverage", values = vec_cols_coverage)+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), limits = c(0,175))

### CUMUL CASE VE
p_SA_cumul_case_VE = df_vaccOutcomes_10yr_summarised%>%
  mutate(VE = factor(VE))%>%
  filter(outcome == "casesAverted",
         cov_vacc == 0.6,
         turnover == 0.15)%>%
  mutate(VE = factor(VE, 
                     levels = c(0.5, 0.7, 0.9), 
                     labels = c("50%", "70%", "90%")))%>%
  ggplot(., aes(x = VE, y = mean, ymin = lower, ymax = upper, colour = VE, fill = VE))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.2)+
  geom_errorbar(colour = "black", width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.direction = "vertical")+
  ylab("Severe LF")+
  scale_fill_manual("Vaccine\nefficacy", values = rev(vec_cols_VE))+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), limits = c(0,175))

### CUMUL CASE ATTR
p_SA_cumul_case_attr = df_vaccOutcomes_10yr_summarised%>%
  mutate(VE = factor(VE))%>%
  filter(VE == 0.7,
         outcome == "casesAverted",
         cov_vacc == 0.6,
         turnover %in% c(0.05, 0.15, 0.25))%>%
  mutate(turnover = factor(turnover,
                           levels = c(0.05, 0.15, 0.25), 
                           labels = c("5%", "15%", "25%")))%>%
  ggplot(., aes(x = turnover, y = mean, ymin = lower, ymax = upper, colour = turnover, fill = turnover))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.2)+
  geom_errorbar(colour = "black", width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.direction = "vertical")+
  ylab("Severe LF")+
  scale_fill_manual("Annual HCW\nattrition rate", values = vec_cols_attrition)+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), limits = c(0,175))

### CUMUL CASE HORIZ
p_SA_cumul_case_horiz = bind_rows(df_vaccOutcomes_2yr_summarised%>%
                                  mutate(horizon = "2 years"),
                                df_vaccOutcomes_5yr_summarised%>%
                                  mutate(horizon = "5 years"),
                                df_vaccOutcomes_10yr_summarised%>%
                                  mutate(horizon = "10 years"))%>%
  mutate(VE = factor(VE))%>%
  filter(outcome == "casesAverted",
         turnover %in% c(0.15),
         VE == 0.7,
         cov_vacc %in% c(0.6)
  )%>%
  mutate(horizon = factor(horizon,
                           levels = c("2 years", "5 years", "10 years")))%>%
  ggplot(., aes(x = horizon, y = mean, ymin = lower, ymax = upper, 
                colour = horizon, fill = horizon))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.2, alpha = 0.6)+
  geom_errorbar(colour = "black", width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.direction = "vertical")+
  ylab("Severe LF")+
  scale_fill_manual("Immunity\nduration", values = vec_cols_horizon)+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), limits = c(0,175))

### SNHL ###
### CUMUL SNHL COV
p_SA_cumul_snhl_cov = df_vaccOutcomes_10yr_summarised%>%
  mutate(VE = factor(VE))%>%
  filter(outcome == "snhlAverted",
         turnover %in% c(0.15),
         VE == 0.7,
         cov_vacc %in% c(0.4, 0.6, 0.8)
  )%>%
  mutate(cov_vacc = factor(cov_vacc, 
                           levels = c(0.4, 0.6, 0.8), 
                           labels = c("40%", "60%", "80%")))%>%
  ggplot(., aes(x = cov_vacc, y = mean, ymin = lower, ymax = upper, colour = cov_vacc, fill = cov_vacc))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.2)+
  geom_errorbar(colour = "black", width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.direction = "vertical")+
  ylab("SNHL")+
  scale_fill_manual("Vaccine\ncoverage", values = vec_cols_coverage)+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), limits = c(0,225))

### CUMUL SNHL VE
p_SA_cumul_snhl_VE = df_vaccOutcomes_10yr_summarised%>%
  mutate(VE = factor(VE))%>%
  filter(outcome == "snhlAverted",
         cov_vacc == 0.6,
         turnover == 0.15)%>%
  mutate(VE = factor(VE, 
                     levels = c(0.5, 0.7, 0.9), 
                     labels = c("50%", "70%", "90%")))%>%
  ggplot(., aes(x = VE, y = mean, ymin = lower, ymax = upper, colour = VE, fill = VE))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.2)+
  geom_errorbar(colour = "black", width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.direction = "vertical")+
  ylab("SNHL")+
  scale_fill_manual("Vaccine\nefficacy", values = rev(vec_cols_VE))+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), limits = c(0,225))

### CUMUL SNHL ATTR
p_SA_cumul_snhl_attr = df_vaccOutcomes_10yr_summarised%>%
  mutate(VE = factor(VE))%>%
  filter(VE == 0.7,
         outcome == "snhlAverted",
         cov_vacc == 0.6,
         turnover %in% c(0.05, 0.15, 0.25))%>%
  mutate(turnover = factor(turnover,
                           levels = c(0.05, 0.15, 0.25), 
                           labels = c("5%", "15%", "25%")))%>%
  ggplot(., aes(x = turnover, y = mean, ymin = lower, ymax = upper, colour = turnover, fill = turnover))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.2)+
  geom_errorbar(colour = "black", width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.direction = "vertical")+
  ylab("SNHL")+
  scale_fill_manual("Annual HCW\nattrition rate", values = vec_cols_attrition)+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), limits = c(0,225))

### CUMUL SNHL HORIZ
p_SA_cumul_snhl_horiz = bind_rows(df_vaccOutcomes_2yr_summarised%>%
                                    mutate(horizon = "2 years"),
                                  df_vaccOutcomes_5yr_summarised%>%
                                    mutate(horizon = "5 years"),
                                  df_vaccOutcomes_10yr_summarised%>%
                                    mutate(horizon = "10 years"))%>%
  mutate(VE = factor(VE))%>%
  filter(outcome == "snhlAverted",
         turnover %in% c(0.15),
         VE == 0.7,
         cov_vacc %in% c(0.6)
  )%>%
  mutate(horizon = factor(horizon,
                          levels = c("2 years", "5 years", "10 years")))%>%
  ggplot(., aes(x = horizon, y = mean, ymin = lower, ymax = upper, 
                colour = horizon, fill = horizon))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.2, alpha = 0.6)+
  geom_errorbar(colour = "black", width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.direction = "vertical")+
  ylab("SNHL")+
  scale_fill_manual("Immunity\nduration", values = vec_cols_horizon)+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), limits = c(0,225))


### DEATHS ###
### CUMUL DEATHS COV
p_SA_cumul_death_cov = df_vaccOutcomes_10yr_summarised%>%
  mutate(VE = factor(VE))%>%
  filter(outcome == "deathsAverted",
         turnover %in% c(0.15),
         VE == 0.7,
         cov_vacc %in% c(0.4, 0.6, 0.8)
  )%>%
  mutate(cov_vacc = factor(cov_vacc, 
                           levels = c(0.4, 0.6, 0.8), 
                           labels = c("40%", "60%", "80%")))%>%
  ggplot(., aes(x = cov_vacc, y = mean, ymin = lower, ymax = upper, colour = cov_vacc, fill = cov_vacc))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.2)+
  geom_errorbar(colour = "black", width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.direction = "vertical")+
  ylab("HCW deaths")+
  scale_fill_manual("Vaccine\ncoverage", values = vec_cols_coverage)+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), limits = c(0,70))

### CUMUL DEATHS VE
p_SA_cumul_death_VE = df_vaccOutcomes_10yr_summarised%>%
  mutate(VE = factor(VE))%>%
  filter(outcome == "deathsAverted",
         cov_vacc == 0.6,
         turnover == 0.15)%>%
  mutate(VE = factor(VE, 
                     levels = c(0.5, 0.7, 0.9), 
                     labels = c("50%", "70%", "90%")))%>%
  ggplot(., aes(x = VE, y = mean, ymin = lower, ymax = upper, colour = VE, fill = VE))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.2)+
  geom_errorbar(colour = "black", width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.direction = "vertical")+
  ylab("HCW deaths")+
  scale_fill_manual("Vaccine\nefficacy ", values = rev(vec_cols_VE))+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), limits = c(0,70))

### CUMUL DEATHS ATTR
p_SA_cumul_death_attr = df_vaccOutcomes_10yr_summarised%>%
  mutate(VE = factor(VE))%>%
  filter(VE == 0.7,
         outcome == "deathsAverted",
         cov_vacc == 0.6,
         turnover %in% c(0.05, 0.15, 0.25))%>%
  mutate(turnover = factor(turnover,
                           levels = c(0.05, 0.15, 0.25), 
                           labels = c("5%", "15%", "25%")))%>%
  ggplot(., aes(x = turnover, y = mean, ymin = lower, ymax = upper, colour = turnover, fill = turnover))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.2)+
  geom_errorbar(colour = "black", width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.direction = "vertical")+
  ylab("HCW deaths")+
  scale_fill_manual("Annual HCW\nattrition rate", values = vec_cols_attrition)+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), limits = c(0,70))

### CUMUL DEATHS HORIZ
p_SA_cumul_death_horiz = bind_rows(df_vaccOutcomes_2yr_summarised%>%
                                    mutate(horizon = "2 years"),
                                  df_vaccOutcomes_5yr_summarised%>%
                                    mutate(horizon = "5 years"),
                                  df_vaccOutcomes_10yr_summarised%>%
                                    mutate(horizon = "10 years"))%>%
  mutate(VE = factor(VE))%>%
  filter(outcome == "deathsAverted",
         turnover %in% c(0.15),
         VE == 0.7,
         cov_vacc %in% c(0.6)
  )%>%
  mutate(horizon = factor(horizon,
                          levels = c("2 years", "5 years", "10 years")))%>%
  ggplot(., aes(x = horizon, y = mean, ymin = lower, ymax = upper, 
                colour = horizon, fill = horizon))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.2, alpha = 0.6)+
  geom_errorbar(colour = "black", width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.direction = "vertical")+
  ylab("HCW deaths")+
  scale_fill_manual("Immunity\nduration", values = vec_cols_horizon)+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), limits = c(0,70))


### DALYs ###
### CUMUL DALYs COV
p_SA_cumul_dalys_cov = df_vaccOutcomes_10yr_summarised%>%
  mutate(VE = factor(VE))%>%
  filter(outcome == "dalysAverted",
         turnover %in% c(0.15),
         VE == 0.7,
         cov_vacc %in% c(0.4, 0.6, 0.8)
  )%>%
  mutate(cov_vacc = factor(cov_vacc, 
                           levels = c(0.4, 0.6, 0.8), 
                           labels = c("40%", "60%", "80%")))%>%
  ggplot(., aes(x = cov_vacc, y = mean, ymin = lower, ymax = upper, colour = cov_vacc, fill = cov_vacc))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.2)+
  geom_errorbar(colour = "black", width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.direction = "vertical")+
  ylab("DALYs")+
  scale_fill_manual("Vaccine\ncoverage", values = vec_cols_coverage)+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), limits = c(0,2050))

### CUMUL DALYs VE
p_SA_cumul_dalys_VE = df_vaccOutcomes_10yr_summarised%>%
  mutate(VE = factor(VE))%>%
  filter(outcome == "dalysAverted",
         cov_vacc == 0.6,
         turnover == 0.15)%>%
  mutate(VE = factor(VE, 
                     levels = c(0.5, 0.7, 0.9), 
                     labels = c("50%", "70%", "90%")))%>%
  ggplot(., aes(x = VE, y = mean, ymin = lower, ymax = upper, colour = VE, fill = VE))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.2)+
  geom_errorbar(colour = "black", width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.direction = "vertical")+
  ylab("DALYs")+
  scale_fill_manual("Vaccine\nefficacy", values = rev(vec_cols_VE))+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), limits = c(0,2050))

### CUMUL DALYs ATTR
p_SA_cumul_dalys_attr = df_vaccOutcomes_10yr_summarised%>%
  mutate(VE = factor(VE))%>%
  filter(VE == 0.7,
         outcome == "dalysAverted",
         cov_vacc == 0.6,
         turnover %in% c(0.05, 0.15, 0.25))%>%
  mutate(turnover = factor(turnover,
                           levels = c(0.05, 0.15, 0.25), 
                           labels = c("5%", "15%", "25%")))%>%
  ggplot(., aes(x = turnover, y = mean, ymin = lower, ymax = upper, colour = turnover, fill = turnover))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.2)+
  geom_errorbar(colour = "black", width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.direction = "vertical")+
  ylab("DALYs")+
  scale_fill_manual("Annual HCW\nattrition rate", values = vec_cols_attrition)+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), limits = c(0,2050))

### CUMUL DEATHS HORIZ
p_SA_cumul_dalys_horiz = bind_rows(df_vaccOutcomes_2yr_summarised%>%
                                     mutate(horizon = "2 years"),
                                   df_vaccOutcomes_5yr_summarised%>%
                                     mutate(horizon = "5 years"),
                                   df_vaccOutcomes_10yr_summarised%>%
                                     mutate(horizon = "10 years"))%>%
  mutate(VE = factor(VE))%>%
  filter(outcome == "dalysAverted",
         turnover %in% c(0.15),
         VE == 0.7,
         cov_vacc %in% c(0.6)
  )%>%
  mutate(horizon = factor(horizon,
                          levels = c("2 years", "5 years", "10 years")))%>%
  ggplot(., aes(x = horizon, y = mean, ymin = lower, ymax = upper, 
                colour = horizon, fill = horizon))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.2, alpha = 0.6)+
  geom_errorbar(colour = "black", width = 0.3, linewidth = 0.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.direction = "vertical")+
  ylab("DALYs")+
  scale_fill_manual("Immunity\nduration", values = vec_cols_horizon)+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), limits = c(0,2050))


### COMBINE ###

p_SA_cov = ggarrange(p_SA_cumul_case_cov, p_SA_cumul_snhl_cov, p_SA_cumul_death_cov, p_SA_cumul_dalys_cov,
                     nrow = 4,
                     common.legend = T,
                     legend = "bottom",
                     align = "v")

p_SA_VE = ggarrange(p_SA_cumul_case_VE, p_SA_cumul_snhl_VE, p_SA_cumul_death_VE, p_SA_cumul_dalys_VE,
                    nrow = 4,
                    common.legend = T,
                    legend = "bottom",
                    align = "v")


p_SA_attr = ggarrange(p_SA_cumul_case_attr, p_SA_cumul_snhl_attr, p_SA_cumul_death_attr, p_SA_cumul_dalys_attr,
                      nrow = 4,
                      common.legend = T,
                      legend = "bottom",
                      align = "v")

p_SA_horiz = ggarrange(p_SA_cumul_case_horiz, p_SA_cumul_snhl_horiz, p_SA_cumul_death_horiz, p_SA_cumul_dalys_horiz,
                      nrow = 4,
                      common.legend = T,
                      legend = "bottom",
                      align = "v")

p_SA_grid = plot_grid(p_SA_cov,
                      p_SA_VE,
                      p_SA_attr,
                      p_SA_horiz,
                      ncol = 4,
                      align = "h", axis = "tb",
                      labels = c("A", "B", "C", "D"))


if(save_plots){
  ggsave(p_SA_grid, file = "p_SA_grid.png", 
         width = 15, height = 18, units = "cm")
  
  ggsave(p_SA_grid, file = "p_SA_grid.pdf", 
         width = 15, height = 18, units = "cm")
}




##########################################################################
### (XX) HEAT MAP OF VACCINE IMPACT PER DOSE RELATIVE TO PREVIOUS WORK ###
##########################################################################

#############
### CASES ###
#############

### combine risk targeted data and HCW data
df_vacc_casesAverted_risk_HCWs = df_vaccOutcomes_10yr_perDose_summarised%>%
  ungroup()%>%
  mutate(VE = factor(VE),
         turnover = factor(turnover),
         strategy = "Healthcare workers",
         mean = mean,
         lower = lower,
         upper = upper)%>%
  rename(outcome = measure)%>%
  filter(VE %in% vec_levels_VE,
         outcome == "casesAverted",
         cov_vacc == 0.6,
         turnover == 0.15)%>%
  mutate(outcome = "casesAvertedPerDose")%>%
  mutate(label = paste0(round(signif(mean,3),2),"\n(",round(signif(lower,3),2)," - ",round(signif(upper,3),2),")"))%>%
  mutate(context = "Healthcare facility-targeted vaccination",
         strategy = "Healthcare workers")%>%
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
df_vacc_deathsAverted_risk_HCWs = df_vaccOutcomes_10yr_perDose_summarised%>%
  ungroup()%>%
  mutate(VE = factor(VE),
         turnover = factor(turnover),
         strategy = "Healthcare workers",
         mean = mean,
         lower = lower,
         upper = upper)%>%
  rename(outcome = measure)%>%
  filter(VE %in% vec_levels_VE,
         outcome == "deathsAverted",
         cov_vacc == 0.6,
         turnover == 0.15)%>%
  mutate(outcome = "deathsAvertedPerDose")%>%
  mutate(label = paste0(round(signif(mean,3),2),"\n(",round(signif(lower,3),2)," - ",round(signif(upper,3),2),")"))%>%
  mutate(context = "Healthcare facility-targeted vaccination",
         strategy = "Healthcare workers")%>%
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
  scale_fill_gradientn("Cumulative HCW\ndeaths averted\nper 1,000 doses", 
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
  ggsave(p_grid_casesAvertedPerDose_deathsAvertedPerDose, file = paste0("p_grid_casesAvertedPerDose_deathsAvertedPerDose.png"),
         width = 18, height = 18, units = "cm")
}

#############
### DALYS ###
#############

### combine risk targeted data and HCW data
df_vacc_dalysAverted_risk_HCWs = df_vaccOutcomes_10yr_perDose_summarised%>%
  ungroup()%>%
  mutate(VE = factor(VE),
         turnover = factor(turnover),
         strategy = "Healthcare workers",
         mean = mean,
         lower = lower,
         upper = upper)%>%
  rename(outcome = measure)%>%
  filter(VE %in% vec_levels_VE,
         outcome == "dalysAverted",
         cov_vacc == 0.6,
         turnover == 0.15)%>%
  mutate(outcome = "dalysAvertedPerDose")%>%
  mutate(label = paste0(round(signif(mean,3),2),"\n(",round(signif(lower,3),2)," - ",round(signif(upper,3),2),")"))%>%
  mutate(context = "Healthcare facility-targeted vaccination",
         strategy = "Healthcare workers")%>%
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


p_dalysAvertedPerDose_byRisk_alt = df_vacc_dalysAverted_risk_HCWs%>%
  mutate(VE = factor(VE,
                     levels = vec_levels_VE,
                     labels = vec_labels_VE),
  )%>%
  mutate(strategy = case_when(strategy == "Adolescents and adults\n(15-49)" ~ "Adolescents\nand adults\n(15-49)",
                              strategy == "Healthcare workers" ~ "HCWs\n",
                              T ~ strategy))%>%
  ggplot(., aes(x = VE, y = strategy, fill = mean, label = label))+
  geom_tile()+
  theme_classic()+
  xlab("Vaccine efficacy")+ylab("Vaccine target group\n")+
  scale_fill_gradientn("Cumulative DALYs averted\nper 1,000 doses", 
                       colours = vec_cols_dalysPerDose, limits = c(0, 10))+
  geom_text(size = 8)+
  theme(legend.position = "bottom")+
  facet_grid(rows = vars(context), scale = "free_y", space = "free_y")+
  theme(strip.background = element_blank(),
        strip.text = element_blank())

if(save_plots){
  ggsave(p_dalysAvertedPerDose_byRisk, file = paste0("p_dalysAvertedPerDose_main.png"),
         width = 13, height = 12, units = "cm")
  
  ggsave(p_dalysAvertedPerDose_byRisk, file = paste0("p_dalysAvertedPerDose_main.pdf"),
         width = 13, height = 12, units = "cm")
  
  ggsave(p_dalysAvertedPerDose_byRisk_alt+
           theme(axis.text = element_text(size = 18),
                 axis.title = element_text(size = 22),
                 legend.text = element_text(size = 18),
                 legend.title = element_text(size = 22),
                 text = element_text(size = 18), 
                 legend.key.width = unit(dev.size()[1] / 2, "cm")), 
         file = paste0("p_dalysAvertedPerDose_main_bigFont.png"),
         width = 22, height = 22, units = "cm")
  
}


### SENSITIVITY ANALYSIS: HOW EFFICIENCY OUTCOMES VARY WITH VE AND ATTRITION

### compare 2 year, 5 year and 10 year scenarios
df_vacc_outcomesAverted_SA = bind_rows(df_vaccOutcomes_2yr_perDose_summarised%>%
                                         mutate(horizon = "2 years"),
                                       df_vaccOutcomes_5yr_perDose_summarised%>%
                                         mutate(horizon = "5 years"),
                                       df_vaccOutcomes_10yr_perDose_summarised%>%
                                         mutate(horizon = "10 years"))%>%
  mutate(VE = factor(VE))%>%
  filter(measure == "dalysAverted",
         cov_vacc == 0.6,
         turnover %in% c(0.05, 0.15, 0.25))%>%
  mutate(turnover = factor(turnover,
                           levels = c(0.05, 0.15, 0.25),
                           labels = c("5%", "15%", "25%")),
         VE = factor(VE, 
                     levels = c(0.5, 0.7, 0.9),
                     labels = paste0(seq(50,90,by=20), "%")))%>%
  mutate(label = paste0(round(signif(mean,3),2),"\n(",round(signif(lower,3),2)," - ",round(signif(upper,3),2),")"))

p_dalysAvertedPerDose_SA_attrition = df_vacc_outcomesAverted_SA%>%
  mutate(horizon = factor(horizon, 
                          levels = c("2 years", "5 years", "10 years"),
                          labels = c("Duration of protection: 2 years", 
                                     "Duration of protection: 5 years", 
                                     "Duration of protection: 10 years")))%>%
  ggplot(., aes(x = VE, y = turnover, fill = mean, label = label))+
  geom_tile()+
  theme_bw()+
  xlab("Vaccine efficacy")+ylab("Annual HCW attrition rate\n")+
  scale_fill_gradientn("Cumulative DALYs averted\nper 1,000 doses", 
                       colours = vec_cols_dalysPerDose, limits = c(0, 19))+
  geom_text()+
  theme(legend.position = "bottom")+
  facet_wrap(facets = vars(horizon), nrow = 3)


if(save_plots){
  ggsave(p_dalysAvertedPerDose_SA_attrition, file = paste0("p_dalysAvertedPerDose_SA_attrition.png"),
         width = 10, height = 17, units = "cm")
  
  ggsave(p_dalysAvertedPerDose_SA_attrition, file = paste0("p_dalysAvertedPerDose_SA_attrition.pdf"),
         width = 10, height = 17, units = "cm")
}




