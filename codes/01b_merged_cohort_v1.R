##########################################################
# Name of file: 01a_Vaccinations_Input.R
# Original author(s): Utkarsh Agrawal
# Description of content: reads in the cohort and merges in vaccination data 
# Approximate run time: Unknown
##########################################################
df_cohort_vacc_g <- readRDS(paste0(Location,"/EAVE/GPanalysis/progs/UA/first_dose_failure/data/df_cohort_coxph_22-3-2021.RDS"))
df_cohort <- readRDS(paste0(Location,"/EAVE/GPanalysis/progs/UA/first_dose_failure/data/df_cohort_ve_failure_22-3-2021.RDS"))
df_cohort_vacc <- readRDS(paste0(Location,"/EAVE/GPanalysis/progs/UA/first_dose_failure/data/df_cohort_ve_failure_vacc_22-3-2021.RDS"))

rm(any_death,EAVE_Weights,rg,z)

# EAVE hospitalisations and death (composite outcome)
covid_hosp_death <- covid_death %>%
  dplyr::rename(NRS.Date.Death= covid_death_date) %>% # 
  full_join(covid_hospitalisations) %>%
  dplyr::rename(hosp_admission_date = admission_date) %>%
  # If hospital admission happens before death then put death date
  # Fill in the remaining with the non-NA dates
  mutate(admission_date = if_else(hosp_admission_date < NRS.Date.Death, hosp_admission_date,
                                  NRS.Date.Death)) %>%
  mutate(admission_date = if_else(is.na(hosp_admission_date), NRS.Date.Death,
                                  hosp_admission_date)) %>%
  mutate(outcome_date = if_else(hosp_admission_date < NRS.Date.Death, "hosp",
                                "death")) %>%
  mutate(outcome_date = if_else(is.na(hosp_admission_date), "death",
                                "hosp"))

# Find end date according to admission date 
a_end <- max(covid_hosp_death$admission_date) # This is the 7th of April

## Vaccination data
# Make anyone vaccinated after the maximum endpoint time unvaccinated
z_vaccinations <- filter(Vaccinations, 
                         date_vacc_1 <= a_end) %>% 
  mutate(vacc_type_2 = if_else(date_vacc_2 >= a_end, NA_character_ , vacc_type_2),
         date_vacc_2 = as.Date(ifelse(date_vacc_2 >= a_end, NA, date_vacc_2), origin=as.Date("1970-01-01")) )

## Vaccination data cohort
# Get the vaccinated individuals in the time period by linking characteristics from df_cohort
z_df <- df_cohort %>%
  left_join(select(z_vaccinations, EAVE_LINKNO, date_vacc_1)) %>%
  filter(!is.na(date_vacc_1)) %>%
  mutate(ur6_2016_name = replace_na(ur6_2016_name, "NA")) %>%
  dplyr::rename(EAVE_LINKNO_vacc = EAVE_LINKNO)


## Whole cohort - vaccination status
z_chrt <- df_cohort %>%
  dplyr::rename(EAVE_LINKNO_uv = EAVE_LINKNO) %>%
  mutate(ur6_2016_name = replace_na(ur6_2016_name, "NA")) %>%
  dplyr::mutate(vacc=if_else(is.na(date_vacc_1),0,1))
  # Create vacc flag if EAVE ID = Vacc EAVE ID
#left_join(select(z_vaccinations, EAVE_LINKNO, date_vacc_1),
#            by=c("EAVE_LINKNO_uv" = "EAVE_LINKNO")) %>%


## Cohort household information (elderly care home indicator linkage)
Cohort_Household <- readRDS("/conf/EAVE/GPanalysis/outputs/temp/Cohort_Household.rds") %>%
  mutate(n_hh_gp = cut(n_hh, breaks=c(0,1,2,5,10,30,100,max(n_hh)),
                       labels=c("1", "2", "3-5", "6-10", "11-30", "31-100", "101+")))%>% 
  mutate(ave_hh_age=if_else(is.na(ave_hh_age), mean(ave_hh_age, na.rm=T), ave_hh_age) )

# Link in household information
z_chrt <- z_chrt %>%
  left_join(select(Cohort_Household, EAVE_LINKNO,
                   n_hh_gp, ave_hh_age, care_home_elderly,n_hh_w_65),
            by= c("EAVE_LINKNO_uv"= "EAVE_LINKNO"))

z_df <- z_df %>%
  left_join(select(Cohort_Household, EAVE_LINKNO,
                   n_hh_gp, ave_hh_age, care_home_elderly),
            by= c("EAVE_LINKNO_vacc"= "EAVE_LINKNO"))

z_df <- df_cohort_vacc %>%
  left_join(select(Cohort_Household, EAVE_LINKNO,
                   n_hh_gp, ave_hh_age, care_home_elderly),
            by= "EAVE_LINKNO")


# Total cohort 
z_chrt_desc <- df_cohort %>%
  dplyr::rename(EAVE_LINKNO_uv = EAVE_LINKNO) %>%
  mutate(ur6_2016_name = replace_na(ur6_2016_name, "NA")) %>%
  # Add in age group
  mutate(age_grp = case_when(ageYear < 65 ~"18-64", 
                             ageYear < 80 ~"65-79",
                             TRUE ~ "80+"))

# Eliminate care homes
z_chrt_desc <- z_chrt_desc %>%
  left_join(select(Cohort_Household, EAVE_LINKNO,
                   n_hh_gp, ave_hh_age, care_home_elderly),
            by= c("EAVE_LINKNO_uv"= "EAVE_LINKNO")) %>% 
  filter(care_home_elderly == 0)%>% 
  dplyr::select(-care_home_elderly)


# Add in vaccination data
z_chrt_desc <- z_chrt_desc %>%
  mutate(dose_no = case_when((vacc_type %in% c("PB", "AZ") & vacc_type_2 %in% c("PB", "AZ")) ~ 2,
                             vacc_type %in% c("PB", "AZ") ~ 1,
                             TRUE ~ 0)) %>%
  mutate(v1_v2_days = date_vacc_2 - date_vacc_1) %>%
  mutate(vacc1 = if_else(vacc_type %in% c("PB", "AZ"), 1,0))  %>%
  mutate(vacc2 = if_else(vacc_type_2 %in% c("PB", "AZ"), 1,0))

# Link in event data
z_chrt_desc <- z_chrt_desc %>%
  left_join(z_event, by=c("EAVE_LINKNO_uv" = "EAVE_LINKNO"))


# # Link in positive test data
# z_chrt_desc <- z_chrt_desc %>%
#   select(-SpecimenDate) %>%
#   left_join(positive_test %>%
#               select(SpecimenDate, EAVE_LINKNO), by=c("EAVE_LINKNO_uv" = "EAVE_LINKNO"))
# 
# colnames(z_chrt_desc)

# Make event indicators post vaccination and overall for uv - Come bac
z_chrt_desc <- z_chrt_desc %>%
  mutate(event = ifelse(admission_date <= date_vacc_1, "0", "1")) %>%
  mutate(event = ifelse(is.na(date_vacc_1) & !is.na(admission_date), "1", event)) %>%
  mutate(event = replace_na(event, "0")) %>%
  #mutate(time_to_event =  as.numeric(admission_date-date_vacc_1)) %>%
  # mutate(period = cut(time_to_event, 
  #                    breaks= c(-1,13,20,27,34,41, 48, 55, 62, 69, 76, 83, max(time_to_event, na.rm=T)),
  #                    labels=c("0:13","14:20","21:27","28:34","35:41", "42:47", "49:55", "56:62", "63:69", 
  #                             "70:76", "77:83", "84+"))) 
  mutate(event_hosp = ifelse(hosp_admission_date <= date_vacc_1,"0", "1")) %>%
  mutate(event_hosp = ifelse(is.na(date_vacc_1) & !is.na(hosp_admission_date), "1", event_hosp)) %>%
  mutate(event_hosp = replace_na(event_hosp, "0")) %>%
  mutate(event_death = ifelse(NRS.Date.Death.y <= date_vacc_1,"0", "1")) %>%
  mutate(event_death = ifelse(is.na(date_vacc_1) & !is.na(NRS.Date.Death.y), "1", event_death)) %>%
  mutate(event_death = replace_na(event_death, "0"))# %>%
#select(event, admission_date, date_vacc_1, vacc1, vacc_type)


##### Summary table - weights ####
explanatory <- c("Sex", "ageYear", "age_grp", "simd2020_sc_quintile", "ur6_2016_name", "n_risk_gps",
                 "EAVE_Smoke", "EAVE_BP", "HB", "ave_hh_age", "n_hh_gp")
dependent <- "vacc1"

summary_factorlist_wt <- function(data, dependent, explanatory){
  # Create list to put in summaries into each element
  summary_tbl_list <- list()
  
  for(i in 1:length(explanatory)){
    
    # Extract variable
    n <- data %>%
      pull(!!sym(explanatory[i]))
    
    # If numeric then make weighted mean
    if(is.numeric(n)) {
      z_mean <- data %>%
        group_by(!!sym(dependent)) %>%
        summarise(mean = round(weighted.mean(!!sym(explanatory[i]), w = eave_weight),1),
                  sd = round(sqrt(spatstat.geom::weighted.var(!!sym(explanatory[i]), w = eave_weight)),1)) %>%
        mutate(mean.sd = paste0(mean, " (",sd,")")) %>%
        select(-mean, -sd) %>%
        mutate("characteristic" = explanatory[i]) %>%
        pivot_wider(names_from = !!sym(dependent), values_from = mean.sd) %>%
        #relocate(characteristic) %>%
        mutate(levels = "mean.sd")
      
      
      z_median <- data %>%
        group_by(!!sym(dependent)) %>%
        summarise(median = spatstat.geom::weighted.median(!!sym(explanatory[i]), w = eave_weight),
                  q1 = spatstat.geom::weighted.quantile(!!sym(explanatory[i]), w = eave_weight, probs = 0.25),
                  q3 = spatstat.geom::weighted.quantile(!!sym(explanatory[i]), w = eave_weight, probs = 0.75)) %>%
        mutate("characteristic" = explanatory[i]) %>%
        mutate(iqr = q3 -q1) %>%
        mutate(median.iqr = paste0(median, " (",iqr,")")) %>%
        select(-q1, -q3, -median, -iqr) %>%
        pivot_wider(names_from = !!sym(dependent), values_from = median.iqr) %>%
        #relocate(characteristic) %>%
        mutate(levels = "median.iqr")
      
      # Combine!!
      summary_tbl_list[[i]] <- full_join(z_mean, z_median)
      
      
      # Else get sum of weights of each level
    } else {
      
      summary_tbl_list[[i]] <- data %>%
        group_by(!!sym(explanatory[i]), !!sym(dependent)) %>%
        summarise(n = sum(eave_weight)) %>%
        ungroup() %>%
        group_by(!!sym(dependent)) %>%
        mutate(perc = sprintf("%.1f",round(n/sum(n)*100,1))) %>%
        mutate(n_perc = paste0(round(n,0), " (", perc,"%)")) %>%
        select(-n, -perc) %>%
        pivot_wider(names_from = !!sym(dependent), values_from = n_perc) %>%
        rename("levels"=explanatory[i])
        #mutate("characteristic" = explanatory[i]) 
        #relocate(characteristic)
      
    }
    
    
    
  }
  
  # Combine list together to make dataset
  summary_tbl_wt <- summary_tbl_list %>%
    reduce(full_join)
  
  summary_tbl_wt
}

explanatory <- c("Sex", "ageYear", "age_grp", "simd2020_sc_quintile", "ur6_2016_name", "n_risk_gps",
                 "EAVE_Smoke", "EAVE_BP", "HB", "ave_hh_age", "n_hh_gp")
dependent <- "vacc1"
z_chrt_desc1<-filter(z_chrt_desc,event==1&date_vacc_1 < admission_date)
z_chrt_desc %>%
  mutate(vacc1 = as.character(vacc1)) %>%
  summary_factorlist(dependent, explanatory)
##############################
