##########################################################
# Name of file: 01a_Vaccinations_Input.R
# Original author(s): Utkarsh Agrawal
# Description of content: Results of vaccination failure cohort 
# Approximate run time: Unknown
##########################################################
# data prep
###################
z<-filter(df_cohort_vacc_g_orig,event==1)
z<-select(z,EAVE_LINKNO,event)
z <- filter(z, !duplicated(EAVE_LINKNO))
df_cohort_vacc<-left_join(df_cohort_vacc,z)
###
# bar chart
df_cohort_vacc_table<-filter(df_cohort_vacc, event==1)
#df_cohort_vacc_table<-filter(df_cohort_vacc_table, !duplicated(EAVE_LINKNO))
#df_cohort_vacc_table$prev_pos_test<-as.character(df_cohort_vacc_table$prev_pos_test)

# df_cohort_vacc_table<- df_cohort_vacc_g_orig %>%
#   dplyr::select(EAVE_LINKNO, Sex, age_gp,simd2020_sc_quintile, ur6_2016_name, n_risk_gps,EAVE_Smoke, 
#                 EAVE_BP,prev_pos_test,vacc_type,event_all)
# df_cohort_vacc_table<-distinct(df_cohort_vacc_table,EAVE_LINKNO,.keep_all = TRUE)
# df_cohort_vacc_table<-filter(df_cohort_vacc_table, event_all==1)

z_df <- df_cohort_vacc_table %>%
  dplyr::select(Sex, age_gp,simd2020_sc_quintile, ur6_2016_name, n_risk_gps,EAVE_Smoke, 
                EAVE_BP,vacc_type) %>% 
  pivot_longer(cols=Sex:EAVE_BP) 

z <- z_df %>% group_by(name, value) %>% 
  dplyr::summarise( both_vacc_event = sum(!is.na(vacc_type))) %>%  ungroup()

z <- z %>% group_by(name) %>% 
  dplyr::mutate( total_event = sum(both_vacc_event)) %>%  ungroup() %>%
  mutate(Percent_of_event = round(both_vacc_event/total_event*100,1))

## all the vaccinated people
df_cohort_vacc_table<-df_cohort_vacc
#df_cohort_vacc_table<-filter(df_cohort_vacc_table, !duplicated(EAVE_LINKNO))

z_df <- df_cohort_vacc_table %>%
  dplyr::select(Sex, age_gp,simd2020_sc_quintile, ur6_2016_name, n_risk_gps,EAVE_Smoke, 
                EAVE_BP,vacc_type) %>% 
  pivot_longer(cols=Sex:EAVE_BP) 

z1 <- z_df %>% group_by(name, value) %>% 
  dplyr::summarise( both_vacc_event = sum(!is.na(vacc_type))) %>%  ungroup()

z1 <- z1 %>% group_by(name) %>% 
  dplyr::mutate( total_event = sum(both_vacc_event)) %>%  ungroup() %>%
  mutate(Percent_vaccinated = round(both_vacc_event/total_event*100,1))
  
z1 <- select(z1, name, value,Percent_vaccinated)
rm(z_df)

bar_chart_data <- z %>% select(name, value,Percent_of_event) %>%
  left_join(z1,by=c("name","value"))
bar_chart_data <- gather(bar_chart_data,"category","percentage",Percent_of_event:Percent_vaccinated)

categories<-unique(bar_chart_data$name)
categories_name<-c("Age","blood Pressure","Smoking Status","No. of risk groups",
                   "Sex", "SIMD","Urban/rural classificaiton")
for (i in 1:length(categories)){
  z <- filter(bar_chart_data,name==categories[i])
  z$category <- factor(z$category,levels = c("Percent_vaccinated","Percent_of_event"))
  print(ggplot(data=z, aes(x=value,y=percentage,fill=category)) +
    geom_bar(stat="identity",position = position_dodge())+
    theme_minimal()+
    ylab("Proportion")+
    xlab(categories_name[i])+
    ggtitle("Proportion comparison plot"))
}

#################
## original code
# z <- z_df %>% group_by(name, value) %>% 
#   dplyr::summarise(N=round(sum(eave_weight)), Vaccinated = sum(vacc), 
#                    AZ = sum(vacc_type=="AZ"), PB = sum(vacc_type=="PB")) %>% 
#   mutate(Unvaccinated=N-Vaccinated, Uptake = Vaccinated/N*100) %>% ungroup()
# #z <- z %>%  group_by(name) %>% mutate(RR=Uptake/first(Uptake)) %>% ungroup() %>% as.data.frame()
# z2 <- z %>% group_by(name) %>% dplyr::summarise(N_U = sum(Unvaccinated), N_V=sum(Vaccinated), 
#                                                 N_AZ = sum(AZ), N_PB=sum(PB), F=first(Uptake))
# z <- z %>% left_join(z2, by="name") %>% 
#   mutate(Percent_of_Vaccinated = round(Vaccinated/N_V*100,1), 
#          Percent_of_Unvaccinated = round(Unvaccinated/N_U*100,1),
#          Percent_of_AZ = round(AZ/N_AZ*100,1),
#          Percent_of_PB = round(PB/N_PB*100,1),
#          RR=Uptake/F)
# z <- z %>% dplyr::select(name, value, Vaccinated, Percent_of_Vaccinated, Unvaccinated, Percent_of_Unvaccinated,
#                          Uptake, RR, AZ, Percent_of_AZ, PB, Percent_of_PB) %>% ungroup() %>% as.data.frame()
