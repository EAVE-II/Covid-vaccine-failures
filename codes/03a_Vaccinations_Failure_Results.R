##########################################################
# Name of file: 01a_Vaccinations_Input.R
# Original author(s): Utkarsh Agrawal
# Description of content: Results of vaccination failure cohort 
# Approximate run time: Unknown
##########################################################

# tables for the paper
##### Summary table - counts #####

# Entire cohort 
#z<-df_cohort_vacc
z<-filter(df_cohort_vacc_g,event==1)
z <- filter(df_cohort_vacc_g, !duplicated(EAVE_LINKNO))
z$event<-as.factor(z$event)
explanatory <- c("Sex", "ageYear", "age_gp","prev_positive_status", "care_home_elderly",
                 "in_hosp_status","simd2020_sc_quintile","ur6_2016_name","n_risk_gps",
                 "EAVE_Smoke","n_tests_gp")
dependent <- "event"

# Vaccine vs not vaccine
z %>%
  mutate(vacc1 = as.character(event)) %>%
  summary_factorlist(dependent, explanatory)

z<-filter(df_cohort_vacc_g1,event==1&vacc_type=="AZ")
z$event<-as.factor(z$event)
z %>%
  mutate(vacc1 = as.character(event)) %>%
  summary_factorlist(dependent, explanatory)
summary(z$ageYear)

z<-filter(df_cohort_vacc_g1,event==1&vacc_type=="PB")
z$event<-as.factor(z$event)
z %>%
  mutate(vacc1 = as.character(event)) %>%
  summary_factorlist(dependent, explanatory)
summary(z$ageYear)
  
###################
# tables for all the vaccinated individuals
addmargins(table(df_cohort_vacc$prev_pos_test))
round(100*addmargins(prop.table(table(df_cohort_vacc$prev_pos_test))),1)

df_cohort_vacc$ur6_2016_name[is.na(df_cohort_vacc$ur6_2016_name)]<-"Unknown"
df_cohort_vacc$Q_DIAG_CKD_LEVEL[df_cohort_vacc$Q_DIAG_CKD_LEVEL>=3]<-"3+"
#round(100*addmargins(prop.table(table(df_cohort_vacc$bmi_cat))),1)

addmargins(table(df_cohort_vacc$Q_DIAG_ASTHMA))
round(100*addmargins(prop.table(table(df_cohort_vacc$Q_DIAG_ASTHMA))),1)
addmargins(table(df_cohort_vacc$Q_DIAG_CKD_LEVEL))
round(100*addmargins(prop.table(table(df_cohort_vacc$Q_DIAG_CKD_LEVEL))),1)
addmargins(table(df_cohort_vacc$Q_DIAG_CIRRHOSIS))
round(100*addmargins(prop.table(table(df_cohort_vacc$Q_DIAG_CIRRHOSIS))),1)
addmargins(table(df_cohort_vacc$Q_DIAG_NEURO))
round(100*addmargins(prop.table(table(df_cohort_vacc$Q_DIAG_NEURO))),1)
addmargins(table(df_cohort_vacc$Q_DIAG_CCF))
round(100*addmargins(prop.table(table(df_cohort_vacc$Q_DIAG_CCF))),1)
addmargins(table(df_cohort_vacc$Q_DIAG_DIABETES_1))
round(100*addmargins(prop.table(table(df_cohort_vacc$Q_DIAG_DIABETES_1))),1)
addmargins(table(df_cohort_vacc$Q_DIAG_DIABETES_2))
round(100*addmargins(prop.table(table(df_cohort_vacc$Q_DIAG_DIABETES_2))),1)
addmargins(table(df_cohort_vacc$Q_DIAG_DEMENTIA))
round(100*addmargins(prop.table(table(df_cohort_vacc$Q_DIAG_DEMENTIA))),1)
addmargins(table(df_cohort_vacc$Q_DIAG_CHD))
round(100*addmargins(prop.table(table(df_cohort_vacc$Q_DIAG_CHD))),1)
###############
# tables for all the non-events in vaccinated individuals
df_cohort_vacc_g_table<-filter(df_cohort_vacc_g, event==1)
df_cohort_vacc_g_table<-filter(df_cohort_vacc_g_table, !duplicated(EAVE_LINKNO))

# tables for all the non-events in vaccinated individuals
z<-filter(df_cohort_vacc_g, !(EAVE_LINKNO %in% df_cohort_vacc_g_table$EAVE_LINKNO))
z<-filter(z, !duplicated(EAVE_LINKNO))
df_cohort_vacc_g_table<-z
rm(z)

# plot for proportion of events by dates (number of cases are low)
z<-data.frame(seq(as.Date("2020-12-09"),as.Date("2021-03-05"),by="day"))
colnames(z)[1] <- "weekly_dates"
vis_data<-filter(df_cohort_vacc,event_all==1)
vis_data<-data.frame(table(vis_data$date_vacc_1,vis_data$event_all))
colnames(vis_data)[1]<-"weekly_dates"
vis_data$weekly_dates<-as.Date(vis_data$weekly_dates)
vis_data<-left_join(z,vis_data,by="weekly_dates")
vis_data$Freq[is.na(vis_data$Freq)]<-0
colnames(vis_data)[3]<-"frequency_of_events"
vis_data$Var2<-NULL

vis_data1<-filter(df_cohort_vacc,event_all==0)
vis_data1<-data.frame(table(vis_data1$date_vacc_1,vis_data1$event_all))
colnames(vis_data1)[1]<-"weekly_dates"
vis_data1$weekly_dates<-as.Date(vis_data1$weekly_dates)
vis_data1<-left_join(z,vis_data1,by="weekly_dates")
vis_data1$Freq[is.na(vis_data1$Freq)]<-0
colnames(vis_data1)[3]<-"frequency_of_no_events"
vis_data1$Var2<-NULL

vis_data<-left_join(vis_data,vis_data1,by="weekly_dates")
vis_data<-mutate(vis_data,prop_events=round(100*frequency_of_events/(frequency_of_no_events+frequency_of_events),3))
rm(vis_data1)

ggplot(vis_data, aes(x = weekly_dates, y = Freq, fill = Freq)) +
  geom_bar(stat="identity",fill="steelblue") +
  theme_minimal()+
  labs(title = "Hospitalisation or mortality per day",
       x = "Date of event (2020-21)", y="Number of events")

# # plot for number of events by dates (number of cases are low)
# z<-data.frame(seq(as.Date("2020-12-09"),as.Date("2021-03-05"),by="day"))
# colnames(z)[1] <- "weekly_dates"
# vis_data<-filter(df_cohort_vacc,event_all==1)
# vis_data<-data.frame(table(vis_data$date_vacc_1,vis_data$event_all))
# colnames(vis_data)[1]<-"weekly_dates"
# vis_data$weekly_dates<-as.Date(vis_data$weekly_dates)
# vis_data<-left_join(z,vis_data,by="weekly_dates")
# vis_data$Freq[is.na(vis_data$Freq)]<-0
# ggplot(vis_data, aes(x = weekly_dates, y = Freq, fill = Freq)) +
#   geom_bar(stat="identity",fill="steelblue") +
#   theme_minimal()+
#   labs(title = "Hospitalisation or mortality per day",
#        x = "Date of event (2020-21)", y="Number of events")

# rate ratio plot

###############