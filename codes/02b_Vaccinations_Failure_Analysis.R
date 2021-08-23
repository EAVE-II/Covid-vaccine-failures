# person year calculation

### data for pyeaars calculation
df_cohort_vacc_g_pyrs <- df_cohort_vacc_g

#########
df_cohort_vacc_g_pyrs$ur6_2016_name[is.na(df_cohort_vacc_g_pyrs$ur6_2016_name)]<-"unknown"
chars<-c('Sex','age_gp','prev_positive_status','care_home_elderly',
         'in_hosp_status','simd2020_sc_quintile','ur6_2016_name',
         'EAVE_Smoke','n_risk_gps','n_tests_gp')
event_list<-rbind()
for (i in 1:length(chars)){
  p_years<-pyears(Surv(start_time,end_time,event)~get(chars[i]), scale = 365.25 ,
                  data = df_cohort_vacc_g_pyrs, data.frame = TRUE)
  z<-data.frame(p_years$data$`get(chars[i])`)
  colnames(z)[1]<-"names"
  z1<-data.frame(round((p_years$data$event)*1000/(p_years$data$pyears),1))
  colnames(z1)[1]<-"rate_per_1000_yrs"
  z<-bind_cols(z,z1)
  event_list<-rbind(event_list,z)
}

event_list<-rbind()
for (i in 1:length(chars)){
  p_years<-pyears(Surv(start_time,end_time,event)~get(chars[i]), scale = 365.25, 
                  subset = vacc_type=="AZ", data = df_cohort_vacc_g_pyrs, data.frame = TRUE)
  z<-data.frame(p_years$data$`get(chars[i])`)
  colnames(z)[1]<-"names"
  z1<-data.frame(round((p_years$data$event)*1000/(p_years$data$pyears),1))
  colnames(z1)[1]<-"rate_per_1000_yrs"
  z<-bind_cols(z,z1)
  event_list<-rbind(event_list,z)
}

event_list<-rbind()
for (i in 1:length(chars)){
  p_years<-pyears(Surv(start_time,end_time,event)~get(chars[i]), scale = 365.25, 
                  subset = vacc_type=="PB", data = df_cohort_vacc_g_pyrs, data.frame = TRUE)
  z<-data.frame(p_years$data$`get(chars[i])`)
  colnames(z)[1]<-"names"
  z1<-data.frame(round((p_years$data$event)*1000/(p_years$data$pyears),1))
  colnames(z1)[1]<-"rate_per_1000_yrs"
  z<-bind_cols(z,z1)
  event_list<-rbind(event_list,z)
}


#########
# p years calculation by sex
p_years<-pyears(Surv(start_time,end_time,event)~Sex, scale = 365.25 ,data = df_cohort_vacc_g_pyrs,
                data.frame = TRUE)
summary(p_years)
round((p_years$data$event)*1000/(p_years$data$pyears),1)

#overall rate of event 
sum(p_years$data$event)*1000/sum(p_years$data$pyears)

################
# AZ vaccine
# p years calculation by sex
p_years<-pyears(Surv(start_time,end_time,event)~Sex, scale = 365.25 , subset = vacc_type=="AZ", data = df_cohort_vacc_g_pyrs,
                data.frame = TRUE)
summary(p_years)
round((p_years$data$event)*1000/(p_years$data$pyears),1)

#overall rate of event 
round(sum(p_years$data$event)*1000/sum(p_years$data$pyears),1)

################
# PB vaccine
# p years calculation by sex
p_years<-pyears(Surv(start_time,end_time,event)~Sex, scale = 365.25 , subset = vacc_type=="PB", data = df_cohort_vacc_g_pyrs,
                data.frame = TRUE)
summary(p_years)
round((p_years$data$event)*1000/(p_years$data$pyears),1)

#overall rate of event 
round(sum(p_years$data$event)*1000/sum(p_years$data$pyears),1)

################
# individual comorbidities
df_cohort_vacc_g_pyrs$Q_DIAG_CKD_LEVEL[df_cohort_vacc_g_pyrs$Q_DIAG_CKD_LEVEL>=3]<-"3+"
chars<-c('Q_DIAG_ASTHMA','Q_DIAG_CKD_LEVEL','Q_DIAG_CIRRHOSIS','Q_DIAG_NEURO','Q_DIAG_CCF',
         'Q_DIAG_DIABETES_1','Q_DIAG_DIABETES_2','Q_DIAG_DEMENTIA','Q_DIAG_CHD')
event_list<-rbind()
for (i in 1:length(chars)){
  p_years<-pyears(Surv(start_time,end_time,event)~get(chars[i]), scale = 365.25 ,
                  data = df_cohort_vacc_g_pyrs, data.frame = TRUE)
  z<-data.frame(p_years$data$`get(chars[i])`)
  colnames(z)[1]<-"names"
  z1<-data.frame(round((p_years$data$event)*1000/(p_years$data$pyears),1))
  colnames(z1)[1]<-"rate_per_1000_yrs"
  z<-bind_cols(z,z1)
  z<-mutate(z,category=chars[i])
  event_list<-rbind(event_list,z)
}

event_list<-rbind()
for (i in 1:length(chars)){
  p_years<-pyears(Surv(start_time,end_time,event)~get(chars[i]), scale = 365.25, 
                  subset = vacc_type=="PB",data = df_cohort_vacc_g_pyrs, data.frame = TRUE)
  z<-data.frame(p_years$data$`get(chars[i])`)
  colnames(z)[1]<-"names"
  z1<-data.frame(round((p_years$data$event)*1000/(p_years$data$pyears),1))
  colnames(z1)[1]<-"rate_per_1000_yrs"
  z<-bind_cols(z,z1)
  z<-mutate(z,category=chars[i])
  event_list<-rbind(event_list,z)
}

event_list<-rbind()
for (i in 1:length(chars)){
  p_years<-pyears(Surv(start_time,end_time,event)~get(chars[i]), scale = 365.25, 
                  subset = vacc_type=="AZ",data = df_cohort_vacc_g_pyrs, data.frame = TRUE)
  z<-data.frame(p_years$data$`get(chars[i])`)
  colnames(z)[1]<-"names"
  z1<-data.frame(round((p_years$data$event)*1000/(p_years$data$pyears),1))
  colnames(z1)[1]<-"rate_per_1000_yrs"
  z<-bind_cols(z,z1)
  z<-mutate(z,category=chars[i])
  event_list<-rbind(event_list,z)
}

# # CI for events in first 14 days
# p_years<-pyears(Surv(start_time,end_time,event)~Sex, data = df_cohort_vacc_g_pyrs_f14,
#                 data.frame = TRUE)
# summary(p_years)+
# sum(p_years$data$event)*1000/sum(p_years$data$pyears)