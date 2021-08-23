##########################################################
# Name of file: 01a_Vaccinations_Input.R
# Original author(s): Utkarsh Agrawal
# Description of content: Analysis of vaccination failure cohort 
# Approximate run time: Unknown
##########################################################

# 01 Setup ####
#Libraries
library(plyr)
library(tidyverse)
library(survival)
#library(dplyr)
#library(mgcv)
library(tidyr)
library(ggplot2)

Positive_Tests <- readRDS(paste0(Location,"EAVE/GPanalysis/data/Positive_Tests.RDS"))
Positive_Tests <- filter(Positive_Tests, !duplicated(EAVE_LINKNO))
##################
# data prep for the analysis

# # add previous positive test
# z <- select(df_cohort,EAVE_LINKNO,date_vacc_1)
# z <- left_join(Positive_Tests,z,by="EAVE_LINKNO")
# z <- filter(z, specimen_date < date_vacc_1)
# z <- mutate(z, prev_pos_test = 1)
# z <- select(z,EAVE_LINKNO,prev_pos_test)
# df_cohort_vacc_g<-left_join(df_cohort_vacc_g,z,by="EAVE_LINKNO")
# df_cohort_vacc_g$prev_pos_test[is.na(df_cohort_vacc_g$prev_pos_test)]<-0L

######### Modelling
# crude analysis
# sex
# pyears - rate of event per year
# glm_poisson - log rate ratios

# vacc period
p_years<-pyears(Surv(start_time,end_time,event)~date_period, data = df_cohort_vacc_g,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears)) +date_period,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

# sex
p_years<-pyears(Surv(start_time,end_time,event)~Sex, data = df_cohort_vacc_g,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears)) +Sex,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

# age cat
p_years<-pyears(Surv(start_time,end_time,event)~age_gp, data = df_cohort_vacc_g,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears)) + age_gp,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

# previous covid pos test
p_years<-pyears(Surv(start_time,end_time,event)~prev_positive_status, data = df_cohort_vacc_g,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears)) + prev_positive_status,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

# previous history of hospitalisation
p_years<-pyears(Surv(start_time,end_time,event)~in_hosp_status, data = df_cohort_vacc_g,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears)) + in_hosp_status,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

# elderly care home
p_years<-pyears(Surv(start_time,end_time,event)~care_home_elderly, data = df_cohort_vacc_g,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears)) +care_home_elderly,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

# SIMD
p_years<-pyears(Surv(start_time,end_time,event)~SIMD, data = df_cohort_vacc_g,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears)) + SIMD,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

#urban/rural
p_years<-pyears(Surv(start_time,end_time,event)~ur_combined, data = df_cohort_vacc_g,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears)) + ur_combined,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

# # HB
# p_years<-pyears(Surv(start_time,end_time,event)~HB, data = df_cohort_vacc_g,
#                 data.frame = TRUE)
# summary(p_years)
# glm_pos <- glm(event~offset(log(pyears)) + HB,family=poisson,data=p_years$data)
# summary(glm_pos)
# round(exp(glm_pos$coefficients),2)
# round(exp(confint(glm_pos)),2)

# smoking status
# "Non Smoker"<-0
# "Smoker"<-1
# "Ex Smoker"<-2
# "Unknown"<-3
# df_cohort_vacc_g1$EAVE_Smoke <- as.factor(df_cohort_vacc_g1$EAVE_Smoke)
# df_cohort_vacc_g1$EAVE_Smoke <- relevel(df_cohort_vacc_g1$EAVE_Smoke, ref = "Non Smoker")
df_cohort_vacc_g$EAVE_Smoke_n <- as.factor(df_cohort_vacc_g$EAVE_Smoke_n)
#df_cohort_vacc_g$EAVE_Smoke_n <- relevel(df_cohort_vacc_g$EAVE_Smoke_n, ref = 0)
p_years<-pyears(Surv(start_time,end_time,event)~EAVE_Smoke_n, data = df_cohort_vacc_g,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears)) + EAVE_Smoke_n,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

# BMI
df_cohort_vacc_g1<-df_cohort_vacc_g %>% filter(!(bmi_cat=="1"))
p_years<-pyears(Surv(start_time,end_time,event)~bmi_cat, data = df_cohort_vacc_g1,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears)) + bmi_cat,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

# number risk groups
p_years<-pyears(Surv(start_time,end_time,event)~n_risk_gps, data = df_cohort_vacc_g,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears)) + n_risk_gps,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

# number of previous tests
p_years<-pyears(Surv(start_time,end_time,event)~n_tests_gp, data = df_cohort_vacc_g,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears)) + n_tests_gp,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

round(exp(glm_pos$coefficients-coef(summary(glm_pos))[,2]*1.96),2)
round(exp(glm_pos$coefficients+coef(summary(glm_pos))[,2]*1.96),2)
# exp(1.11-0.1346*1.96)
# exp(1.11+0.1346*1.96)

###########

# Asthma
rate_ratios<-rbind()
# comorbidities <- c('Q_DIAG_ASTHMA', 'Q_DIAG_CKD_LEVEL', 'Q_DIAG_CIRRHOSIS')
# for (i in 1:length(comorbidities)){
#   p_years<-pyears(Surv(start_time,end_time,event)~get(comorbidities[i])+Sex+age_gp+SIMD, data = df_cohort_vacc_g,
#                   data.frame = TRUE)
#   glm_pos <- glm(event~offset(log(pyears))+get(comorbidities[i])+Sex+age_gp+SIMD,family=poisson,data=p_years$data)
#   #summary(glm_pos)
#   #coef(summary(glm_pos))[2,4]
#   #round(exp(glm_pos$coefficients[2]),2)
#   #round(exp(confint(glm_pos)[2]),2)
#   #round(exp(confint(glm_pos)[11]),2)
#   
#   tt<-paste(round(exp(glm_pos$coefficients[2]),2),"(",round(exp(confint(glm_pos)[2]),2),"-",
#             round(exp(confint(glm_pos)[11]),2),", p=",round(coef(summary(glm_pos))[2,4],3)," )")
#   rate_ratios<-rbind(rate_ratios,tt)
# }
  
p_years<-pyears(Surv(start_time,end_time,event)~Q_DIAG_ASTHMA+Sex+age_gp+SIMD, data = df_cohort_vacc_g,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears))+Q_DIAG_ASTHMA+Sex+age_gp+SIMD,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

# CKD (3-5)
df_cohort_vacc_g$Q_DIAG_CKD_LEVEL[df_cohort_vacc_g$Q_DIAG_CKD_LEVEL>=3]<-"3+"
p_years<-pyears(Surv(start_time,end_time,event)~Q_DIAG_CKD_LEVEL+Sex+age_gp+SIMD, data = df_cohort_vacc_g,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears))+Q_DIAG_CKD_LEVEL+Sex+age_gp+SIMD,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

# Liver Cirrhosis
p_years<-pyears(Surv(start_time,end_time,event)~Q_DIAG_CIRRHOSIS+Sex+age_gp+SIMD, data = df_cohort_vacc_g,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears))+Q_DIAG_CIRRHOSIS+Sex+age_gp+SIMD,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

# chronic neurological disease
p_years<-pyears(Surv(start_time,end_time,event)~Q_DIAG_NEURO+Sex+age_gp+SIMD, data = df_cohort_vacc_g,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears))+Q_DIAG_NEURO+Sex+age_gp+SIMD,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

# Congestive Cardiac Failure
p_years<-pyears(Surv(start_time,end_time,event)~Q_DIAG_CCF+Sex+age_gp+SIMD, data = df_cohort_vacc_g,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears))+Q_DIAG_CCF+Sex+age_gp+SIMD,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

# Diabetes type 1
p_years<-pyears(Surv(start_time,end_time,event)~Q_DIAG_DIABETES_1+Sex+age_gp+SIMD, data = df_cohort_vacc_g,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears))+Q_DIAG_DIABETES_1+Sex+age_gp+SIMD,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

# Diabetes type 2
p_years<-pyears(Surv(start_time,end_time,event)~Q_DIAG_DIABETES_2+Sex+age_gp+SIMD, data = df_cohort_vacc_g,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears))+Q_DIAG_DIABETES_2+Sex+age_gp+SIMD,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

# Dementia
p_years<-pyears(Surv(start_time,end_time,event)~Q_DIAG_DEMENTIA+Sex+age_gp+SIMD, data = df_cohort_vacc_g,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears))+Q_DIAG_DEMENTIA+Sex+age_gp+SIMD,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

# Coronary Heart Disease
p_years<-pyears(Surv(start_time,end_time,event)~Q_DIAG_CHD+Sex+age_gp+SIMD, data = df_cohort_vacc_g,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears))+Q_DIAG_CHD+Sex+age_gp+SIMD,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

################
df_cohort_vacc_g_both<-df_cohort_vacc_g
df_cohort_vacc_g <- filter(df_cohort_vacc_g_both,vacc_type=="AZ")

################
df_cohort_vacc_g <- filter(df_cohort_vacc_g_both,vacc_type=="PB")

################
# models tried with Chris

# age adjusted vaccination type
p_years<-pyears(Surv(start_time,end_time,event)~age_gp+vacc_type, data = df_cohort_vacc_g,
                data.frame = TRUE)
#summary(p_years)
glm_pos <- glm(event~offset(log(pyears)) + age_gp+vacc_type,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

# vaccinaiton period
p_years<-pyears(Surv(start_time,end_time,event)~date_period, data = df_cohort_vacc_g,
                data.frame = TRUE)
glm_pos <- glm(event~offset(log(pyears)) +date_period,family=poisson,data=p_years$data)
#summary(glm_pos)
round(exp(glm_pos$coefficients),2)
#round(exp(confint(glm_pos)),2)

# age adjusted vaccinaiton period
p_years<-pyears(Surv(start_time,end_time,event)~age_gp + date_period, data = df_cohort_vacc_g,
                data.frame = TRUE)
summary(p_years)
glm_pos <- glm(event~offset(log(pyears)) + age_gp + date_period,family=poisson,data=p_years$data)
summary(glm_pos)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

# adjusted rate ratio analysis
# pyears - rate of event per year
# glm_poisson - log rate ratios
p_years<-pyears(Surv(start_time,end_time,event)~date_period+Sex+age_gp+n_risk_gps, data = df_cohort_vacc_g,
                data.frame = TRUE)
glm_pos <- glm(event~offset(log(pyears)) +date_period+Sex+age_gp+n_risk_gps,family=poisson,data=p_years$data)

##########################
# plotting codes
z<-data.frame(exp(glm_pos$coefficients),exp(confint(glm_pos)))
# plot rate ratio
z<-z[2:nrow(z),]
z1<-rownames(z)
rr_plot_data <- data.frame(yAxis = length(z1):1, boxRR = z[,1], 
                           boxCILow = z[,2], boxCIHigh = z[,3])

# ggplot(rr_plot_data,aes(x = boxRR, y = z1))+
#   geom_vline(aes(xintercept = 0),size=0.25,linetype="dashed")+
#   geom_errorbarh(aes(xmax=boxCIHigh, xmin=boxCILow),size=0.5, height=0.2, color="gray50")+
#   geom_point(size=3.5, color="orange")+
#   theme_bw() + 
#   theme(panel.grid.minor = element_blank())+
#   scale_x_continuous(breaks = log10(seq(0.1,2.5,0.1)),
#                      labels=seq(0.1,2.5,0.1),limits = log10(c(0.09,2.5)))+
#   coord_trans(x=scales:::exp_trans(10))+
#   ylab("Confounders")+
#   xlab("Rate Ratio")+
#   annotate(geom="text", y=1.1, x=log10(1.5),label="Rate ratio plot",size=3.5,hjust=0)+
#   ggtitle("Rate Ratio plot")

ggplot(rr_plot_data,aes(x = boxRR, y = z1))+
  geom_vline(aes(xintercept = 1),size=0.25,linetype="dashed")+
  geom_errorbarh(aes(xmax=boxCIHigh, xmin=boxCILow),size=0.5, height=0.2, color="gray50")+
  geom_point(size=3.5, color="orange")+
  theme_bw() + 
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = seq(-1,11,0.5),
                     labels=seq(-1,11,0.5),limits = c(-1,11))+
  ylab("Confounders")+
  xlab("Rate Ratio")+
  annotate(geom="text", y=1.1, x=7.5,label="Rate ratio plot",size=3.5,hjust=0)+
  ggtitle("Rate Ratio plot")

# adjusted rate ratio analysis
# pyears - rate of event per year
# glm_poisson - log rate ratios
p_years<-pyears(Surv(start_time,end_time,event)~date_period+Sex+age_gp+n_risk_gps+simd2020_sc_quintile, data = df_cohort_vacc_g,
                data.frame = TRUE)
glm_pos <- glm(event~offset(log(pyears)) +date_period+Sex+age_gp+n_risk_gps+simd2020_sc_quintile,family=poisson,data=p_years$data)
exp(glm_pos$coefficients)

# adjusted rate ratio analysis
# pyears - rate of event per year
# glm_poisson - log rate ratios
# removed Q_DIAG_PARKINSONS +Q_DIAG_FRACTURE + Q_DIAG_SEV_MENT_ILL + Q_DIAG_AF + 
# Q_DIAG_SICKLE_CELL  + Q_DIAG_CONGEN_HD + Q_DIAG_CEREBRALPALSY + Q_DIAG_EPILEPSY +
#  Q_DIAG_STROKE + Q_DIAG_PULM_HYPER + Q_DIAG_PVD + Q_DIAG_RA_SLE + 
df_cohort_vacc_g1<-df_cohort_vacc_g %>% filter(!(bmi_cat=="1"))
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(HB=="S08000026"))
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(HB=="S08000025"))
#df_cohort_vacc_g1$SIMD <- as.factor(df_cohort_vacc_g1$SIMD)
#df_cohort_vacc_g1$SIMD <- relevel(df_cohort_vacc_g1$SIMD, ref = 5)

# include care home and n_test_gp

p_years<-pyears(Surv(start_time,end_time,event)~date_period + Sex + age_gp  + 
                  SIMD  + ur_combined + EAVE_Smoke_n + bmi_cat + HB + prev_positive_status + 
                  n_risk_gps + in_hosp_status + n_tests_gp +care_home_elderly+num_weeks_post_vacc, 
                data = df_cohort_vacc_g1, data.frame = TRUE)
glm_pos <- glm(event~offset(log(pyears)) +date_period+ Sex + age_gp + 
                 SIMD  + ur_combined + EAVE_Smoke_n + bmi_cat + HB + prev_positive_status +
                 n_risk_gps  + in_hosp_status + n_tests_gp + care_home_elderly+num_weeks_post_vacc, 
               family=poisson,data=p_years$data)

summary(glm_pos)
#exp(glm_pos$coefficients)
round(exp(glm_pos$coefficients),2)
#round(exp(confint(glm_pos)),2)
round(exp(glm_pos$coefficients-coef(summary(glm_pos))[,2]*1.96),2)
round(exp(glm_pos$coefficients+coef(summary(glm_pos))[,2]*1.96),2)

######
df_cohort_vacc_g1<-filter(df_cohort_vacc_g,vacc_type=="AZ")
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(HB=="S08000026"))
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(HB=="S08000025"))
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(HB=="S08000028"))
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(bmi_cat==1))
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(num_weeks_post_vacc==2))
# removed bmi and prev post test due to low or no events
p_years<-pyears(Surv(start_time,end_time,event)~date_period + Sex + age_gp  + 
                  SIMD  + ur_combined + EAVE_Smoke_n + bmi_cat + HB + prev_positive_status + 
                  n_risk_gps + in_hosp_status + n_tests_gp +care_home_elderly+num_weeks_post_vacc, 
                data = df_cohort_vacc_g1, data.frame = TRUE)
glm_pos <- glm(event~offset(log(pyears)) +date_period+ Sex + age_gp + 
                 SIMD  + ur_combined + EAVE_Smoke_n + bmi_cat + HB + prev_positive_status +
                 n_risk_gps  + in_hosp_status + n_tests_gp + care_home_elderly+num_weeks_post_vacc, 
               family=poisson,data=p_years$data)

summary(glm_pos)
#exp(glm_pos$coefficients)
round(exp(glm_pos$coefficients),2)
#round(exp(confint(glm_pos)),2)
round(exp(glm_pos$coefficients-coef(summary(glm_pos))[,2]*1.96),2)
round(exp(glm_pos$coefficients+coef(summary(glm_pos))[,2]*1.96),2)
######

df_cohort_vacc_g1<-filter(df_cohort_vacc_g,vacc_type=="PB")
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(bmi_cat==1))
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(HB=="S08000026"))
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(HB=="S08000025"))
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(num_weeks_post_vacc==14))
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(num_weeks_post_vacc==15))
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(num_weeks_post_vacc==16))
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(num_weeks_post_vacc==17))

# removed bmi and prev post test due to low or no events
p_years<-pyears(Surv(start_time,end_time,event)~date_period + Sex + age_gp  + 
                  SIMD  + ur_combined + EAVE_Smoke_n + bmi_cat + HB + prev_positive_status + 
                  n_risk_gps + in_hosp_status + n_tests_gp +care_home_elderly+num_weeks_post_vacc, 
                data = df_cohort_vacc_g1, data.frame = TRUE)
glm_pos <- glm(event~offset(log(pyears)) +date_period+ Sex + age_gp + 
                 SIMD  + ur_combined + EAVE_Smoke_n + bmi_cat + HB + prev_positive_status +
                 n_risk_gps  + in_hosp_status + n_tests_gp + care_home_elderly+num_weeks_post_vacc, 
               family=poisson,data=p_years$data)
summary(glm_pos)
#exp(glm_pos$coefficients)
round(exp(glm_pos$coefficients),2)
#round(exp(confint(glm_pos)),2)
round(exp(glm_pos$coefficients-coef(summary(glm_pos))[,2]*1.96),2)
round(exp(glm_pos$coefficients+coef(summary(glm_pos))[,2]*1.96),2)
######

######
df_cohort_vacc_g1<-filter(df_cohort_vacc_g,age_gp=="80+")
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(HB=="S08000028"))
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(HB=="S08000026"))
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(HB=="S08000025"))
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(num_weeks_post_vacc==11))

# removed bmi and prev post test due to low or no events
p_years<-pyears(Surv(start_time,end_time,event)~date_period + Sex  + 
                  SIMD  + ur_combined + EAVE_Smoke_n  + HB + prev_pos_test + 
                  EAVE_BP_n + n_risk_gps + num_weeks_post_vacc, 
                data = df_cohort_vacc_g1, data.frame = TRUE)
glm_pos <- glm(event~offset(log(pyears)) +date_period+ Sex + 
                 SIMD  + ur_combined + EAVE_Smoke_n  + HB + prev_pos_test +
                 EAVE_BP_n + n_risk_gps  + num_weeks_post_vacc, 
               family=poisson,data=p_years$data)
summary(glm_pos)
#exp(glm_pos$coefficients)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)


df_cohort_vacc_g2<-filter(df_cohort_vacc_g1,vacc_type=="PB")
df_cohort_vacc_g2<-filter(df_cohort_vacc_g2,!(num_weeks_post_vacc==9))
df_cohort_vacc_g2<-filter(df_cohort_vacc_g2,!(num_weeks_post_vacc==10))
# removed bmi and prev post test due to low or no events
p_years<-pyears(Surv(start_time,end_time,event)~date_period + Sex  + 
                  SIMD  + ur_combined + EAVE_Smoke_n  + HB + prev_pos_test + 
                  EAVE_BP_n + n_risk_gps + num_weeks_post_vacc, 
                data = df_cohort_vacc_g2, data.frame = TRUE)
glm_pos <- glm(event~offset(log(pyears)) +date_period+ Sex + 
                 SIMD  + ur_combined + EAVE_Smoke_n  + HB + prev_pos_test +
                 EAVE_BP_n + n_risk_gps  + num_weeks_post_vacc, 
               family=poisson,data=p_years$data)
summary(glm_pos)
#exp(glm_pos$coefficients)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

df_cohort_vacc_g2<-filter(df_cohort_vacc_g1,vacc_type=="AZ")
df_cohort_vacc_g2<-filter(df_cohort_vacc_g2,!(HB=="Unknown"))
df_cohort_vacc_g2<-filter(df_cohort_vacc_g2,!(num_weeks_post_vacc==2))
table(df_cohort_vacc_g2$event,df_cohort_vacc_g2$num_weeks_post_vacc)
# removed bmi and prev post test due to low or no events
p_years<-pyears(Surv(start_time,end_time,event)~date_period + Sex  + 
                  SIMD  + ur_combined + EAVE_Smoke_n  + HB + prev_pos_test + 
                  EAVE_BP_n + n_risk_gps + num_weeks_post_vacc, 
                data = df_cohort_vacc_g2, data.frame = TRUE)
glm_pos <- glm(event~offset(log(pyears)) +date_period+ Sex + 
                 SIMD  + ur_combined + EAVE_Smoke_n  + HB + prev_pos_test +
                 EAVE_BP_n + n_risk_gps  + num_weeks_post_vacc, 
               family=poisson,data=p_years$data)
summary(glm_pos)
#exp(glm_pos$coefficients)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)
######

df_cohort_vacc_g1<-filter(df_cohort_vacc_g,age_gp=="80+")
table(df_cohort_vacc_g1$event,df_cohort_vacc_g1$num_weeks_post_vacc)
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(HB=="S08000028"))
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(HB=="S08000026"))
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(HB=="S08000025"))
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!(num_weeks_post_vacc==11))

# removed bmi and prev post test due to low or no events
p_years<-pyears(Surv(start_time,end_time,event)~date_period + Sex  + 
                  SIMD  + ur_combined + EAVE_Smoke_n  + HB + prev_pos_test + 
                  EAVE_BP_n + n_risk_gps + num_weeks_post_vacc, 
                data = df_cohort_vacc_g1, data.frame = TRUE)
glm_pos <- glm(event~offset(log(pyears)) +date_period+ Sex + 
                 SIMD  + ur_combined + EAVE_Smoke_n  + HB + prev_pos_test +
                 EAVE_BP_n + n_risk_gps  + num_weeks_post_vacc, 
               family=poisson,data=p_years$data)
summary(glm_pos)
#exp(glm_pos$coefficients)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)


df_cohort_vacc_g2<-filter(df_cohort_vacc_g1,vacc_type=="PB")
df_cohort_vacc_g2<-filter(df_cohort_vacc_g2,!(num_weeks_post_vacc==9))
df_cohort_vacc_g2<-filter(df_cohort_vacc_g2,!(num_weeks_post_vacc==10))
# removed bmi and prev post test due to low or no events
p_years<-pyears(Surv(start_time,end_time,event)~date_period + Sex  + 
                  SIMD  + ur_combined + EAVE_Smoke_n  + HB + prev_pos_test + 
                  EAVE_BP_n + n_risk_gps + num_weeks_post_vacc, 
                data = df_cohort_vacc_g2, data.frame = TRUE)
glm_pos <- glm(event~offset(log(pyears)) +date_period+ Sex + 
                 SIMD  + ur_combined + EAVE_Smoke_n  + HB + prev_pos_test +
                 EAVE_BP_n + n_risk_gps  + num_weeks_post_vacc, 
               family=poisson,data=p_years$data)
summary(glm_pos)
#exp(glm_pos$coefficients)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)

df_cohort_vacc_g2<-filter(df_cohort_vacc_g1,vacc_type=="AZ")
df_cohort_vacc_g2<-filter(df_cohort_vacc_g2,!(HB=="Unknown"))
df_cohort_vacc_g2<-filter(df_cohort_vacc_g2,!(num_weeks_post_vacc==2))
table(df_cohort_vacc_g2$event,df_cohort_vacc_g2$num_weeks_post_vacc)
# removed bmi and prev post test due to low or no events
p_years<-pyears(Surv(start_time,end_time,event)~date_period + Sex  + 
                  SIMD  + ur_combined + EAVE_Smoke_n  + HB + prev_pos_test + 
                  EAVE_BP_n + n_risk_gps + num_weeks_post_vacc, 
                data = df_cohort_vacc_g2, data.frame = TRUE)
glm_pos <- glm(event~offset(log(pyears)) +date_period+ Sex + 
                 SIMD  + ur_combined + EAVE_Smoke_n  + HB + prev_pos_test +
                 EAVE_BP_n + n_risk_gps  + num_weeks_post_vacc, 
               family=poisson,data=p_years$data)
summary(glm_pos)
#exp(glm_pos$coefficients)
round(exp(glm_pos$coefficients),2)
round(exp(confint(glm_pos)),2)
########

z<-data.frame(exp(glm_pos$coefficients),exp(confint(glm_pos)))
# plot rate ratio
z<-z[2:nrow(z),]
z1<-rownames(z)
rr_plot_data <- data.frame(yAxis = length(z1):1, boxRR = z[,1], 
                           boxCILow = z[,2], boxCIHigh = z[,3])
ggplot(rr_plot_data,aes(x = boxRR, y = z1))+
  geom_vline(aes(xintercept = 1),size=0.25,linetype="dashed")+
  geom_errorbarh(aes(xmax=boxCIHigh, xmin=boxCILow),size=0.5, height=0.2, color="gray50")+
  geom_point(size=3.5, color="orange")+
  theme_bw() + 
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = seq(-1,11,0.5),
                     labels=seq(-1,11,0.5),limits = c(-1,11))+
  ylab("Confounders")+
  xlab("Rate Ratio")+
  annotate(geom="text", y=1.1, x=7.5,label="Rate ratio plot",size=3.5,hjust=0)+
  ggtitle("Rate Ratio plot")

# # adjusted rate ratio analysis
# # pyears - rate of event per year
# # glm_poisson - log rate ratios
# # adjusted rate ratio analysis
# # pyears - rate of event per year
# # glm_poisson - log rate ratios
# p_years<-pyears(Surv(start_time,end_time,event)~date_period + Sex + 
#                   age_gp + simd2020_sc_quintile + ur6_2016_name + EAVE_Smoke + EAVE_BP +
#                   n_risk_gps + Q_DIAG_AF + Q_DIAG_ASTHMA + Q_DIAG_BLOOD_CANCER + Q_DIAG_CCF + 
#                   Q_DIAG_CEREBRALPALSY + Q_DIAG_CHD + Q_DIAG_CIRRHOSIS + Q_DIAG_CONGEN_HD + 
#                   Q_DIAG_EPILEPSY + Q_DIAG_FRACTURE + Q_DIAG_NEURO + Q_DIAG_PARKINSONS + 
#                   Q_DIAG_PULM_HYPER + Q_DIAG_PULM_RARE + Q_DIAG_PVD + Q_DIAG_RA_SLE + 
#                   Q_DIAG_RESP_CANCER + Q_DIAG_SEV_MENT_ILL + Q_DIAG_SICKLE_CELL + Q_DIAG_STROKE + 
#                   Q_DIAG_VTE + Q_LEARN_CAT + Q_DIAG_CKD_LEVEL, data = df_cohort_vacc_g,
#                 data.frame = TRUE)
# glm_pos <- glm(event~offset(log(pyears)) +date_period+ Sex + vacc_type + 
#                  age_gp + simd2020_sc_quintile + ur6_2016_name + EAVE_Smoke + EAVE_BP +
#                  n_risk_gps + Q_DIAG_AF + Q_DIAG_ASTHMA + Q_DIAG_BLOOD_CANCER + Q_DIAG_CCF + 
#                  Q_DIAG_CEREBRALPALSY + Q_DIAG_CHD + Q_DIAG_CIRRHOSIS + Q_DIAG_CONGEN_HD + 
#                  Q_DIAG_COPD + Q_DIAG_DEMENTIA + Q_DIAG_DIABETES_1 + Q_DIAG_DIABETES_2 + 
#                  Q_DIAG_EPILEPSY + Q_DIAG_FRACTURE + Q_DIAG_NEURO + Q_DIAG_PARKINSONS + 
#                  Q_DIAG_PULM_HYPER + Q_DIAG_PULM_RARE + Q_DIAG_PVD + Q_DIAG_RA_SLE + 
#                  Q_DIAG_RESP_CANCER + Q_DIAG_SEV_MENT_ILL + Q_DIAG_SICKLE_CELL + Q_DIAG_STROKE + 
#                  Q_DIAG_VTE + Q_LEARN_CAT + Q_DIAG_CKD_LEVEL,
#                family=poisson,data=p_years$data)
# exp(glm_pos$coefficients)

# # cummulative event incidence plot#
# # sex , simd, age cat, n risk, comorbities
# surv_fit<-survfit(Surv(start_time,end_time,event)~1, data = df_cohort_vacc_g)
# plot(surv_fit,fun="event")
# 
# # stratifying survival time in each different period
# surv_fit<-survfit(Surv(start_time,end_time,event)~strata(date_period), data = df_cohort_vacc_g)
# plot(surv_fit,fun="event")

####################
# df_cohort_vacc_g$simd2020_sc_quintile <- relevel(df_cohort_vacc_g$simd2020_sc_quintile, ref = "5-Low")
# df_cohort_vacc_g$EAVE_Smoke <- as.factor(df_cohort_vacc_g$EAVE_Smoke)
# df_cohort_vacc_g$EAVE_Smoke <- relevel(df_cohort_vacc_g$EAVE_Smoke, ref = "Non Smoker")
# df_cohort_vacc_g$EAVE_BP <- as.factor(df_cohort_vacc_g$EAVE_BP)
# df_cohort_vacc_g$EAVE_BP <- relevel(df_cohort_vacc_g$EAVE_BP, ref = "Normal")

#############################
# vaccine period
#df_cohort_vacc_g<-df_cohort_vacc_g %>% 
#  group_by(EAVE_LINKNO) %>%
#  mutate(vacc_period=last(date_period))

# df_cohort_vacc_g$EAVE_Smoke_n[df_cohort_vacc_g$EAVE_Smoke=="Non Smoker"]<-0
# df_cohort_vacc_g$EAVE_Smoke_n[df_cohort_vacc_g$EAVE_Smoke=="Smoker"]<-1
# df_cohort_vacc_g$EAVE_Smoke_n[df_cohort_vacc_g$EAVE_Smoke=="Ex Smoker"]<-2
# df_cohort_vacc_g$EAVE_Smoke_n[df_cohort_vacc_g$EAVE_Smoke=="Unknown"]<-3

# adding week of event as confounder
# z<-data.frame(seq(as.Date("2020-12-08"),as.Date("2021-03-05"),"day"))
# colnames(z)[1]<-"date_vacc_1"
# z<-mutate(z,week_date=cut(z$date_vacc_1,"weeks"))
# z$week_date<-as.Date(z$week_date)
# z<-mutate(z,week_date=week_date+1)
# z$week_date<-lag(z$week_date,1)
# z$week_date[1]<-z$week_date[2]
# z<-mutate(z,num_weeks_post_vacc=cumsum(!duplicated(week_date)))
# z$week_date<-NULL
# df_cohort_vacc_g<-left_join(df_cohort_vacc_g,z,by="date_vacc_1")

#add previous positive test
# z <- select(df_cohort,EAVE_LINKNO,date_vacc_1)
# z <- left_join(Positive_Tests,z,by="EAVE_LINKNO")
# z <- filter(z, specimen_date < date_vacc_1) 
# z <- mutate(z, prev_pos_test = 1) 
# z <- select(z,EAVE_LINKNO,prev_pos_test)
# df_cohort_vacc_g<-left_join(df_cohort_vacc_g,z,by="EAVE_LINKNO")
# df_cohort_vacc_g$prev_pos_test[is.na(df_cohort_vacc_g$prev_pos_test)]<-0L

# z <- select(df_cohort,EAVE_LINKNO,date_vacc_1)
# z <- left_join(Positive_Tests,z,by="EAVE_LINKNO")
# z <- filter(z, specimen_date < date_vacc_1)
# z <- mutate(z, prev_pos_test = 1)
# z <- select(z,EAVE_LINKNO,prev_pos_test)
# df_cohort_vacc<-left_join(df_cohort_vacc,z,by="EAVE_LINKNO")
# df_cohort_vacc$prev_pos_test[is.na(df_cohort_vacc$prev_pos_test)]<-0L

# BMI cat
# df_cohort_vacc_g$bmi_impute<-round(df_cohort_vacc_g$bmi_impute,1)
# df_cohort_vacc_g<-mutate(df_cohort_vacc_g,bmi_cat =
#                     cut(ageYear, breaks = c(-Inf, 18.5,24.9,29.9,Inf),
#                         labels=c("1","0","2","3")))
#underweight - 1
#healthy - 0
#overweight - 2
#obese - 3


# df_cohort_vacc_g<-mutate(df_cohort_vacc_g,
#                          ur_combined=if_else(ur6_2016_name=="1 Large Urban Areas"|
#                                                ur6_2016_name=="2 Other Urban Areas"|
#                                                ur6_2016_name=="3 Accessible Small Towns",1,2))
# df_cohort_vacc_g$ur_combined <- as.factor(df_cohort_vacc_g$ur_combined)
# df_cohort_vacc_g$ur_combined <- relevel(df_cohort_vacc_g$ur_combined, ref = 1)
# 
# df_cohort_vacc_g$SIMD[df_cohort_vacc_g$simd2020_sc_quintile=="5-Low"]<-5
# df_cohort_vacc_g$SIMD[df_cohort_vacc_g$simd2020_sc_quintile=="4"]<-4
# df_cohort_vacc_g$SIMD[df_cohort_vacc_g$simd2020_sc_quintile=="3"]<-3
# df_cohort_vacc_g$SIMD[df_cohort_vacc_g$simd2020_sc_quintile=="2"]<-2
# df_cohort_vacc_g$SIMD[df_cohort_vacc_g$simd2020_sc_quintile=="1 - High"]<-1

# dont use the following method
# df_cohort_vacc_g<-mutate(df_cohort_vacc_g,
#                          SIMD=case_when(simd2020_sc_quintile=="1 - High"~1,
#                                         simd2020_sc_quintile=="2"~2,
#                                         simd2020_sc_quintile=="3"~3,
#                                         simd2020_sc_quintile=="4"~4,
#                                         simd2020_sc_quintile=="5-Low"~5))
# df_cohort_vacc_g$SIMD <- as.factor(df_cohort_vacc_g$SIMD)
# df_cohort_vacc_g$SIMD <- relevel(df_cohort_vacc_g$SIMD, ref = 5)
###############################