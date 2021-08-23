##########################################################
# Name of file: 01a_Vaccinations_Input.R
# Original author(s): Utkarsh Agrawal
# Description of content: reads in the cohort and merges in vaccination data 
# Approximate run time: Unknown
##########################################################

# 01 Setup ####
#Libraries
library(plyr)
library(tidyverse)
library(survival)
library(dplyr)
library(mgcv)
library(tidyr)
library(scales)
library("finalfit")
#Load data

Location <- "/conf/"  # Server

project_path <- paste0(Location,"EAVE/GPanalysis/analyses/COVID_vaccines_1st_dose/Waning/Covid-vaccine-waning")
df_cohort_rachel<-readRDS(paste0(project_path,"/output/df_cohort.rds"))
df_vaccinations_rachel<-readRDS(paste0(project_path,"/output/df_vaccinations.rds"))

project_path <- paste0(Location,"EAVE/GPanalysis/progs/UA/first_dose_failure")

covid_death <- readRDS(paste0(Location,"/EAVE/GPanalysis/progs/UA/first_dose_failure/data/covid_death_rachel.RDS"))
covid_hospitalisations <- readRDS(paste0(Location,"/EAVE/GPanalysis/progs/UA/first_dose_failure/data/covid_hospitalisations_rachel.RDS"))
covid_hosp_death <- readRDS(paste0(Location,"/EAVE/GPanalysis/progs/UA/first_dose_failure/data/covid_hosp_death_rachel.RDS"))

df_cohort_vacc_g <- readRDS(paste0(Location,"/EAVE/GPanalysis/progs/UA/first_dose_failure/data/df_cohort_coxph_09-4-2021.RDS"))
df_cohort <- readRDS(paste0(Location,"/EAVE/GPanalysis/progs/UA/first_dose_failure/data/df_cohort_ve_failure_09-4-2021.RDS"))
df_cohort_vacc <- readRDS(paste0(Location,"/EAVE/GPanalysis/progs/UA/first_dose_failure/data/df_cohort_ve_failure_vacc_09-4-2021.RDS"))
########

# Outcome = death/hospitalisation
z_event_endpoint <- "death_hosp"

# Secondary outcomes:
# z_event_endpoint <- "covid_hospitalisations"
# z_event_endpoint <- "positive_test"

# Assign z_event to outcome of interest and assign z_test_event to number of days to
# add to specimen data if event time is missing
if (z_event_endpoint =="hosp_covid") {z_event <- covid_hospitalisations
z_test_event <- 7
z_title <- "COVID-19 hospitalisations"}
if (z_event_endpoint =="icu_death") {z_event <- covid_icu_death
z_test_event <- 14}
if (z_event_endpoint =="death") {z_event <- covid_death
z_test_event <- 21
z_title <- "COVID-19 deaths"}
if (z_event_endpoint =="death_hosp") {z_event <- covid_hosp_death
z_test_event_hosp <- 7
z_test_event_death <- 21
z_title <- "COVID-19 hospitalisations and deaths"}
if (z_event_endpoint =="positive_test") {z_event <- positive_test}

# Find end date according to admission date
a_end <- max(z_event$admission_date)
a_begin <- as.Date("2020-12-08")

##### Getting cohort summaries ####
## Using updated weights to get number of people

# Number of participants
n_tot <- sum(z_chrt_desc$eave_weight)
n_tot

# No. vacc
n <- sum(z_chrt_desc$eave_weight[z_chrt_desc$vacc1==1])
n
n/n_tot

#sum(z_df$eave_weight) == n


## Split by vaccine type
n1 <- sum(z_chrt_desc$eave_weight[which(z_chrt_desc$vacc_type=="PB")])
n2 <- sum(z_chrt_desc$eave_weight[which(z_chrt_desc$vacc_type=="AZ")])
n1
n2
n1+n2 == n

n1/n

n2/n

## Number of events post vaccination

n <- sum(z_chrt_desc$eave_weight[which(z_chrt_desc$date_vacc_1 < z_chrt_desc$admission_date)])
n

n <- sum(z_chrt_desc$eave_weight[which(z_chrt_desc$date_vacc_1 < z_chrt_desc$hosp_admission_date)])
n

n <- sum(z_chrt_desc$eave_weight[which(z_chrt_desc$date_vacc_1 < z_chrt_desc$NRS.Date.Death)])
n

# PB
n <- sum(z_chrt_desc$eave_weight[which(z_chrt_desc$date_vacc_1 < z_chrt_desc$admission_date & z_chrt_desc$vacc_type=="AZ")])
n

n <- sum(z_chrt_desc$eave_weight[which(z_chrt_desc$date_vacc_1 < z_chrt_desc$hosp_admission_date & z_chrt_desc$vacc_type=="PB")])
n

n <- sum(z_chrt_desc$eave_weight[which(z_chrt_desc$date_vacc_1 < z_chrt_desc$NRS.Date.Death & z_chrt_desc$vacc_type=="PB")])
n


# # slecting events 14 days post vacc
z1<-filter(df_cohort_vacc_g,event==1)
z2<-filter(z1,date_period=="date_preiod_0007"|date_period=="date_preiod_0714")
df_cohort_vacc_g_orig<-df_cohort_vacc_g
df_cohort_vacc_g <- df_cohort_vacc_g %>%
  filter(!EAVE_LINKNO %in% z2$EAVE_LINKNO)
df_cohort_vacc_g<-filter(df_cohort_vacc_g,date_period!="date_preiod_0007"&
                           date_period!="date_preiod_0714")
# df_cohort_vacc_g<-filter(df_cohort_vacc_g,event==1 & (date_period!="date_preiod_0007"&
#                                                         date_period!="date_preiod_0714"))
df_cohort_vacc <- df_cohort_vacc %>%
  filter(!EAVE_LINKNO %in% z2$EAVE_LINKNO)

length(which(df_cohort_vacc_g$event==1))
length(which(df_cohort_vacc_g$event==1&(df_cohort_vacc_g$date_period=="date_preiod_0007"|
                                          df_cohort_vacc_g$date_period=="date_preiod_0714")))

length(which(df_cohort_vacc_g_orig$event==1))
length(which(df_cohort_vacc_g_orig$event==1&(df_cohort_vacc_g_orig$date_period=="date_preiod_0007"|
                                               df_cohort_vacc_g_orig$date_period=="date_preiod_0714")))

df_cohort_vacc_g$EAVE_Smoke_n[df_cohort_vacc_g$EAVE_Smoke=="Non Smoker"]<-0
df_cohort_vacc_g$EAVE_Smoke_n[df_cohort_vacc_g$EAVE_Smoke=="Smoker"]<-1
df_cohort_vacc_g$EAVE_Smoke_n[df_cohort_vacc_g$EAVE_Smoke=="Ex Smoker"]<-2
df_cohort_vacc_g$EAVE_Smoke_n[df_cohort_vacc_g$EAVE_Smoke=="Unknown"]<-3

# adding week of event as confounder
z<-data.frame(seq(as.Date("2020-12-08"),as.Date("2021-04-18"),"day"))
colnames(z)[1]<-"date_vacc_1"
z<-mutate(z,week_date=cut(z$date_vacc_1,"weeks"))
z$week_date<-as.Date(z$week_date)
z<-mutate(z,week_date=week_date+1)
z$week_date<-lag(z$week_date,1)
z$week_date[1]<-z$week_date[2]
z<-mutate(z,num_weeks_post_vacc=cumsum(!duplicated(week_date)))
z$week_date<-NULL
df_cohort_vacc_g<-left_join(df_cohort_vacc_g,z,by="date_vacc_1")

# BMI cat
df_cohort_vacc_g$bmi_impute<-round(df_cohort_vacc_g$bmi_impute,1)
df_cohort_vacc_g<-mutate(df_cohort_vacc_g,bmi_cat =
                           cut(ageYear, breaks = c(-Inf, 18.5,24.9,29.9,Inf),
                               labels=c("1","0","2","3")))
#underweight - 1
#healthy - 0
#overweight - 2
#obese - 3

# collapse urban rural for ARR analysis
df_cohort_vacc_g<-mutate(df_cohort_vacc_g,
                         ur_combined=if_else(ur6_2016_name=="1 Large Urban Areas"|
                                               ur6_2016_name=="2 Other Urban Areas"|
                                               ur6_2016_name=="3 Accessible Small Towns",1,2))
df_cohort_vacc_g$ur_combined <- as.factor(df_cohort_vacc_g$ur_combined)
df_cohort_vacc_g$ur_combined <- relevel(df_cohort_vacc_g$ur_combined, ref = 1)

# SIMD with numerical cat 
df_cohort_vacc_g$SIMD[df_cohort_vacc_g$simd2020_sc_quintile=="5-Low"]<-5
df_cohort_vacc_g$SIMD[df_cohort_vacc_g$simd2020_sc_quintile=="4"]<-4
df_cohort_vacc_g$SIMD[df_cohort_vacc_g$simd2020_sc_quintile=="3"]<-3
df_cohort_vacc_g$SIMD[df_cohort_vacc_g$simd2020_sc_quintile=="2"]<-2
df_cohort_vacc_g$SIMD[df_cohort_vacc_g$simd2020_sc_quintile=="1 - High"]<-1
df_cohort_vacc_g$SIMD <- as.factor(df_cohort_vacc_g$SIMD)
df_cohort_vacc_g$SIMD <- relevel(df_cohort_vacc_g$SIMD, ref = 5)
##############################
