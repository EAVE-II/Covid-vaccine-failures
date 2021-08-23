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
#Load data

Location <- "/conf/"  # Server

project_path <- paste0(Location,"EAVE/GPanalysis/progs/RM/Vaccine/Vaccine_waning")
df_cohort_rachel<-readRDS(paste0(project_path,"/output/df_cohort.rds"))
df_vaccinations_rachel<-readRDS(paste0(project_path,"/output/df_vaccinations.rds"))
z_chrt_desc<-readRDS(paste0(project_path,"/output/z_chrt_desc.rds"))

project_path <- paste0(Location,"EAVE/GPanalysis/progs/UA/first_dose_failure")

# use the updated file
#EAVE_cohort <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/Cohort_Demog_Endpoints_Times2021-02-24.rds"))
EAVE_cohort <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/Cohort_Demog_Endpoints_Times2021-04-20.rds"))
EAVE_cohort <- filter(EAVE_cohort, !duplicated(EAVE_LINKNO))
a_begin <- as.Date("2020-12-08")
#remove all who have died before the beginning
EAVE_cohort <- filter(EAVE_cohort, is.na(NRS.Date.Death) | (!is.na(NRS.Date.Death) & NRS.Date.Death >= a_begin))
#remove under 18s
EAVE_cohort <- filter(EAVE_cohort, ageYear >= 18)

EAVE_Weights <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_Weights.rds"))
EAVE_cohort  <- EAVE_cohort %>% left_join(EAVE_Weights, by="EAVE_LINKNO")
EAVE_cohort$eave_weight[is.na(EAVE_cohort$eave_weight)] <- mean(EAVE_cohort$eave_weight, na.rm=T)

EAVE_cohort <- EAVE_cohort %>% mutate(death_covid = case_when(death_covid==1 & is.na(NRS.Date.Death) ~ 0,
                                                       TRUE ~ death_covid),
                            icu_death = case_when(icu_death==1 & is.na(date_icu_death) ~ 0,
                                                       TRUE ~ icu_death ) )

# remember to change to change to qcovid risk groups
rg <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_RG_EAVE.rds"))
rg <- filter(rg, !duplicated(EAVE_LINKNO))
rg <- rg %>% dplyr::select(EAVE_LINKNO:EAVE_CHRONIC_LIVER_DIS, EAVE_CHRONIC_LIVER_DIS:EAVE_DIABETES, 
                           EAVE_HYPERTENSION, n_risk_gps)
z <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_RG_EAVE_BP_Smoke.rds"))
z <- filter(z, !duplicated(EAVE_LINKNO))
z <- z %>% dplyr::select(EAVE_LINKNO, EAVE_Smoking_Status_Worst, EAVE_BP) %>% 
  dplyr::rename(EAVE_Smoke = EAVE_Smoking_Status_Worst)

rg <- rg %>% left_join(z, by="EAVE_LINKNO")

#read in the Previous Tests data
z  <- readRDS(paste0(Location,"EAVE/GPanalysis/data/Tests.RDS")) %>% 
  dplyr::select(EAVE_LINKNO, n_tests)
# Remove duplicates - take the row with the highest number of tests
z <- z %>%
  group_by(EAVE_LINKNO) %>%
  dplyr::summarise(n_tests = max(n_tests)) %>% 
  mutate(n_tests = if_else(is.na(n_tests),0L,n_tests) )

rg <- rg %>% left_join(z, by="EAVE_LINKNO")
rg <- rg %>% mutate(n_tests = if_else(is.na(n_tests), 0L, n_tests))


# #read in the GP vaccination data
# z  <- readRDS(paste0(Location,"EAVE/GPanalysis/data/cleaned_data/C19vaccine.rds"))
# 
# print(unique(z$type))
# print(unique(z$stage))
# 
# Vaccinations <- z %>% mutate(Date = as.Date(occurrence_time)) %>%
#   mutate(vacc_type = case_when(grepl("COURAGEOUS", type) ~ "PB",
#                                grepl("TALENT", type) ~ "AZ",
#                                type == "39114911000001105" ~ "AZ",
#                                type == "39115611000001103" ~ "PB",
#                                type == "39115611000001105" ~ "PB",
#                                TRUE ~ "UNK"),
#          dose_number = if_else(stage %in% c(0,3), 1L, stage))
# 
# v1 <- filter(Vaccinations, dose_number==1) %>%
#   dplyr::select(EAVE_LINKNO, Date, vacc_type, dose_number) %>%
#   arrange(EAVE_LINKNO, Date) %>%
#   filter(!duplicated(EAVE_LINKNO))
# v2 <- filter(Vaccinations, dose_number==2) %>%
#   dplyr::select(EAVE_LINKNO, Date, vacc_type, dose_number) %>%
#   arrange(EAVE_LINKNO, Date) %>%
#   filter(!duplicated(EAVE_LINKNO))
# 
# Vaccinations <- left_join(v1,v2, by="EAVE_LINKNO") %>%
#   mutate(date_vacc_1 = as.Date(Date.x),
#          date_vacc_2 = as.Date(Date.y) ) %>%
#   dplyr::rename(vacc_type=vacc_type.x,
#                 vacc_type_2=vacc_type.y) %>%
#   dplyr::select(-dose_number.x, -dose_number.y, -Date.x, -Date.y)
# rm(z,v1,v2)
# 
# print(table(Vaccinations$vacc_type, Vaccinations$vacc_type_2, exclude=NULL))
# #omit inconsistent records
# Vaccinations <- Vaccinations %>% filter(vacc_type %in% c("AZ","PB")) %>%
#   filter(vacc_type_2 %in% c("AZ","PB") | is.na(vacc_type_2)) %>%
#   filter( !(!is.na(vacc_type_2) & (vacc_type_2 != vacc_type)))

Vaccinations<-df_vaccinations_rachel

#read in covid hositalisations since Dec 01
#derived from linking ecoss to rapid - first admission following a positive test
#this file may have people already in hospital excluded
#we should be able to use the endpoints file for this.
#the read in data are admission following a positive test
#data derived in Chris/Respiratory/Coronavirus/Vaccine_Effect

#use the EAVE hospitalisations
z <- EAVE_cohort %>% dplyr::select(EAVE_LINKNO, SpecimenDate, hosp_covid, date_hosp_covid, NRS.Date.Death) %>% 
  filter(hosp_covid==1) %>% 
  filter(date_hosp_covid >= a_begin) %>% 
  dplyr::rename(admission_date = date_hosp_covid) %>% 
  mutate(admission_date = if_else(is.na(NRS.Date.Death) | !is.na(NRS.Date.Death)&(admission_date <= NRS.Date.Death), admission_date, NRS.Date.Death)) %>% 
  dplyr::select(-hosp_covid, -NRS.Date.Death)
covid_hospitalisations <- z
z<-mutate(z,hosp_accquired_cov=if_else(SpecimenDate-admission_date>=3&SpecimenDate-admission_date<=6,1L,0L))
z<-select(z,EAVE_LINKNO,hosp_accquired_cov)
EAVE_cohort<-left_join(EAVE_cohort,z,by="EAVE_LINKNO")
EAVE_cohort$hosp_accquired_cov[is.na(EAVE_cohort$hosp_accquired_cov)]<-0
#use the EAVE severe cases
# z <- EAVE_cohort %>% dplyr::select(EAVE_LINKNO, SpecimenDate, icu_death, date_icu_death, NRS.Date.Death) %>% 
#   filter(icu_death==1) %>% 
#   filter(date_icu_death >= a_begin) %>% 
#   dplyr::rename(admission_date = date_icu_death) %>% 
#   mutate(admission_date = if_else(is.na(NRS.Date.Death) | !is.na(NRS.Date.Death)&(admission_date <= NRS.Date.Death), admission_date, NRS.Date.Death)) %>% 
#   dplyr::select(-icu_death, -NRS.Date.Death)
# covid_icu_death <- z

#use the EAVE death cases
z <- EAVE_cohort %>% dplyr::select(EAVE_LINKNO, SpecimenDate, death_covid, NRS.Date.Death) %>% 
  filter(death_covid==1) %>% 
  filter(NRS.Date.Death >= a_begin) %>% 
  dplyr::rename(covid_death_date = NRS.Date.Death) %>% 
  dplyr::select(-death_covid)
covid_death <- z
z<-select(z,EAVE_LINKNO,covid_death_date)
EAVE_cohort<-left_join(EAVE_cohort,z,by="EAVE_LINKNO")

#use the EAVE death cases
z <- EAVE_cohort %>% dplyr::select(EAVE_LINKNO, SpecimenDate, NRS.Date.Death) %>% 
  filter(!is.na(NRS.Date.Death)) %>% 
  filter(NRS.Date.Death >= a_begin) %>% 
  dplyr::rename(admission_date = NRS.Date.Death)
any_death <- z

# all_hospitalisations  <- readRDS(paste0(Location,"EAVE/GPanalysis/data/all_hosps2021-03-01.rds"))
# all_hospitalisations <- left_join(all_hospitalisations, covid_hospitalisations, by="EAVE_LINKNO", suffix=c("","_covid")) %>% 
#   filter(is.na(admission_date_covid) | !is.na(admission_date_covid)&(admission_date_covid > admission_date + 14) ) %>% 
#   dplyr::select(-SpecimenDate, - admission_date_covid)
# #z_sub <- filter(z, !is.na(admission_date_covid))
# 
# all_nc_emerg_hosp <- filter(all_hospitalisations, emergency==TRUE) %>% dplyr::select(-emergency)
# all_nc_non_emerg_hosp <- filter(all_hospitalisations, emergency==FALSE) %>% dplyr::select(-emergency)

#cohort + risk groups
df_cohort <- EAVE_cohort %>% 
  left_join(rg, by="EAVE_LINKNO") %>% 
  left_join(Vaccinations,by = "EAVE_LINKNO") %>% 
  mutate(vacc_1_status=if_else(is.na(date_vacc_1),0,1)) %>%
  mutate(vacc_2_status=if_else(is.na(date_vacc_2),0,1))
df_cohort<-select(df_cohort,-c(Time.To.Test:Time.To.Hosp))

## add covid positive prior to vaccinaiton as a confounder for cox model
#df_cohort<-mutate(df_cohort,cov_pos_pre_vacc=if_else(SpecimenDate<date_vacc_1,1L,0L))
Positive_Tests <- readRDS(paste0(Location,"EAVE/GPanalysis/data/Positive_Tests.RDS"))
Positive_Tests <- filter(Positive_Tests, !duplicated(EAVE_LINKNO))

z <- Positive_Tests %>%  mutate(days = as.numeric(specimen_date - a_begin)) %>% 
  mutate(test_before_dec8 = cut(days, breaks = c(-Inf, 0, max(days)),
                                labels=c("pre-vac","post-vacc")))
df_cohort <- df_cohort %>% left_join(dplyr::select(z, EAVE_LINKNO, test_before_dec8), by="EAVE_LINKNO")
df_cohort <- df_cohort %>% mutate(test_before_dec8 = as.character(test_before_dec8)) %>% 
  mutate(test_before_dec8 = if_else(is.na(test_before_dec8), "no pos test",test_before_dec8) )

# # add a flag for prior hopsitalisiton
# hosp_adm_nov01<-readRDS(paste0(Location,"EAVE/GPanalysis/data/any_hospitalisation_post_01112020.rds")) %>%
#   filter(!is.na(EAVE_LINKNO))
# colnames(hosp_adm_nov01)[colnames(hosp_adm_nov01) %in% c("admission_date","discharge_date")]<-
#   c("hosp_adm_date_nov01","hosp_dis_date_nov01")
# df_cohort<-left_join(df_cohort,hosp_adm_nov01,by="EAVE_LINKNO")

# add HB flag
z <- read_csv(paste0(Location,"/EAVE/GPanalysis/data/restored/map_files/Datazone2011Lookup.csv")) %>%
  dplyr::select(DataZone, InterZone, Council, HB)
df_cohort <- df_cohort %>% left_join(z, by="DataZone") %>%
  mutate(HB = if_else(is.na(HB),"Unknown", HB),
         InterZone = if_else(is.na(InterZone),"Unknown", InterZone),
         Council = if_else(is.na(Council),"Unknown", Council))

# change age category
df_cohort<-mutate(df_cohort,age_gp = 
                         cut(ageYear, breaks = c(-Inf, 64,79,Inf),
                             labels=c("18-64","65-79","80+")))
# change eave risk groups to qcovid risk groups
z <- readRDS(paste0(Location,"EAVE/GPanalysis/progs/CR/Vaccine/output/temp/Qcovid.rds"))
z <- z %>% dplyr::select(-(Sex:age_gp), -Q_BMI)
z <- filter(z, !duplicated(EAVE_LINKNO))
z1 <- df_cohort %>% dplyr::select(-(EAVE_ASTHMA:EAVE_HYPERTENSION), -EAVE_Smoke, -EAVE_BP, -n_risk_gps, -eave_weight) %>% 
  left_join(z, by="EAVE_LINKNO")
z1 <- z1 %>% mutate(n_tests = if_else(is.na(n_tests),0L,n_tests) )

df_cohort <- filter(z1, !is.na(eave_weight)) #omit any who - need to fix -  do not match

# code to remove duplicate tests
# z<-df_cohort%>%
#   group_by(EAVE_LINKNO)%>%
#   mutate(n_cov_tests=n())%>%
#   ungroup()
# z1<-filter(z,n_cov_tests>=2)
# z1<-z1 %>%
#   group_by(EAVE_LINKNO) %>%
#   mutate(n_tests1=sum(n_tests)) %>%
#   ungroup() %>%
#   filter(!duplicated(EAVE_LINKNO))
# z1$n_tests<-z1$n_tests1
# z1$n_tests1<-NULL
# z2<-filter(z,n_cov_tests==1)
# df_cohort<-bind_rows(z1,z2)
# df_cohort$n_cov_tests<-NULL
df_cohort$n_tests_gp <- cut(df_cohort$n_tests, breaks = c(-1,0,1,2,3,9,100), labels=c("0","1","2","3","4-9","10+"))

# filter incosistent records
#z<-filter(df_cohort, NRS.Date.Death<date_vacc_2 )
#df_cohort<-anti_join(df_cohort,z,by="EAVE_LINKNO")

#df_cohort1<-readRDS(paste0(Location,"/EAVE/GPanalysis/progs/UA/first_dose_failure/data/df_cohort_ve_failure.RDS"))

# combining censor date: hospitalisation/death, 2nd vaccinaiton, end of follow-up
df_cohort_vacc <- filter(df_cohort, !is.na(date_vacc_1)) %>%
  mutate(analysis_end_date = pmin(date_hosp_covid,covid_death_date))
df_cohort_vacc$analysis_end_date <- coalesce(df_cohort_vacc$analysis_end_date,df_cohort_vacc$date_hosp_covid)
df_cohort_vacc$analysis_end_date <- coalesce(df_cohort_vacc$analysis_end_date,df_cohort_vacc$covid_death_date)
a_end<-max(df_cohort_vacc$analysis_end_date,na.rm = TRUE)
df_cohort_vacc$analysis_end_date <- coalesce(df_cohort_vacc$analysis_end_date,df_cohort_vacc$date_vacc_2)
df_cohort_vacc<-mutate(df_cohort_vacc,analysis_end_date=if_else(analysis_end_date>a_end,a_end,analysis_end_date))
#df_cohort_vacc$analysis_end_date[is.na(df_cohort_vacc$analysis_end_date)]<-max(df_cohort_vacc$date_vacc_1)
df_cohort_vacc$analysis_end_date[is.na(df_cohort_vacc$analysis_end_date)]<-a_end

#change analysis end date for coxph model
#a_end<-max(df_cohort_vacc$date_vacc_1)
df_cohort_vacc<-mutate(df_cohort_vacc,analysis_end_date_c=if_else(analysis_end_date<date_vacc_1,a_end, analysis_end_date))
df_cohort_vacc<-filter(df_cohort_vacc,date_vacc_1<=a_end)

# variable to flag event of interest for all combined
df_cohort_vacc<-mutate(df_cohort_vacc,event_all1=if_else(!is.na(date_hosp_covid)|!is.na(covid_death_date),1L, 0L))
df_cohort_vacc<-mutate(df_cohort_vacc,event_all1=if_else(analysis_end_date<=date_vacc_1,0L, event_all1))

#df_cohort_vacc<-mutate(df_cohort_vacc,time_to_event_temp=as.numeric(analysis_end_date_c-date_vacc_1))
#df_cohort_vacc$time_to_event_temp<-cut(df_cohort_vacc$time_to_event_temp, breaks = c(-1,7,17,Inf), labels=c("0:6","7:13","post 14"))

z<-filter(z_chrt_desc,vacc1==1)
z<-select(z,EAVE_LINKNO_uv,event,event_hosp,event_death,in_hosp_status,prev_positive_status,
          care_home_elderly,admission_date)%>%
  mutate(presence=1)
df_cohort_vacc<-left_join(df_cohort_vacc,z,
                          by=c("EAVE_LINKNO"="EAVE_LINKNO_uv"))
df_cohort_vacc<-filter(df_cohort_vacc,!is.na(presence))
length(which(df_cohort_vacc$event==1))
colnames(df_cohort_vacc)[colnames(df_cohort_vacc)=="event"]<-"event_old"
#colnames(df_cohort_vacc)[colnames(df_cohort_vacc_g)=="event"]<-"event_old"
#df_cohort_vacc<-mutate(df_cohort_vacc,analysis_end_date_c1=analysis_end_date_c)
#df_cohort_vacc$analysis_end_date_c<-df_cohort_vacc$admission_date

## variables for time varying cox model
df_cohort_vacc<-mutate(df_cohort_vacc,
                       date_preiod_0007 = if_else(analysis_end_date_c>(date_vacc_1+6),(date_vacc_1+6),analysis_end_date_c))
df_cohort_vacc<-mutate(df_cohort_vacc,
                       date_preiod_0714 = if_else(analysis_end_date_c>(date_vacc_1+13),(date_vacc_1+13),analysis_end_date_c))
df_cohort_vacc<-mutate(df_cohort_vacc,
                       date_preiod_1421 = if_else(analysis_end_date_c>(date_vacc_1+20),(date_vacc_1+20),analysis_end_date_c))
df_cohort_vacc<-mutate(df_cohort_vacc,
                       date_preiod_2128 = if_else(analysis_end_date_c>(date_vacc_1+27),(date_vacc_1+27),analysis_end_date_c))
df_cohort_vacc<-mutate(df_cohort_vacc,
                       date_preiod_2835 = if_else(analysis_end_date_c>(date_vacc_1+34),(date_vacc_1+34),analysis_end_date_c))
df_cohort_vacc<-mutate(df_cohort_vacc,
                       date_preiod_3542 = if_else(analysis_end_date_c>(date_vacc_1+41),(date_vacc_1+41),analysis_end_date_c))
df_cohort_vacc<-mutate(df_cohort_vacc, date_preiod_42o = analysis_end_date_c)


# z<-filter(z_chrt_desc,vacc1==1)
# z<-select(z,EAVE_LINKNO_uv,event,event_hosp,event_death,in_hosp_status,prev_positive_status,
#           care_home_elderly,admission_date)%>%
#   mutate(presence=1)
# df_cohort_vacc<-left_join(df_cohort_vacc,z,
#                            by=c("EAVE_LINKNO"="EAVE_LINKNO_uv"))
# df_cohort_vacc<-filter(df_cohort_vacc,!is.na(presence))
# length(which(df_cohort_vacc$event==1))
# colnames(df_cohort_vacc)[colnames(df_cohort_vacc)=="event"]<-"event_old"
# #colnames(df_cohort_vacc)[colnames(df_cohort_vacc_g)=="event"]<-"event_old"
# df_cohort_vacc<-mutate(df_cohort_vacc,analysis_end_date_c1=analysis_end_date_c)
# df_cohort_vacc$analysis_end_date_c<-df_cohort_vacc$admission_date

#df_cohort_vacc1<-readRDS(paste0(Location,"/EAVE/GPanalysis/progs/UA/first_dose_failure/data/df_cohort_ve_failure_vacc.RDS"))
#df_cohort_vacc_g<-gather(df_cohort_vacc,"date_period","end_date",date_preiod_0007:date_preiod_42o)
df_cohort_vacc_g<-gather(df_cohort_vacc,"date_period","end_date",date_preiod_0007,date_preiod_0714,
                         date_preiod_1421,date_preiod_2128,date_preiod_2835,date_preiod_3542,
                         date_preiod_42o)
df_cohort_vacc_g<-arrange(df_cohort_vacc_g,EAVE_LINKNO,end_date)
df_cohort_vacc_g<-df_cohort_vacc_g %>% 
  group_by(EAVE_LINKNO,end_date) %>%
  filter(!duplicated(EAVE_LINKNO,end_date))
df_cohort_vacc_g<-df_cohort_vacc_g %>% 
  group_by(EAVE_LINKNO) %>%
  mutate(start_date=lag(end_date,1))
df_cohort_vacc_g$start_date<-coalesce(df_cohort_vacc_g$start_date,df_cohort_vacc_g$date_vacc_1)
df_cohort_vacc_g<-mutate(df_cohort_vacc_g,end_date1=if_else(end_date==start_date,end_date+1,end_date))
df_cohort_vacc_g<-df_cohort_vacc_g %>% 
  group_by(EAVE_LINKNO) %>%
  mutate(start_time_date=first(start_date))
df_cohort_vacc_g$start_time<-df_cohort_vacc_g$start_date-df_cohort_vacc_g$start_time_date
df_cohort_vacc_g$end_time<-df_cohort_vacc_g$end_date1-df_cohort_vacc_g$start_time_date
df_cohort_vacc_g$event_old<-as.integer(df_cohort_vacc_g$event_old)
df_cohort_vacc_g<-df_cohort_vacc_g %>%
  group_by(EAVE_LINKNO) %>%
  mutate(event=if_else(row_number()==n(),event_old,0L))
##############
#saveRDS(df_cohort_vacc_g, "/conf/EAVE/GPanalysis/progs/UA/first_dose_failure/data/df_cohort_coxph_09-4-2021.RDS")
#saveRDS(df_cohort_vacc, "/conf/EAVE/GPanalysis/progs/UA/first_dose_failure/data/df_cohort_ve_failure_vacc_09-4-2021.RDS")
#saveRDS(df_cohort, "/conf/EAVE/GPanalysis/progs/UA/first_dose_failure/data/df_cohort_ve_failure_09-4-2021.RDS")
# #saveRDS(df_cohort, paste0(project_path,"/output/temp/df_cohort.rds"))
# #df_cohort <- readRDS(paste0(project_path,"/output/temp/df_cohort.rds"))

#saveRDS(covid_death, "/conf/EAVE/GPanalysis/progs/UA/first_dose_failure/data/covid_death_rachel.RDS")
#saveRDS(covid_hospitalisations, "/conf/EAVE/GPanalysis/progs/UA/first_dose_failure/data/covid_hospitalisations_rachel.RDS")
#saveRDS(covid_hosp_death, "/conf/EAVE/GPanalysis/progs/UA/first_dose_failure/data/covid_hosp_death_rachel.RDS")
###############
# # checking hosp accquired covid
# 
# all_hospitalisations  <- readRDS("/conf/EAVE/GPanalysis/data/any_hospitalisation_post_01112020.rds")
# 
# cases_severe_dates <- readRDS("/conf/EAVE/GPanalysis/data/cases_severe_dates.rds")
# cases_severe_dates<-select(cases_severe_dates,EAVE_LINKNO,SPECIMENDATE)
# all_hospitalisations<-left_join(all_hospitalisations,cases_severe_dates)
# 
# z<-filter(all_hospitalisations,admission_date>=a_begin)
# z<-mutate(z,hosp_accquired_cov=if_else(SPECIMENDATE-admission_date>=3&SPECIMENDATE-admission_date<=6,1L,0L))
# z<-select(z,EAVE_LINKNO,hosp_accquired_cov)
# z$hosp_accquired_cov[is.na(z$hosp_accquired_cov)]<-0
# 
# df_cohort_vacc$hosp_accquired_cov<-NULL
# df_cohort_vacc<-left_join(df_cohort_vacc,z)
# df_cohort_vacc$hosp_accquired_cov[is.na(df_cohort_vacc$hosp_accquired_cov)]<-0
# df_cohort_vacc<- filter(df_cohort_vacc, !duplicated(EAVE_LINKNO))
# table(df_cohort_vacc$event_all,df_cohort_vacc$hosp_accquired_cov)
##############################
