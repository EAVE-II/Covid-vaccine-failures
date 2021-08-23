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

project_path <- paste0(Location,"EAVE/GPanalysis/progs/RM/Vaccine/Vaccine_waning")
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

##### Data set-up ####
# Vaccine data from 8th Dec until end date
z_vaccinations <- filter(df_vaccinations_rachel, date_vacc_1 <= a_end) %>% 
  mutate(vacc_type_2 = if_else(date_vacc_2 >= a_end, NA_character_ , vacc_type_2),
         date_vacc_2 = as.Date(ifelse(date_vacc_2 >= a_end, NA, date_vacc_2), origin=as.Date("1970-01-01")) )

# Total cohort 
z_chrt_desc <- df_cohort_rachel %>%
  dplyr::rename(EAVE_LINKNO_uv = EAVE_LINKNO) %>%
  mutate(ur6_2016_name = replace_na(ur6_2016_name, "NA")) %>%
  # Add in age group
  mutate(age_grp = case_when(ageYear < 65 ~"18-64", 
                             ageYear < 80 ~"65-79",
                             TRUE ~ "80+"))


# Add in household information
Cohort_Household <- readRDS("/conf/EAVE/GPanalysis/outputs/temp/Cohort_Household.rds") %>%
  mutate(n_hh_gp = cut(n_hh, breaks=c(0,1,2,5,10,30,100,max(n_hh)),
                       labels=c("1", "2", "3-5", "6-10", "11-30", "31-100", "101+")))%>% 
  mutate(ave_hh_age=if_else(is.na(ave_hh_age), mean(ave_hh_age, na.rm=T), ave_hh_age) )

# Eliminate care homes
z_chrt_desc <- z_chrt_desc %>%
  left_join(select(Cohort_Household, EAVE_LINKNO,
                   n_hh_gp, ave_hh_age, care_home_elderly),
            by= c("EAVE_LINKNO_uv"= "EAVE_LINKNO")) %>% 
  filter(care_home_elderly == 0)%>% 
  dplyr::select(-care_home_elderly)



# Add in vaccination data
z_chrt_desc <- z_chrt_desc %>%
  left_join(z_vaccinations, by=c("EAVE_LINKNO_uv" = "EAVE_LINKNO")) %>%
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

colnames(z_chrt_desc)



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
  mutate(event_death = ifelse(NRS.Date.Death <= date_vacc_1,"0", "1")) %>%
  mutate(event_death = ifelse(is.na(date_vacc_1) & !is.na(NRS.Date.Death), "1", event_death)) %>%
  mutate(event_death = replace_na(event_death, "0"))# %>%
#select(event, admission_date, date_vacc_1, vacc1, vacc_type)



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

########
z<-filter(z_chrt_desc,vacc1==1)
z<-select(z,EAVE_LINKNO_uv,event,event_hosp,event_death)%>%
  mutate(presence1=1)
df_cohort_vacc1<-left_join(df_cohort_vacc,z,
                           by=c("EAVE_LINKNO"="EAVE_LINKNO_uv"))
df_cohort_vacc1<-filter(df_cohort_vacc1,!is.na(presence1))
length(which(df_cohort_vacc1$event==1))
z<-filter(df_cohort_vacc1,event==1)

# z<-filter(z_chrt_desc,vacc1==1)
# z<-select(z,EAVE_LINKNO_uv,event,event_hosp)%>%
#   mutate(presence=1)
# colnames(df_cohort_vacc_g)[colnames(df_cohort_vacc_g)=="event"]<-"event_old"
# df_cohort_vacc_g1<-left_join(df_cohort_vacc_g,z,
#                            by=c("EAVE_LINKNO"="EAVE_LINKNO_uv"))
# df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,!is.na(presence))
df_cohort_vacc_g1<-df_cohort_vacc_g
length(which(df_cohort_vacc_g1$event==1))
z<-filter(df_cohort_vacc_g1,event==1)


# # slecting events 14 days post vacc
z1<-filter(df_cohort_vacc_g1,event==1)
z2<-filter(z1,date_period=="date_preiod_0007"|date_period=="date_preiod_0714")
df_cohort_vacc_g_orig<-df_cohort_vacc_g
df_cohort_vacc_g1 <- df_cohort_vacc_g1 %>%
  filter(!EAVE_LINKNO %in% z2$EAVE_LINKNO)
df_cohort_vacc_g1<-filter(df_cohort_vacc_g1,date_period!="date_preiod_0007"&
                           date_period!="date_preiod_0714")
df_cohort_vacc1 <- df_cohort_vacc1 %>%
  filter(!EAVE_LINKNO %in% z2$EAVE_LINKNO)

length(which(df_cohort_vacc_g1$event==1))
length(which(df_cohort_vacc_g$event==1&(df_cohort_vacc_g$date_period=="date_preiod_0007"|
                                          df_cohort_vacc_g$date_period=="date_preiod_0714")))

df_cohort_vacc<-df_cohort_vacc1
df_cohort_vacc_g<-df_cohort_vacc_g1
rm(df_cohort_vacc_g1,df_cohort_vacc1)


### add missing variables ###
# add HB flag
z <- read_csv(paste0(Location,"/EAVE/GPanalysis/data/restored/map_files/Datazone2011Lookup.csv")) %>%
  dplyr::select(DataZone, InterZone, Council, HB)
df_cohort_vacc_g <- df_cohort_vacc_g %>% left_join(z, by="DataZone") %>%
  mutate(HB = if_else(is.na(HB),"Unknown", HB),
         InterZone = if_else(is.na(InterZone),"Unknown", InterZone),
         Council = if_else(is.na(Council),"Unknown", Council))

df_cohort_vacc$test_before_dec8<-NULL
df_cohort_vacc_g$test_before_dec8<-NULL
z<-select(z_chrt_desc,EAVE_LINKNO_uv,test_before_dec8,n_hh_gp)%>%
  mutate(presence=1)
df_cohort_vacc<-left_join(df_cohort_vacc,z,
                          by=c("EAVE_LINKNO"="EAVE_LINKNO_uv"))
df_cohort_vacc_g<-left_join(df_cohort_vacc_g,z,
                          by=c("EAVE_LINKNO"="EAVE_LINKNO_uv"))
df_cohort_vacc<-mutate(df_cohort_vacc,age_gp1 = 
                         cut(ageYear, breaks = c(-Inf, 64,79,Inf),
                             labels=c("18-64","65-79","80+")))
df_cohort_vacc_g<-mutate(df_cohort_vacc_g,age_gp1 = 
                           cut(ageYear, breaks = c(-Inf, 64,79,Inf),
                               labels=c("18-64","65-79","80+")))

## selecting covatiates for tables
# z<-filter(z_chrt_desc,vacc1==1)
# z<-select(z,EAVE_LINKNO_uv,n_hh_gp,)%>%
#   mutate(presence=1)
##############################
