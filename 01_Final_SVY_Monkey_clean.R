#This file contains the code to clean survey data
#File was downloaded directly from surveymonkey as 1) "all response data", 2) condensed 3) current View 4) XLS
#File downloaded on 11.18.2018
#File name: "Clostridium difficile Pets Survey 11.18.2018.xlsx"

#Install packages including Latest Janitor Package with Github
# devtools::install_github("sfirke/janitor")
# install.packages("devtools")
# install.packages("dplyr")
# install.packages("visdat")
# install.packages("janitor")
# install.packages("readxl")
# install.packages("tidyr")
# install.packages("tidyverse")


#load libraries
library(janitor) #note using github version
library(dplyr)
library(visdat)
library(naniar)
library(readxl)
library(devtools)
library(tidyr)
library(tidyverse)
library(here)


#Import svy_monkey dat a(see above for details). 
#Downloaded on 11.18.2018. 
#File name: "Clostridium difficile Pets Survey 11.18.2018.xlsx

svy_monkey1 <- read_excel("Clostridium difficile Pets Survey 11.18.2018.xlsx")

#Remove unneeded rows and columns

svy_monkey2<- svy_monkey1  %>%
  clean_names() %>% 
  remove_empty(c("rows", "cols"))

#Remove unwanted columns

svy_monkey3 <-svy_monkey2 %>% 
  subset(select = -c(respondent_id, collector_id, start_date, end_date, ip_address, 
                     i_would_like_to_continue_and_complete_this_22_question_survey))

#Remove observations with NAs in the study_num 

svy_monkey4 <- svy_monkey3 %>% 
  filter(!is.na(study_id_number))
dim(svy_monkey4)


#Rename ugly columns names
svy_monkey5 <- svy_monkey4 %>% 
  rename(study_num = study_id_number) %>% 
  rename(restaurant3 = do_you_eat_at_restaurants_more_than_3_times_per_week) %>% 
  rename(dessert3 = do_you_eat_dessert_more_than_3_times_per_week) %>% 
  rename(meat3 = do_you_eat_meat_more_than_3_times_per_week) %>% 
  rename(salad3 = do_you_eat_salads_more_than_3_times_per_week) %>% 
  rename(redwine3 = do_you_drink_red_wine_more_than_3_times_per_week) %>% 
  rename(dairy = how_many_servings_of_dairy_do_you_consume_each_week_1_serving_8oz_milk_this_includes_cheeses_and_yogurt_but_not_butter) %>% 
  rename(vitamin = do_you_take_either_a_multivitamin_daily_or_vitamin_d_supplements_daily) %>% 
  rename(probiotic = do_you_take_probiotics_daily) %>% 
  rename(antibiotic = did_you_use_any_antibiotics_in_the_three_months_before_or_prior_to_or_at_the_time_of_your_c_diff_test_test_date_in_letter_if_no_enter_no_if_yes_please_enter_yes_followed_by_the_name_of_the_antibiotic_i_e_yes_keflex_or_if_unknown_yes_unknown) %>% 
  rename(omeprazole = did_you_use_any_of_the_following_acid_blocking_medications_in_the_following_4_weeks_prior_to_or_at_the_time_of_your_c_diff_test_test_date_in_letter) %>% 
  rename(esomeprazole = x_1) %>%
  rename(prevacid = x_2) %>%
  rename(dexlansoprazole = x_3) %>% 
  rename(rabeprazole = x_4) %>%
  rename(pantoprazole = x_5) %>% 
  rename(famotidine = x_6) %>%
  rename(ranitidine = x_7) %>% 
  rename(cimetidine = x_8) %>% 
  rename(nizatidine = x_9) %>% 
  rename(acid_blocker = x_10) %>%
  rename(cdi = have_you_ever_in_your_life_tested_positive_for_an_intestinal_infection_called_clostridium_difficile_c_diff_that_required_antibiotics) %>% 
  rename(health_care = does_anyone_who_lives_in_your_household_work_in_a_health_care_setting_where_there_are_patients_being_treated_for_illnesses_hospital_clinic_etc) %>% 
  rename(hospital3 = were_you_admitted_to_a_hospital_in_the_three_months_prior_to_at_time_of_c_diff_test_test_date_in_letter) %>% 
  rename(hc_facility3 = did_you_live_in_a_health_care_facility_nursing_home_rehabilitation_center_prior_to_or_at_the_time_of_your_c_diff_test_test_date_in_letter) %>% 
  rename(feeding_adl = prior_to_your_c_diff_test_on_supply_date_if_not_known_did_you_have_difficulty_or_require_assistance_in_daily_activity_such_as) %>% 
  rename(walking_adl = x_11) %>% 
  rename(transfer_adl = x_12) %>% 
  rename(dressing_adl = x_13) %>% 
  rename(grooming_adl = x_14) %>% 
  rename(toileting_adl = x_15) %>% 
  rename(dog_allerg = are_you_allergic_to_dogs) %>% 
  rename(cat_allerg = are_you_allergic_to_cats) %>% 
  rename(dog = do_you_have_a_dog_that_sleeps_in_your_house_each_night) %>% 
  rename(dog_outside = does_your_dog_go_outside_each_day) %>% 
  rename(cat = do_you_have_a_cat_that_sleeps_in_your_house_each_night) %>% 
  rename(cat_outside = does_your_cat_go_outside_each_day)

#Remove Row 1

svy_monkey6 <- svy_monkey5[-1,]

#Visual Representation of missing Data  
vis_dat(svy_monkey6)
vis_miss(svy_monkey6)

# Mutate to add additional Column antibiotics3mo 
# (where Yes ABx = Yes, No and non-ABx =No)
# Note that some yes = entocort, zofran, these will be sorted to No
# This was personally verified to ensure it transferred 
# appopriately
svy_monkey7<- svy_monkey6

svy_monkey7$antibiotic[which(svy_monkey7$antibiotic%in% c("No", "NO", "N0","no", "entocort", "unknown", "Nop", "not sure"))] = "No"


svy_monkey8 <-  mutate(svy_monkey7, antibiotics3mo = ifelse(grepl("No", svy_monkey7$antibiotic), "No", "Yes"))


#Reorder columns so that Antibiotics next to antibiotics3mo 
#Inorder to verify the accurate mutate
names(svy_monkey8)

svy_monkey9 <- svy_monkey8[c(1:10,38, 11:37)] #Verified

#Convert PPI  and H2RA observations into numeric factors 
# where 1 =ppi/H2RA used and Na = ppi/H2RA not used
#Transfer Verified

svy_monkey9$omeprazole <-as.numeric(factor(svy_monkey9$omeprazole, levels = "Omeprazole (Prilosec)", ordered =TRUE))
svy_monkey9$esomeprazole <-as.numeric(factor(svy_monkey9$esomeprazole, levels = "Esomeprazole (Nexium)", ordered =TRUE))
svy_monkey9$prevacid <-as.numeric(factor(svy_monkey9$prevacid, levels = "Prevacid (lansoprazole)", ordered =TRUE)) 
svy_monkey9$dexlansoprazole <-as.numeric(factor(svy_monkey9$dexlansoprazole, levels = "Dexlansoprazole (Dexilant)", ordered =TRUE))                                                
svy_monkey9$rabeprazole <-as.numeric(factor(svy_monkey9$rabeprazole, levels = "Rabeprazole (Aciphex)", ordered =TRUE)) 
svy_monkey9$pantoprazole <-as.numeric(factor(svy_monkey9$pantoprazole, levels = "Pantoprazole (Protonix)", ordered =TRUE)) 
svy_monkey9$famotidine <-as.numeric(factor(svy_monkey9$famotidine, levels = "Famotidine (Pepcid)", ordered =TRUE))
svy_monkey9$ranitidine <-as.numeric(factor(svy_monkey9$ranitidine, levels = "Ranitidine (Zantac)", ordered =TRUE)) 
svy_monkey9$cimetidine <-as.numeric(factor(svy_monkey9$cimetidine, levels = "Cimetidine (Tagamet)", ordered =TRUE))                                                
svy_monkey9$nizatidine <-as.numeric(factor(svy_monkey9$nizatidine, levels = "Nizatidine (Axid)", ordered =TRUE)) 

#Convert NAs to 0 (now 1=PPI/H2RA use and 0= no PPI/H2RA use)

svy_monkey10<- svy_monkey9

svy_monkey10$omeprazole[is.na(svy_monkey9$omeprazole)] <- 0
svy_monkey10$esomeprazole[is.na(svy_monkey9$esomeprazole)] <- 0
svy_monkey10$prevacid[is.na(svy_monkey9$prevacid)] <- 0
svy_monkey10$dexlansoprazole[is.na(svy_monkey9$dexlansoprazole)] <- 0                                           
svy_monkey10$rabeprazole[is.na(svy_monkey9$rabeprazole)] <- 0
svy_monkey10$pantoprazole[is.na(svy_monkey9$pantoprazole)] <- 0
svy_monkey10$famotidine[is.na(svy_monkey9$famotidine)] <- 0
svy_monkey10$ranitidine[is.na(svy_monkey9$ranitidine)] <- 0
svy_monkey10$cimetidine[is.na(svy_monkey9$cimetidine)] <- 0                                             
svy_monkey10$nizatidine[is.na(svy_monkey9$nizatidine)] <- 0

#Collpase all PPI and H2RA columns into one column 

str(svy_monkey10)

svy_monkey11 <- svy_monkey10 %>% 
  mutate(ppi= omeprazole + esomeprazole + prevacid + dexlansoprazole + rabeprazole + pantoprazole, h2ra =nizatidine + cimetidine + ranitidine +famotidine)

#Note there are 2 peopele on 2 PPIs
#Note there is 1 person on 2 H2RAs
#Change values of 2 to 1 as this catagory only indicates PPI use or not

svy_monkey11$ppi[svy_monkey11$ppi ==2] <-1
svy_monkey11$h2ra[svy_monkey11$h2ra ==2] <-1

#Create new catagory called acid_blocker_yes

svy_monkey12 <- svy_monkey11 %>% 
  mutate(acid_blocker_yes= ppi + h2ra)

#Change values of 2 to 1 as this catagory only indicates PPI+H2RA use or not

svy_monkey12$acid_blocker_yes[svy_monkey12$acid_blocker_yes==2] <-1


#Change adls to factors where Independent =1, Some Assistance = 2, Full Assistance =3

svy_monkey16<- svy_monkey12

svy_monkey16$feeding_adl <-as.numeric(factor(svy_monkey16$feeding_adl, levels = c("Independent", "Some Assistance", "Full Assistance"), ordered =TRUE))
svy_monkey16$walking_adl <-as.numeric(factor(svy_monkey16$walking_adl, levels = c("Independent", "Some Assistance", "Full Assistance"), ordered =TRUE))
svy_monkey16$transfer_adl <-as.numeric(factor(svy_monkey16$transfer_adl, levels = c("Independent", "Some Assistance", "Full Assistance"), ordered =TRUE)) 
svy_monkey16$dressing_adl<-as.numeric(factor(svy_monkey16$dressing_adl, levels = c("Independent", "Some Assistance", "Full Assistance"), ordered =TRUE)) 
svy_monkey16$grooming_adl  <-as.numeric(factor(svy_monkey16$grooming_adl , levels = c("Independent", "Some Assistance", "Full Assistance"), ordered =TRUE))
svy_monkey16$toileting_adl <-as.numeric(factor(svy_monkey16$toileting_adl, levels = c("Independent", "Some Assistance", "Full Assistance"), ordered =TRUE)) 


#Replace study_num 418 and 574 as these patients answered 1 to walking and leftout transfer
svy_monkey16.1<-  svy_monkey16
svy_monkey16.1$transfer_adl[is.na(svy_monkey16.1$transfer_adl)] <- 1


#Create an ADL total score, max = 18
svy_monkey17 <- svy_monkey16.1 %>% 
  mutate(adl_total = feeding_adl + walking_adl + transfer_adl + dressing_adl + grooming_adl + toileting_adl)

#Reorder so that adl_total is beside other adls and verify. 
#This has been verified

test1 <- svy_monkey17[c(27:32,42)] 

#Removed unwanted columns
#Create clean and ordered table

svy_monkey18<- svy_monkey17[c(1:9, 11, 39:41, 23:26, 42, 33:38)]
svy_monkey18.1<- svy_monkey18
svy_monkey19<- svy_monkey18.1

#View(svy_monkey18.1 %>% 
       #filter(study_num==418| study_num==574))

#Create all columns as factors then convert to numeric 
# variables

svy_monkey19$restaurant3 <-ifelse(svy_monkey18.1$restaurant3=="Yes",1,0)
svy_monkey19$dessert3 <-ifelse(svy_monkey18.1$dessert3=="Yes",1,0)
svy_monkey19$meat3 <-ifelse(svy_monkey18.1$meat3=="Yes",1,0)
svy_monkey19$salad3 <-ifelse(svy_monkey18.1$salad3=="Yes",1,0)
svy_monkey19$redwine3 <-ifelse(svy_monkey18.1$redwine3=="Yes",1,0)
svy_monkey19$vitamin <-ifelse(svy_monkey18.1$vitamin=="Yes",1,0)
svy_monkey19$probiotic <-ifelse(svy_monkey18.1$probiotic=="Yes",1,0)
svy_monkey19$antibiotics3mo <-ifelse(svy_monkey18.1$antibiotics3mo=="Yes",1,0)
svy_monkey19$cdi <-ifelse(svy_monkey18.1$cdi=="Yes",1,0)
svy_monkey19$health_care <-ifelse(svy_monkey18.1$health_care=="Yes",1,0)
svy_monkey19$hospital3 <-ifelse(svy_monkey18.1$hospital3=="Yes",1,0)
svy_monkey19$hc_facility3 <-ifelse(svy_monkey18.1$hc_facility3=="Yes",1,0)
svy_monkey19$dog_allerg <-ifelse(svy_monkey18.1$dog_allerg =="Yes",1,0)
svy_monkey19$cat_allerg <-ifelse(svy_monkey18.1$cat_allerg=="Yes",1,0)
svy_monkey19$dog <-ifelse(svy_monkey18.1$dog=="Yes",1,0)
svy_monkey19$dog_outside <-ifelse(svy_monkey18.1$dog_outside=="Yes",1,0)
svy_monkey19$cat <-ifelse(svy_monkey18.1$cat=="Yes",1,0)
svy_monkey19$cat_outside <-ifelse(svy_monkey18.1$cat_outside=="Yes",1,0)
svy_monkey19$dairy <- as.numeric(svy_monkey18.1$dairy)

#Remove all other missing values
#Visualize missing data

#View(svy_monkey19 %>% 
       #filter(study_num==418| study_num==574))

vis_dat(svy_monkey19)
vis_miss(svy_monkey19)

#See which rows have missing values

svy_monkey20<- svy_monkey19
missing1 <- svy_monkey20[!complete.cases(svy_monkey20), ]

#View(missing1)


#Replace study_num 653, 326, 214 with 0 as they do not have dogs or cats therefore their 
#pets do not go outside


svy_monkey21 <- svy_monkey20
svy_monkey21$dog_outside[is.na(svy_monkey21$dog_outside)] <- 0
svy_monkey21$cat_outside[is.na(svy_monkey21$cat_outside)] <- 0


missing2 <- svy_monkey21[!complete.cases(svy_monkey21), ]
#View(missing2)
#View(svy_monkey21)

#View(missing2)
#View(svy_monkey20)

#Remove study_num 657, 108, 260 as it is missing pets data
#dim(svy_monkey21)
svy_monkey22<- svy_monkey21 %>% 
  na.omit()
#dim(svy_monkey22)

#Determine if there are any more missing values
#missing equal 0
vis_miss(svy_monkey22)


#Evaluate for duplicate study_numbers
#3 duplicated responses seen(214, 169, 146), which will be removed

duplicates <- svy_monkey22 %>%
  group_by(study_num) %>% 
  filter(n() > 1)

#View (duplicates)
#Note duplicates include study_num 214, 169, 146

#Filter out duplicates
#dim(svy_monkey22)
svy_monkey23 <-svy_monkey22 %>%
  group_by(study_num) %>% 
  filter(n() < 2)

#dim(svy_monkey23)

write_csv(svy_monkey23, here('01p_smdata.csv'))
