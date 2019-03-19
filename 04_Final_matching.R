
#Create 4 new tables(CDI pos/male, CDI neg/male, CDI pos/female, CDI neg/female)
#Arrange each table by age
#Bind tables horrizontally so that you can build stratum of matched pairs
#Assess mean diff in age
#Re-create a new table build vertically with the new coulm status added. 

#Load Library

library(tidyverse)

#read in data
mtable <- read_csv("02.2_Final_table_with_meds.csv")

#CDI positive and male arranged by age in ascending order
male_cdi_pos <- mtable %>% 
  filter(gender == "M", cdi_status == 1) %>% 
  arrange(age)

dim(male_cdi_pos) #96

#CDI negative and male arranged by age in ascending order

male_cdi_neg<- mtable %>% 
  filter(gender == "M", cdi_status == 0) %>% 
  arrange(age)

dim(male_cdi_neg) #96


#CDI positive and female arranged by age in ascending order

female_cdi_pos<- mtable %>% 
  filter(gender == "F", cdi_status == 1) %>% 
  arrange(age)
dim(female_cdi_pos) #109


#CDI negative and female arranged by age in ascending order
female_cdi_neg<- mtable %>% 
  filter(gender == "F", cdi_status == 0) %>% 
  arrange(age)
dim(female_cdi_neg) #109

#Male table
#Remove excess columns and keep only study_num, cdi_status, gender, age

male_cdi_pos1<- male_cdi_pos %>% 
  select(study_num, cdi_status, gender, age)


male_cdi_neg1<- male_cdi_neg %>% 
  select(study_num, cdi_status, gender, age)


#Rename male_cdi_neg1 
colnames(male_cdi_neg1) <- c("study_num.1","cdi_status.1","gender.1","age.1")

#Join male_cdi_pos and male_cdi_neg

male_table<- cbind(male_cdi_pos1, male_cdi_neg1)

#Create new column with mutate called stratum that contains the matched pair number 1:86
male_table1<- male_table %>% 
  mutate(stratum =1:nrow(male_table))

#Evaluate diff in age
#median_age_diff =1.2,  mindiff=0 maxdiff=5.2

age_diff_table_male<- male_table1 %>% 
mutate(age_diff =abs(age-age.1)) %>% 
  group_by(cdi_status) %>% 
  summarize(median_age_diff=median(age_diff), mindiff=min(age_diff), maxdiff=max(age_diff))


#Keep only the study_num and startum column for CDI pos males
names(male_table1)
male_table_pos_strat<- male_table1[c(1,9)]

#Keep only the study_num and startum column for CDI neg males
male_table_neg_strat <- male_table1[c(5,9)]

#Rename "study_num.1" to study_num

colnames(male_table_neg_strat) <- c("study_num","stratum")


#Left join to add starum to to male_cdi_pos

dim(male_cdi_pos)
male_cdi_pos_strat1 <- merge(male_cdi_pos, y = male_table_pos_strat, by = "study_num", all = TRUE)  
dim(male_cdi_pos_strat1)

#Left join to add starum to to male_cdi_neg

dim(male_cdi_neg)
male_cdi_neg_strat1<-merge(male_cdi_neg, y =male_table_neg_strat, by = "study_num", all = TRUE)
dim(male_cdi_neg_strat1)

####Master Male table
#Create a male table

total_male_table_strata <-rbind(male_cdi_neg_strat1, male_cdi_pos_strat1)
dim(total_male_table_strata)


#female tables
#Remove excess columns and keep only study_num, cdi_status, gener, age
female_cdi_pos1<- female_cdi_pos %>% 
  select(study_num, cdi_status, gender, age)

female_cdi_neg1<- female_cdi_neg %>% 
  select(study_num, cdi_status, gender, age)

#Rename female_cdi_neg1 
colnames(female_cdi_neg1) <- c("study_num.1","cdi_status.1","gender.1","age.1")

#Join female_cdi_pos and female_cdi_neg

female_table<- cbind(female_cdi_pos1, female_cdi_neg1)


#Create new column with mutate called stratum that contains the matched pair number 1:86
female_table1<- female_table %>% 
  mutate(stratum =1:nrow(female_table))

#Eval diff in age
#median_age_diff =1.0,  mindiff=0 maxdiff=4.1
age_diff_table_female<- female_table1 %>% 
  mutate(age_diff =abs(age-age.1)) %>% 
  group_by(cdi_status) %>% 
  summarize(median_age_diff=median(age_diff), mindiff=min(age_diff), maxdiff=max(age_diff))


#Keep only the study_num and startum column for CDI pos females
names(female_table1)
female_table_pos_strat<- female_table1[c(1,9)]

#Keep only the study_num and startum column for CDI neg females

female_table_neg_strat <- female_table1[c(5,9)]

#Rename "study_num.1" to study_num

colnames(female_table_neg_strat) <- c("study_num","stratum")

#Left join to add starum to female_cdi_pos

dim(female_cdi_pos)
female_cdi_pos_strat2 <- merge(female_cdi_pos, y = female_table_pos_strat, by = "study_num", all = TRUE)    
dim(female_cdi_pos_strat2)

#Left join to add starum to to female_cdi_neg

dim(female_cdi_neg)
female_cdi_neg_strat2<-merge(female_cdi_neg, y =female_table_neg_strat, by = "study_num", all = TRUE)  

dim(female_cdi_neg_strat2)

####Master Male table
#Create a male table
total_female_table_strata <-rbind(female_cdi_neg_strat2, female_cdi_pos_strat2)
dim(total_female_table_strata)

#Combine male and female table vertically 

clean_table_strata<-rbind(total_female_table_strata,total_male_table_strata)

dim(clean_table_strata)

write_csv(clean_table_strata, '04p_table_strata.csv')
