
library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(tidyr)
library(readr)
library(here)

#Import dataframe

meds_table2 <- read_csv("02.0j_meddata.csv")

#Nest all meds to one study_num

meds_table3 <- meds_table2 %>%
  group_by(study_num,ipratropium ) %>%
  nest()

#Mutate to create new Catagories antihistamine/inhailedCS/intranasalCS/mastcell  
meds_table4 <- meds_table3 %>% 
  mutate("antihistamine" = ifelse(grepl(c("cetirizine|diphenhydramine|chlorpheniramine|fexofenadine|desloratadine
                                           levocetirizine|hydroxyzine|brompheniramine|loratadine|olopatadine|azelastine"), 
                                         meds_table3$data), 1, 0))

meds_table5 <- meds_table4 %>% 
  mutate("intranasal_cs" = ifelse(grepl(c("fluticasone"), 
                                         meds_table4$data), 1, 0))

meds_table6 <- meds_table5 %>% 
  mutate("intranasal_vc" = ifelse(grepl(c("oxymetazoline|phenylephrine|pseudoephedrine"), 
                                      meds_table5$data), 1, 0))

meds_table7 <- meds_table6 %>% 
  mutate("antileukotriene" = ifelse(grepl(c("montelukast|zafirlukast"), 
                                        meds_table6$data), 1, 0))

meds_table8 <- meds_table7 %>% 
  mutate("mc_stable" = ifelse(grepl(c("cromolyn"), 
                                         meds_table7$data), 1, 0))

meds_table9 <- meds_table8 %>% 
  mutate("atropine_nasal" = ifelse(grepl(c("atropine"), 
                                    meds_table8$data), 1, 0))

#Hide nested data
meds_table10<- meds_table9[-3]

#Create an any anti-allergy mediation catagory
meds_table11 <- meds_table10 %>% 
  mutate(any_all_med =(ifelse(meds_table10$ipratropium==1|meds_table10$antihistamine==1|intranasal_vc==1|
                                meds_table10$intranasal_cs==1|meds_table10$antileukotriene==1|meds_table10$mc_stable==1|
                                meds_table10$atropine_nasal, 1,0)))



write_csv(meds_table11, here('02.1j_clean_meddata.csv'))

