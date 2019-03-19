
# install.packages("leaps")
# install.packages("foreign")
# install.packages("survival")
# install.packages("moderndive")

library(leaps)
library(foreign)
library(survival)
library(moderndive)
library(tidyverse)
library(here)
library(broom)
library(flextable)
library(officer)
library(purrr)
library(knitr)

#read in data from 04p_table_strata.csv
table_strata <- read_csv('04p_table_strata.csv')

#Create a new catagory for any pet
table_strata$pet <- case_when(table_strata$dog+
                                table_strata$cat==0 ~ 0,
                              TRUE ~ 1)
#Create a new catagory for any pet allergy
table_strata$pet_allerg <- case_when(table_strata$dog_allerg+
                                table_strata$cat_allerg==0 ~ 0,
                              TRUE ~ 1)

#Create a new catagory combined SAR/SNF and Hospitalization
table_strata$hc_hosp <- case_when(table_strata$hc_facility3+
                                       table_strata$hospital3==0 ~ 0,
                                     TRUE ~ 1)

table_strata$any_outside_pet <- case_when(table_strata$dog_outside+
                                            table_strata$cat_outside==0 ~ 0,
                                          TRUE ~ 1)

 table_strata1<- table_strata %>% 
select(-c(study_num, cdi))
 
 
##Bivariate Regression
 
#Construct and summarizing multiple univariate models from one chain of tidy code 
#using the _map_ function from the **purrr** package
 
 full_df <-  table_strata1  #1
 predictors_df <- table_strata1 %>%  #2
   dplyr::select(-c(cdi_status, gender, age, race, stratum, mc_stable))
 
 predictors_df %>% 
   map(~survival::clogit(full_df$cdi_status~.x, data=full_df)) %>% 
   map(tidy) -> #4
   list
 
 names(list)<- c("Prior CDI", "Frequent Restaurant Visits","High Dessert Intake", "High Meat Intake", "High Salad Intake", "Frequent Red Wine Intake", "Dairy Intake", "Vitamins", 
                 "Probiotic", "Antibiotic", "PPI",  "H2RA", "Any Acid Suppression", "Healthcare Occupation","Hospitalization", "SAR/SNF",
                 "Total ADL Score ", "Dog Allergy", "Cat Allergy", "Dog Exposure", "Outside Dog", "Cat Exposure", "Outside Cat", "Ipratropium", "Antihistamine", 
                  "Intranasal Corticosteroids", "Intranasal Vasoconstrictor", "Antileukotriene", "Intranasal Atropine",
                  "Any Nasal Decongestant", "Any Pet", "Any Pet Allergy", "Hospitalization/SAR/SNF")
 
 #Table with coefficients and p value
 pred <- names(list) #5
 list %>% 
   bind_rows() %>% #6
   filter(term != "(Intercept)") %>% 
   mutate(predictor = pred) %>% #7
   select(predictor, estimate, p.value) %>% 
   arrange(p.value) -> #8
   bivariate_table
 
 
 list %>% 
   bind_rows() %>% #6
   filter(term != "(Intercept)") %>% 
   mutate(predictor = pred) %>% 
   mutate(odds_ratio = exp(estimate),
          LCB = exp(conf.low),
          UCB = exp(conf.high)) %>% 
   select(predictor, odds_ratio, LCB, UCB, p.value) %>% 
   arrange(p.value) -> #8
   bivariate_table_or
 
 
 kable(bivariate_table_or)
 

#Make graph of ORs and CI with error bars
#Export 7x6 horrizontal to pdf
 

 
 #bivariate_table_or1<- bivariate_table_or %>% 
   #filter(predictor !="Hospitalization/SAR/SNF")
 
 
 uni_graph <-ggplot(bivariate_table_or, aes(reorder(predictor, odds_ratio))) + geom_point(aes(y=odds_ratio)) +
   geom_errorbar(aes(ymin=`LCB`, ymax=`UCB`), width = 0.2) + coord_flip() + scale_y_log10()
 
 uni_graph + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +xlab("CDI Predictors") +ylab("Odds Ratio") +
   geom_hline(yintercept = 1, linetype = "dotted")
 
 #Export 7x6 horrizontal to pdf


#Create Output including pet status
res.clogit2.int <- survival::clogit(cdi_status ~ 
                                      reccurent_cdi+
                                    restaurant3+
                                      dessert3+ 
                                      meat3+
                                      salad3 + 
                                      redwine3+
                                      #dairy+
                                      vitamin+
                                      probiotic+
                                      antibiotics3mo+
                                      ppi+
                                      #h2ra+
                                      acid_blocker_yes +
                                      health_care+
                                      hospital3 +
                                      hc_facility3+
                                      adl_total+  
                                      dog_allerg+
                                      cat_allerg+
                                      dog+
                                      #dog_outside+
                                      #cat+
                                      #cat_outside+
                                      ipratropium +
                                      #antihistamine +
                                      intranasal_cs +
                                      intranasal_vc+
                                      antileukotriene + 
                                      #atropine_nasal + 
                                      any_all_med + 
                                      #pet +
                                    pet_allerg +
                                      hc_hosp+            
                                      strata(stratum), 
                                    table_strata1)

summary(res.clogit2.int)

#Am I supposed to have a model where reccurent cdi is the outcome, what about probiotic? pets?

res.clogit3.int <- survival::clogit(cdi_status ~ hc_facility3 + 
                    probiotic + hospital3 + adl_total +
                    dog + strata(stratum), 
                    table_strata1)

summary(res.clogit3.int)

#Final Regression
#interesting model with cat allergies
res.clogit4.int <- survival::clogit(cdi_status ~  
                #probiotic + 
                  meat3 + hc_hosp + 
                  #hc_facility3 +
                  adl_total + #vitamin +
                  cat_allerg +  
                  salad3 + #health_care + 
                  #h2ra +
                   #antibiotics3mo + 
                 strata(stratum), 
                    table_strata1)
summary(res.clogit4.int)

#Final Interaction based multivariable regression
res.clogit5.int <- survival::clogit(cdi_status ~  
                                      cat_allerg+
                                      cat_allerg:antihistamine +
                                    probiotic +
                                      probiotic:vitamin+
                                      salad3 +
                                      meat3 +
                                     #hc_hosp + 
                                      hc_facility3+
                                      adl_total + #vitamin +
                                        
                                      #restaurant3+
                                      #salad3 + #health_care + 
                                      #h2ra +
                                      #antibiotics3mo + 
                                      strata(stratum), 
                                    table_strata1)
summary(res.clogit5.int)

res.clogit6.int <- survival::clogit(cdi_status ~  
                                      cat_allerg+
                                      #cat_allerg:any_all_med +
                                      #any_all_med +
                                      cat_allerg:intranasal_cs +
                                      #cat_allerg:antihistamine +
                                      #antihistamine +
                                      probiotic +
                                      vitamin +
                                      #probiotic:vitamin+
                                      salad3 +
                                      meat3 +
                                      #hc_hosp + 
                                      hc_facility3+
                                      adl_total + #vitamin +
                                      intranasal_cs + 
                                      #restaurant3+
                                      #salad3 + #health_care + 
                                      #h2ra +
                                      #antibiotics3mo + 
                                      #vitamin:salad3 +
                                      strata(stratum), 
                                    table_strata1)

summary(res.clogit6.int)



model_output3 <- tidy(res.clogit6.int)

#Add in reccurent CDI
res.clogit7.int <- survival::clogit(cdi_status ~ 
                                      reccurent_cdi +
                                      #cat_allerg+
                                      #cat_allerg:any_all_med +
                                      #any_all_med +
                                      #cat_allerg:intranasal_cs +
                                      #cat_allerg:antihistamine +
                                      #antihistamine +
                                      probiotic +
                                      vitamin +
                                      #probiotic:vitamin+
                                      #salad3 +
                                      meat3 +
                                      #hc_hosp + 
                                      hc_facility3+
                                      #adl_total + #vitamin +
                                      #intranasal_cs + 
                                      #restaurant3+
                                      #salad3 + #health_care + 
                                      #h2ra +
                                      #antibiotics3mo + 
                                      #vitamin:salad3 +
                                      strata(stratum), 
                                    table_strata1)

summary(res.clogit7.int)


##Build from scratch
res.clogit8.int <- survival::clogit(cdi_status ~ 
                                      reccurent_cdi+
                                      restaurant3+
                                      dessert3+ 
                                      meat3+
                                      salad3 + 
                                      redwine3+
                                      dairy+
                                      vitamin+
                                      probiotic+
                                      antibiotics3mo+
                                      #ppi+
                                      #h2ra+
                                      acid_blocker_yes +
                                      health_care+
                                      hospital3 +
                                      hc_facility3+
                                      adl_total+  
                                      dog_allerg+
                                      cat_allerg+
                                      dog+
                                      #dog_outside+
                                      #cat+
                                      cat_outside+
                                      ipratropium +
                                      antihistamine +
                                      intranasal_cs +
                                      intranasal_vc+
                                      antileukotriene + 
                                      atropine_nasal + 
                                      any_all_med + 
                                      pet +
                                      pet_allerg +
                                      hc_hosp+            
                                      strata(stratum), 
                                    table_strata1)

summary(res.clogit8.int)

#New Model with meds
#res.clogit5.int <- survival::clogit(cdi_status ~  
             #                         #probiotic + 
              #                        meat3 + hc_hosp + #hc_facility3+
               #                       adl_total + #vitamin +
                #                      cat_allerg +  
                 #                     #salad3 + #health_care + 
                                      #h2ra +
                                      #antibiotics3mo + 
                                      #intranasal_cs + 
                  #                    #any_all_med +
                   #                   intranasal_vc +
                    #                  strata(stratum), 
                     #               table_strata1)
#summary(res.clogit5.int)

#make nice table from interesting regression without interactions
model_output1 <- tidy(res.clogit4.int)
model_output1

ft1 <- model_output1 %>% 
  mutate(odds_ratio = exp(estimate),
         LCB = exp(conf.low),
         UCB = exp(conf.high)) %>% 
  select(term, odds_ratio, LCB, UCB, p.value)

#make nice table from interesting regression with interactions
model_output2 <- tidy(res.clogit5.int)
model_output2

ft1 <- model_output2 %>% 
  mutate(odds_ratio = exp(estimate),
         LCB = exp(conf.low),
         UCB = exp(conf.high)) %>% 
  select(term, odds_ratio, LCB, UCB, p.value)



#Write table

write.table(print(ft, digits=4), file = "multiple regression table", sep = ",", quote = FALSE, row.names = F)

#Plot with confidence intervals

g1<-ggplot(ft, aes(reorder(term, odds_ratio))) + geom_point(aes(y=odds_ratio)) +
  geom_errorbar(aes(ymin=`LCB`, ymax=`UCB`), width = 0.2) + coord_flip() 

g1 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +xlab("CDI Predictors") +ylab("Odds Ratio") +
  scale_x_discrete(labels = c('Meat','Cat Allergy','   Hospitalization/ \n SAR/SNF  ', 'Total ADL Score', 'Salad')) +ylab("Odds Ratio") +
  geom_hline(yintercept = 1, linetype = "dotted")
 
#Univariate Modeling

#Simple Linear regression using probiotic as a predictor of cdi_status
model <-table_strata1%>% 
  lm(cdi_status ~ probiotic, data = .)

model %>% 
  tidy()
#Congitional bivariate regression using probiotic as a predictor of cdi_status
model <-table_strata1%>% 
survival::clogit(cdi_status ~ probiotic  + 
                   strata(stratum), data = .)

#Visualize model
model %>% 
  tidy()

summary(model) 

#


#output to word document
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = here("06p_regress_table.docx"))

#See R Markdwon:Regression Output

