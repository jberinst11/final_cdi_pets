# install.packages("survival")
# install.packages("foreign")
#install.packages("Hmisc")


library(survival)
library(foreign)
library(dplyr)
library(Hmisc)
library(tidyverse)
library(here)
library(corrr)
library(corrplot)
      
# read in table_strata.csv from 04_Final_matching.R
table0 <- read_csv('04p_table_strata.csv')

#Remove MC_stable as no one was not it (1 that was removed)
table<-table0 %>% 
  select(-mc_stable)

#Reaarange so all medications are close

table1<-table[c(1:19, 30:36, 20:29)]

#Temporarily remove study_num and cdi,
# and non-numeric variables
table_corr<- table1 %>% 
  select(-c(study_num, cdi, gender, race))
str(table_corr)


##Rename Columns

colnames(table_corr) <- c("CDI Status", "Prior CDI", "Patient Age", "Frequent Restaurant Visits", "High Dessert Intake", "High Meat Intake", 
                          "High Salad Intake", "Frequent Red Wine Intake", "Dairy Intake", "Vitamins", "Probiotic", "Antibiotic", 
                          "PPI",  "H2RA", "Any Acid Suppresion", "Ipratropium", "Antihistamine", 
                          "Intranasal Corticosteroids", "Intranasal Vasoconstrictor", "Antileukotriene", "Intransal Atropine",
                          "Any Nasal Decongestant", "Healthcare Occupation", "Hospitalization", "SAR/SNF", "Total ADL Score ",
                          "Dog Allergy", "Cat Allergy", "Any Dog", "Outside Dog", "Any Cat", "Outside Cat")
                        
    
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

p.mat <- cor.mtest(table_corr)$p

#Build a Corplot
corrplot(cor(table_corr), method = "color", col = col(200),
         type = "upper", order = "AOE", number.cex = .6,
         #addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, pch.cex = 1, insig = "label_sig", 
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)


#Export as PDF 15x10

