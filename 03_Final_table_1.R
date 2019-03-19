#install packages
#install.packages("tableone")
#install.packages("ReporteRs")
#install.packages("magrittr")

#Load package
#library(tableone)
library(arsenal)
library(officer)
library(flextable)
library(magrittr)
library(tidyverse)
library(here)
library(DT)

# read in mtable from csv
mtable <- read_csv('02.2_Final_table_with_meds.csv')


#Convert gender into factor F=1, M=2

mtable$gender<-factor(mtable$gender, levels=c("F","M"), labels = c("Female", "Male"))

#Convert Race into factor with 1=white, 2= black, 3= Hispanic, 4=asian, 5=American Indian and Alaska Native
#6=Other"7=unknown

mtable$race<-factor(mtable$race, levels = c("white", "black", "Hispanic", "asian", 
                                        "American Indian and Alaska Native", "other","unknown"), 
                        labels=c("White", "Black", "Hispanic", "Asian", 
                      "American Indian and Alaska Native", "Other","Unknown"))

attr(mtable$gender,'label')  <- 'Gender'
attr(mtable$race,'label')  <- 'Race'
attr(mtable$antibiotics3mo,'label')  <- 'Antibiotic'
attr(mtable$ppi,'label')  <- 'PPI'
attr(mtable$h2ra,'label')  <- 'H2RA'
attr(mtable$hospital3,'label')  <- 'Hospitalization'
attr(mtable$hc_facility3,'label')  <- 'SAR/SNF'
attr(mtable$adl_total,'label')  <- 'Total ADL Score'
attr(mtable$dog,'label')  <- 'Dog Exposure'
attr(mtable$cat,'label')  <- 'Cat Exposure'
attr(mtable$any_all_med,'label')  <- 'Any Decongenstant'

#Create Table one, stratified according to CDI status
tab1 <- tableby(cdi_status ~ gender + age + race + antibiotics3mo +
         ppi +h2ra + any_all_med+hospital3+ hc_facility3+ adl_total + dog +cat, data=mtable)

summary(tab1, digits=3, digits.p =2, digits.pct =1)   

