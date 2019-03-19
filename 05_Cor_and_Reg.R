# install.packages("survival")
# install.packages("foreign")
# install.packages("Hmisc")

library(survival)
library(foreign)
library(dplyr)
library(Hmisc)
library(tidyverse)
library(here)
library(corrr)
library(corrplot)
      
# read in table_strata.csv from 04_Final_matching.R
table <- read_csv(here('04p_table_strata.csv'))

#Temporarily remove study_num and cdi,
# and non-numeric variables
table_corr<- table %>% 
  select(-c(study_num, cdi, gender, race, stratum))
str(table_corr)



# Approach 1 --------------------------------------------------------------

## create correlations with corrr package
d <-correlate(table_corr)
#present
d %>% 
  fashion()


# Approach 2 ---------------------------------------------------------------

##  correlations with corrplot package
table_corr %>% 
  cor() %>% 
  round(2)  %>% 
  corrplot(method = "circle")

table_corr %>% 
  cor() %>% 
  round(2)  %>% 
  corrplot( order="hclust", tl.srt=45, diag = F,
         p.mat = p.mat, sig.level = 0.01, insig = "blank")





#Build Correlation matrix with all variables using pearson correlation
# clean_table_stata1<- lapply(table_corr, as.numeric)
# cor_matrix <- cor(as.matrix(clean_table_stata1), method = "pearson", use = "complete.obs")
# res <- cor(clean_table_stata1)
# round(res,2)
# res2 <- rcorr(as.matrix(mtable6))
# res2

#Convert gender and race to numeric for correlation
# mtable6 <- lapply(mtable5, as.numeric)
# str(mtable6)

