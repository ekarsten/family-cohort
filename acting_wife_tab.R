#====================================
# Data Manipulation in the Tidyverse - Replicating Tables from Acting Wife
#====================================

#-----------------------------------
# Setting up workspace
#-----------------------------------

# This snippet of code is a little loop that makes my code work on your computer
root <- getwd()
while(basename(root) != "family-cohort") {
  root <- dirname(root)
}

# This line runs the script in your data.R file so that each person can have
# their data in a different place because everyone's file structure will be 
# a little differnt
source(file.path(root, "data.R"))

# Leoading the packages we want
library(tidyverse)
library(haven) #for reading stata data

#-----------------------------------
# Loading In the Data
#-----------------------------------

survey <- read_dta(file.path(ddir, "Acting Wife", "2_survey.dta"))

#-----------------------------------
# Replicating Summary stats from Survey Data
#-----------------------------------

# we want to know the proportion male, the average age, prop single, serious
# relationship, cohabitating, engagd, maried, no response

# we are going to introduce ideas of the summary function.

survey %>%
  mutate(marital = if_else(marital == ".", NA_character_, marital)) %>%
  summarise(prop_male = 1 - mean(female, na.rm = T),
            prop_single = mean(single, na.rm = T),
            prop_single_alt = mean(marital == 1, na.rm = T),
            prop_engaged = mean(marital == 2, na.rm = T),
            prop_serious = mean(marital == 3, na.rm = T),
            prop_cohab = mean(marital == 4, na.rm = T),
            prop_married = mean(marital == 5, na.rm = T),
            observations = n()) %>%
  gather(key = "variable")


#-----------------------------------
# Replicating Table 2 Col 1 and 2
#-----------------------------------

category_codebook <-
  tibble(single = c(1,1,0,0),
         female = c(1,0,1,0),
         category = c("Single Woman", "Single Man",
                      "Non-Single Woman", "Non-Single Man"))

survey %>%
  left_join(category_codebook) %>%
  filter(!is.na(category)) %>%
  group_by(category) %>%
  summarise(Salary_Negotiation = mean(wage, na.rm = T),
            Leadership_Role = mean(leadership, na.rm = T),
            Presentati)


#-----------------------------------
# Your turn to replicate the other columns!
#-----------------------------------