#====================================
# Final Research Project Analysis 
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
# a little different
source(file.path(root, "df.Rda"))

# Loading the packages we want
library(tidyverse)
library(haven) # for reading stata data
library(lfe) # for fixed effect regression
library(stargazer) # for pretty regression tables

#-----------------------------------
# Loading In the Data
#-----------------------------------

#df <- read_dta(file.path(ddir, "Joneses", "ipums.dta.gz"))

#save(small_df, file = file.path(ddir, "Joneses", "small_df.Rda"))

load(file.path(ddir, "df.Rda"))

head(df)
#-----------------------------------
# Cleaning Our Data
#-----------------------------------

#Here is where we will clean our data

# Sunday, April 28th
NYmetro_df <- df %>%
  filter(PWMETRO == 5600) %>%
  filter(MARST == 1) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9)

NYinter_df <- NYmetro_df %>%
  filter(RACE != RACE_SP)

summary(NYmetro_df)

LAmetro_df <- df %>%
  filter(PWMETRO == 4480) 

#-----------------------------------
# Figures
#-----------------------------------

# May 5th

LAmetro_df <- df %>%
  filter(PWMETRO == 4480) %>%
  filter(MARST == 1) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9)

LAinter_df <- LAmetro_df %>%
  filter(RACE != RACE_SP)

LA1980_df <- LAmetro_df %>%
  filter(YEAR == 1980)

LA1980_inter_df <- LA1980_df %>%
  filter(RACE != RACE_SP)

# percentage of interracial marriage in LA in 1980
322/8755
# result is 0.03677898

LA1990_df <- LAmetro_df %>%
  filter(YEAR == 1990)

LA1990_inter_df <- LA1990_df %>%
  filter(RACE != RACE_SP)

# percentage of interracial marriage in LA in 1990
866/17805
# result is 0.04863802

LA2000_df <- LAmetro_df %>%
  filter(YEAR == 2000)

LA2000_inter_df <- LA2000_df %>%
  filter(RACE != RACE_SP)

# percentage of interracial marriage in LA in 2000
958/14440
#The result is 0.06634349

#Here is where we will make our figures 