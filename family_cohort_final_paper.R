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
source(file.path(root, "data.R"))

# Loading the packages we want
library(tidyverse)
library(haven) # for reading stata data
library(lfe) # for fixed effect regression
library(stargazer) # for pretty regression tables
library(ggplot2)

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

#Here is where we will make our figures 

# Sunday, May 5th

# We decide to work on interracial marriages in Los Angeles-Long Beach this time

# part 1: data cleaning

# clean the dataset for interracial marriages in LA in the 1980s

LAmetro_1980_df <- df %>%
  filter(PWMETRO == 4480) %>%
  filter(YEAR == 1980) %>%
  filter(MARST == 1) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9) 

LAinter_1980_df <- LAmetro_1980_df %>%
  filter(RACE != RACE_SP)

# clean the dataset for interracial marriages in LA in the 1990s

LAmetro_1990_df <- df %>%
  filter(PWMETRO == 4480) %>%
  filter(YEAR == 1990) %>%
  filter(MARST == 1) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9) 

LAinter_1990_df <- LAmetro_1990_df %>%
  filter(RACE != RACE_SP)

# clean the dataset for interracial marriages in LA in the 2000s

LAmetro_2000_df <- df %>%
  filter(PWMETRO == 4480) %>%
  filter(YEAR == 2000) %>%
  filter(MARST == 1) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9) 

LAinter_2000_df <- LAmetro_2000_df %>%
  filter(RACE != RACE_SP)

# part 2: creating a new dataset for the figure

  


