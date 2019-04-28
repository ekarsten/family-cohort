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

#-----------------------------------
# Figures
#-----------------------------------

#Here is where we will make our figures 

#"Proof of Concept" - Cedric

tiny_df <- df[1:1000000, 1:105]

#the following show distribution of Speaks English of Spouse for each English proficiency level of the person who filled out the survey 

#Head of house: "Does not speak English"

clean_df1 <- tiny_df %>%
  filter(SPEAKENG == 1)

p1 <- hist(clean_df1$SPEAKENG_SP)

#Head of house: "Yes, speaks English..."
#this gives: "Error in hist.default(clean_df2$SPEAKENG_SP) : invalid number of 'breaks'"
#it seems that no one responded with "2" := "Yes, speaks English..." 
#so we can probably just disregard this category entirely to make our lives a little easier

clean_df2 <- tiny_df %>%
  filter(SPEAKENG == 2)

p2 <- hist(clean_df2$SPEAKENG_SP)

#Head of house: "Yes, speaks only English"

clean_df3 <- tiny_df %>%
  filter(SPEAKENG == 3)

p3 <- hist(clean_df3$SPEAKENG_SP)

#Head of house: "Yes, speaks very well"

clean_df4 <- tiny_df %>%
  filter(SPEAKENG == 4)

p4 <- hist(clean_df4$SPEAKENG_SP)

#Head of house: "Yes, speaks well" 

clean_df5 <- tiny_df %>%
  filter(SPEAKENG == 5)

p5 <- hist(clean_df5$SPEAKENG_SP)

#Head of house: "Yes, but not well"

clean_df6 <- tiny_df %>%
  filter(SPEAKENG == 6)

p6 <- hist(clean_df6$SPEAKENG_SP)

#The other responses, N/A (Blank), Unkown, and Illegible are irrelevant

plot(SPEAKENG_SP ~ SPEAKENG, data = tiny_df)

#This shows what seems obvious: people are married to people with similar language skills to them
#People who speak only english marry almost exclusively others who speak only english
#It is likely that those who do not have very good English skills and are married share a non-English language
#If we are looking for a cutoff to create a binary "Can English/Cannot English," between 4 and 5 is where a big contrast appears
#Interracial marriage occurs in all language breakdowns:

plot(RACE_SP ~ SPEAKENG, data = tiny_df)

#But, between people who speak only English, there are few interracial marriages and almost no hispanic spouses:

hist(clean_df3$RACE)

hist(clean_df3$RACE_SP)

hist(clean_df3$HISPAN_SP)

#With more language skills, however, both people who responded and their spouses are more racially diverse:

hist(clean_df4$RACE)

hist(clean_df4$RACE_SP)

#Thus, most of our interracial marriage will likely be occuring between those with mutliple shared languages
#I recommend, of SPEAKENG and SPEAKENG_SP we disregard responses 0, 1, 2, 7, and 8
#They are firstly a small portion of the population and are awkward in the context of the other responses
#We can take an angle with the paper of "Interracial marriage between people with at least some English language skills"

#How we could approach this, i.e., use this sample and work from here:

sample_df <- tiny_df %>%
  filter(SPEAKENG > 2,
         SPEAKENG < 7) %>%
  filter(SPEAKENG_SP > 2,
         SPEAKENG_SP <7)

#Recoding 

SPEAKENG_codebook <-
  tibble(SPEAKENG = 3:6,
         ENG = c("Only English", "Very Well", "Well", "Yes, But Not Well"))

SPEAKENGSP_codebook <-
  tibble(SPEAKENG_SP= 3:6,
         ENG_SP = c("Only English", "Very Well", "Well", "Yes, But Not Well"))

sample_df <- 
  sample_df %>%
  left_join(SPEAKENG_codebook) %>%
  left_join(SPEAKENGSP_codebook)

#racial composition within New York metro area

tinyNY_df <- tiny_df %>%
  filter(PWMETRO == 5600)

NY_df <- df %>%
  filter(PWMETRO == 5600)

NY_df12 <- df %>%
  filter(PWMETRO == 5600, MARST == 1 | MARST == 2)

NY_df2 <- NY_df %>%
  filter(MARST == 2)

#MARST == 1: spouse present

NY_df1 <- NY_df %>%
  filter(MARST == 1)

NY_dfint <- NY_df1 %>%
  filter(RACE != RACE_SP) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9) 