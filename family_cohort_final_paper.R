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
library(Hmisc)
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

#sex codebooks
sex_codebook <- tibble(SEX = c(1, 2),
                       sex = c("Male", "Female"))
sexsp_codebook <- tibble(SEX_SP = c(1,2),
                         sexsp = c("Male", "Female"))
#limited df to test stuff on
dflimited <- head(df, 15)

#race codebooks
race_codebook <- tibble(RACE = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                        race = c("White", "Black", "Native",
                                 "Chinese", " Japanese", "OtherAsian",
                                 "Other", "Multi", "Multi"))
racesp_codebook <- tibble(RACE_SP = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                          racesp = c("White", "Black", "Native",
                                 "Chinese", " Japanese", "OtherAsian",
                                 "Other", "Multi", "Multi"))

#demonstration codebooks work on limited version
dflimitedclean <- dflimited %>% left_join(sex_codebook) %>% 
                                left_join(sexsp_codebook) %>%
                                left_join(race_codebook) %>%
                                left_join(racesp_codebook)
#Beginning to get rid of some of the columns for the df
filtereddf <- df %>% select(-GQ, -NFAMS, -NSUBFAM, -NCOUPLES,
                            -MULTGEN, -MULTGEND, -FAMUNIT, -SUBFAM, 
                            -SFTYPE, -SFRELATE, -CBSUBFAM, -CBSFTYPE, 
                            -CBSFRELATE, -NCHILD, -ELDCH, -YNGCH, -RELATE, -RELATED)
#Attempt to clean the larger df; fails b/c of size
filtereddfclean <- filtereddf %>% left_join(sex_codebook) %>% 
  left_join(sexsp_codebook) %>%
  left_join(race_codebook) %>%
  left_join(racesp_codebook)

# for creating a table with the race percentages once the cityname-year dfs exist
cityname_1980 <- cityname_1980 %>% left_join(race_codebook)
racecityname_1980 <- as.data.frame(table(cityname_1980$race))
names(racecityname_1980) <- c("Race", "Population")
observationscityname_1980 <- nrow(cityname_1980)
racecityname_1980$Percent <- 0
racecityname_1980$Percent <- (racecityname_1980$Population / observationscityname_1980)

#-----------------------------------
# Figures
#-----------------------------------

#Here is where we will make our figures 







pwmetro <- tibble(PWMETRO = c(4480), pwmetro = "Los Angeles-Long Beach")








f1 <-
ggplot(data=, aes(x=race, y=percent)) +
    geom_bar(stat="identity", color="blue", fill="white")






