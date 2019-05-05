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


#duplicating the creation of the cityname_year dfs in order to test the rest of the code
#cityname_1980 <- subset(df, PWMETRO == 4480) %>% subset(YEAR == 1980)
#cityname_1990 <- subset(df, PWMETRO == 4480) %>% subset(YEAR == 1990)
#cityname_2000 <- subset(df, PWMETRO == 4480) %>% subset(YEAR == 2000)



#-----------------------------------
# Figures
#-----------------------------------

#Here is where we will make our figures 


# for creating a line graph with the race percentages once the cityname-year dfs exist
#For 1980
cityname_1980 <- cityname_1980 %>% left_join(race_codebook)
observationscityname_1980 <- nrow(cityname_1980)
racecityname_1980 <- as.data.frame(table(cityname_1980$race))
names(racecityname_1980) <- c("Race", "Population")
racecityname_1980$Percent <- 0
racecityname_1980$Percent <- (racecityname_1980$Population / observationscityname_1980)
#For if we want to print this to a table as well
#print(xtable(racecityname_1980, type "latex"), file = "insert file path")
#For 1990
cityname_1990 <- cityname_1990 %>% left_join(race_codebook)
observationscityname_1990 <- nrow(cityname_1990)
racecityname_1990 <- as.data.frame(table(cityname_1990$race))
names(racecityname_1990) <- c("Race", "Population")
racecityname_1990$Percent <- 0
racecityname_1990$Percent <- (racecityname_1990$Population / observationscityname_1990)
#For if we want to print this to a table as well
#print(xtable(racecityname_1990, type "latex"), file = "insert file path")
#For 2000
cityname_2000 <- cityname_2000 %>% left_join(race_codebook)
observationscityname_2000 <- nrow(cityname_2000)
racecityname_2000 <- as.data.frame(table(cityname_2000$race))
names(racecityname_2000) <- c("Race", "Population")
racecityname_2000$Percent <- 0
racecityname_2000$Percent <- (racecityname_2000$Population / observationscityname_2000)
#For if we want to print this to a table as well
#print(xtable(racecityname_2000, type "latex"), file = "insert file path")
#turning them into one table
racecityname_1980$Year <- 1980
racecityname_1990$Year <- 1990
racecityname_2000$Year <- 2000
racecitynameall <- rbind(racecityname_2000, racecityname_1990, racecityname_1980)
#plotting this city's racial composition over the years in a line graph
ggplot(data = racecitynameall, aes(x=Year, y=Percent)) + geom_line(aes(colour=Race))



