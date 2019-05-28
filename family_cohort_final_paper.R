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

library(xtable)
library(Hmisc)

library(ggplot2)

#-----------------------------------
# Loading In the Data
#-----------------------------------

#df <- read_dta(file.path(ddir, "Joneses", "ipums.dta.gz"))

#save(small_df, file = file.path(ddir, "Joneses", "small_df.Rda"))

load(file.path(ddir, "df.Rda"))

#Use this to observe changes in your data 
small_section <- df[1:10,]
View(small_section)

set.seed(100)
sample_df = sample_n(df, 100000)

#-----------------------------------
# Cleaning Our Data
#-----------------------------------

#Here is where we will clean our data

#Trying to figure out #s of people in a given city
cities <- as.data.frame(table(df$PWMETRO))
names(cities) <- c("PWMETRO", "number")
cities <- cities[order(cities$number),]
cities1to45 <- cities[1:45,]
cities46to90 <- cities[46:90,]
cities91to135 <- cities[91:135,]
cities136to180 <- cities[136:180,]
cities181to225 <- cities[181:225,]
cities226to270 <- cities[226:270,]
cities271to300 <- cities[271:300,]
cities301to332 <- cities[301:332,]

print(xtable(cities1to45, type = "latex"), file = "C:/Users/ahkan/Desktop/Family-Cohort/family-cohort/PWMetrotables/pwmetrowhole1-45.tex")
print(xtable(cities46to90, type = "latex"), file = "C:/Users/ahkan/Desktop/Family-Cohort/family-cohort/PWMetrotables/pwmetrowhole46-90.tex")
print(xtable(cities91to135, type = "latex"), file = "C:/Users/ahkan/Desktop/Family-Cohort/family-cohort/PWMetrotables/pwmetrowhole91-135.tex")
print(xtable(cities136to180, type = "latex"), file = "C:/Users/ahkan/Desktop/Family-Cohort/family-cohort/PWMetrotables/pwmetrowhole136-180.tex")
print(xtable(cities181to225, type = "latex"), file = "C:/Users/ahkan/Desktop/Family-Cohort/family-cohort/PWMetrotables/pwmetrowhole181-225.tex")
print(xtable(cities226to270, type = "latex"), file = "C:/Users/ahkan/Desktop/Family-Cohort/family-cohort/PWMetrotables/pwmetrowhole226-270.tex")
print(xtable(cities271to300, type = "latex"), file = "C:/Users/ahkan/Desktop/Family-Cohort/family-cohort/PWMetrotables/pwmetrowhole271-300.tex")
print(xtable(cities301to332, type = "latex"), file = "C:/Users/ahkan/Desktop/Family-Cohort/family-cohort/PWMetrotables/pwmetrowhole301-332.tex")


#organized by city-year (we need to replace cityname w/ the desired name, & replace 4480 with desired city code)
cityname_1980 <- subset(df, PWMETRO == 4480) %>% subset(YEAR == 1980)
cityname_1990 <- subset(df, PWMETRO == 4480) %>% subset(YEAR == 1990)
cityname_2000 <- subset(df, PWMETRO == 4480) %>% subset(YEAR == 2000)




#>>>>>>> 55c6480584623b46164e3ef6b4a9ad988a7497cf
#=======
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

#<<<<<<< HEAD
# for creating a table with the race percentages once the cityname-year dfs exist
cityname_1980 <- cityname_1980 %>% left_join(race_codebook)
racecityname_1980 <- as.data.frame(table(cityname_1980$race))
names(racecityname_1980) <- c("Race", "Population")
observationscityname_1980 <- nrow(cityname_1980)
racecityname_1980$Percent <- 0
racecityname_1980$Percent <- (racecityname_1980$Population / observationscityname_1980)
#>>>>>>> 9010229b46d70ceae3e2d6458d8c2fd0f7413ef2
#=======

#duplicating the creation of the cityname_year dfs in order to test the rest of the code
#cityname_1980 <- subset(df, PWMETRO == 4480) %>% subset(YEAR == 1980)
#cityname_1990 <- subset(df, PWMETRO == 4480) %>% subset(YEAR == 1990)
#cityname_2000 <- subset(df, PWMETRO == 4480) %>% subset(YEAR == 2000)


#>>>>>>> 6a5782310fdaa25e8e4956faea4a1a4a3f89d62b
#=======
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
#>>>>>>> f18bc90efd905c9df7150b4370461806b0ea40c2

#-----------------------------------
# Figures
#-----------------------------------

#Here is where we will make our figures 

metro <- sample_df$PWMETRO

##



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

pwmetro <- tibble(PWMETRO = c(4480), pwmetro = "Los Angeles-Long Beach")








f1 <-
ggplot(data=, aes(x=race, y=percent)) +
    geom_bar(stat="identity", color="blue", fill="white")

# Sunday, May 5th

# We decide to work on interracial marriages in Los Angeles-Long Beach this time

# part 1: data cleaning

LAmetro_df <- df %>%
  filter(PWMETRO == 4480) %>%
  filter(MARST == 1) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9)

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

LAtable <- LAmetro_df %>%
  group_by(YEAR) %>%
  mutate(interracial = as.numeric(RACE != RACE_SP)) %>%
  summarise(prop_interracial = mean(interracial))

# part 3: creating a figure

ggplot(data = LAtable, aes(x = YEAR, y = prop_interracial)) +
  geom_point() +
  geom_line()

# part 4: incorporate new cities and create a similar figure

Citiesmetro_df <- df %>%
  filter(PWMETRO == 4480 | PWMETRO == 5600) %>%
  filter(MARST == 1) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9) 

Citiestable <- Citiesmetro_df %>%
  group_by(YEAR, PWMETRO) %>%
  mutate(interracial = as.numeric(RACE != RACE_SP)) %>%
  summarise(prop_interracial = mean(interracial))



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


NY_dfint <- NY_df1 %>%
  filter(RACE != RACE_SP) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9) 

#Regression

#preparing variables for regression 

#calculating prop of non white people in population
df_regression <- df %>% 
  filter(PWMETRO == 1520 | PWMETRO == 7240 | PWMETRO == 5080 | PWMETRO == 4480 | PWMETRO == 5600 | PWMETRO == 1600) %>%
  mutate(race_non_white = if_else(RACE != 1, 1, 0, missing = NULL)) 

prop_non_white = df_regression %>% 
  group_by(PWMETRO, YEAR) %>%
  summarise(diversity_prop = mean(race_non_white))

df_regression <- left_join(df_regression, prop_non_white, by = c("PWMETRO", "YEAR"))

#adding dummy variable for married or not 
df_regression <- df_regression %>%
  filter(MARST == 1) %>%
  mutate(interracial_marriage = if_else(RACE != RACE_SP, 1, 0, missing = NULL))

#making age groupings

df_regression$AgeGroup <- cut(df_regression$AGE, breaks = c(seq(0, 100, by = 10), Inf), right = FALSE)

#education codebook

#running the regression

test_reg <- lm(interracial_marriage ~ diversity_prop, data = df_regression)
stargazer(test_reg)

educ_codebook <- tibble(EDUC = c(0,1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                        educ = c("NA", "Grade School", "Grade School",
                                 "High School", "High School", "High School",
                                 "High School", "College", "College", "College", "College", "College"))

df_regression <- df_regression %>% left_join(educ_codebook)


View(df_regression)
