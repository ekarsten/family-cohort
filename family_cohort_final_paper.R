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





#-----------------------------------
# Figures
#-----------------------------------

#Here is where we will make our figures 








