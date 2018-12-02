#====================================
# Data Visualization with ggplot - Replicating Figures from
# Mariage Meets the Joneses
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

#df <- read_dta(file.path(ddir, "Joneses", "ipums.dta.gz"))

#set.seed(2123)
#small_df <- sample_n(df, 1000000)

#save(small_df, file = file.path(ddir, "Joneses", "small_df.Rda"))

load(file.path(ddir, "Joneses", "small_df.Rda"))

#-----------------------------------
# Cleaning Our Data
#-----------------------------------

# First we want to elimiante group quarters from our data, so we filter it
# down to GQ == 1 (see codebook)
# Next we want to filter down individual income to reduce top and bottom-coding
# to make this easy, let's make these thresholds $0 and $75,000
clean_df <-
  small_df %>%
  filter(gq == 1) %>%
  filter(inctot < 75000,
         inctot > 0)

# We want to make the mariage status readable
MARST_codebook <-
  tibble(MARST = 1:6,
         marst = c("Married", "Married", "Separated", "Divorced", "Widowed",
                      "Single"))

# We want to make race readable 
RACE_codebook <-
  tibble(RACE = 1:9,
         race = c("White", "Black", "Native_American", "Chinese", "Japanese",
                  "Other Asian", "Other", "Multiracial", "Multiracial"))

# We want to make hispanic origin readable
HISPAN_codebook <-
  tibble(HISPAN = c(0,1,2,3,4,9),
         hispan = c(F, rep(T, 3), NA, NA))

# We want to make sex readable
SEX_codebook <-
  tibble(SEX = c(1,2),
         sex = c("Male", "Female"))

# Want to convert to 2000's dollars, so create codebook of inflation multipliers
INFL_codebook <-
  tibble(year = c(2000, 1990, 1980),
         infl_mult = c(1, 1.3175, 2.0898))

# Now we join on all of these codebooks and create a new row for real_inc
clean_df <-
  clean_df %>%
  rename(MARST = marst, RACE = race, HISPAN = hispan, SEX = sex) %>%
  mutate(MARST = as.integer(MARST),
         RACE = as.integer(RACE),
         HISPAN = as.integer(HISPAN),
         year = as.integer(year),
         SEX = as.integer(SEX)) %>%
  left_join(MARST_codebook) %>%
  left_join(RACE_codebook) %>%
  left_join(HISPAN_codebook) %>%
  left_join(SEX_codebook) %>%
  left_join(INFL_codebook) %>%
  select(-MARST, -RACE, -HISPAN, -SEX) %>%
  mutate(real_inc = inctot * infl_mult)

#-----------------------------------
# Replicating Figure 1a
#-----------------------------------

# filter our data down to white men, nonhispanic, age 25-34
# group our data into real income bins of increments of 2500 dollars,
# for each bin, in each year, compute the fraction married
# then plot the income vs prop maried where lines are colored in by year

fig_1a <-
  clean_df %>%
  filter(race == "White",
         hispan == F,
         age < 35,
         age >= 25) %>%
  mutate(binned_real_inc = round(real_inc/2500)*2500) %>%
  group_by(binned_real_inc, year, sex) %>%
  summarise(prop_married = mean(marst == "Married", na.rm = T)) %>%
  filter(binned_real_inc < 75000)

fig_1a %>%
  filter(sex == "Male") %>%
  ggplot(aes(x = binned_real_inc, y = prop_married, color = as.factor(year))) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Real Income in 2000's dollars",
       y = "Proportion Married",
       color = "Year",
       title = "Marriage Rates by income for Nonhispanic White Men")
  
# A more exciting version that looks at women too
  fig_1a %>%
  ggplot(aes(x = binned_real_inc, y = prop_married,
             color = as.factor(sex), shape = as.factor(year))) +
    geom_point() +
    geom_line() +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(x = "Real Income in 2000's dollars",
         y = "Proportion Married",
         color = "Sex",
         shape = "Year",
         title = "Marriage Rates by income for Nonhispanic Whites")

#-----------------------------------
# Now you get to replicate Figure 1b
#-----------------------------------


#-----------------------------------
# Now you get to replicate Figure 1c
#-----------------------------------


#-----------------------------------
# Now you get to replicate Figure 2
#-----------------------------------

# This one will require some real work coding in the metro areas