#====================================
# Intro to Applied Regression
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
library(haven) # for reading stata data
library(lfe) # for fixed effect regression
library(stargazer) # for pretty regression tables
library(AER) # for some great datasets
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
# Simple Linear Models
#-----------------------------------
# Let's look at the effect of age on real income using the ipums data
# Here are three different OLS regression specifications

ols1 <- lm(real_inc ~ age, data = clean_df)

stargazer(ols1, type = "text",
          title = "Base Specification",
          omit.stat = c("ser", "f"))

#-----------------------------------
# Models with Dummy Variables
#-----------------------------------

clean_df <-
  clean_df %>%
  mutate(sex = relevel(as.factor(sex), "Male"),
         race = relevel(as.factor(race), "White"),
         marst = relevel(as.factor(marst), "Single"))

ols2 <- lm(real_inc ~ age + race + hispan, data = clean_df)
ols3 <- lm(real_inc ~ age + sex, data = clean_df)
ols4 <- lm(real_inc ~ age + race + hispan + sex, data = clean_df)
ols5 <- lm(real_inc ~ age + marst + sex, data = clean_df)

stargazer(ols1,ols2,ols3,ols4,ols5, type = "text",
          title = "Comparison of OLS With Different Controls",
          omit.stat = c("ser", "f"))

#-----------------------------------
# Fixed Effect Models
#-----------------------------------

# Fixed effects are great for projecting out things we don't care about, but
# that we definitely want to control for. In this case, let's do year and race
# fixed effects:

fe1 <- felm(real_inc ~ age | as.factor(year) + race + hispan, data = clean_df)
fe2 <- felm(real_inc ~ age + marst | as.factor(year) + race + hispan, data = clean_df)
fe3 <- felm(real_inc ~ age + sex | as.factor(year) + race + hispan, data = clean_df)
fe4 <- felm(real_inc ~ age + sex + marst | as.factor(year) + race + hispan, data = clean_df)

stargazer(ols1,fe1,fe2,fe3,fe4, type = "text",
          title = "Comparision of Fixed Effect Specifications",
          omit.stat = c("ser", "f"))

#-----------------------------------
# Regression Discontinuity/ Diff in Diff
#-----------------------------------

data("PSID1982")

df <-
  PSID1982 %>%
  mutate(any_college = if_else(education > 12, 1, 0))

PSID1982 %>%
  ggplot(aes(x = education, y = wage, color = occupation)) +
  geom_jitter(alpha = .3) +
  facet_wrap(~gender) +
  labs(title = "Hmm")

mod1 <- lm(wage ~ education, data = df)
mod2 <- lm(wage ~ education + any_college, data = df)
mod3 <- lm(wage ~ education * any_college, data = df)
mod4 <- lm(wage ~ education * any_college + occupation, data = df)

stargazer(mod1,mod2,mod3,mod4, type = "text",
          title = "Trying to Model Prior Figure",
          omit.stat = c("ser", "f"))

#-----------------------------------
# Differences In Differences
#-----------------------------------

dd1 <- lm(wage ~ any_college, data = df)
dd2 <- lm(wage ~ occupation, data = df)
dd3 <- lm(wage ~ any_college + occupation, data = df)
dd4 <- lm(wage ~ any_college * occupation, data = df)

stargazer(dd1,dd2,dd3,dd4, type = "text",
          title = "Trying to Model Different Part of Prior Figure",
          omit.stat = c("ser", "f"))

#-----------------------------------
# Instrumental variables/2SLS
#-----------------------------------

# Saved for another dataset another time

#-----------------------------------
# Logit Models
#-----------------------------------

# What about if we want our left hand side to be binary, then we need to use
# logit (google this to learn more). We interpret these coefficients as 
# percentage change in the odds ratio, so very different form OLS

# Lets say we want to predict mariage from income, let's do it three different
# ways: OLS, OLS with fixed effects, Logit

analysis_df <-
  clean_df %>%
  mutate(married = marst == "Married",
         year = as.factor(year),
         real_inc = real_inc/1000) %>%
  filter(age < 35, age > 24, marst != "Widowed",
         marst != "Separated", marst != "Divorced",
         sex == "Male")

mar1 <- lm(married ~ real_inc, data = analysis_df)
mar2 <- felm(married ~ real_inc | year + race + hispan, data = analysis_df)
mar3 <- felm(married ~ real_inc | year + race + hispan,
             weights = analysis_df$perwt, data = analysis_df)
mar4 <- glm(married ~ real_inc, data = analysis_df, family = "binomial")
mar5 <- glm(married ~ real_inc + year, data = analysis_df, family = "binomial")
mar6 <- glm(married ~ real_inc + year, data = analysis_df, family = "binomial",
            weights = analysis_df$perwt)

stargazer(mar1,mar2,mar3,mar4,mar5,mar6, type = "text",
          omit.stat = c("ser", "f"))
