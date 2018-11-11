# family-cohort
The home of all code for the Oeconomica Family Economics reading group for 2018-2019

# Data managment
All data used in this project will be kept in a google drive folder. Please sync this google drive folder to your computer
- Create a file called `data.R`
- In this file, put the line of code equivalent to `ddir <- "C:/Users/erict/Google Drive/Family Cohort Data"`
- This will allow us to write code that is system agnostic: in other words it can call a standard file (`data.R`) that we will have on all our machines and then we will have an object called `ddir` in our R environment which can be used to tell our computer where to look for data.
