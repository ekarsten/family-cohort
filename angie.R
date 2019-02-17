library(tidyverse)

df <-
  tibble(
    Animal = c("Dog", "Cat", "Dog",
               "Raven", "Cat"), 
    Weight = c(100, 40, 80, 16, 50),
    Height = c(23, 18, 40, 3, 16),
    Family = c("Mammal", "Mammal", "Mammal",
               "Bird", "Mammal"))

