library(tidyverse)

df <-
  tibble(
    Animal = c("Dog", "Cat", "Dog",
               "Raven", "Cat"), 
    Weight = c(100, 40, 80, 16, 50),
    Height = c(23, 18, 40, 3, 16),
    Family = c("Mammal", "Mammal", "Mammal",
               "Bird", "Mammal"))

df %>% select(Animal, Weight)

select(df, Animal, Weight)

df %>% filter(Family == "Mammal")

df %>% filter(Height > 10)

df %>% 
  filter(Family == "Mammal") %>%
  filter(Weight > 80)

df %>%
  filter(Animal == "Cat"|
         Animal == "Raven") %>%
  select(Weight)

df %>%
  filter(Animal %in% c("Cat","Raven")) %>%
  select(Family)

df$Animal

