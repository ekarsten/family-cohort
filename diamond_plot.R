library(tidyverse)
attach(diamonds)

df <- diamonds

df2 <- 
  df %>% select(price) %>%
  filter(price>5000)

# to plot something
# select data
# specify aesthetics
# specify plotting method
# make it pretty

df %>% filter(clarity == "IF") %>%
  ggplot(aes(x = carat, y = price, color = color)) +
  geom_point() +
  facet_wrap(~color) + #
  geom_smooth(method = "lm") +
  labs(title = "correlation between carat & price of diamonds",
       x = "carat",
       y = "price in US dollars") +
  theme_bw() + 
  theme(legend.position = "bottom")

df %>% 
  ggplot(aes(x = carat, y = price, color = color)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~clarity) +
  labs(title = "correlation between carat & price of diamonds",
       x = "carat",
       y = "price in US dollars")
  # theme_bw() + 
  # theme(legend.position = "bottom")