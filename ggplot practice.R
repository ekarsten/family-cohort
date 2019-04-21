library(tidyverse)
diamonds$color_num <-
  as.numeric(diamonds$color)

unique(diamonds$clarity)


diamonds$clarity_num <- rep(1,2,3,4 nrow(diamonds))

diamonds$clarity_num[diamonds$clarity %in% c("I1", "SI2", "SI1")] <- 1
diamonds$clarity_num[diamonds$clarity %in% c("VS2", "VS1")] <- 2
diamonds$clarity_num[diamonds$clarity %in% c("VVS2", "VVS1")] <- 3
diamonds$clarity_num[diamonds$clarity %in% c("IF")] <- 4



diamonds$cut_num <- rep(1, nrow(diamonds))
diamonds$cut_num[diamonds$cut == "Fair"] <- 0


mod1 <- lm(price ~ carat, data = diamonds)
mod2 <- lm(price ~ carat + cut_num, data = diamonds)
mod2 <- lm(price ~ carat * cut_num, data = diamonds)

mod3 <- lm(price ~ carat + color, data = diamonds)


summary(mod3)

summary(mod2)



attach(diamonds)
df <- diamonds
filter(select(df, price), price > 5000)

df %>% filter(clarity == "IF") %>% 
  ggplot( aes(x = carat, y = price, color = color))+ 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "carat x price") + 
  theme_bw() + facet_wrap(~color)

df %>% ggplot(aes(x = carat, y = price, color = color)) + geom_point() + 
  geom_smooth(method = "lm") +
  facet_grid(clarity~.) +
  labs(x = "carat", y = "price", title = "Clarity x Color Price Comparison") +
  theme_bw()



