library(ggplot2)
attach(diamonds)
df <- diamonds


filter(select(df, price), price>5000)
#is equivalent to
df %>% select(price) %>% filter(price>5000)
#What we will do today:
#Select data
#Specify aesthetics
#Specify plotting method
#Make it pretty

df %>% filter(clarity=="IF") %>% ggplot(aes(x=carat, y=price)) + geom_point() + geom_smooth(method="lm") + labs(title="IF Diamonds Graph", x="Carats", y="Price") + theme_bw()

df %>% filter(clarity=="IF") %>% ggplot(aes(x=carat, y=price, color=color)) + geom_point() + geom_smooth(method="lm") + labs(title="IF Diamonds Graph", x="Carats", y="Price") + theme_bw()

df %>% filter(clarity=="IF") %>% ggplot(aes(x=carat, y=price, color=color)) + geom_point() + facet_wrap(~color) + geom_smooth(method="lm") + labs(title="IF Diamonds Graph", x="Carats", y="Price") + theme_bw()

df %>% 
  ggplot(aes(x=carat, y=price,)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_grid(clarity~color)

df %>%
  ggplot(aes(x=carat, y=price, color=color)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_wrap(~clarity)
