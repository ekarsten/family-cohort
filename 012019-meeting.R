library(tidyverse)
attach(diamonds)
df<-diamonds
filter(select(df,price),price>5000)
df%>%select(price)%>%
  filter(price>5000)
df2<-df%>%select(price)%>%
  filter(price>5000)
#select data ^
df%>%ggplot(aes(x=carat,y=price))
df%>%filter(clarity=="IF")%>%ggplot(aes(x=carat,y=price))
#specify aesthetics^
df%>%filter(clarity=="IF")%>%ggplot(aes(x=carat,y=price))+geom_point()+
  geom_smooth(method="lm")
#specify plotting method^ (lm for linear model)
df%>%filter(clarity=="IF")%>%ggplot(aes(x=carat,y=price))+geom_point()+
  geom_smooth(method="lm")+
  labs(title="This is a TITLE")+
  theme_bw()
#make it pretty
df%>%filter(clarity=="IF")%>%ggplot(aes(x=carat,y=price,color=color))+
  geom_point()+
  facet_wrap(~color)+
  geom_smooth(method="lm")+
  labs(title="This is a TITLE")+
  theme_bw()
df%>%filter(clarity=="IF")%>%ggplot(aes(x=carat,y=price))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(clarity~color)+
  labs(title="color/clarity combination")
#doing some useless stuff
df%>%ggplot(aes(x=carat,y=price,color=color))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(clarity~.)+
  labs(title="color/clarity combination")
#become more useful
df%>%ggplot(aes(x=carat,y=price,color=color))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~clarity)+
  labs(title="color/clarity combination")
#make it more pretty

