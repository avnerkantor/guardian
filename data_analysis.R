library(tidyverse)
library(zoo)
library(lubridate)
library(gridExtra)
library(scales)
library(broom)
library(pwr)

options(scipen = 999)


load("gArticles2.rda")
load("gArticles4.rda")

tmp<-gArticles2%>%
  bind_rows(gArticles4)%>%
  mutate(quarter=as.yearqtr(date, format = "%Y-%m-%d"),
         month=as.yearmon(date, format = "%Y-%m-%d"),
         year=year(date),
         sectionName=tolower(sectionName),
         sectionName=case_when(
           sectionName=="politics"~"news",
           sectionName=="world news"~"news",
           sectionName=="uk news"~"news",
           # sectionName=="us news"~"news",
           TRUE~sectionName
         ),
         datablog=ifelse(datablog==TRUE, "Data journalism", "Traditional journalism")
  )%>%
  filter(sectionName=="news")


yearly_distribution=tmp%>%
  group_by(datablog, year)%>%
  summarise(n=n())%>%
  spread(datablog,n)

ggplot(tmp, aes(x=date , y=replies_ratio, color=datablog, group=datablog))+
  # geom_point()+
  # geom_line()+
  geom_smooth(se=T, method="lm")+ #lm", "glm", "gam", "loess" . locally estimated scatterplot smoothing
  scale_color_manual(values=c("#56B4E9", "#999999"))+
  labs(color="", x="", y="Ratio of replies",
       title="Linear Regression Fit for Ratio of Replies by Article Type"
  )+
  theme_bw()+
  # theme(legend.position="bottom")+
  ylim(0,1.5)

ggplot(tmp, aes(x=factor(year) , y=replies_ratio, color=datablog))+
  geom_boxplot()+
  ylim(0,3)+
  scale_color_manual(values=c("#56B4E9", "#999999"))+
  labs(color="", x="", y="Ratio of replies")+
  theme_bw()+
  theme(legend.position="bottom")

tmp=c%>%filter(datablog=="Data journalism")
summary(tmp$replies_ratio)

c2<-c%>%
  group_by(datablog, year)%>%
  summarise(repliesRatioMean=mean(replies_ratio),
            repliesMean=mean(replies),
            commentsMean=mean(n_comments),
            #rMeanSD=sd(replies_ratio),
            n=n()
  )

c%>%
  group_by(datablog, year)%>%
  summarise(n=n(), 
            min=round(min(replies_ratio), 2),
            max=round(max(replies_ratio),2),
            repliesRatioMean=round(mean(replies_ratio),2),
            rMeanSD=round(sd(replies_ratio),2)
            
  )


ggplot(c, aes(x=date , y=comments, color=datablog, group=datablog))+
  geom_smooth(se=FALSE, method="lm")+ 
  scale_color_manual(values=c("#56B4E9", "#999999"))+
  theme_bw()+
  labs(color="", x="", y="Comments")

ggplot(c, aes(x=date , y=replies, color=datablog, group=datablog))+
  geom_smooth(se=FALSE, method="lm")+ 
  scale_color_manual(values=c("#56B4E9", "#999999"))+
  theme_bw()+
  labs(color="", x="", y="Replies")

tt<-c%>%
  group_by(year)%>%
  do(tidy(t.test(replies_ratio ~ datablog, data = .)))


df=c%>%filter(datablog=="Data journalism")
summary(lm(data=df, replies_ratio~year))

df=c%>%filter(datablog=="News articles")
summary(lm(data=df, replies_ratio~year))

# Adjusted R-squared:  0.195 
pwr.f2.test(u = 1, v=140-1, f2 =0.195/(1-0.195), sig.level = 0.05, power =NULL)
# power = 0.9999394

summary(lm(data=c, replies_ratio~datablog+year))



