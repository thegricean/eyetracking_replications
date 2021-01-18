library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("helpers.R")
setwd('../data')
theme_set(theme_bw())

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 

df = read.csv("trials_merged.csv", header = TRUE)
demo = read.csv("subject_info_merged.csv", header = TRUE)

#formatting
df$response = gsub(" ","",df$response)
df$response = gsub("\\[","",df$response)
df$response = gsub("\\]","",df$response)
df$response = gsub("\\'","",df$response)
df$response = gsub("AOI","",df$response)

df = df %>%
  group_by(workerid)%>%
  mutate(trial_number = seq(1:n())) %>%
  ungroup() %>%
  mutate(trial_group = ifelse(trial_number<31,"first_half","second_half"))

df = separate(df,response,into=c("click1","click2","click3","click4"),sep=",")

# plot proportion of selections by condition
toplot =  df %>%
  filter(ExpFiller=="Exp") %>%
  select(workerid,condition,size,click1,click2,click3,click4,target1,target2,competitor1,competitor2,instruction3) %>%
  mutate(ID = row_number()) %>%
  gather(click_number,location,click1:click4) %>%
  mutate(target=ifelse(location==target1,1,ifelse(location==target2,1,0))) %>%
  mutate(competitor=ifelse(location==competitor1,1,ifelse(location==competitor2,1,0))) %>%
  group_by(condition,size,click_number) %>%
  summarize(m_target=mean(target),m_competitor=mean(competitor),ci_low_target=ci.low(target),ci_high_target=ci.high(target),ci_low_competitor=ci.low(competitor),ci_high_competitor=ci.high(competitor)) %>%
  gather(location,Mean,m_target:m_competitor) %>%
  mutate(CILow=ifelse(location=="m_target",ci_low_target,ifelse(location=="m_competitor",ci_low_competitor,0))) %>%
  mutate(CIHigh=ifelse(location=="m_target",ci_high_target,ifelse(location=="m_competitor",ci_high_competitor,0))) %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  mutate(Region=fct_recode(location,"competitor"="m_competitor","target"="m_target")) %>%
  mutate(Region=fct_rev(Region)) %>%
  ungroup() %>%
  mutate(click_number=fct_recode(click_number,prior="click1",gender="click2",determiner="click3",noun="click4"))

proportions = ggplot(toplot, aes(x=click_number, y=Mean, group=Region)) +
  geom_line(aes(color=Region),size=1.3) +
  geom_point(aes(color=Region),size=2.5,shape="square") +
  geom_errorbar(aes(ymin=YMin, ymax=YMax), width=.2, alpha=.3) +
  facet_grid(size ~condition ) + 
  scale_color_manual(values=c("darkgreen","orange")) +
  xlab("Window") +
  ylab("Proportion of selections") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))

proportions

ggsave(proportions, file="../graphs/proportions.pdf",width=9,height=4.5)

# plot proportion of selections by condition and experiment half
toplot =  df %>%
  filter(ExpFiller=="Exp") %>%
  select(workerid,condition,size,click1,click2,click3,click4,target1,target2,competitor1,competitor2,instruction3,trial_group) %>%
  mutate(ID = row_number()) %>%
  gather(click_number,location,click1:click4) %>%
  mutate(target=ifelse(location==target1,1,ifelse(location==target2,1,0))) %>%
  mutate(competitor=ifelse(location==competitor1,1,ifelse(location==competitor2,1,0))) %>%
  group_by(condition,size,click_number,trial_group) %>%
  summarize(m_target=mean(target),m_competitor=mean(competitor),ci_low_target=ci.low(target),ci_high_target=ci.high(target),ci_low_competitor=ci.low(competitor),ci_high_competitor=ci.high(competitor)) %>%
  gather(location,Mean,m_target:m_competitor) %>%
  mutate(CILow=ifelse(location=="m_target",ci_low_target,ifelse(location=="m_competitor",ci_low_competitor,0))) %>%
  mutate(CIHigh=ifelse(location=="m_target",ci_high_target,ifelse(location=="m_competitor",ci_high_competitor,0))) %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  mutate(Region=fct_recode(location,"competitor"="m_competitor","target"="m_target")) %>%
  mutate(Region=fct_rev(Region)) %>%
  ungroup() %>%
  mutate(click_number=fct_recode(click_number,prior="click1",gender="click2",determiner="click3",noun="click4"))

proportions = ggplot(toplot, aes(x=click_number, y=Mean, group=Region)) +
  geom_line(aes(color=Region),size=1.3) +
  geom_point(aes(color=Region),size=2.5,shape="square") +
  geom_errorbar(aes(ymin=YMin, ymax=YMax), width=.2, alpha=.3) +
  facet_grid(trial_group + size ~condition ) + 
  scale_color_manual(values=c("darkgreen","orange")) +
  xlab("Window") +
  ylab("Proportion of selections") +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))

proportions

ggsave(proportions, file="../graphs/proportions_order.pdf",width=9,height=9)


# recreate Fig 13 from Sun & Breheny 2020
# compute and then plot target preference scores in each window
toplot =  df %>%
  filter(ExpFiller=="Exp") %>%
  select(workerid,condition,size,click1,click2,click3,click4,target1,target2,competitor1,competitor2,instruction3) %>%
  mutate(ID = row_number()) %>%
  gather(click_number,location,click1:click4) %>%
  mutate(target=ifelse(location==target1,1,ifelse(location==target2,1,0))) %>%
  mutate(competitor=ifelse(location==competitor1,1,ifelse(location==competitor2,1,0))) %>%
  # when target == 0 and comp == 0, turn target 0 into .5? or exclude? excluding for time being (leads to exclusion of 3230 data points)
  filter(target == 1 | competitor == 1) %>% 
  group_by(condition,size,click_number,workerid) %>%
  summarize(m_target=mean(target),m_competitor=mean(competitor)+.00000001) %>% 
  ungroup() %>% 
  mutate(prop=(m_target/m_competitor)+.00000001,targetadvantage=log(prop)) %>% 
  group_by(condition,size,click_number) %>%
  summarize(target=mean(targetadvantage),ci_low_target=ci.low(targetadvantage),ci_high_target=ci.high(targetadvantage)) %>%
  ungroup() %>% 
  mutate(YMin=target-ci_low_target,YMax=target+ci_high_target) %>% 
  mutate(Condition=fct_relevel(condition,"all","some"),Size=size) %>%
  mutate(Condition=fct_recode(Condition,"number"="num"))
dodge=position_dodge(0)

ggplot(toplot, aes(x=click_number, y=target, color=Condition, linetype=Size,group=interaction(Condition,Size))) +
  geom_line(size=1.3,position=dodge) +
  geom_point(size=2.5,shape="square",position=dodge) +
  geom_errorbar(aes(ymin=YMin, ymax=YMax), width=.2, alpha=.7,  linetype="solid",position=dodge) +
  # facet_grid(size ~condition ) + 
  scale_color_manual(values=c(cbPalette[2],cbPalette[6],cbPalette[3])) +
  scale_x_discrete(breaks=c("click1","click2","click3","click4"),
                   labels=c("Baseline", "Gender", "Determiner", "Noun")) +
  xlab("Window") +
  ylab("log(P(Target)/P(Competitor))") #+
  # theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
ggsave("../graphs/results-idt.pdf",width=4.5,height=2.5)



# run 2 models: 
# 1. like Sun & Breheny, fit linear models individually to each time window: "We constructed separate linear mixed- effects models for each time window predicting target preference scores from fixed effects of Determiner (all, some or number), Target size (small or big), Time and their interactions, including maximal random effects structure supported by the data."
# 2. do the more principled mixed effects logistic regression on each window

# get just experimental trials and wrangle data
dmodel =  df %>%
  filter(ExpFiller=="Exp") %>%
  select(workerid,condition,size,click1,click2,click3,click4,target1,target2,competitor1,competitor2,instruction3) %>%
  mutate(ID = row_number()) %>%
  gather(click_number,location,click1:click4) %>%
  mutate(target=ifelse(location==target1,1,ifelse(location==target2,1,0))) %>%
  mutate(competitor=ifelse(location==competitor1,1,ifelse(location==competitor2,1,0)))

# condition dataset on just target and competitor clicks
ddet = dmodel %>%
  mutate(TorC=target == 1  | competitor == 1) %>%
  filter(TorC == TRUE) %>%
  mutate(target = as.factor(as.character(target))) %>%
  mutate(condition=fct_relevel(condition,"num","all")) %>% 
  mutate(item=word(as.character(instruction3), -1)) %>% 
  droplevels() 

d_baseline = ddet %>% 
  filter(click_number == "click1") %>% 
  droplevels()
d_gender = ddet %>% 
  filter(click_number == "click2") %>% 
  droplevels()
d_determiner = ddet %>% 
  filter(click_number == "click3") %>% 
  droplevels()
d_noun = ddet %>% 
  filter(click_number == "click4") %>% 
  droplevels()

nrow(d_baseline) # 2160
nrow(d_gender) # 3869
nrow(d_determiner) # 3978
nrow(d_noun) # 4043

# no effect, as expected in prior window
dc_baseline = cbind(d_baseline,myCenter(d_baseline[,c("size","condition")]))
m.baseline = glmer(target ~ condition*csize + (1+condition+csize|workerid) + (1|item),family="binomial",data=dc_baseline)
summary(m.baseline)

# no effect, as expected in gender window
dc_gender = cbind(d_gender,myCenter(d_gender[,c("size","condition")]))
m.gender = glmer(target ~ condition*csize + (1+condition+size|workerid) + (1|item),family="binomial",data=dc_gender)
summary(m.gender)

# the crucial window: (still work on convergence issues)
contrasts(d_determiner$size) # big reference level
dc_determiner = cbind(d_determiner,myCenter(d_determiner[,c("size","condition")]))
m.determiner = glmer(target ~ condition*csize + (1+condition+size|workerid) + (1|item),family="binomial",data=dc_determiner)
summary(m.determiner)

dc_noun = cbind(d_noun,myCenter(d_noun[,c("size","condition")]))
m.noun = glmer(target ~ condition*csize + (1+condition+size|workerid) + (1|item),family="binomial",data=dc_noun)
summary(m.noun)





m.simple = glmer(target ~ condition*size-size + (1|workerid),family="binomial",data=ddet)
summary(m.simple)