
library(dplyr)

bachelors<-tbl_df(read.csv("bachelors_used.csv",header=TRUE,stringsAsFactors=FALSE)) %>%
  select(Subject)

relate<-tbl_df(read.csv("bachelor_relatedness.csv",header=TRUE,stringsAsFactors=FALSE)) %>%
  select(combo,Wang) %>%
  distinct(.)

bach.demo1<-tbl_df(read.csv("bachelor_demography_2010_2011.csv",stringsAsFactors=FALSE,na.strings="")) %>%
  mutate(ID1=Subject) %>%
  mutate(age1=age.yrs) %>%
  mutate(amg1=amg.name) %>%
  select(ID1,age1,amg1)

bach.demo2<-tbl_df(read.csv("bachelor_demography_2010_2011.csv",stringsAsFactors=FALSE,na.strings="")) %>%
  mutate(ID2=Subject) %>%
  mutate(age2=age.yrs) %>%
  mutate(amg2=amg.name) %>%
  select(ID2,age2,amg2)


##Figure 1

groups<-bach.demo1  %>%
  mutate(age.class=cut(age1,breaks=c(6.5,8,10,12,16),labels=c("Subadult (7-8 yrs)","Early-Prime Adult (8-10 yrs)","Mid-Prime Adult (10-12 yrs)", "Late-Prime Adult (>12 yrs)"))) %>%
  select(Subject=ID1,amg.name=amg1,age.class) %>%
  inner_join(bachelors)

Fig1<- groups %>%
  mutate(amg.number=as.factor(amg.name)) %>%
  select(amg.number,age.class) %>%
  ggplot(aes(x=amg.number,fill=age.class)) +
  scale_fill_manual("Legend",values=c("coral","darkmagenta","springgreen4","dodgerblue3")) +
  geom_bar(aes(position="stack",stat="bin"),colour="black") +
  xlab("All-Male Groups") +
  ylab("Number of Bachelors") +
  scale_y_discrete()+
  scale_x_discrete(labels=c("A","B","C","D","E","F","G","H")) +
  theme_classic(base_size=20)

Fig1



##create 2 vectors one repeated each for n length, the other repeated entirely for n length
cID1<-as.character(rep(bachelors$Subject,each=length(bachelors$Subject)))
cID2<-as.character(rep_len(bachelors$Subject,length.out=2304))

###create vector of all dyadic combinations of ID
combo.ID<-c(
)
for (i in 1:length(cID1)) { combo.ID[i]<-paste(sort(c(cID1[i], cID2[i]))[1],sort(c(cID1[i], cID2[i]))[2])}

##get only unique combinations for combo.ID
combo<-unique(combo.ID)
combo<-as.character(gsub(" ","",combo.ID))

full<-tbl_df(as.data.frame(combo,stringsAsFactors=FALSE)) %>%
  left_join(relate) %>%
  select(combo,Wang) %>%
  filter(complete.cases(.)) %>%
  mutate(ID1=substr(combo,1,3)) %>%
  mutate(ID2=substr(combo,4,6)) %>%
  left_join(bach.demo1) %>%
  left_join(bach.demo2) %>%
  mutate(same.amg.yn=ifelse(amg1==amg2,yes="Within All-Male Group",no="Between All-Male Groups")) %>%
  mutate(age.dif=abs(age1-age2)) %>%
  mutate(same.age.yn=ifelse(age.dif<1.5,yes="Same Age Class",no="Different Age Class")) %>%
  arrange(Wang) %>%
  distinct(.)

#write.csv(full,"bachelor_relatedness.csv")
analysis_data<-full %>%
  mutate(scaled_relate=scale(Wang)) %>%
  mutate(age_class_same=ifelse(same.age.yn=="Same Age Class",yes=1,no=0)) %>%
  mutate(same_amg=ifelse(same.amg.yn=="Within All-Male Group",yes=1,no=0)) %>%
  select(scaled_relate,ID1,ID2,age_class_same,same_amg)


##load up packages for model analysis
library(lme4)
library(MuMIn)

global_relate<-glmer(same_amg~(1|ID1)+(1|ID2)+scaled_relate*age_class_same,family="binomial",data=analysis_data)



##first change in options for na.fail
options(na.action = "na.fail") 

##here sample size is sufficiently large to use AIC i.e. 982/5 >40, but sticking with AICc

ms1<-dredge(global_relate,rank="AICc")
subset_models<-model.avg(ms1, subset = delta < 4)
confset.95p <- get.models(ms1, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)

summary.avg<-summary(avgmod.95p)
conf.avg<-confint(avgmod.95p)

summary.avg

library(ggplot2)

##Looks good
Relate.Fig <- full %>%
  select(Wang,same.amg.yn,same.age.yn) %>%
  na.omit() %>%
  ggplot() +
  scale_fill_manual("Legend",values=c("red","blue")) +
  geom_violin(aes(x=same.amg.yn,y=Wang,fill=same.amg.yn,alpha=0.3)) +
  stat_summary(aes(x=same.amg.yn,y=Wang),fun.y="mean",geom="point") +
  ylab("Pairwise Relatedness") +
  xlab("") +
  scale_y_continuous(breaks=c(-0.8,-0.4,0.0,0.4,0.8)) +
  theme_classic(base_size=20) +
  annotate("text",x=1.5,y=0.6,label="**") +
  annotate("text",x=1,y=-0.2,label="N=416") +
  annotate("text",x=2,y=-0.2,label="N=75") +
  theme(legend.position="none")

Relate.Fig


##plot coefficients from full model average
summary.avg
full_betas<-as.numeric(c(avgmod.95p$coefficients[1,1:4]))
std_error<-as.numeric(c("0.1943","0.1416","0.2785","0.2673")) ##gave up and hardcoded this in, stupid arrays
fixeff<-c("Intercept","Pairwise Relatedness","Same Age Class","Same Age Class x Pairwise Relatednss")

coef_fig_data<-as.data.frame(cbind(full_betas,std_error,fixeff),stringsAsFactors=FALSE)

##looks good
coef_fig<-coef_fig_data %>%
  mutate(beta=as.numeric(full_betas)) %>%
  mutate(std_error=as.numeric(std_error)) %>%
  mutate(s1=beta-std_error) %>%
  mutate(s2=beta+std_error) %>%
  select(fixeff,beta,s1,s2) %>%
  filter(fixeff!="Intercept") %>%
  ggplot() +
  geom_errorbar(aes(x=fixeff,y=beta,ymin=s1,ymax=s2,width=0.1)) +
  geom_point(aes(x=fixeff,y=beta),size=4) +
  ylim(c(-1,1)) +
  ylab("Fixed Effect Estimates") +
  xlab("") +
  theme_classic(base_size=18) +
  coord_flip()

coef_fig
  