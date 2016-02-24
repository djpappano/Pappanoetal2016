
dir()
##load pkgs for data processing and visualization
library(dplyr)
library(tidyr)
library(FactoMineR)
library(ggplot2)

##load in bachelor demography
demo1<-tbl_df(read.csv("bachelor_demography_2010_2011.csv",header=TRUE,stringsAsFactors=FALSE)) %>%
  select(id1=Subject,age.yrs1=age.yrs)
demo2<-tbl_df(read.csv("bachelor_demography_2010_2011.csv",header=TRUE,stringsAsFactors=FALSE)) %>%
  select(id2=Subject,age.yrs2=age.yrs)

##load dyadic behavioral rate data, filter for obs time and top 10 behaviors
full_dyad_data<-tbl_df(read.csv("coresident_dyad_rates.csv",header=TRUE,stringsAsFactors=FALSE)) %>%
  select(-X) %>%
  mutate(combo.ID=paste(id1,id2,sep="")) %>%
  filter(total_focals>8) %>%
  select(-all_vf,-all_nasty)

View(full_dyad_data)

##load in relatedness data
relatedness<-tbl_df(read.csv("bachelor_relatedness.csv",header=TRUE,stringsAsFactors=FALSE)) %>%
  mutate(combo.ID=combo) %>%
  select(combo.ID,Wang) %>%
  distinct(.)

##assign rownames to df
row.names(full_dyad_data)<-full_dyad_data$combo.ID

##PCA with FactoMineR, scaled
behavior_PCA<-full_dyad_data %>%
  select(all_groom:all_vt) %>%
  PCA(scale.unit=TRUE,graph=FALSE)

summary(behavior_PCA)

plot(behavior_PCA,axes=c(1,2),choix="var")
plot(behavior_PCA,axes=c(1,3),choix="var")
plot(behavior_PCA,axes=c(2,3),choix="var")
plot(behavior_PCA,axes=c(3,4),choix="var")

#hierarchical clustering over principal components
cluster_PCA<-HCPC(behavior_PCA,nb.clust=-1,graph=FALSE)
cluster_PCA


##put data back together
final_df<-cluster_PCA$data.clust %>%
  add_rownames() %>%
  tbl_df() %>%
  mutate(combo.ID=rowname) %>%
  inner_join(full_dyad_data) %>%
  select(-rowname) %>%
  select(combo.ID,id1,id2,unit,total_focals,clust,all_groom:all_vt) %>%
  mutate(clust=as.character(clust)) %>%
  distinct(.)

summary_behaviors<-final_df %>%
  group_by(clust) %>%
  summarize(count=n(),
            mean_groom=mean(all_groom),
            mean_threat=mean(all_threat),
            mean_wish=mean(all_wish),
            mean_mount=mean(all_mount))

View(summary_behaviors)

##plotting observed data
dev.off()

##this creates a figure to visualize differences in behavioral rates among the "categories" returned
##by the PCA and clustering; cluster 3 corresponds to "bonded" individuals
##scale this somehow to group means

fig_data1 <- final_df %>%
  gather(key=behavior,value=rate,all_groom:all_vt) %>%
  mutate(behavior=as.character(behavior)) %>%
  mutate(bonded=(ifelse(clust==3,yes="yes",no="no"))) %>%
  mutate(behavior=gsub('all_','',behavior))
  
fig_data2<-fig_data1 %>%
  select(unit,behavior,rate) %>%
  group_by(unit,behavior) %>%
  summarize(group_means=mean(rate),
            group_sd=sd(rate)) %>%
  ungroup(.) %>%
  mutate(unit_behavior=paste(unit,behavior,sep="_")) %>%
  select(unit_behavior,group_means,group_sd)

figure_data<-fig_data1 %>%
  mutate(unit_behavior=paste(unit,behavior,sep="_")) %>%
  left_join(fig_data2) %>%
  mutate(standardized_rate=(rate-group_means)/group_sd) %>%
  filter(complete.cases(.))

##include the behaviors that load strongest to the first 3 PC's greater than 0.8
fig1<-figure_data %>%
  select(behavior,bonded,standardized_rate) %>%
  filter(grepl('groom|vg|vt|threat|fear',behavior)) %>%
  mutate(behavior=gsub('vg','contact grunt',behavior)) %>%
  mutate(behavior=gsub('vt','vocalized threat',behavior)) %>%
  mutate(behavior=gsub('fear','lip flip',behavior)) %>%
  ggplot() +
  geom_boxplot(aes(x=behavior,y=standardized_rate,fill=bonded)) +
  scale_fill_manual("Bonded",values=c("lemonchiffon","steelblue4")) +
  ylim(c(-4,4)) +
  theme_classic(base_size=18) +
  theme(legend.position="top") +
  ylab("standardized rate")

fig1

##analysis
analysis_data<-final_df %>%
  left_join(relatedness) %>%
  left_join(demo1) %>%
  left_join(demo2) %>%
  mutate(age.diff=abs(age.yrs1-age.yrs2)) %>%
  mutate(scaled.age.diff=scale(age.diff)) %>%
  mutate(age.class=ifelse(age.diff<=1.5,yes=1,no=0)) %>%
  mutate(output_friend=as.integer(ifelse(clust==3,yes=1,no=0))) %>%
  mutate(related=scale(Wang)) %>%
  select(id1,id2,age.class,output_friend,scaled.age.diff,related,Wang,unit) %>%
  arrange(output_friend) %>%
  filter(complete.cases(.))


##now i'm going to try to use GLMM on "bonded" based on relatedness and age similarity
library(lme4)
library(MuMIn)

options(na.action = "na.fail") #  change the default "na.omit" to prevent models from being fitted to different datasets in case of missing values.

##
friend_model<-glmer(output_friend~(1|id1)+(1|id2)+related*age.class,family="binomial",data=analysis_data)

##
ms1<-dredge(friend_model,rank="AICc")
subset_models<-model.avg(ms1, subset = delta < 4)
confset.95p <- get.models(ms1, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)

summary.avg<-summary(avgmod.95p)
confint(avgmod.95p)

##plot coefficients from full model average
summary.avg
full_betas<-as.numeric(c(avgmod.95p$coefficients[1,1:3]))
std_error<-as.numeric(c("0.845","0.481","0.145")) ##gave up and hardcoded this in, stupid arrays
fixeff<-c("Intercept","Same Age Class","Pairwise Relatedness")

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



