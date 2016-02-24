
##this script loads up bachelor focal data, filters for coresident pairs, then caclulates dyadic behavioral rates irrespective of directionanlity
##i did not include any state behaviors in this script
##Dave P Nov 2105


dir()

##load pkgs
library(dplyr)
library(tidyr)
library(lubridate)

##from previous version of paper, these are the bachelors that I have the best data on
bachelors_used<-tbl_df(read.csv("bachelors_used.csv",header=TRUE,stringsAsFactors=FALSE)) %>%
  select(ID=Subject) %>%
  distinct(.)

##the demographic data for all bachelors from my dissertation
bach_demo<-tbl_df(read.csv("bachelor_demography_2010_2011.csv",header=TRUE,stringsAsFactors=FALSE)) %>%
  select(ID=Subject,New.Name:age.yrs) %>%
  distinct(.)

##load in both 2010 and 2011 focals combine into a single df called focals
bach2010<-tbl_df(read.csv("bach_focals_2010.csv",header=TRUE,stringsAsFactors=FALSE))
bach2011<-tbl_df(read.csv("bach_focals_2011.csv",header=TRUE,stringsAsFactors=FALSE))
focals<-bind_rows(bach2010,bach2011) %>%
  distinct(.)


##creates a df for filtering focals, based on max and min date an indivual was focaled
start_stop_dates<-focals %>%
  group_by(Subject) %>%
  mutate(Date=mdy(Date)) %>%
  summarize(min_date=min(Date),
            max_date=max(Date)) %>%
  ungroup(.) %>%
  select(ID=Subject,start_date=min_date,stop_date=max_date) %>%
  distinct(.)


##creates a large demography of all bachelors used in the MTB paper
full_demo<-bachelors_used %>%
  left_join(bach_demo) %>%
  left_join(start_stop_dates) %>%
  distinct(.)

##for joining later with the all_dyads and combining with various data frames for filtering
unit1<-full_demo %>%
  select(id1=ID,unit1=amg.name,sub_start=start_date,sub_end=stop_date)
unit2<-full_demo %>%
  select(id2=ID,unit2=amg.name,rec_start=start_date,rec_end=stop_date)

##create a vector of all animals used in the dateset, to run the for loop over
individuals_used<-sort(unique(bachelors_used$ID))

##filters the subjects first by their start-stop time in specific units; uses the unit information from the previous dataframe and not the focals
##put back together like the original focal data sheet
focals_goodunits<-focals %>%
  mutate(id1=Subject) %>%
  left_join(unit1) %>%
  distinct(.) %>%
  mutate(DateTime=paste(Date,Time,sep="_")) %>%
  mutate(DateTime=mdy_hms(DateTime)) %>%
  mutate(periodtrue=ifelse(DateTime>=sub_start&DateTime<=sub_end,yes=TRUE,no=FALSE)) %>%
  filter(periodtrue) %>%
  mutate(Unit=unit1) %>%
  distinct(.) %>%
  select(Date:Recipient) %>%
  mutate(Recipient=gsub('[[:space:]]|[[:punct:]]','',Recipient)) %>%
  distinct(.)

##create a list to store each tbl_df of groom and end lines from each focal
##each data from is based on individuals_used as Recipients
groom_list<-list()


##run for loop that filters out all lines where two individuals were not coresident in the same bachelor group at the same time
for (i in seq_along(individuals_used)){
  recip_unit<-unit2
  sub_unit<-unit1
  groom_list[[i]]<-focals_goodunits %>%
    tbl_df() %>%
    mutate(DateTime=paste(Date,Time,sep=" ")) %>%
    mutate(DateTime=mdy_hms(DateTime)) %>%
    mutate(Recipient2=ifelse(State=="END",yes=as.character(individuals_used[i]),no=Recipient)) %>%
    filter(grepl(individuals_used[i],Recipient2,fixed=TRUE)) %>%
    mutate(id2=Recipient2) %>%
    mutate(id1=Subject) %>%
    left_join(recip_unit) %>%
    left_join(sub_unit) %>%
    distinct(.) %>%
    mutate(start_true=ifelse(sub_start<=rec_start,yes=TRUE,no=FALSE)) %>%
    mutate(end_true=ifelse(sub_end<=rec_end,yes=TRUE,no=FALSE)) %>%
    mutate(dyad_start=ifelse(start_true==TRUE,yes=as.character(rec_start),no=as.character(sub_start))) %>%
    mutate(dyad_end=ifelse(end_true==TRUE,yes=as.character(sub_end),no=as.character(rec_end))) %>%
    filter(unit1==unit2) %>%
    mutate(dyad_start=ymd(dyad_start)) %>%
    mutate(dyad_end=ymd(dyad_end)) %>%
    filter(DateTime>=dyad_start) %>%
    filter(DateTime<=dyad_end) %>%
    mutate(combo.id.unit=paste(Subject,Recipient2,Unit,sep="_")) %>%
    distinct(.)
  print(i)
}

##build back into a tbl_df that is tidy
filtered_focals_coresident<-as_data_frame(bind_rows(groom_list)) %>%
  distinct(.) %>%
  select(DateTime,Date,Time,Subject,Recipient2,Unit,combo.id.unit,Social,State)

#write.csv(filtered_focals_coresident,"tidy_bachelor_focals.csv")

##summarize each nonvocal count behavior into a df
nonvocal_counts<-filtered_focals_coresident %>%
  select(combo.id.unit,Social,State) %>%
  mutate(Social=gsub('V','X',Social)) %>%
  group_by(combo.id.unit) %>%
  summarize(subfoc_count=sum(grepl('END',State)),
            g1_count=sum(grepl('G1',Social)),
            g2_count=sum(grepl('G2',Social)),
            g3_count=sum(grepl('G3',Social)),
            a1_count=sum(grepl('A1',Social)),
            a2_count=sum(grepl('A2',Social)),
            l1_count=sum(grepl('L1',Social)),
            l2_count=sum(grepl('L2',Social)),
            t1_count=sum(grepl('T1',Social)),
            t2_count=sum(grepl('T2',Social)),
            n1_count=sum(grepl('N1',Social)),
            n2_count=sum(grepl('N2',Social)),
            n3_count=sum(grepl('N3',Social)),
            f1_count=sum(grepl('F1',Social)),
            f2_count=sum(grepl('F2',Social)),
            k1_count=sum(grepl('K1',Social)),
            k2_count=sum(grepl('K2',Social)),
            w1_count=sum(grepl('W1',Social)),
            w2_count=sum(grepl('W2',Social)),
            m1_count=sum(grepl('M1',Social)),
            m2_count=sum(grepl('M2',Social))) %>%
  ungroup(.) %>%
  separate(combo.id.unit,into=c("id1","id2","unit"),remove=FALSE) %>%
  filter(id1!=id2) %>%
  arrange(unit)

View(nonvocal_counts)


##summarize each vocal count behavior into a df
vocal_counts<-filtered_focals_coresident %>%
  select(combo.id.unit,Social,State) %>%
  group_by(combo.id.unit) %>%
  summarize(vg1_count=sum(grepl('VG1',Social)),
            vg2_count=sum(grepl('VG2',Social)),
            vt1_count=sum(grepl('VT1',Social)),
            vt2_count=sum(grepl('VT2',Social)),
            vf1_count=sum(grepl('VF1',Social)),
            vf2_count=sum(grepl('VF2',Social))) %>%
  ungroup(.) %>%
  separate(combo.id.unit,into=c("id1","id2","unit"),remove=FALSE) %>%
  filter(id1!=id2) %>%
  arrange(unit)

View(vocal_counts)

##combine the nonvocal counts with the vocal counts into a df
behavior_bach_coresident<-nonvocal_counts %>%
  left_join(vocal_counts) %>%
  arrange(subfoc_count)

View(behavior_bach_coresident)


##caclucate dyadic rates for each coresident pair ignoring directionality for behaviors in MTB paper
##this will get plugged into the PCA and clustering
unique_dyad<-behavior_bach_coresident %>%
  select(id1:vf2_count) %>%
  mutate(uniq.dyad=ifelse(id1<id2,yes=paste(id1,id2,sep="_"),no=paste(id2,id1,sep="_"))) %>%
  mutate(all_groom=g1_count+g2_count+g3_count) %>%
  mutate(all_approach=a1_count+a2_count) %>%
  mutate(all_leave=l1_count+l2_count) %>%
  mutate(all_threat=t1_count+t2_count) %>%
  mutate(all_nasty=n1_count+n2_count+n3_count) %>%
  mutate(all_fear=f1_count+f2_count) %>%
  mutate(all_kiss=k1_count+k2_count) %>%
  mutate(all_wish=w1_count+w2_count) %>%
  mutate(all_mount=m1_count+m2_count) %>%
  mutate(all_vg=vg1_count+vg2_count) %>%
  mutate(all_vt=vt1_count+vt2_count) %>%
  mutate(all_vf=vf1_count+vf2_count) %>%
  select(id1,id2,unit,uniq.dyad,total_focals=subfoc_count,all_groom:all_vf) %>%
  group_by(uniq.dyad,unit) %>%
  summarize_each(funs(sum),total_focals:all_vf) %>%
  ungroup(.) %>%
  mutate(focal_hrs=total_focals/4) %>%
  mutate_each(funs(./focal_hrs),all_groom:all_vf) %>%
  separate(uniq.dyad,into=c("id1","id2"),sep="_")

View(unique_dyad)

write.csv(unique_dyad,"coresident_dyad_rates.csv")


