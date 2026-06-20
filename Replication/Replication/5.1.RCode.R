######################################################################
# Assign colors
######################################################################
cbp1 <- c("#A6CEE3", "#E0E0E0", "#E6AB02", "#1F78B4",
          "#878787", "#A6761D")
cbp2 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

######################################################################
# Loading packages
######################################################################
library(dplyr)
library(purrr)
library(ggplot2)
library(ggthemes)
library(haven)
library(boot)
library(reshape2)
library(here)
library(tidyr)

######################################################################
# Unmarried to Married
######################################################################
ssm <- read_dta(here("Data", "2015mar.dta"))
jgss <- read_dta(here("Data", "JGSSmar.dta"))

ssm1 <- bind_rows(ssm) %>% 
  dplyr::select(cor=workch,emp,mar,age,female) %>% 
  mutate(emp=ifelse(emp==3,2,emp))

jgss1 <- bind_rows(jgss) %>% 
  dplyr::select(cor=workch,emp,mar,age,female) %>% 
  mutate(emp=ifelse(emp==3,2,emp))

## For standardization, see Calculating the standard error in Freddie Bray and Jacques Ferlay.pdf

###################### Bootstrap ##############################
meanfun <- function(data, i){
  d <- data[i, ]
  return(mean(d))   
}
set.seed(2022)
set.seed(123)
dd <- lapply(0:1,function(i){
      lapply(1:2,function(j){
      lapply(1:2,function(k){
      lapply(1:7,function(l){
  df <- bind_rows(ssm1,jgss1) %>% 
    filter(female==i) %>% 
    filter(emp==j) %>%
    filter(cor==k) %>%
    filter(age==l) %>%
    data.frame() 
  bo <- boot(df[, "mar", drop = FALSE], statistic=meanfun, R=500)

  x <- data.frame(mean = bo$t0,
                se = sd(bo$t),
                female=i,
                emp=j,
                cor=k,
                age=l)
      })
      })
      })
})

df1 <- melt(dd) %>% 
  filter(variable=="mean"|variable=="se") %>% 
  dplyr::select(female=L1,emp=L2,cor=L3,age=L4,everything()) %>% 
  pivot_wider(names_from = c("variable"),values_from = value) %>% 
  mutate(upper=mean+1.96*se,lower=mean-1.96*se) %>% 
  mutate(emp=case_when(
    emp == 1 ~ "Standard",
    emp == 2 ~ "Non-standard"
  )) %>% 
  mutate(emp = factor(emp,levels=c("Standard","Non-standard"))) %>% 
  mutate(cor=case_when(
    cor == 1 ~ "1970-1989",
    cor == 2 ~ "1990-2015"
  )) %>% 
  mutate(female=case_when(
    female == 1 ~ "Male",
    female == 2 ~ "Female"
  )) %>% 
  mutate(age = case_when(
    age == 1 ~ 15,
    age == 2 ~ 20,
    age == 3 ~ 25,
    age == 4 ~ 30,
    age == 5 ~ 35,
    age == 6 ~ 40,
    age == 7 ~ 45
  )) %>% 
  mutate(type="marriage")

######################################################################
check <- bind_rows(ssm1,jgss1) %>% 
  group_by(cor,emp,age,female) %>% 
  mutate(n=1) %>% 
  summarise_all(list(mean,sum)) %>% 
  mutate(female=case_when(
    female == 0 ~ "Male",
    female == 1 ~ "Female"
  )) %>% 
  mutate(age = case_when(
    age == 1 ~ 15,
    age == 2 ~ 20,
    age == 3 ~ 25,
    age == 4 ~ 30,
    age == 5 ~ 35,
    age == 6 ~ 40,
    age == 7 ~ 45
  )) %>% 
  mutate(emp=case_when(
    emp == 1 ~ "Standard",
    emp == 2 ~ "Non-standard"
  )) 

######################################################################
# Married to childbirth
######################################################################
ssm <- read_dta(here("Data", "2015birth.dta"))
jgss <- read_dta(here("Data", "JGSSbirth.dta"))

ssm2 <- bind_rows(ssm) %>% 
  dplyr::select(cor=workch,emp,bir,age,female) 
jgss2 <- bind_rows(jgss) %>% 
  dplyr::select(cor=workch,emp,bir,age,female) 

###################### Bootstrap ##############################
set.seed(2022)
set.seed(123)
meanfun <- function(data, i){
  d <- data[i, ]
  return(mean(d))   
}

dd <- lapply(0:1,function(i){
  lapply(1:2,function(j){
    lapply(1:2,function(k){
      lapply(1:7,function(l){
        df <- bind_rows(ssm2,jgss2) %>% 
          filter(female==i) %>% 
          filter(emp==j) %>%
          filter(cor==k) %>%
          filter(age==l) %>%
          data.frame() 
        bo <- boot(df[, "bir", drop = FALSE], statistic=meanfun, R=500)
        
        x <- data.frame(mean = bo$t0,
                        se = sd(bo$t),
                        female=i,
                        emp=j,
                        cor=k,
                        age=l)
      })
    })
  })
})

df2 <- melt(dd) %>%
  filter(variable=="mean"|variable=="se") %>%
  dplyr::select(female=L1,emp=L2,cor=L3,age=L4,everything()) %>%
  pivot_wider(names_from = c("variable"),values_from = value) %>%
  mutate(upper=mean+1.96*se,lower=mean-1.96*se) %>%
  mutate(emp=case_when(
    emp == 1 ~ "Standard",
    emp == 2 ~ "Non-standard"
  )) %>%
  mutate(emp = factor(emp,levels=c("Standard","Non-standard"))) %>%
  mutate(cor=case_when(
    cor == 1 ~ "1970-1989",
    cor == 2 ~ "1990-2015"
  )) %>%
  mutate(female=case_when(
    female == 1 ~ "Male",
    female == 2 ~ "Female"
  )) %>%
  mutate(age = case_when(
    age == 1 ~ 15,
    age == 2 ~ 20,
    age == 3 ~ 25,
    age == 4 ~ 30,
    age == 5 ~ 35,
    age == 6 ~ 40,
    age == 7 ~ 45
  )) %>%
  mutate(type="birth")

######################################################################
# ggplot - Fig 2 and 3
######################################################################
desc <- bind_rows(df1,df2) %>% 
  dplyr::select(cor,emp,age,female,mean,upper,lower,name=type) %>% 
  mutate(name = case_when(
    name == "marriage" ~ "First marriage rate",
    name == "birth" ~ "First birth rate"
  )) %>% 
  mutate(name=factor(name,levels=c("First marriage rate",
                                   "First birth rate")))
# First marriage rates
desc %>% 
  filter(age != 45) %>% 
  filter(female == "Male") %>% 
  ggplot(aes(x = age, y = mean,group=emp,color=emp)) +
  geom_point(position=position_dodge(0.3)) +geom_line() + facet_grid(name~cor) +
  ylab("") +xlab("Age")+theme_few() +
  scale_color_brewer(palette="Paired") +
  theme(legend.position = c(0.875, 0.9))+
  geom_errorbar(aes(ymin=lower, ymax=upper),
                width=.1,
                position=position_dodge(0.3)) + labs(color='Emp. status (1st job)') 
  
ggsave(height=6,width=9,dpi=200, filename="Figures/Figure3.pdf",  family = "Helvetica")

desc %>% 
  filter(age != 45) %>% 
  filter(female == "Female") %>% 
  ggplot(aes(x = age, y = mean,group=emp,color=emp)) +
  geom_point(position=position_dodge(0.3)) +geom_line() + facet_grid(name~cor) +
  ylab("") +xlab("Age")+theme_few() +
  scale_color_brewer(palette="Paired") +
  theme(legend.position = c(0.875, 0.9)) +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                width=.1,
                position=position_dodge(0.3)) + labs(color='Emp. status (1st job)') 

ggsave(height=6,width=9,dpi=200, filename="Figures/Figure4.pdf",  family = "Helvetica")

# desc %>% 
#   filter(age != 45) %>% 
#   write.csv("Figures/Desc_rates.csv",row.names = FALSE)

######################################################################
# Unmarried to childbirth
######################################################################
ssm <- read_dta(here("Data", "2015bridal.dta"))
jgss <- read_dta(here("Data", "JGSSbridal.dta"))

ssm3 <- bind_rows(ssm) %>% 
  dplyr::select(cor=workch,emp,bri,age,female) 

jgss3 <- bind_rows(jgss) %>% 
  dplyr::select(cor=workch,emp,bri,age,female) 

df3 <- bind_rows(ssm3,jgss3) %>% 
  group_by(cor,emp,age,female) %>% 
  summarise_all(list(mean)) %>% 
  mutate(emp=case_when(
    emp == 1 ~ "Standard",
    emp == 2 ~ "Non-standard"
  )) %>% 
  mutate(female=case_when(
    female == 0 ~ "Male",
    female == 1 ~ "Female"
  )) %>% 
  mutate(cor=case_when(
    cor == 1 ~ "1970-1989",
    cor == 2 ~ "1990-2015",
    cor == 3 ~ "1990-2015 (cf)"
  )) %>% 
  mutate(age = case_when(
    age == 1 ~ 15,
    age == 2 ~ 20,
    age == 3 ~ 25,
    age == 4 ~ 30,
    age == 5 ~ 35,
    age == 6 ~ 40,
    age == 7 ~ 45
  )) 

######################################################################
# Create counter factual dataset
######################################################################
cf1 <- df1 %>% 
  filter(cor=="1970-1989") %>% 
  mutate(cor="1990-2015") %>% 
  dplyr::select(female,emp,cor,age,mar=mean)

cf3 <- df3 %>% 
  filter(cor=="1970-1989") %>% 
  mutate(cor="1990-2015") 

cf <- cf1 %>% 
  left_join(df2) %>% 
  left_join(df3) %>% 
  mutate(cor="1990-2015 (cf)") %>% 
  dplyr::select(female,emp,cor,age,mar,bri,bir=mean)

df <- df1 %>% 
  dplyr::select(female,emp,cor,age,mar=mean) %>% 
  left_join(df2) %>% 
  dplyr::select(female,emp,cor,age,mar,bir=mean) %>% 
  left_join(df3) %>% 
  bind_rows(cf) %>% 
  mutate(emp = factor(emp,levels=c("Standard","Non-standard"))) %>%
  filter(is.na(age)==F) %>% 
  dplyr::select(female,emp,cor,age,mar,bri,bir)

df <- df %>% 
  group_by(cor, emp, female) %>% 
  arrange(cor, emp, female, age) %>% 
  mutate(n=5,
         nax=5/2,
         lx=if_else(age==15,100000,0),
         lmx=0,
         lbx=0,
         nqx1=(n*mar)/(1+(n-nax)*mar),
         nqx2=(n*bri)/(1+(n-nax)*bri),
         nqx3=(n*bir)/(1+(n-nax)*bir),
         ndx1=lx*nqx1,
         ndx2=lx*nqx2,
         ndx3=lmx*nqx3)

for (i in unique(c(seq(20, 45, by = 5))))
{
  df <- df %>% 
    mutate(lx=if_else(age %in% i,dplyr::lag(lx)-dplyr::lag(ndx1)-dplyr::lag(ndx2),lx)) %>% 
    mutate(lmx=if_else(age %in% i,dplyr::lag(lmx)+dplyr::lag(ndx1)-dplyr::lag(ndx3),lmx)) %>% 
    mutate(lbx=if_else(age %in% i,dplyr::lag(lbx)+dplyr::lag(ndx2)+dplyr::lag(ndx3),lbx)) %>% 
    mutate(ndx1=if_else(age %in% i,nqx1*lx,ndx1)) %>% 
    mutate(ndx2=if_else(age %in% i,nqx2*lx,ndx2)) %>% 
    mutate(ndx3=if_else(age %in% i,nqx3*lmx,ndx3)) 
}  

dfx <- df %>% 
  mutate(sum=lx+lmx+lbx,
         childbirth=lbx/sum,
         marriage=(lmx+lbx)/sum) %>% 
  filter(age==45) %>% 
  dplyr::select(cor,emp,female,childbirth,marriage) %>% 
  mutate(maronly = marriage - childbirth,
         prop = childbirth/marriage) %>% 
  mutate(female=factor(female,levels=c("Male","Female")))

# First childbirth rates (from never married)
# df %>% 
#   filter(cor != "1990-2015 (cf)") %>% 
#   filter(age != 45) %>% 
#   ggplot(aes(x = age, y = bri,group=emp,color=emp,shape=emp)) +
#   geom_point() +geom_line() + facet_grid(female~cor) +
#   ylab("First birth rate") +xlab("Age")+theme_few() +
#   scale_color_brewer(palette="Paired")+
#   theme(legend.title=element_blank()) + ylim(0,0.16)
# ggsave(height=4,width=8,dpi=200, filename="Figures/Bridal.pdf",  family = "Helvetica")

## Multi state life table results
dfx %>% 
  filter(cor != "1990-2015 (cf)") %>% 
  dplyr::select(cor,emp,child=childbirth,mar=maronly,female) %>% 
  pivot_longer(3:4) %>% 
  mutate(name = case_when(
    name == "child" ~ "Married with child",
    name == "mar" ~ "Married without child"
  )) %>% 
  ggplot(aes(x=emp, y=value, fill=name)) + 
  geom_bar(stat="identity",position="stack")+xlab("")+ylab("%")+theme_few()+
  facet_grid(female~cor) +
  theme(legend.title=element_blank())+
  theme(legend.position="bottom",
        legend.margin=margin(t = -0.75, unit='cm'),
        plot.caption = element_text(hjust = 0))+
  scale_y_continuous(labels=scales::percent,limits = c(0,1))+
  geom_label(aes(label =scales::percent(value,0.1)), 
             position = position_stack(vjust = 0.5),
             show.legend = FALSE,
             label.size = NA) +
  scale_fill_brewer(palette = "Paired")+
  labs(caption = "Source: SSM 2015, JGSS 2006, 2012, 2015, 2016, 2017, 2018")
ggsave(height=6,width=6,dpi=200, filename="Figures/Figure5.pdf",  family = "Helvetica")

dfx %>% 
  filter(emp == "Non-standard") %>% 
  dplyr::select(cor,emp,child=childbirth,mar=maronly,female) %>% 
  pivot_longer(3:4) %>% 
  mutate(name = case_when(
    name == "child" ~ "Married with child",
    name == "mar" ~ "Married without child"
  )) %>% 
  ggplot(aes(x=cor, y=value, fill=name)) + 
  geom_bar(stat="identity",position="stack")+xlab("")+ylab("")+theme_few()+
  theme(legend.title=element_blank())+
  theme(legend.position="bottom",
        legend.margin=margin(t = -0.75, unit='cm'),
        plot.caption = element_text(hjust = 0))+
  facet_wrap(.~female)+
  scale_y_continuous(labels=scales::percent,limits = c(0,0.85))+
  geom_label(aes(label =scales::percent(value,0.1)), 
             position = position_stack(vjust = 0.5),
             show.legend = FALSE,
             label.size = NA) +
  scale_fill_brewer(palette = "Paired")+
  labs(caption = "Source: SSM 2015, JGSS 2006, 2012, 2015, 2016, 2017, 2018")
ggsave(height=4,width=6.5,dpi=200, filename="Figures/Figure6.pdf",  family = "Helvetica")

######################################################################
# Total First Marriage Rates
######################################################################
# tfmr <- df %>% dplyr::select(cor,emp,age,female,bir,n) %>% 
#   filter(cor != "1990-2015 (cf)") %>% 
#   filter(female == "Male") %>% 
#   mutate(bir=bir*n) %>% 
#   group_by(cor,emp,female) %>% 
#   summarise_all(list(sum)) %>% 
#   dplyr::select(cor,emp,female,bir)
# 
# sum(2.14*.9459187+1.75*.0540813)
# sum(2.01*.856754+1.68*.143246)
# 
# sum(2.01*.9459187+1.68*.0540813)
