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
  dplyr::select(cor=workch,emp,mar,age,female) 

jgss1 <- bind_rows(jgss) %>% 
  dplyr::select(cor=workch,emp,mar,age,female) 

## For standardization, see Calculating the standard error in Freddie Bray and Jacques Ferlay.pdf
# Add weights

###################### Bootstrap ##############################
meanfun <- function(data, i){
  d <- data[i, ]
  return(mean(d))   
}
set.seed(2022)
set.seed(123)
dd <- lapply(0:1,function(i){
      lapply(1:3,function(j){
      #lapply(1:2,function(k){
      lapply(1:7,function(l){
  df <- bind_rows(ssm1,jgss1) %>% 
    filter(female==i) %>% 
    filter(emp==j) %>%
    #filter(cor==k) %>%
    filter(age==l) %>%
    data.frame() 
  bo <- boot(df[, "mar", drop = FALSE], statistic=meanfun, R=500)

  x <- data.frame(mean = bo$t0,
                se = sd(bo$t),
                female=i,
                emp=j,
                #cor=k,
                age=l)
      })
      })
    #  })
})

df1 <- melt(dd) %>% 
  filter(variable=="mean"|variable=="se") %>% 
  dplyr::select(female=L1,emp=L2,age=L3,everything()) %>% 
  pivot_wider(names_from = c("variable"),values_from = value) %>% 
  mutate(upper=mean+1.96*se,lower=mean-1.96*se) %>% 
  mutate(emp=case_when(
    emp == 1 ~ "Standard",
    emp == 2 ~ "Non-standard - parttime",
    emp == 3 ~ "Non-standard - dispatched"
  )) %>% 
  mutate(emp = factor(emp,levels=c("Standard","Non-standard - parttime","Non-standard - dispatched"))) %>% 
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
    emp == 2 ~ "Non-standard - parttime",
    emp == 3 ~ "Non-standard - dispatched"
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
    #lapply(1:2,function(k){
      lapply(1:7,function(l){
        df <- bind_rows(ssm2,jgss2) %>% 
          filter(female==i) %>% 
          filter(emp==j) %>%
          #filter(cor==k) %>%
          filter(age==l) %>%
          data.frame() 
        bo <- boot(df[, "bir", drop = FALSE], statistic=meanfun, R=500)
        
        x <- data.frame(mean = bo$t0,
                        se = sd(bo$t),
                        female=i,
                        emp=j,
                       # cor=k,
                        age=l)
      })
    })
 # })
})

df2 <- melt(dd) %>% 
  filter(variable=="mean"|variable=="se") %>% 
  dplyr::select(female=L1,emp=L2,age=L3,everything()) %>% 
  pivot_wider(names_from = c("variable"),values_from = value) %>% 
  mutate(upper=mean+1.96*se,lower=mean-1.96*se) %>% 
  mutate(emp=case_when(
    emp == 1 ~ "Standard",
    emp == 2 ~ "Non-standard - parttime",
    emp == 3 ~ "Non-standard - dispatched"
  )) %>% 
  mutate(emp = factor(emp,levels=c("Standard","Non-standard - parttime","Non-standard - dispatched"))) %>% 
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
  dplyr::select(emp,age,female,mean,upper,lower,name=type) %>% 
  mutate(name = case_when(
    name == "marriage" ~ "First marriage rate",
    name == "birth" ~ "First birth rate"
  )) %>% 
  mutate(name=factor(name,levels=c("First marriage rate",
                                   "First birth rate")))

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
  group_by(emp,age,female) %>% 
  summarise_all(list(mean)) %>% 
  mutate(emp=case_when(
    emp == 1 ~ "Standard",
    emp == 2 ~ "Non-standard - parttime",
    emp == 3 ~ "Non-standard - dispatched"
  )) %>% 
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
  )) 

######################################################################
# Create counter factual dataset
######################################################################

df <- df1 %>% 
  dplyr::select(female,emp,age,mar=mean) %>% 
  left_join(df2) %>% 
  dplyr::select(female,emp,age,mar,bir=mean) %>% 
  left_join(df3) %>% 
  mutate(emp = factor(emp,levels=c("Standard","Non-standard - parttime","Non-standard - dispatched"))) %>%
  filter(is.na(age)==F) %>% 
  dplyr::select(female,emp,age,mar,bri,bir)

df <- df %>% 
  group_by(emp, female) %>% 
  arrange(emp, female, age) %>% 
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
  dplyr::select(emp,female,childbirth,marriage) %>% 
  mutate(maronly = marriage - childbirth,
         prop = childbirth/marriage) %>% 
  mutate(female=factor(female,levels=c("Male","Female")))

######################################################################
# ggplot
######################################################################
## Multi state life table results
dfx %>% 
  filter(emp != "Standard") %>% 
  dplyr::select(emp,child=childbirth,mar=maronly,female) %>% 
  pivot_longer(2:3) %>% 
  mutate(name = case_when(
    name == "child" ~ "Married with child",
    name == "mar" ~ "Married without child"
  )) %>% 
  ggplot(aes(x=emp, y=value, fill=name)) + 
  geom_bar(stat="identity",position="stack")+xlab("")+ylab("%")+theme_few()+
  facet_grid(.~female) +
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
ggsave(height=6,width=12,dpi=200, filename="Figures/AppFigure2.pdf",  family = "Helvetica")
