#===============================================================================
# 2026/06/19
# Scarring the Life Course: Early-Career Precarity and Long-Term Fertility Outcomes in Japan (with Manting Chen)
# Fumiya Uchikoshi, uchikoshi@princeton.alumni.edu
#===============================================================================

######################################################################
# Loading packages
######################################################################
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(zoo)
library(egg)
library(estatapi)
library(gridExtra)
######################################################################
# Census marital status
######################################################################

appid <- "xxxxxxxxx" #Enter your own API ID obtained from e-Stat

# 2015(H27) - JPSC 2013
meta2017 <- estat_getMetaInfo(appId = appid, statsDataId = "0003222622")
meta2012 <- estat_getMetaInfo(appId = appid, statsDataId = "0003085082")
meta2007 <- estat_getMetaInfo(appId = appid, statsDataId = "0003004095")

# Year of entry, job status, sex
df07 <- estat_getStatsData(
  appId = appid,
  statsDataId = "0003004095",
  cdCat01 = "000", #Limit to total
  cdCat04 = c("001","002"),
  cdArea = "00000") %>% 
  dplyr::select(emp=cat02_code, time=cat03_code, sex=cat04_code, value) %>% 
  mutate(year=2007,
         emp=as.numeric(emp),
         time=as.numeric(time),
         sex=ifelse(as.numeric(sex)==1,"Male","Female")) %>% 
  mutate(emp=case_when(
    emp == 0 ~ "Total",
    emp == 1  ~ "Self-employed",
    emp == 5  ~ "Self-employed",
    emp == 7 | emp == 8  ~ "Standard",
    emp >= 11 & emp <= 15  ~ "Non-standard"
  )) %>% 
  mutate(time=case_when(
    time == 2  ~ "2007",
    time == 3  ~ "2006",
    time == 4  ~ "2005",
    time == 5  ~ "2004",
    time == 6  ~ "2003",
    time == 7  ~ "2002",
    time == 8  ~ "2001",
    time == 9  ~ "2000",
    time == 10  ~ "1999",
    time == 11  ~ "1998",
    time == 12  ~ "1993-1997",
    time == 13  ~ "1988-1992",
    time == 14  ~ "1982-1987"
  )) %>% 
  filter(is.na(time)==F & is.na(emp)==F)

df12 <- estat_getStatsData(
  appId = appid,
  statsDataId = "0003085082",
  cdCat04 = "000", #Limit to total
  cdArea = "00000") %>% 
  dplyr::select(emp1=cat03_code,emp2=cat01_code,
                time=cat05_code,long=cat02_code,
                value) %>% 
  mutate(year=2012,
         emp1=as.numeric(emp1),
         emp2=as.numeric(emp2),
         long=as.numeric(long),
         time=as.numeric(time)) %>% 
  filter(emp1>=14) %>% 
  mutate(sex=ifelse(emp1<28,"Male","Female")) %>% 
  mutate(emp1=ifelse(sex=="Female",emp1-28,emp1-14)) %>% 
  mutate(emp1=case_when(
    emp1 == 0 ~ "Total",
    emp1 == 1  ~ "Self-employed",
    emp1 == 2  ~ "Self-employed",
    emp1 == 4 | emp1 == 6  ~ "Standard",
    emp1 == 7  ~ "Non-standard"
  )) %>% 
  mutate(emp2=case_when(
    emp2 == 0 ~ "Total",
    emp2 == 2  ~ "Self-employed",
    emp2 == 3  ~ "Self-employed",
    emp2 == 5 | emp2 == 7  ~ "Standard",
    emp2 == 8  ~ "Non-standard"
  )) %>% 
  filter(time != 1) %>% 
  mutate(time= 2014-time) %>% 
  filter(is.na(time)==F & is.na(emp1)==F &
           is.na(emp2)==F & is.na(value)==F & is.na(long)==F) 

df17 <- estat_getStatsData(
  appId = appid,
  statsDataId = "0003222622",
  cdCat01 = c("1","2"),
  cdCat02 = "0", #Limit to total
  cdCat03 = "1", #Limit to total
  cdArea = "00000") %>% 
  dplyr::select(emp1=cat07_code,emp2=cat04_code,
                time=cat05_code,sex=cat01_code,
                long=cat06_code,
                value) %>% 
  mutate(year=2017,
         emp1=as.numeric(emp1),
         emp2=as.numeric(emp2),
         long=as.numeric(long),
         time=as.numeric(time),
         sex=ifelse(as.numeric(sex)==1,"Male","Female")) %>% 
  mutate(emp1=case_when(
    emp1 == 0 ~ "Total",
    emp1 == 1  ~ "Self-employed",
    emp1 == 2  ~ "Self-employed",
    emp1 == 31 | emp1 == 321  ~ "Standard",
    emp1 >= 3221 & emp1 <= 3226  ~ "Non-standard"
  )) %>% 
  mutate(emp2=case_when(
    emp2 == 0 ~ "Total",
    emp2 == 1  ~ "Self-employed",
    emp2 == 2  ~ "Self-employed",
    emp2 == 31 | emp2 == 321  ~ "Standard",
    emp2 >= 3221 & emp2 <= 3226  ~ "Non-standard"
  )) %>% 
  filter(time != 0) %>% 
  mutate(time= 2018-time) %>% 
  filter(is.na(time)==F & is.na(emp1)==F &
           is.na(emp2)==F & is.na(long)==F) %>% 
  mutate(value=ifelse(is.na(value)==T,0,value))

df1 <- bind_rows(df12,df17) %>% 
  filter(emp2=="Total" & long == 0) %>% 
  dplyr::select(-year,-emp2,-long,emp=emp1) %>% 
  group_by(time,emp,sex) %>% 
  summarise_all(list(sum)) %>% 
  pivot_wider(names_from = emp,values_from = value) %>% 
  dplyr::select(time,sex,nse=3,self=4,stan=5,total=6) %>% 
  mutate(prop=nse/total) %>% 
  filter(time<2016) %>% 
  mutate(sex=factor(sex,levels=c("Male","Female")))

df2 <- bind_rows(df12,df17) %>% 
  filter(long==5) %>% 
  filter(emp2=="Total" | emp2=="Non-standard") %>% 
  filter(emp1=="Standard" | emp1=="Non-standard") %>% 
  dplyr::select(-year,-long) %>% 
  group_by(emp1,emp2,sex,time) %>% 
  summarise_all(list(sum)) %>% 
  mutate(value = zoo::rollmean(value, k = 5, fill = NA)) %>% 
  pivot_wider(names_from = emp2,values_from = value) %>% 
  filter(time < 2006) %>% 
  dplyr::select(time,emp1,sex,nse=4,total=5) %>% 
  mutate(prop=nse/total) %>% 
  mutate(sex=paste0(sex,", ",emp1)) %>% 
  mutate(sex=factor(sex,levels=c("Male, Non-standard","Male, Standard",
                                 "Female, Standard","Female, Non-standard"))) %>% 
  filter(is.na(prop)==F)

fig1 <- ggplot(df1,aes(x=time, y= prop, group=sex)) + 
  geom_line(aes(linetype=sex)) +
  theme_few()+xlab("Year of school graduation")+ylab("% non-standard")+
  coord_cartesian(xlim = c(min(df1$time), max(df1$time) + 4)) +
  theme(legend.title=element_blank(),
        legend.position="none",
        plot.caption = element_text(hjust = 0))+
  scale_y_continuous(labels=scales::percent, limits = c(0,0.4))+
  scale_x_continuous(breaks = seq(1985, 2015, by = 5))+
  geom_text_repel(
    data = subset(df1, time == max(time)),
    aes(label = paste(sex)),
    size = 4,
    nudge_x = 10,
    segment.color = NA) + 
  labs(caption = "Source: Employment Status Survey, 2012 and 2017")

fig2 <- ggplot(df2,aes(x=time, y= prop, group=sex)) + 
  geom_line(aes(linetype=sex)) +
  theme_few()+xlab("Year of school graduation")+ylab("% non-standard")+
  coord_cartesian(xlim = c(min(df2$time), max(df2$time) + 6)) +
  theme(legend.title=element_blank(),
        legend.position="none",
        plot.caption = element_text(hjust = 0))+
  scale_y_continuous(labels=scales::percent, limits = c(0,1))+
  scale_x_continuous(breaks = seq(1985, 2005, by = 5))+
  geom_text_repel(
    data = subset(df2, time == max(time)),
    aes(label = paste(sex)),
    size = 3,
    nudge_x = 10,
    segment.color = NA) + 
  labs(caption = "Note: Values are 5-year moving averages. Sample limited to those \nwho worked for at least 10 years.")

combined_plot <- grid.arrange(fig1,fig2, nrow = 1)

ggsave(combined_plot, height=5,width=10,dpi=200, filename="Figures/Figure2.pdf",  family = "Helvetica")

