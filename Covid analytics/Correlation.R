library(covid19.analytics)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(psych)


Covid<-covid19.data(case="ts-confirmed")
mobility<- read.csv(file.choose(), header = T)
#worlddatan<-read.csv(file.choose(), header = T)

Mobility5C <- mobility %>% filter((country_region == 'New Zealand' |
                                     country_region == 'United Kingdom' |
                                     country_region  == 'Spain' |
                                     country_region == 'Italy' |
                                     country_region == 'Sweden')& date<"2020-08-01")%>%
  filter(sub_region_1=="")%>% mutate(date= ymd(date))
MobilityUK<-Mobility5C%>%filter((country_region == 'United Kingdom'))
head(MobilityUK)



worlddatauk<- worlddatan %>% filter(cases > 0)%>% 
  filter(countriesAndTerritories == 'United_Kingdom')%>%
  #group_by(dateRep)%>%
  mutate(dateRep = dmy(dateRep))%>% 
  #mutate(incident_cases = c(0, diff(cumulative_cases)))%>%
  # calculate the growth rate of the country
  arrange(dateRep)%>%
  #Difference in time (just in case there are gaps)
  mutate (Diffe_day = dateRep - lag (dateRep))%>% 
  # Difference in cass between days
  mutate(Diffe_growth = cases - lag(cases))%>%     
  # growth rate
  mutate(Raten_ratio = Diffe_growth/lag(cases))


A<-ggplot(worlddatauk,
          aes(x = dateRep, y = Cumulative_number_for_14_days_of_COVID.19_cases_per_100000)) +
  geom_line(color = "red")+
  theme_bw() +
  #stat_smooth(method = "gam",
  #fill = NA,
  #color = "black")+
  xlab("") + ylab("")

head(worlddatauk)

B<-ggplot(MobilityUK,
          aes(x = date, y = retail_and_recreation_percent_change_from_baseline)) +
  geom_line(color = "red")+
  #geom_hline(yintercept = 0, alpha=0.25)+
  theme_bw() +
  #stat_smooth(method = "gam",
  #fill = NA,
  #color = "black")+
  xlab("") + ylab("")

ggarrange(A,B)


C<-ggplot(MobilityUK,
          aes(x = date, y = retail_and_recreation_percent_change_from_baseline)) +
  geom_line(color = "red")+
  geom_line(data=worlddatauk,aes(x = dateRep, y = Cumulative_number_for_14_days_of_COVID.19_cases_per_100000))
#geom_hline(yintercept = 0, alpha=0.25)+
theme_bw() +
  #stat_smooth(method = "gam",
  #fill = NA,
  #color = "black")+
  xlab("") + ylab("")

D<-ggplot(MobilityUK,
          aes(x = date, y = residential_percent_change_from_baseline)) +
  geom_line(color = "red")+
  geom_line(data=worlddatauk,aes(x = dateRep, y = Cumulative_number_for_14_days_of_COVID.19_cases_per_100000))
#geom_hline(yintercept = 0, alpha=0.25)+
theme_bw() +
  #stat_smooth(method = "gam",
  #fill = NA,
  #color = "black")+
  xlab("") + ylab("")

ggarrange(C,D)


#ideas


# Set color palette  
cols = hcl(c(15, 15+180), 100, 65)

# Set scale factor for second axis
#scl = with(figure_1_sample, max(abs(REPPAY))/max(abs(REPP)))
scl = 1000

ggplot(figure_1_sample, aes(x = Year)) + 
  geom_line(aes(y = REPPAY, colour = "REPPAY")) + 
  geom_line(aes(y = REPP*scl, colour = "REPP")) +
  scale_y_continuous(sec.axis = sec_axis(~./scl, name = "Repurchase permium"))+ 
  ggtitle("Graph A: Number of repurchaser and repurchase premium ") +
  theme(legend.position = "bottom",
        legend.margin=margin(-5,0,0,0),
        plot.title = element_text(hjust = 0.5),
        axis.text.y.right=element_text(colour=cols[1]),
        axis.ticks.y.right=element_line(colour=cols[1]),
        axis.title.y.right=element_text(colour=cols[1]),
        axis.text.y=element_text(colour=cols[2]),
        axis.ticks.y=element_line(colour=cols[2]),
        axis.title.y=element_text(colour=cols[2])) +
  scale_colour_manual(values=cols) +
  labs(colour="")
```