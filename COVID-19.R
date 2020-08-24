

library(ggplot2)




#----------------------------------------------------------------------------------------
#                                       1
#---------------------------------------------------------------------------------------




#plot with the number of affected people worldwide


attach(confirmed_cases_worldwide)
confirmed_cases_worldwide   
str(confirmed_cases_worldwide)
plot(confirmed_cases_worldwide$date, confirmed_cases_worldwide$cum_cases)


#---------------------------------------------------------------------------------------------
#                                       2
#--------------------------------------------------------------------------------------------

#the previous plot using the ggplot library


ggplot(confirmed_cases_worldwide, aes(x = date, y = cum_cases)) +
geom_point()

#the previous as a line graph
ggplot(confirmed_cases_worldwide, aes(x = date, y = cum_cases)) +
geom_line()

#name the y axis as "Cumulative confirmed cases"
ggplot(confirmed_cases_worldwide, aes(x = date, y = cum_cases)) +
geom_line()+
labs(x = "date", y = "Cumulative confirmed cases")


#---------------------------------------------------------------------------------------------
#                                   3
#---------------------------------------------------------------------------------------------


attach(confirmed_cases_china_vs_world )
confirmed_cases_china_vs_world 

str(confirmed_cases_china_vs_world)

#graph of china vs the world


ggplot(confirmed_cases_china_vs_world , aes(x = date, y = cum_cases))+geom_point()
  


plt_cum_confirmed_cases_china_vs_world=ggplot(confirmed_cases_china_vs_world , aes(x = date, y = cum_cases)) 

plt_cum_confirmed_cases_china_vs_world=plt_cum_confirmed_cases_china_vs_world+geom_line(aes(x = date, y = cum_cases, group=is_china, color=is_china))+ylab("Cumulative confirmed cases")
                                                




plt_cum_confirmed_cases_china_vs_world


#---------------------------------------------------------------------------------------------
#                                        4
#---------------------------------------------------------------------------------------------


library(tidyr)
library(readr)
library(dplyr)

#notate some important days for the affection curve



who_events <- tribble(
  ~ date, ~ event,
  "2020-01-30", "Global health\nemergency declared",
  "2020-03-11", "Pandemic\ndeclared",
  "2020-02-13", "China reporting\nchange"
) %>%
  mutate(date = as.Date(date))

plt_cum_confirmed_cases_china_vs_world=ggplot(confirmed_cases_china_vs_world , aes(x = date, y = cum_cases)) +
geom_line(aes(x = date, y = cum_cases, group=is_china, color=is_china)) +ylab("Cumulative confirmed cases")+
geom_vline(data = who_events, aes(xintercept = date),linetype="dashed")





plt_cum_confirmed_cases_china_vs_world +
geom_text(aes(x=date, label=event), y=100000, data=who_events)


#----------------------------------------------------------------------------------------------
#                                         5
#----------------------------------------------------------------------------------------------

#confirmed cases after the february 15 for china


#install.packages("lubridate")
library(lubridate)
#dhmiourgia triwn kainouriwn sthlwn sto dataset confirmed_cases_china_vs_world
#kainouries sthles einai oi year,month,day
confirmed_cases_china_vs_world=confirmed_cases_china_vs_world %>% mutate(year=year(date))
confirmed_cases_china_vs_world=confirmed_cases_china_vs_world %>% mutate(month=month(date))
confirmed_cases_china_vs_world=confirmed_cases_china_vs_world %>% mutate(day=day(date))



a2=confirmed_cases_china_vs_world%>%
  select(is_china,cum_cases,cases,year,month,day,date)%>%
  filter((is_china=="China")&(month==2)&day>=15)

a2=as.data.frame(a2)

a3=confirmed_cases_china_vs_world%>%
  select(is_china,cum_cases,cases,year,month,day,date)%>%
  filter((is_china=="China")&(month==3))

a3=as.data.frame(a3)

a4=merge(a2,a3,all = T)

a4=as.data.frame(a4)

china_after_feb15 <-a4
  

ggplot( china_after_feb15, aes(x = date, y = cum_cases)) +
geom_line()+
stat_smooth(method = "lm", se = F, col = "red")+  
ylab("Cumulative confirmed cases")



#----------------------------------------------------------------------------------------------
#                                         6
#----------------------------------------------------------------------------------------------


#confirmed cases after the february 15 for the world



a5=confirmed_cases_china_vs_world%>%
  select(is_china,cum_cases,cases,year,month,day,date)%>%
  filter((is_china=="Not China")&(month==2)&day>=15)

a5=as.data.frame(a5)

a6=confirmed_cases_china_vs_world%>%
  select(is_china,cum_cases,cases,year,month,day,date)%>%
  filter((is_china=="Not China")&(month==3))

a6=as.data.frame(a6)

a7=merge(a5,a6,all = T)

a7=as.data.frame(a7)

not_china <-a7


#fitting a linear model


plt_not_china_trend_lin <- ggplot( not_china, aes(x = date, y = cum_cases)) +
  geom_line()+
  stat_smooth(method = "lm", se = F, col = "red")+  
  ylab("Cumulative confirmed cases")

plt_not_china_trend_lin 
  
#---------------------------------------------------------------------------------------------
#                                         7
#---------------------------------------------------------------------------------------------

#fitting a model using the legarithmic scale

plt_not_china_trend_lin +  
  scale_y_log10() 



#------------------------------------------------------------------------------------------------
#                                         8
#-----------------------------------------------------------------------------------------------
#find the top countries by cases

glimpse(confirmed_cases_by_country)
confirmed_cases_by_country


attach(confirmed_cases_by_country)
top_countries_by_total_cases <- confirmed_cases_by_country %>% 
  group_by(country) %>% 
  summarize(total_cases=max(cum_cases)) %>% 
  top_n(7) 

grouped <- group_by(confirmed_cases_by_country, country) 
summarized <- summarize(grouped, total_cases=max(cum_cases)) 
top_countries_by_total_cases <- top_n(summarized, 7) 

top_countries_by_total_cases

#------------------------------------------------------------------------------------------------
#                                           9
#-----------------------------------------------------------------------------------------------

glimpse(confirmed_cases_top7_outside_china)
str(confirmed_cases_top7_outside_china)
confirmed_cases_top7_outside_china
attach(confirmed_cases_top7_outside_china)

ggplot(confirmed_cases_top7_outside_china, aes(x = date, y = cum_cases)) +
geom_point()


ggplot(confirmed_cases_top7_outside_china , aes(x = date, y = cum_cases))+ 
geom_line(aes(x = date, y = cum_cases, group=country, color=country)) +ylab("Cumulative confirmed cases")




