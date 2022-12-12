# explore RSV-net cumulative attack rate question
# Mike Famulare
# Dec 9 2022

library(tidyverse)


d = read.csv('Weekly_Rates_of_Laboratory-Confirmed_RSV_Hospitalizations_from_the_RSV-NET_Surveillance_System.csv',
             header=TRUE)
names(d)
d$week=as.numeric(d$MMWR.Week)
d$Week.ending.date=as.Date(d$Week.ending.date,'%m/%d/%Y')

# look at whole country
pd = d %>% 
  filter(State == 'Entire Network (RSV-NET)') %>%
  filter(MMWR.Week!='Overall') %>%
  filter(Age.Category=='Overall') %>%
  filter(Sex == 'Overall') %>%
  filter(Race == 'Overall') %>%
  filter(Week.ending.date>as.Date('2018-09-01'))

ggplot(pd) +
  geom_line(aes(x=Week.ending.date,y=Rate,group=Season,color=Season)) +
  facet_wrap('Season',scale='free_x')


ggplot(pd) +
  geom_line(aes(x=Week.ending.date,y=Rate,group=Season,color=Season))


ggplot(pd) +
  geom_line(aes(x=Week.ending.date,y=cumsum(Rate),color=Season))


# under 5s
pd = d %>% 
  filter(State == 'Entire Network (RSV-NET)') %>%
  filter(MMWR.Week!='Overall') %>%
  filter(Age.Category=='0-4 years') %>%
  filter(Sex == 'Overall') %>%
  filter(Race == 'Overall') %>%
  filter(Week.ending.date>as.Date('2018-09-01')) %>%
  mutate(cumulative_rate=cumsum(Rate))

# fit seasonal ends from pre-covid
m = lm(cumulative_rate ~ Week.ending.date, 
       data=pd %>% 
         # filter(MMWR.Week==18) %>%
         filter(Week.ending.date<as.Date('2020-06-01')))

pd2=data.frame(Week.ending.date=seq.Date(as.Date('2018-05-01'),as.Date('2023-05-07'),by='week'))
pd2$pre_covid_trend=predict(m,newdata=pd2)
pd2 = pd2 %>% filter(pre_covid_trend>0)

ggplot() +
  geom_line(data=pd,aes(x=Week.ending.date,y=cumulative_rate,color=Season))+
  geom_line(data=pd2,aes(x=Week.ending.date,y=pre_covid_trend),linetype='dashed') +
  xlab('') + ylab('cumulative attack rate per 100k') +
  ggtitle('0-4 years old')
ggsave('rsv-net_cumulative_hospitalization_under5.png',units='in',width=6,height=4)

# overall
pd = d %>% 
  filter(State == 'Entire Network (RSV-NET)') %>%
  filter(MMWR.Week!='Overall') %>%
  filter(Age.Category=='Overall') %>%
  filter(Sex == 'Overall') %>%
  filter(Race == 'Overall') %>%
  filter(Week.ending.date>as.Date('2018-09-01')) %>%
  mutate(cumulative_rate=cumsum(Rate))

# fit seasonal ends from pre-covid
m = lm(cumulative_rate ~ Week.ending.date, 
       data=pd %>% 
         # filter(MMWR.Week==18) %>%
         filter(Week.ending.date<as.Date('2020-06-01')))

pd2=data.frame(Week.ending.date=seq.Date(as.Date('2018-05-01'),as.Date('2023-05-07'),by='week'))
pd2$pre_covid_trend=predict(m,newdata=pd2)
pd2 = pd2 %>% filter(pre_covid_trend>0)

ggplot() +
  geom_line(data=pd,aes(x=Week.ending.date,y=cumulative_rate,color=Season))+
  geom_line(data=pd2,aes(x=Week.ending.date,y=pre_covid_trend),linetype='dashed') +
  xlab('') + ylab('cumulative attack rate per 100k')+
  ggtitle('all ages')
ggsave('rsv-net_cumulative_hospitalization_all_ages.png',units='in',width=6,height=4)



# adults
pd = d %>% 
  filter(State == 'Entire Network (RSV-NET)') %>%
  filter(MMWR.Week!='Overall') %>%
  filter(Age.Category=='18+ (Adults)') %>%
  filter(Sex == 'Overall') %>%
  filter(Race == 'Overall') %>%
  # filter(Week.ending.date>as.Date('2018-09-01')) %>%
  mutate(cumulative_rate=cumsum(Rate))

# fit seasonal ends from pre-covid
m = lm(cumulative_rate ~ Week.ending.date, 
       data=pd %>% 
         # filter(MMWR.Week==17) %>%
         filter(Week.ending.date<as.Date('2020-06-01')))

pd2=data.frame(Week.ending.date=seq.Date(as.Date('2013-05-01'),as.Date('2023-05-07'),by='week'))
pd2$pre_covid_trend=predict(m,newdata=pd2)
pd2 = pd2 %>% filter(pre_covid_trend>0)

ggplot() +
  geom_line(data=pd,aes(x=Week.ending.date,y=cumulative_rate,color=Season))+
  geom_line(data=pd2,aes(x=Week.ending.date,y=pre_covid_trend),linetype='dashed') +
  xlab('') + ylab('cumulative attack rate per 100k')+
  ggtitle('18+ (adults)')
ggsave('rsv-net_cumulative_hospitalization_adults.png',units='in',width=6,height=4)

# looks like sampling changed from 2014-2017


# age
pd = d %>% 
  filter(State == 'Entire Network (RSV-NET)') %>%
  filter(MMWR.Week!='Overall') %>%
  filter(grepl('----',Age.Category) | (Age.Category == '50-64 years')) %>%
  filter(Sex == 'Overall') %>%
  filter(Race == 'Overall') %>%
  filter(Week.ending.date>as.Date('2018-09-01')) %>%
  group_by(State,Age.Category) %>%
  mutate(cumulative_rate=cumsum(Rate)) %>%
  mutate(Age.Category = factor(sub('----','',Age.Category), levels = c('0-<6 months',
                                                                       '6-<12 months',
                                                                       '1-<2 years',
                                                                       '2-4 years',
                                                                       '5-11 years',
                                                                       '12-17 years',
                                                                       '18-29 years',
                                                                       '30-39 years',
                                                                       '40-49 years',
                                                                       '50-64 years',
                                                                       '65-74 years',
                                                                       '75-84 years',
                                                                       '85+ years')))

# fit seasonal ends from pre-covid
m = lm(cumulative_rate ~ Week.ending.date:Age.Category + Age.Category, 
       data=pd %>% 
         # filter(MMWR.Week==17) %>%
         filter(Week.ending.date<as.Date('2020-06-01')))
summary(m)

pd2=expand.grid(Week.ending.date=seq.Date(as.Date('2013-05-01'),as.Date('2023-05-07'),by='week'),
               Age.Category = levels(pd$Age.Category))
pd2$pre_covid_trend=predict(m,newdata=pd2)
pd2$pre_covid_trend_lower=predict(m,newdata=pd2)-2*predict(m,newdata=pd2,se=TRUE)$se
pd2$pre_covid_trend_upper=predict(m,newdata=pd2)+2*predict(m,newdata=pd2,se=TRUE)$se

pd2 = pd2 %>% filter(pre_covid_trend>0) %>%
  mutate(pre_covid_trend_lower = pmax(0,pre_covid_trend_lower)) %>%
  mutate(pre_covid_trend_upper = pmax(0,pre_covid_trend_upper)) 


ggplot() +
  geom_ribbon(data=pd2,aes(x=Week.ending.date,ymin=pre_covid_trend_lower,ymax=pre_covid_trend_upper),alpha=0.3) +
  geom_line(data=pd,aes(x=Week.ending.date,y=cumulative_rate)) +
  # geom_line(data=pd,aes(x=Week.ending.date,y=1+cumulative_rate,group=State,color=State))+
  geom_line(data=pd2,aes(x=Week.ending.date,y=pre_covid_trend),linetype='dashed') +
  facet_wrap('Age.Category',scale='free_y')+
  # scale_y_continuous(trans='log10') +
  xlab('') + ylab('cumulative attack rate per 100k') +
  scale_color_viridis_d()
ggsave('rsv-net_cumulative_hospitalization_age.png',units='in',width=6,height=4)

ggplot() +
  geom_ribbon(data=pd2,aes(x=Week.ending.date,ymin=pre_covid_trend_lower,ymax=pre_covid_trend_upper),alpha=0.3) +
  geom_line(data=pd,aes(x=Week.ending.date,y=cumulative_rate)) +
  # geom_line(data=pd,aes(x=Week.ending.date,y=1+cumulative_rate,group=State,color=State))+
  geom_line(data=pd2,aes(x=Week.ending.date,y=pre_covid_trend),linetype='dashed') +
  facet_wrap('Age.Category',scale='free_y')+
  # scale_y_continuous(trans='log10') +
  xlab('') + ylab('cumulative attack rate per 100k') +
  scale_color_viridis_d()
ggsave('rsv-net_cumulative_hospitalization_age_trend_uncertainty.png',units='in',width=6,height=4)



# age_state
pd = d %>% 
  filter(State != 'Entire Network (RSV-NET)') %>%
  filter(MMWR.Week!='Overall') %>%
  filter(grepl('----',Age.Category) | (Age.Category == '50-64 years')) %>%
  filter(Sex == 'Overall') %>%
  filter(Race == 'Overall') %>%
  filter(Week.ending.date>as.Date('2018-09-01')) %>%
  group_by(State,Age.Category) %>%
  mutate(cumulative_rate=cumsum(Rate)) %>%
  mutate(Age.Category = factor(sub('----','',Age.Category), levels = c('0-<6 months',
                                                                       '6-<12 months',
                                                                       '1-<2 years',
                                                                       '2-4 years',
                                                                       '5-11 years',
                                                                       '12-17 years',
                                                                       '18-29 years',
                                                                       '30-39 years',
                                                                       '40-49 years',
                                                                       '50-64 years',
                                                                       '65-74 years',
                                                                       '75-84 years',
                                                                       '85+ years')))

# fit seasonal ends from pre-covid
m = lm(cumulative_rate ~ Week.ending.date:Age.Category:State + Age.Category:State + State, 
       data=pd %>% 
         # filter(MMWR.Week==17) %>%
         filter(Week.ending.date<as.Date('2020-06-01')))
summary(m)

pd2=expand.grid(Week.ending.date=seq.Date(as.Date('2013-05-01'),as.Date('2023-05-07'),by='week'),
                Age.Category = factor(levels(pd$Age.Category),levels=levels(pd$Age.Category)),
                State=unique(pd$State))
pd2$pre_covid_trend=predict(m,newdata=pd2)
pd2 = pd2 %>% filter(pre_covid_trend>0)


# pd = pd %>% left_join(pd2,by = c("State", "Week.ending.date", "Age.Category"))
# no idea why this join is nan-ing out pre_covid_trend...

ggplot() +
  geom_line(data=pd,aes(x=Week.ending.date,y=1+cumulative_rate,group=State,color=State))+
  facet_wrap('Age.Category',scale='free_y')+
  # scale_y_continuous(trans='log10') +
  xlab('') + ylab('cumulative attack rate per 100k') 
  # scale_color_viridis_d()
ggsave('rsv-net_cumulative_hospitalization_age_state.png',units='in',width=8,height=6)

ggplot() +
  geom_line(data=pd,aes(x=Week.ending.date,y=cumulative_rate,group=Age.Category, color=Age.Category)) +
  # geom_line(data=pd,aes(x=Week.ending.date,y=1+cumulative_rate,group=State,color=State))+
  geom_line(data=pd2,aes(x=Week.ending.date,y=pre_covid_trend,group=Age.Category, color=Age.Category),linetype='dashed') +
  facet_wrap('State~Age.Category',scale='free_y')+
  # scale_y_continuous(trans='log10') +
  xlab('') + ylab('cumulative attack rate per 100k') 
# scale_color_viridis_d()
ggsave('rsv-net_cumulative_hospitalization_age_state_trends.png',units='in',width=20,height=15)
