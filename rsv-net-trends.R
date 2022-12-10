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
