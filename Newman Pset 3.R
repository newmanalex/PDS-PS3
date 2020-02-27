#Alex Newman 450781
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
#Problem 1
#visualize the super tuesday polling data

#first load in the PrimaryPoll Data
primaryPolls<-read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%Y")

#subset down to only the states that are involved in super tuesday, also subset down to the main candidates in the race
supertuesday<-primaryPolls[primaryPolls$state%in%c("Alabama", "Arkansas", "California", "Colorado", "Maine", "Massachusetts", "Minnesota", "North Carolina", "Oklahoma", "Tennessee", "Texas", "Utah", "Vermont", "Virginia"),]
supertuesday<-supertuesday[supertuesday$candidate_name%in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg", "Tom Steyer"),]

#plot the candidate results for each state
ggplot(data=supertuesday, mapping=aes(x=start_date, y=pct, color=candidate_name))+ 
  geom_point(alpha=.6)+ #draw points with slightly transparent alpha
  geom_smooth(method = "lm")+ #create a linear trendline
  scale_y_continuous(limits=c(0,100))+ # set the y axis limits between 0% and 100%
  facet_wrap(~state)+ #wrap across each state
  labs(title= "Candidate Polling Performance Across Super Tuesday States", x= "Date of Poll", y= "Percent of Voters")+  #label the axes and provide title
  theme_minimal() +  #set theme to minimal
  theme(axis.text.x = element_text(angle = 90, hjust = 1))#rotate dates so they are readable,


#Problem 2
primaryPolls<-read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%y")

twomonths<-filter(primaryPolls, start_date>="2020-01-26" ) #filter data that was taken in last 2 months
twomonthsnarrowed<-select(twomonths, start_date, pct, state, candidate_name) #narrow data set to variables of interest
ProportionPolls<- mutate(twomonthsnarrowed, proportion=pct/100)#Take narrowed data and add a proportion variable

ProportionPolls%>%
  group_by(candidate_name, state)%>%
  summarise(med_sup=median(pct), count=n())


#Problem 3
library(fivethirtyeight)
library(tidyverse)
polls <- read_csv("https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv")
Endorsements <- endorsements_2020
#rename endorsee to candidate name
Endorsements<-rename(Endorsements, candidate_name= endorsee)
#Change Endorsements into a tibble
Endorsements<-as_tibble(Endorsements)
#Filter poll variable to only include 6 candidates
polls<-filter(polls, polls$candidate_name%in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg") )
#subset down data to variables of interest
polls<-select(polls, candidate_name, sample_size, start_date,party, pct)
levels(Endorsements$candidate_name)
#Change endorsements candidate names to be identical to those in the polls dataset
Endorsements$candidate_name<-Endorsements$candidate_name%>%
  recode( "Bernie Sanders"= "Bernard Sanders", "Joe Biden"= "Joseph R. Biden Jr.")
#join the two datasets together
combinedData<-polls%>%
  inner_join(Endorsements, by="candidate_name")
#show there are now only 5 candidates
unique(combinedData$candidate_name)
 
#calculate number of endorsements for each candidate
#note that combinedData has created a lot of extra data as a result of the joins that is not useful
#I will calculate the number of endorsements from the endorsements dataset
EndorsementCount<-Endorsements%>%
  filter(candidate_name%in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg") )%>%
  group_by(candidate_name)%>%
  summarise(n())

#plot the number of endorsements of each candidate and save as object p
Endorsements<-filter(Endorsements, Endorsements$candidate_name%in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg") )
p<-ggplot(data = Endorsements)+
  geom_bar(mapping=aes(candidate_name))
p+ theme_dark()
p<-p+
  labs(title= "Number of Endorsements Among Candididates", x= "Candidate", y= "Number of Endorsements")
p+theme_classic()
