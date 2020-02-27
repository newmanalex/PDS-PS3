#Alex Newman 450781

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


