## downloading needed libraries
library(dplyr)
library(ggplot2)

##compiling dataframs

# comparing racial breakdown of students in new york state vs edgemont (2020-2021)
ethnicityCompare <- data.frame(matrix(ncol = 5, nrow = 6))
colnames(ethnicityCompare) <- c('race','nys_count', 'nys_percent', 'edgemont_count', 'edgemont_percent')
ethnicityCompare$race <- c('native_american_or_alaska_native','african_american','hispanic_latinx', 'AAPI', 'white', 'multiracial')
ethnicityCompare$nys_count <- c(8421, 198507, 327513, 118868, 500831, 29675)
ethnicityCompare$nys_percent <- c(.711344, 16.768414, 27.665894, 10.041096, 42.306526, 2.506726)
ethnicityCompare$edgemont_count <- c(1, 24, 78, 346, 485, 57)
ethnicityCompare$edgemont_percent <- c(.100908, 2.421796, 7.870838, 34.914228, 48.940465, 5.75176)
View(ethnicityCompare)

# racial breakdown of those who graduated in 2021, 2017 4-year cohort - august
gradRace <- data.frame(matrix(ncol = 3, nrow = 6))
colnames(gradRace) <- c('race','nys_grad_rate','edgemont_grad_rate')
gradRace$race <- c('native_american_or_alaska_native','african_american','hispanic_latinx', 'AAPI', 'white', 'multiracial')
gradRace$nys_grad_rate <- c(82,80,80,92,90,84)
gradRace$edgemont_grad_rate <- c(NA,100,92,100,100,100)
View(gradRace)

#edgemont racial breakdown past 2 decades
edgemont20race <- data.frame(matrix(ncol = 8, nrow = 21))
colnames(edgemont20race) <- c('year', 'white', 'african_american','asian','hispanic_latinx','native_american','pacific_islander', 'multiracial')
edgemont20race$year <- c('2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018','2019','2020','2021')
edgemont20race$white <- c(553,720,720,696,763,786,660,671,650,663,606,536,533,539,516,513,513,509,503,487,485)
edgemont20race$african_american <- c(8,9,9,6,4,3,12,16,14,16,18,19,27,19,18,19,17,16,14,20,24)
edgemont20race$asian <- c(169,108,108,153,131,132,216,222,231,244,244,248,248,259,259,268,288,309,326,340,346)
edgemont20race$hispanic_latinx <- c(21,13,13,9,21,15,31,29,23,23,38,75,71,60,61,59,62,68,70,83,78)
edgemont20race$native_american <- c(0,0,0,0,0,0,0,0,2,2,1,2,1,1,0,0,0,1,1,1,1)
edgemont20race$pacific_islander <- c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0)
edgemont20race$multiracial <- c(0,0,0,0,0,0,0,0,0,0,7,38,39,44,49,47,51,46,52,54,57)
View(edgemont20race)

##making plots, basic analysis

#racial breakdown comparison
ggplot(ethnicityCompare, aes(x=race, y=nys_percent))+
  geom_point(aes(color='NYS'), shape=18, size=3)+
  geom_point(aes(x=race, y=edgemont_percent, color='EHS'), shape=18, size=3)+
  scale_color_manual(name='legend',
                     labels=c('NYS','EHS'),
                     values=c('NYS'='red','EHS'='blue'))+
  labs(title='Figure 1 - racial breakdown',
       y = 'percent of population')+
  scale_x_discrete(labels=c('AAPI','Black','Latinx', 'Multiracial', 'Indigenous','White'))+
  theme_minimal()+
  theme(legend.position=c(.745,.7), 
        legend.background = element_rect(fill = "gray"),   
        legend.key = element_rect(fill = "lightgray", color='white'),
        title = element_text(face='bold', family='mono'),
        legend.text = element_text(family='serif'),
        axis.text.x = element_text(family='serif', margin=margin(1,1,9,1)),
        axis.text.y = element_text(family='serif', margin=margin(1,1,1,9)))

#grad rate comparison
ggplot(gradRace, aes(x=race, y=nys_grad_rate))+
  geom_point(aes(color='NYS'), shape=18, size=3)+
  geom_point(aes(x=race, y=edgemont_grad_rate, color='EHS'), shape=18, size=3)+
  scale_color_manual(name='legend',
                     labels=c('NYS','EHS'),
                     values=c('NYS'='red','EHS'='blue'))+
  labs(title='Figure 4 - grad rates for various ethnicities',
       y = 'grad rate')+
  scale_x_discrete(labels=c('AAPI','Black','Latinx', 'Multiracial', 'Indigenous','White'))+
  theme_minimal()+
  theme(legend.position=c(.745,.7), 
        legend.background = element_rect(fill = "gray"),   
        legend.key = element_rect(fill = "lightgray", color='white'),
        title = element_text(face='bold', family='mono'),
        legend.text = element_text(family='serif'),
        axis.text.x = element_text(family='serif', margin=margin(1,1,9,1)),
        axis.text.y = element_text(family='serif', margin=margin(1,1,1,9)))

#ehs racial breakdown across 2 decades
ggplot(edgemont20race, aes(x=year, y=white, group=1, color='w'))+
  geom_point()+
  geom_path()+
  geom_point(aes(y=african_american, group=1, color='aa'))+
  geom_path(aes(y=african_american, color='aa'))+
  geom_point(aes(y=asian, group=1, color='a'))+
  geom_path(aes(y=asian, color='a'))+
  geom_point(aes(y=hispanic_latinx, group=1, color='hislat'))+
  geom_path(aes(y=hispanic_latinx, color='hislat'))+
  geom_point(aes(y=native_american, group=1, color='na'))+
  geom_path(aes(y=native_american, color='na'))+
  geom_point(aes(y=pacific_islander, group=1, color='pi'))+
  geom_path(aes(y=pacific_islander, color='pi'))+
  geom_point(aes(y=multiracial, group=1, color='mr'))+
  geom_path(aes(y=multiracial, color='mr'))+
  scale_color_manual(name='race',
                     labels=c('White','African-American','Asian','Latinx','Indigenous','Pacific Islander','Multiracial'),
                     values=c('w'='red', 'aa'='darkorange2','a'='goldenrod1','hislat'='chartreuse3','na'='blue','pi'='purple','mr'='magenta'))+
  theme_minimal()+
  labs(title='Figure 2 - EHS racial distribution timeline',
       y='number of students')+
  theme(legend.position=c(.18,.55), 
        legend.key.size = unit(.39, "cm"),
        legend.background = element_rect(fill = "gray"),   
        legend.key = element_rect(fill = "white", color='black'),
        title = element_text(face='bold', family='mono'),
        legend.text = element_text(family='serif'),
        axis.text.x = element_text(family='serif', size=8, angle=45),
        axis.text.y = element_text(family='serif', margin=margin(1,1,1,6)))
              
ggplot(edgemont20race, aes(x=year, y=hispanic_latinx, group=1, color='hislat'))+
  geom_point()+
  geom_path()+
  geom_point(aes(y=african_american, group=1, color='aa'))+
  geom_path(aes(y=african_american, color='aa'))+
  geom_point(aes(y=native_american, group=1, color='na'))+
  geom_path(aes(y=native_american, color='na'))+
  geom_point(aes(y=pacific_islander, group=1, color='pi'))+
  geom_path(aes(y=pacific_islander, color='pi'))+
  geom_point(aes(y=multiracial, group=1, color='mr'))+
  geom_path(aes(y=multiracial, color='mr'))+
  scale_color_manual(name='race',
                      labels=c('Latinx','African-American','Indigenous','Pacific Islander','Multiracial'),
                      values=c('aa'='darkorange2','hislat'='chartreuse3','na'='blue','pi'='purple','mr'='magenta'))+
  theme_minimal()+ 
  labs(title='figure 3 - minority populations',
        y='number of students')+
  scale_y_continuous(breaks=seq(from=0, to=80, by=10))+
  theme(legend.position=c(.194,.74), 
         legend.key.size = unit(.39, "cm"),
        legend.background = element_rect(fill = "gray"),   
        legend.key = element_rect(fill = "white", color='black'),
        title = element_text(face='bold', family='mono'),
        legend.text = element_text(family='serif'),
        axis.text.x = element_text(family='serif', size=8, angle=45),
        axis.text.y = element_text(family='serif', margin=margin(1,1,1,6)))
    
  
##diversity score
diversity <- mutate(edgemont20race,
                    diversity_score = 1-((white*(white-1)+
                                            african_american*(african_american-1)+
                                            asian*(asian-1)+
                                            hispanic_latinx*(hispanic_latinx-1)+
                                            native_american*(native_american-1)+
                                            pacific_islander*(pacific_islander-1)+
                                            multiracial*(multiracial-1))
                                         /((white+
                                              african_american+
                                              asian+
                                              hispanic_latinx+
                                              native_american+
                                              pacific_islander+
                                              multiracial)*
                                             (white+
                                                african_american+
                                                asian+hispanic_latinx+
                                                native_american+
                                                pacific_islander+
                                                multiracial-1))))
diversity <- select(diversity, year, diversity_score)
View(diversity)
ggplot(diversity, aes(x=year, y=diversity_score, group=1))+
  geom_point()+
  geom_path()+
  labs(title='figure 5 - diversity score timeline',
       y='diversity score')+
  theme_minimal()+
  theme(legend.position=c(.194,.74), 
        legend.key.size = unit(.39, "cm"),
        legend.background = element_rect(fill = "gray"),   
        legend.key = element_rect(fill = "white", color='black'),
        title = element_text(face='bold', family='mono'),
        legend.text = element_text(family='serif'),
        axis.text.x = element_text(family='serif', size=8, angle=45),
        axis.text.y = element_text(family='serif', margin=margin(1,1,1,6)))+
  annotate("text", x = 17, y = .56, 
           label = "plateau",
           color = "red", fontface = 2)+
  annotate("text", x = 8.5, y = .46, 
           label = "plateau",
           color = "red", fontface = 2)+
  annotate("text", x = 19, y = .44, 
           label = "possible increase,
  similar to 2010-2012 ?",
           color = "purple", size=3, fontface = 1)+
  geom_segment(x = 19, y = .47, xend = 21, yend = .62, color='purple',
               arrow = arrow(length = unit(0.5, "cm")))+
  annotate('text',x=8.8,y=.41,
           label='increase', color='blue', fontface=2)+
  geom_segment(x = 8.5, y = .4, xend = 6.6, yend = .35, color='blue',
               arrow = arrow(length = unit(0.2, "cm")))+
  geom_curve(x = 10.1, y = .41, xend = 11.5, yend = .52, color='blue',
               arrow = arrow(length = unit(0.2, "cm")))
  
