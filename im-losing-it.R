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
gradRace$nys_grad_rate <- c(82,92,80,80,90,84)
gradRace$edgemont_grad_rate <- c(NA,100,100,100,92,100)
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
  geom_point()+
  geom_point(aes(x=race, y=edgemont_percent))

#grad rate comparison
ggplot(gradRace, aes(x=race, y=nys_grad_rate))+
  geom_point()+
  geom_point(aes(x=race, y=edgemont_grad_rate))

#ehs racial breakdown across 2 decades
ggplot(edgemont20race, aes(x=year, y=white, group=1))+
  geom_point()+
  geom_path()+
  geom_point(aes(y=african_american, group=1))+
  geom_path(aes(y=african_american))+
  geom_point(aes(y=asian, group=1))+
  geom_path(aes(y=asian))+
  geom_point(aes(y=hispanic_latinx, group=1))+
  geom_path(aes(y=hispanic_latinx))+
  geom_point(aes(y=native_american, group=1))+
  geom_path(aes(y=native_american))+
  geom_point(aes(y=pacific_islander, group=1))+
  geom_path(aes(y=pacific_islander))+
  geom_point(aes(y=multiracial, group=1))+
  geom_path(aes(y=multiracial))
              
ggplot(edgemont20race, aes(x=year, y=hispanic_latinx, group=1))+
  geom_point()+
  geom_path()+
  geom_point(aes(y=native_american, group=1))+
  geom_path(aes(y=native_american))+
  geom_point(aes(y=pacific_islander, group=1))+
  geom_path(aes(y=pacific_islander))+
  geom_point(aes(y=multiracial, group=1))+
  geom_path(aes(y=multiracial))
  
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
  geom_path()
