## downloading needed libraries
library(dplyr)

# comparing racial breakdown of students in new york state vs edgemont (2020-2021)
ethnicityCompare <- data.frame(matrix(ncol = 5, nrow = 6))
colnames(ethnicityCompare) <- c('race','nys_count', 'nys_percent', 'edgemont_count', 'edgemont_percent')
ethnicityCompare$race <- c('native_american_or_alaska_native','african_american','hispanic_latinx', 'AAPI', 'white', 'multiracial')
ethnicityCompare$nys_count <- c(8421, 198507, 327513, 118868, 500831, 29675)
ethnicityCompare$nys_percent <- c(.711344, 16.768414, 27.665894, 10.041096, 42.306526, 2.506726)
ethnicityCompare$edgemont_count <- c(1, 24, 78, 346, 485, 57)
ethnicityCompare$edgemont_percent <- c(.100908, 2.421796, 7.870838, 34.914228, 48.940465, 5.75176)
View(ethnicityCompare)
saveRDS(ethnicityCompare, 'ethnicityCompare')
