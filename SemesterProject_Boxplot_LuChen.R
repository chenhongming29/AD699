#Boxplot# (Lu Chen)
QueenAnne <- read.csv('QueenAnne.CSV')
library(ggplot2)
ggplot(QueenAnne,aes(x=property_type,y=price)) + geom_boxplot()
