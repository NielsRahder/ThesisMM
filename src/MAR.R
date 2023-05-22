#check for MAR data 
install.packages('naniar')
install.packages('mice')
install.packages('VIM')
install.packages('missForest')
install.packages('GGally')
library(naniar)
library(mice)
library(VIM)
library(missForest)
library(GGally)
#create dataset with only the specific questions
data_MAR <- subset(data, select = -c(RandomID, 
                                     FL_13_DO, 
                                     `Duration (in seconds)`))
#count number of NA's (= 53)
sum(is.na(data_MAR))

#create plot of missing data 
md.pattern(data_MAR, plot = TRUE, rotate.names = TRUE)

#create graph of missing data 
missplot <- aggr(data_MAR, col = c('blue', 'red'), 
                 numbers = TRUE, sortVars=TRUE, 
                 labels = names(data_MAR), cex.axis = .7, 
                 gap = 3, ylab=c('Missing data', 'Pattern'))

summary(data_MAR)
data_MAR <- as.numeric(data_MAR$Q7_1)
