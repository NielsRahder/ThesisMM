library(lavaan)
library(psych)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

#load data 
data <- read_delim("Data/data.csv", delim = ";", 
                   escape_double = FALSE, trim_ws = TRUE)
#clean data 
#delete columns 
data <- data[, -c(1:5)] 
data <- data[, -c(2:24)]
data <- data[-c(1,2), ]

#delete people who took to long 
data$`Duration (in seconds)` <- as.numeric(data$`Duration (in seconds)`)
upper_bound <- quantile(data$`Duration (in seconds)`, 0.75) + 1.5 * IQR(data$`Duration (in seconds)`)
lower_bound <- quantile(data$`Duration (in seconds)`, 0.25) - 1.5 * IQR(data$`Duration (in seconds)`)
data <- data[data$`Duration (in seconds)` >= lower_bound & data$`Duration (in seconds)` <= upper_bound, ] #82 items were removed, move from 1158 to 1076
1158-1076

#the upper_bound is 420.125 seconds, the lower bound was -102,875 seconds (which ofcourse cannot happen)
lower_bound
upper_bound
upper_bound / 60

#delete people who filled in that they were over 110 years old this resulted in the deletion of another 19 people so we arrive at 661
data$Q14 <- as.numeric(data$Q14)
data <- data[data$Q14 < 110, ]
1076-1043

data$Q9_3 <- as.numeric(data$Q9_3)
data <- data %>%
  filter(Q9_3 == 2) #filter out all the people who did not pass the attention check this lowers the total number from 1043 to 733 people 
1043-733

#recode so that control = 1 and cashier = 2 and michelle = 3
data <- data %>% 
  mutate(treatmentcondition = recode(FL_13_DO, 
                                     'BlockControl' = 1,
                                     'BlockCashier' = 2,
                                     'BlockMichelle' = 3))

#make numeric scores 
data <- data %>%
  mutate_at(vars(-Q13, Q14, Q15, Q16), as.numeric)

#recode question 8 due to a technical error
data$Q8_1 <- 1/3 + (2/3)*data$Q8_1
data$Q8_2 <- 1/3 + (2/3)*data$Q8_2
data$Q8_3 <- 1/3 + (2/3)*data$Q8_3
data$Q8_4 <- 1/3 + (2/3)*data$Q8_4

#create composite scores
data <- 
  data %>% 
  rowwise() %>%
  mutate(Trust = mean(c(Q11_1, Q11_2, Q11_3, Q11_4)))

data <- 
  data %>% 
  rowwise() %>%
  mutate(Identification = mean(c(Q10_1, Q10_2, Q10_3, Q10_4, Q10_5, Q10_6)))

data <- 
  data %>% 
  rowwise() %>%
  mutate(Commitment = mean(c(Q9_1, Q9_2, Q9_3, Q9_4, Q9_5)))

data <- 
  data %>% 
  rowwise() %>%
  mutate(PE = mean(c(Q7_1, Q7_2, Q7_3, Q7_4, Q7_5)))

data <- 
  data %>% 
  rowwise() %>%
  mutate(Intent = mean(c(Q8_1, Q8_2, Q8_3, Q8_4)))

duration <- mean(data$`Duration (in seconds)`, na.rm = T)
duration / 60






















