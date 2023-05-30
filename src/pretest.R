#load data 
library(readr)
library(tidyr)
library(dplyr)
library(reshape) 

#load data 
Data_PT <- read_csv("Data/pretest status CSM-LM_April 11, 2023_02.26 2.csv")

#clean data 
Data_PT <- select(Data_PT, -(1:8))
Data_PT <- select(Data_PT, -(2:9), -c(Q29_NPS_GROUP, Q37_NPS_GROUP, Q44_NPS_GROUP, Q49_NPS_GROUP, Q54_NPS_GROUP, Q59_NPS_GROUP))
data_PT <- data[-c(1, 2), ] #remove rows 

data_PT <- data_PT %>%
  mutate_at(vars(-ResponseId), as.numeric)

# Create a new data frame with the desired columns and their means
data_attractiveness <- data.frame(
  "EM-E" = rowMeans(data_PT[, c("Q38_1", "Q38_2", "Q38_3")]),
  "MO-F" = rowMeans(data_PT[, c("Q45_1", "Q45_2", "Q45_1")]),
  "MS-R" = rowMeans(data_PT[, c("Q50_1", "Q50_2", "Q50_3")]),
  "LA-C" = rowMeans(data_PT[, c("Q55_1", "Q55_2", "Q55_3")]),
  "JN-L" = rowMeans(data_PT[, c("Q60_1", "Q60_2", "Q60_3")]),
  "MC-D" = rowMeans(data_PT[, c("Q65_1", "Q65_2", "Q65_3")])
)

data_competence <- data.frame( 
  "EM-E" = rowMeans(data_PT[, c("Q40_1", "Q40_2", "Q40_3")]),
  "MO-F" = rowMeans(data_PT[, c("Q47_1", "Q47_2", "Q47_1")]),
  "MS-R" = rowMeans(data_PT[, c("Q52_1", "Q52_2", "Q52_3")]),
  "LA-C" = rowMeans(data_PT[, c("Q57_1", "Q57_2", "Q57_3")]),
  "JN-L" = rowMeans(data_PT[, c("Q62_1", "Q62_2", "Q62_3")]),
  "MC-D" = rowMeans(data_PT[, c("Q67_1", "Q67_2", "Q67_3")])
)

data_trustworthiness <- data.frame( 
  "EM-E" = rowMeans(data_PT[, c("Q39_1", "Q39_2", "Q39_3")]),
  "MO-F" = rowMeans(data_PT[, c("Q46_1", "Q46_2", "Q46_1")]),
  "MS-R" = rowMeans(data_PT[, c("Q51_1", "Q51_2", "Q51_3")]),
  "LA-C" = rowMeans(data_PT[, c("Q56_1", "Q56_2", "Q56_3")]),
  "JN-L" = rowMeans(data_PT[, c("Q61_1", "Q61_2", "Q61_3")]),
  "MC-D" = rowMeans(data_PT[, c("Q66_1", "Q66_2", "Q66_3")])
)

data_socioeconomic <- data.frame(
  "EM-E" = rowMeans(data_PT[, c("Q37")]),
  "MO-F" = rowMeans(data_PT[, c("Q44")]),
  "MS-R" = rowMeans(data_PT[, c("Q49")]),
  "LA-C" = rowMeans(data_PT[, c("Q54")]),
  "JN-L" = rowMeans(data_PT[, c("Q59")]),
  "MC-D" = rowMeans(data_PT[, c("Q64")])
)

  
#add id colum based on rownumber 
data_attractiveness$ID <- seq.int(nrow(data_attractiveness))
data_competence$ID <- seq.int(nrow(data_competence))
data_trustworthiness$ID <- seq.int(nrow(data_trustworthiness))
data_socioeconomic$ID <- seq.int(nrow(data_socioeconomic))

#create matrix 
Matrix_attractiveness <- data.matrix(data_attractiveness)
Matrix_competence <- data.matrix(data_competence)
Matrix_trustworthiness <- data.matrix(data_trustworthiness)
Matrix_socioeconomic <- data.matrix(data_socioeconomic)

#convert to long format
data_attractiveness_long <- melt(data_attractiveness, id.vars = "ID")
data_competence_long <- melt(data_competence, id.vars = "ID")
data_trustworthiness_long <- melt(data_trustworthiness, id.vars = "ID")
data_socioeconomic_long <- melt(data_socioeconomic, id.vars = "ID")

#friedman 
friedman.test(Matrix_attractiveness)
friedman.test(Matrix_competence)
friedman.test(Matrix_trustworthiness)
friedman.test(Matrix_socioeconomic)

# Legend:
# EM.E = Elon Musk (Entrepreneur)
# MO.F = Michelle Obama (First Lady)
# MS.R = Marnix Storm (Receptionist)
# LA.C = Lyla Aleesha (Cashier)
# JN.L = Jessica Nikoletta (Lawyer)
# MC.D = Mohammed Cafer (Dentist)

library(ggplot2)
ggplot(data_socioeconomic_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(0.2), size = 2) +
  labs(title = "Perceived socioeconomic", x = "Variable", y = "Value") +
  scale_fill_manual(values = c("EM.E" = "#FFD2D2",
                               "MO.F" = "#D2D2FF",
                               "MS.R" = "#D2FFD2",
                               "LA.C" = "#FFE6CC",
                               "JN.L" = "#E6CCFF",
                               "MC.D" = "#FFFFCC"),
                    labels = c("Elon Musk (Entrepreneur)",
                               "Michelle Obama (First Lady)",
                               "Marnix Storm (Receptionist)",
                               "Lyla Aleesha (Cashier)",
                               "Jessica Nikoletta (Lawyer)",
                               "Mohammed Cafer (Dentist)")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

#calculate means ans SD's 

calculate_mean_sd <- function(data, individuals) {
  result <- data.frame(Individual = character(), Mean = numeric(), SD = numeric(), stringsAsFactors = FALSE)
  
  for (i in 1:length(individuals)) {
    individual <- individuals[i]
    mean_value <- mean(data[[individual]], na.rm = TRUE)
    sd_value <- sd(data[[individual]], na.rm = TRUE)
    
    result[i, "Individual"] <- individual
    result[i, "Mean"] <- mean_value
    result[i, "SD"] <- sd_value
  }
  
  return(result)
}

calculate_mean_sd(data_trustworthiness, individuals)
calculate_mean_sd(data_competence, individuals)
calculate_mean_sd(data_attractiveness, individuals)
calculate_mean_sd(data_socioeconomic, individuals)

#data wrangling
data_trustworthiness <- round(data_trustworthiness, 2)
data_competence <- round(data_competence, 2)
data_attractiveness <- round(data_attractiveness, 2 )
data_socioeconomic <- round(data_socioeconomic, 2)

data_trustworthiness <- na.omit(data_trustworthiness)
data_competence <- na.omit(data_competence)
data_attractiveness <- na.omit(data_attractiveness)
data_socioeconomic <-na.omit(data_socioeconomic)

#data_trustworthiness <- subset(data_trustworthiness, select = -ID)
#data_attractiveness <- subset(data_attractiveness, select = -ID)
#data_competence <- subset(data_competence, select = -ID)
library(coin)
library(survival)
install.packages("coin")
# Create an empty list to store the test results
test_results <- list()

# List of individuals
individuals <- c('EM.E', 'MO.F', 'MS.R', 'LA.C', 'JN.L', 'MC.D')

# Perform the Wilcoxon signed-rank test for each pair of individuals
for (i in 1:(length(individuals)-1)) {
  for (j in (i+1):length(individuals)) {
    individual1 <- individuals[i]
    individual2 <- individuals[j]
    
    # Perform the Wilcoxon signed-rank test for the pair
    result <- wilcoxsign_test(data_competence[[individual1]] ~ data_competence[[individual2]], distribution = "exact")
    
    # Create a unique identifier for the test result
    result_name <- paste(individual1, "vs", individual2, sep = "_")
    
    # Store the test result in the list
    test_results[[result_name]] <- result
  }
}

# Print the test results
for (i in 1:length(test_results)) {
  result_name <- names(test_results)[i]
  result <- test_results[[result_name]]
  cat("Comparison:", result_name, "\n")
  print(result)
  cat("\n")
}


wilcox.test(data_competence$EM.E, data_competence$MO.F, paired = TRUE)
4.5327 / sqrt(62)

0.05 / 15 
0.01 / 15 
0.001 / 15 
