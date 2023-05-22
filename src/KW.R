#kruskal wallis test for the different latent variables, with resect to the treatment condition
shapiro.test(data$Trust)
shapiro.test(data$Commitment)
shapiro.test(data$Identification)
shapiro.test(data$Intent)

KW_Trust <- kruskal.test(Trust ~ treatmentcondition, data = data)
KW_Identification <-kruskal.test(Identification ~ treatmentcondition, data = data)
KW_Commitment <-kruskal.test(Commitment ~ treatmentcondition, data = data)
KW_PE <-kruskal.test(PE ~ treatmentcondition, data = data)
KW_Intent <-kruskal.test(Intent ~ treatmentcondition, data = data)

print(KW_Trust) # H = 1.2152, df = 2, p-value = 0.5446
print(KW_Identification) # H = 3.9376, df = 2, p-value = 0.1396
print(KW_Commitment) # H = 0.57979, df = 2, p-value = 0.7483
print(KW_PE) # H = 0.92369, df = 2, p-value = 0.6301
print(KW_Intent) # H = 0.77697, df = 2, p-value = 0.6781

#create boxplots for latent variables, with different treatment condition.
par(mfrow=c(2,3))

boxplot(Trust ~ treatmentcondition, data = data, xlab = "Treatment Condition", ylab = "Trust")
boxplot(Identification ~ treatmentcondition, data = data, xlab = "Identification", ylab = "Identification")
boxplot(Commitment ~ treatmentcondition, data = data, xlab = "Commitment", ylab = "Commitment")
boxplot(PE ~ treatmentcondition, data = data, xlab = "Perceived Effectiveness", ylab = "PE")
boxplot(Intent ~ treatmentcondition, data = data, xlab = "Intent", ylab = "Intent")

par(mfrow=c(1,1))

#Function to calculate the mean and sd for the specified condition
intent_summary <- function(data, var_name, condition){
  median_val <- median(data[[var_name]][data$treatmentcondition == condition], na.rm = TRUE)
  q1 <- quantile(data[[var_name]][data$treatmentcondition == condition], 0.25, na.rm = TRUE)
  q3 <- quantile(data[[var_name]][data$treatmentcondition == condition], 0.75, na.rm = TRUE)
  iqr_val <- q3 - q1
  # Return the results as a data frame
  return(data.frame(condition = condition, median_val = median_val, iqr_val = iqr_val, q1, q3))
}

intent_summary(data, 'Intent', 1)
intent_summary(data, 'Intent', 2)
intent_summary(data, 'Intent', 3)

intent_summary(data, 'Trust', 1)
intent_summary(data, 'Trust', 2)
intent_summary(data, 'Trust', 3)

intent_summary(data, 'Identification', 1)
intent_summary(data, 'Identification', 2)
intent_summary(data, 'Identification', 3)

intent_summary(data, 'Commitment', 1)
intent_summary(data, 'Commitment', 2)
intent_summary(data, 'Commitment', 3)

intent_summary(data, 'PE', 1)
intent_summary(data, 'PE', 2)
intent_summary(data, 'PE', 3)
