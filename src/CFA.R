#Confirmatory Factor Analysis 
library(lavaan)

#Confirmatory Factor Analysis for model 1 
modelCFA <- 'TRUST =~ Q11_1 + Q11_2 + Q11_3 + Q11_4
             IDENTIFICATION =~ Q10_1 + Q10_2 + Q10_3 + Q10_4 + Q10_5 + Q10_6
             COMMITMENT =~ Q9_1 + Q9_2 + Q9_4 + Q9_5
             INTENT =~ Q8_1 + Q8_2 + Q8_3 + Q8_4
             treatment =~ treatmentcondition
             '

#confirmatory factor analysis with diagonally weighted least sqaures 
modelCFA.DWLS <- cfa(modelCFA, data = data, estimator = "WLSMV")
summary(modelCFA.DWLS, fit.measures = TRUE, ci = TRUE, standardized = TRUE)

#Confirmatory Factor Analysis for model 1 (without Q11_4)
modelCFA <- 'TRUST =~ Q11_1 + Q11_2 + Q11_3
             IDENTIFICATION =~ Q10_1 + Q10_2 + Q10_3 + Q10_4 + Q10_5 + Q10_6
             COMMITMENT =~ Q9_1 + Q9_2 + Q9_4 + Q9_5
             INTENT =~ Q8_1 + Q8_2 + Q8_3 + Q8_4
             treatment =~ treatmentcondition
             '
modelCFA.DWLS <- cfa(modelCFA, data = data, estimator = "WLSMV")
summary(modelCFA.DWLS, fit.measures = TRUE, ci = TRUE, standardized = TRUE)

#Confirmatory factor analysis for model 2
modelMOD <- 'PCE_MOD =~ Q7_1 + Q7_2 + Q7_3 + Q7_4 + Q7_5
             INTENT =~ Q8_1 + Q8_2 + Q8_3 + Q8_4
             treatment =~ treatmentcondition
             '
modelMOD <- cfa(modelMOD, data = data, estimator = "WLSMV")
summary(modelMOD, fit.measures = TRUE, ci = TRUE, standardized = TRUE)

#calculate Chronbachs alpha values 
install.packages('psy')
library(psy)

#trust
alpha_trust <- psych::alpha(data[, c('Q11_1', 'Q11_2', 'Q11_3', "Q11_4")])
alpha_trust <- alpha_trust$total$raw_alpha
print(alpha_trust)

alpha_trust <- psych::alpha(data[, c('Q11_1', 'Q11_2', 'Q11_3')])
alpha_trust <- alpha_trust$total$raw_alpha
print(alpha_trust)

#identification
alpha_identification <- psych::alpha(data[, c('Q10_1', 'Q10_2', 'Q10_3', 'Q10_4', 'Q10_5', 'Q10_6')])
alpha_identification <- alpha_identification$total$raw_alpha
print(alpha_identification)

#commitment
alpha_commitment <- psych::alpha(data[, c('Q9_1', 'Q9_2', 'Q9_4', 'Q9_5')])
alpha_commitment <- alpha_commitment$total$raw_alpha
print(alpha_commitment)

#PCE
alpha_PCE <- psych::alpha(data[, c('Q7_1', 'Q7_2', 'Q7_3', 'Q7_4', 'Q7_5')])
alpha_PCE <- alpha_PCE$total$raw_alpha
print(alpha_PCE)

#intent
alpha_intent <- psych::alpha(data[, c('Q8_1', 'Q8_2', 'Q8_3', 'Q8_4')])
alpha_intent <- alpha_intent$total$raw_alpha
print(alpha_intent)

#calculate mean and std deviations 
question_vars <- c("Q11_1", "Q11_2", "Q11_3", "Q11_4",
                   "Q10_1", "Q10_2", "Q10_3", "Q10_4", "Q10_5", "Q10_6",
                   "Q9_1", "Q9_2", "Q9_4", "Q9_5",
                   "Q7_1", "Q7_2", "Q7_3", "Q7_4", "Q7_5",
                   "Q8_1", "Q8_2", "Q8_3", "Q8_4")

# Function to calculate means and standard deviations for multiple variables
calculate_means_sds <- function(data, vars) {
  means <- sapply(vars, function(var) mean(data[[var]], na.rm = TRUE))
  sds <- sapply(vars, function(var) sd(data[[var]], na.rm = TRUE))
  result <- data.frame(Variable = vars, Mean = means, SD = sds)
  return(result)
}

calculate_means_sds(data = data, question_vars)








