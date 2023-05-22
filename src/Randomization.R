#check for differences between groups for age 
model_age <- aov(data$Q14 ~ data$treatmentcondition)
summary(model_age)

#conduct Chi-Square test for income 
model_income <- chisq.test(data$Q15, data$treatmentcondition)
model_income

# Calculate the expected frequency count for gender
table_gender <- table(data$Q13, data$treatmentcondition)
expected_counts_gender <- chisq.test(table_gender)$expected

# Check if more than 20% of the cells have expected counts less than 5
prop_small_expected <- sum(expected_counts_gender < 5) / length(expected_counts_gender)
if (prop_small_expected > 0.2) {
  print("The assumption for the chi-square test for gender is violated.")
} else {
  print("The assumption for the chi-square test for gender is not violated.")
}

fisher.test(table_gender, workspace = 2e8, hybrid = TRUE, control = list(trace = 1))

# Calculate the expected frequency count for education  
table_education <- table(data$Q16, data$treatmentcondition)
expected_counts_education <- chisq.test(table_education)$expected

# Check if more than 20% of the cells have expected counts less than 5
prop_small_expected <- sum(expected_counts_education < 5) / length(expected_counts_education)
if (prop_small_expected > 0.2) {
  print("The assumption for the chi-square test for education is violated.")
} else {
  print("The assumption for the chi-square test for education is not violated.")
}

fisher.test(table_education, workspace = 2e8, hybrid = TRUE, control = list(trace = 1))

#calculate the expected frequency for income
table_income <- table(data$Q15, data$treatmentcondition)
expected_counts_income <- chisq.test(table_income)$expected






