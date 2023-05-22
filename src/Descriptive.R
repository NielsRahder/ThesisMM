#descriptive statistics general 
#age 
mean(data$Q14, na.rm = TRUE)
sd(data$Q14, na.rm = TRUE)

#gender 
sum(data$Q13 == 1, na.rm = TRUE)
sum(data$Q13 == 2, na.rm = TRUE)
sum(data$Q13 == 3, na.rm = TRUE)
sum(data$Q13 == 4, na.rm = TRUE)
sum(is.na(data$Q13))


#income group 
sum(data$Q15 == 1, na.rm = TRUE)
sum(data$Q15 == 2, na.rm = TRUE) 
sum(data$Q15 == 3, na.rm = TRUE)
sum(data$Q15 == 4, na.rm = TRUE)
sum(data$Q15 == 5, na.rm = TRUE)

#level of education 
sum(data$Q16 == 1, na.rm = TRUE)
sum(data$Q16 == 2, na.rm = TRUE)
sum(data$Q16 == 3, na.rm = TRUE)
sum(data$Q16 == 4, na.rm = TRUE)
sum(data$Q16 == 5, na.rm = TRUE)
sum(data$Q16 == 6, na.rm = TRUE)
sum(data$Q16 == 7, na.rm = TRUE)

data$Q13 <- as.numeric(data$Q13)

#for treatmentgrou = 1 (control)
data %>% 
  filter(treatmentcondition == 1) %>%
  pull(Q14) %>%
  mean(na.rm = TRUE)
data %>% 
  filter(treatmentcondition == 1) %>%
  pull(Q14) %>%
  sd(na.rm = TRUE)


#gender
sum(data$Q13 == 1 & data$treatmentcondition == 1, na.rm = TRUE)
sum(data$Q13 == 2 & data$treatmentcondition == 1, na.rm = TRUE)
sum(data$Q13 == 3 & data$treatmentcondition == 1, na.rm = TRUE)
sum(data$Q13 == 4 & data$treatmentcondition == 1, na.rm = TRUE)

#level of income  
sum(data$treatmentcondition == 1 & data$Q15 == 1, na.rm = TRUE)
sum(data$treatmentcondition == 1 & data$Q15 == 2, na.rm = TRUE)
sum(data$treatmentcondition == 1 & data$Q15 == 3, na.rm = TRUE)
sum(data$treatmentcondition == 1 & data$Q15 == 4, na.rm = TRUE)
sum(data$treatmentcondition == 1 & data$Q15 == 5, na.rm = TRUE)


#level of education
sum(data$treatmentcondition == 1 & data$Q16 == 1, na.rm = TRUE)
sum(data$treatmentcondition == 1 & data$Q16 == 2, na.rm = TRUE)
sum(data$treatmentcondition == 1 & data$Q16 == 3, na.rm = TRUE)
sum(data$treatmentcondition == 1 & data$Q16 == 4, na.rm = TRUE)
sum(data$treatmentcondition == 1 & data$Q16 == 5, na.rm = TRUE)
sum(data$treatmentcondition == 1 & data$Q16 == 6, na.rm = TRUE)
sum(data$treatmentcondition == 1 & data$Q16 == 7, na.rm = TRUE)


#for treatment condition = 2 = Lyla Aleesha
data %>% 
  filter(treatmentcondition == 2) %>%
  pull(Q14) %>%
  mean(na.rm = TRUE)
data %>% 
  filter(treatmentcondition == 2) %>%
  pull(Q14) %>%
  sd(na.rm = TRUE)


#gender
sum(data$Q13 == 1 & data$treatmentcondition == 2, na.rm = TRUE)
sum(data$Q13 == 2 & data$treatmentcondition == 2, na.rm = TRUE)
sum(data$Q13 == 3 & data$treatmentcondition == 2, na.rm = TRUE)
sum(data$Q13 == 4 & data$treatmentcondition == 2, na.rm = TRUE)

#level of income  
sum(data$treatmentcondition == 2 & data$Q15 == 1, na.rm = TRUE)
sum(data$treatmentcondition == 2 & data$Q15 == 2, na.rm = TRUE)
sum(data$treatmentcondition == 2 & data$Q15 == 3, na.rm = TRUE)
sum(data$treatmentcondition == 2 & data$Q15 == 4, na.rm = TRUE)
sum(data$treatmentcondition == 2 & data$Q15 == 5, na.rm = TRUE)

#level of education
sum(data$treatmentcondition == 2 & data$Q16 == 1, na.rm = TRUE)
sum(data$treatmentcondition == 2 & data$Q16 == 2, na.rm = TRUE)
sum(data$treatmentcondition == 2 & data$Q16 == 3, na.rm = TRUE)
sum(data$treatmentcondition == 2 & data$Q16 == 4, na.rm = TRUE)
sum(data$treatmentcondition == 2 & data$Q16 == 5, na.rm = TRUE)
sum(data$treatmentcondition == 2 & data$Q16 == 6, na.rm = TRUE)
sum(data$treatmentcondition == 2 & data$Q16 == 7, na.rm = TRUE)


#treatmentgroup 3 = michelle 
#for treatment condition = 2
data %>% 
  filter(treatmentcondition == 3) %>%
  pull(Q14) %>%
  mean(na.rm = TRUE)
data %>% 
  filter(treatmentcondition == 3) %>%
  pull(Q14) %>%
  sd(na.rm = TRUE)


#gender
sum(data$Q13 == 1 & data$treatmentcondition == 3, na.rm = TRUE)
sum(data$Q13 == 2 & data$treatmentcondition == 3, na.rm = TRUE)
sum(data$Q13 == 3 & data$treatmentcondition == 3, na.rm = TRUE)
sum(data$Q13 == 4 & data$treatmentcondition == 3, na.rm = TRUE)

#level of income  
sum(data$treatmentcondition == 3 & data$Q15 == 1, na.rm = TRUE)
sum(data$treatmentcondition == 3 & data$Q15 == 2, na.rm = TRUE)
sum(data$treatmentcondition == 3 & data$Q15 == 3, na.rm = TRUE)
sum(data$treatmentcondition == 3 & data$Q15 == 4, na.rm = TRUE)
sum(data$treatmentcondition == 3 & data$Q15 == 5, na.rm = TRUE)

#level of education
sum(data$treatmentcondition == 3 & data$Q16 == 1, na.rm = TRUE)
sum(data$treatmentcondition == 3 & data$Q16 == 2, na.rm = TRUE)
sum(data$treatmentcondition == 3 & data$Q16 == 3, na.rm = TRUE)
sum(data$treatmentcondition == 3 & data$Q16 == 4, na.rm = TRUE)
sum(data$treatmentcondition == 3 & data$Q16 == 5, na.rm = TRUE)
sum(data$treatmentcondition == 3 & data$Q16 == 6, na.rm = TRUE)
sum(data$treatmentcondition == 3 & data$Q16 == 7, na.rm = TRUE)


#######additional ddescriptive statistics per group 
# convert to numeric
data_2_gender <- data[!(data$Q13 == 3), ]
data_2_gender$Q13 <- as.numeric(data_2_gender$Q13)

# Convert the trust variable to an ordered factor
data_2_gender$Trust <- factor(data_2_gender$Trust, ordered = TRUE)
data_2_gender$Identification <- factor(data_2_gender$Identification, ordered = TRUE)
data_2_gender$Commitment <- factor(data_2_gender$Commitment, ordered = TRUE)
data_2_gender$PE <- factor(data_2_gender$PE, ordered = TRUE)
data_2_gender$Intent <- factor(data_2_gender$Intent, ordered = TRUE)

# Fit the proportional odds model with gender and treatment condition as predictors
model_trust <- polr(Trust ~ Q13 + treatmentcondition, data = data_2_gender)
model_trust <- coef(summary(model_trust))
p <- pnorm(abs(model_trust[, "t value"]), lower.tail = FALSE) * 2 #combine the tables
(model_trust <- cbind(model_trust, "p value" = p ))

model_identification <- polr(Identification ~ Q13 + treatmentcondition, data = data_2_gender)
model_identification <- coef(summary(model_identification))
p <- pnorm(abs(model_identification[, "t value"]), lower.tail = FALSE) * 2
(model_identification <- cbind(model_identification, "p value" = p ))

model_commitment <- polr(Commitment ~ Q13 + treatmentcondition, data = data_2_gender)
model_commitment <- coef(summary(model_commitment))
p <- pnorm(abs(model_commitment[, "t value"]), lower.tail = FALSE) * 2
(model_commitment <- cbind(model_commitment, "p value" = p ))

model_PE <- polr(PE ~ Q13 + treatmentcondition, data = data_2_gender)
model_PE <- coef(summary(model_PE))
p <- pnorm(abs(model_PE[, "t value"]), lower.tail = FALSE) * 2
(model_PE <- cbind(model_PE, "p value" = p ))

model_intent <- polr(Intent ~ Q13 + treatmentcondition, data = data_2_gender)
model_intent <- coef(summary(model_intent))
p <- pnorm(abs(model_intent[, "t value"]), lower.tail = FALSE) * 2
(model_intent <- cbind(model_intent, "p value" = p ))

# Run Wilcoxon rank sum test while controlling for Treatment
wilcox.test(Trust ~ Q13 | treatmentcondition, data = data_2_gender)

# Conduct the Mann-Whitney U test for Gender on variables 
wilcox.test(Identification ~ Q13, data = data_2_gender)
wilcox.test(Commitment ~ Q13, data = data_2_gender)
wilcox.test(Intent ~ Q13, data = data_2_gender)
wilcox.test(PE ~ Q13, data = data_2_gender)

library(ggplot2)
#boxplot
ggplot(data, aes(x = Intent, y = Q13)) +
  geom_boxplot() +
  facet_wrap(~Q13, ncol = 1) +
  ylab("Q13") +
  xlab("Gender") +
  ggtitle("Boxplot of Q13 by Gender")

#linear model for age (in years)
data$Trust <- factor(data$Trust)

polr(Trust ~ Q14, data = data)
lmidentification <- lm(Identification ~ Q14, data = data)
lmcommitment <- lm(Commitment ~ Q14, data = data)
lmintent <- lm(Intent ~ Q14, data = data)
lmpe <- lm(PE ~ Q14, data = data)

lapply(list(lmtrust, lmidentification, lmcommitment, lmintent, lmpe), summary)
summary(lmtrust)

#One way Anova for income group 
aov(Intent ~ Q15, data = data)
aov(Trust ~ Q15, data = data)
aov(Identification ~ Q15, data = data)
aov(Commitment ~ Q15, data = data)
aov(PE ~ Q15, data = data)

#One way Anova for education level  
aov(Intent ~ Q16, data = data)
aov(Trust ~ Q16, data = data)
aov(Identification ~ Q16, data = data)
aov(Commitment ~ Q16, data = data)
aov(PE ~ Q16, data = data)




