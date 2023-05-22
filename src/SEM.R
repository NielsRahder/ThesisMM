#Structural equation modeling 
library(lavaan)

####SEM for model 1  
test <- '
Trust_MM =~ Q11_1 + Q11_2 + Q11_3 
Identification_MM =~ Q10_1 + Q10_2 + Q10_3 + Q10_4 + Q10_5 + Q10_6
Commitment_MM =~ Q9_1 + Q9_2 + Q9_4 + Q9_5
Intent_MM =~ Q8_1 + Q8_2 + Q8_3 + Q8_4
Treatment_MM =~ treatmentcondition

#mediators
Trust_MM ~ a1*Treatment_MM
Identification_MM ~ a2*Treatment_MM
Commitment_MM ~ b1*Trust_MM
Commitment_MM ~ b2*Identification_MM 
Intent_MM ~ d1*Commitment_MM
Intent_MM ~ g1* Treatment_MM

treat_trust_commit := a1 * b1
treat_ID_Commit := a2 * b2
Trust_Commit_Intent := b1 * d1
Id_Commit_Intent := b2 * d1
'

test_mod <- sem(test, data = data, estimator = "WLSMV" )
summary(test_mod, fit.measures = TRUE, ci = TRUE, standardized = TRUE)
parameterEstimates(test_mod)

#SEM for model 1b 
model1b <- '
Trust_MM =~ Q11_1 + Q11_2 + Q11_3 
Identification_MM =~ Q10_1 + Q10_2 + Q10_3 + Q10_4 + Q10_5 + Q10_6
Commitment_MM =~ Q9_1 + Q9_2 + Q9_4 + Q9_5
Intent_MM =~ Q8_1 + Q8_2 + Q8_3 + Q8_4

#mediators
Commitment_MM ~ b1*Trust_MM
Commitment_MM ~ b2*Identification_MM 

Intent_MM ~ d1*Commitment_MM
Intent_MM ~ e1*Trust_MM
Intent_MM ~ f1*Identification_MM

Trust_Commit_Intent := b1 * d1
Id_Commit_Intent := b2 * d1
'

model1b <- sem(model1b, data = data, estimator = "WLSMV" )
summary(model1b, fit.measures = TRUE, ci = TRUE, standardized = TRUE)



####SEM for model 2 
PCE_Mod <- "
PCE =~ Q7_1 + Q7_2 + Q7_3 + Q7_4 + Q7_5
INTENT =~ Q8_1 + Q8_2 + Q8_3 + Q8_4
Treatment =~ treatmentcondition

INTENT ~ f2 * PCE 
PCE ~ f1 * Treatment 
INTENT ~ d1 * Treatment

indirect := f1*f2
"
PCE_Mod <- sem(PCE_Mod, data = data, estimator = "WLSMV")
summary(PCE_Mod, fit.measures = TRUE, ci = TRUE, standardized = TRUE)


