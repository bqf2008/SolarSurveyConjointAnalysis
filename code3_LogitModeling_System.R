# clear workspace
rm(list=ls())

# read in conjoint data
choiceM <- read.csv("choiceM_System.csv",stringsAsFactors=FALSE)

# logit regression 
library(mlogit)

# prepare data
conjointM <- mlogit.data(choiceM, choice= "Choice", shape = "long", id.var = "pid", alt.var = "Concept")

#Att1 - Panel Efficiency
#Att2 - Panel Visibility
#Att3 - Inverter Type
#Att4 - Failures in 5 years
#Att5 - Equivalent area of forest
#Att6 - Savings

# Multinomial Logit Model with mlogit package
conjointM.logit <- mlogit(Choice ~ None + Att1_1 + Att1_2 + Att1_3 + Att1_4
                          + Att2_1  
                          + Att3_1 + Att3_2 
                          + Att4_1 + Att4_2 + Att4_3 
                          + Att5_1 + Att5_2  
                          + Att6_1 + Att6_2 + Att6_3 + Att6_4 | 0 ,
                          data = conjointM)
summary(conjointM.logit)


# write coefficients to .JSON file
library(jsonlite)

Coef_S <- conjointM.logit$coefficients

scheme <- fromJSON(
'{
"EParamTypes::HODesignDecisionCO2": [],
"EParamTypes::HODesignDecisionFailures": [],
"EParamTypes::HODesignDecisionEstimatedNetSavings": [],
"EParamTypes::HODesignDecisionInverterType": [],
"EParamTypes::HODesignDecisionUtilityNone": [],
"EParamTypes::HODesignDecisionPanelVisibility": [],
"EParamTypes::HODesignDecisionPanelEfficiency": []
}')

scheme[1]$`EParamTypes::HODesignDecisionCO2` <- 
  c(Coef_S[12], Coef_S[13], (0-Coef_S[12]-Coef_S[13]))
scheme[2]$`EParamTypes::HODesignDecisionFailures` <- 
  c(Coef_S[9], Coef_S[10], Coef_S[11], (0-Coef_S[9]-Coef_S[10]-Coef_S[11]))
scheme[3]$`EParamTypes::HODesignDecisionEstimatedNetSavings` <-
  c(Coef_S[14], Coef_S[15], Coef_S[16], Coef_S[17], (0 - Coef_S[14] - Coef_S[15] - Coef_S[16] - Coef_S[17]) )
scheme[4]$`EParamTypes::HODesignDecisionInverterType` <-
  c(Coef_S[7], Coef_S[8], (0-Coef_S[7]-Coef_S[8]))
scheme[5]$`EParamTypes::HODesignDecisionUtilityNone` <-
  Coef_S[1]
scheme[6]$`EParamTypes::HODesignDecisionPanelVisibility` <-
  c(Coef_S[6], -Coef_S[6])
scheme[7]$`EParamTypes::HODesignDecisionPanelEfficiency` <-
  c(Coef_S[2], Coef_S[3], Coef_S[4], Coef_S[5], (0 - Coef_S[2] - Coef_S[3] - Coef_S[4] - Coef_S[5]) )

spline_points <- fromJSON(
'{
"EParamTypes::HODesignDecisionEstimatedNetSavings": [
0.1,
0.25,
0.4,
0.55,
0.7
],
"EParamTypes::HODesignDecisionPanelEfficiency": [
0.155,
0.18,
0.205,
0.23,
0.25
],
"EParamTypes::HODesignDecisionCO2": [
3.0,
6.0,
9.0
],
"EParamTypes::HODesignDecisionFailures": [
0.0,
1.0,
2.0,
3.0
]}')
  
type1 <- list(scheme = scheme, frequency = as.integer(1), spline_points = spline_points)
json <- list(type1 = type1)

write(toJSON(json, pretty = TRUE), "ho-designdecisions.json")








# Multinomial Logit Model with mlogit package, with some continuous variables
conjointMc.logit <- mlogit(Choice ~ None + Att1c
                           + Att2_1   
                           + Att3_1 + Att3_2 
                           + Att4c
                           + Att5c
                           + Att6c | 0 ,
                           data = conjointM)
summary(conjointMc.logit)




# Mixed Logit Model with mlogit package
conjointM1.mixl <- mlogit(Choice ~ None + Att1_1 + Att1_2
                        + Att2_1  
                        + Att3_1 + Att3_2 
                        + Att4_1 + Att4_2 + Att4_3 
                        + Att5_1 + Att5_2  
                        + Att6_1 + Att6_2 + Att6_3 + Att6_4 | 0 ,
                        data = conjointM,
                        R = 50, 
                        rpar = c(None = "n",
                                 Att1_1 = "n", Att1_2 = "n", 
                                 Att2_1 = "n", 
                                 Att3_1 = "n", Att3_2 = "n",  
                                 Att4_1 = "n", Att4_2 = "n", Att4_3 = "n", 
                                 Att5_1 = "n", Att5_2 = "n", 
                                 Att6_1 = "n", Att6_2 = "n", Att6_3 = "n", Att6_4 = "n"),
                        panel = TRUE,
                        correlation = FALSE)

summary(conjointM1.mixl)

# Mixed Logit Model with gmnl package
library(gmnl)
conjointM2.mixl <- gmnl(Choice ~ None + Att1_1 + Att1_2
                        + Att2_1  
                        + Att3_1 + Att3_2 
                        + Att4_1 + Att4_2 + Att4_3 
                        + Att5_1 + Att5_2  
                        + Att6_1 + Att6_2 + Att6_3 + Att6_4 | 0 ,
                        data = conjointM,
                        model = 'mixl', 
                        R = 50, 
                        panel = TRUE,
                        ranp = c(None = "n",
                                 Att1_1 = "n", Att1_2 = "n", 
                                 Att2_1 = "n", 
                                 Att3_1 = "n", Att3_2 = "n",  
                                 Att4_1 = "n", Att4_2 = "n", Att4_3 = "n", 
                                 Att5_1 = "n", Att5_2 = "n", 
                                 Att6_1 = "n", Att6_2 = "n", Att6_3 = "n", Att6_4 = "n"),
                        correlation = FALSE)

summary(conjointM2.mixl)




