# clear workspace
rm(list=ls())

# read in conjoint data
choiceM <- read.csv("choiceM_Installer.csv",stringsAsFactors=FALSE)

# logit regression
library(mlogit)
conjointM <- mlogit.data(choiceM, choice= "Choice", shape = "long", id.var = "pid", alt.var = "Concept")

#Att1 - Independent Reviewer Rating
#Att2 - Installer Consumer Interaction
#Att3 - Equipment Technology
#Att4 - Total Project Time
#Att5 - Warranty
#Att6 - Savings

# Multinomial Logit Model with mlogit package
conjointM.logit <- mlogit(Choice ~ None + Att1_1 + Att1_2
                          + Att2_1 + Att2_2  
                          + Att3_1 + Att3_2 
                          + Att4_1 + Att4_2 + Att4_3 
                          + Att5_1 + Att5_2  
                          + Att6_1 + Att6_2 + Att6_3 + Att6_4 | 0 ,
                          data = conjointM)
summary(conjointM.logit)

# write coefficients to .JSON file
library(jsonlite)

Coef_I <- unname(conjointM.logit$coefficients)

scheme <- fromJSON(
'{
"EParamTypes::SEIInteractionType": [],
"EParamTypes::HOSEIDecisionTotalProjectTime": [],
"EParamTypes::HOSEIDecisionEstimatedNetSavings": [],
"EParamTypes::SEIWarranty": [],
"EParamTypes::HOSEIDecisionUtilityNone": [],
"EParamTypes::SEIEquipmentType": [],
"EParamTypes::SEIRating": []
}')

scheme[1]$`EParamTypes::SEIInteractionType` <- 
  c(Coef_I[4], Coef_I[5], (0-Coef_I[4]-Coef_I[5]))
scheme[2]$`EParamTypes::HOSEIDecisionTotalProjectTime` <- 
  c(Coef_I[8], Coef_I[9], Coef_I[10], (0 - Coef_I[8] - Coef_I[9] - Coef_I[10] ))
scheme[3]$`EParamTypes::HOSEIDecisionEstimatedNetSavings` <- 
  c(Coef_I[13], Coef_I[14], Coef_I[15], Coef_I[16], (0 - Coef_I[13] - Coef_I[14] - Coef_I[15] - Coef_I[16]))
scheme[4]$`EParamTypes::SEIWarranty` <- 
  c(Coef_I[11], Coef_I[12], (0 - Coef_I[11] - Coef_I[12]))
scheme[5]$`EParamTypes::HOSEIDecisionUtilityNone` <- 
  Coef_I[1]
scheme[6]$`EParamTypes::SEIEquipmentType` <- 
  c(Coef_I[6], Coef_I[7], (0 - Coef_I[6] - Coef_I[7]))
scheme[7]$`EParamTypes::SEIRating` <-
  c(Coef_I[2], Coef_I[3], (0 - Coef_I[2] - Coef_I[3]))

spline_points <- fromJSON(
'{
  "EParamTypes::HOSEIDecisionEstimatedNetSavings": [
    0.1,
    0.25,
    0.4,
    0.55,
    0.7
    ],
  "EParamTypes::HOSEIDecisionTotalProjectTime": [
    0.5,
    1.0,
    2.0,
    4.0
    ]
}')

type1 <- list(scheme = scheme, frequency = as.integer(1), spline_points = spline_points)
json <- list(type1 = type1)

write(toJSON(json, pretty = TRUE), "ho-installerdecisions.json")






# Multinomial Logit Model with mlogit package, with some continuous variables
conjointMc.logit <- mlogit(Choice ~ None + Att1_1 + Att1_2
                          + Att2_1 + Att2_2  
                          + Att3_1 + Att3_2 
                          + Att4c
                          + Att5_1 + Att5_2  # wrranty
                          + Att6c | 0 ,
                          data = conjointM)
summary(conjointMc.logit)


# Mixed Logit Model with mlogit package
conjointM1.mixl <- mlogit(Choice ~ None + Att1_1 + Att1_2
                          + Att2_1 + Att2_2  
                          + Att3_1 + Att3_2 
                          + Att4_1 + Att4_2 + Att4_3 
                          + Att5_1 + Att5_2  
                          + Att6_1 + Att6_2 + Att6_3 + Att6_4 | 0 ,
                          data = conjointM,
                          R = 50, 
                          rpar = c(None = "n",
                                   Att1_1 = "n", Att1_2 = "n", 
                                   Att2_1 = "n", Att2_2 = "n", 
                                   Att3_1 = "n", Att3_2 = "n",  
                                   Att4_1 = "n", Att4_2 = "n", Att4_3 = "n", 
                                   Att5_1 = "n", Att5_2 = "n", 
                                   Att6_1 = "n", Att6_2 = "n", Att6_3 = "n", Att6_4 = "n"),
                          panel = TRUE,
                          correlation = FALSE)

# Mixed Logit Model with gmnl package
library(gmnl)

conjointM2.mixl <- gmnl(Choice ~ None + Att1_1 + Att1_2
                        + Att2_1 + Att2_2 
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
                                Att2_1 = "n", Att2_2 = "n", 
                                Att3_1 = "n", Att3_2 = "n",  
                                Att4_1 = "n", Att4_2 = "n", Att4_3 = "n", 
                                Att5_1 = "n", Att5_2 = "n", 
                                Att6_1 = "n", Att6_2 = "n", Att6_3 = "n", Att6_4 = "n"),
                       correlation = FALSE)

summary(conjointM2.mixl)

