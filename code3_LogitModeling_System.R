# clear workspace
rm(list=ls())

# read in conjoint data
choiceM <- read.csv("choiceM_System.csv",stringsAsFactors=FALSE)

# logit regression
library(mlogit)
conjointM <- mlogit.data(choiceM, choice= "Choice", shape = "long", id.var = "pid", alt.var = "Concept")

# Mixed Logit Model
library(gmnl)
# conjointM1.mixl <- gmnl(Choice ~ Att1 + Att2 + Att3 + Att4 + Att5 + Att6 | 0 ,
#                 data = conjointM,
#                 model = 'mixl', 
#                 R = 50, panel = TRUE,
#                 ranp = c(Att1 = "n", Att2 = "n", Att3 = "n", Att4 = "n", Att5 ="n", Att6 = "n"),
#                 correlation = FALSE)

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

# conjointM3.mixl <- gmnl(Choice ~ Att1_1 + Att1_2 + Att1_3
#                         + Att2_1 + Att2_2 + Att2_3 
#                         + Att3_1 + Att3_2 + Att3_3
#                         + Att4_1 + Att4_2 + Att4_3 + Att4_4
#                         + Att5_1 + Att5_2 + Att5_3
#                         + Att6_1 + Att6_2 + Att6_3 + Att6_4 + Att6_5 | 0 ,
#                         data = conjointM,
#                         model = 'mixl', 
#                         R = 50, 
#                         panel = TRUE,
#                         ranp = c(Att1_1 = "n", Att1_2 = "n", Att1_3 = "n",
#                                  Att2_1 = "n", Att2_2 = "n", Att2_3 = "n",
#                                  Att3_1 = "n", Att3_2 = "n", Att3_3 = "n",
#                                  Att4_1 = "n", Att4_2 = "n", Att4_3 = "n", Att4_4 = "n",
#                                  Att5_1 = "n", Att5_2 = "n", Att5_3 = "n",
#                                  Att6_1 = "n", Att6_2 = "n", Att6_3 = "n", Att6_4 = "n", Att6_5 = "n"),
#                         correlation = FALSE)


