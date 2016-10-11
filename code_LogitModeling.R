# clear workspace
rm(list=ls())

# read in conjoint data
choiceM <- read.csv("choiceM_Installer.csv",stringsAsFactors=FALSE)

# logit regression
library(mlogit)
CV <- mlogit.data(choiceM, choice= "Choice", shape = "long", id.var = "pid", alt.var = "Concept")

# Mixed Logit Model
library(gmnl)
CV.mixl <- gmnl(Choice ~ Att1 + Att2 + Att3 + Att4 + Att5 + Att6 | 0 ,
                data = CV,
                model = 'mixl', 
                R = 50, panel = TRUE,
                ranp = c(Att1 = "n", Att2 = "n", Att3 = "n", Att4 = "n", Att5 ="n", Att6 = "n"),
                correlation = FALSE)
