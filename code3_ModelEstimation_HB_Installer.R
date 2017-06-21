# clear workspace
# rm(list=ls())
graphics.off()
library(RSGHB)

# read in conjoint data
choiceM <- read.csv("choiceM_Installer.csv",stringsAsFactors=FALSE)


# Organize choicedata for modeling
option1 <- subset(choiceM, Concept == 1)
option2 <- subset(choiceM, Concept == 2)
option3 <- subset(choiceM, Concept == 3)
option4 <- subset(choiceM, Concept == 4)

choicedata <- data.frame("ID" = option1$pid, "thecount" = option1$Task)
choicedata[,c("o1_None", "o1_Att1c", "o1_Att2_1", "o1_Att2_2", "o1_Att3_1", "o1_Att3_2", 
              "o1_Att4c", "o1_Att5_1", "o1_Att5_2", "o1_Att6c")]  <- 
          option1[,c("None", "Att1c", "Att2_1", "Att2_2", "Att3_1", "Att3_2", 
                     "Att4c", "Att5_1", "Att5_2", "Att6c")]
choicedata[,c("o2_None", "o2_Att1c", "o2_Att2_1", "o2_Att2_2", "o2_Att3_1", "o2_Att3_2", 
              "o2_Att4c", "o2_Att5_1", "o2_Att5_2", "o2_Att6c")]  <- 
          option2[,c("None", "Att1c", "Att2_1", "Att2_2", "Att3_1", "Att3_2", 
                     "Att4c", "Att5_1", "Att5_2", "Att6c")]
choicedata[,c("o3_None", "o3_Att1c", "o3_Att2_1", "o3_Att2_2", "o3_Att3_1", "o3_Att3_2", 
              "o3_Att4c", "o3_Att5_1", "o3_Att5_2", "o3_Att6c")]  <- 
          option3[,c("None", "Att1c", "Att2_1", "Att2_2", "Att3_1", "Att3_2", 
                     "Att4c", "Att5_1", "Att5_2", "Att6c")]
choicedata[,c("o4_None", "o4_Att1c", "o4_Att2_1", "o4_Att2_2", "o4_Att3_1", "o4_Att3_2", 
              "o4_Att4c", "o4_Att5_1", "o4_Att5_2", "o4_Att6c")]  <- 
          option4[,c("None", "Att1c", "Att2_1", "Att2_2", "Att3_1", "Att3_2", 
                     "Att4c", "Att5_1", "Att5_2", "Att6c")]

choice1 <- option1$Choice
choice2 <- option2$Choice
choice3 <- option3$Choice
choice4 <- option4$Choice
choice <- data.frame(choice1, choice2, choice3, choice4)
choicedata[,"Choice"] <- 1*(choice[,1]==1) + 2*(choice[,2]==1) + 3*(choice[,3]==1) + 4*(choice[,4]==1)

o1_None <- choicedata$o1_None
o1_Att1c <- choicedata$o1_Att1c
o1_Att2_1 <- choicedata$o1_Att2_1
o1_Att2_2 <- choicedata$o1_Att2_2
o1_Att3_1 <- choicedata$o1_Att3_1
o1_Att3_2 <- choicedata$o1_Att3_2
o1_Att4c <- choicedata$o1_Att4c
o1_Att5_1 <- choicedata$o1_Att5_1
o1_Att5_2 <- choicedata$o1_Att5_2
o1_Att6c <- choicedata$o1_Att6c

o2_None <- choicedata$o2_None
o2_Att1c <- choicedata$o2_Att1c
o2_Att2_1 <- choicedata$o2_Att2_1
o2_Att2_2 <- choicedata$o2_Att2_2
o2_Att3_1 <- choicedata$o2_Att3_1
o2_Att3_2 <- choicedata$o2_Att3_2
o2_Att4c <- choicedata$o2_Att4c
o2_Att5_1 <- choicedata$o2_Att5_1
o2_Att5_2 <- choicedata$o2_Att5_2
o2_Att6c <- choicedata$o2_Att6c

o3_None <- choicedata$o3_None
o3_Att1c <- choicedata$o3_Att1c
o3_Att2_1 <- choicedata$o3_Att2_1
o3_Att2_2 <- choicedata$o3_Att2_2
o3_Att3_1 <- choicedata$o3_Att3_1
o3_Att3_2 <- choicedata$o3_Att3_2
o3_Att4c <- choicedata$o3_Att4c
o3_Att5_1 <- choicedata$o3_Att5_1
o3_Att5_2 <- choicedata$o3_Att5_2
o3_Att6c <- choicedata$o3_Att6c

o4_None <- choicedata$o4_None
o4_Att1c <- choicedata$o4_Att1c
o4_Att2_1 <- choicedata$o4_Att2_1
o4_Att2_2 <- choicedata$o4_Att2_2
o4_Att3_1 <- choicedata$o4_Att3_1
o4_Att3_2 <- choicedata$o4_Att3_2
o4_Att4c <- choicedata$o4_Att4c
o4_Att5_1 <- choicedata$o4_Att5_1
o4_Att5_2 <- choicedata$o4_Att5_2
o4_Att6c <- choicedata$o4_Att6c



# The model likelihood function
likelihood <- function(fc, b) {
  
  # Assign Beta vectors to named parameters for convenience
  cc <- 1
  ce_None <- b[, cc]; cc <- cc + 1 # none varies over indivdiual
  ce_Att1c <- b[, cc]; cc <- cc + 1
  ce_Att2_1 <- b[, cc]; cc <- cc + 1
  ce_Att2_2 <- b[, cc]; cc <- cc + 1
  ce_Att3_1 <- b[, cc]; cc <- cc + 1
  ce_Att3_2 <- b[, cc]; cc <- cc + 1
  ce_Att4c <- b[, cc]; cc <- cc + 1
  ce_Att5_1 <- b[, cc]; cc <- cc + 1
  ce_Att5_2 <- b[, cc]; cc <- cc + 1
  
  ce_Att6c <- fc[1] # fixed savings for each individual
  # ce_Att6c <- b[, cc]
  
  # Discrete choice utility in WTP-space
  v1 <- ce_Att1c * o1_Att1c + ce_Att2_1 * o1_Att2_1 + ce_Att2_2 * o1_Att2_2 + ce_Att3_1 * o1_Att3_1 + ce_Att3_2 * o1_Att3_2 + ce_Att4c * o1_Att4c + ce_Att5_1 * o1_Att5_1 + ce_Att5_2 * o1_Att5_2 + ce_Att6c * o1_Att6c  
  v2 <- ce_Att1c * o2_Att1c + ce_Att2_1 * o2_Att2_1 + ce_Att2_2 * o2_Att2_2 + ce_Att3_1 * o2_Att3_1 + ce_Att3_2 * o2_Att3_2 + ce_Att4c * o2_Att4c + ce_Att5_1 * o2_Att5_1 + ce_Att5_2 * o2_Att5_2 + ce_Att6c * o2_Att6c  
  v3 <- ce_Att1c * o3_Att1c + ce_Att2_1 * o3_Att2_1 + ce_Att2_2 * o3_Att2_2 + ce_Att3_1 * o3_Att3_1 + ce_Att3_2 * o3_Att3_2 + ce_Att4c * o3_Att4c + ce_Att5_1 * o3_Att5_1 + ce_Att5_2 * o3_Att5_2 + ce_Att6c * o3_Att6c  
  v4 <- ce_None * o4_None + ce_Att1c * o4_Att1c + ce_Att2_1 * o4_Att2_1 + ce_Att2_2 * o4_Att2_2 + ce_Att3_1 * o4_Att3_1 + ce_Att3_2 * o4_Att3_2 + ce_Att4c * o4_Att4c + ce_Att5_1 * o4_Att5_1 + ce_Att5_2 * o4_Att5_2 + ce_Att6c * o4_Att6c  
    
  # Return the probability of choice
  p <- (exp(v1)*choice1 + exp(v2)*choice2 + exp(v3)*choice3 + exp(v4)*choice4) / (exp(v1) + exp(v2) + exp(v3) + exp(v4))
  return(p)
}



# Estimation controls/settings
control <- list(
  modelname = "HB_mxl_installer",
  gDIST = c(1, 2, 1, 1, 1, 1, 2, 1, 1),
  gVarNamesNormal = c("None", "Att1c", "Att2_1", "Att2_2", "Att3_1", "Att3_2", 
                      "Att4c", "Att5_1", "Att5_2"),
  gVarNamesFixed = c("Att6c"),
  gNCREP = 20, # default to 100000
  gNEREP = 20, # default to 100000
  gINFOSKIP = 250, # default to 250
  gNSKIP = 1, # default to 1
  nodiagnostics = TRUE,
  gSeed = 1978,
  gStoreDraws = FALSE # TRUE to store individual level parameters
)



# Estimate the model
model <- doHB(likelihood, choicedata, control)

#plot(model)
#plot(model, type = "A")

# Clean folder "HB_output"
fn_A <- paste(getwd(),"/HB_output/HB_mxl_installer_A.csv",sep = "")
fn_B <- paste(getwd(),"/HB_output/HB_mxl_installer_B.csv",sep = "")
fn_Bsd <- paste(getwd(),"/HB_output/HB_mxl_installer_Bsd.csv",sep = "")
fn_C <- paste(getwd(),"/HB_output/HB_mxl_installer_C.csv",sep = "")
fn_Csd <- paste(getwd(),"/HB_output/HB_mxl_installer_Csd.csv",sep = "")
fn_D <- paste(getwd(),"/HB_output/HB_mxl_installer_D.csv",sep = "")
fn_F <- paste(getwd(),"/HB_output/HB_mxl_installer_F.csv",sep = "")
fn_pvMatrix <- paste(getwd(),"/HB_output/HB_mxl_installer_pvMatrix.csv",sep = "")
fn_log <- paste(getwd(),"/HB_output/HB_mxl_installer.log",sep = "")

if (file.exists(fn_A)) file.remove(fn_A)
if (file.exists(fn_B)) file.remove(fn_B)
if (file.exists(fn_Bsd)) file.remove(fn_Bsd)
if (file.exists(fn_C)) file.remove(fn_C)
if (file.exists(fn_Csd)) file.remove(fn_Csd)
if (file.exists(fn_D)) file.remove(fn_D)
if (file.exists(fn_F)) file.remove(fn_F)
if (file.exists(fn_pvMatrix)) file.remove(fn_pvMatrix)
if (file.exists(fn_log)) file.remove(fn_log)

writeModel(model, writeDraws = FALSE, path = paste(getwd(),"/HB_output",sep = ""))
