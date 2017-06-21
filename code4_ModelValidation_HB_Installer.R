### extact the 16th choice info ###

### clear workspace
#rm(list=ls())

### read in choice data and conjoint design
# data_InstallerConjoint <- read.csv("SUInstallerConjoint_data_valid.csv",stringsAsFactors=FALSE)
# designM <- read.csv("designM_Installer.csv",stringsAsFactors=FALSE)


#### construct conjoint table for prediction

# construct choice table
choice <- c("sys_CBCVersion_InstallerCBC", "pid", "InstallerCBC_Random15")

choice_InstallerConjoint <- data_InstallerConjoint[choice]

for (j in 1:4) {
  name_Column <- paste('R15_',j,sep ="")
  choice_Column <- paste("InstallerCBC_Random15")
  choice_InstallerConjoint[,name_Column] <- 1 * (choice_InstallerConjoint[choice_Column] == j)
}

choiceM <- data.frame(pid = character(),
                      Version = numeric(),
                      Task = numeric(),
                      Concept = numeric(),
                      Att1 = numeric(),
                      Att2 = numeric(),
                      Att3 = numeric(),
                      Att4 = numeric(),
                      Att5 = numeric(),
                      Att6 = numeric(),
                      Choice = numeric(),
                      stringsAsFactors = FALSE)

n_Response = nrow(choice_InstallerConjoint)
for (i in 1:n_Response) {
  choiceM[((i-1)*4+1):((i-1)*4+4),1] <- choice_InstallerConjoint$pid[i]
  v <- choice_InstallerConjoint$sys_CBCVersion_InstallerCBC[i] #conjoint version
  choiceM[((i-1)*4+1):((i-1)*4+4),2:10] <- designM[((v-1)*60+57):((v-1)*60+60),]
  choiceM[((i-1)*4+1):((i-1)*4+4),11] <- t(choice_InstallerConjoint[i,4:7])
}


# construct attribute table
choiceM[,"None"] <- 1*(choiceM$Concept == 4)
levels_Att = c(3,3,3,4,3,5)
for (i in 1:6) {
  l = levels_Att[i]
  for (j in 1:(l-1)) {
    name_ColumnAtt <- paste('Att',i,'_',j,sep ="")
    att_Column <- paste("Att",i,sep ="")
    choiceM[,name_ColumnAtt] <- 1 * (choiceM[att_Column] == j) - 1 * (choiceM[att_Column] == l)
  }
}

choiceM[,"Att1c"] <- 4*(choiceM$Att1 == 0) + 3*(choiceM$Att1 == 1) + 4*(choiceM$Att1 == 2) + 5*(choiceM$Att1 == 3)
# flip the sign of Att4c [lognormal distribution]
choiceM[,"Att4c"] <- -2^0.5*(choiceM$Att4 == 0) - 0.5*(choiceM$Att4 == 1) - 1*(choiceM$Att4 == 2) - 2*(choiceM$Att4 == 3) - 4*(choiceM$Att4 == 4)
choiceM[,"Att5c"] <- 15*(choiceM$Att5 == 0) + 5*(choiceM$Att5 == 1) + 15*(choiceM$Att5 == 2) + 25*(choiceM$Att5 == 3)
choiceM[,"Att6c"] <- 40*(choiceM$Att6 == 0) + 10*(choiceM$Att6 == 1) + 25*(choiceM$Att6 == 2) + 40*(choiceM$Att6 == 3) + 55*(choiceM$Att6 == 4) + 70*(choiceM$Att6 == 5)


#### Making prediction
choiceCompare <- data.frame("Actual" = data_InstallerConjoint$InstallerCBC_Random15)
choiceCompare$Predict <- 0

coefficient <- read.csv(paste(getwd(),"/HB_output/HB_mxl_installer_C.csv",sep = ""))
coefficient_Att6c <- read.csv(paste(getwd(),"/HB_output/HB_mxl_installer_F.csv",sep = ""))
co_Att6c <- mean(coefficient_Att6c[(nrow(coefficient_Att6c)/2+1):nrow(coefficient_Att6c),"Att6c"])
for (i in 1:n_Response) {
  beta <- data.matrix(coefficient[i, c("None", "Att1c", "Att2_1", "Att2_2", "Att3_1", 
                                       "Att3_2", "Att4c", "Att5_1", "Att5_2")])
  x <- data.matrix( choiceM[((i-1)*4+1):((i-1)*4+4), c("None", "Att1c", "Att2_1", "Att2_2", "Att3_1", 
                                                       "Att3_2", "Att4c", "Att5_1", "Att5_2")])
  x_Att6c <- data.matrix(choiceM[((i-1)*4+1):((i-1)*4+4), c("Att6c")])
  utility <- x %*% t(beta) + co_Att6c * x_Att6c
  choiceCompare[i,"Predict"] <- which(utility==max(utility))
}

choiceCompare$Compare <- 1* (choiceCompare$Actual == choiceCompare$Predict)
accuracy <- sum(choiceCompare$Compare)/n_Response
print(paste("Accuracy = ", accuracy))
