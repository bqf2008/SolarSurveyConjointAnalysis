### extact the 16th choice info ###

# clear workspace
#rm(list=ls())

# read in choice data and conjoint design
# data_SystemConjoint <- read.csv("SUSystemConjoint_data_valid.csv",stringsAsFactors=FALSE)
designM <- read.csv("designM_System.csv",stringsAsFactors=FALSE)


#### construct conjoint table for prediction

# construct choice table
choice <- c("sys_CBCVersion_System", "pid", "System_Random15")

choice_SystemConjoint <- data_SystemConjoint[choice]

for (j in 1:4) {
  name_Column <- paste('R15_',j,sep ="")
  choice_Column <- paste("System_Random15")
  choice_SystemConjoint[,name_Column] <- 1 * (choice_SystemConjoint[choice_Column] == j)
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

n_Response = nrow(choice_SystemConjoint)
for (i in 1:n_Response) {
  choiceM[((i-1)*4+1):((i-1)*4+4),1] <- choice_SystemConjoint$pid[i]
  v <- choice_SystemConjoint$sys_CBCVersion_System[i] #conjoint version
  choiceM[((i-1)*4+1):((i-1)*4+4),2:10] <- designM[((v-1)*60+57):((v-1)*60+60),]
  choiceM[((i-1)*4+1):((i-1)*4+4),11] <- t(choice_SystemConjoint[i,4:7])
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

choiceM[,"Att1c"] <- 20.5*(choiceM$Att1 == 0) + 15.5*(choiceM$Att1 == 1) + 18*(choiceM$Att1 == 2) + 20.5*(choiceM$Att1 == 3) + 23*(choiceM$Att1 == 4) + 25.5*(choiceM$Att1 == 5)
# flip the sign of Att4c [lognormal distribution]
choiceM[,"Att4c"] <- - 1.5*(choiceM$Att4 == 0) - 0*(choiceM$Att4 == 1) - 1*(choiceM$Att4 == 2) - 2*(choiceM$Att4 == 3) - 3*(choiceM$Att4 == 4)
choiceM[,"Att5c"] <- 6*(choiceM$Att5 == 0) + 3*(choiceM$Att5 == 1) + 6*(choiceM$Att5 == 2) + 9*(choiceM$Att5 == 3)
choiceM[,"Att6c"] <- 40*(choiceM$Att6 == 0) + 10*(choiceM$Att6 == 1) + 25*(choiceM$Att6 == 2) + 40*(choiceM$Att6 == 3) + 55*(choiceM$Att6 == 4) + 70*(choiceM$Att6 == 5)


#### Making prediction
choiceCompare <- data.frame("Actual" = data_SystemConjoint$System_Random15)
choiceCompare$Predict <- 0


coefficient <- read.csv(paste(getwd(),"/HB_output/HB_mxl_system_C.csv",sep = ""))
coefficient_Att6c <- read.csv(paste(getwd(),"/HB_output/HB_mxl_system_F.csv",sep = ""))
co_Att6c <- mean(coefficient_Att6c[(nrow(coefficient_Att6c)/2+1):nrow(coefficient_Att6c),"Att6c"])
for (i in 1:n_Response) {
  beta <- data.matrix(coefficient[i, c("None", "Att1c", "Att2_1", "Att3_1", "Att3_2", "Att4c", "Att5c")])
  x <- data.matrix( choiceM[((i-1)*4+1):((i-1)*4+4), c("None", "Att1c", "Att2_1", "Att3_1", "Att3_2", "Att4c", "Att5c")])
  x_Att6c <- data.matrix(choiceM[((i-1)*4+1):((i-1)*4+4), c("Att6c")])
  utility <- x %*% t(beta) + co_Att6c * x_Att6c
  choiceCompare[i,"Predict"] <- which(utility==max(utility))
}

choiceCompare$Compare <- 1* (choiceCompare$Actual == choiceCompare$Predict)
accuracy <- sum(choiceCompare$Compare)/n_Response
print(paste("Accuracy = ", accuracy))
