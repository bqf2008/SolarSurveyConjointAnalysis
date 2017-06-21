### extact the first 15 choices to construct choiceMatrix, as training set ###

# clear workspace
rm(list=ls())

# read in choice data and conjoint design
data_SystemConjoint <- read.csv("SUSystemConjoint_data_valid.csv",stringsAsFactors=FALSE)
designM <- read.csv("designM_System.csv",stringsAsFactors=FALSE)

# sort choice data by pid (ID)
data_SystemConjoint <- data_SystemConjoint[order(data_SystemConjoint$pid),]

#### construct conjoint table for modeling

# construct choice table
choice <- c("sys_CBCVersion_System","pid",
            "System_Random1","System_Random2",
            "System_Random3","System_Random4",
            "System_Random5","System_Random6",
            "System_Random7","System_Random8",
            "System_Random9","System_Random10",
            "System_Random11","System_Random12",
            "System_Random13","System_Random14",
            "System_Fixed1")

choice_SystemConjoint <- data_SystemConjoint[choice]

for (i in 1:14) {
  for (j in 1:4) {
    name_Column <- paste('R',i,'_',j,sep ="")
    choice_Column <- paste("System_Random",i,sep ="")
    choice_SystemConjoint[,name_Column] <- 1 * (choice_SystemConjoint[choice_Column] == j)
  }
}

choice_SystemConjoint$F1_1 <- 1 * (choice_SystemConjoint$System_Fixed1 == 1)
choice_SystemConjoint$F1_2 <- 1 * (choice_SystemConjoint$System_Fixed1 == 2)
choice_SystemConjoint$F1_3 <- 1 * (choice_SystemConjoint$System_Fixed1 == 3)
choice_SystemConjoint$F1_4 <- 1 * (choice_SystemConjoint$System_Fixed1 == 4)


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
  choiceM[((i-1)*60+1):((i-1)*60+60),1] <- choice_SystemConjoint$pid[i]
  v <- choice_SystemConjoint$sys_CBCVersion_System[i] #conjoint version
  choiceM[((i-1)*60+1):((i-1)*60+56),2:10] <- designM[((v-1)*60+1):((v-1)*60+56),]
  choiceM[((i-1)*60+57):((i-1)*60+60),2:10] <- designM[18001:18004,]
  choiceM[((i-1)*60+57):((i-1)*60+60),3] <- 15
  choiceM[((i-1)*60+1):((i-1)*60+60),11] <- t(choice_SystemConjoint[i,18:77])
}

# construct attribute table
choiceM[,"None"] <- 1*(choiceM$Concept == 4)
levels_Att = c(5,2,3,4,3,5)
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

write.table(choiceM,"choiceM_System.csv", sep=",",row.names = FALSE)


