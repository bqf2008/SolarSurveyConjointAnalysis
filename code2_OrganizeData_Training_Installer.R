### extact the first 15 choices to construct choiceMatrix, as training set ###

# clear workspace
rm(list=ls())

# read in choice data and conjoint design
data_InstallerConjoint <- read.csv("SUInstallerConjoint_data_valid.csv",stringsAsFactors=FALSE)
designM <- read.csv("designM_Installer.csv",stringsAsFactors=FALSE)

#### construct conjoint table for modeling

# construct choice table
choice <- c("sys_CBCVersion_InstallerCBC","pid",
  "InstallerCBC_Random1","InstallerCBC_Random2",
  "InstallerCBC_Random3","InstallerCBC_Random4",
  "InstallerCBC_Random5","InstallerCBC_Random6",
  "InstallerCBC_Random7","InstallerCBC_Random8",
  "InstallerCBC_Random9","InstallerCBC_Random10",
  "InstallerCBC_Random11","InstallerCBC_Random12",
  "InstallerCBC_Random13","InstallerCBC_Random14",
  "InstallerCBC_Fixed1")

choice_InstallerConjoint <- data_InstallerConjoint[choice]

for (i in 1:14) {
  for (j in 1:4) {
    name_Column <- paste('R',i,'_',j,sep ="")
    choice_Column <- paste("InstallerCBC_Random",i,sep ="")
    choice_InstallerConjoint[,name_Column] <- 1 * (choice_InstallerConjoint[choice_Column] == j)
  }
}

choice_InstallerConjoint$F1_1 <- 1 * (choice_InstallerConjoint$InstallerCBC_Fixed1 == 1)
choice_InstallerConjoint$F1_2 <- 1 * (choice_InstallerConjoint$InstallerCBC_Fixed1 == 2)
choice_InstallerConjoint$F1_3 <- 1 * (choice_InstallerConjoint$InstallerCBC_Fixed1 == 3)
choice_InstallerConjoint$F1_4 <- 1 * (choice_InstallerConjoint$InstallerCBC_Fixed1 == 4)


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
  choiceM[((i-1)*60+1):((i-1)*60+60),1] <- choice_InstallerConjoint$pid[i]
  v <- choice_InstallerConjoint$sys_CBCVersion_InstallerCBC[i] #conjoint version
  choiceM[((i-1)*60+1):((i-1)*60+56),2:10] <- designM[((v-1)*60+1):((v-1)*60+56),]
  choiceM[((i-1)*60+57):((i-1)*60+60),2:10] <- designM[18001:18004,]
  choiceM[((i-1)*60+57):((i-1)*60+60),3] <- 15
  choiceM[((i-1)*60+1):((i-1)*60+60),11] <- t(choice_InstallerConjoint[i,18:77])
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

write.table(choiceM,"choiceM_Installer.csv", sep=",",row.names = FALSE)



