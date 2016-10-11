# clear workspace
rm(list=ls())

# read in choice data and conjoint design
data_InstallerConjoint <- read.csv("data_InstallerConjoint.csv",stringsAsFactors=FALSE)
designM <- read.csv("designM_Installer.csv",stringsAsFactors=FALSE)

#### construct conjoint table for modeling

# construct choice table
choice <- c("sys_RespNum","pid",
  "InstallerCBC_Random1","InstallerCBC_Random2",
  "InstallerCBC_Random3","InstallerCBC_Random4",
  "InstallerCBC_Random5","InstallerCBC_Random6",
  "InstallerCBC_Random7","InstallerCBC_Random8",
  "InstallerCBC_Random9","InstallerCBC_Random10",
  "InstallerCBC_Random11","InstallerCBC_Random12",
  "InstallerCBC_Random13","InstallerCBC_Random14",
  "InstallerCBC_Random15","InstallerCBC_Fixed1")

choice_InstallerConjoint <- data_InstallerConjoint[choice]

for (i in 1:15) {
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
  choiceM[((i-1)*64+1):((i-1)*64+64),1] <- choice_InstallerConjoint$pid[i]
  v <- choice_InstallerConjoint$sys_RespNum[i] #conjoint version
  choiceM[((i-1)*64+1):((i-1)*64+60),2:10] <- designM[((v-1)*60+1):((v-1)*60+60),]
  choiceM[((i-1)*64+61):((i-1)*64+64),2:10] <- designM[18001:18004,]
  choiceM[((i-1)*64+1):((i-1)*64+64),11] <- t(choice_InstallerConjoint[i,19:82])
}

# construct attribute table
levels_Att = c(3,3,3,4,3,5)
for (i in 1:6) {
  l = levels_Att[i]
  for (j in 1:l) {
    name_ColumnAtt <- paste('Att',i,'_',j,sep ="")
    att_Column <- paste("Att",i,sep ="")
    choiceM[,name_ColumnAtt] <- 1 * (choiceM[att_Column] == j)
  }
}

write.table(choiceM,"choiceM_Installer.csv", sep=",",row.names = FALSE)



