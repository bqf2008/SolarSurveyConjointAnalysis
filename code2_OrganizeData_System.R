# clear workspace
rm(list=ls())

# read in choice data and conjoint design
data_SystemConjoint <- read.csv("data_SystemConjoint.csv",stringsAsFactors=FALSE)
designM <- read.csv("designM_System.csv",stringsAsFactors=FALSE)

#### construct conjoint table for modeling

# construct choice table
choice <- c("sys_RespNum","pid",
            "System_Random1","System_Random2",
            "System_Random3","System_Random4",
            "System_Random5","System_Random6",
            "System_Random7","System_Random8",
            "System_Random9","System_Random10",
            "System_Random11","System_Random12",
            "System_Random13","System_Random14",
            "System_Random15","System_Fixed1")

choice_SystemConjoint <- data_SystemConjoint[choice]

for (i in 1:15) {
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
  choiceM[((i-1)*64+1):((i-1)*64+64),1] <- choice_SystemConjoint$pid[i]
  v <- choice_SystemConjoint$sys_RespNum[i] #conjoint version
  choiceM[((i-1)*64+1):((i-1)*64+60),2:10] <- designM[((v-1)*60+1):((v-1)*60+60),]
  choiceM[((i-1)*64+61):((i-1)*64+64),2:10] <- designM[18001:18004,]
  choiceM[((i-1)*64+1):((i-1)*64+64),11] <- t(choice_SystemConjoint[i,19:82])
}

# construct attribute table
choiceM[,"None"] <- 1*(choiceM$Concept == 4)
levels_Att = c(3,2,3,4,3,5)
for (i in 1:6) {
  l = levels_Att[i]
  for (j in 1:(l-1)) {
    name_ColumnAtt <- paste('Att',i,'_',j,sep ="")
    att_Column <- paste("Att",i,sep ="")
    choiceM[,name_ColumnAtt] <- 1 * (choiceM[att_Column] == j) - 1 * (choiceM[att_Column] == l)
  }
}


write.table(choiceM,"choiceM_System.csv", sep=",",row.names = FALSE)



