# clear workspace
rm(list=ls())

# read int choice data and conjoint design
design_SystemConjoint <- read.csv("design_SystemConjoint.csv",stringsAsFactors=FALSE)

# reorganize conjoint design table
design_Random <- design_SystemConjoint[4:13503,]

designM <- data.frame(Version = numeric(),
                      Task = numeric(),
                      Concept = numeric(),
                      Att1 = numeric(),
                      Att2 = numeric(),
                      Att3 = numeric(),
                      Att4 = numeric(),
                      Att5 = numeric(),
                      Att6 = numeric(),
                      stringsAsFactors=FALSE)

for (i in 1:300) {
  for (j in 1:15) {
    designM[((i-1)*60+(j-1)*4+1):((i-1)*60+(j-1)*4+3),] <- design_Random[((i-1)*45+(j-1)*3+1):((i-1)*45+(j-1)*3+3),]
    designM[((i-1)*60+(j-1)*4+4),] <- c(i,j,4,0,0,0,0,0,0)
  }
}

design_Fixed <- rbind(design_SystemConjoint[1:3,],0)
design_Fixed$Task <- 16
design_Fixed$Concept[4] = 4

designM[18001:18004,] <- design_Fixed

write.table(designM,"designM_System.csv", sep=",",row.names = FALSE)