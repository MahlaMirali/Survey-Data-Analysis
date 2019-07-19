library(tidyverse)
library(lubridate)
library(Hmisc)
library(ggplot2)
setwd("C:\\Users\\Salman\\Documents\\R")
elect <- read.csv("Survey_database.csv", skip = 1)


## BAsed on Q2
P4 <- tibble(elect_new$Q2, elect_new$Q4)
print(P4, n= 129)
wp4 = table(P4, exclude = NULL)
wp4
tp4 = as.data.frame(wp4)
tp4
barplot(wp4, ylim = c(0, 60), main = "Q4\n Accessibility of the Information\n Based on Sight Loss",
        xlab = "Accessibility of the Information", ylab = "Frequency",
        legend.text = c("Mild", "Significant", "NA"),
        args.legend = list(x = "topright"))
tbl4 = table(elect_new$Q2, elect_new$Q4) 
tbl4 
chisq.test(tbl4)
## Warning message:
## In chisq.test(tbl4) : Chi-squared approximation may be incorrect
ctbl4 = cbind(tbl4[,"All"], tbl4[,"None"] + tbl4[,"Not_Aware"]+ tbl4[,"Not_Receive"], tbl4[,"Some"]) 
ctbl4
chisq.test(ctbl4)
## Significant Differences
labels <- c("All", "None", "Some")
mp <- barplot(ctbl4, axes = FALSE, axisnames = FALSE, ylim = c(0, 50), main = "Q4\n Accessibility of the Information\n Based on Sight Loss",
              xlab = "Accessibility of the Information", ylab = "Frequency",
              legend.text = c("Mild", "Significant"),
              args.legend = list(x = "topright"))
text(mp, par("usr")[3], labels = labels, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
axis(2)

P7 <- separate(LVII.2, Qnew, c("Q7", "Q2"))
P7
w7 = table(P7$Q7, exclude = NA)
w7
t7 = as.data.frame(w7)
t7
print(t7)
p7 <- tibble(P7$Q2, P7$Q7)
p7
wp7 <- table(p7, useNA = "no")
wp7
barplot(wp7, ylim = c(0, 100), main = "Q7\n Assistive Devices\n Based on Sight Loss",
        xlab = "Methods", ylab = "Frequency",
        legend.text = c("Mild", "Significant", "NA"),
        args.legend = list(x = "topright"))
QP7_NA <- which(str_detect(P7$Q7, "NA")) 
P7$Q7[QP7_NA] <- NA
QP2_NA <- which(str_detect(P7$Q2, "NA")) 
P7$Q2[QP2_NA] <- NA
unique(P7$Q7)
tbl7 = table(P7$Q2, P7$Q7, useNA = "no")
tbl7
chisq.test(tbl7)
##Warning message:
##In chisq.test(tbl7) : Chi-squared approximation may be incorrect
ctbl7 = cbind(tbl7[,"App"]+ tbl7[,"CCTV"]+ tbl7[,"Kurzweil"]+ tbl7[,"Ocutech"] + tbl7[,"OrCam"]+ tbl7[,"PhoneTablet"]+
                tbl7[,"VMagnifier"] , tbl7[,"Braille"]+tbl7[,"Glasses"]+tbl7[,"Magnifier"], tbl7[,"Person"], tbl7[,"NotRequire"]) 
ctbl7
chisq.test(ctbl7)
## Significant differences
labels <- c("Digital", "Non Digital", "Person", "Not Require")
mp <- barplot(ctbl7, axes = FALSE, axisnames = FALSE, ylim = c(0, 40), main = "Q7\n Assistive Devices\n Based on Sight Loss",
              xlab = "Methods", ylab = "Frequency",
              legend.text = c("Mild", "Significant"),
              args.legend = list(x = "topright"))
text(mp, par("usr")[3], labels = labels, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
axis(2)