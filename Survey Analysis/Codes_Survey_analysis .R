library(tidyverse)
library(lubridate)
library(Hmisc)
library(ggplot2)
setwd("C:\\Users\\Salman\\Documents\\R")
elect <- read.csv("Survey_database.csv", skip = 1)
glimpse(elect)
colnames(elect)[1]<- 'Respondent'
colnames(elect)[2]<- 'Collector'
colnames(elect)[3]<- 'StartDate'
elect$StartDate <- mdy_hm(elect$StartDate)

colnames(elect)[4]<- 'EndDate'
elect$EndDate <- mdy_hm(elect$EndDate)

elect1 <- unite (elect, Q1.1, I.consent, I.do.not.consent)
elect1$Q1.1 <- str_remove(elect1$Q1.1,"_")
Q1.1.freq <- table(elect1$Q1.1, exclude = NULL) 
print(Q1.1.freq)
w1 = table(elect1$Q1.1, exclude = NULL)
w1
t1 = as.data.frame(w1)
t1
colnames(t1) <- c("Q1", "Freq")
print(t1)

elect_new <- unite(elect, Q1, I.consent, I.do.not.consent)
elect_new$Q1 <- str_remove(elect_new$Q1,"_")
elect_new$Q1 <- as.factor(elect_new$Q1)
nonconsent <- which(elect_new$Q1 == 'I do not consent')
elect_new <- elect_new [-nonconsent,]

# Grouping mild and moderate; Grouping Total and significant
elect_new <- unite(elect_new, Q2, Mild..I.have.most.of.my.vision, Moderate..I.have.some.of.my.vision,
                   Significant..I.can.see.very.little, Total..I.am.completely.blind,
                   It.varies.depending.on.the.environment.I.m.in)
unique(elect_new$Q2)
elect_new$Q2 <- str_replace(elect_new$Q2, "____It varies depending on the environment I'm in", "____")
elect_new$Q2 <- str_replace(elect_new$Q2, "_Moderate, I have some of my vision___", "Mild")
elect_new$Q2 <- str_replace(elect_new$Q2, "___Total, I am completely blind_", "Significant")
elect_new$Q2 <- str_replace(elect_new$Q2, "Mild, I have most of my vision____", "Mild")
elect_new$Q2 <- str_replace(elect_new$Q2, "__Significant, I can see very little__", "Significant")
Q2_NA <- which(str_detect(elect_new$Q2, "____")) 
elect_new$Q2[Q2_NA] <- NA
elect_new$Q2 <- as.factor(elect_new$Q2) 
w2 = table(elect_new$Q2, exclude = NULL)
w2
t2 = as.data.frame(w2)
t2
colnames(t2) <- c("Q2", "Freq")
print(t2)

colnames(elect_new)[12]<- "Friend.Family"
elect_new$Friend.Family <- str_replace(elect_new$Friend.Family, "Friend or family member", "Friend")
colnames(elect_new)[13]<- "El.ON.Ad"
elect_new$El.ON.Ad <- str_replace(elect_new$El.ON.Ad, "Elections Ontario advertisement", "Ad")
colnames(elect_new)[14]<- "El.ON.Brochure"
elect_new$El.ON.Brochure <- str_replace(elect_new$El.ON.Brochure, "Elections Ontario brochure", "Brochure")
colnames(elect_new)[15]<- "El.ON.Info"
elect_new$El.ON.Info <- str_replace(elect_new$El.ON.Info, "Elections Ontario information at a community centre, library, or shelter", "Info")
colnames(elect_new)[16]<- "El.ON.Event"
elect_new$El.ON.Event <- str_replace(elect_new$El.ON.Event, "Elections Ontario public event, such as on campus or in the community", "Event")
colnames(elect_new)[17]<- "El.ON.Voter.Card"
elect_new$El.ON.Voter.Card <- str_replace(elect_new$El.ON.Voter.Card, "Elections OntarioÂ Voter Information Card", "VoterCard")
# Or the set of these!
elect_new$El.ON.Voter.Card <- str_replace(elect_new$El.ON.Voter.Card, "Â", "")
elect_new$El.ON.Voter.Card <- str_replace(elect_new$El.ON.Voter.Card, "Elections", "")
elect_new$El.ON.Voter.Card <- str_replace(elect_new$El.ON.Voter.Card, " Ontario", "")
elect_new$El.ON.Voter.Card <- str_replace(elect_new$El.ON.Voter.Card, " Information ", "")
elect_new$El.ON.Voter.Card <- str_trim(elect_new$El.ON.Voter.Card)
colnames(elect_new)[18]<- "El.ON.Web"
elect_new$El.ON.Web <- str_replace(elect_new$El.ON.Web, "Elections Ontarioâ???Ts website", "Website")
colnames(elect_new)[19]<- "News"
elect_new$News <- str_replace(elect_new$News, "News, such as TV, Newspaper, Online, Radio", "News")
colnames(elect_new)[20]<- "Candidate"
elect_new$Candidate <- str_replace(elect_new$Candidate, "Political candidate", "Candidate")
colnames(elect_new)[21]<- "Social.Media"
elect_new$Social.Media<- str_replace(elect_new$Social.Media, "Social media, such as Facebook or Twitter", "SMedia")
colnames(elect_new)[22]<- "Other"
elect_new$Other<- str_replace(elect_new$Other, "CNIBk", "CNIB")
elect_new$Other<- str_replace(elect_new$Other, "CNIBÃ±", "CNIB")
elect_new$Other<- str_replace(elect_new$Other, "Recieved Card in Mail", "VoterCard")
elect_new$Other<- str_replace(elect_new$Other, "Retirement home", "Friend")
elect_new$Other<- str_replace(elect_new$Other, "CNIB-CCB campaign to update ADP", "CNIB")
elect_new$Other<- str_replace(elect_new$Other, "Through my friends in the blind community", "Friend")
elect_new$Other<- str_replace(elect_new$Other, "Emails from the CNIB, the CCB and Balance for Blind Adults.", "CNIB")
elect_new$Other<- str_replace(elect_new$Other, "Member of my local Liberal Association", "Friend")
elect_new$Other<- str_replace(elect_new$Other, "BEING  ALERT TO CURRENT EVENTS & NEWS!", "News")
elect_new$Other<- str_replace(elect_new$Other, "every where", "News")
WIII <- elect_new[, c(12:22)]
glimpse(WIII)
unique(WIII$Other)
WIII$Friend.Family<- str_replace(WIII$Friend.Family, "Friend", "1")
WIII$Other<- str_replace(WIII$Other, "Friend", "1")
WIII$El.ON.Ad<- str_replace(WIII$El.ON.Ad, "Ad", "2")
WIII$El.ON.Brochure<- str_replace(WIII$El.ON.Brochure, "Brochure", "3")
WIII$El.ON.Info<- str_replace(WIII$El.ON.Info, "Info", "4")
WIII$El.ON.Event<- str_replace(WIII$El.ON.Event, "Event", "5")
WIII$El.ON.Voter.Card<- str_replace(WIII$El.ON.Voter.Card, "VoterCard", "6")
WIII$Other<- str_replace(WIII$Other, "VoterCard", "6")
WIII$El.ON.Web<- str_replace(WIII$El.ON.Web, "Website", "7")
WIII$News<- str_replace(WIII$News, "News", "8")
WIII$Other<- str_replace(WIII$Other, "News", "8")
WIII$Candidate<- str_replace(WIII$Candidate, "Candidate", "9")
WIII$Social.Media<- str_replace(WIII$Social.Media, "SMedia", "10")
WIII$Other<- str_replace(WIII$Other, "CNIB", "11")
WIII$Friend.Family <- as.numeric(WIII$Friend.Family) 
WIII$El.ON.Ad <- as.numeric(WIII$El.ON.Ad) 
WIII$El.ON.Brochure <- as.numeric(WIII$El.ON.Brochure) 
WIII$El.ON.Info <- as.numeric(WIII$El.ON.Info) 
WIII$El.ON.Event <- as.numeric(WIII$El.ON.Event) 
WIII$El.ON.Voter.Card <- as.numeric(WIII$El.ON.Voter.Card) 
WIII$El.ON.Web <- as.numeric(WIII$El.ON.Web) 
WIII$News <- as.numeric(WIII$News) 
WIII$Candidate <- as.numeric(WIII$Candidate) 
WIII$Social.Media <- as.numeric(WIII$Social.Media) 
WIII$Other <- as.numeric(WIII$Other) 
QIII <- unite(WIII, Q3, Friend.Family, El.ON.Ad, El.ON.Brochure, El.ON.Info, 
              El.ON.Event, El.ON.Voter.Card, El.ON.Web, News, Candidate, Social.Media, Other, sep = ",")
lev <- levels(factor(QIII$Q3))
lev <- unique(unlist(strsplit(lev, ",")))
mnames <- gsub(" ", "_", paste("var", lev, sep = "."))
result <- matrix(data = "0", nrow = length(QIII$Q3), ncol = length(lev))
char.var <- as.character(QIII$Q3)
for (i in 1:length(lev)) {
  result[grep(lev[i], char.var, fixed = TRUE), i] <- "1"
}
result <- data.frame(result, stringsAsFactors = TRUE)
colnames(result) <- mnames
QIII <- cbind(QIII,result)
QIII
vars <- c("var.1","var.2","var.3", "var.4", "var.5", "var.6", "var.7", "var.8", "var.9", "var.10", "var.NA", "var.11")
Q3Var <- as.table(sapply(QIII[,vars], function(v) {
  sel <- as.numeric(v==1)
  sum(sel)
}), exclude = NULL)
print(Q3Var)
unique(Q3Var)
colnames(t1) <- c("Q1", "Freq")
print(t1)
elect_new <- unite(elect_new, Q3, Friend.Family, El.ON.Ad, El.ON.Brochure, El.ON.Info, 
              El.ON.Event, El.ON.Voter.Card, El.ON.Web, News, Candidate, Social.Media, Other, sep = ",")
elect_new$Q3 <- as.factor(elect_new$Q3)


elect_new <- unite(elect_new, Q4, Yes.I.could.access.all.of.the.information, I.could.access.some.of.the.information,
                   I.couldn.t.access.any.of.the.information, I.didnâ..t.receive.any.information,
                   I.m.not.aware.I.received.any.information)
elect_new$Q4 <- str_replace(elect_new$Q4, "_I could access some of the information___", "Some")
elect_new$Q4 <- str_replace(elect_new$Q4, "____I'm not aware I received any information", "Not_Aware")
elect_new$Q4 <- str_replace(elect_new$Q4, "Yes I could access all of the information____", "All")
elect_new$Q4 <- str_replace(elect_new$Q4, "___I didnâ???Tt receive any information_", "Not_Receive")
elect_new$Q4 <- str_replace(elect_new$Q4, "__I couldn't access any of the information__", "None")
Q4_NA <- which(str_detect(elect_new$Q4, "____")) 
elect_new$Q4[Q4_NA] <- NA
elect_new$Q4 <- as.factor(elect_new$Q4) 
w4 = table(elect_new$Q4, exclude = NULL)
w4
t4 = as.data.frame(w4)
t4
colnames(t4) <- c("Q4", "Freq")
print(t4)


elect_new <- unite(elect_new, Q5, Yes, No)
elect_new$Q5 <- str_replace(elect_new$Q5, "_No", "No")
elect_new$Q5 <- str_replace(elect_new$Q5, "Yes_", "Yes")
Q5_NA <- which(str_detect(elect_new$Q5, "_")) 
elect_new$Q5[Q5_NA] <- NA
elect_new$Q5 <- as.factor(elect_new$Q5)
Q5.freq <- table(elect_new$Q5, exclude = NULL) 
print(Q5.freq)
w5 = table(elect_new$Q5, exclude = NULL)
w5
t5 = as.data.frame(w5)
t5
colnames(t5) <- c("Q5", "Freq")
print(t5)

elect_new <- unite(elect_new, Q6, Yes.1, No.1)
elect_new$Q6 <- str_replace(elect_new$Q6, "_No", "No")
elect_new$Q6 <- str_replace(elect_new$Q6, "Yes_", "Yes")
Q6_NA <- which(str_detect(elect_new$Q6, "_")) 
elect_new$Q6[Q6_NA] <- NA
elect_new$Q6 <- as.factor(elect_new$Q6)
Q6.freq <- table(elect_new$Q6, exclude = NULL) 
print(Q6.freq)
w6 = table(elect_new$Q6, exclude = NULL)
w6
t6 = as.data.frame(w6)
t6
colnames(t6) <- c("Q6", "Freq")
print(t6)

colnames(elect_new)[16]<- "Not.Require"
elect_new$Not.Require <- str_replace(elect_new$Not.Require, "I.did.not.require.any.assistive.devices.or.aids", "Not_Require")
colnames(elect_new)[18]<- "Video.Magnifier"
elect_new$Video.Magnifier <- str_replace(elect_new$Video.Magnifier, "Handheld video magnifier", "Video_Magnifier")
colnames(elect_new)[19]<- "Magnifier"
elect_new$Magnifier <- str_replace(elect_new$Magnifier, "Handheld magnifier that was not a video device", "Magnifier")
colnames(elect_new)[22]<- "App"
elect_new$App <- str_replace(elect_new$App, "Smartphone or tablet app, such as KNFB Reader or Seeing AI", "App")
colnames(elect_new)[23]<- "Phone.Tablet"
elect_new$Phone.Tablet <- str_replace(elect_new$Phone.Tablet, "Smartphone or tablet, without a specialized app", "Phone_Tablet")
colnames(elect_new)[24]<- "Person"
elect_new$Person <- str_replace(elect_new$Person, "Someone read the material to me", "Person")
colnames(elect_new)[25]<- "Other"
elect_new$Other<- str_replace(elect_new$Other, "I use glasses of about 2.0-2.75 for reading material depending on the size.  For very fine print I would use a handheld magnifier.",
                              "Magnifier")
elect_new$Other<- str_replace(elect_new$Other, "Kurzweil and scanner", "Kurzweil")
elect_new$Other<- str_replace(elect_new$Other, "docuscan", "App")
elect_new$Other<- str_replace(elect_new$Other, "did not receive any info", "")
elect_new$Other<- str_replace(elect_new$Other, "Husband  informed me", "Person")
elect_new$Other<- str_replace(elect_new$Other, "I wear a special pair of glasses called an ocutech--this is how I read print.", "Ocutech")
elect_new$Other<- str_replace(elect_new$Other, "I used my Kurzweil scanner with its speech software.", "Kurzweil")
elect_new$Other<- str_replace(elect_new$Other, "I used my Kurzweil scanner to read the information.", "Kurzweil")
elect_new$Other<- str_replace(elect_new$Other, "One of Elections Ontario's staff members sent me a Braille copy of my voter information card at my request, along with a Braille list of the candidates in my riding.", "Braille")
elect_new$Other<- str_replace(elect_new$Other, "My wife and I voted together so there wasn't an issue.", "Person")
elect_new$Other<- str_replace(elect_new$Other, "Kurzweil Scaqnner", "Kurzweil")
elect_new$Other<- str_replace(elect_new$Other, "Bifocal glasses", "Glasses")
elect_new$Other<- str_replace(elect_new$Other, "Glasses with magnifier in them", "Glasses")
elect_new$Other<- str_replace(elect_new$Other, "A sighted person had to read it to me.", "Person")
elect_new$Other <- str_trim(elect_new$Other)
unique(elect_new$Other)
EIII <- elect_new[, c(16:25)]
glimpse(EIII)
EIII$Not.Require<- str_replace(EIII$Not.Require, "Not_Require", "1")
EIII$CCTV<- str_replace(EIII$CCTV, "CCTV", "2")
EIII$Video.Magnifier<- str_replace(EIII$Video.Magnifier, "Video_Magnifier", "3")
EIII$Magnifier<- str_replace(EIII$Magnifier, "Magnifier", "4")
EIII$eSight<- str_replace(EIII$eSight, "eSight", "5")
EIII$OrCam<- str_replace(EIII$OrCam, "OrCam", "6")
EIII$App<- str_replace(EIII$App, "App", "7")
EIII$Phone.Tablet<- str_replace(EIII$Phone.Tablet, "Phone_Tablet", "8")
EIII$Person<- str_replace(EIII$Person, "Person", "9")
unique(EIII$Other)
EIII$Other<- str_replace(EIII$Other, "Magnifier", "4")
EIII$Other<- str_replace(EIII$Other, "Kurzweil", "10")
EIII$Other<- str_replace(EIII$Other, "App", "7")
EIII$Other<- str_replace(EIII$Other, "Person", "9")
EIII$Other<- str_replace(EIII$Other, "Ocutech", "11")
EIII$Other<- str_replace(EIII$Other, "Braille", "12")
EIII$Other<- str_replace(EIII$Other, "Glasses", "13")
EIII$Not.Require <- as.numeric(EIII$Not.Require) 
EIII$CCTV <- as.numeric(EIII$CCTV) 
EIII$Video.Magnifier <- as.numeric(EIII$Video.Magnifier) 
EIII$Magnifier <- as.numeric(EIII$Magnifier) 
EIII$eSight <- as.numeric(EIII$eSight) 
EIII$OrCam <- as.numeric(EIII$OrCam) 
EIII$App <- as.numeric(EIII$App) 
EIII$Phone.Tablet <- as.numeric(EIII$Phone.Tablet) 
EIII$Person <- as.numeric(EIII$Person) 
EIII$Other<- as.numeric(EIII$Other) 
QVII<- unite(EIII, Q7, Not.Require, CCTV, Video.Magnifier, Magnifier, eSight, OrCam, 
             App, Phone.Tablet, Person, Other, sep = ",")
unique(QVII$Q7)
lev <- levels(factor(QVII$Q7))
lev <- unique(unlist(strsplit(lev, ",")))
mnames <- gsub(" ", "_", paste("var", lev, sep = "."))
result <- matrix(data = "0", nrow = length(QVII$Q7), ncol = length(lev))
char.var <- as.character(QVII$Q7)
for (i in 1:length(lev)) {
  result[grep(lev[i], char.var, fixed = TRUE), i] <- "1"
}
result <- data.frame(result, stringsAsFactors = TRUE)
colnames(result) <- mnames
QVII <- cbind(QVII,result)
QVII
vars <- c("var.1","var.NA", "var.9", "var.2", "var.4", "var.3", "var.7", "var.8", "var.6", "var.10", "var.11", "var.12", "var.13")
Q7var <- as.table(sapply(QVII[,vars], function(v) {
  sel <- as.numeric(v==1)
  sum(sel)
}))
print(Q7var)
elect_new <- unite(elect_new, Q7, Not.Require, CCTV, Video.Magnifier, Magnifier, eSight, OrCam, 
                   App, Phone.Tablet, Person, Other, sep = ",")
elect_new$Q7 <- as.factor(elect_new$Q7)