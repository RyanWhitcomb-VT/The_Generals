library(devtools)
install_github("MangoTheCat/radarchart")
install_github("mjockers/syuzhet")
library(tm)
library(syuzhet)
library(radarchart)
library(jsonlite)
library(dplyr)
library(descr)
################################################################
# Make individual charts for each of the bases

# Fort Belvoir
belvoir <- read.csv("C:/Users/Ryan/Documents/2016 Fall Semester/CMDA 4864/Project/Sentiment Radar Chart/Army_Fort_Belvoir.csv", stringsAsFactors = FALSE)
belvoir$date <- as.Date(belvoir$created_time)
belvoir$Comment <- iconv(belvoir$Comment, "latin1", "ASCII", sub = "")

belvoir2 <- rbind(belvoir)
belvoircomments <- belvoir2$Comment
belvoircomments <- toUTF8(belvoircomments)

sentimentsBelvoir <- get_nrc_sentiment(belvoircomments)

sumSentsBelvoir <- as.data.frame(colSums(sentimentsBelvoir))

sumSentsBelvoir$Names <- row.names(sumSentsBelvoir)

colnames(sumSentsBelvoir) <- c("Sums","Type")

sumSentsBelvoir1 <- sumSentsBelvoir[2:1]
save(sumSentsBelvoir1, file = "sumsentsBelvoir.RData")

sumSentsBelvoir1 <- sumSentsBelvoir1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsBelvoir1) <- c("Label", "Belvoir")




#Aberdeen Proving Grounds
aberdeen <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Aberdeen Proving Ground_comments.csv", stringsAsFactors = FALSE)
aberdeen$date <- as.Date(aberdeen$created_time)
aberdeen$Comment <- iconv(aberdeen$Comment, "latin1", "ASCII", sub = "")
aberdeen2 <- rbind(aberdeen)
aberdeencomments <- aberdeen2$Comment
aberdeencomments <- toUTF8(aberdeencomments)

sentimentsAberdeen <- get_nrc_sentiment(aberdeencomments)

sumSentsAberdeen <- as.data.frame(colSums(sentimentsAberdeen))

sumSentsAberdeen$Names <- row.names(sumSentsAberdeen)

colnames(sumSentsAberdeen) <- c("Sums","Type")

sumSentsAberdeen1 <- sumSentsAberdeen[2:1]
save(sumSentsAberdeen1, file = "sumsentsAberdeen.RData")

sumSentsAberdeen1 <- sumSentsAberdeen1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsAberdeen1) <- c("Label", "Aberdeen")

#Anniston Army Depot
anniston <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Anniston Army Depot_comments.csv", stringsAsFactors = FALSE)
anniston$date <- as.Date(anniston$created_time)
anniston$Comment <- iconv(anniston$Comment, "latin1", "ASCII", sub = "")
anniston2 <- rbind(anniston)
annistoncomments <- anniston2$Comment
annistoncomments <- toUTF8(annistoncomments)

sentimentsAnniston <- get_nrc_sentiment(annistoncomments)

sumSentsAnniston <- as.data.frame(colSums(sentimentsAnniston))

sumSentsAnniston$Names <- row.names(sumSentsAnniston)

colnames(sumSentsAnniston) <- c("Sums","Type")

sumSentsAnniston1 <- sumSentsAnniston[2:1]
save(sumSentsAnniston1, file = "sumsentsAnniston.RData")

sumSentsAnniston1 <- sumSentsAnniston1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsAnniston1) <- c("Label", "Anniston")

#Biggs Army Airfield
biggs <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Biggs Army Airfield_comments.csv", stringsAsFactors = FALSE)
biggs$date <- as.Date(biggs$created_time)
biggs$Comment <- iconv(biggs$Comment, "latin1", "ASCII", sub = "")
biggs2 <- rbind(biggs)
biggscomments <- biggs2$Comment
biggscomments <- toUTF8(biggscomments)

sentimentsBiggs <- get_nrc_sentiment(biggscomments)

sumSentsBiggs <- as.data.frame(colSums(sentimentsBiggs))

sumSentsBiggs$Names <- row.names(sumSentsBiggs)

colnames(sumSentsBiggs) <- c("Sums","Type")

sumSentsBiggs1 <- sumSentsBiggs[2:1]
save(sumSentsBiggs1, file = "sumsentsBiggs.RData")

sumSentsBiggs1 <- sumSentsBiggs1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsBiggs1) <- c("Label", "Biggs")

#Carlisle Barracks
carlisle <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Carlisle Barracks_comments.csv", stringsAsFactors = FALSE)
carlisle$date <- as.Date(carlisle$created_time)
carlisle$Comment <- iconv(carlisle$Comment, "latin1", "ASCII", sub = "")
carlisle2 <- rbind(carlisle)
carlislecomments <- carlisle2$Comment
carlislecomments <- toUTF8(carlislecomments)

sentimentsCarlisle <- get_nrc_sentiment(carlislecomments)

sumSentsCarlisle <- as.data.frame(colSums(sentimentsCarlisle))

sumSentsCarlisle$Names <- row.names(sumSentsCarlisle)

colnames(sumSentsCarlisle) <- c("Sums","Type")

sumSentsCarlisle1 <- sumSentsCarlisle[2:1]
save(sumSentsCarlisle1, file = "sumsentsCarlisle.RData")

sumSentsCarlisle1 <- sumSentsCarlisle1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsCarlisle1) <- c("Label", "Carlisle")

#Corpus Christi Army Depot
corpus <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Corpus Christi Army Depot_comments.csv", stringsAsFactors = FALSE)
corpus$date <- as.Date(corpus$created_time)
corpus$Comment <- iconv(corpus$Comment, "latin1", "ASCII", sub = "")
corpus2 <- rbind(corpus)
corpuscomments <- corpus2$Comment
corpuscomments <- toUTF8(corpuscomments)

sentimentsCorpus <- get_nrc_sentiment(corpuscomments)

sumSentsCorpus <- as.data.frame(colSums(sentimentsCorpus))

sumSentsCorpus$Names <- row.names(sumSentsCorpus)

colnames(sumSentsCorpus) <- c("Sums","Type")

sumSentsCorpus1 <- sumSentsCorpus[2:1]
save(sumSentsCorpus1, file = "sumsentsCorpus.RData")

sumSentsCorpus1 <- sumSentsCorpus1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsCorpus1) <- c("Label", "Corpus")

#Dugway Proving Ground
dugway <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Dugway Proving Ground_comments.csv", stringsAsFactors = FALSE)
dugway$date <- as.Date(dugway$created_time)
dugway$Comment <- iconv(dugway$Comment, "latin1", "ASCII", sub = "")
dugway2 <- rbind(dugway)
dugwaycomments <- dugway2$Comment
dugwaycomments <- toUTF8(dugwaycomments)

sentimentsDugway <- get_nrc_sentiment(dugwaycomments)

sumSentsDugway <- as.data.frame(colSums(sentimentsDugway))

sumSentsDugway$Names <- row.names(sumSentsDugway)

colnames(sumSentsDugway) <- c("Sums","Type")

sumSentsDugway1 <- sumSentsDugway[2:1]
save(sumSentsDugway1, file = "sumsentsDugway.RData")

sumSentsDugway1 <- sumSentsDugway1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsDugway1) <- c("Label", "Dugway")

#Fort A.P. Hill
aphill <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort A.P. Hill.csv", stringsAsFactors = FALSE)
aphill$date <- as.Date(aphill$created_time)
aphill$Comment <- iconv(aphill$Comment, "latin1", "ASCII", sub = "")
aphill2 <- rbind(aphill)
aphillcomments <- aphill2$Comment
aphillcomments <- toUTF8(aphillcomments)

sentimentsAphill <- get_nrc_sentiment(aphillcomments)

sumSentsAphill <- as.data.frame(colSums(sentimentsAphill))

sumSentsAphill$Names <- row.names(sumSentsAphill)

colnames(sumSentsAphill) <- c("Sums","Type")

sumSentsAphill1 <- sumSentsAphill[2:1]
save(sumSentsAphill1, file = "sumsentsAphill.RData")

sumSentsAphill1 <- sumSentsAphill1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsAphill1) <- c("Label", "Aphill")

#Fort Belvior
belvoir <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Belvoir_comments.csv", stringsAsFactors = FALSE)
belvoir$date <- as.Date(belvoir$created_time)
belvoir$Comment <- iconv(belvoir$Comment, "latin1", "ASCII", sub = "")
belvoir2 <- rbind(belvoir)
belvoircomments <- belvoir2$Comment
belvoircomments <- toUTF8(belvoircomments)

sentimentsBelvoir <- get_nrc_sentiment(belvoircomments)

sumSentsBelvoir <- as.data.frame(colSums(sentimentsBelvoir))

sumSentsBelvoir$Names <- row.names(sumSentsBelvoir)

colnames(sumSentsBelvoir) <- c("Sums","Type")

sumSentsBelvoir1 <- sumSentsBelvoir[2:1]
save(sumSentsBelvoir1, file = "sumsentsBelvoir.RData")

sumSentsBelvoir1 <- sumSentsBelvoir1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsBelvoir1) <- c("Label", "Belvoir")


#Fort Benning
benning <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Benning_comments.csv", stringsAsFactors = FALSE)
benning$date <- as.Date(benning$created_time)
benning$Comment <- iconv(benning$Comment, "latin1", "ASCII", sub = "")
benning2 <- rbind(benning)
benningcomments <- benning2$Comment
benningcomments <- toUTF8(benningcomments)

sentimentsBenning <- get_nrc_sentiment(benningcomments)

sumSentsBenning <- as.data.frame(colSums(sentimentsBenning))

sumSentsBenning$Names <- row.names(sumSentsBenning)

colnames(sumSentsBenning) <- c("Sums","Type")

sumSentsBenning1 <- sumSentsBenning[2:1]
save(sumSentsBenning1, file = "sumsentsBenning.RData")

sumSentsBenning1 <- sumSentsBenning1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsBenning1) <- c("Label", "Benning")


#Fort Bliss
bliss <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Bliss_comments.csv", stringsAsFactors = FALSE)
bliss$date <- as.Date(bliss$created_time)
bliss$Comment <- iconv(bliss$Comment, "latin1", "ASCII", sub = "")
bliss2 <- rbind(bliss)
blisscomments <- bliss2$Comment
blisscomments <- toUTF8(blisscomments)

sentimentsBliss <- get_nrc_sentiment(blisscomments)

sumSentsBliss <- as.data.frame(colSums(sentimentsBliss))

sumSentsBliss$Names <- row.names(sumSentsBliss)

colnames(sumSentsBliss) <- c("Sums","Type")

sumSentsBliss1 <- sumSentsBliss[2:1]
save(sumSentsBliss1, file = "sumsentsBliss.RData")

sumSentsBliss1 <- sumSentsBliss1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsBliss1) <- c("Label", "Bliss")



#Fort Buchanan
buchanan <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Buchanan_comments.csv", stringsAsFactors = FALSE)
buchanan$date <- as.Date(buchanan$created_time)
buchanan$Comment <- iconv(buchanan$Comment, "latin1", "ASCII", sub = "")
buchanan2 <- rbind(buchanan)
buchanancomments <- buchanan2$Comment
buchanancomments <- toUTF8(buchanancomments)

sentimentsBuchanan <- get_nrc_sentiment(buchanancomments)

sumSentsBuchanan <- as.data.frame(colSums(sentimentsBuchanan))

sumSentsBuchanan$Names <- row.names(sumSentsBuchanan)

colnames(sumSentsBuchanan) <- c("Sums","Type")

sumSentsBuchanan1 <- sumSentsBuchanan[2:1]
save(sumSentsBuchanan1, file = "sumsentsBuchanan.RData")

sumSentsBuchanan1 <- sumSentsBuchanan1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsBuchanan1) <- c("Label", "Buchanan")


#Fort Campbell
campbell <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Campbell_comments.csv", stringsAsFactors = FALSE)
campbell$date <- as.Date(campbell$created_time)
campbell$Comment <- iconv(campbell$Comment, "latin1", "ASCII", sub = "")
campbell2 <- rbind(campbell)
campbellcomments <- campbell2$Comment
campbellcomments <- toUTF8(campbellcomments)

sentimentsCampbell <- get_nrc_sentiment(campbellcomments)

sumSentsCampbell <- as.data.frame(colSums(sentimentsCampbell))

sumSentsCampbell$Names <- row.names(sumSentsCampbell)

colnames(sumSentsCampbell) <- c("Sums","Type")

sumSentsCampbell1 <- sumSentsCampbell[2:1]
save(sumSentsCampbell1, file = "sumsentsCampbell.RData")

sumSentsCampbell1 <- sumSentsCampbell1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsCampbell1) <- c("Label", "Campbell")



#Fort Carson
carson <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Carson_comments.csv", stringsAsFactors = FALSE)
carson$date <- as.Date(carson$created_time)
carson$Comment <- iconv(carson$Comment, "latin1", "ASCII", sub = "")
carson2 <- rbind(carson)
carsoncomments <- carson2$Comment
carsoncomments <- toUTF8(carsoncomments)

sentimentsCarson <- get_nrc_sentiment(carsoncomments)

sumSentsCarson <- as.data.frame(colSums(sentimentsCarson))

sumSentsCarson$Names <- row.names(sumSentsCarson)

colnames(sumSentsCarson) <- c("Sums","Type")

sumSentsCarson1 <- sumSentsCarson[2:1]
save(sumSentsCarson1, file = "sumsentsCarson.RData")

sumSentsCarson1 <- sumSentsCarson1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsCarson1) <- c("Label", "Carson")


#Fort Detrick
detrick <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Detrick_comments.csv", stringsAsFactors = FALSE)
detrick$date <- as.Date(detrick$created_time)
detrick$Comment <- iconv(detrick$Comment, "latin1", "ASCII", sub = "")
detrick2 <- rbind(detrick)
detrickcomments <- detrick2$Comment
detrickcomments <- toUTF8(detrickcomments)

sentimentsDetrick <- get_nrc_sentiment(detrickcomments)

sumSentsDetrick <- as.data.frame(colSums(sentimentsDetrick))

sumSentsDetrick$Names <- row.names(sumSentsDetrick)

colnames(sumSentsDetrick) <- c("Sums","Type")

sumSentsDetrick1 <- sumSentsDetrick[2:1]
save(sumSentsDetrick1, file = "sumsentsDetrick.RData")

sumSentsDetrick1 <- sumSentsDetrick1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsDetrick1) <- c("Label", "Detrick")



#Fort Drum
drum <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Drum_comments.csv", stringsAsFactors = FALSE)
drum$date <- as.Date(drum$created_time)
drum$Comment <- iconv(drum$Comment, "latin1", "ASCII", sub = "")
drum2 <- rbind(drum)
drumcomments <- drum2$Comment
drumcomments <- toUTF8(drumcomments)

sentimentsDrum <- get_nrc_sentiment(drumcomments)

sumSentsDrum <- as.data.frame(colSums(sentimentsDrum))

sumSentsDrum$Names <- row.names(sumSentsDrum)

colnames(sumSentsDrum) <- c("Sums","Type")

sumSentsDrum1 <- sumSentsDrum[2:1]
save(sumSentsDrum1, file = "sumsentsDrum.RData")

sumSentsDrum1 <- sumSentsDrum1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsDrum1) <- c("Label", "Drum")

#Fort Eustis
eustis <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Eustis (Joint Base Langley-Eustis)_comments.csv", stringsAsFactors = FALSE)
eustis$date <- as.Date(eustis$created_time)
eustis$Comment <- iconv(eustis$Comment, "latin1", "ASCII", sub = "")
eustis2 <- rbind(eustis)
eustiscomments <- eustis2$Comment
eustiscomments <- toUTF8(eustiscomments)

sentimentsEustis <- get_nrc_sentiment(eustiscomments)

sumSentsEustis <- as.data.frame(colSums(sentimentsEustis))

sumSentsEustis$Names <- row.names(sumSentsEustis)

colnames(sumSentsEustis) <- c("Sums","Type")

sumSentsEustis1 <- sumSentsEustis[2:1]
save(sumSentsEustis1, file = "sumsentsEustis.RData")

sumSentsEustis1 <- sumSentsEustis1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsEustis1) <- c("Label", "Eustis")


#Fort Gordon
gordon <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Gordon_comments.csv", stringsAsFactors = FALSE)
gordon$date <- as.Date(gordon$created_time)
gordon$Comment <- iconv(gordon$Comment, "latin1", "ASCII", sub = "")
gordon2 <- rbind(gordon)
gordoncomments <- gordon2$Comment
gordoncomments <- toUTF8(gordoncomments)

sentimentsGordon <- get_nrc_sentiment(gordoncomments)

sumSentsGordon <- as.data.frame(colSums(sentimentsGordon))

sumSentsGordon$Names <- row.names(sumSentsGordon)

colnames(sumSentsGordon) <- c("Sums","Type")

sumSentsGordon1 <- sumSentsGordon[2:1]
save(sumSentsGordon1, file = "sumsentsGordon.RData")

sumSentsGordon1 <- sumSentsGordon1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsGordon1) <- c("Label", "Gordon")


#Fort Greely
greely <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Greely_comments.csv", stringsAsFactors = FALSE)
greely$date <- as.Date(greely$created_time)
greely$Comment <- iconv(greely$Comment, "latin1", "ASCII", sub = "")
greely2 <- rbind(greely)
greelycomments <- greely2$Comment
greelycomments <- toUTF8(greelycomments)

sentimentsGreely <- get_nrc_sentiment(greelycomments)

sumSentsGreely <- as.data.frame(colSums(sentimentsGreely))

sumSentsGreely$Names <- row.names(sumSentsGreely)

colnames(sumSentsGreely) <- c("Sums","Type")

sumSentsGreely1 <- sumSentsGreely[2:1]
save(sumSentsGreely1, file = "sumsentsGreely.RData")

sumSentsGreely1 <- sumSentsGreely1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsGreely1) <- c("Label", "Greely")

#Fort Hamilton
hamilton <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Hamilton_comments.csv", stringsAsFactors = FALSE)
hamilton$date <- as.Date(hamilton$created_time)
hamilton$Comment <- iconv(hamilton$Comment, "latin1", "ASCII", sub = "")
hamilton2 <- rbind(hamilton)
hamiltoncomments <- hamilton2$Comment
hamiltoncomments <- toUTF8(hamiltoncomments)

sentimentsHamilton <- get_nrc_sentiment(hamiltoncomments)

sumSentsHamilton <- as.data.frame(colSums(sentimentsHamilton))

sumSentsHamilton$Names <- row.names(sumSentsHamilton)

colnames(sumSentsHamilton) <- c("Sums","Type")

sumSentsHamilton1 <- sumSentsHamilton[2:1]
save(sumSentsHamilton1, file = "sumsentsHamilton.RData")

sumSentsHamilton1 <- sumSentsHamilton1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsHamilton1) <- c("Label", "Hamilton")

#Fort Huachuca
huachuca <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Huachuca_comments.csv", stringsAsFactors = FALSE)
huachuca$date <- as.Date(huachuca$created_time)
huachuca$Comment <- iconv(huachuca$Comment, "latin1", "ASCII", sub = "")
huachuca2 <- rbind(huachuca)
huachucacomments <- huachuca2$Comment
huachucacomments <- toUTF8(huachucacomments)

sentimentsHuachuca <- get_nrc_sentiment(huachucacomments)

sumSentsHuachuca <- as.data.frame(colSums(sentimentsHuachuca))

sumSentsHuachuca$Names <- row.names(sumSentsHuachuca)

colnames(sumSentsHuachuca) <- c("Sums","Type")

sumSentsHuachuca1 <- sumSentsHuachuca[2:1]
save(sumSentsHuachuca1, file = "sumsentsHuachuca.RData")

sumSentsHuachuca1 <- sumSentsHuachuca1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsHuachuca1) <- c("Label", "Huachuca")

#Fort Irwin
irwin <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Irwin_comments.csv", stringsAsFactors = FALSE)
irwin$date <- as.Date(irwin$created_time)
irwin$Comment <- iconv(irwin$Comment, "latin1", "ASCII", sub = "")
irwin2 <- rbind(irwin)
irwincomments <- irwin2$Comment
irwincomments <- toUTF8(irwincomments)

sentimentsIrwin <- get_nrc_sentiment(irwincomments)

sumSentsIrwin <- as.data.frame(colSums(sentimentsIrwin))

sumSentsIrwin$Names <- row.names(sumSentsIrwin)

colnames(sumSentsIrwin) <- c("Sums","Type")

sumSentsIrwin1 <- sumSentsIrwin[2:1]
save(sumSentsIrwin1, file = "sumsentsIrwin.RData")

sumSentsIrwin1 <- sumSentsIrwin1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsIrwin1) <- c("Label", "Irwin")

#Fort Jackson
jackson <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Jackson_comments.csv", stringsAsFactors = FALSE)
jackson$date <- as.Date(jackson$created_time)
jackson$Comment <- iconv(jackson$Comment, "latin1", "ASCII", sub = "")
jackson2 <- rbind(jackson)
jacksoncomments <- jackson2$Comment
jacksoncomments <- toUTF8(jacksoncomments)

sentimentsJackson <- get_nrc_sentiment(jacksoncomments)

sumSentsJackson <- as.data.frame(colSums(sentimentsJackson))

sumSentsJackson$Names <- row.names(sumSentsJackson)

colnames(sumSentsJackson) <- c("Sums","Type")

sumSentsJackson1 <- sumSentsJackson[2:1]
save(sumSentsJackson1, file = "sumsentsJackson.RData")

sumSentsJackson1 <- sumSentsJackson1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsJackson1) <- c("Label", "Jackson")

#Fort Knox
knox <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Knox_comments.csv", stringsAsFactors = FALSE)
knox$date <- as.Date(knox$created_time)
knox$Comment <- iconv(knox$Comment, "latin1", "ASCII", sub = "")
knox2 <- rbind(knox)
knoxcomments <- knox2$Comment
knoxcomments <- toUTF8(knoxcomments)

sentimentsKnox <- get_nrc_sentiment(knoxcomments)

sumSentsKnox <- as.data.frame(colSums(sentimentsKnox))

sumSentsKnox$Names <- row.names(sumSentsKnox)

colnames(sumSentsKnox) <- c("Sums","Type")

sumSentsKnox1 <- sumSentsKnox[2:1]
save(sumSentsKnox1, file = "sumsentsKnox.RData")

sumSentsKnox1 <- sumSentsKnox1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsKnox1) <- c("Label", "Knox")

#Fort Leavenworth
leavenworth <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Leavenworth_comments.csv", stringsAsFactors = FALSE)
leavenworth$date <- as.Date(leavenworth$created_time)
leavenworth$Comment <- iconv(leavenworth$Comment, "latin1", "ASCII", sub = "")
leavenworth2 <- rbind(leavenworth)
leavenworthcomments <- leavenworth2$Comment
leavenworthcomments <- toUTF8(leavenworthcomments)

sentimentsLeavenworth <- get_nrc_sentiment(leavenworthcomments)

sumSentsLeavenworth <- as.data.frame(colSums(sentimentsLeavenworth))

sumSentsLeavenworth$Names <- row.names(sumSentsLeavenworth)

colnames(sumSentsLeavenworth) <- c("Sums","Type")

sumSentsLeavenworth1 <- sumSentsLeavenworth[2:1]
save(sumSentsLeavenworth1, file = "sumsentsLeavenworth.RData")

sumSentsLeavenworth1 <- sumSentsLeavenworth1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsLeavenworth1) <- c("Label", "Leavenworth")

#Fort Lee
lee <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Lee_comments.csv", stringsAsFactors = FALSE)
lee$date <- as.Date(lee$created_time)
lee$Comment <- iconv(lee$Comment, "latin1", "ASCII", sub = "")
lee2 <- rbind(lee)
leecomments <- lee2$Comment
leecomments <- toUTF8(leecomments)

sentimentsLee <- get_nrc_sentiment(leecomments)

sumSentsLee <- as.data.frame(colSums(sentimentsLee))

sumSentsLee$Names <- row.names(sumSentsLee)

colnames(sumSentsLee) <- c("Sums","Type")

sumSentsLee1 <- sumSentsLee[2:1]
save(sumSentsLee1, file = "sumsentsLee.RData")

sumSentsLee1 <- sumSentsLee1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsLee1) <- c("Label", "Lee")


#Fort Leonard Wood
leanord <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Leonard Wood_comments.csv", stringsAsFactors = FALSE)
leanord$date <- as.Date(leanord$created_time)
leanord$Comment <- iconv(leanord$Comment, "latin1", "ASCII", sub = "")
leanord2 <- rbind(leanord)
leanordcomments <- leanord2$Comment
leanordcomments <- toUTF8(leanordcomments)

sentimentsLeanord <- get_nrc_sentiment(leanordcomments)

sumSentsLeanord <- as.data.frame(colSums(sentimentsLeanord))

sumSentsLeanord$Names <- row.names(sumSentsLeanord)

colnames(sumSentsLeanord) <- c("Sums","Type")

sumSentsLeanord1 <- sumSentsLeanord[2:1]
save(sumSentsLeanord1, file = "sumsentsLeanord.RData")

sumSentsLeanord1 <- sumSentsLeanord1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsLeanord1) <- c("Label", "Leanord")

#Fort Meade
meade <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Meade_comments.csv", stringsAsFactors = FALSE)
meade$date <- as.Date(meade$created_time)
meade$Comment <- iconv(meade$Comment, "latin1", "ASCII", sub = "")
meade2 <- rbind(meade)
meadecomments <- meade2$Comment
meadecomments <- toUTF8(meadecomments)

sentimentsMeade <- get_nrc_sentiment(meadecomments)

sumSentsMeade <- as.data.frame(colSums(sentimentsMeade))

sumSentsMeade$Names <- row.names(sumSentsMeade)

colnames(sumSentsMeade) <- c("Sums","Type")

sumSentsMeade1 <- sumSentsMeade[2:1]
save(sumSentsMeade1, file = "sumsentsMeade.RData")

sumSentsMeade1 <- sumSentsMeade1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsMeade1) <- c("Label", "Meade")

#Fort Meyer
meyer <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Meyer (Joint Base Myer-Henderson Hall)_comments.csv", stringsAsFactors = FALSE)
meyer$date <- as.Date(meyer$created_time)
meyer$Comment <- iconv(meyer$Comment, "latin1", "ASCII", sub = "")
meyer2 <- rbind(meyer)
meyercomments <- meyer2$Comment
meyercomments <- toUTF8(meyercomments)

sentimentsMeyer <- get_nrc_sentiment(meyercomments)

sumSentsMeyer <- as.data.frame(colSums(sentimentsMeyer))

sumSentsMeyer$Names <- row.names(sumSentsMeyer)

colnames(sumSentsMeyer) <- c("Sums","Type")

sumSentsMeyer1 <- sumSentsMeyer[2:1]
save(sumSentsMeyer1, file = "sumsentsMeyer.RData")

sumSentsMeyer1 <- sumSentsMeyer1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsMeyer1) <- c("Label", "Meyer")

#Fort Polk
polk <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Polk_comments.csv", stringsAsFactors = FALSE)
polk$date <- as.Date(polk$created_time)
polk$Comment <- iconv(polk$Comment, "latin1", "ASCII", sub = "")
polk2 <- rbind(polk)
polkcomments <- polk2$Comment
polkcomments <- toUTF8(polkcomments)

sentimentsPolk <- get_nrc_sentiment(polkcomments)

sumSentsPolk <- as.data.frame(colSums(sentimentsPolk))

sumSentsPolk$Names <- row.names(sumSentsPolk)

colnames(sumSentsPolk) <- c("Sums","Type")

sumSentsPolk1 <- sumSentsPolk[2:1]
save(sumSentsPolk1, file = "sumsentsPolk.RData")

sumSentsPolk1 <- sumSentsPolk1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsPolk1) <- c("Label", "Polk")

#Fort Riley
riley <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Riley_comments.csv", stringsAsFactors = FALSE)
riley$date <- as.Date(riley$created_time)
riley$Comment <- iconv(riley$Comment, "latin1", "ASCII", sub = "")
riley2 <- rbind(riley)
rileycomments <- riley2$Comment
rileycomments <- toUTF8(rileycomments)

sentimentsRiley <- get_nrc_sentiment(rileycomments)

sumSentsRiley <- as.data.frame(colSums(sentimentsRiley))

sumSentsRiley$Names <- row.names(sumSentsRiley)

colnames(sumSentsRiley) <- c("Sums","Type")

sumSentsRiley1 <- sumSentsRiley[2:1]
save(sumSentsRiley1, file = "sumsentsRiley.RData")

sumSentsRiley1 <- sumSentsRiley1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsRiley1) <- c("Label", "Riley")

#Fort Stewart
stewart <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Stewart_comments.csv", stringsAsFactors = FALSE)
stewart$date <- as.Date(stewart$created_time)
stewart$Comment <- iconv(stewart$Comment, "latin1", "ASCII", sub = "")
stewart2 <- rbind(stewart)
stewartcomments <- stewart2$Comment
stewartcomments <- toUTF8(stewartcomments)

sentimentsStewart <- get_nrc_sentiment(stewartcomments)

sumSentsStewart <- as.data.frame(colSums(sentimentsStewart))

sumSentsStewart$Names <- row.names(sumSentsStewart)

colnames(sumSentsStewart) <- c("Sums","Type")

sumSentsStewart1 <- sumSentsStewart[2:1]
save(sumSentsStewart1, file = "sumsentsStewart.RData")

sumSentsStewart1 <- sumSentsStewart1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsStewart1) <- c("Label", "Stewart")

#Fort Wainwright
wainwright <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Fort Wainwright_comments.csv", stringsAsFactors = FALSE)
wainwright$date <- as.Date(wainwright$created_time)
wainwright$Comment <- iconv(wainwright$Comment, "latin1", "ASCII", sub = "")
wainwright2 <- rbind(wainwright)
wainwrightcomments <- wainwright2$Comment
wainwrightcomments <- toUTF8(wainwrightcomments)

sentimentsWainwright <- get_nrc_sentiment(wainwrightcomments)

sumSentsWainwright <- as.data.frame(colSums(sentimentsWainwright))

sumSentsWainwright$Names <- row.names(sumSentsWainwright)

colnames(sumSentsWainwright) <- c("Sums","Type")

sumSentsWainwright1 <- sumSentsWainwright[2:1]
save(sumSentsWainwright1, file = "sumsentsWainwright.RData")

sumSentsWainwright1 <- sumSentsWainwright1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsWainwright1) <- c("Label", "Wainwright")

#Hunter Army Airfield
hunter <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Hunter Army Airfield_comments.csv", stringsAsFactors = FALSE)
hunter$date <- as.Date(hunter$created_time)
hunter$Comment <- iconv(hunter$Comment, "latin1", "ASCII", sub = "")
hunter2 <- rbind(hunter)
huntercomments <- hunter2$Comment
huntercomments <- toUTF8(huntercomments)

sentimentsHunter <- get_nrc_sentiment(huntercomments)

sumSentsHunter <- as.data.frame(colSums(sentimentsHunter))

sumSentsHunter$Names <- row.names(sumSentsHunter)

colnames(sumSentsHunter) <- c("Sums","Type")

sumSentsHunter1 <- sumSentsHunter[2:1]
save(sumSentsHunter1, file = "sumsentsHunter.RData")

sumSentsHunter1 <- sumSentsHunter1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsHunter1) <- c("Label", "Hunter")

#JBLM
jblm <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Joint Base Lewis-McChord [JBLM]_comments.csv", stringsAsFactors = FALSE)
jblm$date <- as.Date(jblm$created_time)
jblm$Comment <- iconv(jblm$Comment, "latin1", "ASCII", sub = "")
jblm2 <- rbind(jblm)
jblmcomments <- jblm2$Comment
jblmcomments <- toUTF8(jblmcomments)

sentimentsJblm <- get_nrc_sentiment(jblmcomments)

sumSentsJblm <- as.data.frame(colSums(sentimentsJblm))

sumSentsJblm$Names <- row.names(sumSentsJblm)

colnames(sumSentsJblm) <- c("Sums","Type")

sumSentsJblm1 <- sumSentsJblm[2:1]
save(sumSentsJblm1, file = "sumsentsJblm.RData")

sumSentsJblm1 <- sumSentsJblm1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsJblm1) <- c("Label", "Jblm")

#Letterkenny Army Depot
letterkenny <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Letterkenny Army Depot_comments.csv", stringsAsFactors = FALSE)
letterkenny$date <- as.Date(letterkenny$created_time)
letterkenny$Comment <- iconv(letterkenny$Comment, "latin1", "ASCII", sub = "")
letterkenny2 <- rbind(letterkenny)
letterkennycomments <- letterkenny2$Comment
letterkennycomments <- toUTF8(letterkennycomments)

sentimentsLetterkenny <- get_nrc_sentiment(letterkennycomments)

sumSentsLetterkenny <- as.data.frame(colSums(sentimentsLetterkenny))

sumSentsLetterkenny$Names <- row.names(sumSentsLetterkenny)

colnames(sumSentsLetterkenny) <- c("Sums","Type")

sumSentsLetterkenny1 <- sumSentsLetterkenny[2:1]
save(sumSentsLetterkenny1, file = "sumsentsLetterkenny.RData")

sumSentsLetterkenny1 <- sumSentsLetterkenny1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsLetterkenny1) <- c("Label", "Letterkenny")

#Picatinny Arsenal
picatinny <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Picatinny Arsenal_comments.csv", stringsAsFactors = FALSE)
picatinny$date <- as.Date(picatinny$created_time)
picatinny$Comment <- iconv(picatinny$Comment, "latin1", "ASCII", sub = "")
picatinny2 <- rbind(picatinny)
picatinnycomments <- picatinny2$Comment
picatinnycomments <- toUTF8(picatinnycomments)

sentimentsPicatinny <- get_nrc_sentiment(picatinnycomments)

sumSentsPicatinny <- as.data.frame(colSums(sentimentsPicatinny))

sumSentsPicatinny$Names <- row.names(sumSentsPicatinny)

colnames(sumSentsPicatinny) <- c("Sums","Type")

sumSentsPicatinny1 <- sumSentsPicatinny[2:1]
save(sumSentsPicatinny1, file = "sumsentsPicatinny.RData")

sumSentsPicatinny1 <- sumSentsPicatinny1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsPicatinny1) <- c("Label", "Picatinny")

#Pine Bluff Arsenal
pinebluff <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Pine Bluff Arsenal_comments.csv", stringsAsFactors = FALSE)
pinebluff$date <- as.Date(pinebluff$created_time)
pinebluff$Comment <- iconv(pinebluff$Comment, "latin1", "ASCII", sub = "")
pinebluff2 <- rbind(pinebluff)
pinebluffcomments <- pinebluff2$Comment
pinebluffcomments <- toUTF8(pinebluffcomments)

sentimentsPinebluff <- get_nrc_sentiment(pinebluffcomments)

sumSentsPinebluff <- as.data.frame(colSums(sentimentsPinebluff))

sumSentsPinebluff$Names <- row.names(sumSentsPinebluff)

colnames(sumSentsPinebluff) <- c("Sums","Type")

sumSentsPinebluff1 <- sumSentsPinebluff[2:1]
save(sumSentsPinebluff1, file = "sumsentsPinebluff.RData")

sumSentsPinebluff1 <- sumSentsPinebluff1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsPinebluff1) <- c("Label", "Pinebluff")

#Presidio of Monterey
presidio <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Presidio of Monterey_comments.csv", stringsAsFactors = FALSE)
presidio$date <- as.Date(presidio$created_time)
presidio$Comment <- iconv(presidio$Comment, "latin1", "ASCII", sub = "")
presidio2 <- rbind(presidio)
presidiocomments <- presidio2$Comment
presidiocomments <- toUTF8(presidiocomments)

sentimentsPresidio <- get_nrc_sentiment(presidiocomments)

sumSentsPresidio <- as.data.frame(colSums(sentimentsPresidio))

sumSentsPresidio$Names <- row.names(sumSentsPresidio)

colnames(sumSentsPresidio) <- c("Sums","Type")

sumSentsPresidio1 <- sumSentsPresidio[2:1]
save(sumSentsPresidio1, file = "sumsentsPresidio.RData")

sumSentsPresidio1 <- sumSentsPresidio1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsPresidio1) <- c("Label", "Presidio")

#Pueblo Chemical Depot
pueblo <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Pueblo Chemical Depot_comments.csv", stringsAsFactors = FALSE)
pueblo$date <- as.Date(pueblo$created_time)
pueblo$Comment <- iconv(pueblo$Comment, "latin1", "ASCII", sub = "")
pueblo2 <- rbind(pueblo)
pueblocomments <- pueblo2$Comment
pueblocomments <- toUTF8(pueblocomments)

sentimentsPueblo <- get_nrc_sentiment(pueblocomments)

sumSentsPueblo <- as.data.frame(colSums(sentimentsPueblo))

sumSentsPueblo$Names <- row.names(sumSentsPueblo)

colnames(sumSentsPueblo) <- c("Sums","Type")

sumSentsPueblo1 <- sumSentsPueblo[2:1]
save(sumSentsPueblo1, file = "sumsentsPueblo.RData")

sumSentsPueblo1 <- sumSentsPueblo1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsPueblo1) <- c("Label", "Pueblo")

#Red River Army Depot
redriver <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Red River Army Depot_comments.csv", stringsAsFactors = FALSE)
redriver$date <- as.Date(redriver$created_time)
redriver$Comment <- iconv(redriver$Comment, "latin1", "ASCII", sub = "")
redriver2 <- rbind(redriver)
redrivercomments <- redriver2$Comment
redrivercomments <- toUTF8(redrivercomments)

sentimentsRedriver <- get_nrc_sentiment(redrivercomments)

sumSentsRedriver <- as.data.frame(colSums(sentimentsRedriver))

sumSentsRedriver$Names <- row.names(sumSentsRedriver)

colnames(sumSentsRedriver) <- c("Sums","Type")

sumSentsRedriver1 <- sumSentsRedriver[2:1]
save(sumSentsRedriver1, file = "sumsentsRedriver.RData")

sumSentsRedriver1 <- sumSentsRedriver1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsRedriver1) <- c("Label", "Redriver")

#Rock Island Arsenal
rockisland <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Rock Island Arsenal_comments.csv", stringsAsFactors = FALSE)
rockisland$date <- as.Date(rockisland$created_time)
rockisland$Comment <- iconv(rockisland$Comment, "latin1", "ASCII", sub = "")
rockisland2 <- rbind(rockisland)
rockislandcomments <- rockisland2$Comment
rockislandcomments <- toUTF8(rockislandcomments)

sentimentsRockisland <- get_nrc_sentiment(rockislandcomments)

sumSentsRockisland <- as.data.frame(colSums(sentimentsRockisland))

sumSentsRockisland$Names <- row.names(sumSentsRockisland)

colnames(sumSentsRockisland) <- c("Sums","Type")

sumSentsRockisland1 <- sumSentsRockisland[2:1]
save(sumSentsRockisland1, file = "sumsentsRockisland.RData")

sumSentsRockisland1 <- sumSentsRockisland1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsRockisland1) <- c("Label", "Rockisland")

#Soldier System Center
ssc <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Soldier Systems Center_comments.csv", stringsAsFactors = FALSE)
ssc$date <- as.Date(ssc$created_time)
ssc$Comment <- iconv(ssc$Comment, "latin1", "ASCII", sub = "")
ssc2 <- rbind(ssc)
ssccomments <- ssc2$Comment
ssccomments <- toUTF8(ssccomments)

sentimentsSsc <- get_nrc_sentiment(ssccomments)

sumSentsSsc <- as.data.frame(colSums(sentimentsSsc))

sumSentsSsc$Names <- row.names(sumSentsSsc)

colnames(sumSentsSsc) <- c("Sums","Type")

sumSentsSsc1 <- sumSentsSsc[2:1]
save(sumSentsSsc1, file = "sumsentsSsc.RData")

sumSentsSsc1 <- sumSentsSsc1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsSsc1) <- c("Label", "Ssc")

#Tobyhanna Army Depot
toby <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Tobyhanna Army Depot_comments.csv", stringsAsFactors = FALSE)
toby$date <- as.Date(toby$created_time)
toby$Comment <- iconv(toby$Comment, "latin1", "ASCII", sub = "")
toby2 <- rbind(toby)
tobycomments <- toby2$Comment
tobycomments <- toUTF8(tobycomments)

sentimentsToby <- get_nrc_sentiment(tobycomments)

sumSentsToby <- as.data.frame(colSums(sentimentsToby))

sumSentsToby$Names <- row.names(sumSentsToby)

colnames(sumSentsToby) <- c("Sums","Type")

sumSentsToby1 <- sumSentsToby[2:1]
save(sumSentsToby1, file = "sumsentsToby.RData")

sumSentsToby1 <- sumSentsToby1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsToby1) <- c("Label", "Toby")

#Walter Reed Army Medical Center
walterreed <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Walter Reed Army Medical Center_comments.csv", stringsAsFactors = FALSE)
walterreed$date <- as.Date(walterreed$created_time)
walterreed$Comment <- iconv(walterreed$Comment, "latin1", "ASCII", sub = "")
walterreed2 <- rbind(walterreed)
walterreedcomments <- walterreed2$Comment
walterreedcomments <- toUTF8(walterreedcomments)

sentimentsWalterreed <- get_nrc_sentiment(walterreedcomments)

sumSentsWalterreed <- as.data.frame(colSums(sentimentsWalterreed))

sumSentsWalterreed$Names <- row.names(sumSentsWalterreed)

colnames(sumSentsWalterreed) <- c("Sums","Type")

sumSentsWalterreed1 <- sumSentsWalterreed[2:1]
save(sumSentsWalterreed1, file = "sumsentsWalterreed.RData")

sumSentsWalterreed1 <- sumSentsWalterreed1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsWalterreed1) <- c("Label", "Walterreed")

#Watervliet Arsenal
watervliet <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Watervliet Arsenal_comments.csv", stringsAsFactors = FALSE)
watervliet$date <- as.Date(watervliet$created_time)
watervliet$Comment <- iconv(watervliet$Comment, "latin1", "ASCII", sub = "")
watervliet2 <- rbind(watervliet)
watervlietcomments <- watervliet2$Comment
watervlietcomments <- toUTF8(watervlietcomments)

sentimentswatervliet <- get_nrc_sentiment(watervlietcomments)

sumSentsWatervliet <- as.data.frame(colSums(sentimentswatervliet))

sumSentsWatervliet$Names <- row.names(sumSentsWatervliet)

colnames(sumSentsWatervliet) <- c("Sums","Type")

sumSentsWatervliet1 <- sumSentsWatervliet[2:1]
save(sumSentsWatervliet1, file = "sumsentsWatervliet.RData")

sumSentsWatervliet1 <- sumSentsWatervliet1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsWatervliet1) <- c("Label", "Watervliet")

#Yuma Proving Grounds
yuma <- read.csv("C:/Users/Ian/Documents/Homework/CMDA 4654/The_Generals-master/Comment Scraping/Processed_Data/Yuma Proving Ground_comments.csv", stringsAsFactors = FALSE)
yuma$date <- as.Date(yuma$created_time)
yuma$Comment <- iconv(yuma$Comment, "latin1", "ASCII", sub = "")
yuma2 <- rbind(yuma)
yumacomments <- yuma2$Comment
yumacomments <- toUTF8(yumacomments)

sentimentsYuma <- get_nrc_sentiment(yumacomments)

sumSentsYuma <- as.data.frame(colSums(sentimentsYuma))

sumSentsYuma$Names <- row.names(sumSentsYuma)

colnames(sumSentsYuma) <- c("Sums","Type")

sumSentsYuma1 <- sumSentsYuma[2:1]
save(sumSentsYuma1, file = "sumsentsYuma.RData")

sumSentsYuma1 <- sumSentsYuma1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsYuma1) <- c("Label", "Yuma")


#NorthEast
north_viz <- left_join(sumSentsCarlisle1, sumSentsAberdeen1, by = c("Label"="Label"))
north_viz <- left_join(north_viz, sumSentsDetrick1, by = c("Label"="Label"))
north_viz <- left_join(north_viz, sumSentsDrum1, by = c("Label"="Label"))
north_viz <- left_join(north_viz, sumSentsEustis1, by = c("Label"="Label"))
north_viz <- left_join(north_viz, sumSentsGordon1, by = c("Label"="Label"))
north_viz <- left_join(north_viz, sumSentsHamilton1, by = c("Label"="Label"))
north_viz <- left_join(north_viz, sumSentsLee1, by = c("Label"="Label"))
north_viz <- left_join(north_viz, sumSentsMeade1, by = c("Label"="Label"))
north_viz <- left_join(north_viz, sumSentsLetterkenny1, by = c("Label"="Label"))
north_viz <- left_join(north_viz, sumSentsPicatinny1, by = c("Label"="Label"))
north_viz <- left_join(north_viz, sumSentsSsc1, by = c("Label"="Label"))
north_viz <- left_join(north_viz, sumSentsToby1, by = c("Label"="Label"))
north_viz <- left_join(north_viz, sumSentsWalterreed1, by = c("Label"="Label"))
north_viz <- left_join(north_viz, sumSentsWatervliet1, by = c("Label"="Label"))

north_anger_viz = unlist(north_viz[1,c(2:16)])
north_antic_viz = unlist(north_viz[2,c(2:16)])
north_disgust_viz = unlist(north_viz[3,c(2:16)])
north_fear_viz = unlist(north_viz[4,c(2:16)])
north_joy_viz = unlist(north_viz[5,c(2:16)])
north_sad_viz = unlist(north_viz[6,c(2:16)])
north_surprise_viz = unlist(north_viz[7,c(2:16)])
north_trust_viz = unlist(north_viz[8,c(2:16)])
north_neg_viz = unlist(north_viz[9,c(2:16)])
north_pos_viz = unlist(north_viz[10,c(2:16)])

boxplot(north_anger_viz, north_antic_viz, north_disgust_viz, north_fear_viz, 
        north_joy_viz, north_sad_viz, north_surprise_viz, north_trust_viz,
        north_neg_viz, north_pos_viz,
        col=c("maroon","orange","gray"), ylab = "Sentiment Score",
        names = c("Anger", "Anticipation", "Disgust", "Fear", "Joy", 
                  "Sadness", "Surprise", "Trust", "Negative", "Positive"),
        pch=16,  las = 2, main='Variability of Base Sentiment of Northern Bases')


#South
south_viz <- left_join(sumSentsCorpus1, sumSentsBiggs1, by = c("Label"="Label"))
south_viz <- left_join(south_viz, sumSentsAphill1, by = c("Label"="Label"))
south_viz <- left_join(south_viz, sumSentsBelvoir1, by = c("Label"="Label"))
south_viz <- left_join(south_viz, sumSentsBenning1, by = c("Label"="Label"))
south_viz <- left_join(south_viz, sumSentsBliss1, by = c("Label"="Label"))
south_viz <- left_join(south_viz, sumSentsBuchanan1, by = c("Label"="Label"))
south_viz <- left_join(south_viz, sumSentsCampbell1, by = c("Label"="Label"))
south_viz <- left_join(south_viz, sumSentsJackson1, by = c("Label"="Label"))
south_viz <- left_join(south_viz, sumSentsKnox1, by = c("Label"="Label"))
south_viz <- left_join(south_viz, sumSentsMeyer1, by = c("Label"="Label"))
south_viz <- left_join(south_viz, sumSentsPolk1, by = c("Label"="Label"))
south_viz <- left_join(south_viz, sumSentsStewart1, by = c("Label"="Label"))
south_viz <- left_join(south_viz, sumSentsHunter1, by = c("Label"="Label"))
south_viz <- left_join(south_viz, sumSentsPinebluff1, by = c("Label"="Label"))
south_viz <- left_join(south_viz, sumSentsRedriver1, by = c("Label"="Label"))

south_anger_viz = unlist(south_viz[1,c(2:17)])
south_antic_viz = unlist(south_viz[2,c(2:17)])
south_disgust_viz = unlist(south_viz[3,c(2:17)])
south_fear_viz = unlist(south_viz[4,c(2:17)])
south_joy_viz = unlist(south_viz[5,c(2:17)])
south_sad_viz = unlist(south_viz[6,c(2:17)])
south_surprise_viz = unlist(south_viz[7,c(2:17)])
south_trust_viz = unlist(south_viz[8,c(2:17)])
south_neg_viz = unlist(south_viz[9,c(2:17)])
south_pos_viz = unlist(south_viz[10,c(2:17)])

boxplot(south_anger_viz, south_antic_viz, south_disgust_viz, south_fear_viz, 
        south_joy_viz, south_sad_viz, south_surprise_viz, south_trust_viz,
        south_neg_viz, south_pos_viz,
        col=c("maroon","orange","gray"), ylab = "Sentiment Score",
        names = c("Anger", "Anticipation", "Disgust", "Fear", "Joy", 
                  "Sadness", "Surprise", "Trust", "Negative", "Positive"),
        pch=16,  las = 2, main='Variability of Base Sentiment of Southern Bases')

#Midwest
midwest_viz <- left_join(sumSentsLeavenworth1, sumSentsCarson1, by = c("Label"="Label"))
midwest_viz <- left_join(midwest_viz, sumSentsLeanord1, by = c("Label"="Label"))
midwest_viz <- left_join(midwest_viz, sumSentsRiley1, by = c("Label"="Label"))
midwest_viz <- left_join(midwest_viz, sumSentsPueblo1, by = c("Label"="Label"))
midwest_viz <- left_join(midwest_viz, sumSentsRockisland1, by = c("Label"="Label"))

midwest_anger_viz = unlist(midwest_viz[1,c(2:7)])
midwest_antic_viz = unlist(midwest_viz[2,c(2:7)])
midwest_disgust_viz = unlist(midwest_viz[3,c(2:7)])
midwest_fear_viz = unlist(midwest_viz[4,c(2:7)])
midwest_joy_viz = unlist(midwest_viz[5,c(2:7)])
midwest_sad_viz = unlist(midwest_viz[6,c(2:7)])
midwest_surprise_viz = unlist(midwest_viz[7,c(2:7)])
midwest_trust_viz = unlist(midwest_viz[8,c(2:7)])
midwest_neg_viz = unlist(midwest_viz[9,c(2:7)])
midwest_pos_viz = unlist(midwest_viz[10,c(2:7)])

boxplot(midwest_anger_viz, midwest_antic_viz, midwest_disgust_viz, midwest_fear_viz, 
        midwest_joy_viz, midwest_sad_viz, midwest_surprise_viz, midwest_trust_viz,
        midwest_neg_viz, midwest_pos_viz,
        col=c("maroon","orange","gray"), ylab = "Sentiment Score",
        names = c("Anger", "Anticipation", "Disgust", "Fear", "Joy", 
                  "Sadness", "Surprise", "Trust", "Negative", "Positive"),
        pch=16,  las = 2, main='Variability of Base Sentiment of Midwestern Bases')


#West
west_viz <- left_join(sumSentsAnniston1, sumSentsDugway1, by = c("Label"="Label"))
west_viz <- left_join(west_viz, sumSentsGreely1, by = c("Label"="Label"))
west_viz <- left_join(west_viz, sumSentsHuachuca1, by = c("Label"="Label"))
west_viz <- left_join(west_viz, sumSentsIrwin1, by = c("Label"="Label"))
west_viz <- left_join(west_viz, sumSentsWainwright1, by = c("Label"="Label"))
west_viz <- left_join(west_viz, sumSentsJblm1, by = c("Label"="Label"))
west_viz <- left_join(west_viz, sumSentsPresidio1, by = c("Label"="Label"))
west_viz <- left_join(west_viz, sumSentsYuma1, by = c("Label"="Label"))

west_anger_viz = unlist(west_viz[1,c(2:10)])
west_antic_viz = unlist(west_viz[2,c(2:10)])
west_disgust_viz = unlist(west_viz[3,c(2:10)])
west_fear_viz = unlist(west_viz[4,c(2:10)])
west_joy_viz = unlist(west_viz[5,c(2:10)])
west_sad_viz = unlist(west_viz[6,c(2:10)])
west_surprise_viz = unlist(west_viz[7,c(2:10)])
west_trust_viz = unlist(west_viz[8,c(2:10)])
west_neg_viz = unlist(west_viz[9,c(2:10)])
west_pos_viz = unlist(west_viz[10,c(2:10)])

boxplot(west_anger_viz, west_antic_viz, west_disgust_viz, west_fear_viz, 
        west_joy_viz, west_sad_viz, west_surprise_viz, west_trust_viz,
        west_neg_viz, west_pos_viz,
        col=c("maroon","orange","gray"), ylab = "Sentiment Score",
        names = c("Anger", "Anticipation", "Disgust", "Fear", "Joy", 
                  "Sadness", "Surprise", "Trust", "Negative", "Positive"),
        pch=16,  las = 2, main='Variability of Base Sentiment of Western Bases')











# Create boxplots for the different Sentiment metrics of the different bases
final_viz <- left_join(sumSentsAberdeen1, sumSentsAnniston1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsBiggs1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsCarlisle1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsCorpus1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsDugway1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsAphill1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsBelvoir1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsBenning1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsBliss1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsBuchanan1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsCampbell1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsCarson1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsDetrick1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsDrum1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsEustis1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsGordon1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsGreely1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsHamilton1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsHuachuca1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsIrwin1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsJackson1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsKnox1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsLeavenworth1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsLee1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsLeanord1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsMeade1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsMeyer1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsPolk1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsRiley1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsStewart1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsWainwright1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsHunter1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsJblm1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsLetterkenny1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsPicatinny1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsPinebluff1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsPresidio1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsPueblo1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsRedriver1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsRockisland1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsSsc1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsToby1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsWalterreed1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsWatervliet1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsYuma1, by = c("Label" = "Label"))


anger_viz = unlist(final_viz[1,c(2:47)])
antic_viz = unlist(final_viz[2,c(2:47)])
disgust_viz = unlist(final_viz[3,c(2:47)])
fear_viz = unlist(final_viz[4,c(2:47)])
joy_viz = unlist(final_viz[5,c(2:47)])
sad_viz = unlist(final_viz[6,c(2:47)])
surprise_viz = unlist(final_viz[7,c(2:47)])
trust_viz = unlist(final_viz[8,c(2:47)])
neg_viz = unlist(final_viz[9,c(2:47)])
pos_viz = unlist(final_viz[10,c(2:47)])

boxplot(anger_viz, antic_viz, disgust_viz, fear_viz, joy_viz, sad_viz, 
        surprise_viz, trust_viz, neg_viz, pos_viz,
        col=c("maroon","orange","gray"), ylab = "Sentiment Score",
        names = c("Anger", "Anticipation", "Disgust", "Fear", "Joy", 
                  "Sadness", "Surprise", "Trust", "Negative", "Positive"),
        pch=16,  las = 2, main='Variability of Base Sentiment')
