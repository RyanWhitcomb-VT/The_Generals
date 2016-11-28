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
#make Facebook sentiment table for radar chart

#unoffarmycom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/unoffarmycommentsA.csv", stringsAsFactors = FALSE)
#milfamcom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/milfamcommentsA.csv", stringsAsFactors = FALSE)
#armyfamcom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/armyfamcommentsA.csv", stringsAsFactors = FALSE)
#unoffmilcom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/unoffmilcommentsA.csv", stringsAsFactors = FALSE)
knox <- read.csv("C:/Users/Ryan/Documents/2016 Fall Semester/CMDA 4864/Project/Army_Fort_Knox.csv", stringsAsFactors = FALSE)
#offarmycom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/offarmycommentsA.csv", stringsAsFactors = FALSE)

#unoffarmycom$date <- as.Date(unoffarmycom$created_time)
#milfamcom$date <- as.Date(milfamcom$created_time)
#armyfamcom$date <- as.Date(armyfamcom$created_time)
#unoffmilcom$date <- as.Date(unoffmilcom$created_time)
knox$date <- as.Date(knox$created_time)
#offarmycom$date <- as.Date(offarmycom$created_time)

#unoffarmycom$Comment <- iconv(unoffarmycom$Comment, "latin1", "ASCII", sub = "")
#milfamcom$Comment <- iconv(milfamcom$Comment, "latin1", "ASCII", sub = "")
#armyfamcom$Comment <- iconv(armyfamcom$Comment, "latin1", "ASCII", sub = "")
#unoffmilcom$Comment <- iconv(unoffmilcom$Comment, "latin1", "ASCII", sub = "")
knox$Comment <- iconv(knox$Comment, "latin1", "ASCII", sub = "")
#offarmycom$Comment <- iconv(offarmycom$Comment, "latin1", "ASCII", sub = "")

allfb <- rbind(knox)
fbcomments <- allfb$Comment
fbcomments <- toUTF8(fbcomments)

sentimentsFB <- get_nrc_sentiment(fbcomments)

sumSentsFB <- as.data.frame(colSums(sentimentsFB))

sumSentsFB$Names <- row.names(sumSentsFB)

colnames(sumSentsFB) <- c("Sums","Type")

sumSentsFB1 <- sumSentsFB[2:1]
save(sumSentsFB1, file = "sumsentsfb.RData")

#########################################################
#make Youtube sentiment table for radar chart

youtube <- read.csv("/home/sdal/projects/dod_social_media/YouTube/alltexts.csv", stringsAsFactors = FALSE)
youtube$Comment <- iconv(youtube$Comment, "latin1", "ASCII", sub = "")
ytcomments <- youtube$Comment
ytcomments <- toUTF8(ytcomments)
sentimentsYT <- get_nrc_sentiment(ytcomments)

sumSentsYT <- as.data.frame(colSums(sentimentsYT))

sumSentsYT$Names <- row.names(sumSentsYT)

colnames(sumSentsYT) <- c("Sums","Type")


sumSentsYT1 <- sumSentsYT[2:1]
save(sumSentsYT1, file = "sumsentsyoutube.RData")

###########################################################
#MAKE SENTIMENT RADAR CHART

load("/home/willhs/sdal/projects/dod_social_media/sumsentsyoutube.RData")
sumSentsYT1 <- sumSentsYT1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsYT1) <- c("Label", "Youtube")

load("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/sumsentsfb.RData")
sumSentsFB1 <- sumSentsFB1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsFB1) <- c("Label", "Facebook")

load("/home/willhs/sdal/projects/dod_social_media/Poster/Twitter/Data/twittersents.RData")
sumSentstot1 <- sumSentstot1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentstot1) <- c("Label", "Twitter")

df <- left_join(sumSentsYT1, sumSentsFB1, by = c("Label" = "Label"))
df1 <- left_join(df, sumSentstot1, by = c("Label" = "Label"))
colors <- as.matrix(data.frame(c(204,24,30),c(59,89,152),c(0,172,237)))
chartJSRadar(scores = df1,labelSize = 60, alpha = 11, polyAlpha = 1, colMatrix = colors, addDots = FALSE)

