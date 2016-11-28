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

colors <- as.matrix(data.frame(c(204,24,30)))
chartJSRadar(scores = sumSentsBelvoir1,labelSize = 60, alpha = 11, polyAlpha = 1, colMatrix = colors, addDots = FALSE)

# Fort Benning
benning <- read.csv("C:/Users/Ryan/Documents/2016 Fall Semester/CMDA 4864/Project/Sentiment Radar Chart/Army_Fort_Benning.csv", stringsAsFactors = FALSE)
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

colors <- as.matrix(data.frame(c(204,24,30)))
chartJSRadar(scores = sumSentsBenning1,labelSize = 60, alpha = 11, polyAlpha = 1, colMatrix = colors, addDots = FALSE)

# Fort Campbell
campbell <- read.csv("C:/Users/Ryan/Documents/2016 Fall Semester/CMDA 4864/Project/Sentiment Radar Chart/Army_Fort_Campbell.csv", stringsAsFactors = FALSE)
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

colors <- as.matrix(data.frame(c(204,24,30)))
chartJSRadar(scores = sumSentsCampbell1,labelSize = 60, alpha = 11, polyAlpha = 1, colMatrix = colors, addDots = FALSE)

# Fort Carson
carson <- read.csv("C:/Users/Ryan/Documents/2016 Fall Semester/CMDA 4864/Project/Sentiment Radar Chart/Army_Fort_Carson.csv", stringsAsFactors = FALSE)
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
save(sumSentsCarson1, file = "sumsentscarson.RData")

sumSentsCarson1 <- sumSentsCarson1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsCarson1) <- c("Label", "Carson")

colors <- as.matrix(data.frame(c(204,24,30)))
chartJSRadar(scores = sumSentsCampbell1,labelSize = 60, alpha = 11, polyAlpha = 1, colMatrix = colors, addDots = FALSE)

# Fort Huachuca
huachuca <- read.csv("C:/Users/Ryan/Documents/2016 Fall Semester/CMDA 4864/Project/Sentiment Radar Chart/Army_Fort_Huachuca.csv", stringsAsFactors = FALSE)
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

colors <- as.matrix(data.frame(c(204,24,30)))
chartJSRadar(scores = sumSentsHuachuca1,labelSize = 60, alpha = 11, polyAlpha = 1, colMatrix = colors, addDots = FALSE)

# Fort Knox
knox <- read.csv("C:/Users/Ryan/Documents/2016 Fall Semester/CMDA 4864/Project/Sentiment Radar Chart/Army_Fort_Knox.csv", stringsAsFactors = FALSE)
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
save(sumSentsKnox1, file = "sumsentsknox.RData")

sumSentsKnox1 <- sumSentsKnox1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsKnox1) <- c("Label", "Knox")

colors <- as.matrix(data.frame(c(204,24,30)))
chartJSRadar(scores = sumSentsKnox1,labelSize = 60, alpha = 11, polyAlpha = 1, colMatrix = colors, addDots = FALSE)

# Fort Lee
lee <- read.csv("C:/Users/Ryan/Documents/2016 Fall Semester/CMDA 4864/Project/Sentiment Radar Chart/Army_Fort_Lee.csv", stringsAsFactors = FALSE)
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
save(sumSentsLee1, file = "sumsentslee.RData")

sumSentsLee1 <- sumSentsLee1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsLee1) <- c("Label", "Lee")

colors <- as.matrix(data.frame(c(204,24,30)))
chartJSRadar(scores = sumSentsLee1,labelSize = 60, alpha = 11, polyAlpha = 1, colMatrix = colors, addDots = FALSE)

# Fort Leonard Wood
wood <- read.csv("C:/Users/Ryan/Documents/2016 Fall Semester/CMDA 4864/Project/Sentiment Radar Chart/Army_Fort_Leonard_Wood.csv", stringsAsFactors = FALSE)
wood$date <- as.Date(wood$created_time)
wood$Comment <- iconv(wood$Comment, "latin1", "ASCII", sub = "")

wood2 <- rbind(wood)
woodcomments <- wood2$Comment
woodcomments <- toUTF8(woodcomments)

sentimentsWood <- get_nrc_sentiment(woodcomments)

sumSentsWood <- as.data.frame(colSums(sentimentsWood))

sumSentsWood$Names <- row.names(sumSentsWood)

colnames(sumSentsWood) <- c("Sums","Type")

sumSentsWood1 <- sumSentsWood[2:1]
save(sumSentsWood1, file = "sumsentsWood.RData")

sumSentsWood1 <- sumSentsWood1 %>% mutate(norm = Sums/max(Sums)) %>% dplyr::select(Type, norm)
names(sumSentsWood1) <- c("Label", "Wood")

colors <- as.matrix(data.frame(c(204,24,30)))
chartJSRadar(scores = sumSentsWood1,labelSize = 60, alpha = 11, polyAlpha = 1, colMatrix = colors, addDots = FALSE)

# Combine the three selected together into one graph
df <- left_join(sumSentsKnox1, sumSentsCarson1, by = c("Label" = "Label"))
df1 <- left_join(df, sumSentsWood1, by = c("Label" = "Label"))
colors <- as.matrix(data.frame(c(204,24,0),c(59,89,254),c(0,172,26)))
chartJSRadar(scores = df1,labelSize = 60, alpha = 11, polyAlpha = 1, colMatrix = colors, addDots = FALSE)

# Create boxplots for the different Sentiment metrics of the different bases
final_viz <- left_join(final_viz, sumSentsCampbell1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsCarson1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsHuachuca1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsKnox1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsLee1, by = c("Label" = "Label"))
final_viz <- left_join(final_viz, sumSentsWood1, by = c("Label" = "Label"))


anger_viz = unlist(final_viz[1,c(2:9)])
antic_viz = unlist(final_viz[2,c(2:9)])
disgust_viz = unlist(final_viz[3,c(2:9)])
fear_viz = unlist(final_viz[4,c(2:9)])
joy_viz = unlist(final_viz[5,c(2:9)])
sad_viz = unlist(final_viz[6,c(2:9)])
surprise_viz = unlist(final_viz[7,c(2:9)])
trust_viz = unlist(final_viz[8,c(2:9)])
neg_viz = unlist(final_viz[9,c(2:9)])
pos_viz = unlist(final_viz[10,c(2:9)])

boxplot(anger_viz, antic_viz, disgust_viz, fear_viz, joy_viz, sad_viz, 
        surprise_viz, trust_viz, neg_viz, pos_viz,
        col=c("maroon","orange","gray"), ylab = "Sentiment Score",
        names = c("Anger", "Anticipation", "Disgust", "Fear", "Joy", 
                  "Sadness", "Surprise", "Trust", "Negative", "Positive"),
        pch=16,  las = 2, main='Variability of Base Sentiment')

# Make Scatter Plot of Joy and Negative sentiment
par(mar=c(7,4,4,2))
plot(neg_viz,pch=16, ylim = c(.15,.75), las = 2, col = "red", 
     ylab = "Sentiment Score", xlab=" ", main = "Base Sentiment Comparisons", xaxt="n")
axis(1, at=1:8, labels=c("Belvoir", "Benning", "Campbell", "Carson", "Huachuca", 
               "Knox", "Lee", "Leonard-Wood"), las = 2)
points(joy_viz, pch=16, col="Blue")
