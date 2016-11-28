library(dplyr)
library(stringr)
library(data.table)
library(ggplot2)
library(tm)

unoffarmycom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/unoffarmycommentsA.csv", stringsAsFactors = FALSE)
milfamcom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/milfamcommentsA.csv", stringsAsFactors = FALSE)
armyfamcom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/armyfamcommentsA.csv", stringsAsFactors = FALSE)
unoffmilcom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/unoffmilcommentsA.csv", stringsAsFactors = FALSE)
armybasescom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/armybasescommentsA.csv", stringsAsFactors = FALSE)
offarmycom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/offarmycommentsA.csv", stringsAsFactors = FALSE)

unoffarmycom$date <- as.Date(unoffarmycom$created_time)
milfamcom$date <- as.Date(milfamcom$created_time)
armyfamcom$date <- as.Date(armyfamcom$created_time)
unoffmilcom$date <- as.Date(unoffmilcom$created_time)
armybasescom$date <- as.Date(armybasescom$created_time)
offarmycom$date <- as.Date(offarmycom$created_time)

unoffarmycom$Comment <- iconv(unoffarmycom$Comment, "latin1", "ASCII", sub = "")
milfamcom$Comment <- iconv(milfamcom$Comment, "latin1", "ASCII", sub = "")
armyfamcom$Comment <- iconv(armyfamcom$Comment, "latin1", "ASCII", sub = "")
unoffmilcom$Comment <- iconv(unoffmilcom$Comment, "latin1", "ASCII", sub = "")
armybasescom$Comment <- iconv(armybasescom$Comment, "latin1", "ASCII", sub = "")
offarmycom$Comment <- iconv(offarmycom$Comment, "latin1", "ASCII", sub = "")

################################################################################
#intermediate functions that will be used in the final function hallelujah

pastecoms <- function(dt, tb){
  table <- tb %>% filter(date == dt)
  beg <- paste(table$Comment, collapse = " ")
  return(beg)
}

makecomvec <- function(datevec, tb){
  vec <- sapply(datevec, pastecoms, tb)
}

makecomframe <- function(table){
  table$Comment <- iconv(table$Comment, "latin1", "ASCII", sub = "")
  table$Comment <- tolower(table$Comment)
  table$Comment <- removePunctuation(table$Comment)
  uniqdates <- unique(table$date)
  comvec <- makecomvec(uniqdates, table)
  df <- data_frame(uniqdates, comvec)
  return(df)
}

makecomlist <- function(df){
  biglist <- sapply(df$comvec, str_split, " ")
  names(biglist) <- df$uniqdates
  return(biglist)
}

check <- function(vec, pattern){
  if(nchar(pattern) > 3){
    pattern <- paste0("^", pattern)
    return(sum(grepl(pattern,vec)))
  }
  else{
    return(sum(vec == pattern))
  }
}

countfunc <- function(vec, pattern){
  ratio <- check(vec, pattern)/length(vec)
  return(ratio)
}


wrapfunc <- function(pattern, list){
  temp <- list %>% lapply(countfunc, pattern)
}

nextlist <- function(list, dic){
  balls <- dic %>% lapply(wrapfunc, list)
  names(balls) <- dic
  return(balls)
}
  
nextstep <- function(pat, list){
    templist <- list[[pat]]
    pattern <- rep(pat, times = length(templist))
    date <- names(templist)
    ratio <- unlist(unname(templist))
    df <- data_frame(date, pattern, ratio)
    return(df)
}

finalframe <- function(list){
  nm <- names(list)
  newlist <- lapply(nm, nextstep, list)
  frame <- rbindlist(newlist)
}

#THIS IS THE FUNCTION NEEDED TO MAKE THE DATA FRAME USED IN TIME SERIES
    #df is the data frame of comments returned by makecommentsdf in FacebookCommentScraping.R
    #dic is a vector of words to count frequencies of
hallelujah <- function(df, dic){
  comframe <- makecomframe(df)
  comlist <- makecomlist(comframe)
  mynextlist <- nextlist(comlist, dic)
  lastdf <- finalframe(mynextlist)
  return(lastdf)
}

###########################################################################
#FOR WORDS IN YOUTUBE DICTIONARY

load("~/sdal/projects/dod_social_media/dictionary.RData")
mydic <- as.vector(as.character(dict[,1]))

allfb <- rbind(armybasescom, armyfamcom, milfamcom, offarmycom, unoffarmycom, unoffmilcom)
allfbfil <- allfb %>% filter(date > as.Date("2013-12-31"))
allfbcomfildic <- hallelujah(allfbfil, mydic) 
#write.csv(allfbcomfildic, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/allfbcomfildic.csv") 

fbdic <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/allfbcomfildic.csv", stringsAsFactors = FALSE)

temp <- fbdic %>% filter(pattern == "wear")
ggplot(temp) + 
           geom_line(aes(x = as.Date(date), y = ratio), color = "#3b5998") +
           labs(title = "wear", x = "Date", y = "Ratio") +
           theme_bw() + 
           theme(panel.border = element_blank(),
                 axis.text.x = element_text(angle = 45, hjust = 1, size = 4)) +
           scale_x_date(date_breaks = "1 month")
#########################################################
#FOR POLICY RELATED WORDS

allfb <- rbind(armybasescom, armyfamcom, milfamcom, offarmycom, unoffarmycom, unoffmilcom)
allfbfil <- allfb %>% filter(date > as.Date("2013-12-31"))
newvec <- c("privacy", "diversity", "grant", "homeowner", "language")
allfbnewseries <- hallelujah(allfbfil, newvec)
#write.csv(allfbnewseries, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/allfbnewseries.csv")

fbnew <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/allfbnewseries.csv", stringsAsFactors = FALSE)

for(i in 1:length(newvec)){   
  temp <- fbnew %>% filter(pattern == newvec[i])
  assign(x = newvec[i], ggplot(temp) + 
           geom_line(aes(x = as.Date(date), y = ratio), color = "#3b5998") +
           labs(title = as.character(newvec[i]), x = "Date", y = "Ratio") +
           theme_bw() + 
           theme(panel.border = element_blank(),
                 axis.text.x = element_text(angle = 45, hjust = 1, size = 4)) +
           scale_x_date(date_breaks = "1 month"))
}  
 







##################################

load("~/sdal/projects/dod_social_media/dictionary.RData") 
mydic <- as.vector(as.character(dict[,1])) 

regexit <- Vectorize(function(x){ 
  if(nchar(x) == 3){ 
    return(paste0("\\s+", x, "[^a-z]"))  
  } 
  else{
    return(paste0("\\s+", x)) 
  }
})

mydic <- regexit(mydic)

countit <- function(pattern, comvec){
  count <- sum(grepl(pattern, comvec))
} 

countcom <- function(dic, comvec){
  list <- dic %>% lapply(countit, comvec) 
  vec <- unlist(list)
  df <- data_frame(dic, vec)
  names(df) <- c("word", "count")
  df <- df %>% arrange(desc(count))
  return(df)
} 

unoffarmycom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/unoffarmycommentsA.csv", stringsAsFactors = FALSE)
milfamcom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/milfamcommentsA.csv", stringsAsFactors = FALSE)
armyfamcom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/armyfamcommentsA.csv", stringsAsFactors = FALSE)
unoffmilcom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/unoffmilcommentsA.csv", stringsAsFactors = FALSE)
armybasescom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/armybasescommentsA.csv", stringsAsFactors = FALSE)
offarmycom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/offarmycommentsA.csv", stringsAsFactors = FALSE)


unoffarmycom$date <- as.Date(unoffarmycom$created_time)
milfamcom$date <- as.Date(milfamcom$created_time)
armyfamcom$date <- as.Date(armyfamcom$created_time)
unoffmilcom$date <- as.Date(unoffmilcom$created_time)
armybasescom$date <- as.Date(armybasescom$created_time)
offarmycom$date <- as.Date(offarmycom$created_time)

unoffarmycom$Comment <- iconv(unoffarmycom$Comment, "latin1", "ASCII", sub = "")
milfamcom$Comment <- iconv(milfamcom$Comment, "latin1", "ASCII", sub = "")
armyfamcom$Comment <- iconv(armyfamcom$Comment, "latin1", "ASCII", sub = "")
unoffmilcom$Comment <- iconv(unoffmilcom$Comment, "latin1", "ASCII", sub = "")
armybasescom$Comment <- iconv(armybasescom$Comment, "latin1", "ASCII", sub = "")
offarmycom$Comment <- iconv(offarmycom$Comment, "latin1", "ASCII", sub = "")



