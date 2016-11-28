library(plyr)
library(dplyr)
library(tidyr)
library(tm)


#function to replace NAs with 0
replacena <- function(x){
  x[is.na(x)] <- 0
  return(x)
}

#function to make a data frame of word and count for all words (apart from stopwords) that appear in the comdf comments
    #comdf is the data frame returned by makecommentdf in FacebookCommentScraping.R
makefreq <- function(comdf){
  comCorpus <- Corpus(VectorSource(comdf$Comment))
  comCorpus <- tm_map(comCorpus, PlainTextDocument)
  comCorpus <- tm_map(comCorpus, content_transformer(tolower))
  comCorpus <- tm_map(comCorpus, removePunctuation)
  comCorpus <- tm_map(comCorpus, removeWords, stopwords('english'))
  tdm <- TermDocumentMatrix(comCorpus)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m), decreasing = TRUE)
  df <- data_frame(names(v),v)
  df$v <- as.numeric(df$v)
  names(df) <- c("word", "count")
  return(df)
}

#function to merge two tables
mymerge <- function(df1, df2){
  temp <- full_join(df1, df2, by = c("word" = "word"))
  temp$count.x <- temp$count.x %>% as.numeric() %>% replacena()
  temp$count.y <- temp$count.y %>% as.numeric() %>% replacena()
  temp <- temp %>% mutate(count = count.x + count.y) %>% dplyr::select(word, count)
  return(temp)
}

##############################################################
#ARMY BASES
armybasescom <- read.csv("C:/Users/Ryan/Documents/2016 Fall Semester/CMDA 4864/Project/Army_Fort_Knox.csv", stringsAsFactors = FALSE)
armybasescom$Comment <- iconv(armybasescom$Comment, "latin1", "ASCII", sub = "")

z1 <- armybasescom[1:50000,] %>% makefreq()
z2 <- armybasescom[50001:100000,] %>% makefreq()
z3 <- armybasescom[100001:150000,] %>% makefreq()
z4 <- armybasescom[150001:200000,] %>% makefreq()
z5 <- armybasescom[200001:248015,] %>% makefreq()

m1 <- mymerge(z1,z2)
m2 <- mymerge(m1,z3)
m3 <- mymerge(m2,z4)
m4 <- mymerge(m3,z5)
m4 <- m4 %>% arrange(desc(count))
m4 <- m4 %>% mutate(ratio = count/sum(count))
#write.csv(m4, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/armybasescomfreq.csv")


#UNOFFICIAL ARMY

# unoffarmycom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/unoffarmycommentsA.csv", stringsAsFactors = FALSE)
# unoffarmycom$Comment <- iconv(unoffarmycom$Comment, "latin1", "ASCII", sub = "")
# a1 <- unoffarmycom %>% makefreq() %>% 
#   arrange(desc(count)) %>%
#   mutate(ratio = count/sum(count))
#write.csv(a1, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/unoffarmycomfreq.csv")


#MILITARY FAMILY

# milfamcom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/milfamcommentsA.csv", stringsAsFactors = FALSE)
# milfamcom$Comment <- iconv(milfamcom$Comment, "latin1", "ASCII", sub = "")
# b1 <- milfamcom[1:50000,] %>% makefreq()
# b2 <- milfamcom[50001:100000,] %>% makefreq()
# b3 <- milfamcom[100001:150000,] %>% makefreq()
# b4 <- milfamcom[150001:200000,] %>% makefreq()
# b5 <- milfamcom[200001:250000,] %>% makefreq()
# b6 <- milfamcom[250001:300000,] %>% makefreq()
# b7 <- milfamcom[300001:350000,] %>% makefreq()
# b8 <- milfamcom[350001:400000,] %>% makefreq()
# b9 <- milfamcom[400001:450000,] %>% makefreq()
# b10 <- milfamcom[450001:500000,] %>% makefreq()
# b11 <- milfamcom[500001:550000,] %>% makefreq()
# b12 <- milfamcom[550001:600000,] %>% makefreq()
# b13 <- milfamcom[600001:650000,] %>% makefreq()
# b14 <- milfamcom[650001:700000,] %>% makefreq()
# b15 <- milfamcom[700001:741319,] %>% makefreq()

# n1 <- mymerge(b1,b2)
# n2 <- mymerge(n1,b3)
# n3 <- mymerge(n2,b4)
# n4 <- mymerge(n3,b5)
# n5 <- mymerge(n4,b6)
# n6 <- mymerge(n5,b7)
# n7 <- mymerge(n6,b8)
# n8 <- mymerge(n7,b9)
# n9 <- mymerge(n8,b10)
# n10 <- mymerge(n9,b11)
# n11 <- mymerge(n10,b12)
# n12 <- mymerge(n11,b13)
# n13 <- mymerge(n12,b14)
# n14 <- mymerge(n13,b15)
# n14 <- n14 %>% arrange(desc(count)) %>%
#   mutate(ratio = count/sum(count))
#write.csv(n14, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/milfamcomfreq.csv")


#ARMY FAMILY

# armyfamcom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/armyfamcommentsA.csv", stringsAsFactors = FALSE)
# armyfamcom$Comment <- iconv(armyfamcom$Comment, "latin1", "ASCII", sub = "")
# c1 <- armyfamcom[1:50000,] %>% makefreq()
# c2 <- armyfamcom[50001:100000,] %>% makefreq()
# c3 <- armyfamcom[100001:150000,] %>% makefreq()
# c4 <- armyfamcom[150001:200000,] %>% makefreq()
# c5 <- armyfamcom[200001:250000,] %>% makefreq()
# c6 <- armyfamcom[250001:300000,] %>% makefreq()
# c7 <- armyfamcom[300001:350000,] %>% makefreq()
# c8 <- armyfamcom[350001:400000,] %>% makefreq()
# c9 <- armyfamcom[400001:450000,] %>% makefreq()
# c10 <- armyfamcom[450001:500000,] %>% makefreq()
# c11 <- armyfamcom[500001:550000,] %>% makefreq()
# c12 <- armyfamcom[550001:567622,] %>% makefreq()

# o1 <- mymerge(c1,c2)
# o2 <- mymerge(o1,c3)
# o3 <- mymerge(o2,c4)
# o4 <- mymerge(o3,c5)
# o5 <- mymerge(o4,c6)
# o6 <- mymerge(o5,c7)
# o7 <- mymerge(o6,c8)
# o8 <- mymerge(o7,c9)
# o9 <- mymerge(o8,c10)
# o10 <- mymerge(o9,c11)
# o11 <- mymerge(o10,c12)
# o11 <- o11 %>% arrange(desc(count)) %>%
#   mutate(ratio = count/sum(count))
#write.csv(o11, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/armyfamcomfreq.csv")


#UNOFFICIAL MILITARY

# options(mc.cores=1)

# unoffmilcom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/unoffmilcommentsA.csv", stringsAsFactors = FALSE)
# unoffmilcom$Comment <- iconv(unoffmilcom$Comment, "latin1", "ASCII", sub = "")
# d1 <- unoffmilcom[1:50000,] %>% makefreq()
# d2 <- unoffmilcom[50001:100000,] %>% makefreq()
# d3 <- unoffmilcom[100001:150000,] %>% makefreq()
# d4 <- unoffmilcom[150001:200000,] %>% makefreq()
# d5 <- unoffmilcom[200001:250000,] %>% makefreq()
# d6 <- unoffmilcom[250001:300000,] %>% makefreq()
# d7 <- unoffmilcom[300001:350000,] %>% makefreq()
# d8 <- unoffmilcom[350001:400000,] %>% makefreq()
# d9 <- unoffmilcom[400001:450000,] %>% makefreq()
# d10 <- unoffmilcom[450001:500000,] %>% makefreq()
# d11 <- unoffmilcom[500001:550000,] %>% makefreq()
# d12 <- unoffmilcom[550001:600000,] %>% makefreq()
# d13 <- unoffmilcom[600001:625000,] %>% makefreq()
# d14 <- unoffmilcom[625001:650000,] %>% makefreq()
# d15 <- unoffmilcom[650001:675000,] %>% makefreq()
# d16 <- unoffmilcom[675001:700000,] %>% makefreq()

# d17 <- unoffmilcom[700001:725000,] %>% makefreq()
# d18 <- unoffmilcom[725001:750000,] %>% makefreq()
# d19 <- unoffmilcom[750001:775000,] %>% makefreq()
# d20 <- unoffmilcom[775001:800000,] %>% makefreq()
# d21 <- unoffmilcom[800001:825000,] %>% makefreq()
# d22 <- unoffmilcom[825001:850000,] %>% makefreq()
# d23 <- unoffmilcom[850001:875000,] %>% makefreq()
# d24 <- unoffmilcom[875001:900000,] %>% makefreq()
# d25 <- unoffmilcom[900001:925000,] %>% makefreq()
# d26 <- unoffmilcom[925001:950000,] %>% makefreq()
# d27 <- unoffmilcom[950001:975000,] %>% makefreq()
# d28 <- unoffmilcom[975001:1000000,] %>% makefreq()
# d29 <- unoffmilcom[1000001:1025000,] %>% makefreq()
# d30 <- unoffmilcom[1025001:1050000,] %>% makefreq()
# d31 <- unoffmilcom[1050001:1075000,] %>% makefreq()
# d32 <- unoffmilcom[1075001:1100000,] %>% makefreq()
# d33 <- unoffmilcom[1100001:1125000,] %>% makefreq()
# d34 <- unoffmilcom[1125001:1150000,] %>% makefreq()
# d35 <- unoffmilcom[1150001:1175000,] %>% makefreq()
# d36 <- unoffmilcom[1175001:1200000,] %>% makefreq()
# d37 <- unoffmilcom[1200001:1225000,] %>% makefreq()
# d38 <- unoffmilcom[1225001:1250000,] %>% makefreq()
# d39 <- unoffmilcom[1250001:1275000,] %>% makefreq()
# d40 <- unoffmilcom[1275001:1300000,] %>% makefreq()
# d41 <- unoffmilcom[1300001:1325000,] %>% makefreq()
# d42 <- unoffmilcom[1325001:1350000,] %>% makefreq()
# d43 <- unoffmilcom[1350001:1375000,] %>% makefreq()
# d44 <- unoffmilcom[1375001:1400000,] %>% makefreq()
# d45 <- unoffmilcom[1400001:1425000,] %>% makefreq()
# d46 <- unoffmilcom[1425001:1450000,] %>% makefreq()
# d47 <- unoffmilcom[1450001:1475000,] %>% makefreq()
# d48 <- unoffmilcom[1475001:1500000,] %>% makefreq()
# d49 <- unoffmilcom[1500001:1525000,] %>% makefreq()
# d50 <- unoffmilcom[1525001:1550000,] %>% makefreq()
# d51 <- unoffmilcom[1550001:1575000,] %>% makefreq()
# d52 <- unoffmilcom[1575001:1600000,] %>% makefreq()
# d53 <- unoffmilcom[1600001:1625000,] %>% makefreq()
# d54 <- unoffmilcom[1625001:1650000,] %>% makefreq()
# d55 <- unoffmilcom[1650001:1675000,] %>% makefreq()
# d56 <- unoffmilcom[1675001:1700000,] %>% makefreq()
# d57 <- unoffmilcom[1700001:1725000,] %>% makefreq()
# d58 <- unoffmilcom[1725001:1750000,] %>% makefreq()
# d59 <- unoffmilcom[1750001:1775000,] %>% makefreq()
# d60 <- unoffmilcom[1775001:1800000,] %>% makefreq()
# d61 <- unoffmilcom[1800001:1825000,] %>% makefreq()
# d62 <- unoffmilcom[1825001:1850000,] %>% makefreq()
# d63 <- unoffmilcom[1850001:1875000,] %>% makefreq()
# d64 <- unoffmilcom[1875001:1900000,] %>% makefreq()
# d65 <- unoffmilcom[1900001:1925000,] %>% makefreq()
# d66 <- unoffmilcom[1925001:1950000,] %>% makefreq()
# d67 <- unoffmilcom[1950001:1975000,] %>% makefreq()
# d68 <- unoffmilcom[1975001:2000000,] %>% makefreq()
# d69 <- unoffmilcom[2000001:2025000,] %>% makefreq()
# d70 <- unoffmilcom[2025001:2050000,] %>% makefreq()
# d71 <- unoffmilcom[2050001:2075000,] %>% makefreq()
# d72 <- unoffmilcom[2075001:2100000,] %>% makefreq()
# d73 <- unoffmilcom[2100001:2150000,] %>% makefreq()
# d74 <- unoffmilcom[2100001:2150000,] %>% makefreq()
# d75 <- unoffmilcom[2150001:2175000,] %>% makefreq()
# d76 <- unoffmilcom[2175001:2200000,] %>% makefreq()
# d77 <- unoffmilcom[2200001:2225000,] %>% makefreq()
# d78 <- unoffmilcom[2225001:2250000,] %>% makefreq()
# d79 <- unoffmilcom[2250001:2275000,] %>% makefreq()
# d80 <- unoffmilcom[2275001:2297587,] %>% makefreq()


# p1 <- mymerge(d1,d2)
# p2 <- mymerge(p1,d3)
# p3 <- mymerge(p2,d4)
# p4 <- mymerge(p3,d5)
# p5 <- mymerge(p4,d6)
# p6 <- mymerge(p5,d7)
# p7 <- mymerge(p6,d8)
# p8 <- mymerge(p7,d9)
# p9 <- mymerge(p8,d10)
# p10 <- mymerge(p9,d11)
# p11 <- mymerge(p10,d12)
# p12 <- mymerge(p11,d13)
# p13 <- mymerge(p12,d14)
# p14 <- mymerge(p13,d15)
# p15 <- mymerge(p14,d16)
# p16 <- mymerge(p15,d17)
# p17 <- mymerge(p16,d18)
# p18 <- mymerge(p17,d19)
# p19 <- mymerge(p18,d20)
# p20 <- mymerge(p19,d21)
# p21 <- mymerge(p20,d22)
# p22 <- mymerge(p21,d23)
# p23 <- mymerge(p22,d24)
# p24 <- mymerge(p23,d25)
# p25 <- mymerge(p24,d26)
# p26 <- mymerge(p25,d27)
# p27 <- mymerge(p26,d28)
# p28 <- mymerge(p27,d29)
# p29 <- mymerge(p28,d30)
# p30 <- mymerge(p29,d31)
# p31 <- mymerge(p30,d32)
# p32 <- mymerge(p31,d33)
# p33 <- mymerge(p32,d34)
# p34 <- mymerge(p33,d35)
# p35 <- mymerge(p34,d36)
# p36 <- mymerge(p35,d37)
# p37 <- mymerge(p36,d38)
# p38 <- mymerge(p37,d39)
# p39 <- mymerge(p38,d40)
# p40 <- mymerge(p39,d41)
# p41 <- mymerge(p40,d42)
# p42 <- mymerge(p41,d43)
# p43 <- mymerge(p42,d44)
# p44 <- mymerge(p43,d45)
# p45 <- mymerge(p44,d46)
# p46 <- mymerge(p45,d47)
# p47 <- mymerge(p46,d48)
# p48 <- mymerge(p47,d49)
# p49 <- mymerge(p48,d50)
# p50 <- mymerge(p49,d51)
# p51 <- mymerge(p50,d52)
# p52 <- mymerge(p51,d53)
# p53 <- mymerge(p52,d54)
# p54 <- mymerge(p53,d55)
# p55 <- mymerge(p54,d56)
# p56 <- mymerge(p55,d57)
# p57 <- mymerge(p56,d58)
# p58 <- mymerge(p57,d59)
# p59 <- mymerge(p58,d60)
# p60 <- mymerge(p59,d61)
# p61 <- mymerge(p60,d62)
# p62 <- mymerge(p61,d63)
# p63 <- mymerge(p62,d64)
# p64 <- mymerge(p63,d65)
# p65 <- mymerge(p64,d66)
# p66 <- mymerge(p65,d67)
# p67 <- mymerge(p66,d68)
# p68 <- mymerge(p67,d69)
# p69 <- mymerge(p68,d70)
# p70 <- mymerge(p69,d71)
# p71 <- mymerge(p70,d72)
# p72 <- mymerge(p71,d73)
# p73 <- mymerge(p72,d74)
# p74 <- mymerge(p73,d75)
# p75 <- mymerge(p74,d76)
# p76 <- mymerge(p75,d77)
# p77 <- mymerge(p76,d78)
# p78 <- mymerge(p77,d79)
# p79 <- mymerge(p78,d80)
# p79 <- p79 %>% arrange(desc(count)) %>%
#   mutate(ratio = count/sum(count))
#write.csv(p79, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/unoffmilcomfreq.csv")


#OFFICIAL ARMY

# offarmycom <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/offarmycommentsA.csv", stringsAsFactors = FALSE)
# offarmycom$Comment <- iconv(offarmycom$Comment, "latin1", "ASCII", sub = "")
# t1 <- offarmycom[1:50000,] %>% makefreq()
# t2 <- offarmycom[50001:100000,] %>% makefreq()
# t3 <- offarmycom[100001:150000,] %>% makefreq()
# t4 <- offarmycom[150001:200000,] %>% makefreq()
# t5 <- offarmycom[200001:250000,] %>% makefreq()
# t6 <- offarmycom[250001:300000,] %>% makefreq()
# t7 <- offarmycom[300001:350000,] %>% makefreq()
# t8 <- offarmycom[350001:400000,] %>% makefreq()
# t9 <- offarmycom[400001:450000,] %>% makefreq()
# t10 <- offarmycom[450001:500000,] %>% makefreq()
# t11 <- offarmycom[500001:550000,] %>% makefreq()
# t12 <- offarmycom[550001:600000,] %>% makefreq()
# t13 <- offarmycom[600001:650000,] %>% makefreq()
# t14 <- offarmycom[650001:700000,] %>% makefreq()
# t15 <- offarmycom[700001:750000,] %>% makefreq()
# t16 <- offarmycom[750001:800000,] %>% makefreq()
# t17 <- offarmycom[800001:850000,] %>% makefreq()
# t18 <- offarmycom[850001:900000,] %>% makefreq()
# t19 <- offarmycom[900001:950000,] %>% makefreq()
# t20 <- offarmycom[950001:1000000,] %>% makefreq()
# t21 <- offarmycom[1000001:1050000,] %>% makefreq()
# t22 <- offarmycom[1050001:1100000,] %>% makefreq()
# t23 <- offarmycom[1100001:1150000,] %>% makefreq()
# t24 <- offarmycom[1200001:1250000,] %>% makefreq()
# t25 <- offarmycom[1250001:1300000,] %>% makefreq()
# t26 <- offarmycom[1300001:1350000,] %>% makefreq()
# t27 <- offarmycom[1350001:1400000,] %>% makefreq()
# t28 <- offarmycom[1400001:1450000,] %>% makefreq()
# t29 <- offarmycom[1450001:1500000,] %>% makefreq()
# t30 <- offarmycom[1500001:1550000,] %>% makefreq()
# t31 <- offarmycom[1550001:1600000,] %>% makefreq()
# t32 <- offarmycom[1600001:1650000,] %>% makefreq()
# t33 <- offarmycom[1650001:1700000,] %>% makefreq()
# t34 <- offarmycom[1700001:1750000,] %>% makefreq()
# t35 <- offarmycom[1750001:1800000,] %>% makefreq()
# t36 <- offarmycom[1800001:1850000,] %>% makefreq()
# t37 <- offarmycom[1850001:1900000,] %>% makefreq()
# t38 <- offarmycom[1900001:1950000,] %>% makefreq()
# t39 <- offarmycom[1950001:2000000,] %>% makefreq()
# t40 <- offarmycom[2000001:2050000,] %>% makefreq()
# t41 <- offarmycom[2050001:2100000,] %>% makefreq()
# t42 <- offarmycom[2100001:2150000,] %>% makefreq()
# t43 <- offarmycom[2150001:2200000,] %>% makefreq()
# t44 <- offarmycom[2200001:2233472,] %>% makefreq()

# q1 <- mymerge(t1,t2)
# q2 <- mymerge(q1,t3)
# q3 <- mymerge(q2,t4)
# q4 <- mymerge(q3,t5)
# q5 <- mymerge(q4,t6)
# q6 <- mymerge(q5,t7)
# q7 <- mymerge(q6,t8)
# q8 <- mymerge(q7,t9)
# q9 <- mymerge(q8,t10)
# q10 <- mymerge(q9,t11)
# q11 <- mymerge(q10,t12)
# q12 <- mymerge(q11,t13)
# q13 <- mymerge(q12,t14)
# q14 <- mymerge(q13,t15)
# q15 <- mymerge(q14,t16)
# q16 <- mymerge(q15,t17)
# q17 <- mymerge(q16,t18)
# q18 <- mymerge(q17,t19)
# q19 <- mymerge(q18,t20)
# q20 <- mymerge(q19,t21)
# q21 <- mymerge(q20,t22)
# q22 <- mymerge(q21,t23)
# q23 <- mymerge(q22,t24)
# q24 <- mymerge(q23,t25)
# q25 <- mymerge(q24,t26)
# q26 <- mymerge(q25,t27)
# q27 <- mymerge(q26,t28)
# q28 <- mymerge(q27,t29)
# q29 <- mymerge(q28,t30)
# q30 <- mymerge(q29,t31)
# q31 <- mymerge(q30,t32)
# q32 <- mymerge(q31,t33)
# q33 <- mymerge(q32,t34)
# q34 <- mymerge(q33,t35)
# q35 <- mymerge(q34,t36)
# q36 <- mymerge(q35,t37)
# q37 <- mymerge(q36,t38)
# q38 <- mymerge(q37,t39)
# q39 <- mymerge(q38,t40)
# q40 <- mymerge(q39,t41)
# q41 <- mymerge(q40,t42)
# q42 <- mymerge(q41,t43)
# q43 <- mymerge(q42,t44)
# q43 <- q43 %>% arrange(desc(count)) %>%
#   mutate(ratio = count/sum(count))
#write.csv(q43, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/offarmycomfreq.csv")

#######################################################################################

# offarmycomfreq <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/offarmycomfreq.csv", stringsAsFactors = FALSE)
# offarmycomfreq$offarmyratio <- offarmycomfreq$ratio
# offarmycomfreq <- offarmycomfreq %>% dplyr::select(word, offarmyratio)
# 
# armyfamcomfreq <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/armyfamcomfreq.csv", stringsAsFactors = FALSE)
# armyfamcomfreq$armyfamratio <- armyfamcomfreq$ratio
# armyfamcomfreq <- armyfamcomfreq %>% dplyr::select(word, armyfamratio)
# 
# milfamcomfreq <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/milfamcomfreq.csv", stringsAsFactors = FALSE)
# milfamcomfreq$milfamratio <- milfamcomfreq$ratio
# milfamcomfreq <- milfamcomfreq %>% dplyr::select(word, milfamratio)
# 
# unoffarmycomfreq <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/unoffarmycomfreq.csv", stringsAsFactors = FALSE)
# unoffarmycomfreq$unoffarmyratio <- unoffarmycomfreq$ratio
# unoffarmycomfreq <- unoffarmycomfreq %>% dplyr::select(word, unoffarmyratio)

armybasescomfreq <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/armybasescomfreq.csv", stringsAsFactors = FALSE)
armybasescomfreq$armybasesratio <- armybasescomfreq$ratio
armybasescomfreq <- armybasescomfreq %>% dplyr::select(word, armybasesratio)

# unoffmilcomfreq <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/unoffmilcomfreq.csv", stringsAsFactors = FALSE)
# unoffmilcomfreq$unoffmilratio <- unoffmilcomfreq$ratio
# unoffmilcomfreq <- unoffmilcomfreq %>% dplyr::select(word, unoffmilratio)

#make table including all groups
# w1 <- inner_join(offarmycomfreq, armyfamcomfreq, by = c("word" = "word"))
# w2 <- inner_join(w1, milfamcomfreq, by = c("word" = "word"))
# w3 <- inner_join(w2, unoffarmycomfreq, by = c("word" = "word"))
# w4 <- inner_join(w3, armybasescomfreq, by = c("word" = "word"))
# final <- inner_join(w4, unoffmilcomfreq, by = c("word" = "word"))
# final <- final %>% dplyr::select(word, offarmyratio, armyfamratio, milfamratio, unoffarmyratio, armybasesratio, unoffmilratio)
#write.csv(final, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/topicstable.csv")

#keep only words related to policy
# topicstable <- read.csv("/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/topicstable.csv", stringsAsFactors = FALSE) %>%
#   dplyr::select(-X)
# topicstable <- topicstable %>% filter(word %in% c("army", "god", "soldier", "love", "service", "safe", "military", "home", 
#                                                   "training", "country", "proud", "job", "family", "airborne", "work", "life",
#                                                   "join", "help", "son", "future", "school", "world", "troops", "infantry", "bct",
#                                                    "war", "combat", "women"))

#make table into format needed for radar chart
hope <- topicstable %>% gather(score, value, -word)
hope$family <- "words"
hope$item <- hope$word
hope <- hope %>% dplyr::select(family, item, score, value)

#function to create radar chart
polarHistogram<-function(
  df,
  binSize=1,
  spaceItem=0.2,
  spaceFamily=1.2,
  innerRadius=0.3,
  outerRadius=1,
  guides=c(10,20,40,80),
  alphaStart=-0.3,
  circleProportion=0.8,
  direction="inwards",
  familyLabels=FALSE){
  
  # ordering
  df<-arrange(df,family,item,score)
  
  # summing up to one
  # TO DO: replace NA by 0 because cumsum doesn't ignore NA's.
  df<-ddply(df,.(family,item),transform,value=cumsum(value/(sum(value))))
  
  # getting previous value
  df<-ddply(df,.(family,item),transform,previous=c(0,head(value,length(value)-1)))
  
  # family and item indices. There must be a better way to do this
  df2<-ddply(df,.(family,item),summarise,indexItem=1)
  df2$indexItem<-cumsum(df2$indexItem)
  df3<-ddply(df,.(family),summarise,indexFamily=1)
  df3$indexFamily<-cumsum(df3$indexFamily)
  
  df<-merge(df,df2,by=c("family",'item'))
  df<-merge(df,df3,by="family")
  
  df<-arrange(df,family,item,score)
  
  # define the bins
  # linear projection  
  affine<-switch(direction,
                 'inwards'= function(y) (outerRadius-innerRadius)*y+innerRadius,
                 'outwards'=function(y) (outerRadius-innerRadius)*(1-y)+innerRadius,
                 stop(paste("Unknown direction")))
  
  df<-within(df,{
    xmin<-(indexItem-1)*binSize+(indexItem-1)*spaceItem+(indexFamily-1)*(spaceFamily-spaceItem)
    xmax<-xmin+binSize
    ymin<-affine(1-previous)
    ymax<-affine(1-value)
  }
  )
  
  # build the guides
 
  
  
  # Building the ggplot object
  
  totalLength<-tail(df$xmin+binSize+spaceFamily,1)/circleProportion-0
  
  # histograms
  p<-ggplot(df)+geom_rect(
    aes(
      xmin=xmin,
      xmax=xmax,
      ymin=ymin,
      ymax=ymax,
      fill=score)
  )
  
  # item labels
  readableAngle<-function(x){
    angle<-x*(-360/totalLength)-alphaStart*180/pi+90
    angle+ifelse(sign(cos(angle*pi/180))+sign(sin(angle*pi/180))==-2,180,0)
  }
  readableJustification<-function(x){
    angle<-x*(-360/totalLength)-alphaStart*180/pi+90
    ifelse(sign(cos(angle*pi/180))+sign(sin(angle*pi/180))==-2,1,0)
  }
  
  dfItemLabels<-ddply(df,.(item),summarize,xmin=xmin[1])
  dfItemLabels<-within(dfItemLabels,{
    x<-xmin+binSize/2
    angle<-readableAngle(xmin+binSize/2)
    hjust<-readableJustification(xmin+binSize/2)
  })
  
  p<-p+geom_text(
    aes(
      x=x,
      label=item,
      angle=angle,
      hjust=hjust),
    y=1.02,
    size=6,
    vjust=0.5,
    data=dfItemLabels)
  
  # guides  

  

  
  # family labels
  if(familyLabels){
    #     familyLabelsDF<-ddply(df,.(family),summarise,x=mean(xmin+binSize),angle=mean(xmin+binSize)*(-360/totalLength)-alphaStart*180/pi)
    familyLabelsDF<-aggregate(xmin~family,data=df,FUN=function(s) mean(s+binSize))
    familyLabelsDF<-within(familyLabelsDF,{
      x<-xmin
      angle<-xmin*(-360/totalLength)-alphaStart*180/pi
    })
    
    p<-p+geom_text(
      aes(
        x=x,
        label=family,
        angle=angle),
      data=familyLabelsDF,
      y=1.2)
  }  
  #   # empty background and remove guide lines, ticks and labels
  p<-p+theme(
    panel.background=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank()
  )
  
  # x and y limits
  p<-p+xlim(0,tail(df$xmin+binSize+spaceFamily,1)/circleProportion)
  p<-p+ylim(0,outerRadius+0.2)
  
  # project to polar coordinates
  p<-p+coord_polar(start=alphaStart)
  
  # nice colour scale
  p<-p+scale_fill_brewer(palette='Set3',type='qual')
  
  p
}


#CREATE POLAR HISTOGRAM
p<-polarHistogram(hope,familyLabel=FALSE, guides = NULL, direction = "outwards")
print(p)

