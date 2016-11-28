#THIS IS THE FILE FOR SCRAPING THE FACEBOOK PAGE POSTS

library(Rfacebook)
library(dplyr)


#initialize variable for Facebook API App token
tokenapp <- #PUT YOUR APP TOKEN HERE

#number of posts to scrape per day
numposts <- 40

#function to scrape posts and make data frame
    #begdate is the earliest date you want to scrape posts from
          #can be a date before the page even existed; errors will not cause the loop to break
    #enddate is the latest date you want to scrape posts from
          #can be a date after the present date; errors will not cause the loop to break
    #page is the tag (not name) of the page you want to scrape
    #token is your Facebook App token
    #numposts is the number of posts to scrape each day
getAllPosts <- function(begdate, enddate, page, token, numposts){ 
  
  seq <- seq(as.Date(begdate), as.Date(enddate), by = "days")
  df <- data.frame()
  pb <- txtProgressBar(min = 2, max = length(seq), char = "/", style = 3)
  
  for(i in 2:length(seq)){
    setTxtProgressBar(pb, i)
    tryCatch({
      beg <- seq[i-1]
      end <- seq[i]
      
      temptable <- getPage(page, tokenapp, numposts, since = beg, until = end)
      df <- rbind(df, temptable)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  } 
  close(pb) 
  df <- df %>% arrange(desc(created_time))
  return(df)
}


#################################################
#pages grouped by category:

officialarmy <- c("USarmy", 
                  "goarmy", 
                  "WestPointUSMA", 
                  "ArmyChaplainCorps", 
                  "USACEHQ", 
                  "combinedarmsdoctrinedirectorate", 
                  "ArmyFutureSoldierCenter")
armybases <- c("USArmyFortCarson",
               "FortCampbell",
               "u.s.armyforthuachuca",
               "fortbenningfans",
               "ArmyFortLee",
               "fortleonardwoodmissouri",
               "FortKnoxKY",
               "fortbelvoir")
unofficialarmy<- c("CsaSoldierForLife")
unofficialmilitary <- c("terminallance",
                        "DoctrineMan",
                        "Militarydotcom",
                        "militarynetwork")
armyfamily <- c("goarmyparents",
                "usarmyfuturesoldierfamily",
                "FamilyMWR",
                "ArmyReserveFamilyPrograms",
                "ArmyCSF2")
militaryfamily <- c("militaryfamily",
                    "NMSNetwork",
                    "TAPSorg",
                    "bluestarmothers",
                    "wearyellowribbon",
                    "for.military.wives",
                    "I.have.someone.in.MILITARY",
                    "MilitaryFamilySupportGroupMFSG",
                    "SupportingOurVeterans")

###############################################################################
#SCRAPING

#OFFICIAL ARMY

#USarmy <- getAllPosts("2011-01-01", "2016-07-06", "USarmy", token = tokenapp, numposts)
#write.csv(USarmy, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/USarmy2016_07_06.csv")

#goarmy <- getAllPosts("1999-01-01", "2016-07-06", "goarmy", tokenapp, numposts)
#write.csv(goarmy, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/goarmy2016_07_06.csv")

#WPUSMA <- getAllPosts("2010-01-01", "2016-07-06", "WestPointUSMA", tokenapp, numposts)
#write.csv(WPUSMA, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/WPUSMA2016_07_06.csv")

#ACC <- getAllPosts("2010-06-01", "2016-07-06", "ArmyChaplainCorps", tokenapp, numposts)
#write.csv(ACC, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/ACC2016_07_06.csv")

#USACEHQ <- getAllPosts("2010-06-01", "2016-07-06", "USACEHQ", tokenapp, numposts)
#write.csv(USACEHQ, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/USACEHQ2016_07_06.csv")

#cadd <- getAllPosts("2015-06-01", "2016-07-06", "combinedarmsdoctrinedirectorate", tokenapp, numposts)
#write.csv(cadd, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/cadd2016_07_06.csv")

#afsc <- getAllPosts("2010-01-01", "2016-07-06", "ArmyFutureSoldierCenter", tokenapp, 50)
#write.csv(afsc, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/afsc2016_07_06.csv")

####################################################

#ARMY BASES

#usafc <- getAllPosts("2011-06-01", "2016-07-06", "USArmyFortCarson", tokenapp, numposts)
#write.csv(usafc, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/usafc2016_07_06.csv")

#fc <- getAllPosts("2009-08-01", "2016-07-06", "FortCampbell", tokenapp, numposts)
#write.csv(fc, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/fc2016_07_06.csv")

#usafh <- getAllPosts("2009-06-01", "2016-07-06", "u.s.armyforthuachuca", tokenapp, numposts)
#write.csv(usafh, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/usafh2016_07_06.csv")

#fbf <- getAllPosts("2009-06-01", "2016-07-06", "fortbenningfans", tokenapp, numposts)
#write.csv(fbf, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/fbf2016_07_06.csv")

#afl <- getAllPosts("2010-01-01", "2016-07-06", "ArmyFortLee", tokenapp, numposts)
#write.csv(afl, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/afl2016_07_06.csv")

#flwm <- getAllPosts("2009-06-01", "2016-07-06", "fortleonardwoodmissouri", tokenapp, numposts)
#write.csv(flwm, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/flwm2016_07_06.csv")

#fkky <- getAllPosts("2005-01-01", "2016-07-06", "FortKnoxKY", tokenapp, numposts)
#write.csv(fkky, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/fkky2016_07_06.csv")

#fb <- getAllPosts("2005-01-01", "2016-07-06", "fortbelvoir", tokenapp, numposts)
#write.csv(fb, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/fb2016_07_06.csv")

####################################################

#UNOFFICIAL ARMY

#csasfl <- getAllPosts("2005-01-01", "2016-07-06", "CsaSoldierForLife", tokenapp, numposts)
#write.csv(csasfl, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/csasfl2016_07_06.csv")

####################################################

#UNOFFICIAL MILITARY

#tl <- getAllPosts("2005-01-01", "2016-07-06", "terminallance", tokenapp, numposts)
#write.csv(tl, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/tl2016_07_06.csv")

#dm <- getAllPosts("2005-01-01", "2016-07-06", "DoctrineMan", tokenapp, numposts)
#write.csv(dm, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/dm2016_07_06.csv")

#mdc <- getAllPosts("2005-01-01", "2016-07-06", "Militarydotcom", tokenapp, numposts)
#write.csv(mdc, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/mdc2016_07_06.csv")

#mn <- getAllPosts("2005-01-01", "2016-07-06", "militarynetwork", tokenapp, numposts)
#write.csv(mn, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/mn2016_07_06.csv")

####################################################

#ARMY FAMILY

#gap <- getAllPosts("2009-01-01", "2016-07-06", "goarmyparents", tokenapp, numposts)
#write.csv(gap, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/gap2016_07_06.csv")

#usafsf <- getAllPosts("2011-03-01", "2016-07-06", "usarmyfuturesoldierfamily", tokenapp, numposts)
#write.csv(usafsf, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/usafsf2016_07_06.csv")

#FMWR <- getAllPosts("2005-01-01", "2016-07-06", "FamilyMWR", tokenapp, numposts)
#write.csv(FMWR, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/FMWR2016_07_06.csv")

#ARFP <- getAllPosts("2005-01-01", "2016-07-06", "ArmyReserveFamilyPrograms", tokenapp, numposts)
#write.csv(ARFP, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/ARFP2016_07_06.csv")

#ACSF2 <- getAllPosts("2005-01-01", "2016-07-06", "ArmyCSF2", tokenapp, numposts)
#write.csv(ACSF2, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/ACSF22016_07_06.csv")

####################################################

#MILITARY FAMILY

#mf <- getAllPosts("2005-01-01", "2016-07-06", "militaryfamily", tokenapp, numposts)
#write.csv(mf, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/mf2016_07_06.csv")

#NMSN <- getAllPosts("2005-01-01", "2016-07-06", "NMSNetwork", tokenapp, numposts)
#write.csv(NMSN, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/NMSN2016_07_06.csv")

#TAPS <- getAllPosts("2005-01-01", "2016-07-06", "TAPSorg", tokenapp, numposts)
#write.csv(TAPS, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/TAPS2016_07_06.csv")

#bsm <- getAllPosts("2005-01-01", "2016-07-06", "bluestarmothers", tokenapp, numposts)
#write.csv(bsm, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/bsm2016_07_06.csv")

#wyr <- getAllPosts("2012-01-01", "2016-07-06", "wearyellowribbon", tokenapp, numposts)
#write.csv(wyr, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/wyr2016_07_06.csv")

#fmw <- getAllPosts("2005-01-01", "2016-07-06", "for.military.wives", tokenapp, numposts)
#write.csv(fmw, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/fmw2016_07_06.csv")

#IhsiM <- getAllPosts("2005-01-01", "2016-07-06", "I.have.someone.in.MILITARY", tokenapp, numposts)
#write.csv(IhsiM, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/IhsiM2016_07_06.csv")

#MFSG <- getAllPosts("2005-01-01", "2016-07-06", "MilitaryFamilySupportGroupMFSG", tokenapp, numposts)
#write.csv(MFSG, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/MFSG2016_07_06.csv")

#SOV <- getAllPosts("2005-01-01", "2010-07-06", "SupportingOurVeterans", tokenapp, numposts)
#write.csv(SOV, "/home/willhs/sdal/projects/dod_social_media/Poster/Facebook/Data/SOV2016_07_06.csv")

####################################################








