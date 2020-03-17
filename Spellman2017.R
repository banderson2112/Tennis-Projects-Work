setwd("~/GoogleDrive/Spellman2017")

#read in BerdychvsRHnis match data
BerdychvsRH <- read_csv("~/GoogleDrive/Spellman2017/BerdychvsRH.csv")
#new package
library(plyr)

#stratify points into two sides
BerdychvsRH$Deuce=rep(0,nrow(BerdychvsRH))
BerdychvsRH$Ad=rep(0,nrow(BerdychvsRH))


#Shortcut around deuce match id bullshit
deuce <- c("0-0", "15-15", "30-0", "40-15", "0-30", "15-40", "30-30", "40-40")
ad <- c("15-0","0-15","30-15","15-30","40-0","0-40","40-30","30-40","AD-40","40-AD")

#USE THIS***

BerdychvsRH$Deuce<-ifelse(BerdychvsRH$Pts %in% deuce,1,0)
BerdychvsRH$Ad<-ifelse(BerdychvsRH$Pts %in% ad,1,0)


#New column using substring to get 1st and 2nd values
BerdychvsRH$FirstShot<-substr(BerdychvsRH$rallyNoDirection,start=1,stop=1)
#First two shots
BerdychvsRH$First2Shots<-substr(BerdychvsRH$rallyNoDirection, start=1,stop=2)
#Serve Location
#1st
BerdychvsRH$FirstServeLocation<-substr(BerdychvsRH$X1stNoLet,start=1,stop=1)
#2nd
BerdychvsRH$SecondServeLocation<-substr(BerdychvsRH$X2ndNoLet,start=1,stop=1)

#Get Rid of some columns
BerdychvsRH$isRally1st<-NULL
BerdychvsRH$isRally2nd<-NULL
BerdychvsRH$Notes<-NULL
BerdychvsRH$Svr<-NULL
BerdychvsRH$Ret<-NULL
BerdychvsRH$X1stNoSV<-NULL
BerdychvsRH$X1stSV<-NULL
BerdychvsRH$X2ndNoSV<-NULL
BerdychvsRH$X2ndSV<-NULL

##ONLY RH RETURNERS##


##Forehand##
##CHANGE SECOndSERVELOCATOIN="" to XX1stIn==1 NEXT TIME##
#Location-Forehand 1st Serve Total
BerdychvsRH$Serve1stForehand<-ifelse(BerdychvsRH$Deuce==1&BerdychvsRH$FirstServeLocation==4&BerdychvsRH$XX1stIn==1,1,
                      ifelse(grepl("f|r|u|l",BerdychvsRH$FirstShot)&BerdychvsRH$Deuce==1&BerdychvsRH$XX1stIn==1,1,0))
sum(BerdychvsRH$Serve1stForehand&BerdychvsRH$Serving=="TB",na.rm=T)
#Location-Deuce 2nd Serve Total
BerdychvsRH$Serve2ndForehand<-ifelse(BerdychvsRH$Deuce==1&BerdychvsRH$SecondServeLocation==4,1,
                      ifelse(grepl("f|r|u|l",BerdychvsRH$FirstShot)&BerdychvsRH$Deuce==1&BerdychvsRH$X2ndIn==1,1,0))
sum(BerdychvsRH$Serve2ndForehand,na.rm=T)
#Location-Ad 1st Serve Total
BerdychvsRH$Serve1stForehandAd<-ifelse(BerdychvsRH$Ad==1&BerdychvsRH$FirstServeLocation==6&BerdychvsRH$XX1stIn==1,1,
                        ifelse(grepl("f|r|u|l",BerdychvsRH$FirstShot)&BerdychvsRH$Ad==1&BerdychvsRH$XX1stIn==1,1,0))
#Location-Ad 2nd Serve Total
BerdychvsRH$Serve2ndForehandAd<-ifelse(BerdychvsRH$Ad==1&BerdychvsRH$SecondServeLocation==6,1,
                        ifelse(grepl("f|r|u|l",BerdychvsRH$FirstShot)&BerdychvsRH$Ad==1&BerdychvsRH$X2ndIn==1,1,0))
##Backhand##

#Location-Deuce 1st Serve Total
BerdychvsRH$Serve1stBackhand<-ifelse(BerdychvsRH$Deuce==1&BerdychvsRH$FirstServeLocation==6&BerdychvsRH$XX1stIn==1,1,
                             ifelse(grepl("b|s|y|m",BerdychvsRH$FirstShot)&BerdychvsRH$Deuce==1&BerdychvsRH$XX1stIn==1,1,0))
#Location-Deuce 2nd Serve Total
BerdychvsRH$Serve2ndBackhand<-ifelse(BerdychvsRH$Deuce==1&BerdychvsRH$SecondServeLocation==6,1,
                             ifelse(grepl("b|s|y|m",BerdychvsRH$FirstShot)&BerdychvsRH$Deuce==1&BerdychvsRH$X2ndIn==1,1,0))

#Location-Ad 1st Serve Total
BerdychvsRH$Serve1stBackhandAd<-ifelse(BerdychvsRH$Ad==1&BerdychvsRH$FirstServeLocation==4&BerdychvsRH$XX1stIn==1,1,
                               ifelse(grepl("b|s|y|m",BerdychvsRH$FirstShot)&BerdychvsRH$Ad==1&BerdychvsRH$XX1stIn==1,1,0))
#Location-Ad 2nd Serve Total
BerdychvsRH$Serve2ndBackhandAd<-ifelse(BerdychvsRH$Ad==1&BerdychvsRH$SecondServeLocation==4,1,
                               ifelse(grepl("b|s|y|m",BerdychvsRH$FirstShot)&BerdychvsRH$Ad==1&BerdychvsRH$X2ndIn==1,1,0))



#Grep command to add new columns based on values in substring
ifelse(grepl("^fb$|^fs$|^fz$|^fp$|^fy$|^fm$|^fi$|^fk$|^rb$|^rs$|^rz$|^rp$|^ry$|^rm$|^ri$|^rk$|^ub$|^us$|^uz$|^up$|^uy$|^um$|^ui$|^uk$|^lb$|^ls$|^lz$|^lp$|^ly$|^lm$|^li$|^lk$",BerdychvsRH$First2Shots),1,0)

#Points that result in FB FF
BerdychvsRH$ServeFB<-ifelse(grepl("^fb$|^fs$|^fz$|^fp$|^fy$|^fm$|^fi$|^fk$|^rb$|^rs$|^rz$|^rp$|^ry$|^rm$|^ri$|^rk$|^ub$|^us$|^uz$|^up$|^uy$|^um$|^ui$|^uk$|^lb$|^ls$|^lz$|^lp$|^ly$|^lm$|^li$|^lk$",BerdychvsRH$First2Shots),1,0)
BerdychvsRH$ServeFF<-ifelse(grepl("^ff$|^fr$|^fv$|^fo$|^fu$|^fl$|^fh$|^fj$|^nd$|^rr$|^rv$|^ro$|^ru$|^rl$|^rh$|^rj$|^uf$|^ur$|^uv$|^uo$|^uu$|^ul$|^uh$|^uj$|^lf$|^lr$|^lv$|^lo$|^lu$|^ll$|^lh$|^lj$",BerdychvsRH$First2Shots),1,0)

#Points that result in BF BB
BerdychvsRH$ServeBF<-ifelse(grepl("^bf$|^br$|^bv$|^bo$|^bu$|^bl$|^bh$|^bj$|^sf$|^sr$|^sv$|^so$|^su$|^sl$|^sh$|^sj$|^yf$|^yr$|^yv$|^yo$|^yu$|^yl$|^yh$|^yj$|^mf$|^mr$|^mv$|^mo$|^mu$|^ml$|^mh$|^mj$",BerdychvsRH$First2Shots),1,0)
BerdychvsRH$ServeBB<-ifelse(grepl("^bb$|^bs$|^bz$|^bp$|^by$|^bm$|^bi$|^bk$|^sb$|^ss$|^sz$|^sp$|^sy$|^sm$|^si$|^sk$|^yb$|^ys$|^yz$|^yp$|^yy$|^ym$|^yi$|^yk$|^mb$|^ms$|^mz$|^mp$|^my$|^mm$|^mi$|^mk$",BerdychvsRH$First2Shots),1,0)

#Stratify by 1st/2nd serve forehand w/ forehand return points

#Forehand to Forehand-1st
BerdychvsRH$ServeFF1st<-ifelse(BerdychvsRH$XX1stIn==1&BerdychvsRH$ServeFF==1,1,0)
#2nd
BerdychvsRH$ServeFF2nd<-ifelse(BerdychvsRH$X2ndIn==1&BerdychvsRH$ServeFF==1,1,0)
BerdychvsRH$ServeFF2nd[is.na(BerdychvsRH$ServeFF2nd)] <- 0

#Forehnad to Backhand-1st
BerdychvsRH$ServeFB1st<-ifelse(BerdychvsRH$XX1stIn==1&BerdychvsRH$ServeFB==1,1,0)
#2nd
BerdychvsRH$ServeFB2nd<-ifelse(BerdychvsRH$X2ndIn==1&BerdychvsRH$ServeFB==1,1,0)
BerdychvsRH$ServeFB2nd[is.na(BerdychvsRH$ServeFB2nd)] <- 0


#Backhand to Backhand
BerdychvsRH$ServeBB1st<-ifelse(BerdychvsRH$XX1stIn==1&BerdychvsRH$ServeBB==1,1,0)
#2nd
BerdychvsRH$ServeBB2nd<-ifelse(BerdychvsRH$X2ndIn==1&BerdychvsRH$ServeBB==1,1,0)
BerdychvsRH$ServeBB2nd[is.na(BerdychvsRH$ServeBB2nd)] <- 0

#Backhand to Forehand
BerdychvsRH$ServeBF1st<-ifelse(BerdychvsRH$XX1stIn==1&BerdychvsRH$ServeBF==1,1,0)
#2nd
BerdychvsRH$ServeBF2nd<-ifelse(BerdychvsRH$X2ndIn==1&BerdychvsRH$ServeBF==1,1,0)
BerdychvsRH$ServeBF2nd[is.na(BerdychvsRH$ServeBF2nd)] <- 0



#DF to forehand side-both service boxes *Assuming body DF would be a forehand return by all players*
#Deuce-F
BerdychvsRH$ServeFDF<-ifelse(BerdychvsRH$isDouble=="TRUE"&BerdychvsRH$Deuce==1&(BerdychvsRH$SecondServeLocation==4|BerdychvsRH$SecondServeLocation==5),1,0)
#Deuce-B
BerdychvsRH$ServeBDF<-ifelse(BerdychvsRH$isDouble=="TRUE"&BerdychvsRH$Deuce==1&(BerdychvsRH$SecondServeLocation==6),1,0)
#Ad-F
BerdychvsRH$ServeFDFAd<-ifelse(BerdychvsRH$isDouble=="TRUE"&BerdychvsRH$Ad==1&(BerdychvsRH$SecondServeLocation==5|BerdychvsRH$SecondServeLocation==6),1,0)
#Ad-B
BerdychvsRH$ServeBDFAd<-ifelse(BerdychvsRH$isDouble=="TRUE"&BerdychvsRH$Ad==1&(BerdychvsRH$SecondServeLocation==4),1,0)



#Clean up code by combining possibilities for forehands
FHand1stShot <- c("f", "r", "u", "l")
BHand1stShot <- c("b", "s", "y", "m")

#Ace/Unreturned Serve-Stratify first and second serves
#*RIGHT HAndERS*#
#Deuce-F1st and 2nd- IS COMPLETE FOR FOREHAnd-follow template for BH
BerdychvsRH$AceUnreturnedF1st<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$Deuce==1&BerdychvsRH$rallyCount==1&
                                 BerdychvsRH$XX1stIn==1&
                                 BerdychvsRH$FirstShot %in% FHand1stShot,1,0)


BerdychvsRH$AceUnreturnedF2nd<-ifelse(BerdychvsRH$Deuce==1&BerdychvsRH$rallyCount==1&BerdychvsRH$X2ndIn==1&
                             (BerdychvsRH$SecondServeLocation==4|
                             (BerdychvsRH$FirstShot %in% FHand1stShot)),1,0)
#Ad-F1st and 2nd
BerdychvsRH$AceUnreturnedF1stAd<-ifelse(BerdychvsRH$Ad==1&BerdychvsRH$rallyCount==1&
                                   BerdychvsRH$XX1stIn==1&
                                   BerdychvsRH$FirstShot %in% FHand1stShot,1,0)
                                
BerdychvsRH$AceUnreturnedF2ndAd<-ifelse(BerdychvsRH$Ad==1&BerdychvsRH$X2ndIn==1&BerdychvsRH$rallyCount==1&
                               (BerdychvsRH$SecondServeLocation==6|
                               (BerdychvsRH$FirstShot %in% FHand1stShot)),1,0)

#FINALIZE THESE ALL-make sure they arent double counted on first+second serves and account for F vs B for them all
#Backhand
#Deuce Side
BerdychvsRH$AceUnreturnedB1st<-ifelse(BerdychvsRH$Deuce==1&BerdychvsRH$rallyCount==1&
                              BerdychvsRH$XX1stIn==1 &
                              BerdychvsRH$FirstShot %in% BHand1stShot,1,0)


BerdychvsRH$AceUnreturnedB2nd<-ifelse(BerdychvsRH$Deuce==1&BerdychvsRH$X2ndIn==1&BerdychvsRH$rallyCount==1&
                                        (BerdychvsRH$SecondServeLocation==6 |
                                        BerdychvsRH$FirstShot %in% BHand1stShot),1,0)

#Ad Side
BerdychvsRH$AceUnreturnedB1stAd<-ifelse(BerdychvsRH$Ad==1&BerdychvsRH$rallyCount==1&
                                   BerdychvsRH$XX1stIn=="1"&
                                   BerdychvsRH$FirstShot %in% BHand1stShot,1,0)

BerdychvsRH$AceUnreturnedB2ndAd<-ifelse(BerdychvsRH$Ad==1&BerdychvsRH$rallyCount==1&BerdychvsRH$SecondServeLocation!=""&
                               (BerdychvsRH$SecondServeLocation==4&BerdychvsRH$isAce=="TRUE"|
                               (BerdychvsRH$FirstShot %in% BHand1stShot)),1,0)


#Djokovic Win Ratio-Forehand vs. Backhand Serve
##FOREHAnd SERVE##
#Deuce Side
BerdychvsRH$DeuceFWin<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$isSvrWinner==1&(BerdychvsRH$Serve1stForehand==1|BerdychvsRH$Serve2ndForehand==1),1,0)
BerdychvsRH$DeuceFTotal<-ifelse(BerdychvsRH$Serving=="TB"&(BerdychvsRH$Serve1stForehand==1|BerdychvsRH$Serve2ndForehand==1),1,0)
sum(BerdychvsRH$DeuceFWin,na.rm=T)/sum(BerdychvsRH$DeuceFTotal,na.rm=T) #0.545 win ratio
#Ad
BerdychvsRH$AdFWin<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$isSvrWinner==1&(BerdychvsRH$Serve1stForehandAd==1|BerdychvsRH$Serve2ndForehandAd==1),1,0)
BerdychvsRH$AdFTotal<-ifelse(BerdychvsRH$Serving=="TB"&(BerdychvsRH$Serve1stForehandAd==1|BerdychvsRH$Serve2ndForehandAd==1),1,0)
sum(BerdychvsRH$AdFWin,na.rm=T)/sum(BerdychvsRH$AdFTotal,na.rm=T) #0.564 win ratio

##BACKHAnd SERVE##
#Deuce Side
BerdychvsRH$DeuceBWin<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$isSvrWinner==1&(BerdychvsRH$Serve1stBackhand==1|BerdychvsRH$Serve2ndBackhand==1),1,0)
BerdychvsRH$DeuceBTotal<-ifelse(BerdychvsRH$Serving=="TB"&(BerdychvsRH$Serve1stBackhand==1|BerdychvsRH$Serve2ndBackhand==1),1,0)
sum(BerdychvsRH$DeuceBWin,na.rm=T)/sum(BerdychvsRH$DeuceBTotal,na.rm=T) #0.566 win ratio

#Ad
BerdychvsRH$AdBWin<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$isSvrWinner==1&(BerdychvsRH$Serve1stBackhandAd==1|BerdychvsRH$Serve2ndBackhandAd==1),1,0)
BerdychvsRH$AdBTotal<-ifelse(BerdychvsRH$Serving=="TB"&(BerdychvsRH$Serve1stBackhandAd==1|BerdychvsRH$Serve2ndBackhandAd==1),1,0)
sum(BerdychvsRH$AdBWin,na.rm=T)/sum(BerdychvsRH$AdBTotal,na.rm=T) #0.582 win ratio


##Double Faults##
sum(BerdychvsRH$ServeFDF,na.rm=T) #142-Fed  172-Djoker 40-Berdych 45-Del Potro
sum(BerdychvsRH$ServeFDFAd,na.rm=T) #163-Fed  183-Djoker 45-Berdych 46-Del Potro
sum(BerdychvsRH$ServeBDF,na.rm=T) #108-Fed  97-Djoker 28-Berdych 47-Del Potro
sum(BerdychvsRH$ServeBDFAd,na.rm=T) #102-Fed  93-Djoker 25-Berdych 31-Del Potro

##Unreturned Serves## CHANGE THISSSS
sum(BerdychvsRH$AceUnreturnedF1st,na.rm=T) #416-Fed 423-Djoker 110-Berdych 58-Del Potro
sum(BerdychvsRH$AceUnreturnedB1st,na.rm=T) #656  616-Djoker 179-Berdych 204-Del Potro
sum(BerdychvsRH$AceUnreturnedF1stAd,na.rm=T) #503  598-Djoker 159-Berdych 153-Del Potro
sum(BerdychvsRH$AceUnreturnedB1stAd,na.rm=T) #664  619-Djoker 144-Berdych 148-Del Potro
sum(BerdychvsRH$AceUnreturnedF2nd,na.rm=T) #1039  1130-Djoker 67-Berdych 61-Del Potro
sum(BerdychvsRH$AceUnreturnedB2nd,na.rm=T) #330   277-Djoker  95-Berdych 98-Del Potro
sum(BerdychvsRH$AceUnreturnedF2ndAd,na.rm=T) #768  826-Djoker 39-Berdych 55-Del Potro
sum(BerdychvsRH$AceUnreturnedB2ndAd,na.rm=T) #375  291-Djoker 103-Berdych 97-Del Potro

####Fed/Djoker-First Serve Vs Second Serve Returns####

##FF##
BerdychvsRH$FFWin1st<-ifelse(BerdychvsRH$Serving!="TB"&BerdychvsRH$XX1stIn==1&BerdychvsRH$isSvrWinner==0&BerdychvsRH$ServeFF==1,1,0)
BerdychvsRH$FF1stTotal<-ifelse(BerdychvsRH$Serving!="TB"&BerdychvsRH$XX1stIn==1&BerdychvsRH$ServeFF==1,1,0)
sum(BerdychvsRH$FFWin1st,na.rm=T)/sum(BerdychvsRH$FF1stTotal,na.rm=T) #win rate=0.457
sum(BerdychvsRH$FFWin1st,na.rm=T) #434
sum(BerdychvsRH$FF1stTotal,na.rm=T) #949
#2nd serve-made serves
BerdychvsRH$FFWin2nd<-ifelse(BerdychvsRH$Serving!="TB"&BerdychvsRH$X2ndIn==1&BerdychvsRH$isSvrWinner==0&BerdychvsRH$ServeFF==1,1,0)
BerdychvsRH$FF2ndTotal<-ifelse(BerdychvsRH$Serving!="TB"&BerdychvsRH$X2ndIn==1&BerdychvsRH$ServeFF==1,1,0)
sum(BerdychvsRH$FFWin2nd,na.rm=T)/sum(BerdychvsRH$FF2ndTotal,na.rm=T) #win rate=0.474
sum(BerdychvsRH$FFWin2nd,na.rm=T) #153
sum(BerdychvsRH$FF2ndTotal,na.rm=T) #323


##FB##
BerdychvsRH$FBWin1st<-ifelse(BerdychvsRH$Serving!="TB"& BerdychvsRH$XX1stIn==1& BerdychvsRH$isSvrWinner==0&BerdychvsRH$ServeFB==1,1,0)
BerdychvsRH$FB1stTotal<-ifelse(BerdychvsRH$Serving!="TB"& BerdychvsRH$XX1stIn==1& BerdychvsRH$ServeFB==1,1,0)
sum(BerdychvsRH$FBWin1st,na.rm=T)/sum(BerdychvsRH$FB1stTotal,na.rm=T) #win rate=0.483
sum(BerdychvsRH$FBWin1st,na.rm=T) #281
sum(BerdychvsRH$FB1stTotal,na.rm=T) #582
#2nd serve-made serves
BerdychvsRH$FBWin2nd<-ifelse(BerdychvsRH$Serving!="TB"&BerdychvsRH$X2ndIn==1&BerdychvsRH$isSvrWinner==0&BerdychvsRH$ServeFB==1,1,0)
BerdychvsRH$FB2ndTotal<-ifelse(BerdychvsRH$Serving!="TB"&BerdychvsRH$X2ndIn==1&BerdychvsRH$ServeFB==1,1,0)
sum(BerdychvsRH$FBWin2nd,na.rm=T)/sum(BerdychvsRH$FB2ndTotal,na.rm=T) #win rate=0.551
sum(BerdychvsRH$FBWin2nd,na.rm=T) #217
sum(BerdychvsRH$FB2ndTotal,na.rm=T)#394


#BF
BerdychvsRH$BFWin1st<-ifelse(BerdychvsRH$Serving!="TB"& BerdychvsRH$XX1stIn==1& BerdychvsRH$isSvrWinner==0&BerdychvsRH$ServeBF==1,1,0)
BerdychvsRH$BF1stTotal<-ifelse(BerdychvsRH$Serving!="TB"& BerdychvsRH$XX1stIn==1& BerdychvsRH$ServeBF==1,1,0)
sum(BerdychvsRH$BFWin1st,na.rm=T)/sum(BerdychvsRH$BF1stTotal,na.rm=T) #win rate=0.382
sum(BerdychvsRH$BFWin1st,na.rm=T) #436
sum(BerdychvsRH$BF1stTotal,na.rm=T) #1140
#2nd serve-made serves
BerdychvsRH$BFWin2nd<-ifelse(BerdychvsRH$Serving!="TB"&BerdychvsRH$X2ndIn==1&BerdychvsRH$isSvrWinner==0&BerdychvsRH$ServeBF==1,1,0)
BerdychvsRH$BF2ndTotal<-ifelse(BerdychvsRH$Serving!="TB"&BerdychvsRH$X2ndIn==1&BerdychvsRH$ServeBF==1,1,0)
sum(BerdychvsRH$BFWin2nd,na.rm=T)/sum(BerdychvsRH$BF2ndTotal,na.rm=T) #win rate=0.503
sum(BerdychvsRH$BFWin2nd,na.rm=T) #253
sum(BerdychvsRH$BF2ndTotal,na.rm=T) #503


#BB
BerdychvsRH$BBWin1st<-ifelse(BerdychvsRH$Serving!="TB"& BerdychvsRH$XX1stIn==1 &BerdychvsRH$isSvrWinner==0&BerdychvsRH$ServeBB==1,1,0)
BerdychvsRH$BB1stTotal<-ifelse(BerdychvsRH$Serving!="TB"& BerdychvsRH$XX1stIn==1& BerdychvsRH$ServeBB==1,1,0)
sum(BerdychvsRH$BBWin1st,na.rm=T)/sum(BerdychvsRH$BB1stTotal,na.rm=T) #win rate=0.497
sum(BerdychvsRH$BBWin1st,na.rm=T) #293
sum(BerdychvsRH$BB1stTotal,na.rm=T) #589
#2nd serve-made serves
BerdychvsRH$BBWin2nd<-ifelse(BerdychvsRH$Serving!="TB"&BerdychvsRH$X2ndIn==1&BerdychvsRH$isSvrWinner==0&BerdychvsRH$ServeBB==1,1,0)
BerdychvsRH$BB2ndTotal<-ifelse(BerdychvsRH$Serving!="TB"&BerdychvsRH$X2ndIn==1&BerdychvsRH$ServeBB==1,1,0)
sum(BerdychvsRH$BBWin2nd,na.rm=T)/sum(BerdychvsRH$BB2ndTotal,na.rm=T) #win rate=0.574
sum(BerdychvsRH$BBWin2nd,na.rm=T) #549
sum(BerdychvsRH$BB2ndTotal,na.rm=T) #956


#Pearson Stat Test-Murray Returns#

#FF vs FB-1st Serve
FFobs = c(434,515 )        # observed frequencies
FFexp = c(0.467015, 0.532985)  # expected proportions

chisq.test(x = FFobs,
           p = FFexp) #0.358 w/ p-value=0.550 so not significant
#FF vs FB-2nd Serve
FFobs2nd = c(153,170) #observed frequencies
FFexp2nd = c(0.516039, 0.483961)  # expected proportions

chisq.test(x = FFobs2nd,
           p = FFexp2nd) #2.320 w/ p-value=0.128 so not significant but really close

#BF vs BB-1st Serve
BFobs = c(436,704 )        # observed frequencies
BFexp = c(0.421631, 0.578369)  # expected proportions

chisq.test(x = BFobs,
           p = BFexp) #7.1743 w/ p-value=0.007 so not significant at all not even some hahahaha. 
#BF vs BB-2nd Serve
BFobs2nd = c(253,250 )        # observed frequencies
BFexp2nd = c(0.549692, 0.450308)  # expected proportions

chisq.test(x = BFobs2nd,
           p = BFexp2nd) #4.434 w/ p-value=0.035 so significant



#Pearson Stat Test-Federer Returns#

#FF vs FB-1st Serve
FFobs = c(375,420 )        # observed frequencies
FFexp = c(0.521583, 0.478417)  # expected proportions

chisq.test(x = FFobs,
           p = FFexp) #7.928 w/ p-value=0.005 so significant
#FF vs FB-2nd Serve
FFobs2nd = c(240,184) #observed frequencies
FFexp2nd = c(0.583875, 0.416125)  # expected proportions

chisq.test(x = FFobs2nd,
           p = FFexp2nd) #0.555 w/ p-value=0.456 so not significant but really close

#BF vs BB-1st Serve
BFobs = c(585,777 )        # observed frequencies
BFexp = c(0.45412, 0.54588)  # expected proportions

chisq.test(x = BFobs,
           p = BFexp) #3.326 w/ p-value=0.069 so not significant at all not even some hahahaha. 
#BF vs BB-2nd Serve
BFobs2nd = c(449,465 )        # observed frequencies
BFexp2nd = c(0.522838, 0.477162)  # expected proportions

chisq.test(x = BFobs2nd,
           p = BFexp2nd) #3.656 w/ p-value=0.056 so not significant


#Pearson Stat Test-Djokovic Returns#

#FF vs FB-1st Serve
FFobs = c(654,644 )        # observed frequencies
FFexp = c(0.508764, 0.491236)  # expected proportions

chisq.test(x = FFobs,
           p = FFexp) #0.125 w/ p-value=0.723 so not significant
#FF vs FB-2nd Serve
FFobs2nd = c(247,184) #observed frequencies
FFexp2nd = c(0.588608, 0.411122)  # expected proportions

chisq.test(x = FFobs2nd,
           p = FFexp2nd) #0.429 w/ p-value=0.513 so not significant but really close

#BF vs BB-1st Serve
BFobs = c(575,689 )        # observed frequencies
BFexp = c(0.508897, 0.491103)  # expected proportions

chisq.test(x = BFobs,
           p = BFexp) #14.744 w/ p-value=0.000 so not significant at all not even some hahahaha. 
#BF vs BB-2nd Serve
BFobs2nd = c(614,468 )        # observed frequencies
BFexp2nd = c(0.58104, 0.41896)  # expected proportions

chisq.test(x = BFobs2nd,
           p = BFexp2nd) #0.819 w/ p-value=0.366 so not significant


#Pearson Stat Test-Nadal Returns#

#FF vs FB-1st Serve
FFobs = c(399,440 )        # observed frequencies
FFexp = c(0.49744, 0.50256)  # expected proportions

chisq.test(x = FFobs,
           p = FFexp) #1.606 w/ p-value=0.205 so not significant
#FF vs FB-2nd Serve
FFobs2nd = c(280,213)        # observed frequencies
FFexp2nd = c(0.582371, 0.417629)  # expected proportions

chisq.test(x = FFobs2nd,
           p = FFexp2nd) #0.421 w/ p-value=0.516 so not significant but really close

#BF vs BB-1st Serve
BFobs = c(831,1012 )        # observed frequencies
BFexp = c(0.452923, 0.547077)  # expected proportions

chisq.test(x = BFobs,
           p = BFexp) #0.031 w/ p-value=0.861 so not
#BF vs BB-2nd Serve
BFobs2nd = c(510,409 )        # observed frequencies
BFexp2nd = c(0.572089, 0.427911)  # expected proportions

chisq.test(x = BFobs2nd,
           p = BFexp2nd) #1.103 w/ p-value=0.294 so not significant


#Pearson Stat Test-Berdych Returns#

#FF vs FB-1st Serve
FFobs = c(128,169 )        # observed frequencies
FFexp = c(0.431373, 0.568627)  # expected proportions

chisq.test(x = FFobs,
           p = FFexp) #0.000 w/ p-value=0.989 so not significant
#FF vs FB-2nd Serve
FFobs2nd = c(82,72 )        # observed frequencies
FFexp2nd = c(0.560538, 0.439462)  # expected proportions

chisq.test(x = FFobs2nd,
           p = FFexp2nd) #0.493 w/ p-value=0.483 so not significant but really close

#BF vs BB-1st Serve
BFobs = c(128,261 )        # observed frequencies
BFexp = c(0.376963, 0.623037)  # expected proportions

chisq.test(x = BFobs,
           p = BFexp) #3.803 w/ p-value=0.050 so ya
#BF vs BB-2nd Serve
BFobs2nd = c(143,123 )        # observed frequencies
BFexp2nd = c(0.545988, 0.454012)  # expected proportions

chisq.test(x = BFobs2nd,
           p = BFexp2nd) #0.076 w/ p-value=0.783 so not significant


#Pearson Stat Test-Wawrinka Returns#

#FF vs FB-1st Serve
FFobs = c(144,192 )        # observed frequencies
FFexp = c(0.458167, 0.541833)  # expected proportions

chisq.test(x = FFobs,
           p = FFexp) #1.186 w/ p-value=0.276 so not significant
#FF vs FB-2nd Serve
FFobs2nd = c(137,120 )        # observed frequencies
FFexp2nd = c(0.528875, 0.471125)  # expected proportions

chisq.test(x = FFobs2nd,
           p = FFexp2nd) #0.018 w/ p-value=0.893 so not significant but really close

#BF vs BB-1st Serve
BFobs = c(176,315 )        # observed frequencies
BFexp = c(0.398196, 0.601804)  # expected proportions

chisq.test(x = BFobs,
           p = BFexp) #3.24 w/ p-value=0.072 so not
#BF vs BB-2nd Serve
BFobs2nd = c(160,159 )        # observed frequencies
BFexp2nd = c(0.505821, 0.494179)  # expected proportions

chisq.test(x = BFobs2nd,
           p = BFexp2nd) #0.023 w/ p-value=0.879 so not significant



#Pearson Stat Test-Nishikori Returns#

#FF vs FB-1st Serve
FFobs = c(140,142 )        # observed frequencies
FFexp = c(0.482679, 0.517321)  # expected proportions

chisq.test(x = FFobs,
           p = FFexp) #0.214 w/ p-value=0.643 so not significant
#FF vs FB-2nd Serve
FFobs2nd = c(74,65 )        # observed frequencies
FFexp2nd = c(0.581633, 0.418367)  # expected proportions

chisq.test(x = FFobs2nd,
           p = FFexp2nd) #1.386 w/ p-value=0.239 so not significant but really close

#BF vs BB-1st Serve
BFobs = c(92,179 )        # observed frequencies
BFexp = c(0.425051, 0.574949)  # expected proportions

chisq.test(x = BFobs,
           p = BFexp) #8.1193 w/ p-value=0.004 so SIGNIFICANT
#BF vs BB-2nd Serve
BFobs2nd = c(92,75 )        # observed frequencies
BFexp2nd = c(0.57085, 0.42915)  # expected proportions

chisq.test(x = BFobs2nd,
           p = BFexp2nd) #0.271 w/ p-value=0.602 so not significant


#Pearson Stat Test-Raonic Returns#

#FF vs FB-1st Serve
FFobs = c(171,222 )        # observed frequencies
FFexp = c(0.43762, 0.56238)  # expected proportions

chisq.test(x = FFobs,
           p = FFexp) #0.010 w/ p-value=0.920 so not significant
#FF vs FB-2nd Serve
FFobs2nd = c(100,91 )        # observed frequencies
FFexp2nd = c(0.517123, 0.482877)  # expected proportions

chisq.test(x = FFobs2nd,
           p = FFexp2nd) #0.032 w/ p-value=0.859 so not significant but really close

#BF vs BB-1st Serve
BFobs = c(171,273 )        # observed frequencies
BFexp = c(0.384483, 0.615517)  # expected proportions

chisq.test(x = BFobs,
           p = BFexp) #0.001 w/ p-value=0.978 so not significant
#BF vs BB-2nd Serve
BFobs2nd = c(81,111 )        # observed frequencies
BFexp2nd = c(0.475728, 0.524272)  # expected proportions

chisq.test(x = BFobs2nd,
           p = BFexp2nd) #2.233 w/ p-value=0.135 so not significant


#Pearson Stat Test-Del Potro Returns#

#FF vs FB-1st Serve
FFobs = c(171,200 )        # observed frequencies
FFexp = c(0.473881, 0.526119)  # expected proportions

chisq.test(x = FFobs,
           p = FFexp) #0.250 w/ p-value=0.617 so not significant
#FF vs FB-2nd Serve
FFobs2nd = c(70,67 )        # observed frequencies
FFexp2nd = c(0.488152, 0.511848)  # expected proportions

chisq.test(x = FFobs2nd,
           p = FFexp2nd) #0.285 w/ p-value=0.594 so not significant but really close

#BF vs BB-1st Serve
BFobs = c(165,276 )        # observed frequencies
BFexp = c(0.3958, 0.6042)  # expected proportions

chisq.test(x = BFobs,
           p = BFexp) #0.864 w/ p-value=0.353 so not significant
#BF vs BB-2nd Serve
BFobs2nd = c(148,159 )        # observed frequencies
BFexp2nd = c(0.489569, 0.510431)  # expected proportions

chisq.test(x = BFobs2nd,
           p = BFexp2nd) #0.069 w/ p-value=0.793 so not significant


#Pearson Stat Test-Ferrer Returns#

#FF vs FB-1st Serve
FFobs = c(83,113 )        # observed frequencies
FFexp = c(0.460641, 0.539359)  # expected proportions

chisq.test(x = FFobs,
           p = FFexp) #1.09 w/ p-value=0.297 so not significant
#FF vs FB-2nd Serve
FFobs2nd = c(30,20 )        # observed frequencies
FFexp2nd = c(0.6, 0.4)  # expected proportions

chisq.test(x = FFobs2nd,
           p = FFexp2nd) #0 w/ p-value=1.0 so not significant but really close

#BF vs BB-1st Serve
BFobs = c(69,146 )        # observed frequencies
BFexp = c(0.35241, 0.64759)  # expected proportions

chisq.test(x = BFobs,
           p = BFexp) #0.936 w/ p-value=0.334 so not significant
#BF vs BB-2nd Serve
BFobs2nd = c(74,73 )        # observed frequencies
BFexp2nd = c(0.508046, 0.491954)  # expected proportions

chisq.test(x = BFobs2nd,
           p = BFexp2nd) #0.013 w/ p-value=0.910 so not significant



#Pearson Test-Forehand vs Backhand win % on 1st vs second serves

###Raonic Win Ratio-Forehand vs. Backhand Serve###

##FOREHAnd 1ST SERVE##
#Deuce Side
BerdychvsRH$DeuceFWin<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$isSvrWinner==1&BerdychvsRH$Serve1stForehand==1,1,0)
BerdychvsRH$DeuceFTotal<-ifelse(BerdychvsRH$Serving=="TB"& BerdychvsRH$Serve1stForehand==1,1,0)
sum(BerdychvsRH$DeuceFWin,na.rm=T)/sum(BerdychvsRH$DeuceFTotal,na.rm=T) #0.807 win ratio
sum(BerdychvsRH$DeuceFWin,na.rm=T) #368
sum(BerdychvsRH$DeuceFTotal,na.rm=T) #456
#Ad
BerdychvsRH$AdFWin<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$isSvrWinner==1&BerdychvsRH$Serve1stForehandAd==1,1,0)
BerdychvsRH$AdFTotal<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$Serve1stForehandAd==1,1,0)
sum(BerdychvsRH$AdFWin,na.rm=T)/sum(BerdychvsRH$AdFTotal,na.rm=T) #0.752 win ratio
sum(BerdychvsRH$AdFWin,na.rm=T) #258
sum(BerdychvsRH$AdFTotal,na.rm=T) #343

##FOREHAnd 2nd SERVE##
#Deuce Side
BerdychvsRH$DeuceF2ndWin<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$isSvrWinner==1&BerdychvsRH$Serve2ndForehand==1,1,0)
BerdychvsRH$DeuceF2ndTotal<-ifelse(BerdychvsRH$Serving=="TB"& BerdychvsRH$Serve2ndForehand==1,1,0)
sum(BerdychvsRH$DeuceF2ndWin,na.rm=T)/sum(BerdychvsRH$DeuceF2ndTotal,na.rm=T) #0.548 win ratio
sum(BerdychvsRH$DeuceF2ndWin,na.rm=T) #97
sum(BerdychvsRH$DeuceF2ndTotal,na.rm=T) #177
#Ad
BerdychvsRH$AdF2ndWin<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$isSvrWinner==1&BerdychvsRH$Serve2ndForehandAd==1,1,0)
BerdychvsRH$AdF2ndTotal<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$Serve2ndForehandAd==1,1,0)
sum(BerdychvsRH$AdF2ndWin,na.rm=T)/sum(BerdychvsRH$AdF2ndTotal,na.rm=T) #0.533 win ratio
sum(BerdychvsRH$AdF2ndWin,na.rm=T) #57
sum(BerdychvsRH$AdF2ndTotal,na.rm=T) #107


##BACKHAnd 1ST SERVE##
#Deuce Side
BerdychvsRH$DeuceBWin<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$isSvrWinner==1&BerdychvsRH$Serve1stBackhand==1,1,0)
BerdychvsRH$DeuceBTotal<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$Serve1stBackhand==1,1,0)
sum(BerdychvsRH$DeuceBWin,na.rm=T)/sum(BerdychvsRH$DeuceBTotal,na.rm=T) #0.802 win ratio
sum(BerdychvsRH$DeuceBWin, na.rm=T) #349
sum(BerdychvsRH$DeuceBTotal,na.rm=T) #435
#Ad
BerdychvsRH$AdBWin<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$isSvrWinner==1&BerdychvsRH$Serve1stBackhandAd==1,1,0)
BerdychvsRH$AdBTotal<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$Serve1stBackhandAd==1,1,0)
sum(BerdychvsRH$AdBWin,na.rm=T)/sum(BerdychvsRH$AdBTotal,na.rm=T) #0.796 win ratio
sum(BerdychvsRH$AdBWin,na.rm=T) #344
sum(BerdychvsRH$AdBTotal,na.rm=T) #432

##BACKHAnd 2nd SERVE##
#Deuce Side
BerdychvsRH$DeuceB2ndWin<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$isSvrWinner==1&BerdychvsRH$Serve2ndBackhand==1,1,0)
BerdychvsRH$DeuceB2ndTotal<-ifelse(BerdychvsRH$Serving=="TB"& BerdychvsRH$Serve2ndBackhand==1,1,0)
sum(BerdychvsRH$DeuceB2ndWin,na.rm=T)/sum(BerdychvsRH$DeuceB2ndTotal,na.rm=T) #0.515 win ratio
sum(BerdychvsRH$DeuceB2ndWin,na.rm=T) #170
sum(BerdychvsRH$DeuceB2ndTotal,na.rm=T) #330
#Ad
BerdychvsRH$AdB2ndWin<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$isSvrWinner==1&BerdychvsRH$Serve2ndBackhandAd==1,1,0)
BerdychvsRH$AdB2ndTotal<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$Serve2ndBackhandAd==1,1,0)
sum(BerdychvsRH$AdB2ndWin,na.rm=T)/sum(BerdychvsRH$AdB2ndTotal,na.rm=T) #0.551 win ratio
sum(BerdychvsRH$AdB2ndWin,na.rm=T) #200
sum(BerdychvsRH$AdB2ndTotal,na.rm=T) #363


##Pearson Stat Testing-Raonic##

#Deuce-1st Serve
D1stobserved = c(368,88 )        # observed frequencies
D1stexpected = c(0.804714, 0.195286)  # expected proportions

chisq.test(x = D1stobserved,
           p = D1stexpected) #0.015 w/ p-value=0.901 so not significant
#Ad-1st Serve
D1stAdobserved = c(258,85 )        # observed frequencies
D1stAdexpected = c(0.776774, 0.223226)  # expected proportions

chisq.test(x = D1stAdobserved,
           p = D1stAdexpected) #1.196 w/ p-value=0.274 so significant at 10%

#Deuce-2nd Serve
D2ndobserved = c(97,80 )        # observed frequencies
D2ndexpected = c(0.526627, 0.473373)  # expected proportions

chisq.test(x = D2ndobserved,
           p = D2ndexpected) #0.325 w/ p-value=0.569 so not significant

#Ad-2nd Serve
D2ndAdobserved = c(57,50 )        # observed frequencies
D2ndAdexpected = c(0.546809, 0.453191)  # expected proportions

chisq.test(x = D2ndAdobserved,
           p = D2ndAdexpected) #0.086 w/ p-value=0.770 so not significant  


##Pearson Stat Testing-Ferrer##

#Deuce-1st Serve
D1stobserved = c(234,113 )        # observed frequencies
D1stexpected = c(0.658009, 0.341991)  # expected proportions

chisq.test(x = D1stobserved,
           p = D1stexpected) #0.411 w/ p-value=0.521 so not significant
#Ad-1st Serve
D1stAdobserved = c(198,115 )        # observed frequencies
D1stAdexpected = c(0.637255, 0.362745)  # expected proportions

chisq.test(x = D1stAdobserved,
           p = D1stAdexpected) #0.029 w/ p-value=0.864 so significant at 10%

#Deuce-2nd Serve
D2ndobserved = c(88,64 )        # observed frequencies
D2ndexpected = c(0.528117, 0.471883)  # expected proportions

chisq.test(x = D2ndobserved,
           p = D2ndexpected) #1.576 w/ p-value=0.209 so not significant

#Ad-2nd Serve
D2ndAdobserved = c(40,33 )        # observed frequencies
D2ndAdexpected = c(0.516883, 0.483117)  # expected proportions

chisq.test(x = D2ndAdobserved,
           p = D2ndAdexpected) #0.282 w/ p-value=0.595 so not significant  



##Pearson Stat Testing-Nishikori##

#Deuce-1st Serve
D1stobserved = c(364,138 )        # observed frequencies
D1stexpected = c(0.712137, 0.287863)  # expected proportions

chisq.test(x = D1stobserved,
           p = D1stexpected) #0.411 w/ p-value=0.521 so not significant
#Ad-1st Serve
D1stAdobserved = c(249,119 )        # observed frequencies
D1stAdexpected = c(0.680348, 0.319652)  # expected proportions

chisq.test(x = D1stAdobserved,
           p = D1stAdexpected) #0.023 w/ p-value=0.879 so significant at 10%

#Deuce-2nd Serve
D2ndobserved = c(126,113 )        # observed frequencies
D2ndexpected = c(0.514803, 0.485197)  # expected proportions

chisq.test(x = D2ndobserved,
           p = D2ndexpected) #0.147 w/ p-value=0.701 so not significant

#Ad-2nd Serve
D2ndAdobserved = c(99,98 )        # observed frequencies
D2ndAdexpected = c(0.5189, 0.4811)  # expected proportions

chisq.test(x = D2ndAdobserved,
           p = D2ndAdexpected) #0.211 w/ p-value=0.6458 so significant at 10% 


##Pearson Stat Testing-Wawrinka##

#Deuce-1st Serve
D1stobserved = c(469,166 )        # observed frequencies
D1stexpected = c(0.74074, 0.25926)  # expected proportions

chisq.test(x = D1stobserved,
           p = D1stexpected) #0.015 w/ p-value=0.901 so significant-1%
#Ad-1st Serve
D1stAdobserved = c(337,153 )        # observed frequencies
D1stAdexpected = c(0.705198, 0.294802)  # expected proportions

chisq.test(x = D1stAdobserved,
           p = D1stAdexpected) #0.717 w/ p-value=0.397 so significant at 10%

#Deuce-2nd Serve
D2ndobserved = c(170,124 )        # observed frequencies
D2ndexpected = c(0.559173, 0.440827)  # expected proportions

chisq.test(x = D2ndobserved,
           p = D2ndexpected) #0.433 w/ p-value=0.510 so not significant

#Ad-2nd Serve
D2ndAdobserved = c(85,101 )        # observed frequencies
D2ndAdexpected = c(0.525301, 0.474699)  # expected proportions

chisq.test(x = D2ndAdobserved,
           p = D2ndAdexpected) #3.481 w/ p-value=0.062 so significant at 10% 



##Pearson Stat Testing-Murray##

#Deuce-1st Serve
D1stobserved = c(974,403 )        # observed frequencies
D1stexpected = c(0.739867, 0.260133)  # expected proportions

chisq.test(x = D1stobserved,
           p = D1stexpected) #7.572 w/ p-value=0.006 so significant-1%
#Ad-1st Serve
D1stAdobserved = c(852,368 )        # observed frequencies
D1stAdexpected = c(0.720876, 0.279124)  # expected proportions

chisq.test(x = D1stAdobserved,
           p = D1stAdexpected) #3.07 w/ p-value=0.080 so significant at 10%

#Deuce-2nd Serve
D2ndobserved = c(279,240 )        # observed frequencies
D2ndexpected = c(0.513871, 0.486129)  # expected proportions

chisq.test(x = D2ndobserved,
           p = D2ndexpected) #1.167 w/ p-value=0.280 so not significant

#Ad-2nd Serve
D2ndAdobserved = c(232,201 )        # observed frequencies
D2ndAdexpected = c(0.508018, 0.491982)  # expected proportions

chisq.test(x = D2ndAdobserved,
           p = D2ndAdexpected) #1.337 w/ p-value=0.248 so not significant 


##Pearson Stat Testing-Nadal##

#Deuce-1st Serve
D1stobserved = c(594,251 )        # observed frequencies
D1stexpected = c(0.701967, 0.298033)  # expected proportions

chisq.test(x = D1stobserved,
           p = D1stexpected) #0.004 w/ p-value=0.950 so not significant
#Ad-1st Serve
D1stAdobserved = c(632,250 )        # observed frequencies
D1stAdexpected = c(0.719025, 0.280975)  # expected proportions

chisq.test(x = D1stAdobserved,
           p = D1stAdexpected) #0.027 w/ p-value=0.870 so not significant 

#Deuce-2nd Serve
D2ndobserved = c(333,267 )        # observed frequencies
D2ndexpected = c(0.556333, 0.443667)  # expected proportions

chisq.test(x = D2ndobserved,
           p = D2ndexpected) #0.004 w/ p-value=0.948 so not significant

#Ad-2nd Serve
D2ndAdobserved = c(171,141 )        # observed frequencies
D2ndAdexpected = c(0.57344, 0.42656)  # expected proportions

chisq.test(x = D2ndAdobserved,
           p = D2ndAdexpected) #0.820 w/ p-value=0.365 so not significant 

##Pearson Stat Testing-Djokovic##

#Deuce-1st Serve
D1stobserved = c(1217,498 )        # observed frequencies
D1stexpected = c(0.716531, 0.283469)  # expected proportions

chisq.test(x = D1stobserved,
           p = D1stexpected) #0.403 w/ p-value=0.526 so not significant
#Ad-1st Serve
D1stAdobserved = c(996,450 )        # observed frequencies
D1stAdexpected = c(0.702363, 0.297637)  # expected proportions

chisq.test(x = D1stAdobserved,
           p = D1stAdexpected) #1.273 w/ p-value=0.2592 so not significant 

#Deuce-2nd Serve
D2ndobserved = c(490,402 )        # observed frequencies
D2ndexpected = c(0.572272, 0.427728)  # expected proportions

chisq.test(x = D2ndobserved,
           p = D2ndexpected) #1.919 w/ p-value=0.166 so not significant

#Ad-2nd Serve
D2ndAdobserved = c(367,293 )        # observed frequencies
D2ndAdexpected = c(0.563242, 0.436758)  # expected proportions

chisq.test(x = D2ndAdobserved,
           p = D2ndAdexpected) #0.138 w/ p-value=0.710 so not significant 


##Pearson Stat Testing-Federer##
#Deuce-1st Serve
D1stobserved = c(1192,382 )        # observed frequencies
D1stexpected = c(0.781362, 0.218638)  # expected proportions

chisq.test(x = D1stobserved,
           p = D1stexpected) #5.332 w/ p-value=0.021 so significant at 5%
#Ad-1st Serve
D1stAdobserved = c(909,281 )        # observed frequencies
D1stAdexpected = c(0.776735, 0.223265)  # expected proportions

chisq.test(x = D1stAdobserved,
           p = D1stAdexpected) #1.1365 w/ p-value=0.2864 so not significant 

#Deuce-2nd Serve
D2ndobserved = c(371,311 )        # observed frequencies
D2ndexpected = c(0.556147, 0.443853)  # expected proportions

chisq.test(x = D2ndobserved,
           p = D2ndexpected) #0.408 w/ p-value=0.523 so not significant

#Ad-2nd Serve
D2ndAdobserved = c(320,242 )        # observed frequencies
D2ndAdexpected = c(0.578269, 0.421731)  # expected proportions

chisq.test(x = D2ndAdobserved,
           p = D2ndAdexpected) #0.181 w/ p-value=0.6701 so not significant


##Pearson Stat Testing-Berdych##
#Deuce-1st Serve
D1stobserved = c(347,100 )        # observed frequencies
D1stexpected = c(0.752626, 0.247374)  # expected proportions

chisq.test(x = D1stobserved,
           p = D1stexpected) # 1.34 w/ p-value=0.246 so not significant 
#Ad-1st Serve
D1stAdobserved = c(310,80 )        # observed frequencies
D1stAdexpected = c(0.773121, 0.226879)  # expected proportions

chisq.test(x = D1stAdobserved,
           p = D1stAdexpected) # 1.1158 w/ p-value=0.2908 so not significant 

#Deuce-2nd Serve
D2ndobserved = c(100,87 )        # observed frequencies
D2ndexpected = c(0.505119, 0.494881)  # expected proportions

chisq.test(x = D2ndobserved,
           p = D2ndexpected) #0.657 w/ p-value=0.4175 so not significant
#Ad-2nd Serve
D2ndAdobserved = c(49,66 )        # observed frequencies
D2ndAdexpected = c(0.501764, 0.498236)  # expected proportions

chisq.test(x = D2ndAdobserved,
           p = D2ndAdexpected) #2.6345 w/ p-value=0.105 so SIGNIFICANT

##Pearson Stat Testing-DelPo##
#Deuce-1st Serve
D1stobserved = c(218,96 )        # observed frequencies
D1stexpected = c(0.73927, 0.26073)  # expected proportions

chisq.test(x = D1stobserved,
           p = D1stexpected) # 3.2992 w/ p-value=0.06931 so SIGNIFICANT at 10%
#Ad-1st Serve
D1stAdobserved = c(318,113 )        # observed frequencies
D1stAdexpected = c(0.730104, 0.269896)  # expected proportions

chisq.test(x = D1stAdobserved,
           p = D1stAdexpected) # 0.185 w/ p-value=0.667 so not significant 

#Deuce-2nd Serve
D2ndobserved = c(62,54 )        # observed frequencies
D2ndexpected = c(0.514563, 0.485437)  # expected proportions

chisq.test(x = D2ndobserved,
           p = D2ndexpected) #0.184 w/ p-value=0.667 so not significant

#Ad-2nd Serve
D2ndAdobserved = c(64,37 )        # observed frequencies
D2ndAdexpected = c(0.579185, 0.420815)  # expected proportions

chisq.test(x = D2ndAdobserved,
           p = D2ndAdexpected) #1.23 w/ p-value=0.267 so not significant


##Notes: For each guy...(Fed,Murray,Djoker,Nadal,Stan) vs RH returners
#1. Need to do second serves for ff, fb, bb, bf and compare to 1st CHECK*
#2. Overall 1st/2nd serve win percentages-EV(F) vs. EV(B)
#3. Overall distribution plot of some summary stats (percentages on points in each category ff vs fb etc. for each player)
#4. Pearson stat test!!-Should by game theory be equal win %s
#5. Latex it all-table for stats-then R plot of them in graph
#6. Next steps: Apply to more players-how to do this more efficiently? Left vs rh differences? Analyse time-boxed differences?

##Heterogeneity-matches decided by few points % wise close (look up and cite source)##
ReturnF1st<-c(0.337,0.323,0.336,0.368,0.270,0.294,0.248,0.278,0.297,0.281)
mean(ReturnF1st) #0.303
ReturnF2nd<-c(0.520,0.496,0.486,0.548,0.520,0.457,0.517,0.447,0.452,0.498)
mean(ReturnF2nd) #0.494
ReturnB1st<-c(0.302,0.296,0.303,0.341,0.209,0.243,0.250,0.246,0.266,0.255)
mean(ReturnB1st) #0.271
ReturnB2nd<-c(0.543,0.482,0.501,0.514,0.467,0.467,0.503,0.460,0.470,0.526)
mean(ReturnB2nd) #0.493
PlayerReturn<-c("Novak Djokovic","Roger Federer","Andy Murray","Rafael Nadal","David Ferrer","Juan Martin Del Potro","Tomas Berdych","Milos Raonic","Stanislas Wawrinka","Kei Nishikori")

#Plot Scatter-get mean bars for each group

#Data Frame of values
A <- data.frame(group="1st Forehand", value=c(0.337,0.323,0.336,0.368,0.270,0.294,0.248,0.278,0.297,0.281),player=c("Novak Djokovic","Roger Federer","Andy Murray","Rafael Nadal","David Ferrer","Juan Martin Del Potro","Tomas Berdych","Milos Raonic","Stanislas Wawrinka","Kei Nishikori"))
B <- data.frame(group="2nd Forehand", value=c(0.520,0.496,0.486,0.548,0.520,0.457,0.517,0.447,0.452,0.498),player=c("Novak Djokovic","Roger Federer","Andy Murray","Rafael Nadal","David Ferrer","Juan Martin Del Potro","Tomas Berdych","Milos Raonic","Stanislas Wawrinka","Kei Nishikori"))
C <- data.frame(group="1st Backhand", value=c(0.302,0.296,0.303,0.341,0.209,0.243,0.250,0.246,0.266,0.255),player=c("Novak Djokovic","Roger Federer","Andy Murray","Rafael Nadal","David Ferrer","Juan Martin Del Potro","Tomas Berdych","Milos Raonic","Stanislas Wawrinka","Kei Nishikori"))
D <- data.frame(group="2nd Backhand", value=c(0.543,0.482,0.501,0.514,0.467,0.467,0.503,0.460,0.470,0.526),player=c("Novak Djokovic","Roger Federer","Andy Murray","Rafael Nadal","David Ferrer","Juan Martin Del Potro","Tomas Berdych","Milos Raonic","Stanislas Wawrinka","Kei Nishikori"))
hetero <- rbind(A,B,C,D) #row combine

#Create new variable with same value as desired legend label
hetero$mean<-rep('Group Average', 40) 
colnames(hetero)<-c("group","value","player","Group Average") #change column name to legend label
hetero<-subset(hetero, select = -c(yo,k) )#remove columns titled yo and k

#Plot
HeteroReturn<-ggplot(hetero, aes(x=group, y=value,color=player)) +
     geom_point(size=6, alpha=0.9, shape=18,position=position_jitter(w=0.15, h=0),show.legend = TRUE) +
     scale_x_discrete(name="Return")+
     scale_y_continuous(name="Win Rate",breaks=seq(0.2,0.6,0.05))+
     geom_segment(aes(x = 0.65, y = 0.303, xend = 1.35, yend = 0.303),linetype="longdash",color="black",size=0.75)+
     geom_segment(aes(x = 1.65, y = 0.494, xend = 2.35, yend = 0.494),linetype="longdash",color="black",size=0.75)+
     geom_segment(aes(x = 2.65, y = 0.271, xend = 3.35, yend = 0.271),linetype="longdash",color="black",size=0.75)+
     geom_segment(aes(x = 3.65, y = 0.493, xend = 4.35, yend = 0.493),linetype="longdash",color="black",size=0.75)+
     scale_color_discrete(name = "Player")+
     labs(title="Returning Outcomes")+
   theme(plot.title=element_text(hjust=0.5))+
   theme(title = black.bold, axis.title = black.bold)
HeteroReturn


#geom_hline(yintercept, linetype, color, size)
#Black bold text/italicized
black.bold.italic.text <- element_text(face = "bold.italic", color = "black")
black.bold<-element_text(face="bold",color="black")
 
#ND-1stF=0.337 2ndF=0.540 1stB=0.302 2ndB=0.543
#RF-1stF=0.323 2ndF=0.496 1stB=0.296 2ndB=0.482
#AM-1stF=0.336 2ndF=0.486 1stB=0.303 2ndB=0.501
#RN-1stF=0.368 2ndF=0.548 1stB=0.341 2ndB=0.514
#DF- 1stF=0.270 2ndF=0.520 1stB=0.209 2ndB=0.467
#JMD- 1stF=0.294 2ndF=0.457 1stB=0.243 2ndB=0.467
#TB- 1stF=0.248 2ndF=0.517 1stB=0.250 2ndB=0.503
#MR- 1stF=0.278 2ndF=0.447 1stB=0.246 2ndB=0.460
#SW- 1stF=0.297 2ndF=0.452 1stB=0.266 2ndB=0.470
#KN- 1stF=0.281 2ndF=0.498 1stB=0.255 2ndB=0.526

#Djokovic Win Ratio-Forehand vs. Backhand Return
##FOREHAnd Return##
#1st Serve
BerdychvsRH$F1stWin<-ifelse(BerdychvsRH$Serving!="TB"&BerdychvsRH$isSvrWinner==0&(BerdychvsRH$Serve1stForehand==1|BerdychvsRH$Serve1stForehandAd==1),1,0)
BerdychvsRH$F1stTotal<-ifelse(BerdychvsRH$Serving!="TB"&(BerdychvsRH$Serve1stForehand==1|BerdychvsRH$Serve1stForehandAd==1),1,0)
sum(BerdychvsRH$F1stWin,na.rm=T)/sum(BerdychvsRH$F1stTotal,na.rm=T) #0.323 win ratio
#2nd Serve
BerdychvsRH$F2ndWin<-ifelse(BerdychvsRH$Serving!="TB"&BerdychvsRH$isSvrWinner==0&(BerdychvsRH$Serve2ndForehand==1|BerdychvsRH$Serve2ndForehandAd==1),1,0)
BerdychvsRH$F2ndTotal<-ifelse(BerdychvsRH$Serving!="TB"&(BerdychvsRH$Serve2ndForehand==1|BerdychvsRH$Serve2ndForehandAd==1),1,0)
sum(BerdychvsRH$F2ndWin,na.rm=T)/sum(BerdychvsRH$F2ndTotal,na.rm=T) #0.496 win ratio

##BACKHAnd SERVE##
#1st Serve 
BerdychvsRH$B1stWin<-ifelse(BerdychvsRH$Serving!="TB"&BerdychvsRH$isSvrWinner==0&(BerdychvsRH$Serve1stBackhand==1|BerdychvsRH$Serve1stBackhandAd==1),1,0)
BerdychvsRH$B1stTotal<-ifelse(BerdychvsRH$Serving!="TB"&(BerdychvsRH$Serve1stBackhand==1|BerdychvsRH$Serve1stBackhandAd==1),1,0)
sum(BerdychvsRH$B1stWin,na.rm=T)/sum(BerdychvsRH$B1stTotal,na.rm=T) #0.296 win ratio

#2nd Serve
BerdychvsRH$B2ndWin<-ifelse(BerdychvsRH$Serving!="TB"&BerdychvsRH$isSvrWinner==0&(BerdychvsRH$Serve2ndBackhand==1|BerdychvsRH$Serve2ndBackhandAd==1),1,0)
BerdychvsRH$B2ndTotal<-ifelse(BerdychvsRH$Serving!="TB"&(BerdychvsRH$Serve2ndBackhand==1|BerdychvsRH$Serve2ndBackhandAd==1),1,0)
sum(BerdychvsRH$B2ndWin,na.rm=T)/sum(BerdychvsRH$B2ndTotal,na.rm=T) #0.482 win ratio



#First serves
deuceF1stServe<-c(0.757,0.710,0.703,0.707,0.739,0.725,0.694,0.674,0.807,0.776)
mean(deuceF1stServe) #0.7292
deuceB1stServe<-c(0.807,0.723,0.702,0.778,0.743,0.697,0.762,0.642,0.802,0.727)
mean(deuceB1stServe) #0.7383
adF1stServe<-c(0.767,0.689,0.717,0.698,0.688,0.677,0.737,0.633,0.752,0.795)
mean(adF1stServe) #0.7153
adB1stServe<-c(0.787,0.714,0.720,0.751,0.719,0.683,0.722,0.642,0.796,0.745)
mean(adB1stServe) #0.7279
#Second Serves
deuceF2ndServe<-c()
mean(deuceF2ndServe)
deuceB2ndServe<-c()
mean(deuceB2ndServe)
adF2ndServe<-c()
mean(adF2ndServe)
adB2ndServe<-c()
mean(adB2ndServe)

#Returns 1st serves
FF1stReturn<-c(0.472,0.504,0.476,0.457,0.429,0.496,0.461,0.423,0.435,0.431)
mean(FF1stReturn) #0.458
FB1stReturn<-c(0.589,0.516,0.553,0.483,0.518,0.457,0.503,0.510,0.445,0.432)
mean(FB1stReturn) #0.501
BF1stReturn<-c(0.429,0.455,0.451,0.382,0.358,0.339,0.374,0.320,0.385,0.329)
mean(BF1stReturn) #0.382
BB1stReturn<-c(0.497,0.542,0.462,0.497,0.467,0.532,0.449,0.410,0.382,0.478)
mean(BB1stReturn) #0.472
#Return 2nd serves
FF2ndReturn<-c(0.566,0.573,0.568,0.474,0.533,0.532,0.511,0.6,0.524,0.532)
mean(FF2ndReturn) #0.541
FB2ndReturn<-c(0.606,0.602,0.598,0.551,0.514,0.626,0.446,0.6,0.505,0.623)
mean(FB2ndReturn) #0.567
BF2ndReturn<-c(0.491,0.567,0.555,0.503,0.502,0.551,0.482,0.503,0.422,0.538)
mean(BF2ndReturn) #0.511
BB2ndReturn<-c(0.548,0.593,0.617,0.574,0.509,0.581,0.495,0.510,0.522,0.555)
mean(BB2ndReturn) #0.550

#Difference in w% after first two shots-Djoker-Fed
fedtwoshots<-c(0.594,0.556,0.691,0.601,0.542,0.395,0.553,0.499)
djokertwoshots<-c(0.581,0.522,0.642,0.532,0.505,0.514,0.593,0.53)
diff<-fedtwoshots-djokertwoshots

#Plot of two differences
plot(diff,ylab="Difference: Federer-Djokovic",xaxt="n",xlab="Two Shots",main="Federer vs Djokovic: Win %'s First Two Shots",pch=16,col= ifelse(diff >= 0, "green", "red"))
axis(1, at=1:8, labels=labels)
labels=c("FF","FB","BF","BB","FF2nd","FB2nd","BF2nd","BB2nd")


#Bar Chart of matches
#Rafa-99, Fed-115, Djoker-127, Murray-95, Del Potro-35, Berdych-41
#Ferrer-30, Nishikori-38, Raonic-34, Wawrinka-48
matches<-c(127,115,99,95,48,41,38,35,34,30)
players<-c("Djokovic","Federer","Nadal","Murray","Wawrinka","Berdych","Nishikori","Del Potro","Raonic","Ferrer")
MatchChart<-ggplot(data=NULL,aes(x=players,y=matches))+
  scale_x_discrete(name="Player")+
  scale_y_continuous(name="Matches",breaks=seq(0,140,20))+
  geom_bar(stat="identity",width=0.7,position=position_dodge(width=0.7),col="red",fill="red")+
  labs(title="Charted Match Distribution")+
  theme(plot.title=element_text(hjust=0.5),axis.text.x=(element_text(face="bold",size=10)))+
  geom_label(aes(label=c(127,115,99,95,48,41,38,35,34,30)),col="red", position=position_dodge(width=0.7), vjust=-0.1,show.legend = FALSE)+
  theme(title = red.bold.text, axis.title = red.bold.text)
MatchChart
#Keep in order of data
players <- factor(players, levels=unique(players))
#Axis text red, italicized and bold
red.bold.text <- element_text(face = "bold", color = "red")

###Rivalry Analysis-Federer Djokovic###




##ONLY RH RETURNERS##



#Robustness Check-2nd serve location
##Forehand##

#Location-Out Wide Deuce 2nd Serve Total
BerdychvsRH$Serve2ndFSpot<-ifelse(BerdychvsRH$Deuce==1&BerdychvsRH$SecondServeLocation==4&BerdychvsRH$X2ndIn==1,1,0)
sum(BerdychvsRH$Serve2ndFSpot,na.rm=T)
#Location-Up the T Ad 2nd Serve Total
BerdychvsRH$Serve2ndFSpotAd<-ifelse(BerdychvsRH$Ad==1&BerdychvsRH$SecondServeLocation==6&BerdychvsRH$X2ndIn==1,1,0)
#Location-Up the T Deuce 2nd Serve Total
BerdychvsRH$Serve2ndBSpot<-ifelse(BerdychvsRH$Deuce==1&BerdychvsRH$SecondServeLocation==6&BerdychvsRH$X2ndIn==1,1,0)
#Location-Out wide Ad 2nd Serve Total
BerdychvsRH$Serve2ndBSpotAd<-ifelse(BerdychvsRH$Ad==1&BerdychvsRH$SecondServeLocation==4&BerdychvsRH$X2ndIn==1,1,0)
         

##FOREHAnd 2nd SERVE##
#Deuce Side
BerdychvsRH$DeuceF2ndWin<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$isSvrWinner==1&BerdychvsRH$Serve2ndFSpot==1,1,0)
BerdychvsRH$DeuceF2ndTotal<-ifelse(BerdychvsRH$Serving=="TB"& BerdychvsRH$Serve2ndFSpot==1,1,0)
sum(BerdychvsRH$DeuceF2ndWin,na.rm=T)/sum(BerdychvsRH$DeuceF2ndTotal,na.rm=T) #0.590 win ratio
sum(BerdychvsRH$DeuceF2ndWin,na.rm=T) #252
sum(BerdychvsRH$DeuceF2ndTotal,na.rm=T) #427
#Ad
BerdychvsRH$AdF2ndWin<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$isSvrWinner==1&BerdychvsRH$Serve2ndFSpotAd==1,1,0)
BerdychvsRH$AdF2ndTotal<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$Serve2ndFSpotAd==1,1,0)
sum(BerdychvsRH$AdF2ndWin,na.rm=T)/sum(BerdychvsRH$AdF2ndTotal,na.rm=T) #0.644 win ratio
sum(BerdychvsRH$AdF2ndWin,na.rm=T) #217
sum(BerdychvsRH$AdF2ndTotal,na.rm=T) #337


##BACKHAnd 2nd SERVE##
#Deuce Side
BerdychvsRH$DeuceB2ndWin<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$isSvrWinner==1&BerdychvsRH$Serve2ndBSpot==1,1,0)
BerdychvsRH$DeuceB2ndTotal<-ifelse(BerdychvsRH$Serving=="TB"& BerdychvsRH$Serve2ndBSpot==1,1,0)
sum(BerdychvsRH$DeuceB2ndWin,na.rm=T)/sum(BerdychvsRH$DeuceB2ndTotal,na.rm=T) #0.607 win ratio
sum(BerdychvsRH$DeuceB2ndWin,na.rm=T) #86
sum(BerdychvsRH$DeuceB2ndTotal,na.rm=T) #151
#Ad
BerdychvsRH$AdB2ndWin<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$isSvrWinner==1&BerdychvsRH$Serve2ndBSpotAd==1,1,0)
BerdychvsRH$AdB2ndTotal<-ifelse(BerdychvsRH$Serving=="TB"&BerdychvsRH$Serve2ndBSpotAd==1,1,0)
sum(BerdychvsRH$AdB2ndWin,na.rm=T)/sum(BerdychvsRH$AdB2ndTotal,na.rm=T) #0.603 win ratio
sum(BerdychvsRH$AdB2ndWin,na.rm=T) #119
sum(BerdychvsRH$AdB2ndTotal,na.rm=T) #192

##Robustness checks show that when accounting for serve location versus return shot hit
##Significance does not change-players mainly optimizing-and just increased w% due to no middle/body serves

RStudio.Version() #Cite in paper

#Lastly, I need to test serial independence
#To do this, I need to count runs for F/B on each 1st, 2nd serves
#and returns for each player
#Using that, I then use the statistical technique to assign
#the probabilities of obtaining x runs or more and y runs or less
#for a 0.05 total p-value
rm(list=ls())
BerdychvsRH<-read.csv(file="BerdychvsRH.csv",header=TRUE,sep=",")

#1=forehand, -1=backhand location
#1st serve each court side, 2nd serve the same
#Will assume match to match doesnt matter-all straight through (error)
BerdychvsRHServing<-subset(BerdychvsRH,BerdychvsRH$Serving=="TB")
BerdychvsRHServing$ServeSpot<-ifelse(BerdychvsRHServing$FirstShot=="f"|BerdychvsRHServing$FirstShot=="r"|BerdychvsRHServing$FirstShot=="l"|BerdychvsRHServing$FirstShot=="u"|(BerdychvsRHServing$FirstServeLocation==4&BerdychvsRHServing$Deuce==1&BerdychvsRHServing$rallyCount==1)|(BerdychvsRHServing$SecondServeLocation==4&BerdychvsRHServing$Deuce==1&BerdychvsRHServing$rallyCount==1)|(BerdychvsRHServing$FirstServeLocation==6&BerdychvsRHServing$Ad==1&BerdychvsRHServing$rallyCount==1)|(BerdychvsRHServing$SecondServeLocation==6&BerdychvsRHServing$Ad==1&BerdychvsRHServing$rallyCount==1)|(BerdychvsRHServing$rallyCount==0&BerdychvsRHServing$SecondServeLocation==5)|(BerdychvsRHServing$rallyCount==0&BerdychvsRHServing$Deuce==1&BerdychvsRHServing$SecondServeLocation==4)|(BerdychvsRHServing$rallyCount==0&BerdychvsRHServing$Ad==1&BerdychvsRHServing$SecondServeLocation==6),1,-1)
#Now stratify by court side + 1st/2nd serve
BerdychvsRHServingDeuce<-subset(BerdychvsRHServing,BerdychvsRHServing$Deuce==1)
BerdychvsRHServingAd<-subset(BerdychvsRHServing,BerdychvsRHServing$Ad==1)
#Deuce Side
BerdychvsRHServingDeuce1st<-subset(BerdychvsRHServingDeuce,BerdychvsRHServingDeuce$XX1stIn==1)
BerdychvsRHServingDeuce2nd<-subset(BerdychvsRHServingDeuce,BerdychvsRHServingDeuce$XX1stIn==0)
#Ad Side
BerdychvsRHServingAd1st<-subset(BerdychvsRHServingAd,BerdychvsRHServingAd$XX1stIn==1)
BerdychvsRHServingAd2nd<-subset(BerdychvsRHServingAd,BerdychvsRHServingAd$XX1stIn==0)

#Perform runs test using Wald-Wolfowitz-Deuce 1st Serve-Manually

BerdychRunslistdeuce<-BerdychvsRHServingDeuce1st[,c("match_id","ServeSpot")]
#I have each data frame in a list
test_deuce1st<-split(BerdychRunslistdeuce,BerdychRunslistdeuce$match_id)
#With each deuce point data frame, I want to test for number of runs, then add
#Use lapply on each data frame in list to get runs
runs_deuce1st<-lapply(test_deuce1st,"[",,2)

#Now in runs for deuce on 1st serve, count number in each df
runs_deuce1st<-sapply(runs_deuce1st, function(x) rle(x))
#Now I want to sum the lengths of each to get total runs on deuce1st
#Double the number of total runs below
runs_deuce1stcount<-sapply(runs_deuce1st, function(x) length(x))
#So divide by 2-TOTAL RUNS
sum(runs_deuce1stcount)/2 #496 runs on deuce 1st serve
#Calculate Wald–Wolfowitz runs test statistic
#407 Backhands (-1), 443 Forehands(1)
r_expected=(2*407*443/(407+443)+1) #1700.97
sd_runs=(2*407*443*(2*407*443-407-443))/((407+443)^2 *(407+443-1))
z_score=(496-r_expected)/(sqrt(sd_runs)) #4.866

##AD SIDE FIRST SERVE##
#Perform runs test using Wald-Wolfowitz-AD 1st Serve-Manually

BerdychRunslistad<-BerdychvsRHServingAd1st[,c("match_id","ServeSpot")]
#I have each data frame in a list
test_ad1st<-split(BerdychRunslistad,BerdychRunslistad$match_id)
#With each deuce point data frame, I want to test for number of runs, then add
#Use lapply on each data frame in list to get runs
runs_ad1st<-lapply(test_ad1st,"[",,2)

#Now in runs for ad on 1st serve, count number in each df
runs_ad1st<-sapply(runs_ad1st, function(x) rle(x))
#Now I want to sum the lengths of each to get total runs on deuce1st
#Double the number of total runs below
runs_ad1stcount<-sapply(runs_ad1st, function(x) length(x))
#So divide by 2-TOTAL RUNS
sum(runs_ad1stcount)/2 #405 runs on ad 1st serve
#Calculate Wald–Wolfowitz runs test statistic
#300 Backhands (-1), 389 Forehands(1)
r_expected=(2*300*389/(300+389)+1) #1523.97
sd_runs=(2*300*389*(2*300*389-300-389))/((300+389)^2 *(300+389-1))
z_score=(405-r_expected)/(sqrt(sd_runs)) #5.060

#DEUCE 2ND SERVE#

BerdychRunslistdeuce2<-BerdychvsRHServingDeuce2nd[,c("match_id","ServeSpot")]
#I have each data frame in a list
test_deuce2nd<-split(BerdychRunslistdeuce2,BerdychRunslistdeuce2$match_id)
#With each deuce point data frame, I want to test for number of runs, then add
#Use lapply on each data frame in list to get runs
runs_deuce2nd<-lapply(test_deuce2nd,"[",,2)

#Now in runs for deuce on 1st serve, count number in each df
runs_deuce2nd<-sapply(runs_deuce2nd, function(x) rle(x))
#Now I want to sum the lengths of each to get total runs on deuce1st
#Double the number of total runs below
runs_deuce2ndcount<-sapply(runs_deuce2nd, function(x) length(x))
#So divide by 2-TOTAL RUNS
sum(runs_deuce2ndcount)/2 #286 runs on deuce 2nd serve
#Calculate Wald–Wolfowitz runs test statistic
#365 Backhands (-1), 211 Forehands(1)
r_expected=(2*365*211/(365+211)+1) #824.8
sd_runs=(2*365*211*(2*365*211-365-211))/((365+211)^2 *(365+211-1))
z_score=(286-r_expected)/(sqrt(sd_runs)) #1.580

##AD SIDE Second SERVE##

BerdychRunslistad2<-BerdychvsRHServingAd2nd[,c("match_id","ServeSpot")]
#I have each data frame in a list
test_ad2nd<-split(BerdychRunslistad2,BerdychRunslistad2$match_id)
#With each deuce point data frame, I want to test for number of runs, then add
#Use lapply on each data frame in list to get runs
runs_ad2nd<-lapply(test_ad2nd,"[",,2)

#Now in runs for ad on 2nd serve, count number in each df
runs_ad2nd<-sapply(runs_ad2nd, function(x) rle(x))
#Now I want to sum the lengths of each to get total runs on deuce1st
#Double the number of total runs below
runs_ad2ndcount<-sapply(runs_ad2nd, function(x) length(x))
#So divide by 2-TOTAL RUNS
sum(runs_ad2ndcount)/2 #253 runs on ad 2nd serve
#Calculate Wald–Wolfowitz runs test statistic
#423 Backhands (-1), 159 forehands(1)
r_expected=(2*423*159/(423+159)+1) #775.5
sd_runs=(2*423*159*(2*423*159-423-159))/((423+159)^2 *(423+159-1))
z_score=(253-r_expected)/(sqrt(sd_runs)) #2.182



             