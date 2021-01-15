##########################################################################
##########################################################################
# Reliability of web-based affective auditory stimuli presentation
# Seow & Hauser, 2021

#### clear all ####
rm(list = ls())
options(scipen = 999)

##### packages ####
library(plyr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(lmerTest)
library(lme4)
library(ggrepel)
library("utf8")
rho<- utf8_print("\u03C1")

##### functions ####
logposition <- function(value, min, max) {
  minp <- 0
  maxp <- 100
  
  
  minv <- log(min)
  maxv <- log(max)
  
  
  scale <- (maxv - minv) / (maxp - minp)
  
  return((log(value) - minv) / scale + minp)
}

# functions that calculate the correlation between 2 ratings of the same thing for each participant
valFunc <- function(xx) {
  return(data.frame(COR = cor(xx$valRating.x, xx$valRating.y), method = c("spearman")))
}
arouFunc <- function(xx) {
  return(data.frame(COR = cor(xx$arouRating.x, xx$arouRating.y), method = c("spearman")))
}


#valFuncPear <- function(xx) {
#  return(data.frame(COR = cor(xx$valRating.x, xx$valRating.y), method = c("pearson")))
#}
#arouFuncPear <- function(xx) {
#  return(data.frame(COR = cor(xx$arouRating.x, xx$arouRating.y), method = c("pearson")))
#}


# sort into indiv ratings per sound for the internal consistency measure
indivRatings <- function(sound, scale) {
  sound.1 <- indivData[indivData$soundFocus == sound, ]
  sound.1Cut <- t(sound.1[, c(scale)])
  
  return(sound.1Cut)
}

#### set folder ####
setwd("C:/Users/dream/OneDrive - University College London/Lab/Projects/audioPilot/data/")


# if save figures automatically
print = 1
printFolder= "C:/Users/dream/OneDrive - University College London/Lab/Projects/audioPilot/figures/clean/"


#### import data ####
freqData <- data.frame(read.csv("audio_pilot_freq.csv", header = T)) # load ratings data
taskData <- data.frame(read.csv("audio_pilot_ratings.csv", header = T)) # load task performance data
questData <- data.frame(read.csv("audio_pilot_quest.csv", header = T)) # load quest data


##########################################################################
##########################################################################
#### Fig 1. Adjusted volume and frequency ####

# a) Volume ##############################################################
taskVol <- taskData[taskData$volumePer == 1.0, ]

require(dplyr)
taskVol <- taskVol %>%
  group_by(userID) %>%
  summarise(volume = max(volume))
taskVol$volNotLog <- logposition(taskVol$volume, 1, 100)

volPlotDat <- data.frame("userID" = taskVol$userID, "volume" = taskVol$volNotLog)

# volume plot
plotDatmelt <- reshape2::melt(volPlotDat, id.vars = c("userID"))
Fig1a <- ggplot(plotDatmelt, aes(x = variable, y = value)) +
  geom_jitter(position = position_jitter(width = 0.1, height = 0.1), shape = 21, colour = "black", size = 3, stroke = 1.2) +
  # geom_dotplot(binaxis='y', stackdir='center',position="jitter",width = 1)+
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", color = "red", size = 1, position = position_nudge(x = 0.35, y = 0)) +
  labs(
    title = " ",
    y = "Volume\n(position on log scale)",
    x = " "
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold")
  ) + ylim(0,105)


# b) Frequency ##############################################################
# frequency plot
plotDatmelt <- melt(freqData, id.vars = c("userID"))
Fig1b <- ggplot(plotDatmelt, aes(x = variable, y = value)) +
  geom_jitter(position = position_jitter(width = 0.1, height = 0.1), size = 3, color = "black", shape = 21, stroke = 1.2) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", color = "red", size = 1, position = position_nudge(x = 0.3, y = 0)) +
  labs(
    title = " ",
    y = "Frequency (Hz)",
    x = " "
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold")
  ) +
  scale_x_discrete(labels = c(
    "freqChosen" = "Max Audible\nFrequency", "freq1" = "Frequency 1",
    "freq2" = "Frequency 2"
  ))


#### Supplementary Fig 1. Adjusted frequency x age ####
freqQuest <- Reduce(
  function(dtf1, dtf2) merge(dtf1, dtf2, by = "userID", all.x = TRUE),
  list(freqData, questData)
)

cor.test(freqQuest$age,freqQuest$freqChosen)

SFig1 <- ggplot(freqQuest, aes(x = age, y = freqChosen)) +
  # geom_point(color = "black", size = 3, alpha=0.5) +
  geom_jitter(position = position_jitter(width = 0.1, height = 0.1), size = 3, color = "black", shape = 21, stroke = 1.2) +
  geom_smooth(
    method = lm, linetype = "dashed",
    color = "black", fill = "grey", size = 1.5
  ) +
  labs(
    title = " ",
    y = "Adjusted frequency threshold (Hz)",
    x = "Age"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold")
  )+ ylim(2500,22500)



##########################################################################
##########################################################################
#### Fig 2. High reliablity of affective ratings


audioRateUser <- ddply(taskData, c("userID", "soundFocus", "volumePer"), summarise,
                       meanRT = mean(qnRT), sdRT = sd(qnRT),
                       meanVal = mean(valRating), sdVal = sd(valRating),
                       meanArou = mean(arouRating), sdArou = sd(arouRating)
)


audioRateFinal <- ddply(audioRateUser, c("soundFocus", "volumePer"), summarise,
                        meanRTAll = mean(meanRT), sdRTAll = sd(meanRT) ,
                        meanValAll = mean(meanVal), sdValAll = sd(meanVal) ,
                        meanArouAll = mean(meanArou), sdArouAll = sd(meanArou) 
)


audioRatePart <- ddply(taskData, c("userID","volumePer","rateTime"), summarise,
                       meanRT = mean(qnRT), sdRT = sd(qnRT),
                       meanVal = mean(valRating), sdVal = sd(valRating),
                       meanArou = mean(arouRating), sdArou = sd(arouRating)
)

audioRatePart2 <- ddply(taskData, c("userID","rateTime"), summarise,
                        meanRT = mean(qnRT), sdRT = sd(qnRT),
                        meanVal = mean(valRating), sdVal = sd(valRating),
                        meanArou = mean(arouRating), sdArou = sd(arouRating)
)


# a) Test re-test ########################################################


# compare between the first and second rating of the same sound
firstRate <- taskData[taskData$rateTime == 1, ]
secondRate <- taskData[taskData$rateTime == 2, ]

compareConst <- Reduce(
  function(dtf1, dtf2) merge(dtf1, dtf2, by = c("userID", "soundFocus", "volumePer"), all.x = TRUE),
  list(firstRate[, c("userID", "soundFocus", "volumePer", "valRating", "arouRating")], secondRate[, c("userID", "soundFocus", "volumePer", "valRating", "arouRating")])
)

# if divide by volume
lowVolCompare <- compareConst[compareConst$volumePer == 0.5, ]
highVolCompare <- compareConst[compareConst$volumePer == 1.0, ]

# spearman correlation for 2 x 2 conditions of volume x scaleType
# reliability across all sounds
valRelibleLow <- ddply(lowVolCompare, .(userID), valFunc)
arouRelibleLow <- ddply(lowVolCompare, .(userID), arouFunc)
valRelibleHigh <- ddply(highVolCompare, .(userID), valFunc)
arouRelibleHigh <- ddply(highVolCompare, .(userID), arouFunc)

# pearson's
#valRelibleLow <- ddply(lowVolCompare, .(userID), valFuncPear)
#arouRelibleLow <- ddply(lowVolCompare, .(userID), arouFuncPear)
#valRelibleHigh <- ddply(highVolCompare, .(userID), valFuncPear)
#arouRelibleHigh <- ddply(highVolCompare, .(userID), arouFuncPear)

colnames(valRelibleLow) <- c("userID", "cor", "Volume")
valRelibleLow$Volume <- 0.5
valRelibleLow$Rating <- "Valence"

colnames(valRelibleHigh) <- c("userID", "cor", "Volume")
valRelibleHigh$Volume <- 1.0
valRelibleHigh$Rating <- "Valence"

colnames(arouRelibleLow) <- c("userID", "cor", "Volume")
arouRelibleLow$Volume <- 0.5
arouRelibleLow$Rating <- "Arousal"

colnames(arouRelibleHigh) <- c("userID", "cor", "Volume")
arouRelibleHigh$Volume <- 1.0
arouRelibleHigh$Rating <- "Arousal"

relValTemp <- rbind(valRelibleLow, valRelibleHigh)
relArouTemp <- rbind(arouRelibleLow, arouRelibleHigh)

colnames(relValTemp) <- c("userID", "Cor", "Volume", "Rating")
colnames(relArouTemp) <- c("userID", "Cor", "Volume", "Rating")
relAll <- rbind(relValTemp, relArouTemp)


ylabel<- paste('Correlation (',rho,')\nbetween repeated ratings', sep="")        

Fig2a <- ggplot(relAll, aes(x = as.factor(Rating), y = Cor, color = as.factor(Volume * 100))) +
  geom_jitter(position = position_jitter(width = 0.15, height = 0), size = 3, shape = 21, stroke = 1.2) +
  scale_color_manual(values = c("#ffa600", "#003f5c")) +
  stat_summary(
    fun.data = mean_sdl, fun.args = list(mult = 1), aes(group = as.factor(Volume * 100)),
    geom = "pointrange", color = c("#ffa600", "#ffa600", "#003f5c", "#003f5c"), size = 1, position = position_nudge(x = 0.4, y = 0)
  ) +
  labs(
    title = " ",
    y = ylabel,
    x = "Affective Scale",
    color = "Volume (%)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  )


#### Regressions of reliaility and volume

comRelBind <- rbind(valRelibleLow, valRelibleHigh, arouRelibleLow, arouRelibleHigh)
comRelBind$Volume.f <- factor(comRelBind$Volume)
comRelBind$Rating.f <- factor(comRelBind$Rating)

# main effect of volume and scaleType
relReg <- lmer(cor ~ Volume.f + Rating.f + (1 + Volume.f + Rating.f | userID), comRelBind)
summary(relReg)


# b) Intraindividual Test re-test ########################################################

rateTime1<-audioRatePart[audioRatePart$rateTime==1,]
rateTime2<-audioRatePart[audioRatePart$rateTime==2,]

rateComp <- Reduce(
  function(dtf1, dtf2) merge(dtf1, dtf2, by = c("userID","volumePer"), all.x = TRUE),
  list(rateTime1, rateTime2)
)

cor.test(rateComp$meanVal.x,rateComp$meanVal.y, method= "spearman",exact=FALSE)
cor.test(rateComp$sdVal.x,rateComp$sdVal.y, method= "spearman",exact=FALSE)
cor.test(rateComp$meanArou.x,rateComp$meanArou.y, method= "spearman",exact=FALSE)
cor.test(rateComp$sdArou.x,rateComp$sdArou.y, method= "spearman",exact=FALSE)

Fig2b1<-ggplot(rateComp, aes(x = meanVal.x, y = meanVal.y, colour = as.factor(volumePer*100))) +
  geom_point(shape = 21, size = 3, stroke = 1.2, alpha =0.8) +
  scale_color_manual(values=c("#ffa600", "#003f5c"))+
  geom_smooth(
    method = lm, linetype = "dashed",
    fill = "grey", size = 1.5,se=F
  ) +
  labs(
    title = " ",
    y = "Mean valence rating from\nsecond sound presentation",
    x = "Mean valence rating from\nfirst sound presentation",
    colour = "Volume (%)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12),
    legend.position = "none"
  ) + xlim(10,65) + ylim(10,65)


Fig2b2<-ggplot(rateComp, aes(x = meanArou.x, y = meanArou.y, colour = as.factor(volumePer*100))) +
  geom_point(shape = 21, size = 3, stroke = 1.2, alpha =0.8) +
  scale_color_manual(values=c("#ffa600", "#003f5c"))+
  geom_smooth(
    method = lm, linetype = "dashed",
    fill = "grey", size = 1.5,se=F
  ) +
  labs(
    title = " ",
    y = "Mean arousal rating from\nsecond sound presentation",
    x = "Mean arousal rating from\nfirst sound presentation",
    colour = "Volume (%)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12),
    legend.position = "none"
  )+ xlim(30,100) + ylim(30,100)



Fig2b3<-ggplot(rateComp, aes(x = sdVal.x, y = sdVal.y, colour = as.factor(volumePer*100))) +
  geom_point(shape = 21, size = 3, stroke = 1.2, alpha =0.8) +
  scale_color_manual(values=c("#ffa600", "#003f5c"))+
  geom_smooth(
    method = lm, linetype = "dashed",
    fill = "grey", size = 1.5,se=F
  ) +
  labs(
    title = " ",
    y = "Variance (SD) of valence ratings\nfrom second sound presentation",
    x = "Variance (SD) of valence ratings\nfrom first sound presentation",
    colour = "Volume (%)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12),
    legend.position = "none"
  )+ xlim(10,52) + ylim(10,52)


Fig2b4<-ggplot(rateComp, aes(x = sdArou.x, y = sdArou.y, colour = as.factor(volumePer*100))) +
  geom_point(shape = 21, size = 3, stroke = 1.2, alpha =0.8) +
  scale_color_manual(values=c("#ffa600", "#003f5c"))+
  geom_smooth(
    method = lm, linetype = "dashed",
    fill = "grey", size = 1.5,se=F
  ) +
  labs(
    title = " ",
    y = "Variance (SD) of arousal ratings\nfrom second sound presentation",
    x = "Variance (SD) of arousal ratings\nfrom first sound presentation",
    colour = "Volume (%)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12),
    legend.position = "none"
  )+ xlim(0,45) + ylim(0,45)



##########################################################################
##########################################################################
#### Internal consistency for ratings

idList <- unique(taskData$userID)

allVal <- data.frame()
allArou <- data.frame()

# for every participant,
for (i in 1:length(idList)) {
  indivData <- taskData[taskData$userID == idList[i], ]
  indivData <- indivData[with(indivData, order(indivData$userID, indivData$volumePer, indivData$qnNum)), ] # make sure it is ordered
  
  valScale <- NULL
  arouScale <- NULL
  colAll <- NULL
  
  # for every sound,
  for (s in 1:length(unique(taskData$soundFocus))) {
    
    # permute ratings such that vector includes first 2 is 0.5 volume ratings, then 1.0 volume ratings for one particular sound
    sound.title <- unique(taskData$soundFocus)[s]
    
    sound.val <- indivRatings(sound.title, "valRating")
    sound.arou <- indivRatings(sound.title, "arouRating")
    
    sound.title.1 <- paste(sound.title, "0.5-1", sep = " ")
    sound.title.2 <- paste(sound.title, "0.5-2", sep = " ")
    sound.title.3 <- paste(sound.title, "1.0-1", sep = " ")
    sound.title.4 <- paste(sound.title, "1.0-2", sep = " ")
    
    colID <- c(sound.title.1, sound.title.2, sound.title.3, sound.title.4)
    
    valScale <- cbind(valScale, sound.val)
    arouScale <- cbind(arouScale, sound.arou)
    colAll <- cbind(colAll, colID)
  }
  colAll <- as.vector(colAll, mode = "any")
  valScale <- data.frame(valScale)
  arouScale <- data.frame(arouScale)
  colnames(valScale) <- c(colAll)
  colnames(arouScale) <- c(colAll)
  valScale$userID <- idList[i]
  arouScale$userID <- idList[i]
  
  allVal <- rbind(allVal, valScale)
  allArou <- rbind(allArou, arouScale)
}


allValCo <- allVal[, c(1:60)]
allArouCo <- allArou[, c(1:60)]

psych::alpha(allValCo, check.keys = TRUE)$total$std.alpha
psych::alpha(allArouCo, check.keys = TRUE)$total$std.alpha

library(goeveg)
cv(audioRateFinal$meanValAll, na.rm = FALSE)
cv(audioRateFinal$meanArouAll, na.rm = FALSE)


##########################################################################
##########################################################################
#### Suppl Fig 2. Intra-class (sound) correlation 

# Correlation for each sound separately across participants

allsound.frame <- data.frame()
for (t in seq(1, length(colAll), by = 4)) {
  
  # correlate the 0.5 vol
  sound <- substr(colAll[t],1,nchar(colAll[t])-6)
  sound.1.cor<-  cor.test(allValCo[[colAll[t]]], allValCo[[colAll[t + 1]]],method = c("spearman"), exact=FALSE)
  
  # correlate the 1.0 vol
  sound.2.cor<-cor.test(allValCo[[colAll[t + 2]]], allValCo[[colAll[t + 3]]],method = c("spearman"), exact=FALSE)
  
  sound.frame<-data.frame(sound,sound.1.cor$estimate,sound.1.cor$statistic,sound.1.cor$p.value,
                          sound.2.cor$estimate,sound.2.cor$statistic,sound.2.cor$p.value)
  colnames(sound.frame)<-c("Sound", "0.5_Rel", "0.5_S", "0.5_p", "1.0_Rel", "1.0_S", "1.0_p")
  
  allsound.frame<- rbind(allsound.frame,sound.frame)
}

# plot
xlabel<- paste('Correlation (',rho,') between repeated\nratings at 50% volume', sep="")
ylabel<- paste('Correlation (',rho,') between repeated\nratings at 100% volume', sep="")               

SFig2<- ggplot(allsound.frame, aes(x = `0.5_Rel`, y = `1.0_Rel`)) +
  geom_point(color = "black", size = 3) + 
  labs(
    title = "",
    x= xlabel,
    y = ylabel
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold")
  ) + 
  xlim(0.55, 0.8) +   ylim(0.55,0.8)+
  geom_text_repel(aes(label = Sound), point.padding = 0.3)


mean(allsound.frame$`0.5_Rel`)
sd(allsound.frame$`0.5_Rel`)

mean(allsound.frame$`1.0_Rel`)
sd(allsound.frame$`1.0_Rel`)

allsound.frame[order(allsound.frame$`0.5_Rel`),]
allsound.frame[order(allsound.frame$`1.0_Rel`),]

t.test(allsound.frame$`0.5_Rel`,allsound.frame$`1.0_Rel`)


##########################################################################
##########################################################################
#### Fig 3. Correlation with prior studies
library("readxl")
IADSE <- data.frame(read_excel("C:/Users/dream/Documents/MEGAsync/Experimental Tasks/IADS-E/Sound Ratings.xlsx")) # load check data

# female scream (0276), cicada chirps (0335), running water (0921) and a piano melody (1360)
soundsUsed<- c("0276","0335", "0921","1360" )
IADSEleft <- IADSE[IADSE$Sound.ID %in% soundsUsed, ]
IADSEleft<- IADSEleft[,c("Description","AroMN","ValMN")]
colnames(IADSEleft)<-c("Sound","IADSE_Arou","IADSE_Val")
IADSEleft$Sound<-c("Scream\n(IADS-E; Yang et al., 2018)","Cicada\n(IADS-E; Yang et al., 2018)","Sea Wave\n(IADS-E; Yang et al., 2018)","Piano Melody\n(IADS-E; Yang et al., 2018)")
IADS2<-c( "Scream\n(Morriss et al., 2015; 2016; 2020)", 7.79, 1.63 ) 

IADSEleft<-rbind(IADSEleft,IADS2)

audioComp<-audioRateFinal[audioRateFinal$volumePer==1.0,]
audioUsed<- c("Cicada\n(IADS-E; Yang et al., 2018)","Piano Melody\n(IADS-E; Yang et al., 2018)", "Scream\n(IADS-E; Yang et al., 2018)","Sea Wave\n(IADS-E; Yang et al., 2018)","Scream\n(Morriss et al., 2015; 2016; 2020)")
audioLeft <- audioComp[audioComp$soundFocus %in% audioUsed, ]
audioLeft<- audioLeft[,c("soundFocus","meanValAll","meanArouAll")]
colnames(audioLeft)<-c("Sound","Cur_Val","Cur_Arou")


compData <- Reduce(
  function(dtf1, dtf2) merge(dtf1, dtf2, by = "Sound", all.x = TRUE),
  list(IADSEleft, audioLeft)
)

compData[,c(2:4)]<- lapply(compData[,c(2:4)],as.numeric)

cor.test(compData$IADSE_Arou,compData$Cur_Arou, method="spearman", exact=FALSE)
cor.test(compData$IADSE_Val,compData$Cur_Val, method="spearman", exact=FALSE)

cor.test(compData$IADSE_Arou,compData$Cur_Arou, method="pearson")
cor.test(compData$IADSE_Val,compData$Cur_Val, method="pearson")

Fig3a<- ggplot(compData, aes(x = IADSE_Arou, y = Cur_Arou)) +
  geom_point(color = "black", size = 3) + 
  geom_smooth(
    method = lm, se=FALSE ,linetype = "dashed",
    color = "grey", size = 1.2, alpha=0.5
  ) +
  labs(
    title = " ",
    x= "Arousal Rating\n(Prior studies)",
    y = "Arousal Rating\n(Seow & Hauser, 2021)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold")
  ) + 
  xlim(0, 10) +   ylim(0,100)+
  geom_text_repel(aes(label = Sound), point.padding = 0.5)


Fig3b<- ggplot(compData, aes(x = IADSE_Val, y = Cur_Val)) +
  geom_point(color = "black", size = 3) + 
  geom_smooth(
    method = lm, se=FALSE ,linetype = "dashed",
    color = "grey", size = 1.2, alpha=0.5
  ) +
  labs(
    title = " ",
    x= "Valance Rating\n(Prior studies)",
    y = "Valance Rating\n(Seow & Hauser, 2021)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold")
  ) + 
  xlim(0, 10) +   ylim(0,100)+
  geom_text_repel(aes(label = Sound), point.padding = 0.5)


## What about variance
# audioRateVar <- ddply(audioRateUser, c("soundFocus", "volumePer"), summarise,
#                       meanValAll = mean(meanVal/10), sdValAll = sd(meanVal/10) ,
#                       meanArouAll = mean(meanArou/10), sdArouAll = sd(meanArou/10) 
# )

# soundsUsed<- c("0276","0335", "0921","1360" )
# IADSEVar <- IADSE[IADSE$Sound.ID %in% soundsUsed, ]
# IADSEVar<- IADSEVar[,c("Description","AroMN", "AroSD","ValMN","ValSD")]
# IADSEVar$Sound<-c("Scream\n(IADS-E; Yang et al., 2018)","Cicada\n(IADS-E; Yang et al., 2018)","Sea Wave\n(IADS-E; Yang et al., 2018)","Piano Melody\n(IADS-E; Yang et al., 2018)")
# IADSEVar$AroCV<-IADSEVar$AroSD/IADSEVar$AroMN*100
# IADSEVar$ValCV<-IADSEVar$ValSD/IADSEVar$ValMN*100
# 
# audioComp<-audioRateVar[audioRateFinal$volumePer==1.0,]
# audioUsed<- c("Cicada\n(IADS-E; Yang et al., 2018)","Piano Melody\n(IADS-E; Yang et al., 2018)", "Scream\n(IADS-E; Yang et al., 2018)","Sea Wave\n(IADS-E; Yang et al., 2018)","Scream\n(Morriss et al., 2015; 2016; 2020)")
# audioVar <- audioComp[audioComp$soundFocus %in% audioUsed, ]
# audioVar<- audioVar[,c("soundFocus","meanValAll","sdValAll","meanArouAll","sdArouAll")]
# audioVar$AroCV<-audioVar$sdArouAll/audioVar$meanArouAll*100
# audioVar$ValCV<-audioVar$sdValAll/audioVar$meanValAll*100
# 
# compDataVar <- Reduce(
#   function(dtf1, dtf2) merge(dtf1, dtf2, by = "Sound", all.x = TRUE),
#   list(IADSEVar, audioVar)
# )
# 


##########################################################################
##########################################################################
#### Fig 4. Ranking affective ratings

#### average ratings across its repeated presentation ####

# main effect of volume on ratings
valReg<- lmer(meanVal ~ volumePer + (1 + volumePer | userID), audioRateUser)
arouReg<- lmer(meanArou ~ volumePer + (1 + volumePer | userID), audioRateUser)

summary(valReg)
summary(arouReg)


#### plot the rankings ####

# a) Valence #############################################################
# Valence Grouped
rateOrderVal <- ddply(audioRateFinal, c("soundFocus"), summarise,meanVal = mean(meanValAll))
rateOrderVal<- rateOrderVal %>% arrange(desc(meanVal)) #arrange by most aversive on top                    
rateOrderVal$soundFocus <- factor(rateOrderVal$soundFocus)
audioRateFinal$ValOrder <- factor(audioRateFinal$soundFocus, levels = rateOrderVal$soundFocus)


Fig4a<-ggplot(audioRateFinal, aes(colour = as.factor(volumePer*100), y = ValOrder, x = meanValAll)) +
  geom_point( size = 3,position = position_dodge(width=0.55)) +
  geom_errorbarh(aes(xmax = meanValAll + sdValAll, xmin = meanValAll - sdValAll, height = 0.1), 
                 size =0.9,position = position_dodge(width=0.55))+
  # change color of bars
  scale_color_manual(values=c("#ffa600", "#003f5c"))+
  labs(
    title = "Valence Scale",
    y = " ",
    x = "Pleasantness", colour = "Volume (%)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12)
  )+ xlim (-5,105)



# b) Arousal #############################################################
# Arousal Grouped
rateOrderArou <- ddply(audioRateFinal, c("soundFocus"), summarise,meanArou = mean(meanArouAll))
rateOrderArou<- rateOrderArou %>% arrange(meanArou) #arrange by most aversive on top                    
rateOrderArou$soundFocus <- factor(rateOrderArou$soundFocus)
audioRateFinal$ArouOrder <- factor(audioRateFinal$soundFocus, levels = rateOrderArou$soundFocus)

Fig4b <- ggplot(audioRateFinal, aes(colour = as.factor(volumePer*100), y = ArouOrder, x = meanArouAll)) +
  geom_point( size = 3,position = position_dodge(width=0.55)) +
  geom_errorbarh(aes(xmax = meanArouAll + sdArouAll, xmin = meanArouAll - sdArouAll, height = 0.1), 
                 size =0.9,position = position_dodge(width=0.55))+
  # change color of bars
  scale_color_manual(values=c("#ffa600", "#003f5c"))+
  labs(
    title = "Arousal Scale",
    y = " ",
    x = "Awakeness", colour = "Volume (%)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12)
  )+ xlim (-5,105)


# testing the effect of the rating difference between low vs high volume per sound... 

valVol=c()
arouVol= c()
soundList = unique(audioRateUser$soundFocus)
for (i in 1:length(soundList)){
  
  sound = soundList[i]
  test1 = audioRateUser[audioRateUser$soundFocus == sound & audioRateUser$volumePer == 0.5, ]
  test2 = audioRateUser[audioRateUser$soundFocus == sound & audioRateUser$volumePer == 1.0, ]
  
  valTest =t.test(test1$meanVal, test2$meanVal, paired = TRUE )
  
  valVolTemp = c( sound, valTest$statistic,  valTest$conf.int[1],valTest$conf.int[2], valTest$p.value) 
  valVol = rbind(valVol,valVolTemp)
  
  arouTest =t.test(test1$meanArou, test2$meanArou, paired = TRUE )
  
  arouVolTemp = c( sound, arouTest$statistic,  arouTest$conf.int[1],arouTest$conf.int[2], arouTest$p.value) 
  arouVol = rbind(arouVol,arouVolTemp)
  
}

colnames(valVol)<- c("sound" , "t-value", "conf.int1", "conf.int2","p-value" ) 
colnames(arouVol)<- c("sound" , "t-value", "conf.int1", "conf.int2","p-value" ) 

valVol<-data.frame(valVol)
arouVol<-data.frame(arouVol)

valVol[,2:5] <- lapply(valVol[,2:5], as.numeric)
arouVol[,2:5] <- lapply(arouVol[,2:5], as.numeric)

valVol <- valVol[order(valVol$t.value),]
arouVol <- arouVol[order(arouVol$t.value),]


test1 = audioRateUser[audioRateUser$soundFocus == "Scream\n(Morriss et al., 2015; 2016; 2020)" & audioRateUser$volumePer == 1.0, ]
test2 = audioRateUser[audioRateUser$soundFocus == "Scream\n(IADS-E; Yang et al., 2018)" & audioRateUser$volumePer == 1.0, ]

test1 = audioRateUser[audioRateUser$soundFocus == "Scream\n(Morriss et al., 2015; 2016; 2020)" & audioRateUser$volumePer == 0.5, ]
test2 = audioRateUser[audioRateUser$soundFocus == "Scream\n(IADS-E; Yang et al., 2018)" & audioRateUser$volumePer == 0.5, ]


test1 = audioRateUser[audioRateUser$soundFocus == "Cicada\n(IADS-E; Yang et al., 2018)" & audioRateUser$volumePer == 1.0, ]
test2 = audioRateUser[audioRateUser$soundFocus == "Piano Melody\n(IADS-E; Yang et al., 2018)" & audioRateUser$volumePer == 1.0, ]

test1 = audioRateUser[audioRateUser$soundFocus == "Cicada\n(IADS-E; Yang et al., 2018)" & audioRateUser$volumePer == 0.5, ]
test2 = audioRateUser[audioRateUser$soundFocus == "Piano Melody\n(IADS-E; Yang et al., 2018)" & audioRateUser$volumePer == 0.5, ]


test1 = audioRateUser[audioRateUser$soundFocus == "Frequency 1\n(M = 8,033 Hz)" & audioRateUser$volumePer == 0.5, ]
test2 = audioRateUser[audioRateUser$soundFocus == "Frequency 2\n(M = 4,004 Hz)" & audioRateUser$volumePer == 0.5, ]

test1 = audioRateUser[audioRateUser$soundFocus == "Frequency 1\n(M = 8,033 Hz)" & audioRateUser$volumePer == 1.0, ]
test2 = audioRateUser[audioRateUser$soundFocus == "Frequency 2\n(M = 4,004 Hz)" & audioRateUser$volumePer == 1.0, ]

test1 = audioRateUser[audioRateUser$soundFocus == "5,000 Hz" & audioRateUser$volumePer == 0.5, ]
test2 = audioRateUser[audioRateUser$soundFocus == "800 Hz" & audioRateUser$volumePer == 0.5, ]

t.test(test1$meanVal, test2$meanVal, paired = TRUE )

t.test(test1$meanArou, test2$meanArou, paired = TRUE )



##########################################################################
##########################################################################
#### Suppl Fig 3. Valence vs Arousal ratings

SFig3a<-ggplot(audioRateUser, aes(colour = as.factor(volumePer*100), y = meanVal , x = meanArou)) +
  geom_jitter(position = position_jitter(width = 0.15, height = 0), size = 3, shape = 21, stroke = 1.2) +
  scale_color_manual(values = c("#ffa600", "#003f5c")) +
  labs(
    title = "",
    y = "Valence Rating",
    x = "Arousal Rating",
    colour = "Volume (%)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12)
  ) + xlim (-0.5,100.5)+ ylim (-0.5,100.5)

# Correlation of valence mean vs arousal mean, by volume
SFig3b<-ggplot(audioRateFinal, aes(colour = as.factor(volumePer*100), y = meanValAll, x = meanArouAll)) +
  geom_jitter(position = position_jitter(width = 0.15, height = 0), size = 3, shape = 21, stroke = 1.2) +
  scale_color_manual(values = c("#ffa600", "#003f5c")) +
  geom_smooth(
    method = lm, linetype = "dashed",
    fill = "grey", size = 1.5,se=F
  ) +
  labs(
    title = "",
    y = "Valence Rating (Mean)",
    x = "Arousal Rating (Mean)",
    colour = "Volume (%)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12)
  ) + xlim (0,100)+ ylim (0,100)

# Correlation of valence SD vs arousal SD, by volume
SFig3c<-ggplot(audioRateFinal, aes(colour = as.factor(volumePer*100), y = sdValAll, x = sdArouAll)) +
  geom_jitter(position = position_jitter(width = 0.15, height = 0), size = 3, shape = 21, stroke = 1.2) +
  scale_color_manual(values = c("#ffa600", "#003f5c")) +
  labs(
    title = "",
    y = "Valence (SD)",
    x = "Arousal (SD)",
    colour = "Volume (%)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12)
  ) + xlim (10,21)+ ylim (10,21)



##########################################################################
##########################################################################
#### Psychiatric questionnaires

# scaling the demographics
questData$gender <- factor(questData$gender) #female is 0, male is 1, other is 3?
questData$age.sc = scale(questData$age)

# scaling the quesitonaire scores
questData$STAIY1_total.sc = scale(questData$STAIY1_total) #state anxiety
questData$STAIY2_total.sc = scale(questData$STAIY2_total) #trait anxiety
questData$OCIR_total.sc = scale(questData$OCIR_total) #OCD

regData <- Reduce(
  function(dtf1, dtf2) merge(dtf1, dtf2, by = "userID", all.x = TRUE),
  list(audioRateUser, questData)
)

#### Suppl Fig 4. Questionnare score histograms 


SFig4a<-ggplot(questData, aes(x=STAIY1_total)) + 
  geom_histogram(color="black", fill="white", binwidth = 4) +
  geom_vline(aes(xintercept=mean(STAIY1_total)),
             color="black", linetype="dashed", size=1) +
  labs(
    title = "STAI-Y1 (State Anxiety)",
    y = "No. of participants",
    x = "Score"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold")
  )


SFig4b<-ggplot(questData, aes(x=STAIY2_total)) + 
  geom_histogram(color="black", fill="white", binwidth = 4) +
  geom_vline(aes(xintercept=mean(STAIY2_total)),
             color="black", linetype="dashed", size=1) +
  labs(
    title = "STAI-Y2 (Trait Anxiety)",
    y = "No. of participants",
    x = "Score"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold")
  )


SFig4c<-ggplot(questData, aes(x=OCIR_total)) + 
  geom_histogram(color="black", fill="white", binwidth = 4) +
  geom_vline(aes(xintercept=mean(OCIR_total)),
             color="black", linetype="dashed", size=1) +
  labs(
    title = "OCI-R",
    y = "No. of participants",
    x = "Score"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold")
  )


#### Suppl Fig 5. Correlations BETWEEN questionnaire scores 

cor.test(questData$STAIY1_total, questData$STAIY2_total)
cor.test(questData$OCIR_total, questData$STAIY1_total)
cor.test(questData$OCIR_total, questData$STAIY2_total)


SFig5a <- ggplot(questData, aes(y = STAIY1_total, x = STAIY2_total)) +
  geom_point(shape = 21, colour = "black", size = 3, stroke = 1.2) +
  geom_smooth(
    method = lm, linetype = "dashed",
    color = "black", fill = "grey", size = 1.5
  ) +
  labs(
    title = "",
    y = "STAI-Y1 (State Anxiety)",
    x = "STAI-Y2 (Trait Anxiety)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold")
  ) + xlim (20,80)+ ylim (20,80)



SFig5b  <- ggplot(questData, aes(x = STAIY1_total, y = OCIR_total)) +
  geom_point(shape = 21, colour = "black", size = 3, stroke = 1.2) +
  geom_smooth(
    method = lm, linetype = "dashed",
    color = "black", fill = "grey", size = 1.5
  ) +
  labs(
    title = " ",
    y = "OCI-R",
    x = "STAI-Y1 (State Anxiety)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold")
  )+ xlim (20,80)+ ylim (0,72)

SFig5c  <- ggplot(questData, aes(x = STAIY2_total, y = OCIR_total)) +
  geom_point(shape = 21, colour = "black", size = 3, stroke = 1.2) +
  geom_smooth(
    method = lm, linetype = "dashed",
    color = "black", fill = "grey", size = 1.5
  ) +
  labs(
    title = " ",
    y = "OCI-R",
    x = "STAI-Y2 (Trait Anxiety)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold")
  )+ xlim (20,80)+ ylim (0,72)


# regressions
valReg<- lmer(meanAver ~ volumePer* (OCIR_total.sc) + (1 + volumePer | userID), regData)
arouReg<- lmer(meanArou ~ volumePer* (OCIR_total.sc) + (1 + volumePer | userID), regData)

summary(valReg)
summary(arouReg)

valReg<- lmer(meanAver ~ volumePer* (STAIY1_total.sc) + (1 + volumePer | userID), regData)
arouReg<- lmer(meanArou ~ volumePer* (STAIY1_total.sc) + (1 + volumePer | userID), regData)

summary(valReg)
summary(arouReg)

valReg<- lmer(meanAver ~ volumePer* (STAIY2_total.sc) + (1 + volumePer | userID), regData)
arouReg<- lmer(meanArou ~ volumePer* (STAIY2_total.sc) + (1 + volumePer | userID), regData)

summary(valReg)
summary(arouReg)


##########################################################################
##########################################################################
#### Fig 5a. Psychiatric questionnaires x valence/arousal ratings

audioRateMean <- ddply(audioRateUser, c("userID", "volumePer"), summarise,
                       
                       meanValAll = mean(meanVal), sdValAll = sd(meanVal),
                       meanArouAll = mean(meanArou), sdArouAll = sd(meanArou)
)

regData2 <- Reduce(
  function(dtf1, dtf2) merge(dtf1, dtf2, by = "userID", all.x = TRUE),
  list(audioRateMean, questData)
)

regData3=regData2[regData2$volumePer==0.5,]
cor.test(regData3$OCIR_total, regData3$meanArouAll)


Fig5a1<-ggplot(regData2, aes(x = meanArouAll, y = STAIY1_total, colour = as.factor(volumePer*100))) +
  geom_point(shape = 21, size = 3, stroke = 1.2, alpha =0.8) +
  scale_color_manual(values=c("#ffa600", "#003f5c"))+
  geom_smooth(
    method = lm, linetype = "dashed",
    fill = "grey", size = 1.5,se=F
  ) +
  labs(
    title = " ",
    y = "STAI-Y1 (State Anxiety)",
    x = "Mean Arousal Rating",
    colour = "Volume (%)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12),
    legend.position = "none"
  )

Fig5a2<-ggplot(regData2, aes(x = meanArouAll, y = STAIY2_total, colour = as.factor(volumePer*100))) +
  geom_point(shape = 21, size = 3, stroke = 1.2, alpha =0.8) +
  scale_color_manual(values=c("#ffa600", "#003f5c"))+
  geom_smooth(
    method = lm, linetype = "dashed",
    fill = "grey", size = 1.5,se=F
  ) +
  labs(
    title = " ",
    y = "STAI-Y2 (Trait Anxiety)",
    x = "Mean Arousal Rating",
    colour = "Volume (%)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12),
    legend.position = "none"
  )


Fig5a3<-ggplot(regData2, aes(x = meanArouAll, y = OCIR_total, colour = as.factor(volumePer*100))) +
  geom_point(shape = 21, size = 3, stroke = 1.2, alpha =0.8) +
  scale_color_manual(values=c("#ffa600", "#003f5c"))+
  geom_smooth(
    method = lm, linetype = "dashed",
    fill = "grey", size = 1.5,se=F
  ) +
  labs(
    title = " ",
    y = "OCI-R",
    x = "Mean Arousal Rating",
    colour = "Volume (%)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12),
    legend.position = "none"
  )
#+ xlim (1,100)+ ylim (0,72)


Fig5a4<-ggplot(regData2, aes(x = meanValAll, y = STAIY1_total, colour = as.factor(volumePer*100))) +
  geom_point(shape = 21, size = 3, stroke = 1.2, alpha =0.8) +
  scale_color_manual(values=c("#ffa600", "#003f5c"))+
  geom_smooth(
    method = lm, linetype = "dashed",
    fill = "grey", size = 1.5,se=F
  ) +
  labs(
    title = " ",
    y = "STAI-Y1 (State Anxiety)",
    x = "Mean Valence Rating",
    colour = "Volume (%)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold" ),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12),
    legend.position = "none"
  )



Fig5a5<-ggplot(regData2, aes(x = meanValAll, y = STAIY2_total, colour = as.factor(volumePer*100))) +
  geom_point(shape = 21, size = 3, stroke = 1.2, alpha =0.8) +
  scale_color_manual(values=c("#ffa600", "#003f5c"))+
  geom_smooth(
    method = lm, linetype = "dashed",
    fill = "grey", size = 1.5,se=F
  ) +
  labs(
    title = " ",
    y = "STAI-Y2 (Trait Anxiety)",
    x = "Mean Valence Rating",
    colour = "Volume (%)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12),
    legend.position = "none"
  )


Fig5a6<-ggplot(regData2, aes(x = meanValAll, y = OCIR_total, colour = as.factor(volumePer*100))) +
  geom_point(shape = 21, size = 3, stroke = 1.2, alpha =0.8) +
  scale_color_manual(values=c("#ffa600", "#003f5c"))+
  geom_smooth(
    method = lm, linetype = "dashed",
    fill = "grey", size = 1.5,se=F
  ) +
  labs(
    title = " ",
    y = "OCI-R",
    x = "Mean Valence Rating",
    colour = "Volume (%)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12),
    legend.position = "none"
  )
#+ xlim (1,100)+ ylim (0,72)

##########################################################################
##########################################################################
#### Fig 5b. Psychiatric questionnaires x valence/arousal reliability 

# reliability and questionnaire scores 

relAll <- rbind(relValTemp, relArouTemp)

regDataRel <- Reduce(
  function(dtf1, dtf2) merge(dtf1, dtf2, by = "userID", all.x = TRUE),
  list(relAll, questData)
)


# regressions
regDataRel$volumePer <- factor(regDataRel$Volume) #female is 0, male is 1, other is 3?
regDataRel$regDataRel$volumePer <- factor(regDataRel$Volume) <- factor(regDataRel$userID)

regDataRelVal<- regDataRel[regDataRel$Rating=="Valence",]
regDataRelArou<- regDataRel[regDataRel$Rating=="Arousal",]


valReg<- lm(Cor ~ volumePer* (STAIY1_total.sc), regDataRelVal)
arouReg<- lm(Cor ~ volumePer* (STAIY1_total.sc), regDataRelArou)

summary(valReg)
summary(arouReg)

valReg<- lm(Cor ~ volumePer* (STAIY2_total.sc), regDataRelVal)
arouReg<- lm(Cor ~ volumePer* (STAIY2_total.sc), regDataRelArou)

summary(valReg)
summary(arouReg)

valReg<- lm(Cor ~ volumePer* (OCIR_total.sc), regDataRelVal)
arouReg<- lm(Cor ~ volumePer* (OCIR_total.sc), regDataRelArou)

summary(valReg)
summary(arouReg)

# correlations

xlabel<- paste('Correlation (',rho,') between\nrepeated valence ratings' , sep="")

regDataRelVal<- regDataRel[regDataRel$Rating=="Valence",]
regDataRelVal.0.5<- regDataRel[regDataRel$Volume==0.5,]
regDataRelVal.1.0<- regDataRel[regDataRel$Volume==1.0,]

cor.test(regDataRelVal.0.5$Cor,regDataRelVal.0.5$STAIY1_total.sc, method = "spearman",exact=FALSE)
cor.test(regDataRelVal.1.0$Cor,regDataRelVal.1.0$STAIY1_total.sc, method = "spearman",exact=FALSE)
cor.test(regDataRelVal.0.5$Cor,regDataRelVal.0.5$STAIY2_total.sc, method = "spearman",exact=FALSE)
cor.test(regDataRelVal.1.0$Cor,regDataRelVal.1.0$STAIY2_total.sc, method = "spearman",exact=FALSE)
cor.test(regDataRelVal.0.5$Cor,regDataRelVal.0.5$OCIR_total.sc, method = "spearman",exact=FALSE)
cor.test(regDataRelVal.1.0$Cor,regDataRelVal.1.0$OCIR_total.sc, method = "spearman",exact=FALSE)


Fig5b1<-ggplot(regDataRelVal, aes(x = Cor, y = STAIY1_total, colour = as.factor(Volume*100))) +
  geom_point(shape = 21, size = 3, stroke = 1.2, alpha =0.8) +
  scale_color_manual(values=c("#ffa600", "#003f5c"))+
  geom_smooth(
    method = lm, linetype = "dashed",
    fill = "grey", size = 1.5,se=F
  ) +
  labs(
    title = " ",
    y = "STAI-Y1 (State Anxiety)",
    x = xlabel,
    colour = "Volume (%)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12),
    legend.position = "none"
  )+ ylim (20,80)


Fig5b2<-ggplot(regDataRelVal, aes(x = Cor, y = STAIY2_total, colour = as.factor(Volume*100))) +
  geom_point(shape = 21, size = 3, stroke = 1.2, alpha =0.8) +
  scale_color_manual(values=c("#ffa600", "#003f5c"))+
  geom_smooth(
    method = lm, linetype = "dashed",
    fill = "grey", size = 1.5,se=F
  ) +
  labs(
    title = " ",
    y = "STAI-Y2 (Trait Anxiety)",
    x = xlabel,
    colour = "Volume (%)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12),
    legend.position = "none"
  )+ ylim (20,80)


Fig5b3<-ggplot(regDataRelVal, aes(x = Cor, y = OCIR_total, colour = as.factor(Volume*100))) +
  geom_point(shape = 21, size = 3, stroke = 1.2, alpha =0.8) +
  scale_color_manual(values=c("#ffa600", "#003f5c"))+
  geom_smooth(
    method = lm, linetype = "dashed",
    fill = "grey", size = 1.5,se=F
  ) +
  labs(
    title = " ",
    y = "OCI-R",
    x = xlabel,
    colour = "Volume (%)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12),
    legend.position = "none"
  )+ ylim (0,72)


regDataRelArou<- regDataRel[regDataRel$Rating=="Arousal",]

regDataRelArou.0.5<- regDataRelArou[regDataRelArou$Volume==0.5,]
regDataRelArou.1.0<- regDataRelArou[regDataRelArou$Volume==1.0,]

cor.test(regDataRelArou.0.5$Cor,regDataRelArou.0.5$STAIY1_total.sc, method = "spearman",exact=FALSE)
cor.test(regDataRelArou.1.0$Cor,regDataRelArou.1.0$STAIY1_total.sc, method = "spearman",exact=FALSE)
cor.test(regDataRelArou.0.5$Cor,regDataRelArou.0.5$STAIY2_total.sc, method = "spearman",exact=FALSE)
cor.test(regDataRelArou.1.0$Cor,regDataRelArou.1.0$STAIY2_total.sc, method = "spearman",exact=FALSE)
cor.test(regDataRelArou.0.5$Cor,regDataRelArou.0.5$OCIR_total.sc, method = "spearman",exact=FALSE)
cor.test(regDataRelArou.1.0$Cor,regDataRelArou.1.0$OCIR_total.sc, method = "spearman",exact=FALSE)



xlabel<- paste('Correlation (',rho,') between\nrepeated arousal ratings' , sep="")

Fig5b4<-ggplot(regDataRelArou, aes(x = Cor, y = STAIY1_total, colour = as.factor(Volume*100))) +
  geom_point(shape = 21, size = 3, stroke = 1.2, alpha =0.8) +
  scale_color_manual(values=c("#ffa600", "#003f5c"))+
  geom_smooth(
    method = lm, linetype = "dashed",
    fill = "grey", size = 1.5,se=F
  ) +
  labs(
    title = " ",
    y = "STAI-Y1 (State Anxiety)",
    x = xlabel,
    colour = "Volume (%)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12),
    legend.position = "none"
  )+ ylim (20,80)


Fig5b5<-ggplot(regDataRelArou, aes(x = Cor, y = STAIY2_total, colour = as.factor(Volume*100))) +
  geom_point(shape = 21, size = 3, stroke = 1.2, alpha =0.8) +
  scale_color_manual(values=c("#ffa600", "#003f5c"))+
  geom_smooth(
    method = lm, linetype = "dashed",
    fill = "grey", size = 1.5,se=F
  ) +
  labs(
    title = " ",
    y = "STAI-Y2 (Trait Anxiety)",
    x = xlabel,
    colour = "Volume (%)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12),
    legend.position = "none"
  )+ ylim (20,80)


Fig5b6<-ggplot(regDataRelArou, aes(x = Cor, y = OCIR_total, colour = as.factor(Volume*100))) +
  geom_point(shape = 21, size = 3, stroke = 1.2, alpha =0.8) +
  scale_color_manual(values=c("#ffa600", "#003f5c"))+
  geom_smooth(
    method = lm, linetype = "dashed",
    fill = "grey", size = 1.5,se=F
  ) +
  labs(
    title = " ",
    y = "OCI-R",
    x = xlabel,
    colour = "Volume (%)"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12),
    legend.position = "none"
  )+ ylim (0,72)



##########################################################################
##########################################################################
#### Supplementary Fig 6. Individual sound correlation with questionnaire scores

# do a for loop here
val.0.5.frame <- data.frame()
arou.0.5.frame <- data.frame()
val.1.0.frame <- data.frame()
arou.1.0.frame <- data.frame()

for (p in 1:length(unique(regData$soundFocus))){
  
  sound = unique(regData$soundFocus)[p]
  ratePerSound.1=regData[regData$soundFocus==sound & regData$volumePer ==0.5 ,]
  ratePerSound.2=regData[regData$soundFocus==sound & regData$volumePer ==1.0 ,]
  
  sound.1.stai.y1.val = cor.test(ratePerSound.1$meanVal,ratePerSound.1$STAIY1_total.sc, method = "spearman",exact=FALSE)
  sound.1.stai.y1.arou = cor.test(ratePerSound.1$meanArou,ratePerSound.1$STAIY1_total.sc, method = "spearman",exact=FALSE)
  sound.1.stai.y2.val = cor.test(ratePerSound.1$meanVal,ratePerSound.1$STAIY2_total.sc, method = "spearman",exact=FALSE)
  sound.1.stai.y2.arou = cor.test(ratePerSound.1$meanArou,ratePerSound.1$STAIY2_total.sc, method = "spearman",exact=FALSE)
  sound.1.ocir.val = cor.test(ratePerSound.1$meanVal,ratePerSound.1$OCIR_total.sc, method = "spearman",exact=FALSE)
  sound.1.ocir.arou = cor.test(ratePerSound.1$meanArou,ratePerSound.1$OCIR_total.sc, method = "spearman",exact=FALSE)
  
  sound.2.stai.y1.val = cor.test(ratePerSound.2$meanVal,ratePerSound.2$STAIY1_total.sc, method = "spearman",exact=FALSE)
  sound.2.stai.y1.arou = cor.test(ratePerSound.2$meanArou,ratePerSound.2$STAIY1_total.sc, method = "spearman",exact=FALSE)
  sound.2.stai.y2.val = cor.test(ratePerSound.2$meanVal,ratePerSound.2$STAIY2_total.sc, method = "spearman",exact=FALSE)
  sound.2.stai.y2.arou = cor.test(ratePerSound.2$meanArou,ratePerSound.2$STAIY2_total.sc, method = "spearman",exact=FALSE)
  sound.2.ocir.val = cor.test(ratePerSound.2$meanVal,ratePerSound.2$OCIR_total.sc, method = "spearman",exact=FALSE)
  sound.2.ocir.arou = cor.test(ratePerSound.2$meanArou,ratePerSound.2$OCIR_total.sc, method = "spearman",exact=FALSE)
  
  
  val.cor.0.5<-data.frame("sound" =sound , "STAIY1_cor" = sound.1.stai.y1.val$estimate,"STAIY1_t" = sound.1.stai.y1.val$statistic,"STAIY1_p" = sound.1.stai.y1.val$p.value,
                          "STAIY2_cor" = sound.1.stai.y2.val$estimate,"STAIY2_t" = sound.1.stai.y2.val$statistic,"STAIY2_p" = sound.1.stai.y2.val$p.value,
                          "OCIR_cor" = sound.1.ocir.val$estimate,"OCIR_t" = sound.1.ocir.val$statistic,"OCIR_p" = sound.1.ocir.val$p.value)
  
  
  arou.cor.0.5<-data.frame("sound" =sound , "STAIY1_cor" = sound.1.stai.y1.arou$estimate,"STAIY1_t" = sound.1.stai.y1.arou$statistic,"STAIY1_p" = sound.1.stai.y1.arou$p.value,
                           "STAIY2_cor" = sound.1.stai.y2.arou$estimate,"STAIY2_t" = sound.1.stai.y2.arou$statistic,"STAIY2_p" = sound.1.stai.y2.arou$p.value,
                           "OCIR_cor" = sound.1.ocir.arou$estimate,"OCIR_t" = sound.1.ocir.arou$statistic,"OCIR_p" = sound.1.ocir.arou$p.value)
  
  
  val.cor.1.0<-data.frame("sound" =sound , "STAIY1_cor" = sound.2.stai.y1.val$estimate,"STAIY1_t" = sound.2.stai.y1.val$statistic,"STAIY1_p" = sound.2.stai.y1.val$p.value,
                          "STAIY2_cor" = sound.2.stai.y2.val$estimate,"STAIY2_t" = sound.2.stai.y2.val$statistic,"STAIY2_p" = sound.2.stai.y2.val$p.value,
                          "OCIR_cor" = sound.2.ocir.val$estimate,"OCIR_t" = sound.2.ocir.val$statistic,"OCIR_p" = sound.2.ocir.val$p.value)
  
  
  arou.cor.1.0<-data.frame("sound" =sound , "STAIY1_cor" = sound.2.stai.y1.arou$estimate,"STAIY1_t" = sound.2.stai.y1.arou$statistic,"STAIY1_p" = sound.2.stai.y1.arou$p.value,
                           "STAIY2_cor" = sound.2.stai.y2.arou$estimate,"STAIY2_t" = sound.2.stai.y2.arou$statistic,"STAIY2_p" = sound.2.stai.y2.arou$p.value,
                           "OCIR_cor" = sound.2.ocir.arou$estimate,"OCIR_t" = sound.2.ocir.arou$statistic,"OCIR_p" = sound.2.ocir.arou$p.value)
  
  val.0.5.frame <- rbind(  val.0.5.frame,val.cor.0.5 )
  arou.0.5.frame <- rbind(  arou.0.5.frame,arou.cor.0.5 )
  val.1.0.frame <- rbind(  val.1.0.frame,val.cor.1.0 )
  arou.1.0.frame <- rbind(  arou.1.0.frame,arou.cor.1.0 )
}

xlabel<- paste('Correlation (',rho,')' , sep="")

# valence 50%
val.0.5.frame.temp<- val.0.5.frame[,c("sound","STAIY1_cor","STAIY2_cor","OCIR_cor")]
colnames(val.0.5.frame.temp)<-c("sound","STAIY1","STAIY2","OCIR")
val.0.5.frame.temp<-melt(val.0.5.frame.temp)

SFig6a <- ggplot(val.0.5.frame.temp, aes(colour = as.factor(variable), y = sound, x = value)) +
  geom_point( size = 3,position = position_dodge(width=0.55)) +
  # change color of bars
  scale_color_manual(values=c("#EE7733", "#BBBBBB","#009988"))+
  labs(
    title = "Valence Scale (50% volume)",
    y = " ",
    x = xlabel, colour = "Questionnaire"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12)
  )+ 
  #xlim (-0.4,0.3)
  xlim (-0.4,0.41)

# Arousal 50%
arou.0.5.frame.temp<- arou.0.5.frame[,c("sound","STAIY1_cor","STAIY2_cor","OCIR_cor")]
colnames(arou.0.5.frame.temp)<-c("sound","STAIY1","STAIY2","OCIR")
arou.0.5.frame.temp<-melt(arou.0.5.frame.temp)

SFig6b <- ggplot(arou.0.5.frame.temp, aes(colour = as.factor(variable), y = sound, x = value)) +
  geom_point( size = 3,position = position_dodge(width=0.55)) +
  # change color of bars
  scale_color_manual(values=c("#EE7733", "#BBBBBB","#009988"))+
  labs(
    title = "Arousal Scale (50% volume)",
    y = " ",
    x = xlabel, colour = "Questionnaire"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12)
  )+ 
  # xlim (-0.2,0.41)
  xlim (-0.4,0.41)

# valence 100%
val.1.0.frame.temp<- val.1.0.frame[,c("sound","STAIY1_cor","STAIY2_cor","OCIR_cor")]
colnames(val.1.0.frame.temp)<-c("sound","STAIY1","STAIY2","OCIR")
val.1.0.frame.temp<-melt(val.1.0.frame.temp)

SFig6c <- ggplot(val.1.0.frame.temp, aes(colour = as.factor(variable), y = sound, x = value)) +
  geom_point( size = 3,position = position_dodge(width=0.55)) +
  # change color of bars
  scale_color_manual(values=c("#EE7733", "#BBBBBB","#009988"))+
  labs(
    title = "Valence Scale (100% volume)",
    y = " ",
    x = xlabel, colour = "Questionnaire"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12)
  )+ 
  #  xlim (-0.25,0.2)
  xlim (-0.3,0.3)


# Arousal 100%
arou.1.0.frame.temp<- arou.1.0.frame[,c("sound","STAIY1_cor","STAIY2_cor","OCIR_cor")]
colnames(arou.1.0.frame.temp)<-c("sound","STAIY1","STAIY2","OCIR")
arou.1.0.frame.temp<-melt(arou.1.0.frame.temp)

SFig6d <- ggplot(arou.1.0.frame.temp, aes(colour = as.factor(variable), y = sound, x = value)) +
  geom_point( size = 3,position = position_dodge(width=0.55)) +
  # change color of bars
  scale_color_manual(values=c("#EE7733", "#BBBBBB","#009988"))+
  labs(
    title = "Arousal Scale (100% volume)",
    y = " ",
    x = xlabel, colour = "Questionnaire"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=12)
  )+ 
  xlim (-0.3,0.3)




val.0.5.frame[val.0.5.frame$STAIY1_p <0.05 | val.0.5.frame$STAIY2_p <0.05 |val.0.5.frame$OCIR_p <0.05,]
arou.0.5.frame[arou.0.5.frame$STAIY1_p <0.05 | arou.0.5.frame$STAIY2_p <0.05 |arou.0.5.frame$OCIR_p <0.05,]
val.1.0.frame[val.1.0.frame$STAIY1_p <0.05 | val.1.0.frame$STAIY2_p <0.05 |val.1.0.frame$OCIR_p <0.05,]
arou.1.0.frame[arou.1.0.frame$STAIY1_p <0.05 | arou.1.0.frame$STAIY2_p <0.05 |arou.1.0.frame$OCIR_p <0.05,]


##########################################################################
##########################################################################
#### Save figures


if (print == 1) {
  setwd(printFolder)
  
  # Figure 1 - volume and frequency
  ggsave("Fig1a.png", plot = Fig1a, width = 5 ,height = 10, units = "cm", dpi =300)
  ggsave("Fig1b.png", plot = Fig1b,width = 13 ,height = 11, units = "cm", dpi =300)
  
  # Figure 2a - intra-participant test-rest reliability 
  ggsave("Fig2a.png", plot = Fig2a,width = 10 ,height = 10, units = "cm", dpi =300)
  
  # Figure 2b - inter-participant  test-rest reliability 
  ggsave("Fig2b1.png", plot = Fig2b1,width = 10 ,height = 10, units = "cm", dpi =300)
  ggsave("Fig2b2.png", plot = Fig2b2,width = 10 ,height = 10, units = "cm", dpi =300)
  ggsave("Fig2b3.png", plot = Fig2b3,width = 10 ,height = 10, units = "cm", dpi =300)
  ggsave("Fig2b4.png", plot = Fig2b4,width = 10 ,height = 10, units = "cm", dpi =300)
  
  # Figure 3 - correlation with prior ratings
  ggsave("Fig3a.png", plot = Fig3a,width = 15 ,height = 15, units = "cm", dpi =300)
  ggsave("Fig3b.png", plot = Fig3b,width = 15 ,height = 15, units = "cm", dpi =300)
  
  # Figure 4 - ranking of ratings
  ggsave("Fig4a.png", plot = Fig4a,width = 25 ,height = 20, units = "cm", dpi =300)
  ggsave("Fig4b.png", plot = Fig4b,width = 25 ,height = 20, units = "cm", dpi =300)
  
  # Figure 5a - questionnaires x arousal/valence ratings
  ggsave("Fig5a1.png", plot = Fig5a1,width = 10 ,height = 10, units = "cm", dpi =300)
  ggsave("Fig5a2.png", plot = Fig5a2,width = 10 ,height = 10, units = "cm", dpi =300)
  ggsave("Fig5a3.png", plot = Fig5a3,width = 10 ,height = 10, units = "cm", dpi =300)
  ggsave("Fig5a4.png", plot = Fig5a4,width = 10 ,height = 10, units = "cm", dpi =300)
  ggsave("Fig5a5.png", plot = Fig5a5,width = 10 ,height = 10, units = "cm", dpi =300)
  ggsave("Fig5a6.png", plot = Fig5a6,width = 10 ,height = 10, units = "cm", dpi =300)
  
  # Figure 5b - questionnaires x arousal/valence ratings
  ggsave("Fig5b1.png", plot = Fig5b1,width = 10 ,height = 10, units = "cm", dpi =300)
  ggsave("Fig5b2.png", plot = Fig5b2,width = 10 ,height = 10, units = "cm", dpi =300)
  ggsave("Fig5b3.png", plot = Fig5b3,width = 10 ,height = 10, units = "cm", dpi =300)
  ggsave("Fig5b4.png", plot = Fig5b4,width = 10 ,height = 10, units = "cm", dpi =300)
  ggsave("Fig5b5.png", plot = Fig5b5,width = 10 ,height = 10, units = "cm", dpi =300)
  ggsave("Fig5b6.png", plot = Fig5b6,width = 10 ,height = 10, units = "cm", dpi =300)
  
  
  # Supplemetary Figure 1 - frequency x age
  ggsave("SFig1.png", plot = SFig1,width = 10 ,height = 10, units = "cm", dpi =300)
  
  # Supplemetary Figure 2 - intra-sound test-rest reliability 
  ggsave("SFig2.png", plot = SFig2,width = 20 ,height = 20, units = "cm", dpi =300)
  
  # Supplemetary Figure 3 - Correlation of valence mean vs arousal mean, by volume
  ggsave("SFig3a.png", plot = SFig3a,width = 11 ,height = 9, units = "cm", dpi =300)
  ggsave("SFig3b.png", plot = SFig3b,width = 11 ,height = 9, units = "cm", dpi =300)
  ggsave("SFig3c.png", plot = SFig3c,width = 11 ,height = 9, units = "cm", dpi =300)
  
  # Supplemetary Figure 4 - questionnaires histograms 
  ggsave("SFig4a.png", plot = SFig4a,width = 10 ,height = 10, units = "cm", dpi =300)
  ggsave("SFig4b.png", plot = SFig4b, width = 10 ,height = 10, units = "cm", dpi =300)
  ggsave("SFig4c.png", plot = SFig4c,width = 10 ,height = 10, units = "cm", dpi =300)
  
  # Supplemetary Figure 5 - questionnaires correlations 
  ggsave("SFig5a.png", plot = SFig5a,width = 10 ,height = 10, units = "cm", dpi =300)
  ggsave("SFig5b.png", plot = SFig5b,width = 10 ,height = 10, units = "cm", dpi =300)
  ggsave("SFig5c.png", plot = SFig5c,width = 10 ,height = 10, units = "cm", dpi =300)
  
  # Supplemetary Figure 6 - questionnaires correlations per specific sound
  ggsave("SFig6a.png", plot = SFig6a,width = 25 ,height = 20, units = "cm", dpi =300)
  ggsave("SFig6b.png", plot = SFig6b,width = 25 ,height = 20, units = "cm", dpi =300)
  ggsave("SFig6c.png", plot = SFig6c,width = 25 ,height = 20, units = "cm", dpi =300)
  ggsave("SFig6d.png", plot = SFig6d,width = 25 ,height = 20, units = "cm", dpi =300)
  
  
}
