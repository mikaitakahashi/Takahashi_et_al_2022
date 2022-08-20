# This is the data set which was used in the study titled How do disturbance and competition affect the feeding ecology of scavanging raptors.

#'---
#'author: Mika Takahashi 
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#'---
#'
#'


#' some prep
vp_data <- read.csv("/Volumes/MIKA/Dissertation /DATA/Vigilance_Pecking.csv")

library(dplyr)
library(ggplot2)
library(ggpubr)
library(lme4)
library(lmtest)
library(multcomp)
library(caret)
library(car)
library(performance)
library(MASS)
library(msme)


# keep only those that have vigilance and peck counts
vp_data <- vp_data[!is.na(vp_data$Heads.up),]
vp_data <- vp_data[!is.na(vp_data$Pecks),]
vp_data <- vp_data[!is.na(vp_data$X.conspecific),]
vp_data <- vp_data[!is.na(vp_data$t_distance_m),]
vp_data <- vp_data[!(vp_data$Species == "3"),]
vp_data <- vp_data[!(vp_data$Species == "10"),]
# 
vp_data$LandUseArea <- as.factor(vp_data$LandUseArea)
vp_data$Pecks <- as.numeric(vp_data$Pecks)
vp_data$Species <- as.factor(vp_data$Species)
vp_data$Season <- as.factor(vp_data$Season)
vp_data$Size <- as.factor(vp_data$Size)
vp_data$Settle_avg <- as.numeric(vp_data$Settle_avg)

# new number of birds
vp_data$N.birds <- vp_data$X.Birds - vp_data$X.conspecific

# lappet-faced pair 
vp_data$Lappet_pair <- ifelse(vp_data$X.conspecific == 0, "solo", ifelse(vp_data$X.conspecific == 1, "pair","multiple"))
vp_data$Lappet_pair <- as.factor(vp_data$Lappet_pair)



#' 
#' ***
#' 
#' ### Lappet-faced Vulture (n = 287)
#' 
dplyr::count(subset(vp_data, Species == "8"))

# vigilance model for lappet-faced
model_Lv <- glm.nb(Heads.up ~ Settle_avg + t_distance_m + Lappet_pair + N.birds + Settle_avg:X.Birds, data = subset(vp_data, Species == "8"))
# check_overdispersion(model_Lv)
P__disp(model_Lv)
summary(model_Lv)

# fit the model
model_Lv1 <- glm.nb(Heads.up ~ t_distance_m + Lappet_pair + N.birds + Settle_avg:X.Birds, data = subset(vp_data, Species == "8"))
lrtest(model_Lv1, model_Lv)
model_Lv2 <- glm.nb(Heads.up ~ Lappet_pair + N.birds + Settle_avg:X.Birds, data = subset(vp_data, Species == "8"))
lrtest(model_Lv2, model_Lv1)
model_Lv3 <- glm.nb(Heads.up ~ N.birds + Settle_avg:X.Birds, data = subset(vp_data, Species == "8"))
lrtest(model_Lv3, model_Lv2)
model_Lv4 <- glm.nb(Heads.up ~ Settle_avg:X.Birds, data = subset(vp_data, Species == "8"))
lrtest(model_Lv4, model_Lv3)
model_Lv5 <- glm.nb(Heads.up ~ N.birds, data = subset(vp_data, Species == "8"))
lrtest(model_Lv5, model_Lv3)




# final model ...?
model_Lv5 <- glm.nb(Heads.up ~ N.birds, data = subset(vp_data, Species == "8"))
summary(model_Lv5)
r2_nagelkerke(model_Lv5)
# vif(model_Lv4)
summary(glht(model_Lv, linfct = mcp(Lappet_pair = "Tukey")))
plot(model_Lv5)
performance::check_model(model_Lv5)


#'



# peck model for lappet-faced
model_Lp <- glm.nb(Pecks ~ Settle_avg + t_distance_m + Lappet_pair + N.birds, data = subset(vp_data, Species == "8"))
# check_overdispersion(model_Lp)
P__disp(model_Lp)
summary(model_Lp)

# fit the model
model_Lp1 <- glm.nb(Pecks ~ t_distance_m + Lappet_pair + N.birds, data = subset(vp_data, Species == "8"))
lrtest(model_Lp1, model_Lp)
model_Lp2 <- glm.nb(Pecks ~ Settle_avg + Lappet_pair + N.birds, data = subset(vp_data, Species == "8"))
lrtest(model_Lp2, model_Lp)
model_Lp3 <- glm.nb(Pecks ~ Settle_avg + N.birds, data = subset(vp_data, Species == "8"))
lrtest(model_Lp3, model_Lp2)
model_Lp4 <- glm.nb(Pecks ~ Settle_avg, data = subset(vp_data, Species == "8"))
lrtest(model_Lp4, model_Lp3)


# final model
model_Lp3 <- glm.nb(Pecks ~ Settle_avg + N.birds, data = subset(vp_data, Species == "8"))
summary(model_Lp3)
r2_nagelkerke(model_Lp3)
vif(model_Lp3)
summary(glht(model_Lp, linfct = mcp(Lappet_pair = "Tukey")))
performance::check_model(model_Lp3)




