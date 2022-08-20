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



#' 
#' ***
#' 
#' ### Tawny (n = 116) 
dplyr::count(subset(vp_data, Species == "13"))


# vigilance model for tawny
# no Size in the model since there was only one size recorded
model_Tv <- glm.nb(Heads.up ~ Settle_avg + t_distance_m + X.conspecific + N.birds + Settle_avg:X.Birds, data = subset(vp_data, Species == "13"))
# check_overdispersion(model_Tv)
P__disp(model_Tv)
summary(model_Tv)


# fit the model 
model_Tv1 <- glm.nb(Heads.up ~ t_distance_m + X.conspecific + N.birds + Settle_avg:X.Birds, data = subset(vp_data, Species == "13"))
lrtest(model_Tv1, model_Tv)
model_Tv2 <- glm.nb(Heads.up ~ X.conspecific + N.birds + Settle_avg:X.Birds, data = subset(vp_data, Species == "13"))
lrtest(model_Tv2, model_Tv1)
model_Tv3 <- glm.nb(Heads.up ~ N.birds + Settle_avg:X.Birds, data = subset(vp_data, Species == "13"))
lrtest(model_Tv3, model_Tv2)
model_Tv4 <- glm.nb(Heads.up ~ Settle_avg:X.Birds, data = subset(vp_data, Species == "13"))
lrtest(model_Tv4, model_Tv3)
model_Tv5<- glm.nb(Heads.up ~ 1, data = subset(vp_data, Species == "13"))
lrtest(model_Tv5, model_Tv4)

# final model 
model_Tv5 <- glm.nb(Heads.up ~ 1, data = subset(vp_data, Species == "13"))
summary(model_Tv6)
r2_nagelkerke(model_Tv4)
# vif(model_Tv4)
# performance::check_model(model_Tv4)


# peck model for tawny
model_Tp <- glm.nb(Pecks ~Settle_avg + t_distance_m + X.conspecific + N.birds, data = subset(vp_data, Species == "13"))
# check_overdispersion(model_Tp)
P__disp(model_Tp)
summary(model_Tp)

# fit the model
model_Tp1 <- glm.nb(Pecks ~ t_distance_m + X.conspecific + N.birds, data = subset(vp_data, Species == "13"))
lrtest(model_Tp1, model_Tp)
model_Tp2 <- glm.nb(Pecks ~ X.conspecific + N.birds, data = subset(vp_data, Species == "13"))
lrtest(model_Tp2, model_Tp1)
model_Tp3 <- glm.nb(Pecks ~ N.birds, data = subset(vp_data, Species == "13"))
lrtest(model_Tp3, model_Tp2)
model_Tp4 <- glm.nb(Pecks ~ 1, data = subset(vp_data, Species == "13"))
lrtest(model_Tp4, model_Tp3)

# final model
model_Tp4 <- glm.nb(Pecks ~ 1, data = subset(vp_data, Species == "13"))
summary(model_Tp4)
#vif(model_Tp)



#'
#' ***
#' 
#' ### White-backed Vulture (n = 105) 
dplyr::count(subset(vp_data, Species == "1"))


# vigilance model 
model_WBv <- glm.nb(Heads.up ~ Settle_avg + t_distance_m + X.conspecific + N.birds + Settle_avg:X.Birds, data = subset(vp_data, Species == "1"))
# check_overdispersion(model_WBv)
P__disp(model_WBv)
summary(model_WBv)


# fit the model
model_WBv1 <- glm.nb(Heads.up ~ t_distance_m + X.conspecific + N.birds + Settle_avg:X.Birds, data = subset(vp_data, Species == "1"))
lrtest(model_WBv1, model_WBv)
model_WBv2 <- glm.nb(Heads.up ~ X.conspecific + N.birds + Settle_avg:X.Birds, data = subset(vp_data, Species == "1"))
lrtest(model_WBv2, model_WBv1)
model_WBv3 <- glm.nb(Heads.up ~ t_distance_m + X.conspecific + N.birds + Settle_avg:X.Birds, data = subset(vp_data, Species == "1"))
lrtest(model_WBv3, model_WBv2)
model_WBv4 <- glm.nb(Heads.up ~ t_distance_m + N.birds + Settle_avg:X.Birds, data = subset(vp_data, Species == "1"))
lrtest(model_WBv4, model_WBv3)
model_WBv5 <- glm.nb(Heads.up ~ t_distance_m + X.conspecific + Settle_avg:X.Birds, data = subset(vp_data, Species == "1"))
lrtest(model_WBv5, model_WBv3)
model_WBv6 <- glm.nb(Heads.up ~ t_distance_m + X.conspecific, data = subset(vp_data, Species == "1"))
lrtest(model_WBv6, model_WBv5)
model_WBv7 <- glm.nb(Heads.up ~ Settle_avg + t_distance_m + X.conspecific, data = subset(vp_data, Species == "1"))
lrtest(model_WBv6, model_WBv7)



# final model
model_WBv7 <- glm.nb(Heads.up ~ Settle_avg + t_distance_m + X.conspecific, data = subset(vp_data, Species == "1"))
summary(model_WBv7)
r2_nagelkerke(model_WBv7)
vif(model_WBv7)
performance::check_model(model_WBv7)

#' vigilance decreased as # vulture increased. Could be because they are social vulture species. 



# peck model for WB
model_WBp <- glm.nb(Pecks ~ Settle_avg + t_distance_m + X.conspecific + N.birds, data = subset(vp_data, Species == "1"))
# check_overdispersion(model_WBp)
P__disp(model_WBp)
summary(model_WBp)

# fit the model
model_WBp1 <- glm.nb(Pecks ~ t_distance_m + X.conspecific + N.birds, data = subset(vp_data, Species == "1"))
lrtest(model_WBp1, model_WBp)
model_WBp2 <- glm.nb(Pecks ~ Settle_avg + X.conspecific + N.birds, data = subset(vp_data, Species == "1"))
lrtest(model_WBp2, model_WBp)
model_WBp3 <- glm.nb(Pecks ~ Settle_avg + N.birds, data = subset(vp_data, Species == "1"))
lrtest(model_WBp3, model_WBp2)
model_WBp4 <- glm.nb(Pecks ~ Settle_avg + X.conspecific, data = subset(vp_data, Species == "1"))
lrtest(model_WBp4, model_WBp2)


# final model
model_WBp4 <- glm.nb(Pecks ~ Settle_avg + X.conspecific, data = subset(vp_data, Species == "1"))
summary(model_WBp4)
vif(model_WBp4)
r2_nagelkerke(model_WBp4)
performance::check_model(model_WBp4)




#' 
#' ***
#' 
#' ### Hooded vulture
#' 

#Hooded vulture (n = 58) 
dplyr::count(subset(vp_data, Species == "6"))


# vigilance model for hooded
model_Hv <- glm.nb(Heads.up ~ Season + Settle_avg + t_distance_m + X.conspecific + N.birds + Settle_avg:X.Birds, data = subset(vp_data, Species == "6"))
# check_overdispersion(model_Hv)
P__disp(model_Hv)
summary(model_Hv)

# fit the model
model_Hv1 <- glm.nb(Heads.up ~ t_distance_m + X.conspecific + N.birds + Settle_avg:X.Birds, data = subset(vp_data, Species == "6"))
lrtest(model_Hv1, model_Hv)
model_Hv2 <- glm.nb(Heads.up ~ X.conspecific + N.birds + Settle_avg:X.Birds, data = subset(vp_data, Species == "6"))
lrtest(model_Hv2, model_Hv1)
model_Hv3 <- glm.nb(Heads.up ~ N.birds + Settle_avg:X.Birds, data = subset(vp_data, Species == "6"))
lrtest(model_Hv3, model_Hv2)
model_Hv4 <- glm.nb(Heads.up ~ X.conspecific + Settle_avg:X.Birds, data = subset(vp_data, Species == "6"))
lrtest(model_Hv4, model_Hv2)
model_Hv5 <- glm.nb(Heads.up ~ X.conspecific + N.birds, data = subset(vp_data, Species == "6"))
lrtest(model_Hv5, model_Hv2)
model_Hv6 <- glm.nb(Heads.up ~ Settle_avg + X.conspecific + N.birds, data = subset(vp_data, Species == "6"))
lrtest(model_Hv6, model_Hv5)





# final model
model_Hv5 <- glm.nb(Heads.up ~ X.conspecific + N.birds, data = subset(vp_data, Species == "6"))
summary(model_Hv5)
r2_nagelkerke(model_Hv5)
vif(model_Hv5)
performance::check_model(model_Hv5)





# peck model for hooded
model_Hp <- glm.nb(Pecks ~ Settle_avg + t_distance_m + X.conspecific + N.birds, data = subset(vp_data, Species == "6"))
# check_overdispersion(model_Hp)
P__disp(model_Hp)
summary(model_Hp)

# fit the model 
model_Hp1 <- glm.nb(Pecks ~ t_distance_m + X.conspecific + N.birds, data = subset(vp_data, Species == "6"))
lrtest(model_Hp1, model_Hp)
model_Hp2 <- glm.nb(Pecks ~ X.conspecific + N.birds, data = subset(vp_data, Species == "6"))
lrtest(model_Hp2, model_Hp1)
model_Hp3 <- glm.nb(Pecks ~ N.birds, data = subset(vp_data, Species == "6"))
lrtest(model_Hp3, model_Hp2)
model_Hp4 <- glm.nb(Pecks ~ X.conspecific, data = subset(vp_data, Species == "6"))
lrtest(model_Hp4, model_Hp2)



# final model
model_Hp4 <- glm.nb(Pecks ~ X.conspecific, data = subset(vp_data, Species == "6"))
summary(model_Hp4)
r2_nagelkerke(model_Hp4)
vif(model_Hp4)
performance::check_model(model_Hp4)



#' Ruppple's (n = 49) 
dplyr::count(subset(vp_data, Species == "11"))


# vigilance model for RV
# no size
model_Rv <- glm.nb(Heads.up ~ Settle_avg + t_distance_m + X.conspecific + N.birds + Settle_avg:X.Birds, data = subset(vp_data, Species == "11"))
# check_overdispersion(model_Rv)
P__disp(model_Rv)
summary(model_Rv)


# fit the model
model_Rv1 <- glm.nb(Heads.up ~ t_distance_m + X.conspecific + N.birds + Settle_avg:X.Birds, data = subset(vp_data, Species == "11"))
lrtest(model_Rv1, model_Rv)
model_Rv2 <- glm.nb(Heads.up ~ X.conspecific + N.birds + Settle_avg:X.Birds, data = subset(vp_data, Species == "11"))
lrtest(model_Rv2, model_Rv1)
model_Rv3 <- glm.nb(Heads.up ~ N.birds + Settle_avg:X.Birds, data = subset(vp_data, Species == "11"))
lrtest(model_Rv3, model_Rv2)
model_Rv4 <- glm.nb(Heads.up ~ X.conspecific + Settle_avg:X.Birds, data = subset(vp_data, Species == "11"))
lrtest(model_Rv4, model_Rv2)
model_Rv5 <- glm.nb(Heads.up ~ X.conspecific, data = subset(vp_data, Species == "11"))
lrtest(model_Rv5, model_Rv4)


# final model
model_Rv4 <- glm.nb(Heads.up ~ X.conspecific + Settle_avg:X.Birds, data = subset(vp_data, Species == "11"))
summary(model_Rv4)
r2_nagelkerke(model_Rv4)
vif(model_Rv4)

#' conspecific decreased vigilance rate 


# peck model for RV
model_Rp <- glm.nb(Pecks ~ Settle_avg + t_distance_m + X.conspecific + N.birds, data = subset(vp_data, Species == "11"))
# check_overdispersion(model_Rp)
P__disp(model_Rp)
summary(model_Rp)

# fit the model
model_Rp1<- glm.nb(Pecks ~ t_distance_m + X.conspecific + N.birds, data = subset(vp_data, Species == "11"))
lrtest(model_Rp1, model_Rp)
model_Rp2<- glm.nb(Pecks ~ X.conspecific + N.birds, data = subset(vp_data, Species == "11"))
lrtest(model_Rp2, model_Rp1)
model_Rp3 <- glm.nb(Pecks ~ N.birds, data = subset(vp_data, Species == "11"))
lrtest(model_Rp3, model_Rp2)
model_Rp4 <- glm.nb(Pecks ~ 1, data = subset(vp_data, Species == "11"))
lrtest(model_Rp4, model_Rp3)


# final model
model_Rp4 <- glm.nb(Pecks ~ 1, data = subset(vp_data, Species == "11"))
summary(model_Rp4)
r2_nagelkerke(model_Rp4)
vif(model_Rp4)


#' Bateleur (n = 38) 
dplyr::count(subset(vp_data, Species == "2"))



