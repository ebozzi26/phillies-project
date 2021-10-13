library(tidyverse)
library(dplyr)
library(corrplot)
library(ggcorrplot)
aa <- read.csv("C:/Users/Admin/Downloads/fangraphs-minor-league-leaders (1).csv")
aaa <- read.csv("C:/Users/Admin/Downloads/fangraphs-minor-league-leaders (2).csv")

#Add in AAA plyer column to AA DF
aaaplay <- aaa[c(4,22)]
promotions <- merge(aa,aaaplay, by = "PlayerId", all = TRUE)
promotions <- filter(promotions, promotions$Season > 0)
promotions$AAA <- ifelse(promotions$Level.y == "AAA", 1, 0)
promotions$AAA[is.na(promotions$AAA)] <- 0
promotions <- promotions[-c(23)]


##MLB GOOD 
mlb <- read.csv("C:/Users/Admin/Downloads/FanGraphs Leaderboard (3).csv")
mlb$proj <- sqrt((mlb$WHIP)^2 + (mlb$FIP)^2 + (mlb$BB.9)^2)


#AA Command
promotions$command <- sqrt((promotions$WHIP)^2 + (promotions$FIP)^2 + (promotions$BB.9)^2)


##AA Correlation
promnumbers <- dplyr::select_if(promotions, is.numeric)
promnumbers <- promnumbers[-c(1)]

corr <- cor(promnumbers)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)

##AAA Command
aaa$command <- sqrt((aaa$WHIP)^2 + (aaa$FIP)^2 + (aaa$BB.9)^2)

###Correlate AA Command to AAA Command
aatotals <- aggregate(cbind(aapromotions$IP, aapromotions$K.9, aapromotions$BB.9, aapromotions$K.BB, aapromotions$HR.9, aapromotions$WHIP, 
                            aapromotions$ERA, aapromotions$FIP, aapromotions$E.F), 
                      by=list(PlayerID=aapromotions$PlayerId), FUN=mean)
colnames(aatotals) <- c("PlayerId", "AA.IP", "AA.K.9", "AA.BB.9", "AA.K.BB", "AA.HR.9", "AA.WHIP", "AA.ERA", "AA.FIP", "AA.E.F")
aatotals$AA.command <- sqrt((aatotals$AA.WHIP)^2 + (aatotals$AA.FIP)^2 + (aatotals$AA.BB.9)^2)

aaatotals <- aggregate(cbind(aaa$IP, aaa$K.9, aaa$BB.9, aaa$K.BB, aaa$HR.9, aaa$WHIP, 
                             aaa$ERA, aaa$FIP, aaa$E.F), 
                       by=list(PlayerID=aaa$PlayerId), FUN=mean)
colnames(aaatotals) <- c("PlayerId", "AAA.IP", "AAA.K.9", "AAA.BB.9", "AAA.K.BB", "AAA.HR.9", "AAA.WHIP", "AAA.ERA", "AAA.FIP", "AAA.E.F")

aaatotals$AAA.command <- sqrt((aaatotals$AAA.WHIP)^2 + (aaatotals$AAA.FIP)^2 + (aaatotals$AAA.BB.9)^2)

combined <- merge(aatotals, aaatotals, by = "PlayerId")

combined <- combined[-c(1)]
combinedcorr <- cor(combined)
ggcorrplot(combinedcorr, hc.order = TRUE, type = "lower",
           lab = TRUE)



##Kyle Wright
aa2018 <- filter(aa, aa$Season == 2018)

summary(aa2018)
aakbb <- aa2018[order(aa2018$K.BB),]



aa2021 <- filter(aa, aa$Season == 2021)
aa2021$command <- sqrt((aa2021$IP)^2 + (aa2021$WHIP)^2 + (aa2021$K.BB)^2 + (aa2021$FIP)^2 + (aa2021$ERA)^2)
write.csv(aa2021, "2021aa.csv")

scaledaa2021 <- aa2021 %>% mutate_if(is.numeric, scale)
scaledaa2021$command <- sqrt((scaledaa2021$IP)^2 + (scaledaa2021$WHIP)^2 + (scaledaa2021$K.BB)^2 + (scaledaa2021$FIP)^2 + (scaledaa2021$ERA)^2)

#Regression Model
promotions$AAA <- as.character(promotions$AAA)
model <- promotions %>% mutate_if(is.numeric, scale)
model$command <- sqrt((model$WHIP)^2 + (model$FIP)^2 + (model$BB.9)^2)
model$AAA <- as.numeric(model$AAA)

modelnum <- dplyr::select_if(model, is.numeric)
modelnum <- modelnum[-c(1)]
fit.full <- glm(AAA ~ .,
                data=modelnum, family=binomial())
summary(fit.full)
fit.reduced <- glm(AAA ~ command + xFIP + FIP + IP,
                   data=modelnum, family=binomial())

anova(fit.reduced, fit.full, test="Chisq")
coef(fit.reduced) 
exp(coef(fit.reduced))
model$prob <- predict(fit.reduced, newdata=model,
                         type="response")
#2012-2016 Comp
final <- read.csv("C:/Users/Admin/Downloads/fangraphs-minor-league-leaders (3).csv")
final <- final %>% mutate_if(is.numeric, scale)
final$command <- sqrt((final$WHIP)^2 + (final$FIP)^2 + (final$BB.9)^2)
final$prob <- predict(fit.reduced, newdata=final,
                      type="response")



#Restart
aa <- read.csv("C:/Users/Admin/Downloads/fangraphs-minor-league-leaders (4).csv")
aaa <- read.csv("C:/Users/Admin/Downloads/fangraphs-minor-league-leaders (5).csv")
aaas <- aaa %>% mutate_if(is.numeric, scale)
aas <- aa %>% mutate_if(is.numeric, scale)
aaas$command <- sqrt((aaas$WHIP)^2 + (aaas$FIP)^2 + (aaas$BB.9)^2)
summary(aaas$command)
aas$command <- sqrt((aas$WHIP)^2 + (aas$FIP)^2 + (aas$BB.9)^2)
aaap <- merge(aas,aaas, by = "PlayerId", all = TRUE)
aaap <- na.omit(aaap)
aaap$success <- ifelse(aaap$command.y < 0.9, 1,0)
aafinal <- aaap[c(1:22,44)]
aamodel <- dplyr::select_if(aafinal, is.numeric)

fit.full <- glm(success ~ .,
                data=aamodel, family=binomial())
summary(fit.full)

fit.reduced <- glm(success ~ K.BB.x + xFIP.x + FIP.x + IP.x + command.x,
                   data=aamodel, family=binomial())
anova(fit.reduced, fit.full, test="Chisq")

aafinal$prob <- predict(fit.reduced, newdata=aafinal,
                      type="response")
##Phillies
phillies <- read.csv("C:/Users/Admin/Downloads/fangraphs-minor-league-leaders (6).csv")
phillies <- phillies %>% mutate_if(is.numeric, scale)
phillies$command <- sqrt((phillies$WHIP)^2 + (phillies$FIP)^2 + (phillies$BB.9)^2)
colnames(phillies) <- c("Name", "Team", "Level", "Age", "IP.x",
                        "K.9.x", "BB.9.x", "K.BB.x", "HR.9.x", 
                        "K.x", "BB.x", "K.BB..x", "AVG.x", "WHIP.x", 
                        "BABIP.x", "LOB..x", "ERA.x", "FIP.x", "E.F.x",
                        "xFIP.x", "PlayerId", "command.x")
phillies$prob <- predict(fit.reduced, newdata=phillies,
                         type="response")
aabest <- filter(aafinal, aafinal$IP.x > 0)
