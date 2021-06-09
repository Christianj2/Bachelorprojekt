#Ændring af lambda til robustcheck

write.csv(Cycle_season[77:279,], file = "lamda-400k.csv")

Cycle_season_l1 <- as.data.frame(data$Tid)
Cycle_season_l1 <- rename(Cycle_season_l1, replace = c("data$Tid" = "Tid"))
Cycle_season_l1$Tid <- as.Date(Cycle_season_l1$Tid)

BNP <- hpfilter(Data_season$BNP, freq = 1600)
Cycle_season_l1$BNP <- BNP$cycle
KRE <- hpfilter(Data_season$KRE, freq = 1600)
Cycle_season_l1$KRE <- KRE$cycle
RUR <- hpfilter(Data_season$RUR, freq = 1600)
Cycle_season_l1$RUR <- RUR$cycle
RHI <- hpfilter(Data_season$RHI, freq = 1600)
Cycle_season_l1$RHI <- RHI$cycle

write.csv(Cycle_season_l1[77:279,], file = "lamda-1600.csv")

#######
Cycle_season_l2 <- as.data.frame(data$Tid)
Cycle_season_l2 <- rename(Cycle_season_l2, replace = c("data$Tid" = "Tid"))
Cycle_season_l2$Tid <- as.Date(Cycle_season_l2$Tid)

BNP <- hpfilter(Data_season$BNP, freq = 90000)
Cycle_season_l2$BNP <- BNP$cycle
KRE <- hpfilter(Data_season$KRE, freq = 90000)
Cycle_season_l2$KRE <- KRE$cycle
RUR <- hpfilter(Data_season$RUR, freq = 90000)
Cycle_season_l2$RUR <- RUR$cycle
RHI <- hpfilter(Data_season$RHI, freq = 90000)
Cycle_season_l2$RHI <- RHI$cycle

write.csv(Cycle_season_l2[77:279,], file = "lamda-90k.csv")
