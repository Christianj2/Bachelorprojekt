# Pakker

options(scipen=999)
devtools::install_github("mikkelkrogsholm/statsDK")

Packages <- c("readr", "dynlm", "lubridate", "devtools", "ggplot2", "countrycode", "rvest", "tidyr",  "knitr", "ggthemes", "date",
             "zoo", "dplyr", "plyr", "stringr", "scales", "ggrepel", "stargazer", "waffle", "grid", "reshape2",
             "tidyverse", "readxl", "astrochron", "mFilter", "tseries", "fpp", "FactoMineR", "gridExtra", "vars", "forecast",
             "aTSA", "pracma", "Quandl", "timeSeries", "statsDK", "writexl", "urca")

lapply(Packages, library, character.only = TRUE)

##### Plot-opsætning

#theme_set(theme_light())
#ggplot <- function(...) ggplot2::ggplot(...) + scale_color_brewer(palette="Set1", direction=1)
#th <- theme(title = element_text(colour = "#404040"),
#            plot.background=element_rect(fill="#f3f3f3"),  
#            panel.background = element_rect(fill="#f3f3f3"), 
#            legend.background = element_rect(fill="#f3f3f3"),
#            plot.subtitle = element_text(color="#666666"),
#            plot.caption = element_text(color="#AAAAAA", size=8),
#            legend.key = element_rect(fill = "#f3f3f3", colour = "#f3f3f3"),
#            plot.margin = unit(c(0.5, 0.7, 0.5, 0.7), "cm"))
# axis.title.x=element_blank() tilføjes, hvis x-titel ikke skal med
# axis.text.x=element_text(angle=90,hjust=1,vjust=0.5) tilføjes, hvis x-aksens navne skal roteres
# panel.grid.major.x = element_blank() tilføjes, hvis grid-stregerne skal fjernes (.minor.x eller y)
# HUSK at angive +labs(title = "", subtitle = "", x="", y="", caption = "Kilde:")
# Gemme plots: ggsave("p1.png", plot = p1, width = 20, height = 9, units = "cm", path = "")
# To liner plot.subtitle


th <- theme(title = element_text(colour = "#404040"),
            plot.subtitle = element_text(color="#666666"),
            plot.caption = element_text(color="#AAAAAA", size=8),
            legend.key = element_rect(fill = "#f3f3f3", colour = "#f3f3f3"),
            plot.margin = unit(c(0.5, 0.7, 0.5, 0.7), "cm"))

#Data
setwd("~/Dropbox/Bachelorprojekt/Data")

data <- read_excel("Data.xlsx", sheet = "Samlet")

#Sæsonjustere
Data_season <- as.data.frame(data$Tid)
ts_bnp <- ts(data$`Reale BNP`, frequency = 4, start = 1951)
ts_kre <- ts(data$`Reale udlån - banker`+data$`Reale udlån - realkredit`, frequency = 4, start = 1951)
#ts_rur <- ts(data$`Reale udlån - realkredit`, frequency = 4, start = 1951)
ts_rai <- ts(data$`Reale aktiepris indeks`, frequency = 4, start = 1951)
ts_rhi <- ts(data$`Reale huspriser indeks`, frequency = 4, start = 1951)
ts_ulr <- ts(data$Udlånsrente, frequency = 4, start = 1951)
ts_rkr <- ts(data$`Realkredit rente`, frequency = 4, start = 1951)

decompose_bnp <- decompose(ts_bnp, "additive")
decompose_kre <- decompose(ts_kre, "additive")
decompose_rur <- decompose(ts_rur, "additive")
decompose_rai <- decompose(ts_rai, "additive")
decompose_rhi <- decompose(ts_rhi, "additive")
decompose_ulr <- decompose(ts_ulr, "additive")
decompose_rkr <- decompose(ts_rkr, "additive")

adjust_bnp <- ts_bnp - decompose_bnp$seasonal
adjust_kre <- ts_kre - decompose_kre$seasonal
adjust_rur <- ts_rur - decompose_rur$seasonal
adjust_rai <- ts_rai - decompose_rai$seasonal
adjust_rhi <- ts_rhi - decompose_rhi$seasonal
adjust_ulr <- ts_ulr - decompose_ulr$seasonal
adjust_rkr <- ts_rkr - decompose_rkr$seasonal

Data_season$BNP <- as.numeric(adjust_bnp)
Data_season$KRE <- as.numeric(adjust_kre)
Data_season$RUR <- as.numeric(adjust_rur)
Data_season$RAI <- as.numeric(adjust_rai)
Data_season$RHI <- as.numeric(adjust_rhi)
Data_season$ULR <- as.numeric(adjust_ulr)
Data_season$RKR <- as.numeric(adjust_rkr)

#write_xlsx(Data_season,"test_test_test")

#Cyklus

Cycle_season <- as.data.frame(data$Tid)
Cycle_season <- rename(Cycle_season, replace = c("data$Tid" = "Tid"))
Cycle_season$Tid <- as.Date(Cycle_season$Tid)

BNP <- hpfilter(Data_season$BNP, freq = 400000)
Cycle_season$BNP <- BNP$cycle
KRE <- hpfilter(Data_season$KRE, freq = 400000)
Cycle_season$KRE <- KRE$cycle
RUR <- hpfilter(Data_season$RUR, freq = 400000)
Cycle_season$RUR <- RUR$cycle
RHI <- hpfilter(Data_season$RHI, freq = 400000)
Cycle_season$RHI <- RHI$cycle

#Plot Cyklus

p1 <- ggplot(Cycle_season, aes(x = Tid, y = BNP)) + geom_line() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-12000, 15000), breaks=round(seq(min(-12000), max(15000), by = 3000),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cycle of GDP", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="mil. kr", caption = "Source: (Abildgren, 2019 + DST)") +
  th

p2 <- ggplot(Cycle_season, aes(x = Tid, y = RUB)) + geom_line() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-80000, 200000), breaks=round(seq(min(-80000), max(200000), by = 20000),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cycle of total domestic non-bank credit extended by resident deposit banks", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="mil. kr", caption = "Source: (Abildgren, 2019 + DST)") +
  th

p3 <- ggplot(Cycle_season, aes(x = Tid, y = RUR)) + geom_line() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-60000, 100000), breaks=round(seq(min(-60000), max(100000), by = 10000),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cycle of total domestic non-bank credit extended by resident mortgage banks", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="mil. kr", caption = "Source: (Abildgren, 2019 + DST)") +
  th

p4 <- ggplot(Cycle_season, aes(x = Tid, y = RAI)) + geom_line() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-600, 600), breaks=round(seq(min(-600), max(600), by = 100),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cycle of the danish share price index", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="%-point", caption = "Source: (Abildgren, 2019 + DST)") +
  th

p5 <- ggplot(Cycle_season, aes(x = Tid, y = RHI)) + geom_line() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-30, 60), breaks=round(seq(min(-30), max(60), by = 10),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cycle of the danish house price index", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="%-point", caption = "Source: (Abildgren, 2019 + DST)") +
  th

p6 <- ggplot(Cycle_season, aes(x = Tid, y = ULR)) + geom_line() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-3, 6), breaks=round(seq(min(-3), max(6), by = 1),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cycle of average danish lending rate of deposit banks", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="%-point", caption = "Source: (Abildgren, 2019 + DST)") +
  th

p7 <- ggplot(Cycle_season, aes(x = Tid, y = RKR)) + geom_line() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-4, 7), breaks=round(seq(min(-4), max(7), by = 1),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) + 
  labs(title = "Cycle of the yield on 30-year mortgage bonds", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="%-point", caption = "Source: (Abildgren, 2019)") +
  th

#Save plots

#ggsave("BNP.pdf", plot = p1, width = 20, height = 9, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("RUB.pdf", plot = p2, width = 20, height = 9, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("RUR.pdf", plot = p3, width = 20, height = 9, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("RAI.pdf", plot = p4, width = 20, height = 9, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("RHI.pdf", plot = p5, width = 20, height = 9, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("ULR.pdf", plot = p6, width = 20, height = 9, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("RKR.pdf", plot = p7, width = 20, height = 9, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")

#Robust check hp-filter

hp_robust1 <- read.csv("lamda-1600.csv", sep = ",")
hp_robust2 <- read.csv("lamda-90k.csv", sep = ",")
hp_robust3 <- read.csv("lamda-400k.csv", sep = ",")

colors2.1 <- c("#F8766D","#619CFF","#00BA38")

p1.1 <- tibble(L1 = hp_robust1$BNP,
               L2 = hp_robust2$BNP,
               L3 = hp_robust3$BNP,
               Tid = as.Date(hp_robust1$Tid)) %>% gather(variable, value, -Tid) %>%
  ggplot(aes(x=Tid, y=value, color=variable)) + geom_line(show.legend = T) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-12000, 15000), breaks=round(seq(min(-12000), max(15000), by = 3000),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cycle of GDP", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="mil. kr", caption = "Source: (Abildgren, 2021 + DST)") +
  th + theme(legend.position="bottom") + theme(legend.key=element_blank()) + scale_color_manual(name="Lambda-value",
                                                                                                  labels=c("1.600","90.000","400.000"),
                                                                                                  values=c("#F8766D","#619CFF","#00BA38"))

p2.1<- tibble(L1 = hp_robust1$KRE,
              L2 = hp_robust2$KRE,
              L3 = hp_robust3$KRE,
              Tid = as.Date(hp_robust1$Tid)) %>% gather(variable, value, -Tid) %>%
  ggplot(aes(x=Tid, y=value, color=variable)) + geom_line(show.legend = T) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-109050, 300000), breaks=round(seq(min(-100000), max(300000), by = 50000),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cycle of total credit", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="mil. kr", caption = "Source: (Abildgren, 2021 + DST)") +
  th + theme(legend.position="bottom") + theme(legend.key=element_blank()) + scale_color_manual(name="Lambda-value",
                                                                                                labels=c("1.600","90.000","400.000"),
                                                                                                values=c("#F8766D","#619CFF","#00BA38"))

#p3.1 <- tibble(L1 = hp_robust1$RUR,
#               L2 = hp_robust2$RUR,
#               L3 = hp_robust3$RUR,
#               Tid = as.Date(hp_robust1$Tid)) %>% gather(variable, value, -Tid) %>%
#  ggplot(aes(x=Tid, y=value, color=variable)) + geom_line(show.legend = T) +
#  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-60000, 100000), breaks=round(seq(min(-60000), max(100000), by = 20000),1)) +
#  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
#  labs(title = "Cycle of total domestic non-bank credit extended by resident mortgage banks", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="mil. kr", caption = "Source: (Abildgren, 2019 + DST)") +
#  th + theme(legend.position="bottom") + theme(legend.key=element_blank()) + scale_color_manual(name="Lambda-value",
                                                                                            #   labels=c("1.600","90.000","400.000"),
                                                                                            #   values=c("#F8766D","#619CFF","#00BA38"))

#p4.1 <- tibble(L1 = hp_robust1$RAI,
#               L2 = hp_robust2$RAI,
#               L3 = hp_robust3$RAI,
#               Tid = as.Date(hp_robust1$Tid)) %>% gather(variable, value, -Tid) %>%
#  ggplot(aes(x=Tid, y=value, color=variable)) + geom_line(show.legend = T) +
#  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-600, 600), breaks=round(seq(min(-600), max(600), by = 100),1)) +
#  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
#  labs(title = "Cycle of the danish share price index", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="%-point", caption = "Source: (Abildgren, 2019 + DST)") +
#  th + theme(legend.position="bottom") + theme(legend.key=element_blank()) + scale_color_manual(name="Lambda-value",
                                                                                             #   labels=c("1.600","90.000","400.000"),
                                                                                             #   values=c("#F8766D","#619CFF","#00BA38"))

p5.1 <- tibble(L1 = hp_robust1$RHI,
               L2 = hp_robust2$RHI,
               L3 = hp_robust3$RHI,
               Tid = as.Date(hp_robust1$Tid)) %>% gather(variable, value, -Tid) %>%
  ggplot(aes(x=Tid, y=value, color=variable)) + geom_line(show.legend = T) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-30, 60), breaks=round(seq(min(-30), max(60), by = 10),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cycle of the danish house price index", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="%-point", caption = "Source: (Abildgren, 2021 + DST)") +
  th + theme(legend.position="bottom") + theme(legend.key=element_blank()) + scale_color_manual(name="Lambda-value",
                                                                                                labels=c("1.600","90.000","400.000"),
                                                                                                values=c("#F8766D","#619CFF","#00BA38"))


#p6.1 <- tibble(L1 = hp_robust1$ULR,
#               L2 = hp_robust2$ULR,
#               L3 = hp_robust3$ULR,
#               Tid = as.Date(hp_robust1$Tid)) %>% gather(variable, value, -Tid) %>%
#  ggplot(aes(x=Tid, y=value, color=variable)) + geom_line(show.legend = T) +
#  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-3, 6), breaks=round(seq(min(-3), max(6), by = 1),1)) +
#  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
#  labs(title = "Cycle of average danish lending rate of deposit banks", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="%-point", caption = "Source: (Abildgren, 2019 + DST)") +
#  th + theme(legend.position="bottom") + theme(legend.key=element_blank()) + scale_color_manual(name="Lambda-value",
                                                                                            #    labels=c("1.600","90.000","400.000"),
                                                                                             #   values=c("#F8766D","#619CFF","#00BA38"))


  #p7.1 <- tibble(L1 = hp_robust1$ULR,
  #             L2 = hp_robust2$ULR,
  #             L3 = hp_robust3$ULR,
  #             Tid = as.Date(hp_robust1$Tid)) %>% gather(variable, value, -Tid) %>%
  #ggplot(aes(x=Tid, y=value, color=variable)) + geom_line(show.legend = T) +
  #scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-4, 7), breaks=round(seq(min(-4), max(7), by = 1),1)) +
  #scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) + 
  #labs(title = "Cycle of the yield on 30-year mortgage bonds", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="%-point", caption = "Source: (Abildgren, 2019)") +
  #th + theme(legend.position="bottom") + theme(legend.key=element_blank()) + scale_color_manual(name="Lambda-value",
                                                                                          #      labels=c("1.600","90.000","400.000"),
                                                                                            #    values=c("#F8766D","#619CFF","#00BA38"))

ggsave("BNP_hp.pdf", plot = p1.1, width = 20, height = 12, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
ggsave("KRE_hp.pdf", plot = p2.1, width = 20, height = 12, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("RUR_hp.pdf", plot = p3.1, width = 20, height = 12, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("RAI_hp.pdf", plot = p4.1, width = 20, height = 9, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
ggsave("RHI_hp.pdf", plot = p5.1, width = 20, height = 12, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("ULR_hp.pdf", plot = p6.1, width = 20, height = 9, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("RKR_hp.pdf", plot = p7.1, width = 20, height = 9, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")

#Finasiel cyklus 

Finansiel_data <- as.data.frame(data$Tid)
Finansiel_data <- rename(Finansiel_data, replace = c("data$Tid" = "Tid"))  
Finansiel_data$Tid <- as.Date(Finansiel_data$Tid)
Finansiel_data$Kredit <- data$`Reale udlån - banker` + data$`Reale udlån - realkredit`
Finansiel_data$Hus <- data$`Reale huspriser indeks`

ts_f_kredit <- ts(Finansiel_data$Kredit, frequency = 4, start = 1951)
ts_f_hus <- ts(Finansiel_data$Hus, frequency = 4, start = 1951)
decompose_f_kredit <- decompose(ts_f_kredit, "additive")
decompose_f_hus <- decompose(ts_f_hus, "additive")
adjust_f_kredit <- ts_f_kredit - decompose_f_kredit$seasonal
adjust_f_hus <- ts_f_hus - decompose_f_hus$seasonal

Finansiel_data$Kredit <- as.numeric(adjust_f_kredit)
Finansiel_data$Hus <- as.numeric(adjust_f_hus)

hp_f_kredit <- hpfilter(log(Finansiel_data$Kredit), freq = 400000)
Finansiel_data$Kredit_cyklus <- hp_f_kredit$cycle
Finansiel_data$Kredit_trend <- as.numeric(hp_f_kredit$trend)
hp_f_hus <- hpfilter(log(Finansiel_data$Hus), freq = 400000)
Finansiel_data$Hus_cyklus <- hp_f_hus$cycle
Finansiel_data$Hus_trend <- as.numeric(hp_f_hus$trend)

p8 <- ggplot(Finansiel_data[77:279,], aes(x = Tid, y = Kredit_trend)) + geom_line() +
  geom_line(aes(y = Kredit_trend + Kredit_cyklus), color = "#D55E00") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(11, 14.5), breaks=round(seq(min(11), max(14.5), by = 0.5),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Trend and cycle of credit", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="Log", caption = "Source: (Abildgren, 2021 + DST)") +
  th 

p9 <- ggplot(Finansiel_data[77:279,], aes(x = Tid, y = Kredit_cyklus*100)) + geom_line(color = "#D55E00") +
  geom_line(y = 0, color = "#999999") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-20, 25), breaks=round(seq(min(-20), max(25), by = 5),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyclical deviation of credit", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="Devation from trend in %-point", caption = "Source: (Abildgren, 2021 + DST)") +
  th 

p10 <- ggplot(Finansiel_data[77:279,], aes(x = Tid, y = Hus_trend)) + geom_line() +
  geom_line(aes(y = Hus_trend + Hus_cyklus), color = "#D55E00") + 
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(3.7, 5.4), breaks=round(seq(min(3.7), max(5.4), by = 0.2),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Trend and cycle of house price index", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="Log", caption = "Source: (Abildgren, 2021 + DST)") +
  th 

p11 <- ggplot(Finansiel_data[77:279,], aes(x = Tid, y = Hus_cyklus*100)) + geom_line(color = "#D55E00") +
  geom_line(y = 0, color = "#999999") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-30, 35), breaks=round(seq(min(-30), max(35), by = 5),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyclical deviation of house price index", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="Devation from trend in %-point", caption = "Source: (Abildgren, 2021 + DST)") +
  th 

#ggsave("hp_kredit.pdf", plot = grid.arrange(p8,p9, ncol=2, nrow=1), width = 35, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("hp_hus.pdf", plot = grid.arrange(p10,p11, ncol=2, nrow=1), width = 35, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")

#Realøkonomisk cyklus

Real_data <- as.data.frame(data$Tid)
Real_data <- rename(Real_data, replace = c("data$Tid" = "Tid"))  
Real_data$Tid <- as.Date(Real_data$Tid)  
Real_data$BNP <- data$`Reale BNP`

ts_f_bnp <- ts(Real_data$BNP, frequency = 4, start = 1951)
decompose_f_bnp <- decompose(ts_f_bnp, "additive")
adjust_f_bnp <- ts_f_bnp - decompose_f_bnp$seasonal

Real_data$BNP <- as.numeric(adjust_f_bnp)

hp_f_bnp <- hpfilter(log(Real_data$BNP), freq = 90000)
Real_data$BNP_cyklus <- hp_f_bnp$cycle
Real_data$BNP_trend <- as.numeric(hp_f_bnp$trend)

p12 <- ggplot(Real_data[77:279,], aes(x = Tid, y = BNP_trend)) + geom_line() +
  geom_line(aes(y = BNP_trend + BNP_cyklus), color = "#D55E00") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(10.2, 12.4), breaks=round(seq(min(10.2), max(12.4), by = 0.2),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Trend and cycle of GDP", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="Log", caption = "Source: (Abildgren, 2021 + DST)") +
  th 

p13 <- ggplot(Real_data[77:279,], aes(x = Tid, y = BNP_cyklus*100)) + geom_line(color = "#D55E00") +
  geom_line(y = 0, color = "#999999") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-15, 15), breaks=round(seq(min(-15), max(15), by = 2.5),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyclical deviation of GDP", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="Devation from trend in %-point", caption = "Source: (Abildgren, 2021 + DST)") +
  th 

#ggsave("hp_bnp.pdf", plot = grid.arrange(p12,p13, ncol=2, nrow=1), width = 35, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")

#Økonomisk cyklus
Samlet_cykles <- as.data.frame(data$Tid)
Samlet_cykles <- rename(Samlet_cykles, replace = c("data$Tid" = "Tid"))
Samlet_cykles$Tid <- as.Date(Samlet_cykles$Tid)
Samlet_cykles$BNP <- Real_data$BNP_cyklus*100
Samlet_cykles$Kredit <- Finansiel_data$Kredit_cyklus*100
Samlet_cykles$Hus <- Finansiel_data$Hus_cyklus*100

colors2 <- c("GDP" = "#F8766D", "Credit (Right axis)" = "#619CFF", "House price index (Right axis)" = "#00BA38")


p14 <- ggplot(Samlet_cykles[77:279,], aes(x=Tid, y=BNP, color = "GDP")) + geom_line() +
  geom_line(aes(y=Kredit*15/40, color = "Credit (Right axis)")) +
  geom_line(aes(y=Hus*15/40, color = "House price index (Right axis)")) +
  geom_line(y=0, color = "darkgrey") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-15, 15), breaks=round(seq(min(-15), max(15), by = 5),1),
                     sec.axis = sec_axis(~ .*40/15 , name = "Devation from trend in %-point", breaks=round(seq(min(-40), max(40), by = 10),1))) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("3 years")) +
  labs(title = "Economic cycles for Denmark", subtitle="Seasonally adjusted and deflated by CPI", x=NULL, y="Devation from trend in %-point", caption = "Source: (Abildgren, 2021 + DST)", color=NULL) +
  theme(legend.position="bottom") + th +
  theme(legend.key=element_blank()) + 
  scale_color_manual(values = colors2) + theme(axis.title.y.right = element_text(margin = unit(c(0, 0, 0, 2.5), 'mm')))

fill2 <- c("#F8766D","#00BA38","#619CFF")
scale_fill_manual(values = fill2, labels = c("GDP", "House","Credit"))

#ggsave("Cyklus.pdf", plot = p14, width = 25, height = 12, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")

#Autokorrelation

Auto_data <- as.data.frame(c(-5:5))
Auto_data <- rename(Auto_data, replace = c("c(-5:5)" = "Lag"))

Auto_BNP <- ccf(Samlet_cykles$BNP, Samlet_cykles$BNP, 5, plot = F)
Auto_data$Auto_BNP <- as.numeric(Auto_BNP$acf)

Auto_Kredit <- ccf(Samlet_cykles$Kredit, Samlet_cykles$Kredit, 5, plot = F)
Auto_data$Auto_Kredit <- as.numeric(Auto_Kredit$acf)

Auto_Hus <- ccf(Samlet_cykles$Hus, Samlet_cykles$Hus, 5, plot = F)
Auto_data$Auto_Hus <- as.numeric(Auto_Hus$acf)

kryds_BNP_Kredit <- ccf(Samlet_cykles$BNP, Samlet_cykles$Kredit, 5, plot = F)
Kryds_Hus_Kredit <- ccf(Samlet_cykles$Hus, Samlet_cykles$Kredit, 5, plot = F)
Auto_data$K_BNP_Kredit <- as.numeric(kryds_BNP_Kredit$acf)
Auto_data$K_Hus_Kredit <- as.numeric(Kryds_Hus_Kredit$acf)


kryds_Kredit_BNP <- ccf(Samlet_cykles$Kredit, Samlet_cykles$BNP, 5, plot = F)
kryds_Hus_BNP <- ccf(Samlet_cykles$Hus, Samlet_cykles$BNP, 5, plot = F)
Auto_data$K_Kredit_BNP <- as.numeric(kryds_Kredit_BNP$acf)
Auto_data$K_Hus_BNP <- as.numeric(kryds_Hus_BNP$acf)

kryds_BNP_Hus <- ccf(Samlet_cykles$BNP, Samlet_cykles$Hus, 5, plot = F)
kryds_Kredit_Hus <- ccf(Samlet_cykles$Kredit, Samlet_cykles$Hus, 5, plot = F)
Auto_data$K_BNP_Hus <- as.numeric(kryds_BNP_Hus$acf)
Auto_data$K_Kredit_Hus <- as.numeric(kryds_Kredit_Hus$acf)

colors <- c("GDP" = "#F8766D", "Credit" = "#619CFF", "House price index" = "#00BA38")

p15 <- ggplot(Auto_data) + 
  geom_line(aes(x = Lag, y = 0)) +
  stat_smooth(aes(y = Auto_Kredit, x = Lag, color = "Credit"), formula = y ~ s(x, k = 11), method = "gam", se = FALSE) +
  stat_smooth(aes(y = Auto_Hus, x = Lag, color = "House price index"), formula = y ~ s(x, k = 11), method = "gam", se = FALSE) +
  geom_segment(aes(x = 0, y = 1, xend = -5, yend = 0.12, color = "GDP")) +
  geom_segment(aes(x = 0, y = 1, xend = 5, yend = 0.12, color = "GDP")) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-1, 1), breaks=round(seq(min(-1), max(1), by = 0.5),1)) +
  scale_x_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-5, 5), breaks=round(seq(min(-5), max(5), by = 1),1)) +
  labs(title = "Cross-correlations", x="Lag (quarters)", y="Correlation", caption = "", color=NULL) +
  theme(legend.position="bottom") + th + theme(legend.key=element_blank()) + 
  scale_color_manual(values = colors)

p16 <- ggplot(Auto_data)+
  geom_line(aes(x = Lag, y = 0)) +
  stat_smooth(aes(y = K_BNP_Kredit, x = Lag, color = "GDP"), formula = y ~ s(x, k = 11), method = "gam", se = FALSE) +
  stat_smooth(aes(y = K_Hus_Kredit, x = Lag, color = "House price index"), formula = y ~ s(x, k = 11), method = "gam", se = FALSE) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-1, 1), breaks=round(seq(min(-1), max(1), by = 0.5),1)) +
  scale_x_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-5, 5), breaks=round(seq(min(-5), max(5), by = 1),1)) +
  labs(title = "Cross-correlations: Credit", x="Lag (quarters)", y="Correlation", caption = "Source: Own calculation", color=NULL) +
  theme(legend.position="bottom") + th + theme(legend.key=element_blank()) +
  scale_color_manual(values = colors)

p17 <- ggplot(Auto_data) +
  geom_line(aes(x = Lag, y = 0)) +
  stat_smooth(aes(y = K_Kredit_BNP, x = Lag, color = "Credit"), formula = y ~ s(x, k = 11), method = "gam", se = FALSE) +
  stat_smooth(aes(y = K_Hus_BNP, x = Lag, color = "House price index"), formula = y ~ s(x, k = 11), method = "gam", se = FALSE) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-1, 1), breaks=round(seq(min(-1), max(1), by = 0.5),1)) +
  scale_x_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-5, 5), breaks=round(seq(min(-5), max(5), by = 1),1)) +
  labs(title = "Cross-correlations: GDP", x="Lag (quarters)", y="Correlation", caption = "", color=NULL) +
  theme(legend.position="bottom") + th + theme(legend.key=element_blank()) +
  scale_color_manual(values = colors)

p18 <- ggplot(Auto_data) +
  geom_line(aes(x = Lag, y = 0)) +
  stat_smooth(aes(y = K_Kredit_Hus, x = Lag, color = "Credit"), formula = y ~ s(x, k = 11), method = "gam", se = FALSE) +
  stat_smooth(aes(y = K_BNP_Hus, x = Lag, color = "GDP"), formula = y ~ s(x, k = 11), method = "gam", se = FALSE) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-1, 1), breaks=round(seq(min(-1), max(1), by = 0.5),1)) +
  scale_x_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-5, 5), breaks=round(seq(min(-5), max(5), by = 1),1)) +
  labs(title = "Cross-correlations: House Price Index", x="Lag (quarters)", y="Correlation", caption = "", color=NULL) +
  theme(legend.position="bottom") + th + theme(legend.key=element_blank()) +
  scale_color_manual(values = colors)

#ggsave("Auto.pdf", plot = grid.arrange(p15,p17,p18,p16, ncol=2, nrow=2), width = 30, height = 20, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("crosss.pdf", p15, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("crosss_credit.pdf", p16, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("crosss_gdp.pdf", p17, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("crosss_house.pdf", p18, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")

#stargazer(Auto_data, type = "latex", summary = FALSE, title = "Krydskorrelation", notes = "Egen beregning")

# Var-model

Var <- data.frame(Date = Samlet_cykles$Tid,
                  BNP = Samlet_cykles$BNP,
                  HUS = Samlet_cykles$Hus,
                  KREDIT = Samlet_cykles$Kredit)

#Var <- Var[81:279,]

#Var.BNP <- ts(Var$BNP, start = 1951, freq = 4)
#Var.Kredit <- ts(Var$KREDIT, start = 1951, frequency = 4)
#Var.HUS <- ts(Var$HUS, start = 1951, frequency = 4)
#Var <- cbind(Var.BNP,Var.Kredit, Var.HUS)


#Test stationaritet på 5 lags

#adf.test(Var$BNP)
#adf.test(Var$KREDIT)
#adf.test(Var$HUS)

#stat_bnp <- ur.df(Samlet_cykles$BNP, lags = 5, type = "drift")
#stat_kredit <- ur.df(Samlet_cykles$Kredit, lags = 5, type = "drift")
#stat_hus <- ur.df(Samlet_cykles$Hus, lags = 5, type = "drift")

VARselect(Var[,c(2:4)], lag.max = 13, type = c("const", "trend", "both", "none"))$selection

model1 <- VAR(Var[,c(2:4)], p = 5, type = "const", ic = c("AIC", "HQ", "SC", "FPE"))

stargazer::stargazer(model1$varresult$BNP,
                     model1$varresult$HUS,
                     model1$varresult$KREDIT,
                     title = "summary statitics on VAR(5)",
                     no.space = T, align = T)

#Stable model
roots(model1, modulus = TRUE)

#Diagnotics

#81:279


serial1 <- serial.test(model1, lags.pt = 12, type = c("PT.asymptotic"))

arch1 <- arch.test(model1, lags.multi = 12, multivariate.only = TRUE)

norm1 <- normality.test(model1, multivariate.only = TRUE)

stab1 <- stability(model1, type = "OLS-CUSUM")
plot(stab1)


or1 <- irf(VAR(Var[c(1,2,3,4)][,c(2:4)], p=5), n.ahead = 50, ci = 0.95)

or1_bnp <- as.data.frame(or1$irf$BNP)
or1_bnp$N <- c(0:50)
or1_hus <- as.data.frame(or1$irf$HUS)
or1_hus$N <- c(0:50)
or1_kredit <- as.data.frame(or1$irf$KREDIT)
or1_kredit$N <- c(0:50)
upper_bnp <- as.data.frame(or1$Upper$BNP)
upper_bnp$N <- c(0:50)
upper_hus <- as.data.frame(or1$Upper$HUS)
upper_hus$N <- c(0:50)
upper_kredit <- as.data.frame(or1$Upper$KREDIT)
upper_kredit$N <- c(0:50)
lower_bnp <- as.data.frame(or1$Lower$BNP)
lower_bnp$N <- c(0:50)
lower_hus <- as.data.frame(or1$Lower$HUS)
lower_hus$N <- c(0:50)
lower_kredit <- as.data.frame(or1$Lower$KREDIT)
lower_kredit$N <- c(0:50)

p23 <- ggplot(or1_bnp, aes(x = N, y = BNP)) + geom_line() +
  geom_line(aes(y = 0), color = "black") + 
  geom_line(aes(y = lower_bnp$BNP), linetype = "dashed") +
  geom_line(aes(y = upper_bnp$BNP), linetype = "dashed") +
  scale_y_continuous(limits = c(-1, 3), breaks=round(seq(min(-1), max(3), by = 0.5),1)) +
  scale_x_continuous(breaks = seq(0,50, by = 5), limits = c(0,50)) +
  labs(title = "Shock to GDP",y = "Respons on GDP", x ="Lags (Quarterly)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5))

p24 <- ggplot(or1_bnp, aes(x = N, y = HUS)) + geom_line() +
  geom_line(aes(y = 0), color = "black") + 
  geom_line(aes(y = lower_bnp$HUS), linetype = "dashed") +
  geom_line(aes(y = upper_bnp$HUS), linetype = "dashed") +
  scale_y_continuous(limits = c(-1, 1.7), breaks=round(seq(min(-1), max(1.7), by = 0.50),2)) +
  scale_x_continuous(breaks = seq(0,50, by = 5), limits = c(0,50)) +
  labs(title = "",y = "Respons on House", x ="Lags (Quarterly)",caption = "") +
  th

p25 <- ggplot(or1_bnp, aes(x = N, y = KREDIT)) + geom_line() +
  geom_line(aes(y = 0), color = "black") +
  geom_line(aes(y = lower_bnp$KREDIT), linetype = "dashed") +
  geom_line(aes(y= upper_bnp$KREDIT), linetype = "dashed") +
  scale_y_continuous(limits = c(-1.25, 0.8), breaks=round(seq(min(-1.25), max(0.8), by = 0.4),1)) +
  scale_x_continuous(breaks = seq(0,50, by = 5), limits = c(0,50)) +
  labs(title = "",y = "Respone on Credit", x ="Lags (Quarterly)",caption = "") +
  th

p26 <- ggplot(or1_hus, aes(x = N, y = BNP)) + geom_line() +
  geom_line(aes(y = 0), color = "black") +
  geom_line(aes(y = lower_hus$BNP), linetype = "dashed") +
  geom_line(aes(y= upper_hus$BNP), linetype = "dashed") +
  scale_y_continuous(limits = c(-0.50, 1.50), breaks=round(seq(min(-0.50), max(1.50), by = 0.25),2)) +
  scale_x_continuous(breaks = seq(0,50, by = 5), limits = c(0,50)) +
  labs(title = "Shock to House",y = "", x ="Lags (Quarterly)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5))

p27 <- ggplot(or1_hus, aes(x = N, y = HUS)) + geom_line() +
  geom_line(aes(y = 0), color = "black") +
  geom_line(aes(y = lower_hus$HUS), linetype = "dashed") +
  geom_line(aes(y= upper_hus$HUS), linetype = "dashed") +
  scale_y_continuous(limits = c(-1.5, 4.2), breaks=round(seq(min(-1), max(4), by = 1),1)) +
  scale_x_continuous(breaks = seq(0,50, by = 5), limits = c(0,50)) +
  labs(title = "",y = "", x ="Lags (Quarterly)",caption = "") +
  th

p28 <- ggplot(or1_hus, aes(x = N, y = KREDIT)) + geom_line() +
  geom_line(aes(y = 0), color = "black") +
  geom_line(aes(y = lower_hus$KREDIT), linetype = "dashed") +
  geom_line(aes(y= upper_hus$KREDIT), linetype = "dashed") +
  scale_y_continuous(limits = c(-1, 2.3), breaks=round(seq(min(-1), max(2.3), by = 0.5),1)) +
  scale_x_continuous(breaks = seq(0,50, by = 5), limits = c(0,50)) +
  labs(title = "",y = "", x ="Lags (Quarterly)",caption = "") +
  th

p29 <- ggplot(or1_kredit, aes(x = N, y = BNP)) + geom_line() +
  geom_line(aes(y = 0), color = "black") +
  geom_line(aes(y = lower_kredit$BNP), linetype = "dashed") +
  geom_line(aes(y= upper_kredit$BNP), linetype = "dashed") +
  scale_y_continuous(limits = c(-0.5, 1), breaks=round(seq(min(-0.5), max(1), by = 0.2),1)) +
  scale_x_continuous(breaks = seq(0,50, by = 5), limits = c(0,50)) +
  labs(title = "Shock to Credit",y = "", x ="Lags (Quarterly)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5))

p30 <- ggplot(or1_kredit, aes(x = N, y = HUS)) + geom_line() +
  geom_line(aes(y = 0), color = "black") +
  geom_line(aes(y = lower_kredit$HUS), linetype = "dashed") +
  geom_line(aes(y= upper_kredit$HUS), linetype = "dashed") +
  scale_y_continuous(limits = c(-1.6, 1.8), breaks=round(seq(min(-1.6), max(1.8), by = 0.4),1)) +
  scale_x_continuous(breaks = seq(0,50, by = 5), limits = c(0,50)) +
  labs(title = "",y = "", x ="Lags (Quarterly)",caption = "") +
  th

p31 <- ggplot(or1_kredit, aes(x = N, y = KREDIT)) + geom_line() +
  geom_line(aes(y = 0), color = "black") +
  geom_line(aes(y = lower_kredit$KREDIT), linetype = "dashed") +
  geom_line(aes(y= upper_kredit$KREDIT), linetype = "dashed") +
  scale_y_continuous(limits = c(-1, 2.5), breaks=round(seq(min(-1), max(2.5), by = 0.5),1)) +
  scale_x_continuous(breaks = seq(0,50, by = 5), limits = c(0,50)) +
  labs(title = "",y = "", x ="Lags (Quartely)",caption = "") +
  th

ggsave("IRF.pdf", plot = grid.arrange(p23, p26, p29, p24, p27, p30, p25, p28, p31, nrow =3, ncol=3), width = 30, height = 30, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")


#Kontrol-ordning

or2 <- irf(VAR(Var[c(1,2,4,3)][,c(2:4)], p=5), n.ahead = 50, ci = 0.95)
or3 <- irf(VAR(Var[c(1,3,2,4)][,c(2:4)], p=5), n.ahead = 50, ci = 0.95)
or4 <- irf(VAR(Var[c(1,3,4,2)][,c(2:4)], p=5), n.ahead = 50, ci = 0.95)
or5 <- irf(VAR(Var[c(1,4,2,3)][,c(2:4)], p=5), n.ahead = 50, ci = 0.95)
or6 <- irf(VAR(Var[c(1,4,3,2)][,c(2:4)], p=5), n.ahead = 50, ci = 0.95)


Data_or1_bnp <- as.data.frame(or1$irf$BNP)
Data_or1_hus <- as.data.frame(or1$irf$HUS)
Data_or1_kredit <- as.data.frame(or1$irf$KREDIT)
Data_or2_bnp <- as.data.frame(or2$irf$BNP)
Data_or2_hus <- as.data.frame(or2$irf$HUS)
Data_or2_kredit <- as.data.frame(or2$irf$KREDIT)
Data_or3_bnp <- as.data.frame(or3$irf$BNP)
Data_or3_hus <- as.data.frame(or3$irf$HUS)
Data_or3_kredit <- as.data.frame(or3$irf$KREDIT)
Data_or4_bnp <- as.data.frame(or4$irf$BNP)
Data_or4_hus <- as.data.frame(or4$irf$HUS)
Data_or4_kredit <- as.data.frame(or4$irf$KREDIT)
Data_or5_bnp <- as.data.frame(or5$irf$BNP)
Data_or5_hus <- as.data.frame(or5$irf$HUS)
Data_or5_kredit <- as.data.frame(or5$irf$KREDIT)
Data_or6_bnp <- as.data.frame(or6$irf$BNP)
Data_or6_hus <- as.data.frame(or6$irf$HUS)
Data_or6_kredit <- as.data.frame(or6$irf$KREDIT)

Upper_or1_bnp <- as.data.frame(or1$Upper$BNP)
Upper_or1_hus <- as.data.frame(or1$Upper$HUS)
Upper_or1_kredit <- as.data.frame(or1$Upper$KREDIT)
Upper_or2_bnp <- as.data.frame(or2$Upper$BNP)
Upper_or2_hus <- as.data.frame(or2$Upper$HUS)
Upper_or2_kredit <- as.data.frame(or2$Upper$KREDIT)
Upper_or3_bnp <- as.data.frame(or3$Upper$BNP)
Upper_or3_hus <- as.data.frame(or3$Upper$HUS)
Upper_or3_kredit <- as.data.frame(or3$Upper$KREDIT)
Upper_or4_bnp <- as.data.frame(or4$Upper$BNP)
Upper_or4_hus <- as.data.frame(or4$Upper$HUS)
Upper_or4_kredit <- as.data.frame(or4$Upper$KREDIT)
Upper_or5_bnp <- as.data.frame(or5$Upper$BNP)
Upper_or5_hus <- as.data.frame(or5$Upper$HUS)
Upper_or5_kredit <- as.data.frame(or5$Upper$KREDIT)
Upper_or6_bnp <- as.data.frame(or6$Upper$BNP)
Upper_or6_hus <- as.data.frame(or6$Upper$HUS)
Upper_or6_kredit <- as.data.frame(or6$Upper$KREDIT)

Lower_or1_bnp <- as.data.frame(or1$Lower$BNP)
Lower_or1_hus <- as.data.frame(or1$Lower$HUS)
Lower_or1_kredit <- as.data.frame(or1$Lower$KREDIT)
Lower_or2_bnp <- as.data.frame(or2$Lower$BNP)
Lower_or2_hus <- as.data.frame(or2$Lower$HUS)
Lower_or2_kredit <- as.data.frame(or2$Lower$KREDIT)
Lower_or3_bnp <- as.data.frame(or3$Lower$BNP)
Lower_or3_hus <- as.data.frame(or3$Lower$HUS)
Lower_or3_kredit <- as.data.frame(or3$Lower$KREDIT)
Lower_or4_bnp <- as.data.frame(or4$Lower$BNP)
Lower_or4_hus <- as.data.frame(or4$Lower$HUS)
Lower_or4_kredit <- as.data.frame(or4$Lower$KREDIT)
Lower_or5_bnp <- as.data.frame(or5$Lower$BNP)
Lower_or5_hus <- as.data.frame(or5$Lower$HUS)
Lower_or5_kredit <- as.data.frame(or5$Lower$KREDIT)
Lower_or6_bnp <- as.data.frame(or6$Lower$BNP)
Lower_or6_hus <- as.data.frame(or6$Lower$HUS)
Lower_or6_kredit <- as.data.frame(or6$Lower$KREDIT)

irf_bnp_bnp <- tibble(R1 = Data_or1_bnp$BNP,
                      R2 = Data_or2_bnp$BNP,
                      R3 = Data_or3_bnp$BNP,
                      R4 = Data_or4_bnp$BNP,
                      R5 = Data_or5_bnp$BNP,
                      R6 = Data_or6_bnp$BNP,
                      N = c(0:50),
                      inte = "base") %>% gather(variable, Change, -N, -inte)


irf_bnp_hus <- tibble(R1 = Data_or1_bnp$HUS,
                      R2 = Data_or2_bnp$HUS,
                      R3 = Data_or3_bnp$HUS,
                      R4 = Data_or4_bnp$HUS,
                      R5 = Data_or5_bnp$HUS,
                      R6 = Data_or6_bnp$HUS,
                      N = c(0:50),
                      inte = "base") %>% gather(variable, Change, -N, -inte)

irf_bnp_kredit <- tibble(R1 = Data_or1_bnp$KREDIT,
                         R2 = Data_or2_bnp$KREDIT,
                         R3 = Data_or3_bnp$KREDIT,
                         R4 = Data_or4_bnp$KREDIT,
                         R5 = Data_or5_bnp$KREDIT,
                         R6 = Data_or6_bnp$KREDIT,
                         N = c(0:50),
                         inte = "base") %>% gather(variable, Change, -N, -inte)

irf_hus_bnp <- tibble(R1 = Data_or1_hus$BNP,
                      R2 = Data_or2_hus$BNP,
                      R3 = Data_or3_hus$BNP,
                      R4 = Data_or4_hus$BNP,
                      R5 = Data_or5_hus$BNP,
                      R6 = Data_or6_hus$BNP,
                      N = c(0:50),
                      inte = "base") %>% gather(variable, Change, -N, -inte)

irf_hus_hus <- tibble(R1 = Data_or1_hus$HUS,
                      R2 = Data_or2_hus$HUS,
                      R3 = Data_or3_hus$HUS,
                      R4 = Data_or4_hus$HUS,
                      R5 = Data_or5_hus$HUS,
                      R6 = Data_or6_hus$HUS,
                      N = c(0:50),
                      inte = "base") %>% gather(variable, Change, -N, -inte)

irf_hus_kredit <- tibble(R1 = Data_or1_hus$KREDIT,
                         R2 = Data_or2_hus$KREDIT,
                         R3 = Data_or3_hus$KREDIT,
                         R4 = Data_or4_hus$KREDIT,
                         R5 = Data_or5_hus$KREDIT,
                         R6 = Data_or6_hus$KREDIT,
                         N = c(0:50),
                         inte = "base") %>% gather(variable, Change, -N, -inte)

irf_kredit_bnp <- tibble(R1 = Data_or1_kredit$BNP,
                         R2 = Data_or2_kredit$BNP,
                         R3 = Data_or3_kredit$BNP,
                         R4 = Data_or4_kredit$BNP,
                         R5 = Data_or5_kredit$BNP,
                         R6 = Data_or6_kredit$BNP,
                         N = c(0:50),
                         inte = "base") %>% gather(variable, Change, -N, -inte)

irf_kredit_hus <- tibble(R1 = Data_or1_kredit$HUS,
                         R2 = Data_or2_kredit$HUS,
                         R3 = Data_or3_kredit$HUS,
                         R4 = Data_or4_kredit$HUS,
                         R5 = Data_or5_kredit$HUS,
                         R6 = Data_or6_kredit$HUS,
                         N = c(0:50),
                         inte = "base") %>% gather(variable, Change, -N, -inte)

irf_kredit_kredit <- tibble(R1 = Data_or1_kredit$KREDIT,
                            R2 = Data_or2_kredit$KREDIT,
                            R3 = Data_or3_kredit$KREDIT,
                            R4 = Data_or4_kredit$KREDIT,
                            R5 = Data_or5_kredit$KREDIT,
                            R6 = Data_or6_kredit$KREDIT,
                            N = c(0:50),
                            inte = "base") %>% gather(variable, Change, -N, -inte)

upper_bnp_bnp <- tibble(R1 = Upper_or1_bnp$BNP,
                      R2 = Upper_or2_bnp$BNP,
                      R3 = Upper_or3_bnp$BNP,
                      R4 = Upper_or4_bnp$BNP,
                      R5 = Upper_or5_bnp$BNP,
                      R6 = Upper_or6_bnp$BNP,
                      N = c(0:50),
                      inte = "upper") %>% gather(variable, Change, -N, -inte)

upper_bnp_hus <- tibble(R1 = Upper_or1_bnp$HUS,
                      R2 = Upper_or2_bnp$HUS,
                      R3 = Upper_or3_bnp$HUS,
                      R4 = Upper_or4_bnp$HUS,
                      R5 = Upper_or5_bnp$HUS,
                      R6 = Upper_or6_bnp$HUS,
                      N = c(0:50),
                      inte = "upper") %>% gather(variable, Change, -N, -inte)

upper_bnp_kredit <- tibble(R1 = Upper_or1_bnp$KREDIT,
                         R2 = Upper_or2_bnp$KREDIT,
                         R3 = Upper_or3_bnp$KREDIT,
                         R4 = Upper_or4_bnp$KREDIT,
                         R5 = Upper_or5_bnp$KREDIT,
                         R6 = Upper_or6_bnp$KREDIT,
                         N = c(0:50),
                         inte = "upper") %>% gather(variable, Change, -N, -inte)

upper_hus_bnp <- tibble(R1 = Upper_or1_hus$BNP,
                      R2 = Upper_or2_hus$BNP,
                      R3 = Upper_or3_hus$BNP,
                      R4 = Upper_or4_hus$BNP,
                      R5 = Upper_or5_hus$BNP,
                      R6 = Upper_or6_hus$BNP,
                      N = c(0:50),
                      inte = "upper") %>% gather(variable, Change, -N, -inte)

upper_hus_hus <- tibble(R1 = Upper_or1_hus$HUS,
                      R2 = Upper_or2_hus$HUS,
                      R3 = Upper_or3_hus$HUS,
                      R4 = Upper_or4_hus$HUS,
                      R5 = Upper_or5_hus$HUS,
                      R6 = Upper_or6_hus$HUS,
                      N = c(0:50),
                      inte = "upper") %>% gather(variable, Change, -N, -inte)

upper_hus_kredit <- tibble(R1 = Upper_or1_hus$KREDIT,
                         R2 = Upper_or2_hus$KREDIT,
                         R3 = Upper_or3_hus$KREDIT,
                         R4 = Upper_or4_hus$KREDIT,
                         R5 = Upper_or5_hus$KREDIT,
                         R6 = Upper_or6_hus$KREDIT,
                         N = c(0:50),
                         inte = "upper") %>% gather(variable, Change, -N, -inte)

upper_kredit_bnp <- tibble(R1 = Upper_or1_kredit$BNP,
                         R2 = Upper_or2_kredit$BNP,
                         R3 = Upper_or3_kredit$BNP,
                         R4 = Upper_or4_kredit$BNP,
                         R5 = Upper_or5_kredit$BNP,
                         R6 = Upper_or6_kredit$BNP,
                         N = c(0:50),
                         inte = "upper") %>% gather(variable, Change, -N, -inte)

upper_kredit_hus <- tibble(R1 = Upper_or1_kredit$HUS,
                         R2 = Upper_or2_kredit$HUS,
                         R3 = Upper_or3_kredit$HUS,
                         R4 = Upper_or4_kredit$HUS,
                         R5 = Upper_or5_kredit$HUS,
                         R6 = Upper_or6_kredit$HUS,
                         N = c(0:50),
                         inte = "upper") %>% gather(variable, Change, -N, -inte)

upper_kredit_kredit <- tibble(R1 = Upper_or1_kredit$KREDIT,
                            R2 = Upper_or2_kredit$KREDIT,
                            R3 = Upper_or3_kredit$KREDIT,
                            R4 = Upper_or4_kredit$KREDIT,
                            R5 = Upper_or5_kredit$KREDIT,
                            R6 = Upper_or6_kredit$KREDIT,
                            N = c(0:50),
                            inte = "upper") %>% gather(variable, Change, -N, -inte)

lower_bnp_bnp <- tibble(R1 = Lower_or1_bnp$BNP,
                      R2 = Lower_or2_bnp$BNP,
                      R3 = Lower_or3_bnp$BNP,
                      R4 = Lower_or4_bnp$BNP,
                      R5 = Lower_or5_bnp$BNP,
                      R6 = Lower_or6_bnp$BNP,
                      N = c(0:50),
                      inte = "lower") %>% gather(variable, Change, -N, -inte)

lower_bnp_hus <- tibble(R1 = Lower_or1_bnp$HUS,
                      R2 = Lower_or2_bnp$HUS,
                      R3 = Lower_or3_bnp$HUS,
                      R4 = Lower_or4_bnp$HUS,
                      R5 = Lower_or5_bnp$HUS,
                      R6 = Lower_or6_bnp$HUS,
                      N = c(0:50),
                      inte = "lower") %>% gather(variable, Change, -N, -inte)

lower_bnp_kredit <- tibble(R1 = Lower_or1_bnp$KREDIT,
                         R2 = Lower_or2_bnp$KREDIT,
                         R3 = Lower_or3_bnp$KREDIT,
                         R4 = Lower_or4_bnp$KREDIT,
                         R5 = Lower_or5_bnp$KREDIT,
                         R6 = Lower_or6_bnp$KREDIT,
                         N = c(0:50),
                         inte = "lower") %>% gather(variable, Change, -N, -inte)

lower_hus_bnp <- tibble(R1 = Lower_or1_hus$BNP,
                      R2 = Lower_or2_hus$BNP,
                      R3 = Lower_or3_hus$BNP,
                      R4 = Lower_or4_hus$BNP,
                      R5 = Lower_or5_hus$BNP,
                      R6 = Lower_or6_hus$BNP,
                      N = c(0:50),
                      inte = "lower") %>% gather(variable, Change, -N, -inte)

lower_hus_hus <- tibble(R1 = Lower_or1_hus$HUS,
                      R2 = Lower_or2_hus$HUS,
                      R3 = Lower_or3_hus$HUS,
                      R4 = Lower_or4_hus$HUS,
                      R5 = Lower_or5_hus$HUS,
                      R6 = Lower_or6_hus$HUS,
                      N = c(0:50),
                      inte = "lower") %>% gather(variable, Change, -N, -inte)

lower_hus_kredit <- tibble(R1 = Lower_or1_hus$KREDIT,
                         R2 = Lower_or2_hus$KREDIT,
                         R3 = Lower_or3_hus$KREDIT,
                         R4 = Lower_or4_hus$KREDIT,
                         R5 = Lower_or5_hus$KREDIT,
                         R6 = Lower_or6_hus$KREDIT,
                         N = c(0:50),
                         inte = "lower") %>% gather(variable, Change, -N, -inte)

lower_kredit_bnp <- tibble(R1 = Lower_or1_kredit$BNP,
                         R2 = Lower_or2_kredit$BNP,
                         R3 = Lower_or3_kredit$BNP,
                         R4 = Lower_or4_kredit$BNP,
                         R5 = Lower_or5_kredit$BNP,
                         R6 = Lower_or6_kredit$BNP,
                         N = c(0:50),
                         inte = "lower") %>% gather(variable, Change, -N, -inte)

lower_kredit_hus <- tibble(R1 = Lower_or1_kredit$HUS,
                         R2 = Lower_or2_kredit$HUS,
                         R3 = Lower_or3_kredit$HUS,
                         R4 = Lower_or4_kredit$HUS,
                         R5 = Lower_or5_kredit$HUS,
                         R6 = Lower_or6_kredit$HUS,
                         N = c(0:50),
                         inte = "lower") %>% gather(variable, Change, -N, -inte)

lower_kredit_kredit <- tibble(R1 = Lower_or1_kredit$KREDIT,
                            R2 = Lower_or2_kredit$KREDIT,
                            R3 = Lower_or3_kredit$KREDIT,
                            R4 = Lower_or4_kredit$KREDIT,
                            R5 = Lower_or5_kredit$KREDIT,
                            R6 = Lower_or6_kredit$KREDIT,
                            N = c(0:50),
                            inte = "lower") %>% gather(variable, Change, -N, -inte)
p23.1 <- rbind(irf_bnp_bnp, upper_bnp_bnp, lower_bnp_bnp) %>%
  group_by(variable, inte) %>%
  gather(type, value, -N, -variable, -inte) %>%
  ggplot(aes(N, value, color = variable, linetype = inte)) +
  geom_line(aes(y = 0), color = "black") +
  geom_line(size=0.3) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) +
  scale_color_manual(values = c("black", "black", "black", "black", "black", "black")) +
  scale_x_continuous(breaks = seq(0,50, by = 5), limits = c(0,50)) +
  labs(title = "Shock on GDP",y = "Respons on GDP", x ="Lags (Quaterly)",caption = "") +
  th + theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))
  

p24.1 <- rbind(irf_bnp_hus, upper_bnp_hus, lower_bnp_hus) %>%
  group_by(variable, inte) %>%
  gather(type, value, -N, -variable, -inte) %>%
  ggplot(aes(N, value, color = variable, linetype = inte)) +
  geom_line(aes(y = 0), color = "black") +
  geom_line(size=0.3) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) +
  scale_color_manual(values = c("black", "black", "black", "black", "black", "black")) +
  scale_x_continuous(breaks = seq(0,50, by = 5), limits = c(0,50)) +
  labs(title = "",y = "Respons on House", x ="Lags (Quaterly)",caption = "") +
  th + theme(legend.position = "none")

p25.1 <- rbind(irf_bnp_kredit, upper_bnp_kredit, lower_bnp_kredit) %>%
  group_by(variable, inte) %>%
  gather(type, value, -N, -variable, -inte) %>%
  ggplot(aes(N, value, color = variable, linetype = inte)) +
  geom_line(aes(y = 0), color = "black") +
  geom_line(size=0.3) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) +
  scale_color_manual(values = c("black", "black", "black", "black", "black", "black")) +
  scale_x_continuous(breaks = seq(0,50, by = 5), limits = c(0,50)) +
  labs(title = "",y = "Respons on Credit", x ="Lags (Quaterly)",caption = "") +
  th + theme(legend.position = "none")

p26.1 <- rbind(irf_hus_bnp, upper_hus_bnp, lower_hus_bnp) %>%
  group_by(variable, inte) %>%
  gather(type, value, -N, -variable, -inte) %>%
  ggplot(aes(N, value, color = variable, linetype = inte)) +
  geom_line(aes(y = 0), color = "black") +
  geom_line(size=0.3) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) +
  scale_color_manual(values = c("black", "black", "black", "black", "black", "black")) +
  scale_x_continuous(breaks = seq(0,50, by = 5), limits = c(0,50)) +
  labs(title = "Shock on House",y = "", x ="Lags (Quaterly)",caption = "") +
  th + theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))

p27.1 <- rbind(irf_hus_hus, upper_hus_hus, lower_hus_hus) %>%
  group_by(variable, inte) %>%
  gather(type, value, -N, -variable, -inte) %>%
  ggplot(aes(N, value, color = variable, linetype = inte)) +
  geom_line(aes(y = 0), color = "black") +
  geom_line(size=0.3) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) +
  scale_color_manual(values = c("black", "black", "black", "black", "black", "black")) +
  scale_x_continuous(breaks = seq(0,50, by = 5), limits = c(0,50)) +
  labs(title = "",y = "", x ="Lags (Quaterly)",caption = "") +
  th + theme(legend.position = "none")

p28.1 <- rbind(irf_hus_kredit, upper_hus_kredit, lower_hus_kredit) %>%
  group_by(variable, inte) %>%
  gather(type, value, -N, -variable, -inte) %>%
  ggplot(aes(N, value, color = variable, linetype = inte)) +
  geom_line(aes(y = 0), color = "black") +
  geom_line(size=0.3) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) +
  scale_color_manual(values = c("black", "black", "black", "black", "black", "black")) +
  scale_x_continuous(breaks = seq(0,50, by = 5), limits = c(0,50)) +
  labs(title = "",y = "", x ="Lags (Quaterly)",caption = "") +
  th + theme(legend.position = "none")

p29.1 <- rbind(irf_kredit_bnp, upper_kredit_bnp, lower_kredit_bnp) %>%
  group_by(variable, inte) %>%
  gather(type, value, -N, -variable, -inte) %>%
  ggplot(aes(N, value, color = variable, linetype = inte)) +
  geom_line(aes(y = 0), color = "black") +
  geom_line(size=0.3) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) +
  scale_color_manual(values = c("black", "black", "black", "black", "black", "black")) +
  scale_x_continuous(breaks = seq(0,50, by = 5), limits = c(0,50)) +
  labs(title = "Shock on Credit",y = "", x ="Lags (Quaterly)",caption = "") +
  th + theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))

p30.1 <- rbind(irf_kredit_hus, upper_kredit_hus, lower_kredit_hus) %>%
  group_by(variable, inte) %>%
  gather(type, value, -N, -variable, -inte) %>%
  ggplot(aes(N, value, color = variable, linetype = inte)) +
  geom_line(aes(y = 0), color = "black") +
  geom_line(size=0.3) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) +
  scale_color_manual(values = c("black", "black", "black", "black", "black", "black")) +
  scale_x_continuous(breaks = seq(0,50, by = 5), limits = c(0,50)) +
  labs(title = "",y = "", x ="Lags (Quaterly)",caption = "") +
  th + theme(legend.position = "none")

p31.1 <- rbind(irf_kredit_kredit, upper_kredit_kredit, lower_kredit_kredit) %>%
  group_by(variable, inte) %>%
  gather(type, value, -N, -variable, -inte) %>%
  ggplot(aes(N, value, color = variable, linetype = inte)) +
  geom_line(aes(y = 0), color = "black") +
  geom_line(size=0.3) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) +
  scale_color_manual(values = c("black", "black", "black", "black", "black", "black")) +
  scale_x_continuous(breaks = seq(0,50, by = 5), limits = c(0,50)) +
  labs(title = "",y = "", x ="Lags (Quaterly)",caption = "") +
  th + theme(legend.position = "none") 

#ggsave("Kontrol-IRF.pdf", plot = grid.arrange(p23.1, p26.1, p29.1, p24.1, p27.1, p30.1, p25.1, p28.1, p31.1, nrow =3, ncol=3), width = 30, height = 30, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")


# Forecast error variance decomposistion

fevdecom <- fevd(model1, n.ahead = 20)

fevdecomBNP <- as.data.frame(fevdecom$BNP)
fevdecomBNP$N <- c(1:20)
fevdecomBNP <- fevdecomBNP %>% gather(Variable, Value, -N)

fevdecomHUS <- as.data.frame(fevdecom$HUS)
fevdecomHUS$N <- c(1:20)
fevdecomHUS <- fevdecomHUS %>% gather(Variable, Value, -N)

fevdecomKREDIT <- as.data.frame(fevdecom$KREDIT)
fevdecomKREDIT$N <- c(1:20)
fevdecomKREDIT <- fevdecomKREDIT %>% gather(Variable, Value, -N)

fill <- c("#619CFF","#00BA38","#F8766D")
fill1 <- c("#F8766D", "#619CFF","#00BA38")
fill2 <- c("#F8766D","#00BA38","#619CFF")

p32 <- ggplot(fevdecomBNP) + geom_bar(aes(y = Value, x = N, fill = reorder(Variable, Value)), stat="identity", show.legend = F) +
  scale_x_continuous(breaks = seq(0,20, by = 1)) +
  labs(title = "GDP",y = "", x ="Quarters", caption = "") + scale_fill_manual(values = fill) + th +
  theme(plot.title = element_text(hjust = 0.5))

p33 <- ggplot(fevdecomHUS) + geom_bar(aes(y = Value, x = N, fill = reorder(Variable, Value)), stat="identity", show.legend = F) +
  scale_x_continuous(breaks = seq(0,20, by = 1)) +
  labs(title = "House",y = "", x ="Quarters", caption = "") + scale_fill_manual(values = fill1) + th +
  theme(plot.title = element_text(hjust = 0.5))

p34 <- ggplot(fevdecomKREDIT) + geom_bar(aes(y = Value, x = N, fill = reorder(Variable, Value)), stat="identity", show.legend = T) +
  scale_x_continuous(breaks = seq(0,20, by = 1)) +
  labs(title = "Credit",y = "", x ="Quarters", caption = "Source: Own calculation") + scale_fill_manual(values = fill2, labels = c("GDP", "House","Credit")) + th +
  theme(legend.position="bottom", legend.title = element_blank()) + theme(panel.grid.minor.x = element_blank()) + guides(fill = guide_legend(reverse = F)) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("fevd.pdf", plot = grid.arrange(p32, p33, p34, nrow =3, ncol=1), width = 30, height = 25, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")


#Andel realkredit 

Andel <- read_excel("andel-real.xlsx")
Andel$dato <- as.Date(Andel$dato)
Andel$type <- as.factor(Andel$type)
Andel$percent <- as.numeric(Andel$percent)
Andel_sort <- ddply(Andel, "dato", transform)
Andel_sort$type <- factor(Andel_sort$type, levels = levels(Andel_sort$type))

p19 <- ggplot(Andel_sort, aes(dato, percent)) + geom_area(aes(fill=type), alpha = 0.8) +
  scale_y_continuous(breaks = round(seq(min(0), max(200), by = 10),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) + 
  labs(title="Types of loans for mortgages",
       subtitle = "Fixed rate without installments and variable rate without installments was first introduced in 2003",
       y="Share of mortgages",
       x=NULL,
       caption = "Source: DST", fill=NULL) +
  scale_fill_manual(breaks=c("Fixed rate with installments", "Fixed rate without installments", "Variable rate with installments", "Variable rate without installments"),
                    labels=c("Fixed interest rate with installments", "Fixed interest rate without installments", "Variable interest rate with installments" ,"Variable interest rate without installments"),
                    values=c("Fixed rate with installments"="#9ecae1", "Fixed rate without installments"="#6baed6", "Variable rate with installments"="#08519c", "Variable rate without installments"="#08306b")) +
  th + theme(legend.position="bottom") + theme(legend.key=element_blank()) +
  theme(legend.title = element_text(size = 9),
  legend.text = element_text(size = 7))

#ggsave("Realkreditlån.pdf", p19, width = 25, height = 12, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")

#Andel real/banklån

loans <- read_excel("shareloan.xlsx")

loans$Tid <- as.Date(loans$Tid)
loans$Type <- as.factor(loans$Type)
loans$Values <- as.numeric(loans$Values)
loans_sort <- ddply(loans, "Tid", transform)
loans_sort$Type <- factor(loans_sort$Type, levels = levels(loans_sort$Type))

bank_share <- read_excel("bankshare.xlsx")
bank_share$Tid <- as.Date(bank_share$Tid)
bank_share$Percent <- as.numeric(bank_share$Percent)


p20 <- ggplot(loans_sort[153:558,], aes(Tid, Values/1000)) + geom_area(aes(fill=Type), alpha = 0.8) +
  geom_line(data = bank_share[77:279,], aes(x = Tid, y = Percent*1000*2.5, color = "Bank share of lending (Right Axis)")) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(0, 1600), breaks=round(seq(min(0), max(1600), by = 200),1),
                     sec.axis = sec_axis(~ ./25 , name = "Bank share of lending", breaks=round(seq(min(0), max(60), by = 10),1))) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("3 years")) +
  labs(title="Lending by domestic banks and mortgage-credit institutions",
       subtitle = "Seasonally adjusted and deflated by CPI",
       y="Bil. kr.",
       x=NULL,
       caption = "Source: (Abildgren, 2021)", fill=NULL, color=NULL) +
  scale_fill_manual(breaks=c("Bank", "Realkredit"),
                    labels=c("Banks", "Mortgage-credit institutions"),
                    values=c("Bank"="#6baed6", "Realkredit"="#08519c")) +
  scale_color_manual(values=c("#000000")) +
  th + theme(legend.position="bottom") + theme(legend.key=element_blank()) +
  theme(axis.title.y.right = element_text(margin = unit(c(0, 0, 0, 3), 'mm')))
  
#ggsave("Realkreditlån-banklån.pdf", p20, width = 25, height = 12, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")

#Udlånsgab

loan_gab <- read_xlsx("gab.xlsx", sheet = "Udlånsgab")

loan_gab$Dato <- as.Date(loan_gab$Dato)
loan_gab$`Udlånsgab - pp` <- as.numeric(loan_gab$`Udlånsgab - pp`)

p21 <- ggplot(loan_gab[41:202,], aes(x = Dato, y = `Udlånsgab - pp`)) + geom_col(fill="#08519c") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-35, 45), breaks=round(seq(min(-35), max(45), by = 5),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("3 years")) + 
  labs(title = "Lending gab", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="Devation from trend in %-point", caption = "Source: Det Systemiske Risikoråd") +
  th
  
#ggsave("Udlångab.pdf", p21, width = 30, height = 12, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")

#boligprisgab

house_gab <- read_xlsx("gab.xlsx", sheet = "Boligprisgab")
house_gab$Dato <- as.Date(house_gab$Dato)  
house_gab$`Boligprisgab - pp` <- as.numeric(house_gab$`Boligprisgab - pp`)

p22 <- ggplot(house_gab, aes(x = Dato, y = `Boligprisgab - pp`)) + geom_col(fill="#08519c") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-20, 30), breaks=round(seq(min(-35), max(45), by = 5),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("3 years")) + 
  labs(title = "Housing price gab", subtitle = "Seasonally adjusted and deflated by CPI", x=NULL, y="Devation from trend in %-point", caption = "Source: Det Systemiske Risikoråd") +
  th

#ggsave("boligprisgab.pdf", p22, width = 30, height = 12, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")

## Stød på vores SFC-model


setwd("~/Dropbox/Bachelorprojekt/Data")

sfc_shock2 <- read_excel("StødG1.xlsx")
sfc_shock2 <- rename(sfc_shock2, replace = c("Date" = "Dato"))  
sfc_shock2_ad <- read_excel("StødG1_ad.xlsx")

data_shock2 <- as.data.frame(sfc_shock2$Dato)
data_shock2$`sfc_shock2$Dato`<- as.Date(data_shock2$`sfc_shock2$Dato`)
data_shock2 <- rename(data_shock2, replace = c("sfc_shock2$Dato" = "Dato"))  
data_shock2$ibl_h <- sfc_shock2$ibl_h_for
data_shock2$inv_h <- sfc_shock2$invh_k_for
data_shock2$ur <- sfc_shock2$ur_for
data_shock2$ydh <- sfc_shock2$ydhk_for
data_shock2$y <- sfc_shock2$yk_for
data_shock2$zz_i <- sfc_shock2$zz_i_for
data_shock2$ibl_h_ad <- sfc_shock2_ad$ibl_h_for
data_shock2$invh_h_ad <- sfc_shock2_ad$invh_k_for
data_shock2$ur_ad <- sfc_shock2_ad$ur_for
data_shock2$ydh_ad <- sfc_shock2_ad$ydhk_for
data_shock2$y_ad <- sfc_shock2_ad$yk_for
data_shock2$zz_i_ad <- sfc_shock2_ad$zz_i_for

data_shock2 <- data_shock2[-c(1:7), ]
data_shock2 <- data_shock2[-c(8:15), ]

p41 <- ggplot(data_shock2, aes(x = Dato, y = ibl_h)) + geom_line(color = "#F8766D") + geom_line(aes(y = ibl_h_ad), color = "#619CFF") +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-4.6, 0), breaks=round(seq(min(-4.5), max(0), by = 0.5),2), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in household debt", subtitle ="Shock on government expenditure = Red line\nShock on government expenditure and behavior change = Blue line ",x=NULL, y=NULL, caption = "Source: Own calculation") +
  th + theme(legend.position="none")

p42 <- ggplot(data_shock2, aes(x = Dato, y = inv_h)) + geom_line(color = "#F8766D") + geom_line(aes(y = invh_h_ad), color = "#619CFF") +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-10.1, 0), breaks=round(seq(min(-10), max(0), by = 1),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in household real investment", subtitle ="Shock on government expenditure = Red line\nShock on government expenditure and behavior change = Blue line ", x=NULL, y=NULL, caption = "Source: Own calculation") +
  th + theme(legend.position="none")

p43 <- ggplot(data_shock2, aes(x = Dato, y = ur)) + geom_line(color = "#F8766D") + geom_line(aes(y = ur_ad), color = "#619CFF") +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(0, 1.2), breaks=round(seq(min(0), max(1.2), by = 0.2),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in unemployment rate", subtitle ="Shock on government expenditure = Red line\nShock on government expenditure and behavior change = Blue line ", x=NULL, y="%-point", caption = "Source: Own calculation") +
  th + theme(legend.position="none")

p44 <- ggplot(data_shock2, aes(x = Dato, y = ydh)) + geom_line(color = "#F8766D") + geom_line(aes(y = ydh_ad), color = "#619CFF") +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-0.71, 0), breaks=round(seq(min(-0.70), max(0), by = 0.1),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in household disposable income", subtitle ="Shock on government expenditure = Red line\nShock on government expenditure and behavior change = Blue line ", x=NULL, y=NULL, caption = "Source: Own calculation") +
  th + theme(legend.position="none")

p45 <- ggplot(data_shock2, aes(x = Dato, y = y)) + geom_line(color = "#F8766D") + geom_line(aes(y = y_ad), color = "#619CFF") +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-2, 0), breaks=round(seq(min(-2), max(0), by = 0.25),2), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in real output", subtitle ="Shock on government expenditure = Red line\nShock on government expenditure and behavior change = Blue line ", x=NULL, y=NULL, caption = "Source: Own calculation") +
  th + theme(legend.position="none")

p46 <- ggplot(data_shock2, aes(x = Dato, y = zz_i)) + geom_line(color = "#F8766D") + geom_line(aes(y = zz_i_ad), color = "#619CFF") +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-0.3, 0.05), breaks=round(seq(min(-0.3), max(0.05), by = 0.05),3), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in House Price Index", subtitle ="Shock on government expenditure = Red line\nShock on government expenditure and behavior change = Blue line ", x=NULL, y="%-point", caption = "Source: Own calculation") +
  th + theme(legend.position="none")

#ggsave("gæld_stødG.pdf", p41, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("investeringer_stødG.pdf", p42, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("arbejdsløshed_stødG.pdf", p43, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("disp_indkomst_stødG.pdf", p44, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("output_stødG.pdf", p45, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("boligprisindeks_stødG.pdf", p46, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")

nl <- sfc_shock2 %>% 
  dplyr::select(Dato, nlf_0, nlf_1, nlg_0,nlg_1,nlh_0,nlh_1,nlnf_0,nlnf_1,nlrow_0,nlrow_1,yk_0,yk_1) %>%
  slice(8:14)

nl$Dato <- as.Date(nl$Dato)

nl$nlf <- ((nl$nlf_1/nl$yk_1)*100) - ((nl$nlf_0/nl$yk_0)*100)
nl$nlg <- ((nl$nlg_1/nl$yk_1)*100) - ((nl$nlg_0/nl$yk_0)*100)
nl$nlh <- ((nl$nlh_1/nl$yk_1)*100) - ((nl$nlh_0/nl$yk_0)*100)
nl$nlnf <- ((nl$nlnf_1/nl$yk_1)*100) - ((nl$nlnf_0/nl$yk_0)*100)
nl$nlrow <- ((nl$nlrow_1/nl$yk_1)*100) - ((nl$nlrow_0/nl$yk_0)*100)

colors4 <- c("Government" = "#ff7f00", "Households" = "#377eb8", "Banks (FC)" = "#e41a1c", "Firms (NFC)" = "#4daf4a", "RoW"="#984ea3")

p54 <- ggplot(nl, aes(x = Dato, y = nlf, color = "Banks (FC)")) + geom_line() + geom_line(aes(y = nlg, color = "Government")) +
  geom_line(aes(y = nlh, color = "Households")) + geom_line(aes(y = nlnf, color = "Firms (NFC)")) + geom_line(aes(y = nlrow, color = "RoW")) +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-0.7, 0.9), breaks=round(seq(min(-0.8), max(1), by = 0.2),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in net lending",subtitle = "Reduce in government expenditure", x=NULL, y=NULL, caption = "Source: Own calculation", color = NULL) +
  th + theme(legend.position="bottom") + theme(legend.key=element_blank())

nl_ad <- sfc_shock2_ad %>% 
  dplyr::select(Date, nlf_0, nlf_1, nlg_0,nlg_1,nlh_0,nlh_1,nlnf_0,nlnf_1,nlrow_0,nlrow_1,yk_0,yk_1) %>%
  slice(8:14)

nl_ad$Date <- as.Date(nl_ad$Date)

nl_ad$nlf <- ((nl_ad$nlf_1/nl_ad$yk_1)*100) - ((nl_ad$nlf_0/nl_ad$yk_0)*100)
nl_ad$nlg <- ((nl_ad$nlg_1/nl_ad$yk_1)*100) - ((nl_ad$nlg_0/nl_ad$yk_0)*100)
nl_ad$nlh <- ((nl_ad$nlh_1/nl_ad$yk_1)*100) - ((nl_ad$nlh_0/nl_ad$yk_0)*100)
nl_ad$nlnf <- ((nl_ad$nlnf_1/nl_ad$yk_1)*100) - ((nl_ad$nlnf_0/nl_ad$yk_0)*100)
nl_ad$nlrow <- ((nl_ad$nlrow_1/nl_ad$yk_1)*100) - ((nl_ad$nlrow_0/nl_ad$yk_0)*100)

p55 <- ggplot(nl_ad, aes(x = Date, y = nlf, color = "Banks (FC)")) + geom_line() + geom_line(aes(y = nlg, color = "Government")) +
  geom_line(aes(y = nlh, color = "Households")) + geom_line(aes(y = nlnf, color = "Firms (NFC)")) + geom_line(aes(y = nlrow, color = "RoW")) +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-1.7, 1.2), breaks=round(seq(min(-1.6), max(1.2), by = 0.4),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in net lending",subtitle = "Reduce in government expenditure and behavior change", x=NULL, y=NULL, caption = "Source: Own calculation", color = NULL) +
  th + theme(legend.position="bottom") + theme(legend.key=element_blank())

#ggsave("nlG.pdf", p54, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("nlG_ad.pdf", p55, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")

fin <- sfc_shock2 %>%
  dplyr::select(Dato, ibl_h_0, ibl_h_1, iba_h_0, iba_h_1, eqa_h_0, eqa_h_1, pena_h_0, pena_h_1, fnw_h_0, fnw_h_1) %>%
  slice(8:14)

fin$Dato <- as.Date(fin$Dato)

fin$ibah <- (fin$iba_h_1/fin$iba_h_0)*100-100
fin$iblh <- (fin$ibl_h_1/fin$ibl_h_0)*100-100
fin$eqah <- (fin$eqa_h_1/fin$eqa_h_0)*100-100
fin$penah <- (fin$pena_h_1/fin$pena_h_0)*100-100
fin$fnwh <- (fin$fnw_h_1/fin$fnw_h_0)*100-100

colors5 <- c("Pensions" = "#ff7f00", "I.-b. assets" = "#377eb8", "I.-b. liabilities" = "#e41a1c", "Equities" = "#4daf4a", "Net fin. assets" = "#984ea3")

p56 <- ggplot(fin, aes(x = Dato, y = iblh, color = "I.-b. liabilities")) + geom_line() +
  geom_line(aes(y = penah, color = "Pensions")) + geom_line(aes(y = ibah, color = "I.-b. assets")) +
  geom_line(aes(y = eqah, color = "Equities")) + geom_line(aes(y = fnwh, color = "Net fin. assets")) + geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-0.8, 0), breaks=round(seq(min(-0.8), max(0.0), by = 0.1),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in household financial balances",subtitle = "Reduce in government expenditure", x=NULL, y=NULL, caption = "Source: Own calculation\nI.-b. = Interest-bearing", color = NULL) +
  th + theme(legend.position="bottom") + theme(legend.key=element_blank())

fin_ad <- sfc_shock2_ad %>%
  dplyr::select(Date, ibl_h_0, ibl_h_1, iba_h_0, iba_h_1, eqa_h_0, eqa_h_1, pena_h_0, pena_h_1, fnw_h_0, fnw_h_1) %>%
  slice(8:14)

fin_ad$Date <- as.Date(fin_ad$Date)

fin_ad$ibah <- (fin_ad$iba_h_1/fin_ad$iba_h_0)*100-100
fin_ad$iblh <- (fin_ad$ibl_h_1/fin_ad$ibl_h_0)*100-100
fin_ad$eqah <- (fin_ad$eqa_h_1/fin_ad$eqa_h_0)*100-100
fin_ad$penah <- (fin_ad$pena_h_1/fin_ad$pena_h_0)*100-100
fin_ad$fnwh <- (fin_ad$fnw_h_1/fin_ad$fnw_h_0)*100-100

p57 <- ggplot(fin_ad, aes(x = Date, y = iblh, color = "I.-b. liabilities")) + geom_line() +
  geom_line(aes(y = penah, color = "Pensions")) + geom_line(aes(y = ibah, color = "I.-b. assets")) +
  geom_line(aes(y = eqah, color = "Equities")) + geom_line(aes(y = fnwh, color = "Net fin. assets")) + geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-4.56, 4.3), breaks=round(seq(min(-5), max(5), by = 1),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in household financial balances",subtitle = "Reduce in government expenditure and behavior change", x=NULL, y=NULL, caption = "Source: Own calculation\nI.-b. = Interest-bearing", color = NULL) +
  th + theme(legend.position="bottom") + theme(legend.key=element_blank())

y_ch <- sfc_shock2 %>% 
  dplyr::select(Dato, nxk_0, nxk_1, chk_0,chk_1,ik_0,ik_1,yk_0,yk_1, gk_0, gk_1) %>%
  slice(8:14)

y_ch$Dato <- as.Date(y_ch$Dato)

y_ch$nx <- (y_ch$nxk_1-y_ch$nxk_0)/(y_ch$yk_1-y_ch$yk_0)
y_ch$inv <- (y_ch$ik_1-y_ch$ik_0)/(y_ch$yk_1-y_ch$yk_0)
y_ch$chk <- (y_ch$chk_1-y_ch$chk_0)/(y_ch$yk_1-y_ch$yk_0)
y_ch$g <- (y_ch$gk_1-y_ch$gk_0)/(y_ch$yk_1-y_ch$yk_0)

y_ch_ad <- sfc_shock2_ad %>% 
  dplyr::select(Date, nxk_0, nxk_1, chk_0,chk_1,ik_0,ik_1,yk_0,yk_1, gk_0, gk_1) %>%
  slice(8:14)

y_ch_ad$Date <- as.Date(y_ch_ad$Date)

y_ch_ad$nx <- (y_ch_ad$nxk_1-y_ch_ad$nxk_0)/(y_ch_ad$yk_1-y_ch_ad$yk_0)
y_ch_ad$inv <- (y_ch_ad$ik_1-y_ch_ad$ik_0)/(y_ch_ad$yk_1-y_ch_ad$yk_0)
y_ch_ad$chk <- (y_ch_ad$chk_1-y_ch_ad$chk_0)/(y_ch_ad$yk_1-y_ch_ad$yk_0)
y_ch_ad$g <- (y_ch_ad$gk_1-y_ch_ad$gk_0)/(y_ch_ad$yk_1-y_ch_ad$yk_0)


#ggsave("balanceG.pdf", p56, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("balanceG_ad.pdf", p57, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")

# Udvikling i G
sfc_g <- read_excel("StødG1.xlsx")
sfc_g <- rename(sfc_g, replace = c("Date" = "Dato"))  

data_g <- as.data.frame(sfc_g$Dato)
data_g$`sfc_g$Dato`<- as.Date(data_g$`sfc_g$Dato`)
data_g <- rename(data_g, replace = c("sfc_g$Dato" = "Dato"))  
data_g$g_0 <- sfc_g$g_0
data_g$g_1 <- sfc_g$g_1

data_g <- data_g[-c(1:3), ]

p47 <- ggplot(data_g, aes(x = Dato, y = g_1, color = "After shock")) + geom_line() +
  geom_line(aes(y = g_0, color = "Before shock"), linetype = "dashed") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", decimal.mark = ".") ,limits = c(280000, 530000), breaks=round(seq(min(280000), max(530000), by = 50000),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in government expenditure", subtitle = "Before and after a 5% reduction on government expenditure in 2004", x=NULL, y="mil. kr", caption = "Source: Own calculation") +
  th + scale_color_manual(name = "", values=c("#F8766D", "black"), labels = c("After shock", "Before shock")) + theme(legend.position="bottom") +
  theme(legend.key=element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = c("dashed", "solid")), reverse = T))


#ggsave("g_udvikling.pdf", plot = p47, width = 25, height = 12, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")

# Model perfomance

model_perform <- read_excel("modelperform1.xlsx")
model_perform <- rename(model_perform, replace = c("Date" = "Dato"))

data_perfom <- as.data.frame(model_perform$Dato)
data_perfom$`model_perform$Dato` <- as.Date(data_perfom$`model_perform$Dato`)
data_perfom <- rename(data_perfom, replace = c("model_perform$Dato" = "Dato"))
data_perfom$inv <- model_perform$invh_k
data_perfom$inv_0 <- model_perform$invh_k_0
data_perfom$ibl <- model_perform$ibl_h
data_perfom$ibl_0 <- model_perform$ibl_h_0
data_perfom$ur <- model_perform$ur
data_perfom$ur_0 <- model_perform$ur_0
data_perfom$ydh <- model_perform$ydhk
data_perfom$ydh_0 <- model_perform$ydhk_0
data_perfom$yk <- model_perform$yk
data_perfom$yk_0 <- model_perform$yk_0
data_perfom$zz_i <- model_perform$zz_i
data_perfom$zz_i_0 <- model_perform$zz_i_0


#Plot model performance

p48 <- ggplot(data_perfom, aes(x = Dato, y = inv_0, color = "Model (Baseline)")) + geom_line() +
  geom_line(aes(y = inv, color = "Actual values"), linetype = "dashed") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", decimal.mark = ".") ,limits = c(50000, 140000), breaks=round(seq(min(50000), max(140000), by = 10000),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years")) +
  labs(title = "Development in household real investment", subtitle = NULL, x=NULL, y="mil. kr", caption = "Source: Own calculation") +
  th + scale_color_manual(name = "", values=c("black","#F8766D"), labels = c("Actual values", "Model (Baseline)")) + theme(legend.position="bottom") + theme(legend.key=element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = c("dashed", "solid"))))
  
p49 <- ggplot(data_perfom, aes(x = Dato, y = ibl_0, color = "Model (Baseline)")) + geom_line() +
  geom_line(aes(y = ibl, color = "Actual values"), linetype = "dashed") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", decimal.mark = ".") ,limits = c(900000, 3000000), breaks=round(seq(min(900000), max(3000000), by = 300000),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years")) +
  labs(title = "Development in household debt", subtitle = NULL, x=NULL, y="mil. kr", caption = "Source: Own calculation") +
  th + scale_color_manual(name = "", values=c("black", "#F8766D"), labels = c("Actual values", "Model (Baseline)")) + theme(legend.position="bottom") + theme(legend.key=element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = c("dashed", "solid"))))

p50 <- ggplot(data_perfom, aes(x = Dato, y = ur_0, color = "Model (Baseline)")) + geom_line() +
  geom_line(aes(y = ur, color = "Actual Value"), linetype = "dashed") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", decimal.mark = ".") ,limits = c(0.01, 0.1), breaks=round(seq(min(0.01), max(0.1), by = 0.010),2)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years")) +
  labs(title = "Development in unemployment rate", subtitle = NULL, x=NULL, y="%", caption = "Source: Own calculation") +
  th + scale_color_manual(name = "", values=c("black", "#F8766D"), labels = c("Actual values", "Model (Baseline)")) + theme(legend.position="bottom") + theme(legend.key=element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = c("dashed", "solid"))))
  
p51 <- ggplot(data_perfom, aes(x = Dato, y = ydh_0, color = "Model (Baseline)")) + geom_line() +
  geom_line(aes(y = ydh, color = "Actual values"), linetype = "dashed") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", decimal.mark = ".") ,limits = c(700000, 1000000), breaks=round(seq(min(700000), max(1000000), by = 50000),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years")) +
  labs(title = "Development in household disposable income", subtitle = NULL, x=NULL, y="mil. kr", caption = "Source: Own calculation") +
  th + scale_color_manual(name = "", values=c("black", "#F8766D"), labels = c("Actual values", "Model (Baseline)")) + theme(legend.position="bottom") + theme(legend.key=element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = c("dashed", "solid"))))

p52 <- ggplot(data_perfom, aes(x = Dato, y = yk_0, color = "Model (Baseline)")) + geom_line() +
  geom_line(aes(y = yk, color = "Actual values"), linetype = "dashed") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", decimal.mark = ".") ,limits = c(1390000, 2000000), breaks=round(seq(min(1400000), max(2000000), by = 100000),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years")) +
  labs(title = "Development in real output", subtitle = NULL, x=NULL, y="mil. kr", caption = "Source: Own calculation") +
  th + scale_color_manual(name = "", values=c("black", "#F8766D"), labels = c("Actual values", "Model (Baseline)")) + theme(legend.position="bottom") + theme(legend.key=element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = c("dashed", "solid"))))

p53 <- ggplot(data_perfom, aes(x = Dato, y = zz_i_0, color = "Model (Baseline)")) + geom_line() +
  geom_line(aes(y = zz_i, color = "Actual values"), linetype = "dashed") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", decimal.mark = ".") ,limits = c(0.02, 0.36), breaks=round(seq(min(0.02), max(0.36), by = 0.04),2)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years")) +
  labs(title = "Development in House Price Index", subtitle = NULL, x=NULL, y="%", caption = "Source: Own calculation") +
  th + scale_color_manual(name = "", values=c("black", "#F8766D"), labels = c("Actual values", "Model (Baseline)")) + theme(legend.position="bottom") + theme(legend.key=element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = c("dashed", "solid"))))
  
#ggsave("gæld_per.pdf", p49, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("investeringer_per.pdf", p48, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("arbejdsløshed_per.pdf", p50, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("disp_indkomst_per.pdf", p51, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("output_per.pdf", p52, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("boligprisindeks_per.pdf", p53, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")

debt <- read.csv2("Stødres1.csv")

debt_ratio <- debt %>%
  dplyr::select(Date,lev_h)

debt_ratio$Date <- as.Date(debt_ratio$Date)

p58 <- ggplot(debt_ratio, aes(x = Date, y = lev_h)) + geom_line() +
  scale_y_continuous(limits = c(1.7, 3.2), breaks=round(seq(min(1.7), max(3.2), by = 0.1),1)) +
  scale_x_continuous(limits = c(1995,2016), breaks=round(seq(min(1995), max(2016), by = 2),1)) +
  annotate("rect", xmin=2003, xmax=2008, ymin=-Inf, ymax=Inf, alpha=0.2) +
  labs(title = "Household debt-to-income ratio",subtitle = "The grey area represents where restriction is activated", x=NULL, y="Debt-to-income ratio", caption = "Source: Own calculation", color = NULL) +
  th + theme(legend.position="bottom") + theme(legend.key=element_blank())

#ggsave("Debt-to-income.pdf", plot = p58, width = 25, height = 12, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")

sfc_shock3 <- read_excel("Stødres1.xlsx")
sfc_shock3 <- rename(sfc_shock3, replace = c("Date" = "Dato")) 

sfc_shock3_ad <- read_excel("Stødres_ad.xlsx")

data_shock3 <- as.data.frame(sfc_shock3$Dato)
data_shock3$`sfc_shock3$Dato`<- as.Date(data_shock3$`sfc_shock3$Dato`)
data_shock3 <- rename(data_shock3, replace = c("sfc_shock3$Dato" = "Dato")) 

data_shock3$ibl_h <- sfc_shock3$ibl_h_for
data_shock3$inv_h <- sfc_shock3$invh_k_for
data_shock3$ur <- sfc_shock3$ur_for
data_shock3$ydh <- sfc_shock3$ydhk_for
data_shock3$y <- sfc_shock3$yk_for
data_shock3$zz_i <- sfc_shock3$zz_i_for
data_shock3$ibl_h_ad <- sfc_shock3_ad$ibl_h_for
data_shock3$invh_h_ad <- sfc_shock3_ad$invh_k_for
data_shock3$ur_ad <- sfc_shock3_ad$ur_for
data_shock3$ydh_ad <- sfc_shock3_ad$ydhk_for
data_shock3$y_ad <- sfc_shock3_ad$yk_for
data_shock3$zz_i_ad <- sfc_shock3_ad$zz_i_for

data_shock3 <- data_shock3[-c(1:7), ]
data_shock3 <- data_shock3[-c(8:15), ]

p59 <- ggplot(data_shock3, aes(x = Dato, y = ibl_h)) + geom_line(color = "#F8766D") + geom_line(aes(y = ibl_h_ad), color = "#619CFF") +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-15, 0), breaks=round(seq(min(-16), max(0), by = 2),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in household debt", subtitle ="Restriction on credit = Red line\nRestriction on credit and behavior change = Blue line",x=NULL, y=NULL, caption = "Source: Own calculation") +
  th + theme(legend.position="none")

p60 <- ggplot(data_shock3, aes(x = Dato, y = inv_h)) + geom_line(color = "#F8766D") + geom_line(aes(y = invh_h_ad), color = "#619CFF") +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-36, 0), breaks=round(seq(min(-35), max(0), by = 5),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in household real investment", subtitle ="Restriction on credit = Red line\nRestriction on credit and behavior change = Blue line", x=NULL, y=NULL, caption = "Source: Own calculation") +
  th + theme(legend.position="none")

p61 <- ggplot(data_shock3, aes(x = Dato, y = ur)) + geom_line(color = "#F8766D") + geom_line(aes(y = ur_ad), color = "#619CFF") +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(0, 0.7), breaks=round(seq(min(0), max(0.7), by = 0.1),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in unemployment rate", subtitle ="Restriction on credit = Red line\nRestriction on credit and behavior change = Blue line", x=NULL, y="%-point", caption = "Source: Own calculation") +
  th + theme(legend.position="none")

p62 <- ggplot(data_shock3, aes(x = Dato, y = ydh)) + geom_line(color = "#F8766D") + geom_line(aes(y = ydh_ad), color = "#619CFF") +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-0.1, 0.4), breaks=round(seq(min(-0.1), max(0.4), by = 0.1),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in household disposable income", subtitle ="Restriction on credit = Red line\nRestriction on credit and behavior change = Blue line", x=NULL, y=NULL, caption = "Source: Own calculation") +
  th + theme(legend.position="none")

p63 <- ggplot(data_shock3, aes(x = Dato, y = y)) + geom_line(color = "#F8766D") + geom_line(aes(y = y_ad), color = "#619CFF") +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-1.1, 0), breaks=round(seq(min(-1.1), max(0), by = 0.1),2), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in real output", subtitle ="Restriction on credit = Red line\nRestriction on credit and behavior change = Blue line", x=NULL, y=NULL, caption = "Source: Own calculation") +
  th + theme(legend.position="none")

p64 <- ggplot(data_shock3, aes(x = Dato, y = zz_i)) + geom_line(color = "#F8766D") + geom_line(aes(y = zz_i_ad), color = "#619CFF") +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(0, 1), breaks=round(seq(min(0), max(1), by = 0.1),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in House Price Index", subtitle ="Restriction on credit = Red line\nRestriction on credit and behavior change = Blue line", x=NULL, y="%-point", caption = "Source: Own calculation") +
  th + theme(legend.position="none")

#ggsave("gæld_stødres.pdf", p59, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("investeringer_stødres.pdf", p60, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("arbejdsløshed_stødres.pdf", p61, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("disp_indkomst_stødres.pdf", p62, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("output_stødres.pdf", p63, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("boligprisindeks_stødres.pdf", p64, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")

nl_2 <- sfc_shock3 %>% 
  dplyr::select(Dato, nlf_0, nlf_1, nlg_0,nlg_1,nlh_0,nlh_1,nlnf_0,nlnf_1,nlrow_0,nlrow_1,yk_0,yk_1) %>%
  slice(8:14)

nl_2$Dato <- as.Date(nl_2$Dato)

nl_2$nlf <- ((nl_2$nlf_1/nl_2$yk_1)*100) - ((nl_2$nlf_0/nl_2$yk_0)*100)
nl_2$nlg <- ((nl_2$nlg_1/nl_2$yk_1)*100) - ((nl_2$nlg_0/nl_2$yk_0)*100)
nl_2$nlh <- ((nl_2$nlh_1/nl_2$yk_1)*100) - ((nl_2$nlh_0/nl_2$yk_0)*100)
nl_2$nlnf <- ((nl_2$nlnf_1/nl_2$yk_1)*100) - ((nl_2$nlnf_0/nl_2$yk_0)*100)
nl_2$nlrow <- ((nl_2$nlrow_1/nl_2$yk_1)*100) - ((nl_2$nlrow_0/nl_2$yk_0)*100)

colors4 <- c("Government" = "#ff7f00", "Households" = "#377eb8", "Banks (FC)" = "#e41a1c", "Firms (NFC)" = "#4daf4a", "RoW"="#984ea3")

p65 <- ggplot(nl_2, aes(x = Dato, y = nlf, color = "Banks (FC)")) + geom_line() + geom_line(aes(y = nlg, color = "Government")) +
  geom_line(aes(y = nlh, color = "Households")) + geom_line(aes(y = nlnf, color = "Firms (NFC)")) + geom_line(aes(y = nlrow, color = "RoW")) +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-2, 2.5), breaks=round(seq(min(-2), max(2.5), by = 0.5),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in net lending",subtitle = "Restriction on credit", x=NULL, y=NULL, caption = "Source: Own calculation", color = NULL) +
  th + theme(legend.position="bottom") + theme(legend.key=element_blank())

nl_2_ad <- sfc_shock3_ad %>% 
  dplyr::select(Date, nlf_0, nlf_1, nlg_0,nlg_1,nlh_0,nlh_1,nlnf_0,nlnf_1,nlrow_0,nlrow_1,yk_0,yk_1) %>%
  slice(8:14)

nl_2_ad$Date <- as.Date(nl_2_ad$Date)

nl_2_ad$nlf <- ((nl_2_ad$nlf_1/nl_2_ad$yk_1)*100) - ((nl_2_ad$nlf_0/nl_2_ad$yk_0)*100)
nl_2_ad$nlg <- ((nl_2_ad$nlg_1/nl_2_ad$yk_1)*100) - ((nl_2_ad$nlg_0/nl_2_ad$yk_0)*100)
nl_2_ad$nlh <- ((nl_2_ad$nlh_1/nl_2_ad$yk_1)*100) - ((nl_2_ad$nlh_0/nl_2_ad$yk_0)*100)
nl_2_ad$nlnf <- ((nl_2_ad$nlnf_1/nl_2_ad$yk_1)*100) - ((nl_2_ad$nlnf_0/nl_2_ad$yk_0)*100)
nl_2_ad$nlrow <- ((nl_2_ad$nlrow_1/nl_2_ad$yk_1)*100) - ((nl_2_ad$nlrow_0/nl_2_ad$yk_0)*100)

p66 <- ggplot(nl_2_ad, aes(x = Date, y = nlf, color = "Banks (FC)")) + geom_line() + geom_line(aes(y = nlg, color = "Government")) +
  geom_line(aes(y = nlh, color = "Households")) + geom_line(aes(y = nlnf, color = "Firms (NFC)")) + geom_line(aes(y = nlrow, color = "RoW")) +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-2.5, 3.01), breaks=round(seq(min(-2.5), max(3), by = 0.5),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in net lending",subtitle = "Restriction on credit and behavior change", x=NULL, y=NULL, caption = "Source: Own calculation", color = NULL) +
  th + theme(legend.position="bottom") + theme(legend.key=element_blank())

#ggsave("nlres.pdf", p65, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("nlres_ad.pdf", p66, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")

fin_2 <- sfc_shock3 %>%
  dplyr::select(Dato, ibl_h_0, ibl_h_1, iba_h_0, iba_h_1, eqa_h_0, eqa_h_1, pena_h_0, pena_h_1, fnw_h_0, fnw_h_1) %>%
  slice(8:14)

fin_2$Dato <- as.Date(fin_2$Dato)

fin_2$ibah <- (fin_2$iba_h_1/fin_2$iba_h_0)*100-100
fin_2$iblh <- (fin_2$ibl_h_1/fin_2$ibl_h_0)*100-100
fin_2$eqah <- (fin_2$eqa_h_1/fin_2$eqa_h_0)*100-100
fin_2$penah <- (fin_2$pena_h_1/fin_2$pena_h_0)*100-100
fin_2$fnwh <- (fin_2$fnw_h_1/fin_2$fnw_h_0)*100-100

colors5 <- c("Pensions" = "#ff7f00", "I.-b. assets" = "#377eb8", "I.-b. liabilities" = "#e41a1c", "Equities" = "#4daf4a", "Net fin. assets" = "#984ea3")

p67 <- ggplot(fin_2, aes(x = Dato, y = iblh, color = "I.-b. liabilities")) + geom_line() +
  geom_line(aes(y = penah, color = "Pensions")) + geom_line(aes(y = ibah, color = "I.-b. assets")) +
  geom_line(aes(y = eqah, color = "Equities")) + geom_line(aes(y = fnwh, color = "Net fin. assets")) + geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-14, 8), breaks=round(seq(min(-14), max(8), by = 2),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in household financial balances",subtitle = "Restriction on credit", x=NULL, y=NULL, caption = "Source: Own calculation\nI.-b. = Interest-bearing", color = NULL) +
  th + theme(legend.position="bottom") + theme(legend.key=element_blank())

fin_2_ad <- sfc_shock3_ad %>%
  dplyr::select(Date, ibl_h_0, ibl_h_1, iba_h_0, iba_h_1, eqa_h_0, eqa_h_1, pena_h_0, pena_h_1, fnw_h_0, fnw_h_1) %>%
  slice(8:14)

fin_2_ad$Date <- as.Date(fin_2_ad$Date)

fin_2_ad$ibah <- (fin_2_ad$iba_h_1/fin_2_ad$iba_h_0)*100-100
fin_2_ad$iblh <- (fin_2_ad$ibl_h_1/fin_2_ad$ibl_h_0)*100-100
fin_2_ad$eqah <- (fin_2_ad$eqa_h_1/fin_2_ad$eqa_h_0)*100-100
fin_2_ad$penah <- (fin_2_ad$pena_h_1/fin_2_ad$pena_h_0)*100-100
fin_2_ad$fnwh <- (fin_2_ad$fnw_h_1/fin_2_ad$fnw_h_0)*100-100

p68 <- ggplot(fin_2_ad, aes(x = Date, y = iblh, color = "I.-b. liabilities")) + geom_line() +
  geom_line(aes(y = penah, color = "Pensions")) + geom_line(aes(y = ibah, color = "I.-b. assets")) +
  geom_line(aes(y = eqah, color = "Equities")) + geom_line(aes(y = fnwh, color = "Net fin. assets")) + geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-15, 12), breaks=round(seq(min(-16), max(12), by = 2),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in household financial balances",subtitle = "Restriction on credit and behavior change", x=NULL, y=NULL, caption = "Source: Own calculation\nI.-b. = Interest-bearing", color = NULL) +
  th + theme(legend.position="bottom") + theme(legend.key=element_blank())

#ggsave("balanceres.pdf", p67, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("balanceres_ad.pdf", p68, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")

y_ch_2 <- sfc_shock3 %>% 
  dplyr::select(Dato, nxk_0, nxk_1, chk_0,chk_1,ik_0,ik_1,yk_0,yk_1, gk_0, gk_1) %>%
  slice(8:14)

y_ch_2$Dato <- as.Date(y_ch_2$Dato)

y_ch_2$nx <- (y_ch_2$nxk_1-y_ch_2$nxk_0)/(y_ch_2$yk_1-y_ch_2$yk_0)
y_ch_2$inv <- (y_ch_2$ik_1-y_ch_2$ik_0)/(y_ch_2$yk_1-y_ch_2$yk_0)
y_ch_2$chk <- (y_ch_2$chk_1-y_ch_2$chk_0)/(y_ch_2$yk_1-y_ch_2$yk_0)
y_ch_2$g <- (y_ch_2$gk_1-y_ch_2$gk_0)/(y_ch_2$yk_1-y_ch_2$yk_0)

y_ch_2_ad <- sfc_shock3_ad %>% 
  dplyr::select(Date, nxk_0, nxk_1, chk_0,chk_1,ik_0,ik_1,yk_0,yk_1, gk_0, gk_1) %>%
  slice(8:14)

y_ch_2_ad$Date <- as.Date(y_ch_2_ad$Date)

y_ch_2_ad$nx <- (y_ch_2_ad$nxk_1-y_ch_2_ad$nxk_0)/(y_ch_2_ad$yk_1-y_ch_2_ad$yk_0)
y_ch_2_ad$inv <- (y_ch_2_ad$ik_1-y_ch_2_ad$ik_0)/(y_ch_2_ad$yk_1-y_ch_2_ad$yk_0)
y_ch_2_ad$chk <- (y_ch_2_ad$chk_1-y_ch_2_ad$chk_0)/(y_ch_2_ad$yk_1-y_ch_2_ad$yk_0)
y_ch_2_ad$g <- (y_ch_2_ad$gk_1-y_ch_2_ad$gk_0)/(y_ch_2_ad$yk_1-y_ch_2_ad$yk_0)

#Debtled or growthled?
dg <- sfc_shock3 %>%
  dplyr::select(Dato, nxk, nxk_0, nxk_1, chk, chk_0,chk_1, ik, ik_0,ik_1,yk, yk_0,yk_1, gk, gk_0, gk_1) %>%
  slice(8:14)

dg$Dato <- as.Date(dg$Dato)

dg$chk_y <- (dg$chk/dg$yk)*100
dg$ik_y <- (dg$ik/dg$yk)*100
dg$gk_y <- (dg$gk/dg$yk)*100
dg$nxk_y <- (dg$nxk/dg$yk)*100

dg$chk_y_0 <- (dg$chk_0/dg$yk_0)*100
dg$ik_y_0 <- (dg$ik_0/dg$yk_0)*100
dg$gk_y_0 <- (dg$gk_0/dg$yk_0)*100
dg$nxk_y_0 <- (dg$nxk_0/dg$yk_0)*100

dg$chk_y_1 <- (dg$chk_1/dg$yk_1)*100
dg$ik_y_1 <- (dg$ik_1/dg$yk_1)*100
dg$gk_y_1 <- (dg$gk_1/dg$yk_1)*100
dg$nxk_y_1 <- (dg$nxk_1/dg$yk_1)*100

dg_g <- sfc_shock2 %>%
  dplyr::select(Dato, nxk, nxk_0, nxk_1, chk, chk_0,chk_1, ik, ik_0,ik_1,yk, yk_0,yk_1, gk, gk_0, gk_1) %>%
  slice(8:14)

dg$chk_y_1_g <- (dg_g$chk_1/dg_g$yk_1)*100
dg$ik_y_1_g <- (dg_g$ik_1/dg_g$yk_1)*100
dg$gk_y_1_g <- (dg_g$gk_1/dg_g$yk_1)*100
dg$nxk_y_1_g <- (dg_g$nxk_1/dg_g$yk_1)*100

dg_g_ad <- sfc_shock2_ad %>%
  dplyr::select(Date, nxk, nxk_0, nxk_1, chk, chk_0,chk_1, ik, ik_0,ik_1,yk, yk_0,yk_1, gk, gk_0, gk_1) %>%
  slice(8:14)

dg$chk_y_1_g_ad <- (dg_g_ad$chk_1/dg_g_ad$yk_1)*100
dg$ik_y_1_g_ad <- (dg_g_ad$ik_1/dg_g_ad$yk_1)*100
dg$gk_y_1_g_ad <- (dg_g_ad$gk_1/dg_g_ad$yk_1)*100
dg$nxk_y_1_g_ad <- (dg_g_ad$nxk_1/dg_g_ad$yk_1)*100

dg_res_ad <- sfc_shock3_ad %>%
  dplyr::select(Date, nxk, nxk_0, nxk_1, chk, chk_0,chk_1, ik, ik_0,ik_1,yk, yk_0,yk_1, gk, gk_0, gk_1) %>%
  slice(8:14)

dg$chk_y_1_res_ad <- (dg_res_ad$chk_1/dg_res_ad$yk_1)*100
dg$ik_y_1_res_ad <- (dg_res_ad$ik_1/dg_res_ad$yk_1)*100
dg$gk_y_1_res_ad <- (dg_res_ad$gk_1/dg_res_ad$yk_1)*100
dg$nxk_y_1_res_ad <- (dg_res_ad$nxk_1/dg_res_ad$yk_1)*100

#Stød til huspriser

sfc_shock4 <- read_excel("husstød1.xlsx")
sfc_shock4 <- rename(sfc_shock4, replace = c("Date" = "Dato")) 

data_shock4 <- as.data.frame(sfc_shock4$Dato)
data_shock4$`sfc_shock4$Dato`<- as.Date(data_shock4$`sfc_shock4$Dato`)
data_shock4 <- rename(data_shock4, replace = c("sfc_shock4$Dato" = "Dato")) 

data_shock4$ibl_h <- sfc_shock4$ibl_h_for
data_shock4$inv_h <- sfc_shock4$invh_k_for
data_shock4$ur <- sfc_shock4$ur_for
data_shock4$ydh <- sfc_shock4$ydhk_for
data_shock4$y <- sfc_shock4$yk_for
data_shock4$chk <- sfc_shock4$chk_for

data_shock4 <- data_shock4[-c(1:7), ]
data_shock4 <- data_shock4[-c(8:15), ]

p69 <- ggplot(data_shock4, aes(x = Dato, y = ibl_h)) + geom_line(color = "#F8766D") +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-8, 0), breaks=round(seq(min(-8), max(0), by = 1),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in household debt",subtitle = "Decreasing house price with 30%",x=NULL, y=NULL, caption = "Source: Own calculation") +
  th + theme(legend.position="none")

p70 <- ggplot(data_shock4, aes(x = Dato, y = inv_h)) + geom_line(color = "#F8766D") +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-16, 0), breaks=round(seq(min(-16), max(0), by = 2),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in household real investment", subtitle ="Decreasing house price with 30%", x=NULL, y=NULL, caption = "Source: Own calculation") +
  th + theme(legend.position="none")

p71 <- ggplot(data_shock4, aes(x = Dato, y = ur)) + geom_line(color = "#F8766D") +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(0, 0.25), breaks=round(seq(min(0), max(0.25), by = 0.05),2), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in unemployment rate", subtitle ="Decreasing house price with 30%", x=NULL, y="%-point", caption = "Source: Own calculation") +
  th + theme(legend.position="none")

p72 <- ggplot(data_shock4, aes(x = Dato, y = ydh)) + geom_line(color = "#F8766D") +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-0.07, 0.25), breaks=round(seq(min(-0.05), max(0.25), by = 0.05),2), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in household disposable income", subtitle ="Decreasing house price with 30%", x=NULL, y=NULL, caption = "Source: Own calculation") +
  th + theme(legend.position="none")

p73 <- ggplot(data_shock4, aes(x = Dato, y = y)) + geom_line(color = "#F8766D") +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-0.5, 0), breaks=round(seq(min(-0.50), max(0), by = 0.1),2), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in real output", subtitle ="Decreasing house price with 30%", x=NULL, y=NULL, caption = "Source: Own calculation") +
  th + theme(legend.position="none")

p74 <- ggplot(data_shock4, aes(x = Dato, y = chk)) + geom_line(color = "#F8766D") +
  geom_line(y = 0, color = "black") +
  scale_y_continuous(limits = c(-0.22, 0), breaks=round(seq(min(-0.22), max(0), by = 0.02),3), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title = "Development in household real consumption", subtitle ="Decreasing house price with 30%", x=NULL, y=NULL, caption = "Source: Own calculation") +
  th + theme(legend.position="none")

#ggsave("gæld_stødhus.pdf", p69, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("investeringer_stødhus.pdf", p70, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("arbejdsløshed_stødhus.pdf", p71, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("disp_indkomst_stødhus.pdf", p72, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("output_stødhus.pdf", p73, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")
#ggsave("forbrug_stødhus.pdf", p74, width = 15, height = 10, units = "cm", path = "~/Dropbox/Apps/Overleaf/Bachelorprojekt/Fig")

