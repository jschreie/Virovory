library(data.table)
library(tidyverse)

flowData <- fread('Ehux374_1516_PrelimGrowth.csv')

flowData$Ehux_ml <- flowData$Ehux_AbsCounts*(1/flowData$DilutionFactor)

#Collapse technical reps
Ehux <- flowData %>% 
  na.omit() %>% 
  group_by(., Time, Culture, Rep) %>% 
  summarize(., Ehux_ml=mean(Ehux_ml))

Eh1516 <- Ehux$Culture=='1516'
Eh374 <- Ehux$Culture=='374'
A <- Ehux$Rep=='A'
B <- Ehux$Rep=='B'

plotter<- function(time, N, color){
  points(time, N, pch=16, col=color)
  lines(time, N, pch=16, col=color, lwd=2)
}


plot(Ehux$Time[Eh374 & A], Ehux$Ehux_ml[Eh374 & A], pch=16, log='y', ylim=c(1E5, 1E7), type='n')
plotter(Ehux$Time[Eh374 & A], Ehux$Ehux_ml[Eh374 & A], color='blue')
plotter(Ehux$Time[Eh374 & B], Ehux$Ehux_ml[Eh374 & B], color='skyblue')

plotter(Ehux$Time[Eh1516 & A], Ehux$Ehux_ml[Eh1516 & A], color='red')
plotter(Ehux$Time[Eh1516 & B], Ehux$Ehux_ml[Eh1516 & B], color='darkred')
