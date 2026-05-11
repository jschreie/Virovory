library(data.table)
library(tidyverse)
library(scales)

meta <- fread('ViralInactivationTests/Version_4/UVinactivation_v4_meta.csv')


flowData <- fread('ViralInactivationTests/Version_4/UVinactivation_v4.csv') %>% 
  separate(., col='Sample', sep='_', into=c('Sample', 'DilutionFactor')) %>% 
  inner_join(., meta)

flowData$DilutionFactor <- as.numeric(flowData$DilutionFactor)
flowData$Ehux_ml <- flowData$Ehux*(1/flowData$DilutionFactor)
flowData$Bact1_ml <- flowData$Bact1*(1/flowData$DilutionFactor)
flowData$Bact2_ml <- flowData$Bact2*(1/flowData$DilutionFactor)
flowData$EhV86_ml <- flowData$EhV86*(1/flowData$DilutionFactor)



#Collapse technical reps
data <- flowData %>% 
  group_by(., Day, Condition, Rep) %>% 
  summarize(., Ehux_ml=mean(Ehux_ml), Bact1_ml=mean(Bact1_ml), Bact2_ml=mean(Bact2_ml), EhV86_ml=mean(EhV86_ml))


UVTreat_initial <- c(data$EhV86_ml[data$Day==0 & data$Condition=='UV1'],data$EhV86_ml[data$Day==0 & data$Condition=='UV3'], data$EhV86_ml[data$Day==0 & data$Condition=='UV5'])
UVTreat_final <- c(data$EhV86_ml[data$Day==7 & data$Condition=='UV1'],data$EhV86_ml[data$Day==7 & data$Condition=='UV3'], data$EhV86_ml[data$Day==7 & data$Condition=='UV5'])

mean(mean(UVTreat_initial)/ UVTreat_final)
sd(mean(UVTreat_initial)/ UVTreat_final)


meanData <- data %>% 
  group_by(., Day, Condition) %>% 
  summarize(., meanEhux=mean(Ehux_ml), meanBact1=mean(Bact1_ml), meanBact2=mean(Bact2_ml), meanEhV86=mean(EhV86_ml),sdEhux=sd(Ehux_ml), sdBact1=sd(Bact1_ml), sdBact2=sd(Bact2_ml), sdEhV86=sd(EhV86_ml))

meanData$Day <- as.numeric(meanData$Day)
plotter<- function(time, N, sd, color){
  points(time, N, pch=16, col=color, cex=2)
  lines(time, N, pch=16, col=color, lwd=2)
  segments(x0=time, y0=N-sd, y1=N+sd, col=color, lwd=2)
}

virus <- meanData$Condition=='positive'
UV1 <- meanData$Condition=='UV1'
UV3 <- meanData$Condition=='UV3'
UV5 <- meanData$Condition=='UV5'
noVir <- meanData$Condition=='negative'
noHost <- meanData$Condition=='noHost'

virColor <- alpha('skyblue', 0.6)
UV1Color <- alpha('violet', 0.6)
UV3Color <- alpha('purple1', 0.6)
UV5Color <- alpha('purple4', 0.6)
controlColor <- alpha('darkgreen', 0.6)
noHostColor <- alpha('grey', 0.6)


pdf('Figures/viralInactivation_v4_Results_20260427.pdf', width=10, height=5)
par(mfrow=c(1,2))
plot(meanData$Day, meanData$meanEhux, las=1, bty='n', xlab='Day', ylab='meanEhux_ml', log='y', ylim=c(1E3, 1E7), type='n')
plotter(meanData$Day[virus], meanData$meanEhux[virus], meanData$sdEhux[virus], col=virColor)
plotter(meanData$Day[UV1], meanData$meanEhux[UV1], meanData$sdEhux[UV1], col=UV1Color)
plotter(meanData$Day[UV3], meanData$meanEhux[UV3], meanData$sdEhux[UV3], col=UV3Color)
plotter(meanData$Day[UV5], meanData$meanEhux[UV5], meanData$sdEhux[UV5], col=UV5Color)
plotter(meanData$Day[noVir], meanData$meanEhux[noVir], meanData$sdEhux[noVir], col=controlColor)


plot(meanData$Day, meanData$meanEhV86, las=1, bty='n', xlab='Day', ylab='meanEhV86_ml', log='y', ylim=c(1E5, 1E9), type='n')
plotter(meanData$Day[virus], meanData$meanEhV86[virus], meanData$sdEhux[virus], col=virColor)
plotter(meanData$Day[UV1], meanData$meanEhV86[UV1], meanData$sdEhux[UV1], col=UV1Color)
plotter(meanData$Day[UV3], meanData$meanEhV86[UV3], meanData$sdEhux[UV3], col=UV3Color)
plotter(meanData$Day[UV5], meanData$meanEhV86[UV5], meanData$sdEhux[UV5], col=UV5Color)
plotter(meanData$Day[noHost], meanData$meanEhV86[noHost], meanData$sdEhux[noHost], col=noHostColor)

legend(3, 1E8, legend=c('1 min UV', '3 min UV', '5 min UV', 'no UV', 'uninfected control', 'no host'), col=c(UV1Color, UV3Color, UV5Color, virColor, controlColor, noHostColor), pch=16, bty='n', lty=1)

dev.off()