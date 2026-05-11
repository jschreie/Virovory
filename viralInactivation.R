library(data.table)
library(tidyverse)
library(scales)

meta <- fread('ViralInactivationTests/Version_1/metaData_viralInactivation.csv')
flowData <- fread('ViralInactivationTests/Version_1/viralInactivation_20260219.csv') %>% 
  separate(., col='Sample', sep='_', into=c('Sample', 'Day', 'DilutionFactor')) %>% 
  inner_join(meta, .)
flowData$DilutionFactor <- as.numeric(flowData$DilutionFactor)
flowData$Ehux_ml <- flowData$Ehux*(1/flowData$DilutionFactor)
flowData$Bact_ml <- flowData$Bact*(1/flowData$DilutionFactor)
flowData$EhV86_ml <- flowData$EhV86*(1/flowData$DilutionFactor)


#Collapse technical reps
data <- flowData %>% 
  group_by(., Day, Condition, Rep) %>% 
  summarize(., Ehux_ml=mean(Ehux_ml), Bact_ml=mean(Bact_ml), EhV86_ml=mean(EhV86_ml))

heatTreat_initial <- c(data$EhV86_ml[data$Day==0 & data$Condition=='Heat20'],data$EhV86_ml[data$Day==0 & data$Condition=='Heat50'])
heatTreat_final <- c(data$EhV86_ml[data$Day==6 & data$Condition=='Heat20'],data$EhV86_ml[data$Day==6 & data$Condition=='Heat50'])

mean(mean(heatTreat_initial)/ heatTreat_final)
sd(mean(heatTreat_initial)/ heatTreat_final)


meanData <- data %>% 
  group_by(., Day, Condition) %>% 
  summarize(., meanEhux=mean(Ehux_ml), meanBact=mean(Bact_ml), meanEhV86=mean(EhV86_ml),sdEhux=sd(Ehux_ml), sdBact=sd(Bact_ml), sdEhV86=sd(EhV86_ml))

meanData$Day <- as.numeric(meanData$Day)
plotter<- function(time, N, sd, color){
  points(time, N, pch=16, col=color, cex=2)
  lines(time, N, pch=16, col=color, lwd=2)
  segments(x0=time, y0=N-sd, y1=N+sd, col=color, lwd=2)
}

heat20 <- meanData$Condition=='Heat20'
heat50 <- meanData$Condition=='Heat50'
virus <- meanData$Condition=='noHeat'
cont <- meanData$Condition=='noVirus'


heat20Color <- alpha('red', 0.6)
heat50Color <- alpha('firebrick', 0.6)
virusColor <- alpha('skyblue3', 0.6)
controlColor <- alpha('darkgreen', 0.6)


pdf('Figures/viralInactivationResults_.pdf', width=10, height=5)
par(mfrow=c(1,2))

plot(meanData$Day, meanData$meanEhux, las=1, bty='n', xlab='Day', ylab='meanEhux_ml', log='y', ylim=c(1E3, 1E7), type='n')
plotter(meanData$Day[heat20], meanData$meanEhux[heat20], meanData$sdEhux[heat20], col=heat20Color)
plotter(meanData$Day[heat50], meanData$meanEhux[heat50], meanData$sdEhux[heat50], col=heat50Color)
plotter(meanData$Day[virus], meanData$meanEhux[virus], meanData$sdEhux[virus], col=virusColor)
plotter(meanData$Day[cont], meanData$meanEhux[cont], meanData$sdEhux[cont], col=controlColor)



plot(meanData$Day, meanData$meanEhV86, las=1, bty='n', xlab='Day', ylab='meanEhV86_ml', log='y', ylim=c(1E5, 1E8), type='n')
plotter(meanData$Day[heat20], meanData$meanEhV86[heat20], meanData$sdEhV86[heat20], col=heat20Color)
plotter(meanData$Day[heat50], meanData$meanEhV86[heat50], meanData$sdEhV86[heat50], col=heat50Color)
plotter(meanData$Day[virus], meanData$meanEhV86[virus], meanData$sdEhV86[virus], col=virusColor)
plotter(meanData$Day[cont], meanData$meanEhV86[cont], meanData$sdEhV86[cont], col=controlColor)
legend(1, 1E6, legend=c('50C_20min', '50C_60min', 'noHeat', 'uninfected'), col=c(heat20Color, heat50Color, virusColor, controlColor), pch=16, bty='n', lty=1)
# 
# plot(meanData$Day, meanData$meanBact, las=1, bty='n', xlab='Day', ylab='meanBact_ml', log='y', ylim=c(1E4, 1E7), type='n')
# plotter(meanData$Day[heat20], meanData$meanBact[heat20], meanData$sdBact[heat20], col=heat20Color)
# plotter(meanData$Day[heat50], meanData$meanBact[heat50], meanData$sdBact[heat50], col=heat50Color)
# plotter(meanData$Day[virus], meanData$meanBact[virus], meanData$sdBact[virus], col=virusColor)
# plotter(meanData$Day[cont], meanData$meanBact[cont], meanData$sdBact[cont], col=controlColor)
dev.off()


burst <- (data$EhV86_ml[data$Day==4 & data$Condition=='noHeat'] - data$EhV86_ml[data$Day==0 & data$Condition=='noHeat'])/(data$Ehux_ml[data$Day==0 & data$Condition=='noHeat'] - data$Ehux_ml[data$Day==4 & data$Condition=='noHeat'])

