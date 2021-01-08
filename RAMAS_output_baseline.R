###############################################
# script to read .MP output for baseline scenario
# 
###############################################
library(mptools)

time=2020:2050
mp1 <- ('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/RAMAS/Clivia_miniata_2020/Clivia_miniata_baseline_s1_l.MP')
mp2 <- ('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/RAMAS/Clivia_miniata_2020/Clivia_miniata_baseline_s1_u.MP')
mp3 <- ('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/RAMAS/Clivia_miniata_2020/Clivia_miniata_baseline_s2_l.MP')
mp4 <- ('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/RAMAS/Clivia_miniata_2020/Clivia_miniata_baseline_s2_u.MP')

res1 <- results(mp=mp1)
res2 <- results(mp=mp2)
res3 <- results(mp=mp3)
res4 <- results(mp=mp4)

te<-data.frame(time,res1$results[,,'ALL'],res2$results[,,'ALL'],res3$results[,,'ALL'],res4$results[,,'ALL'])
head(te)

c1 <- rainbow(10)
c2 <- rainbow(10, alpha=0.2)
c3 <- rainbow(10, v=0.7)

plot(time,te$mean,ylim=c(0,400),col='white', ylab='abundance [%]',xlab='year',main='baseline - mean')
legend("topleft", fill = c(c2[[5]],c2[[9]]), legend = c('site1','site2'),box.col='white',inset=0.01)

polygon(c(time, rev(time)),c((te$mean-(2*te$sd))*100/te$mean[[1]], rev((te$mean.1+(2*te$sd.1))*100/te$mean.1[[1]])), col=c2[[5]], border =NA) 
polygon(c(time, rev(time)),c((te$mean.2-(2*te$sd.2))*100/te$mean.2[[1]], rev((te$mean.3+(2*te$sd.3))*100/te$mean.3[[1]])), col=c2[[9]], border =NA) 

#############################################################################################
#### END ###     
#############################################################################################
