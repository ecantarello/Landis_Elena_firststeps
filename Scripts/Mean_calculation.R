library(plyr)
library(ggplot2)

Landis_calibrate<-read.csv("Landis_v1/Century-calibrate-log.csv")
str(Landis_calibrate)

leafb<-ddply(Landis_calibrate,.(Year,Month),summarise,mean_leaf=mean(CohortLeafB))
leafb_mean<-ddply(Landis_calibrate,.(Month),summarise,mean_leaf=mean(CohortLeafB))

ggplot(leafb_mean,aes(x=Month,y=mean_leaf))+geom_point()
