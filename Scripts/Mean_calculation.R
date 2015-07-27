library(plyr)

Landis_calibrate<-read.csv("Landis_v1/Century-calibrate-log.csv")
str(Landis_calibrate)

ddply(Landis_calibrate,.(Year,Month),summarise,mean_leaf=mean(CohortLeafB))

