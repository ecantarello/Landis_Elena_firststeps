library(plyr)
library(ggplot2)

Landis_calibrate<-read.csv("Landis_v1/Century-calibrate-log.csv")
str(Landis_calibrate)

leafb<-ddply(Landis_calibrate,.(Year,Month),summarise,mean_leaf=mean(CohortLeafB))
leafb_mean<-ddply(Landis_calibrate,.(Month),summarise,mean_leaf=mean(CohortLeafB))

ggplot(leafb_mean,aes(x=Month,y=mean_leaf))+geom_point()+geom_line()


Nit_precip<-data.frame(Precip=c(800,900,1000),Nit_dep=c(1,1.35,1.7))

lm(Nit_dep~Precip,data=Nit_precip)

Landis_comparison<-read.csv("Landis_v1/Century-succession-monthly-log_comp.csv")
View(Landis_comparison)
Landis_comparison$N_month<-NA
Landis_comparison$N_month[1]<-1
for (i in 2:nrow(Landis_comparison)){
  Landis_comparison$N_month[i]<-Landis_comparison$N_month[i-1]+1
}

Comp_comp1<-ggplot(Landis_comparison,aes(x=N_month,y=avgNEE))+geom_point(colour="red")+geom_line(alpha=0.2,colour="red")
Comp_comp2<-Comp_comp1+geom_point(data=Landis_comparison,aes(y=NEE_measured),colour="blue")+geom_line(alpha=0.2,colour="blue",aes(y=NEE_measured))
Comp_comp2+ylab("NEE")+xlab("Number of months after first measurement")
