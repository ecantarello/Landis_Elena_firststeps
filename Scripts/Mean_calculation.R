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

<<<<<<< HEAD
Comp_comp1<-ggplot(Landis_comparison,aes(x=N_month,y=avgNEE))+geom_point(colour="red")+geom_line(alpha=0.2,colour="red")
=======
install.packages("ggplot2")


Comp_comp1<-ggplot(Landis_comparison,aes(x=N_month,y=avgNEE,group=Time))+geom_point(colour="red")+geom_line(alpha=0.2,colour="red")
>>>>>>> 79832518977a97442a3019f4b52b4bc2696ea64d
Comp_comp2<-Comp_comp1+geom_point(data=Landis_comparison,aes(y=NEE_measured),colour="blue")+geom_line(alpha=0.2,colour="blue",aes(y=NEE_measured))
Comp_comp2+ylab("NEE")+xlab("Number of months after first measurement")

ppt<-read.csv("Landis_v1/Precipitation_2004-2011.csv")
View(ppt)

ppt_mean<-ddply(ppt,.(Year),summarise,mean_ppt=mean(ppt..mm.month.))
ppt_sum<-ddply(ppt,.(Year),summarise,sum_ppt=sum(ppt..mm.month.))

write.csv(ppt_mean, file = "ppt_mean.csv")

?write.csv

nitro<-read.csv("Landis_v1/Nitrogen_deposition.csv")
lm(N_dep~ppt_mm,data=nitro)

ggplot(nitro,aes(x=ppt_mm,y=N_dep))+geom_point()+geom_line()


Landis_comparison<-read.csv("LandisNF_singlecell_mc136_270715v3/Century-succession-monthly-log_comp.csv")
View(Landis_comparison)
Landis_comparison$N_month<-NA
Landis_comparison$N_month[1]<-1
for (i in 2:nrow(Landis_comparison)){
  Landis_comparison$N_month[i]<-Landis_comparison$N_month[i-1]+1
}

install.packages("ggplot2")


Comp_comp1<-ggplot(Landis_comparison,aes(x=N_month,y=avgNEE,group=Time))+geom_point(colour="red")+geom_line(alpha=0.2,colour="red")
Comp_comp2<-Comp_comp1+geom_point(data=Landis_comparison,aes(y=NEE_measured),colour="blue")+geom_line(alpha=0.2,colour="blue",aes(y=NEE_measured))
Comp_comp2+ylab("NEE")+xlab("Number of months after first measurement")

e<- read.csv("LandisNF_singlecell_mc136_270715v3/Century-succession-monthly-log_comp.csv")
attach(e)
#does linear regression
mod1<- lm(avgNEE~NEE_measured)
summary(mod1)

Landis_calibrate<-read.csv("LandisNF_singlecell_mc136_270715v3/Century-calibrate-log.csv")
str(Landis_calibrate)

leafb<-ddply(Landis_calibrate,.(Year,Month),summarise,mean_leaf=mean(CohortLeafB))
leafb_mean<-ddply(Landis_calibrate,.(Month,SpeciesName),summarise,mean_leaf=mean(CohortLeafB))

ggplot(leafb_mean,aes(x=Month,y=mean_leaf))+geom_point()+geom_line()

c<-read.csv("LandisNF_singlecell_mc136_270715v3/Century-succession-log.csv")
ggplot(c,aes(x=Time,y=AGB))+geom_point()+geom_line()
ggplot(c,aes(x=Time,y=SOMTC))+geom_point()+geom_line()
ggplot(c,aes(x=Time,y=TotalSoilN))+geom_point()+geom_line()


attach(c)   
# Calculate range from 0 to max value
y_range <- range(0, SOMTC)
x_range <- range (0, Time)

# set up the plot
plot(y_range, x_range, type="n", xlab="Years",
     ylab="SOMTC (g/m2)" )
plot(Time, SOMTC)
