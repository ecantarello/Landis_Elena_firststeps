library(plyr)
library(ggplot2)

e<-read.csv("LandisNF_singlecell_280715v6/Century-succession-monthly-log_comp.csv")
str(d)

View(e)
e$N_month<-NA
e$N_month[1]<-1
for (i in 2:nrow(e)){
  e$N_month[i]<-e$N_month[i-1]+1
}

install.packages("ggplot2")
install.packages("plyr")

Comp_comp1<-ggplot(e,aes(x=N_month,y=avgNEE,group=Time))+geom_point(colour="red")+geom_line(alpha=0.2,colour="red")
Comp_comp2<-Comp_comp1+geom_point(data=e,aes(y=NEE_measured),colour="blue")+geom_line(alpha=0.2,colour="blue",aes(y=NEE_measured))
Comp_comp2+ylab("NEE")+xlab("Number of months after first measurement")


attach(e)
#does linear regression
mod1<- lm(avgNEE~NEE_measured)
summary(mod1)

d<-read.csv("LandisNF_singlecell_280715v6/Century-succession-log.csv")
ggplot(d,aes(x=Time,y=AGB))+geom_point()+geom_line()
ggplot(d,aes(x=Time,y=SOMTC))+geom_point()+geom_line()
ggplot(d,aes(x=Time,y=TotalSoilN))+geom_point()+geom_line()

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

a<-read.csv("LandisNF_singlecell_280715v6/Century-calibrate-log.csv")
str(a)

leafb<-ddply(a,.(Year,Month),summarise,mean_leaf=mean(CohortLeafB))
leafb_mean<-ddply(a,.(Month,rlai, SpeciesName),summarise,mean_leaf=mean(CohortLeafB))
ltlai_mean<-ddply(a,.(Month,tlai, SpeciesName),summarise,mean_leaf=mean(tlai))

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