library(plyr)
library(ggplot2)

e<-read.csv("LandisNF_070815v1/Century-succession-monthly-log_comp.csv")
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
# Calculate range from 0 to max value
e_range <- range(0, avgNEE, NEE_measured)

plot(avgNEE, type="o", col="blue", ylim=e_range, 
     axes=FALSE, ann=FALSE)

# Create box around plot
box()

# Graph NEE_measured with red dashed line and square points
lines(NEE_measured, pch=22, lty=2, col="red")

# Label the x and y axes with dark green text
title(xlab="Month", col.lab=rgb(0,0.5,0))
title(ylab="NEE (g/m2)", col.lab=rgb(0,0.5,0))

# Create a title with a red, bold/italic font
title(main="NEE modelled and measured", col.main="red", font.main=4)

attach(e)   
# Calculate range from 0 to max value
y_range <- range(0, avgNEE)
x_range <- range (0, Month)

# set up the plot
plot(y_range, x_range, type="n", xlab="Month",
     ylab="avgNEE (g/m2)" )
plot(Month, avgNEE)

attach(e)
#does linear regression
mod1<- lm(avgNEE~NEE_measured)
summary(mod1)

d<-read.csv("LandisNF_050815v2/Century-succession-log.csv")
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

a<-read.csv("LandisNF_070815v2/Century-calibrate-log.csv")
str(a)

leafb<-ddply(a,.(Month),summarise,mean_leaf=mean(CohortLeafB))
leafb_mean<-ddply(a,.(Month,rlai, SpeciesName),summarise,mean_leaf=mean(CohortLeafB))
ltlai_mean<-ddply(a,.(Month,tlai, SpeciesName),summarise,mean_leaf=mean(tlai))

ggplot(leafb,aes(x=Month,y=mean_leaf))+geom_point()+geom_line()+
  theme(axis.title.y = element_text(size = rel(3), angle = 90,vjust=0.5),axis.title.x = element_text(size = rel(3)))+ ## rel = size relative to other text; v-just adjusts the position of the axis labels in this case
  theme(axis.text.x=element_text(angle=0, size=30, vjust=0.5)) + # This specifies the size of your x-axis values
  theme(axis.text.y=element_text(angle=0, size=30, vjust=0.5)) # This specifies the size of your y-axis values

#start here to analyse AGB, SOMTC, ect.
c<-read.csv("LandisNF_070815v2/Century-succession-log.csv")
ggplot(c,aes(x=Time,y=AGB))+geom_point()+geom_line()
ggplot(c,aes(x=Time,y=SOMTC))+geom_point()+geom_line()
ggplot(c,aes(x=Time,y=TotalSoilN))+geom_point()+geom_line()

AGB_mean<-ddply(c,.(Time),summarise,mean_AGB=mean(AGB))

#start Paul's code
ggplot(c,aes(x=Time,y=AGB))+geom_point()+geom_line()+
  theme(axis.title.y = element_text(size = rel(3), angle = 90,vjust=0.5),axis.title.x = element_text(size = rel(3)))+ ## rel = size relative to other text; v-just adjusts the position of the axis labels in this case
  theme(axis.text.x=element_text(angle=0, size=30, vjust=0.5)) + # This specifies the size of your x-axis values
  theme(axis.text.y=element_text(angle=0, size=30, vjust=0.5)) # This specifies the size of your y-axis values
ggsave("F:name.jpg",width = 8,height = 6,units = "in",dpi = 400) # outputs the graph at specified dimensions
#finish Paul's code

attach(c)   
# Calculate range from 0 to max value
y_range <- range(0, SOMTC)
x_range <- range (0, Time)

# set up the plot
plot(y_range, x_range, type="n", xlab="Years",
     ylab="SOMTC (g/m2)" )
plot(Time, SOMTC)

# Calculate range from 0 to max value
y_range <- range(0, AGB)
x_range <- range (0, Time)

# set up the plot
plot(y_range, x_range, type="n", xlab="Years",
     ylab="AGB (g/m2)" )
plot(Time, AGB)

# Calculate range from 0 to max value
y_range <- range(0, TotalSoilN)
x_range <- range (0, Time)

# set up the plot
plot(y_range, x_range, type="n", xlab="Years",
     ylab="TotalSoilN (g/m2)" )
plot(Time, TotalSoilN)

# Calculate range from 0 to max value
y_range <- range(0, C_DeadWood)
x_range <- range (0, Time)

# set up the plot
plot(y_range, x_range, type="n", xlab="Years",
     ylab="C_DeadWood (g/m2)" )
plot(Time, C_DeadWood)

#establishment probability
b<-read.csv("LandisNF_050815v2/Century-prob-establish-log.csv")

ProbEst_mean<-ddply(b,.(Species,Ecoregion),summarise,mean_ProbEst=mean(ProbEst))
write.csv(ProbEst_mean, file="ProbEst_mean.csv")

ProbEst_mean<-ddply(b,.(Species),summarise,mean_ProbEst=mean(ProbEst))

#Full land scenario
a<-read.csv("LandisNF_060815v4/Century-succession-log.csv")
str(a)
b<--read.csv("LandisNF_300715v10/Century-succession-monthly-log.csv")

AGB_mean<-ddply(a,.(Time),summarise,mean_AGB=mean(AGB))
SOMTC_mean<-ddply(a,.(Time),summarise,mean_SOMTC=mean(SOMTC))
TotalSoilN_mean<-ddply(a,.(Time),summarise,mean_TotalSoilN=mean(TotalSoilN))
TotalNdep_mean<-ddply(a,.(Time),summarise,mean_TotalNdep=mean(TotalNdep))
TotalCDeadwood_mean<-ddply(a,.(Time),summarise,mean_C_DeadWood=mean(C_DeadWood))




ggplot(AGB_mean,aes(x=Time,y=mean_AGB))+geom_point()+geom_line()+
  theme(axis.title.y = element_text(size = rel(3), angle = 90,vjust=0.5),axis.title.x = element_text(size = rel(3)))+ ## rel = size relative to other text; v-just adjusts the position of the axis labels in this case
  theme(axis.text.x=element_text(angle=0, size=30, vjust=0.5)) + # This specifies the size of your x-axis values
  theme(axis.text.y=element_text(angle=0, size=30, vjust=0.5)) # This specifies the size of your y-axis values
ggplot(SOMTC_mean,aes(x=Time,y=mean_SOMTC))+geom_point()+geom_line()+
  theme(axis.title.y = element_text(size = rel(3), angle = 90,vjust=0.5),axis.title.x = element_text(size = rel(3)))+ ## rel = size relative to other text; v-just adjusts the position of the axis labels in this case
  theme(axis.text.x=element_text(angle=0, size=30, vjust=0.5)) + # This specifies the size of your x-axis values
  theme(axis.text.y=element_text(angle=0, size=30, vjust=0.5)) # This specifies the size of your y-axis values
ggplot(TotalSoilN_mean,aes(x=Time,y=mean_TotalSoilN))+geom_point()+geom_line()+
  theme(axis.title.y = element_text(size = rel(3), angle = 90,vjust=0.5),axis.title.x = element_text(size = rel(3)))+ ## rel = size relative to other text; v-just adjusts the position of the axis labels in this case
  theme(axis.text.x=element_text(angle=0, size=30, vjust=0.5)) + # This specifies the size of your x-axis values
  theme(axis.text.y=element_text(angle=0, size=30, vjust=0.5)) # This specifies the size of your y-axis values
ggplot(TotalCDeadwood_mean,aes(x=Time,y=mean_C_DeadWood))+geom_point()+geom_line()+
  theme(axis.title.y = element_text(size = rel(3), angle = 90,vjust=0.5),axis.title.x = element_text(size = rel(3)))+ ## rel = size relative to other text; v-just adjusts the position of the axis labels in this case
  theme(axis.text.x=element_text(angle=0, size=30, vjust=0.5)) + # This specifies the size of your x-axis values
  theme(axis.text.y=element_text(angle=0, size=30, vjust=0.5)) # This specifies the size of your y-axis values

#spp biomass
d<-read.csv("LandisNF_060815v4/spp-biomass-log_yr0.csv")
e<- d[,c(4:37)]
?colSums
f<-colMeans(e, na.rm = TRUE, dims = 1) #sum colums values #remember to set no decimals in excel
f<-round(f, digits = 2)
g<-sort (f, decreasing = TRUE)
View(g)

#spp biomass time year 140
h<-read.csv("LandisNF_060815v4/spp-biomass-log_yr140.csv")
i<- h[,c(5:38)]
?colSums
l<-colMeans(i, na.rm = TRUE, dims = 1) #sum colums values #remember to set no decimals in excel
l<-round(l, digits = 0)
m<-sort (l, decreasing = TRUE)
View(m)


#establishment probability
b<-read.csv("LandisNF_060815v4/Century-prob-establish-log.csv")
ProbEst_mean<-ddply(b,.(Species),summarise,mean_ProbEst=mean(ProbEst))
write.csv(ProbEst_mean, file="ProbEst_mean.csv")

#avgNEE
c<-read.csv("LandisNF_060815v4/Century-succession-monthly-log.csv")
avgNEE_mean<-ddply(c,.(Month),summarise,mean_avgNEE=mean(avgNEE))

