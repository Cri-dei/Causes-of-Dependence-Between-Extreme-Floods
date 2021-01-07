library(plotly)
library(gridExtra)
library(ggpubr)
library(viridis)
library(lattice)
library(ggplot2)
library(hrbrthemes)
library(GGally)
library(tidyverse)
library(sf)
library(maps)       # Provides functions that let us plot the maps
library(mapdata)
library(mapproj)
library(ggplot2)
library(cowplot)




###################
ALL_LAG<-list()
ALL_LAG_POS<-list()
ALL_LAG_IND<-list()
pvalue<-0.01
Lag_time<-c(5,7,10)

for (x in 1:length(Lag_time))
{

  setwd(paste0("C:/Users/39349/Documents/Regional/Lag time/Lagtime_",Lag_time[x],"/Workspace"))

  load(file=paste0("M_ALL_",pvalue,"_perc_LAG_",Lag_time[x],".RData"))
  load(file=paste0("M_ALL_perc_LAG_",Lag_time[x],".RData"))
  load(file=paste0("M_ALL_IND_perc_",pvalue,"_LAG_",Lag_time[x],".RData"))
  ALL_LAG_POS[[x]]<- M_ALL_0.01_POS_perc
  ALL_LAG[[x]]<- M_ALL
  ALL_LAG_IND[[x]]<- M_ALL_IND_perc0.01
}

names(ALL_LAG)<-Lag_time
names(ALL_LAG_POS)<-Lag_time
names(ALL_LAG_IND)<-Lag_time
###########################

setwd("C:/Users/39349/Documents/Regional/Lag time")

pdf(file="LAG TIME_plot2.pdf",width=9, height=9)  
for (i in 1:length(Lag_time))
{
  
########### 3. Kendall Tau and Distance with regression line ###########

M_ALL_0.01_POS<-ALL_LAG_POS[[i]]

gh<-which( M_ALL_0.01$KendalT.value>0 )

M_ALL_0.01_POS$Distance<-  M_ALL_0.01_POS$Distance.x
#Regression for Only positive
q1<-M_ALL_0.01$Distance[gh]
y<- M_ALL_0.01$KendalT.value[gh]

#Regression for All
#q1<-M_ALL_0.01$Distance
#y<- M_ALL_0.01$KendalT.value


model <- lm(y ~ poly(q1,4))
predicted.intervals <- predict(model,data.frame(x=q1),interval='confidence', level=0.99)


predicted<-cbind(q1,predicted.intervals[,1])
require(data.table)
predicted<- data.table(predicted, key="q1")

par(mfrow=c(1,1))

xx = c(220,220,240,240)
yy = c(-1,1.5,1.5,-1)

## Distance selection
Dist_sel<-230

M_near_01_POS<-M_ALL_0.01_POS[M_ALL_0.01_POS$Distance<=Dist_sel,]
Near_perc_POS<-M_ALL_0.01_POS_perc[M_ALL_0.01_POS_perc$Distance.x<=Dist_sel,]
M_far_01_POS<-M_ALL_0.01_POS[M_ALL_0.01_POS$Distance>Dist_sel,]
Far_perc_POS<-M_ALL_0.01_POS_perc[M_ALL_0.01_POS_perc$Distance.x>Dist_sel,]

############### 
# par(mfrow=c(1,1))
# par(mar=c(5, 4, 4, 2) )
# plot(M_ALL_0.01$Distance[M_ALL_0.01$Distance<Dist_sel], M_ALL_0.01$KendalT.value[M_ALL_0.01$Distance<230],col=rgb(0,0,1,1/4),xlab='Distance [km]',
#      ylab="Kendall's tau",pch=19, cex.lab=1.2, cex.axis=1.2,  cex.sub=1.2, ylim=c(0,1), xlim=c(0,600),main=paste0("Lag time=",names(ALL_LAG[i])))
# 
# points(M_ALL_0.01$Distance[M_ALL_0.01$Distance>=Dist_sel], M_ALL_0.01$KendalT.value[M_ALL_0.01$Distance>=230],col=rgb(1,0,0,1/4),xlab='Distance [km]',
#        ylab="Kendall's tau",pch=19, cex.lab=1.2, cex.axis=1.2,  cex.sub=1.2, ylim=c(0,1))
# 
# 
# lines(predicted,lwd=2,col='deepskyblue4')
# abline(v =230, untf = FALSE, col="darkgrey",lty = "dashed",lwd=2)
# 
# polygon(xx,yy,col = rgb(0.8,0.8,0.8,0.5), border = FALSE)
# 


##Code for increasing size: cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5

################## 4. Mirrored Histogram: Near and Far couple ########################################


par(mfrow=c(2,1)) 
par(mar=c(0,5,3,3))
hist(M_near_01_POS$N.SY.N.ALL, col=rgb(0,0,1,1/4) ,main=paste0("Lag time=",names(ALL_LAG[i])),xaxt="n", xlab="", xlim=c(0,1),cex.lab=1.2)
legend("topright", legend=c("Near","Far"), col=c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)), pt.cex=2, pch=15, cex=1.2)  
#hist(M_far_01_POS$N.SY.N.ALL, add=T,col=rgb(1,0,0,1/4),cex.lab=1.3)

par(mar=c(5,5,0,3))
hist(M_far_01_POS$N.SY.N.ALL,col=rgb(1,0,0,1/4), xlab="Syn", ylim=c(100,0),main="", xlim=c(0,1), ylab="",cex.lab=1.2) 


}
dev.off()

######################histo

###############  2. Correlogram     ###################################
#### CORRELOGRAM UPDATED AFTER FIRST REVISION ############

setwd("C:/Users/39349/Documents/Regional/Lag time/correlogram")

for (i in 1:length(Lag_time))
{
 ##  FIRST ROW: Density function with Legend
M_ALL<-ALL_LAG[[i]]

GG_1<-ggplot(data=M_ALL, aes(x=KendalT.value,group=as.factor(X), fill=as.factor(X)) )+
  geom_histogram(aes(y=0.1*..density..),
                 alpha=0.5,position='identity',binwidth=0.1) + theme_bw()+ 
  scale_fill_manual(values=c("#FF3333", "#33CCCC"))+ labs(x = "Kendall's tau", y="Density")+
  theme(legend.position = "none")


GG_11<- GG_1 + scale_x_continuous(
  labels = scales::number_format(accuracy = 0.01,
                                 decimal.mark = '.'), limits = c(-1,1))

GG_2<-ggplot(data=M_ALL, aes(x=N.SY.N.ALL, group=as.factor(X), fill=as.factor(X)) )+
  geom_histogram(aes(y=0.08*..density..),
                 alpha=0.5,position='identity',binwidth=0.08) + theme_bw()+ 
  scale_fill_manual(values=c("#FF3333", "#33CCCC"))+ labs(x =  "Syn", y="Density")+ 
  theme(legend.position = "none")

GG_22<- GG_2 + scale_x_continuous(
  labels = scales::number_format(accuracy = 0.01,
                                 decimal.mark = '.'), limits = c(-0.1,1.1), breaks = c(0,0.25,0.50,0.75,1))


M_ALL$XG<-ifelse(M_ALL$KendalT.p.value<=0.01,"Dependent","Independent")


cols <- c("Dependent"= "#FF3333", "Independent"= "#33CCCC")


GG_3<-ggplot(data=M_ALL, aes(x=Distance, group=XG, fill=XG ))+
  geom_histogram(aes(y=6*..density..),
                 alpha=0.5,position='identity',binwidth=6) + theme_bw()+ 
  scale_fill_manual(values=c( "#33CCCC", "#FF3333"))+ labs(x = "Distance [km]", y="Density")+
  theme(legend.position = c(0.8, 0.5),legend.title = element_blank(),legend.text=element_text(size=10))




GG_4v2<-ggplot(data=M_ALL ,aes(x=KendalT.value,y=N.SY.N.ALL,color = as.factor(X)))+geom_point(aes(shape=as.factor(X)),alpha = 0.3, size=0.9) + theme_bw()+ 
  scale_fill_manual(values=c("#FF3333", "#33CCCC"))+ labs(x = "Kendall's tau", y="Syn")+
  scale_size_manual(values=c(0.1,0.8))+
theme(legend.position = "none")


GG_44v2<-GG_4v2 + scale_x_continuous(
  labels = scales::number_format(accuracy = 0.01,
                                 decimal.mark = '.'), limits = c(-1,1))

GG_5v2<-ggplot(data=M_ALL ,aes(x=Distance,y= N.SY.N.ALL,color = as.factor(X)))+geom_point(aes(shape=as.factor(X)),alpha = 0.3, size=0.9) + theme_bw()+ 
  scale_fill_manual(values=c("#FF3333", "#33CCCC"))+ labs(x = "Distance [km]", y="Syn") + 
  theme(legend.position = "none")


GG_6v2<-ggplot(data=M_ALL ,aes(x=Distance,y= KendalT.value,color = as.factor(X)))+geom_point(aes(shape=as.factor(X)),alpha = 0.3, size=0.9) + theme_bw()+ 
  scale_fill_manual(values=c("#FF3333", "#33CCCC"))+ labs(x = "Distance [km]", y="Kendall's tau")+ 
  theme(legend.position = "none")


GG_66v2<-GG_6v2 + scale_x_continuous(
  labels = scales::number_format(accuracy = 0.01,
                                 decimal.mark = '.'))+ scale_y_continuous(limits=c(-1,1))


GG_correl_v2<-ggarrange(GG_11,GG_22,GG_3,GG_44v2,GG_5v2,GG_66v2,ncol=3,nrow=2, labels=c("a)","b)","c)","d)","e)","f)") ,
                         font.label = list(size = 12)) 



ggsave(paste0("Correlog_LAGTIME_",names(ALL_LAG[i]),".jpeg"), GG_correl_v2,units="in",dpi=400, height=7,width =12)

}

dev.off()


##################################################################################################################################

final_length_depdep<-list()
final_length_depind<-list()
final_length_Dep<-list()
final_length_Ind<-list()

pdf("BOXPLOT_LAG_comb1.pdf")

for (i in 1:length(Lag_time))
{
  ##  FIRST ROW: Density function with Legend
  M_ALL_0.01_POS_perc<-ALL_LAG_POS[[i]]
  M_ALL_IND_perc0.01<-ALL_LAG_IND[[i]]
  
  
  M_ALL_0.01_POS_perc$Class<-NA        
  M_ALL_0.01_POS_perc$Class[M_ALL_0.01_POS_perc$N.SY.N.ALL>=0.60]<-"High Syn"
  M_ALL_0.01_POS_perc$X.1[M_ALL_0.01_POS_perc$N.SY.N.ALL>=0.60]<-c(3)
  M_ALL_0.01_POS_perc$Class[M_ALL_0.01_POS_perc$X.1==1]<-"Depind"
  M_ALL_0.01_POS_perc$Class[M_ALL_0.01_POS_perc$X.1==2]<-"Depdep"
  #Depdep-DepInd
  
  
  ###########################CHOOSE INITIAL MATRIX#################################
  
  ## DEPENDENT COUPLES
  Var_1<-c("N.SY.N.ALL","BFIHOST....", "SAAR_61.90","KendalT.value", "Class",        
           "X.1","Distance.x","KT_pvalue_Asy", "Num_Syncr_occ", "ID_Station_1","ID_Station_2" )
  
  
  M1_DEP_0<-M_ALL_0.01_POS_perc[,Var_1]
  
  colnames(M1_DEP_0)<-c("Meteo","Hydrology","Climatology","KendallTau","Class","X.1","Distance","Asypvalue","Num Syn","Station1","Station2")
  
  
  ## DEPENDENT COUPLES
  M1_IND_0<-M_ALL_IND_perc0.01[,Var_1]
  
  colnames(M1_IND_0)<-c("Meteo","Hydrology","Climatology","KendallTau","Class","X.1","Distance","Asypvalue","Num Syn","Station1","Station2")
  
  
  
  ########## Choose Meterological Index  ############
  #Number of syncrony /N all
  M1_DEP_0[,1]<- 1-M1_DEP_0[,1]  
  M1_IND_0[,1]<- 1-M1_IND_0[,1]  
  
  ######## Choose Climatological Index
  # SAAR
  
  M1_DEP_0[,3]<- M_ALL_0.01_POS_perc$SAAR_61.90
  M1_IND_0[,3]<- M_ALL_IND_perc0.01$SAAR_61.90
  
  
  Final_D<-c("X50.Altitude","BFIHOST....","SPRHOST","LDP..km.", "Mountain.heath.bog" , 
             "Arable.horticultural","Catchment.area","PROPWET")
  
  
  M1_DEP_0[,2]<- apply(M_ALL_0.01_POS_perc[,Final_D],1,mean)   
  M1_IND_0[,2]<- apply(M_ALL_IND_perc0.01[,Final_D],1,mean)   
  
  ############# Not consider NA in Hydro Index ##################################
  
  M1_DEP<-M1_DEP_0[complete.cases(M1_DEP_0[,1:3]),]
  M1_IND<-M1_IND_0[complete.cases(M1_IND_0[,1:3]),]

   par(mfrow=c(2,3))

   boxplot(M1_DEP$Meteo[M1_DEP$Class=="Depdep"],M1_DEP$Meteo[M1_DEP$Class=="Depind"],M1_DEP$Meteo[M1_DEP$Class=="High Syn"]
           ,names=c("Dep-Dep","Dep-Ind","High Syn"),ylim=c(0,1),outline=FALSE, cex.main=1.5,cex.axis=1.3,
           cex.lab=1.5,ylab="Index", main=paste0("Lag time:", names(ALL_LAG_IND[i])))

   title("a) Asyncrony", adj =0.97, line = -1)

   boxplot(M1_DEP$Climatology[M1_DEP$Class=="Depdep"],M1_DEP$Climatology[M1_DEP$Class=="Depind"],M1_DEP$Climatology[M1_DEP$Class=="High Syn"]
           ,names=c("Dep-Dep","Dep-Ind","High Syn"),ylim=c(0,1),outline=FALSE, cex.main=1.5,cex.axis=1.3,
           cex.lab=1.5, ylab="Dissimilarity Index")
   title("b) Climatology", adj =0.97, line = -1)

   boxplot(M1_DEP$Hydrology[M1_DEP$Class=="Depdep"],M1_DEP$Hydrology[M1_DEP$Class=="Depind"],M1_DEP$Hydrology[M1_DEP$Class=="High Syn"]
           ,names=c("Dep-Dep","Dep-Ind","High Syn"),ylim=c(0,1),outline=FALSE, cex.main=1.5,cex.axis=1.3,
           cex.lab=1.5, ylab="Dissimilarity Index")

   title("c) Hydrology", adj =0.97, line = -1)

   boxplot(M1_DEP$Distance[which(M1_DEP$Class=="Depdep")],M1_DEP$Distance[which(M1_DEP$Class=="Depind")],M1_DEP$Distance[which(M1_DEP$Class=="High Syn")],
           names=c("Dep-Dep","Dep-Ind","High Syn"),outline=FALSE, cex.main=1.5,cex.axis=1.3,
           cex.lab=1.5, ylab="Distance")
   title("d)", adj =0.03, line = -1)

   boxplot(M1_DEP$KendallTau[M1_DEP$Class=="Depdep"],M1_DEP$KendallTau[M1_DEP$Class=="Depind"],M1_DEP$KendallTau[M1_DEP$Class=="High Syn"]
           ,names=c("Dep-Dep","Dep-Ind","High Syn"),ylim=c(0,1),outline=FALSE, cex.main=1.5,cex.axis=1.3,
           cex.lab=1.5, ylab="Kendall's Tau")
  title("e)", adj =0.03, line = -1)


  final_length_depdep[i]<-nrow(M1_DEP[which(M1_DEP$Class=="Depdep"),])
  final_length_depind[i]<-nrow(M1_DEP[which(M1_DEP$Class=="Depind"),])
  final_length_Dep[i]<-nrow(M1_DEP)
  final_length_Ind[i]<-nrow(M1_IND)

#  # Dependent- Independent
#   par(mfrow=c(2,2))
# 
#  # boxplot(M1_DEP$Distance[which(M1_DEP$Class=="Depdep")],M1_DEP$Distance[which(M1_DEP$Class=="Depind")],M1_DEP$Distance[which(M1_DEP$Class=="High Syn")],
# #          names=c("Depdep","DepInd","High Syn"), main="Distance Groups")
# 
#   boxplot(M1_DEP$Meteo,M1_IND$Meteo
#           ,names=c("Dependent","Independent"), main=paste("LAG",names(ALL_LAG_IND[i]),": Asyncrony"))
# 
#   boxplot(M1_DEP$Climatology,M1_IND$Climatology
#           ,names=c("Dependent","Independent"), main="ALL: Climatology")
# 
#   boxplot(M1_DEP$Hydrology,M1_IND$Hydrology
#           ,names=c("Dependent","Independent"), main="ALL: Hydrology")
# 
#   nam0<-paste(colnames(M_ALL_0.01_POS_perc[Final_D]), collapse = ' , ')
#   nam<-paste0(nam0, "  Number of points: DEPDEP:",final_length_dep," DEPIND:", final_length_ind)
#   text.p <- ggparagraph(text =nam, face = "italic", size = 12, color = "black")
#   text.p


  
  }
dev.off()

names(final_length_depdep)<-Lag_time
names(final_length_depind)<-Lag_time
names(final_length_Ind)<-Lag_time
names(final_length_Dep)<-Lag_time






pdf(file="LAG TIME comparison2.pdf",width=9, height=9)  

par(mfrow=c(3,1)) 

for (x in 1:length(Lag_time))
{
  ALL_LAG[[x]]
  
  
  HSyn<- length(which(ALL_LAG[[x]]$N.SY.N.ALL>=0.60))
  MediumSyn<- length(which(ALL_LAG[[x]]$N.SY.N.ALL>=0.40 & ALL_LAG[[x]]$N.SY.N.ALL<0.60))
  LowSyn<-length(which(ALL_LAG[[x]]$N.SY.N.ALL<0.40))
  
  
  slices <- c(HSyn, MediumSyn,LowSyn)
  lbls <- c(paste0("High Syn(>=60%): ",HSyn), paste0("Medium Syn(>=40% and <60%): ",MediumSyn), paste0("Low Syn(<40%): ",LowSyn))
  pie(slices, labels = lbls, main=paste("Dependent Couples: Lag time ",names(ALL_LAG[x])))
  
}
  dev.off()

  par(mfrow=c(3,1)) 
  
  for (x in 1:length(Lag_time))
  {  
    
hist(ALL_LAG[[x]]$Distance.x[ALL_LAG[[x]]$X.1==2],col="red", xlab=c("Distance"), main=paste("Lag time",names(ALL_LAG[x])))
hist(ALL_LAG[[x]]$Distance.x[ALL_LAG[[x]]$X.1==1],add=T,col="blue") 
legend("topright", legend=c("DepDep","DepInd"), col=c("red","blue"), pt.cex=2, pch=15, cex=1.2)  
  
  }

  pdf(file="AREA.pdf",width=9, height=9) 
  
  par(mfrow=c(2,1))   
  for (x in 1:length(Lag_time))
  {  
    
    hist(ALL_LAG[[x]]$Area_1[ALL_LAG[[x]]$X.1==2],col="red", xlab=c("Area"), main=paste("Lag time",names(ALL_LAG[x])))
    hist(ALL_LAG[[x]]$Area_1[ALL_LAG[[x]]$X.1==1],col="blue") 
    legend("topright", legend=c("DepDep","DepInd"), col=c("red","blue"), pt.cex=2, pch=15, cex=1.2)  
    
  }
  
  dev.off()
  
  
##BOXPLOT DISTANCE
  
  par(mfrow=c(1,3))
 
   for (x in 1:length(Lag_time))
  {  
  
     boxplot(ALL_LAG[[x]]$Distance.x[ALL_LAG[[x]]$X.1==2],ALL_LAG[[x]]$Distance.x[ALL_LAG[[x]]$X.1==1],ALL_LAG[[x]]$Distance.x[ALL_LAG[[x]]$N.SY.N.ALL>=0.60]
             ,names=c("DepDep","DepInd","High Syn"), main=paste("Distance Lag time",names(ALL_LAG[x])),outline=FALSE,ylim=c(0,500))
   }

  
    ##BOXPLOT AREA
  
  par(mfrow=c(1,3))
  
  for (x in 1:length(Lag_time))
  {  
    
    boxplot(ALL_LAG[[x]]$Area_1[ALL_LAG[[x]]$X.1==2],ALL_LAG[[x]]$Area_1[ALL_LAG[[x]]$X.1==1],ALL_LAG[[x]]$Area_1[ALL_LAG[[x]]$N.SY.N.ALL>=0.60]
            ,names=c("DepDep","DepInd","High Syn"), main=paste("AREA Lag time",names(ALL_LAG[x])),outline=FALSE,ylim=c(0,3600))
  }  
  
  ##BOXPLOT AREA
  
  par(mfrow=c(1,3))
  
    
    boxplot(ALL_LAG[[1]]$Area_1[ALL_LAG[[1]]$X.1==2],ALL_LAG[[2]]$Area_1[ALL_LAG[[2]]$X.1==2],ALL_LAG[[3]]$Area_1[ALL_LAG[[3]]$X.1==2]
            ,names=c("lag 5","lag 7","lag 10"), main=c("AREA DEPDEP"),col=c("blue","green","red"),outline=FALSE,ylim=c(0,3600))

    boxplot(ALL_LAG[[1]]$Area_1[ALL_LAG[[1]]$X.1==1],ALL_LAG[[2]]$Area_1[ALL_LAG[[2]]$X.1==1],ALL_LAG[[3]]$Area_1[ALL_LAG[[3]]$X.1==1]
            ,names=c("lag 5","lag 7","lag 10"), main=c("AREA DEPIND"),col=c("blue","green","red"),outline=FALSE,ylim=c(0,3600))
  
    boxplot(ALL_LAG[[1]]$Area_1[ALL_LAG[[1]]$N.SY.N.ALL>=0.60],ALL_LAG[[2]]$Area_1[ALL_LAG[[2]]$N.SY.N.ALL>=0.60],ALL_LAG[[3]]$Area_1[ALL_LAG[[3]]$N.SY.N.ALL>=0.60]
            ,names=c("lag 5","lag 7","lag 10"), main=c("AREA HIGH SYN"),col=c("blue","green","red"),outline=FALSE,ylim=c(0,3600))
    
  
############

Table<-data.frame(matrix (,nrow=length(Lag_time),ncol=5))

colnames(Table)<-c("Asy>20","Median ASYSY","Dep-Dep","Dep-Ind","High Syn")
rownames(Table)<-names(ALL_LAG)


for (x in 1:length(Lag_time))
{
Table$`Asy>20`[x]<-length(which(ALL_LAG[[x]]$Num_Asyncr_occ>=20))
Table$`Median ASYSY`<-mean(ALL_LAG[[x]]$N.SY.N.ALL[ALL_LAG[[x]]$Num_Asyncr_occ<20])
Table$`Dep-Dep`[x]<-length(which(ALL_LAG[[x]]$X.1==2))
Table$`Dep-Ind`[x]<-length(which(ALL_LAG[[x]]$X.1==1))  
Table$`High Syn`[x]<-length(which(ALL_LAG[[x]]$N.SY.N.ALL>=0.60))  }
  
par(mfrow=c(1,3))

for (x in 1:length(Lag_time))
{    
hist(ALL_LAG[[x]]$N.SY.N.ALL,xlab=c("N Sy/N data"), main=paste("Lag time",names(ALL_LAG[x])))
} 

########## FAR AND NEAR

Table2<-data.frame(matrix (,nrow=length(Lag_time),ncol=4))

colnames(Table2)<-c("NEAR_DEPDEP","FAR_DEPDEP","NEAR_DEPIND","FAR_DEPIND")
rownames(Table2)<-names(ALL_LAG)

for (x in 1:length(Lag_time))
{
  Table2$NEAR_DEPDEP[x]<-length(which(ALL_LAG[[x]]$X.1==2 & ALL_LAG[[x]]$Distance.x<=230 ))
  Table2$FAR_DEPDEP[x]<-length(which(ALL_LAG[[x]]$X.1==2 & ALL_LAG[[x]]$Distance.x>230 ))
  Table2$NEAR_DEPIND[x]<-length(which(ALL_LAG[[x]]$X.1==1 & ALL_LAG[[x]]$Distance.x<=230 ))
  Table2$FAR_DEPIND[x]<-length(which(ALL_LAG[[x]]$X.1==1 & ALL_LAG[[x]]$Distance.x>230 ))
}



#######################
par(mfrow=c(1,3))

for(x in 1:3){
  
  slices <- c(Table2$NEAR_DEPDEP[x], Table2$FAR_DEPDEP[x],Table2$NEAR_DEPIND[x],Table2$FAR_DEPIND[x])
  lbls <- colnames(Table2)
  pie(slices, labels = lbls, main=paste("Lag time ",names(ALL_LAG[x]))) 
}


#####################
library(reshape2)
library(ggplot2)


#CASO 1

Table2$LAG<-Lag_time
#Convert to long format
d = melt(Table2, id.vars = "LAG")

ggplot(data = d,
       mapping = aes(x = LAG, y = value, fill = variable)) + 
  geom_bar(position="fill", stat="identity")

ggsave("BARPLOT LAGTIME.jpeg", units="in", dpi=400, width=7.28,height=7.76)

#CASO 2
ggplot(data = d,
       mapping = aes(x = LAG, y = value, fill = variable))+
  geom_col(position = position_dodge())
ggsave("BARPLOT LAGTIME2.jpeg", units="in", dpi=400, width=8.28,height=7.76)

#CASO 3
ggplot(data = d,
       mapping = aes(x = variable, y = value, fill = LAG ))+
  geom_col(position = position_dodge())
ggsave("BARPLOT LAGTIME3.jpeg", units="in", dpi=400, width=8.28,height=7.76)

