###########  PAPER QQ ########
#### CODE FOR THE GRAPH ##########
######Author: Cristina Deidda ##########

##### Enjoy :) #############
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


load("C:/Users/39349/Documents/Regional/Workspace/M_ALL_perc.RData")
#load("C:/Users/39349/Documents/Regional/Distance_selection_workspace.RData")
#load("C:/Users/39349/Documents/Regional/M_ALL.RData")
#
############# Load all the data ##########################
#
#setwd("D:/PROJECTS/Regional/DISTANCE_selection/Data")
#M_ALL<- read.table("ELAB_UK_CORR_LAG10_20_Rformat_3008.csv", header = TRUE, sep=";")     #1 =IND, 2=DEP
#M_ALL<- read.table("ELAB_UK_CORR_LAG10_20_Rformat_3008_noempty.csv", header = TRUE, sep=";")
#
#M_ALL$Distance<- M_ALL$Distance/1000 
#
#CC<-which(is.na(M_ALL[,1])==FALSE)
#M_ALL<- M_ALL[CC,]
#
#Wrongst<- which(M_ALL$ID_Station_1=="39004" | M_ALL$ID_Station_2=="39004" )
#
#M_ALL<- M_ALL[-Wrongst,]
#


load("C:/Users/39349/Documents/Regional/Workspace/ALLDATA01_andonlyPOSITIVE.RData")
load("C:/Users/39349/Documents/Regional/Workspace/M_ALL_INDIPENDENT_perc.RData")
load("C:/Users/39349/Documents/Regional/Workspace/M_ALL_001.RData")

##############################
####################          PLOT CODE            ##########################                      
                       ###########################
##########################KT AND MAP UK #################################                       
# 1.1. plot: KT e P value 

###### 1. Kendall Tau- P value with Subplot   #############
M_ALL_0.01<- M_ALL[M_ALL$KendalT.p.value<=0.01,]

op <- par(no.readonly = TRUE)
set.seed(42)

plot(M_ALL[,4], M_ALL[,5], xlab=c("Kendall's tau"), ylab=c("p-value"), col="blue", cex.lab=1.2, cex.axis=1.2,  cex.sub=1.2) 
abline(h =0.01, untf = FALSE, col="red",lty = "dashed",lwd=2) 

par(new=TRUE, oma=c(12,2,1,1))
layout(matrix(1:2,1))
plot(M_ALL_0.01[,4], M_ALL_0.01[,5], xlab=c("Kendall's tau"), ylab=c("p-value"), col="blue") 
abline(h =0.01, untf = FALSE, col="red",lty = "dashed",lwd=2) 

par(op)

######### 1.2 UK MAP CODE##############################


UK <- map_data(map = "world", region = "UK")

df<-data.frame(M_ALL_0.01$Northing_1,M_ALL_0.01$Easting_1)
colnames(df)<-c("Northing_1","Easting_1")

coord_point<-df %>%
  st_as_sf(coords = c("Easting_1", "Northing_1"), crs = 27700) %>%
  st_transform(4326) %>%
  st_coordinates() %>%
  as_tibble()

UK_map<-ggplot() + geom_polygon(data = UK, aes(x = long, y = lat, group = group),fill="white", colour = "black") +
  coord_map()+geom_point(data = coord_point,  aes(x = X, y = Y, group = NULL), colour = "black", size = 1.2) 

UK_map<-UK_map+ theme(panel.background = element_rect(fill = 'white', colour = 'black'))

##################1.3 KT_pvalue + UK map ##########################

Plot_KT<-"C:/Users/39349/Documents/Regional/Plot/Plot_01_KT_mod.jpeg"
#Plot_KT<-"C:/Users/39349/Documents/Regional/Plot/01.KT-pvalue1.jpeg"

ggdraw()+draw_image(Plot_KT)+ draw_plot(UK_map, x = 0, y = .45, width = .4, height = .4)

###############################################################################

###############  2. Correlogram     ###################################
#old
g2<-ggpairs(M_ALL, legend=1,columns = c(4,15,25), aes(color = as.factor(X)),  lower = list(continuous = wrap("points", alpha = 0.3, size=0.9)),
        diag = list(discrete="densityDiag",
                    continuous = wrap("densityDiag", alpha=0.5 )), 
        upper = list(combo = wrap("box_no_facet", alpha=0.5),
                     continuous = wrap("cor", size=4, alignPercent=0.8))  ) 
                     
 print(g2 + theme(text = element_text(size = 15, lineheight = 1))) #For increasing size
 
#new
 M_ggpair<-M_ALL[,c(4,15,25)]
 colnames(M_ALL)[c(4,15,25)]<-c("Kendall's Tau", "Syn", "Distance")
 
 g3<-ggpairs(M_ALL ,columns = c(4,15,25),legend=1, aes(color = as.factor(X)),  lower = list(continuous = wrap("points", alpha = 0.3, size=0.9)),
             diag = list(discrete="densityDiag",
                         continuous = wrap("densityDiag", alpha=0.5 )), 
             upper = "blank"  ) + theme_bw()
 g3
 print(g3 + theme(text = element_text(size = 15, lineheight = 1))) #For increasing size
 
 #############CORRECT ONE  ###########################
 GG<-ggpairs(M_ALL ,columns = c(4,15,25), aes(color = as.factor(X)),  lower = list(continuous = wrap("points", alpha = 0.3, size=0.9), axisLabels = "internal"),
             diag = list(discrete="densityDiag",
                         continuous = wrap("densityDiag", alpha=0.5 )), 
             upper = "blank"  ) + theme_bw()
 
 GG[1,1]<-ggplot(data=M_ALL, aes(x=M_ALL[,4],group=as.factor(X), fill=as.factor(X)) )+
   geom_histogram(aes(y=0.1*..density..),
                  alpha=0.5,position='identity',binwidth=0.1) + theme_bw()+ 
   scale_fill_manual(values=c("#FF3333", "#33CCCC"))
 
 GG[2,2]<-ggplot(data=M_ALL, aes(x=M_ALL[,15], group=as.factor(X), fill=as.factor(X)) )+
   geom_histogram(aes(y=0.1*..density..),
                  alpha=0.5,position='identity',binwidth=0.1) + theme_bw()+ 
   scale_fill_manual(values=c("#FF3333", "#33CCCC"))
 
 GG[3,3]<-ggplot(data=M_ALL, aes(x=M_ALL[,25], group=as.factor(X), fill=as.factor(X)) )+
   geom_histogram(aes(y=6*..density..),
                  alpha=0.5,position='identity',binwidth=6) + theme_bw()+ 
   scale_fill_manual(values=c("#FF3333", "#33CCCC"))
 
 GG
 
#########UPDATE########################################àà 

 

GG<-ggpairs(M_ALL ,columns = c(4,15,25), aes(color = as.factor(X)),  lower = list(continuous = wrap("points", alpha = 0.3, size=0.9)),
            diag = list(discrete="densityDiag",
                        continuous = wrap("densityDiag", alpha=0.5 ),
                        axisLabels = "show" ), 
            upper = "blank"  ) + theme_bw()

GG_1<-ggplot(data=M_ALL, aes(x=M_ALL[,4],group=as.factor(X), fill=as.factor(X)) )+
  geom_histogram(aes(y=0.1*..density..),
                 alpha=0.5,position='identity',binwidth=0.1) + theme_bw()+ 
  scale_fill_manual(values=c("#FF3333", "#33CCCC"))+ labs(x = "Kendall's tau", y="Density")+
  theme(legend.position = "none")
#0.03

GG_11<- GG_1 + scale_x_continuous(
  labels = scales::number_format(accuracy = 0.01,
                                 decimal.mark = '.'), limits = c(-1,1))
GG_11
GG_2<-ggplot(data=M_ALL, aes(x=M_ALL[,15], group=as.factor(X), fill=as.factor(X)) )+
  geom_histogram(aes(y=0.08*..density..),
                 alpha=0.5,position='identity',binwidth=0.08) + theme_bw()+ 
  scale_fill_manual(values=c("#FF3333", "#33CCCC"))+ labs(x =  "Syn", y="Density")+ 
  theme(legend.position = "none")

#0.04
GG_22<- GG_2 + scale_x_continuous(
  labels = scales::number_format(accuracy = 0.01,
                                 decimal.mark = '.'), limits = c(-0.1,1.1), breaks = c(0,0.25,0.50,0.75,1))


GG_3<-ggplot(data=M_ALL, aes(x=M_ALL[,25], group=as.factor(X), fill=as.factor(X)) )+
  geom_histogram(aes(y=6*..density..),
                 alpha=0.5,position='identity',binwidth=6) + theme_bw()+ 
  scale_fill_manual(values=c("#FF3333", "#33CCCC"))+ labs(x = "Distance [km]", y="Density")+
  theme(legend.position = "none")


GG_4<-ggplot(data=M_ALL ,aes(x=M_ALL[,4],y= M_ALL[,15],color = as.factor(X)))+geom_point(alpha = 0.3, size=0.9) + theme_bw()+ 
      scale_fill_manual(values=c("#FF3333", "#33CCCC"))+ labs(x = "Kendall's tau", y="Syn")+  
      theme(legend.position = "none")
GG_44<-GG_4 + scale_x_continuous(
  labels = scales::number_format(accuracy = 0.01,
                                 decimal.mark = '.'), limits = c(-1,1))

GG_5<-ggplot(data=M_ALL ,aes(x=M_ALL[,15],y= M_ALL[,25],color = as.factor(X)))+geom_point(alpha = 0.3, size=0.9) + theme_bw()+ 
  scale_fill_manual(values=c("#FF3333", "#33CCCC"))+ labs(x = "Syn", y="Distance [km]") + 
  theme(legend.position = "none")

GG_55<-GG_5 + scale_x_continuous(
  labels = scales::number_format(accuracy = 0.01,
                                 decimal.mark = '.'), limits = c(-0.1,1.1), breaks = c(0,0.25,0.50,0.75,1))

GG_6<-ggplot(data=M_ALL ,aes(x=M_ALL[,4],y= M_ALL[,25],color = as.factor(X)))+geom_point(alpha = 0.3, size=0.9) + theme_bw()+ 
  scale_fill_manual(values=c("#FF3333", "#33CCCC"))+ labs(x = "Kendall's tau", y="Distance [km]")+ 
  theme(legend.position = "none")

GG_66<-GG_6 + scale_x_continuous(
  labels = scales::number_format(accuracy = 0.01,
                                 decimal.mark = '.'), limits = c(-1,1))

#GG_update<-ggarrange(GG_11,GG_22,GG_3,GG_44,GG_55,GG_66,ncol=3,nrow=2) 

GG_update<-ggarrange(GG_11,GG_22,GG_3,GG_44,GG_55,GG_66,ncol=3,nrow=2, labels=c("a)","b)","c)","d)","e)","f)") ,font.label = list(size = 13)) 
GG_update


#
########### 3. Kendall Tau and Distance with regression line ###########
#M_ALL_0.01<-M_ALL_0.01_POS
 
 
gh<-which( M_ALL_0.01$KendalT.value>0 )

#Regression for Only positive
q1<-M_ALL_0.01$Distance[gh]
y<- M_ALL_0.01$KendalT.value[gh]

#Regression for All
#q1<-M_ALL_0.01$Distance
#y<- M_ALL_0.01$KendalT.value


model <- lm(y ~ poly(q1,4))
predicted.intervals <- predict(model,data.frame(x=q1),interval='confidence', level=0.99)

plot(M_ALL_0.01$Distance, M_ALL_0.01$KendalT.value,col='cyan3',xlab='Distance [km]',ylab="Kendall's tau",pch=20, cex.lab=1.2, cex.axis=1.2,  cex.sub=1.2, ylim=c(0,1))

points(q1,predicted.intervals[,1],col='deepskyblue4',lwd=0.3,pch=20)

#lines(q1,predicted.intervals[,1],col='deepskyblue4',lwd=0.3,pch=20, type = "b")
#lines(sort(q1), sort(predicted.intervals[,1]), xlim=range(q1), ylim=range(predicted.intervals[,1]), pch=16)


#legend("topright",c("Fitted","Dependent"),col=c("deepskyblue4","cyan3"), lwd=c(3,NA,NA), pch=c(NA,20,20),cex=1.2 )
abline(v =230, untf = FALSE, col="darkgrey",lty = "dashed",lwd=2)

####################new########

predicted<-cbind(q1,predicted.intervals[,1])
require(data.table)
predicted<- data.table(predicted, key="q1")

plot(M_ALL_0.01$Distance, M_ALL_0.01$KendalT.value,col='cyan3',xlab='Distance [km]',ylab="Kendall's tau",pch=20, cex.lab=1.2, cex.axis=1.2,  cex.sub=1.2, ylim=c(0,1))

lines(predicted,lwd=4,col='deepskyblue4')
abline(v =230, untf = FALSE, col="darkgrey",lty = "dashed",lwd=2)





ggsave("C:/Users/39349/Documents/Regional/Plot/01.POS_KTDIST.jpeg", units="in", dpi=400)

##Code for increasing size: cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5

################## 4. Mirrored Histogram: Near and Far couple ########################################

par(mfrow=c(2,1)) 
par(mar=c(0,5,3,3))
hist(M_near_01_POS$N.SY.N.ALL, col=rgb(0,0,1,1/4) ,main="",xaxt="n", xlab="", xlim=c(0,1),cex.lab=1.2)
#legend("topright", legend=c("Near","Far"), col=c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)), pt.cex=2, pch=15, cex=1.2)  
#hist(M_far_01_POS$N.SY.N.ALL, add=T,col=rgb(1,0,0,1/4),cex.lab=1.3)

par(mar=c(5,5,0,3))
hist(M_far_01_POS$N.SY.N.ALL,col=rgb(1,0,0,1/4), xlab="Syn", ylim=c(100,0),main="", xlim=c(0,1), ylab="",cex.lab=1.2) 


##### 5. Pixel map ############
# See the code in the directory: 
#D:\PROJECTS\Regional\DISTANCE_selection\Asy_analysis\DD GRAPH\Median value\Pixel map\ Code_PixelMAP. R 

#M_ALL_perc<-rbind(Near_perc, Far_perc)

#M_ALL_perc$Class[M_ALL_perc$KendalT.value>0]<-"Positive"
#M_ALL_perc$Class[M_ALL_perc$KendalT.value<=0]<-"Negative"
#M_ALL_perc$Class[M_ALL_perc$N.SY.N.ALL>=0.60]<-"High Syncrony"

###########################CHOOSE INITIAL MATRIX#################################

## CHOSE POSITIVE COUPLES
M1<-M_ALL_0.01_POS_perc[,c(15,57,69,4,68,18,25)]

colnames(M1)<-c("Meteo","Hydrology","Climatology","KendallTau","Class","X.1","Distance")

########## Choose Meterological Index  ############
#Number of syncrony /N all
M1[,1]<- 1-M1[,1]  
  
######## Choose Hydrological Index ###############
# Mean BFHOST and MAX ALTITUDE
M1[,2]<- apply(M_ALL_0.01_POS_perc[,c(57,50,56,58,60,52:55)],1,max)   


######## Choose Climatological Index
# SAAR

M1[,3]<- M_ALL_0.01_POS_perc[,69]

####### Median SAAR e PROPWET   ###################
#M1[,3]<- apply(M_ALL[,c(69,64)],1,mean)


                   #########################
################### CODE FOR DD PIXEL MAPS##############
                       ################ 


# Select Pixel size####

l_p<-0.05 # Size pixel

Gridx<-seq(l_p,1,l_p)
num_int<-length(Gridx)


######## Meteo-Hydrological Index ##############

M_grid1<-matrix(data=NA,nrow=num_int,ncol=num_int)


colnames(M_grid1)<-Gridx
rownames(M_grid1)<-Gridx


y0<-0
x0<-0




for (xx in 1:num_int)
{
  x1<-x0
  x2<-x1+l_p

for(i in 1:num_int)
{
  y1<-y0
  y2<-y1+l_p
  
  CC<-which(M1$Meteo<x2 & M1$Meteo>=x1 & M1$Hydrology<y2 & M1$Hydrology>=y1)
  
  M_grid1[i,xx]<-mean(M1[CC,4])
  
  y0<-y0+l_p
}
  y0<-0
  x0<-x0+l_p
 
}

coul <- viridis(100)

P1<-levelplot(t(M_grid1), col.regions = coul, main="", xlab="Meteorological Dissimilarity Index",ylab="Hydrological Dissimilarity Index") 
P1

#ggsave('3.DDMAP_Meteo-Clima_POS.png', width =11, height = 8, dpi = 100)

######## Meteo-Climatological Index ##############

M_grid2<-matrix(data=NA,nrow=num_int,ncol=num_int)
#M_grid3<-matrix(data=NA,nrow=num_int,ncol=num_int)

colnames(M_grid2)<-Gridx
rownames(M_grid2)<-Gridx


y0<-0
x0<-0




for (xx in 1:num_int)
{
  x1<-x0
  x2<-x1+l_p
  
  for(i in 1:num_int)
  {
    y1<-y0
    y2<-y1+l_p
    
    CC<-which(M1$Meteo<x2 & M1$Meteo>=x1 & M1$Climatology<y2 & M1$Climatology>=y1)
    
    M_grid2[i,xx]<-mean(M1[CC,4])
    #M_grid3 [i,xx]<-length(CC)
    y0<-y0+l_p
  }
  y0<-0
  x0<-x0+l_p
  
}

coul <- viridis(100)

P2<-levelplot(t(M_grid2), col.regions = coul, main="", xlab="Meteorological Dissimilarity Index",ylab="Climatological Dissimilarity Index") 

P2
######## Clima-Hydrological Index ##############

M_grid3<-matrix(data=NA,nrow=num_int,ncol=num_int)


colnames(M_grid3)<-Gridx
rownames(M_grid3)<-Gridx


y0<-0
x0<-0




for (xx in 1:num_int)
{
  x1<-x0
  x3<-x1+l_p
  
  for(i in 1:num_int)
  {
    y1<-y0
    y3<-y1+l_p
    
    CC<-which(M1$Climatology<x3 & M1$Climatology>=x1 & M1$Hydrology<y3 & M1$Hydrology>=y1)
    
    M_grid3[i,xx]<-mean(M1[CC,4])
    
    y0<-y0+l_p
  }
  y0<-0
  x0<-x0+l_p
  
}

coul <- viridis(100)

P3<-levelplot(t(M_grid3), col.regions = coul, main="", xlab="Climatological Index",ylab="Hydrological Index")
P3
############# pdf PLOT ###########

setwd("C:/Users/39349/Documents/Regional/Plot/DDMAP/1106")

pdf(file=paste("DEPENDENT_VEDIAMOM.pdf"),width=9, height=9, onefile=TRUE, family="Times", title="Trend variance_ALTITUDE",   
    fonts=NULL, version="1.4", paper="special", encoding="default",bg="transparent", fg="black", pointsize=16,                             
    pagecentre=TRUE, colormodel="rgb",useDingbats=TRUE, useKerning=TRUE, fillOddEven=FALSE)   


par(mfrow=c(2,2))

P1
P2
P3

dev.off()


############# THREEE TOGETHER ###########

pdf(file=paste("DEP_3index_maxall.pdf"),width=9, height=9, onefile=TRUE, family="Times", title="Trend variance_ALTITUDE",   
    fonts=NULL, version="1.4", paper="special", encoding="default",bg="transparent", fg="black", pointsize=16,                             
    pagecentre=TRUE, colormodel="rgb",useDingbats=TRUE, useKerning=TRUE, fillOddEven=FALSE)   

M_grid3<-matrix(data=NA,nrow=num_int,ncol=num_int)


colnames(M_grid3)<-Gridx
rownames(M_grid3)<-Gridx


y0<-0
x0<-0




for (xx in 1:num_int)
{
  x1<-x0
  x3<-x1+l_p
  
  for(i in 1:num_int)
  {
    y1<-y0
    y3<-y1+l_p
    
    CC<-which(M1$Climatology<x3 & M1$Climatology>=x1 & M1$Hydrology<y3 & M1$Hydrology>=y1)
    
    M_grid3[i,xx]<-mean(M1[CC,1])
    
    y0<-y0+l_p
  }
  y0<-0
  x0<-x0+l_p
  
}

coul <- viridis(100)

P3<-levelplot(t(M_grid3), col.regions = coul, main="DEPENDENT- together", xlab="Climatological Index",ylab="Hydrological Index")

par(mfrow=c(2,2))

P3
hist(M1$Meteo)

dev.off()


###################### 6. Group in DD maps #################################
 
ggplot(M1,aes(x=Meteo,y=Climatology, colour=Class,shape=Class,size=Distance))+
  geom_point() +
  theme_ipsum( base_size=17,  axis_title_size = 16,  axis_text_size = 15)+ xlim(0, 1) + ylim(0, 1)+ labs(title = "Meteo-Clima")  + scale_color_manual(values=c("deepskyblue4", "blue4", "deepskyblue"))+ guides(shape = guide_legend(override.aes = list(size = 5)))

ggsave('3.Group_Distance_Meteo-Climav0.png', width =11, height = 8, dpi = 100)


ggplot(M1,aes(x=Meteo,y=Hydrology, colour=Class,shape=Class,size=Distance))+
  geom_point() +
  theme_ipsum( base_size=17,  axis_title_size = 16,  axis_text_size = 15)+ xlim(0, 1) + ylim(0, 1)+ labs(title = "Meteo-Hydro")+ scale_color_manual(values=c("deepskyblue4", "blue4", "deepskyblue"))+ guides(shape = guide_legend(override.aes = list(size = 5)))

ggsave('3.Group_Distance_Meteo-Hydrov0.png', width =11, height = 8, dpi = 100)


ggplot(M1,aes(x=Climatology,y=Hydrology, colour=Class,shape=Class,size=Distance))+
    geom_point() +
    theme_ipsum( base_size=17,  axis_title_size = 16,  axis_text_size = 15)+ xlim(0, 1) + ylim(0, 1)+ labs(title = "Clima-Hydro")  + scale_color_manual(values=c("deepskyblue4", "blue4", "deepskyblue")) + guides(shape = guide_legend(override.aes = list(size = 5)))

ggsave('3.Group_Distance_Clima-Hydrov0.png', width =11, height = 8, dpi = 100)

######### SUMMARY ##########
Summary<-matrix(,6,9)

Summary[1,1]<-"All Couple"
Summary[1,2]<-nrow(M_ALL_0.01_POS)
Summary[2,1]<-"Independent"
Summary[2,2]<-nrow(M_ALL_0.01_POS)- nrow(M_ALL_0.01_POS_perc)
Summary[2,3]<-round((nrow(M_ALL_0.01_POS)- nrow(M_ALL_0.01_POS_perc))*100/nrow(M_ALL_0.01_POS),digit=2)
Summary[3,1]<-"Dependent"
Summary[3,2]<-nrow(M_ALL_0.01_POS_perc)
Summary[3,3]<-round(nrow(M_ALL_0.01_POS_perc)*100/nrow(M_ALL_0.01_POS),digit=2)

Summary[4,1]<-"Near couple"
Summary[4,2]<-nrow(Near_perc_POS)
Summary[4,3]<-round(nrow(Near_perc_POS)/nrow(M_ALL_0.01_POS_perc),digit=2)
Summary[4,4]<-round(median(Near_perc_POS$KendalT.value),digit=2)
Summary[4,5]<-round(max(Near_perc_POS$KendalT.value),digit=2)
Summary[4,6]<-round(min(Near_perc_POS$KendalT.value),digit=2)
Summary[4,7]<-round(median(Near_perc_POS$N.SY.N.ALL),digit=2)
Summary[4,8]<-round(max(Near_perc_POS$N.SY.N.ALL),digit=2)
Summary[4,9]<-round(min(Near_perc_POS$N.SY.N.ALL),digit=2)



Summary[5,1]<-"Far couple"
Summary[5,2]<-nrow(Far_perc_POS)
Summary[5,3]<-round(nrow(Far_perc_POS)/nrow(M_ALL_0.01_POS_perc),digit=2)
Summary[5,4]<-round(median(Far_perc_POS$KendalT.value),digit=2)
Summary[5,5]<-round(max(Far_perc_POS$KendalT.value),digit=2)
Summary[5,6]<-round(min(Far_perc_POS$KendalT.value),digit=2)
Summary[5,7]<-round(median(Far_perc_POS$N.SY.N.ALL),digit=2)
Summary[5,8]<-round(max(Far_perc_POS$N.SY.N.ALL),digit=2)
Summary[5,9]<-round(min(Far_perc_POS$N.SY.N.ALL),digit=2)

colnames(Summary)<-c("Type","Number","Percentage","Median KT","Max KT","Min KT","Median SYN","Max SYN","Min SYN")

Hsyn<-M_ALL_0.01_POS[which(M_ALL_0.01_POS$N.SY.N.ALL>=0.60),]

Summary[6,1]<-"High Syncrony couple"
Summary[6,2]<-nrow(Hsyn)
Summary[6,3]<-round(nrow(Hsyn)/nrow(M_ALL_0.01_POS_perc),digit=2)
Summary[6,4]<-round(median(Hsyn$KendalT.value),digit=2)
Summary[6,5]<-round(max(Hsyn$KendalT.value),digit=2)
Summary[6,6]<-round(min(Hsyn$KendalT.value),digit=2)
Summary[6,7]<-round(median(Hsyn$N.SY.N.ALL),digit=2)
Summary[6,8]<-round(max(Hsyn$N.SY.N.ALL),digit=2)
Summary[6,9]<-round(min(Hsyn$N.SY.N.ALL),digit=2)

Mediumsyn<-M_ALL_0.01_POS[which(M_ALL_0.01_POS$N.SY.N.ALL>=0.40 & M_ALL_0.01_POS$N.SY.N.ALL<0.60),]

Summary[6,1]<-"Medium Syncrony couple"
Summary[6,2]<-nrow(Mediumsyn)
Summary[6,3]<-round(nrow(Mediumsyn)/nrow(M_ALL_0.01_POS_perc),digit=2)
Summary[6,4]<-round(median(Mediumsyn$KendalT.value),digit=2)
Summary[6,5]<-round(max(Mediumsyn$KendalT.value),digit=2)
Summary[6,6]<-round(min(Mediumsyn$KendalT.value),digit=2)
Summary[6,7]<-round(median(Mediumsyn$N.SY.N.ALL),digit=2)
Summary[6,8]<-round(max(Mediumsyn$N.SY.N.ALL),digit=2)
Summary[6,9]<-round(min(Mediumsyn$N.SY.N.ALL),digit=2)


#save it

setwd("C:/Users/39349/Documents/Regional/Summary tables")
png("Summary_allvariables.png", height = 50*nrow(Summary), width = 200*ncol(Summary))
grid.table(Summary)
dev.off()


#write.table(Summary, file="Summary.txt")

HSyn<- length(which(M_ALL_0.01_POS$N.SY.N.ALL>=0.60))
MediumSyn<- length(which(M_ALL_0.01_POS$N.SY.N.ALL>=0.40 & M_ALL_0.01_POS$N.SY.N.ALL<0.60))
LowSyn<-length(which(M_ALL_0.01_POS$N.SY.N.ALL<0.40))


slices <- c(HSyn, MediumSyn,LowSyn)
lbls <- c(paste0("High Syn(>=60%): ",HSyn), paste0("Medium Syn(>=40% and <60%): ",MediumSyn), paste0("Low Syn(<40%): ",LowSyn))
pie(slices, labels = lbls, main="Dependent Couples")

load("C:/Users/39349/Documents/Regional/Workspace/Ind_perc.RData")



################## HIGH AND LOW SYNCRONY ###########################
par(mfrow=c(1,2))

BassaSincronia<-M1[which(M1$Meteo>0.8),]
AltaSincronia<-M1[which(M1$Meteo<=0.8),]

boxplot(AltaSincronia$Hydrology,AltaSincronia$Climatology,AltaSincronia$Meteo,
        main="High Syncrony",
        col=c("red","green","blue"),names=c("HYDRO","CLIMA","METEO"),ylim=c(0,1))

boxplot(BassaSincronia$Hydrology,BassaSincronia$Climatology,BassaSincronia$Meteo,
        main="Low Syncrony",
        col=c("red","green","blue"),names=c("HYDRO","CLIMA","METEO"),ylim=c(0,1))

##### HIgh and Low Syn
par(mfrow=c(1,2))

boxplot(AltaSincronia$Hydrology*AltaSincronia$Climatology*AltaSincronia$Meteo,
        main="High Syncrony",
        col=c("red"),names=c("Compound"),ylim=c(0,1))

boxplot(BassaSincronia$Hydrology*BassaSincronia$Climatology*BassaSincronia$Meteo,
        main="Low Syncrony",
        col=c("green"),names=c("Compound"),ylim=c(0,1))

#one for compound

boxplot(BassaSincronia$Hydrology*BassaSincronia$Climatology*BassaSincronia$Meteo,
        AltaSincronia$Hydrology*AltaSincronia$Climatology*AltaSincronia$Meteo,
        main="Product of the three indexis",
        col=c("green","red"),names=c("Low Syn","High Syn"),ylim=c(0,1))


boxplot(BassaSincronia$Hydrology*BassaSincronia$Climatology,
        AltaSincronia$Hydrology*AltaSincronia$Climatology,
        main="Product of the Hydro*Meteo indexis",
        col=c("green","red"),names=c("Low Syn","High Syn"),ylim=c(0,1))


par(mfrow=c(1,2))

boxplot(BassaSincronia$Hydrology,BassaSincronia$Climatology,BassaSincronia$Meteo,
        main="Low Syncrony",
        col=c("red","green","blue"),names=c("HYDRO","CLIMA","METEO"),ylim=c(0,1))




CHECK<-BassaSincronia[which(BassaSincronia$Hydrology>MH),]

boxplot(CHECK$Hydrology,CHECK$Climatology,CHECK$Meteo,
        main="Low Syncrony",
        col=c("red","green","blue"),names=c("HYDRO","CLIMA","METEO"),ylim=c(0,1))

par(mfrow=c(1,2))

boxplot(M1$Hydrology,M1$Climatology,M1$Meteo,
        main="DEPENDENT",
        col=c("red","green","blue"),names=c("HYDRO","CLIMA","METEO"),ylim=c(0,1))

boxplot(M_IND$Hydrology,M_IND$Climatology,M_IND$Meteo,
        main="INDEPENDENT",
        col=c("red","green","blue"),names=c("HYDRO","CLIMA","METEO"),ylim=c(0,1))



boxplot(AltaSincronia$Hydrology,AltaSincronia$Climatology,AltaSincronia$Meteo,
        main="High Syncrony",
        col=c("red","green","blue"),names=c("HYDRO","CLIMA","METEO"),ylim=c(0,1))

boxplot(BassaSincronia$Hydrology,BassaSincronia$Climatology,BassaSincronia$Meteo,
        main="Low Syncrony",
        col=c("red","green","blue"),names=c("HYDRO","CLIMA","METEO"),ylim=c(0,1))






