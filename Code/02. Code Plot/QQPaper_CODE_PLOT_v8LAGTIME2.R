###########  PAPER QQ ########
#### CODE FOR THE GRAPH ##########
######Author: Cristina Deidda ##########

###this is equal to V8 but for checking different lag time ####
######REVISED CODE AFTER 1 REVISION: 28 OCTOBER 2020#################
#################################################################

########IN THIS CODE ARE CREATED THE PLOTS FOR PAPER:
#1. KENDALLTAU E PVALUE WITH UK MAP
#2. CORRELOGRAM
#3. Kendall Tau and Distance with regression line
#4. Mirrored Histogram for Near and Far couples

#For the DDMAP see code in DDMAPCODE#
############################################################
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

########### CHOOSE INITIAL DATA #############
#Lagtime 10 days
#Lag_time<-10
#load("C:/Users/39349/Documents/Regional/Workspace/M_ALL_perc.RData")

##FOR LAGTIME 10 CHECK PREVIOUS CODE V8
#THIS WILL GIVE YOU ERRORS 

#Lagtime 5 or 7 days
Lag_time<-5

##Choose directory

#pc ufficio
#setwd("D:/PROJECTS/Regional/DISTANCE_selection/Data")
path<-c("C:/PROJECTS 2021/QQ")
#pc portatile
#path<-c("C:/Users/39349/Documents/Regional")



setwd(paste0(path,"/Lag time/Lagtime_",Lag_time,"/Workspace"))
load(paste0("M_ALL_perc_LAG_",Lag_time,".RData"))

############# Load all the data ##########################


load("Pvalue.RData")

load(file=paste0("M_ALL_perc_LAG_",Lag_time,".RData"))
load(file=paste0("M_ALL_IND_perc_",pvalue,"_LAG_",Lag_time,".RData"))
load(file=paste0("M_ALL_",pvalue,"_perc_LAG_",Lag_time,".RData"))

#load("ALLDATA01_andonlyPOSITIVE.RData")
#load("M_ALL_001.RData")
#load("C:/Users/39349/Documents/Regional/Workspace/M_ALL_INDIPENDENT_perc.RData")


setwd(paste0(path,"/Lag time/Lagtime_",Lag_time,"/Plotcheck"))

##############################
####################          PLOT CODE            ##########################                      
                       ###########################
##########################KT AND MAP UK #################################                       
# 1.1. plot: KT e P value 

###### 1. Kendall Tau- P value with Subplot   #############
M_ALL_0.01<- M_ALL[M_ALL$KendalT.p.value<=0.01,]

op <- par(no.readonly = TRUE)
set.seed(42)

plot(M_ALL$KendalT.value, M_ALL$KendalT.p.value, xlab=c("Kendall's tau"), ylab=c("p-value"), col=M_ALL_0.01$Distance, cex.lab=1.2, cex.axis=1.2,  cex.sub=1.2) 
abline(h =0.01, untf = FALSE, col="red",lty = "dashed",lwd=2) 

par(new=TRUE, oma=c(12,2,1,1))
layout(matrix(1:2,1))
plot(M_ALL_0.01$KendalT.value, M_ALL_0.01$KendalT.p.value, xlab=c("Kendall's tau"), ylab=c("p-value"), col=M_ALL_0.01$Distance) 
abline(h =0.01, untf = FALSE, col="red",lty = "dashed",lwd=2) 

par(op)

###### 1.2 Kendall Tau- P value and distance with Subplot ################

ggplot(data=M_ALL_0.01,aes(x=KendalT.value, y=KendalT.p.value))+
  geom_point(aes(color =Distance),size = 2)+ scale_color_gradientn(colours = rainbow(8))+
  xlab("Kendall's Tau")+ ylab("p-value")


main.plot<-ggplot(data=M_ALL, aes(x=KendalT.value, y=KendalT.p.value))+
  geom_point(aes(color =Distance),size = 2)+ scale_color_gradientn(name="Distance [km]", colours = rainbow(8))+
  xlab("Kendall's Tau")+ ylab("p-value")


main.plot<- main.plot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.background = element_blank(), axis.line = element_line(colour = "black"))


inset.plot <- ggplot(data=M_ALL_0.01,aes(x=KendalT.value, y=KendalT.p.value))+
  geom_point(aes(color =Distance),size = 2, show.legend = FALSE)+ scale_color_gradientn(colours = rainbow(8))+
  xlab("Kendall's Tau")+ ylab("p-value")

inset.plot <- inset.plot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               panel.background = element_blank(), axis.line = element_line(colour = "black"))

plot.with.inset <-
  ggdraw() +
  draw_plot(main.plot) +
  draw_plot(inset.plot, x = 0.6, y = .7, width = .3, height = .3)


png(filename = "01-KT_pval_distance.png",
    width = 10.33, height = 7, units = "in",   res =400) 

plot.with.inset

dev.off()

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
  coord_map()+geom_point(data = coord_point,  aes(x = X, y = Y, group = NULL), colour = "black", size = 0.7) 

UK_map<-UK_map+ theme(panel.background = element_rect(fill = 'white', colour = 'black'))

##################1.3 KT_pvalue + UK map ##########################

Plot_KT<-"01-KT_pval_distance.png"


png(filename = "01-KT_UK-mod1.png",
    width = 10.33, height = 7, units = "in",   res =400) 

ggdraw()+draw_image(Plot_KT)+ draw_plot(UK_map, x = 0, y = .6, width = .4, height = .4)

dev.off()

###############################################################################

###############  2. Correlogram     ###################################
#### CORRELOGRAM UPDATED AFTER FIRST REVISION ############

##  FIRST ROW: Density function with Legend

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

#plot just first row
#ggarrange(GG_11,GG_22,GG_3, nrow=1,ncol=3)

#### VERSION 1  #####

######Second ROW: Scatterplot with unique SHAPE
# 
# GG_4v1<-ggplot(data=M_ALL ,aes(x=M_ALL[,4],y= M_ALL[,15],color = as.factor(X)))+geom_point(alpha = 0.3, size=0.9) + theme_bw()+ 
#   scale_fill_manual(values=c("#FF3333", "#33CCCC"))+ labs(x = "Kendall's tau", y="Syn")+  
#   theme(legend.position = "none")
# 
# 
# GG_44v1<-GG_4v1 + scale_x_continuous(
#   labels = scales::number_format(accuracy = 0.01,
#                                  decimal.mark = '.'), limits = c(-1,1))
# 
# GG_5v1<-ggplot(data=M_ALL ,aes(x=M_ALL[,25],y= M_ALL[,15],color = as.factor(X)))+geom_point(alpha = 0.3, size=0.9) + theme_bw()+ 
#   scale_fill_manual(values=c("#FF3333", "#33CCCC"))+ labs(x = "Distance [km]", y="Syn") + 
#   theme(legend.position = "none")
# 
# 
# GG_6v1<-ggplot(data=M_ALL ,aes(x=M_ALL[,25],y= M_ALL[,4],color = as.factor(X)))+geom_point(alpha = 0.3, size=0.9) + theme_bw()+ 
#   scale_fill_manual(values=c("#FF3333", "#33CCCC"))+ labs(x = "Distance [km]", y="Kendall's tau")+ 
#   theme(legend.position = "none")
# 
# 
# GG_66v1<-GG_6v1 + scale_x_continuous(
#   labels = scales::number_format(accuracy = 0.01,
#                                  decimal.mark = '.'))+ scale_y_continuous(limits=c(-1,1))
# 
# 
# 
# GG_correl_v1<-ggarrange(GG_11,GG_22,GG_3,GG_44v1,GG_5v1,GG_66v1,ncol=3,nrow=2, labels=c("a)","b)","c)","d)","e)","f)") ,font.label = list(size = 12)) 
# 
# GG_correl_v1
# 
# #Save Correlogram version 1
# ggsave("02.Correlogram_wleg.jpeg", units="in",dpi=400, height=7,width =12)
# 

#### VERSION 2  #####

######Second ROW: Scatterplot with 2 SHAPES


GG_4v2<-ggplot(data=M_ALL ,aes(x=KendalT.value,y=N.SY.N.ALL,color = as.factor(X)))+geom_point(aes(shape=as.factor(X)),alpha = 0.3, size=0.9) + theme_bw()+ 
  scale_fill_manual(values=c("#FF3333", "#33CCCC"))+ labs(x = "Kendall's tau", y="Syn")+
  scale_size_manual(values=c(0.1,0.8))
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


GG_correl_v2<-ggarrange(GG_11,GG_22,GG_3,GG_44v2,GG_5v2,GG_66v2,ncol=3,nrow=2, labels=c("a)","b)","c)","d)","e)","f)") ,font.label = list(size = 12)) 

GG_correl_v2

#Save Correlogram version 2
ggsave("02.Correlogram_wlg_v2.jpeg", units="in",dpi=400, height=7,width =12)

#### VERSION 3  #####

######Second ROW: Scatterplot with 2 SHAPES and 2 SIZE
# 
size_val=c(0.9,2)
shape_val=c(4,17)

GG_4v2<-ggplot(data=M_ALL ,aes(x=KendalT.value,y= N.SY.N.ALL,color = as.factor(X)))+geom_point(aes(shape=as.factor(X),size=as.factor(X)),alpha = 0.3) + theme_bw()+
  scale_fill_manual(values=c("#FF3333", "#33CCCC"))+ labs(x = "Kendall's tau", y="Syn")+
  scale_size_manual(values=size_val)+
  scale_shape_manual(values=shape_val)+
  theme(legend.position = "none")


GG_44v2<-GG_4v2 + scale_x_continuous(
  labels = scales::number_format(accuracy = 0.01,
                                 decimal.mark = '.'), limits = c(-1,1))

GG_5v2<-ggplot(data=M_ALL ,aes(x=Distance,y= N.SY.N.ALL,color = as.factor(X)))+geom_point(aes(shape=as.factor(X),size=as.factor(X)),alpha = 0.3) + theme_bw()+
  scale_fill_manual(values=c("#FF3333", "#33CCCC"))+ labs(x = "Distance [km]", y="Syn") +
  scale_size_manual(values=size_val)+
  scale_shape_manual(values=shape_val)+
  theme(legend.position = "none")


GG_6v2<-ggplot(data=M_ALL ,aes(x=Distance,y= KendalT.value,color = as.factor(X)))+geom_point(aes(shape=as.factor(X),size=as.factor(X)),alpha = 0.3) + theme_bw()+
  scale_fill_manual(values=c("#FF3333", "#33CCCC"))+ labs(x = "Distance [km]", y="Kendall's tau")+
  scale_size_manual(values=size_val)+
  scale_shape_manual(values=shape_val)+
  theme(legend.position = "none")


GG_66v2<-GG_6v2 + scale_x_continuous(
  labels = scales::number_format(accuracy = 0.01,
                                 decimal.mark = '.'))+ scale_y_continuous(limits=c(-1,1))


GG_correl_v3<-ggarrange(GG_11,GG_22,GG_3,GG_44v2,GG_5v2,GG_66v2,ncol=3,nrow=2, labels=c("a)","b)","c)","d)","e)","f)") ,font.label = list(size = 12))

GG_correl_v3

#Save Correlogram version 3
ggsave("02.Correlogram_wlg_v3.jpeg", units="in",dpi=400, height=7,width =12)

# 


########### 3. Kendall Tau and Distance with regression line ###########

#M_ALL_0.01<-M_ALL_0.01_POS

png(filename = "03_KTHIST-1.png",
    width = 10.33, height = 6.29, units = "in",   res =400) 
 
gh<-which( M_ALL_0.01$KendalT.value>0 )

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
plot(M_ALL_0.01$Distance[M_ALL_0.01$Distance<Dist_sel], M_ALL_0.01$KendalT.value[M_ALL_0.01$Distance<230],col=rgb(0,0,1,1/4),xlab='Distance [km]',
                                                ylab="Kendall's tau",pch=19, cex.lab=1.2, cex.axis=1.2,  cex.sub=1.2, ylim=c(0,1), xlim=c(0,600))

points(M_ALL_0.01$Distance[M_ALL_0.01$Distance>=Dist_sel], M_ALL_0.01$KendalT.value[M_ALL_0.01$Distance>=230],col=rgb(1,0,0,1/4),xlab='Distance [km]',
                                                                ylab="Kendall's tau",pch=19, cex.lab=1.2, cex.axis=1.2,  cex.sub=1.2, ylim=c(0,1))


lines(predicted,lwd=2,col='deepskyblue4')
abline(v =230, untf = FALSE, col="darkgrey",lty = "dashed",lwd=2)

polygon(xx,yy,col = rgb(0.8,0.8,0.8,0.5), border = FALSE)

dev.off()

##Code for increasing size: cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5

################## 4. Mirrored Histogram: Near and Far couple ########################################

png(filename = "03_KTHIST-2.png",
    width = 10.33, height = 6.29, units = "in",   res =400)

par(mfrow=c(2,1)) 
par(mar=c(0,5,3,3))
hist(M_near_01_POS$N.SY.N.ALL, col=rgb(0,0,1,1/4) ,main="",xaxt="n", xlab="", xlim=c(0,1),cex.lab=1.2)
legend("topright", legend=c("Near","Far"), col=c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)), pt.cex=2, pch=15, cex=1.2)  
#hist(M_far_01_POS$N.SY.N.ALL, add=T,col=rgb(1,0,0,1/4),cex.lab=1.3)

par(mar=c(5,5,0,3))
hist(M_far_01_POS$N.SY.N.ALL,col=rgb(1,0,0,1/4), xlab="Syn", ylim=c(100,0),main="", xlim=c(0,1), ylab="",cex.lab=1.2) 

dev.off()


KT_distance<-"03_KTHIST-1.png"
Histogram<-"03_KTHIST-2.png"


Hist<-ggdraw() +
  draw_image(Histogram)

KT_dist<-ggdraw() +
  draw_image(KT_distance) 

ggarrange(KT_dist,Hist,ncol=1,nrow=2,labels=c("a)","b)"), font.label = list(size=12))

ggsave("02.KT-HIST1.jpeg", units="in", dpi=400, width=7.28,height=7.76)


##############################               SUMMARY                      ######################
#######################################################################################################################################################

##If you want to see previous version of DDMAAP , also Group of couple in DDMAP see code:   QQPaper_CODE_PLOT_v7, here I cleaned it up!#############

######### SUMMARY ##########
Summary<-data.frame(matrix(,6,9))

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

png("Summary_allvariables.png", height = 50*nrow(Summary), width = 200*ncol(Summary))
grid.table(Summary)
dev.off()


##TORTA PIE#
#write.table(Summary, file="Summary.txt")

png(filename = "PIECHART.png",
    width = 10.33, height = 6.29, units = "in",   res =400)

HSyn<- length(which(M_ALL_0.01_POS$N.SY.N.ALL>=0.60))
MediumSyn<- length(which(M_ALL_0.01_POS$N.SY.N.ALL>=0.40 & M_ALL_0.01_POS$N.SY.N.ALL<0.60))
LowSyn<-length(which(M_ALL_0.01_POS$N.SY.N.ALL<0.40))


slices <- c(HSyn, MediumSyn,LowSyn)
lbls <- c(paste0("High Syn(>=60%): ",HSyn), paste0("Medium Syn(>=40% and <60%): ",MediumSyn), paste0("Low Syn(<40%): ",LowSyn))
pie(slices, labels = lbls, main="Dependent Couples")

dev.off()
load("C:/Users/39349/Documents/Regional/Workspace/Ind_perc.RData")







