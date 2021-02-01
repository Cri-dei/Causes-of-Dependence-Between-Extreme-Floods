#################################################################
############################DDMAP############################
################################QQWORK###########################
###################Author: Cristina Deidda ####################
##############################and###############################
#########################Leila Rahimi##########################
################################################################


#In this code it is computed the DDMAP for Dependent and Independent couples #
# Taking into account all possible combination for Hydro index######
# DDMAP, 3D plot and BOXPLOT#

#Clear all before___ Be cautious!!!!!#####
rm(list = ls())

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
library(gtools)
library(grid)
library(scatterplot3d)

#######################################################################
#Load Workspace
################## CODE FOR PVALUE= 0.01 ################################

#CHOOSE LAG TIME
#### LAG TIME #########

Lag_time<-5

#######################################################################
#Load Workspace

##Choose directory

#pc ufficio
#setwd("D:/PROJECTS/Regional/DISTANCE_selection/Data")
#path<-c("C:/PROJECTS 2021/QQ")
path<-c("C:/PROJECTS 2021/QQ/Results/POT_max")

setwd("C:/PROJECTS 2021/QQ/Results/POT_max")
#pc portatile
#path<-c("C:/Users/39349/Documents/Regional")

################################################################
#### Load workspace pvalue=0.01 ##################
################################################################

#setwd(paste0(path,"/Lag time/Lagtime_",Lag_time,"/Workspace"))
setwd(paste0(path,"/Results/POT_max"))


#Workspace needed:
#01)M_ALL_perc_LAG_%% -> ALL M_ALL and M_ALL_perc
#02)M_ALL_IND_perc_LAG_%% -> M_ALL for indipendent couples with selected pvalue
#03)M_ALL_0.01_perc_LAG_%% -> M_ALL for dependent couples with selected pvalue and positive

load("Pvalue.RData")

load(file=paste0("M_ALL_perc_LAG_",Lag_time,".RData"))
load(file=paste0("M_ALL_IND_perc_",pvalue,"_LAG_",Lag_time,".RData"))
load(file=paste0("M_ALL_",pvalue,"_perc_LAG_",Lag_time,".RData"))

################################################################
#### Load workspace pvalue Bonferroni adjusted ##################
################################################################


setwd(paste0(path,"/Lag time/Lagtime_",Lag_time,"/Workspace_Bonf"))
load(paste0("M_ALLbonf_perc_LAG_",Lag_time,".RData"))


############# Load all the data ##########################


load("Pvalue.RData")

load(file=paste0("M_ALLbonf_perc_LAG_",Lag_time,".RData"))
load(file=paste0("M_ALLbonf_DEP_perc_LAG_",Lag_time,".RData"))
load(file=paste0("M_ALLbonf_IND_perc_LAG_",Lag_time,".RData"))

pvalue<- Pv_th

#################### BONFERRONI initial matrix ###############

M_ALL_0.01_perc<-M_ALL_DEP_perc

M_ALL_0.01_POS_perc<-M_ALL_DEP_POS_perc

M_ALL_IND_perc0.01<-M_ALL_IND_perc

M_ALL_IND_perc0.01$X.1<-NA

####THE PVALUE IS CHOOSEN IN THE PREVIOUS CODE ############
################## CODE FOR PVALUE= 0.01 ################################

print( paste("The pvalue selected is",pvalue))

##################################

M_ALL_0.01_POS_perc$Class[M_ALL_0.01_POS_perc$N.SY.N.ALL>=0.60]<-"High Syn"
M_ALL_0.01_POS_perc$X.1[M_ALL_0.01_POS_perc$N.SY.N.ALL>=0.60]<-c(3)
M_ALL_0.01_POS_perc$Class[M_ALL_0.01_POS_perc$X.1==1]<-"Depind"
M_ALL_0.01_POS_perc$Class[M_ALL_0.01_POS_perc$X.1==2]<-"Depdep"


###################### 1. DISSIMILARITY INDEXES ################################

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


M_ALL_IND_perc<-M_ALL_IND_perc0.01


Density_perc<-data.frame(matrix(,nrow=1006,ncol=10))
colnames(Density_perc)<-c("CODE","Selected","DEP_MeteoHydro","DEP_MeteoClima","DEP_ClimaHydro","DEP_3Ind","IND_MeteoHydro","IND_MeteoClima","IND_ClimaHydro","IND_3Ind")

#setwd(paste0(path,"/Lag time/Lagtime_",Lag_time,"/Plot_bonf"))


pdf(file=paste0("DDMAP_",Lag_time,".pdf"),width=9, height=9)  

xxx<-1

#Predictors<-c("Arable.horticultural", "Grassland", "Mountain.heath.bog","Urban.extent","ASPBAR..?..",         
#              "DPSBAR..m.km.","LDP..km.","Catchment.area","Maximum.altitude","Max.Min.Altitude","Min.Altitude")

# Predictors<-c("Arable.horticultural", "Grassland", "Mountain.heath.bog","Urban.extent",         
#               "DPSBAR..m.km.","LDP..km.")

Predictors<-c("Catchment.area", "BFIHOST...." , "QMED",
              "DPSBAR..m.km.","LDP..km.","SPRHOST","Maximum.altitude")

Predictors<-c("Arable.horticultural","Grassland","Urban.extent"   ,"Maximum.altitude")



#v1<- match(Predictors,colnames(M_ALL_0.01_POS_perc))
v1<-Predictors
 
for(hh in 4:6){

  
    hh<-length(Predictors)

  k<-combinations(length(Predictors), hh, v=v1, set=TRUE, repeats.allowed=FALSE)
    #ff<-1
for(ff in 1:nrow(k)){
    
#for(ff in c(3,5,6,7)){
    
    Density_perc$CODE[xxx]<-paste(hh,ff)
    Density_perc$Selected[xxx]<-paste(k[ff,], collapse = ' , ')
    colnames(M_ALL_0.01_POS_perc)[c(k[ff,])]
    
    M1_DEP_0[,2]<- apply(M_ALL_0.01_POS_perc[,k[ff,]],1,mean)   
    M1_IND_0[,2]<- apply(M_ALL_IND_perc0.01[,k[ff,]],1,mean)   
    
    ############# Not consider NA in Hydro Index ##################################
    
    M1_DEP<-M1_DEP_0[complete.cases(M1_DEP_0[,1:3]),]
    M1_IND<-M1_IND_0[complete.cases(M1_IND_0[,1:3]),]
    
    final_length_dep<-nrow(M1_DEP)
    final_length_ind<-nrow(M1_IND)
    #Print indexes
    final_length_depdep<-length(which(M1_DEP$X.1==2))
    final_length_depind<-length(which(M1_DEP$X.1==1))
    
    title=print(paste0("Hydrological Indexes selected: ",k[ff,]))
    
    Final_predictors<-k[ff,]
    nam0<-paste(k[ff,], collapse = ' , ')
    
    ############################## DDMAP###################################################
    ######################### DEPENDENT COUPLES#######################################????
    
    
    # Select Pixel size####
    
    l_p<-0.05 # Size pixel
    
    #Create a grid dividing 1 in 0.05 length
    
    Gridx<-seq(l_p,1,l_p)
    num_int<-length(Gridx)
    
    
    ######## Meteo-Hydrological Index ##############
    
    M_grid1_DEP<-matrix(data=NA,nrow=num_int,ncol=num_int)
    
    
    colnames(M_grid1_DEP)<-Gridx
    rownames(M_grid1_DEP)<-Gridx
    
    
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
        
        CC<-which(M1_DEP$Meteo<x2 & M1_DEP$Meteo>=x1 & M1_DEP$Hydrology<y2 & M1_DEP$Hydrology>=y1)
        
        M_grid1_DEP[i,xx]<-mean(M1_DEP[CC,4])
        
        y0<-y0+l_p
      }
      y0<-0
      x0<-x0+l_p
      
    }
    
    
    coul <- viridis(100)
    
    P1_DEP<-levelplot(t(M_grid1_DEP), col.regions = coul, cex.main=0.05, 
                      xlab="Meteorological Dissimilarity Index",ylab="Hydrological Dissimilarity Index",at=c(seq(-1,1,0.05)))
    
    Density_perc$DEP_MeteoHydro[xxx]<-length(which(is.na(M_grid1_DEP[1:10,1:10])==FALSE))/length(which(is.na(M_grid1_DEP[,])==FALSE))*100
    
    
    
    
    #ggsave('3.DDMAP_Meteo-Clima_POS.png', width =11, height = 8, dpi = 100)
    
    ######## Meteo-Climatological Index ##############
    
    M_grid2_DEP<-matrix(data=NA,nrow=num_int,ncol=num_int)
    #M_grid3_DEP<-matrix(data=NA,nrow=num_int,ncol=num_int)
    
    colnames(M_grid2_DEP)<-Gridx
    rownames(M_grid2_DEP)<-Gridx
    
    
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
        
        CC<-which(M1_DEP$Meteo<x2 & M1_DEP$Meteo>=x1 & M1_DEP$Climatology<y2 & M1_DEP$Climatology>=y1)
        
        M_grid2_DEP[i,xx]<-mean(M1_DEP[CC,4])
        #M_grid3_DEP [i,xx]<-mean(M1_DEP[CC,4])
        y0<-y0+l_p
      }
      y0<-0
      x0<-x0+l_p
      
    }
    
    coul <- viridis(100)
    
    P2_DEP<-levelplot(t(M_grid2_DEP), col.regions = coul,cex.main=0.05, 
                                      xlab="Meteorological Dissimilarity Index",ylab="Climatological Dissimilarity Index",at=c(seq(-1,1,0.05))) 
    
    Density_perc$DEP_MeteoClima[xxx]<-length(which(is.na(M_grid2_DEP[1:10,1:10])==FALSE))/length(which(is.na(M_grid2_DEP[,])==FALSE))*100
    
    ######## Clima-Hydrological Index ##############
    
    M_grid3_DEP<-matrix(data=NA,nrow=num_int,ncol=num_int)
    
    
    colnames(M_grid3_DEP)<-Gridx
    rownames(M_grid3_DEP)<-Gridx
    
    
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
        
        CC<-which(M1_DEP$Climatology<x3 & M1_DEP$Climatology>=x1 & M1_DEP$Hydrology<y3 & M1_DEP$Hydrology>=y1)
        
        M_grid3_DEP[i,xx]<-mean(M1_DEP[CC,4])
        
        y0<-y0+l_p
      }
      y0<-0
      x0<-x0+l_p
      
    }
    
    coul <- viridis(100)
    
    P3_DEP<-levelplot(t(M_grid3_DEP), col.regions = coul,cex.main=0.05 ,
                                      xlab="Climatological Index",ylab="Hydrological Index",at=c(seq(-1,1,0.05)))
    
    Density_perc$DEP_ClimaHydro[xxx]<-length(which(is.na(M_grid3_DEP[1:10,1:10])==FALSE))/length(which(is.na(M_grid3_DEP[,])==FALSE))*100
    
    ######## Clima-Hydro-Meteo DEPex ##############
    
    M_grid4_DEP<-matrix(data=NA,nrow=num_int,ncol=num_int)
    
    
    colnames(M_grid4_DEP)<-Gridx
    rownames(M_grid4_DEP)<-Gridx
    
    
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
        
        CC<-which(M1_DEP$Climatology<x3 & M1_DEP$Climatology>=x1 & M1_DEP$Hydrology<y3 & M1_DEP$Hydrology>=y1)
        
        M_grid4_DEP[i,xx]<-mean(M1_DEP[CC,1])
        
        y0<-y0+l_p
      }
      y0<-0
      x0<-x0+l_p
      
    }
    
    coul <- viridis(100)
    
    P4_DEP<-levelplot(t(M_grid4_DEP), col.regions = coul, main="CLIMA-HYDRO-METEO", 
                                      xlab="Climatological DEPex",ylab="Hydrological DEPex",at=c(seq(0,1,0.05)))
    
    Density_perc$DEP_3Ind[xxx]<-length(which(is.na(M_grid4_DEP[1:10,1:10])==FALSE))/length(which(is.na(M_grid4_DEP[,])==FALSE))*100
    
    ############################## DDMAP###################################################
    ######################### INDENDENT COUPLES#######################################????
    
    
    # Select Pixel size####
    
    l_p<-0.05 # Size pixel
    
    #Create a grid dividing 1 in 0.05 length
    
    Gridx<-seq(l_p,1,l_p)
    num_int<-length(Gridx)
    
    
    ######## Meteo-Hydrological Index ##############
    
    M_grid1_IND<-matrix(data=NA,nrow=num_int,ncol=num_int)
    
    
    colnames(M_grid1_IND)<-Gridx
    rownames(M_grid1_IND)<-Gridx
    
    
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
        
        CC<-which(M1_IND$Meteo<x2 & M1_IND$Meteo>=x1 & M1_IND$Hydrology<y2 & M1_IND$Hydrology>=y1)
        
        M_grid1_IND[i,xx]<-mean(M1_IND[CC,4])
        
        y0<-y0+l_p
      }
      y0<-0
      x0<-x0+l_p
      
    }
    
    coul <- viridis(100)
    
    P1_IND<-levelplot(t(M_grid1_IND), col.regions = coul, main="",cex=0.05,
                                       xlab="Meteorological Dissimilarity Index",ylab="Hydrological Dissimilarity Index",at=c(seq(-1,1,0.05)))
    
    Density_perc$IND_MeteoHydro[xxx]<-length(which(is.na(M_grid1_IND[1:10,1:10])==FALSE))/length(which(is.na(M_grid1_IND[,])==FALSE))*100
    
    #ggsave('3.DDMAP_Meteo-Clima_POS.png', width =11, height = 8, dpi = 100)
    
    ######## Meteo-Climatological Index ##############
    
    M_grid2_IND<-matrix(data=NA,nrow=num_int,ncol=num_int)
    #M_grid3_IND<-matrix(data=NA,nrow=num_int,ncol=num_int)
    
    colnames(M_grid2_IND)<-Gridx
    rownames(M_grid2_IND)<-Gridx
    
    
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
        
        CC<-which(M1_IND$Meteo<x2 & M1_IND$Meteo>=x1 & M1_IND$Climatology<y2 & M1_IND$Climatology>=y1)
        
        M_grid2_IND[i,xx]<-mean(M1_IND[CC,4])
        #M_grid3_IND [i,xx]<-mean(M1_DEP[CC,4])
        y0<-y0+l_p
      }
      y0<-0
      x0<-x0+l_p
      
    }
    
    coul <- viridis(100)
    
    P2_IND<-levelplot(t(M_grid2_IND), col.regions = coul,  main="",cex.main=0.05,
                                       xlab="Meteorological Dissimilarity Index",ylab="Climatological Dissimilarity Index",at=c(seq(-1,1,0.05)))
    
    Density_perc$IND_MeteoClima[xxx]<-length(which(is.na(M_grid2_IND[1:10,1:10])==FALSE))/length(which(is.na(M_grid2_IND[,])==FALSE))*100
    
    ######## Clima-Hydrological Index ##############
    
    M_grid3_IND<-matrix(data=NA,nrow=num_int,ncol=num_int)
    
    
    colnames(M_grid3_IND)<-Gridx
    rownames(M_grid3_IND)<-Gridx
    
    
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
        
        CC<-which(M1_IND$Climatology<x3 & M1_IND$Climatology>=x1 & M1_IND$Hydrology<y3 & M1_IND$Hydrology>=y1)
        
        M_grid3_IND[i,xx]<-mean(M1_IND[CC,4])
        
        y0<-y0+l_p
      }
      y0<-0
      x0<-x0+l_p
      
    }
    
    coul <- viridis(100)
    
    P3_IND<-levelplot(t(M_grid3_IND), col.regions = coul, main=" ",cex.main=0.05, 
                                      xlab="Climatological Index",ylab="Hydrological Index",at=c(seq(-1,1,0.05)))
    
    Density_perc$IND_ClimaHydro[xxx]<-length(which(is.na(M_grid3_IND[1:10,1:10])==FALSE))/length(which(is.na(M_grid3_IND[,])==FALSE))*100
    
    
    ##############################################################????
    ######## Clima-Hydro-Meteo Index ##############
    
    M_grid4_IND<-matrix(data=NA,nrow=num_int,ncol=num_int)
    
    
    colnames(M_grid4_IND)<-Gridx
    rownames(M_grid4_IND)<-Gridx
    
    
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
        
        CC<-which(M1_IND$Climatology<x3 & M1_IND$Climatology>=x1 & M1_IND$Hydrology<y3 & M1_IND$Hydrology>=y1)
        
        M_grid4_IND[i,xx]<-mean(M1_IND[CC,1])
        
        y0<-y0+l_p
      }
      y0<-0
      x0<-x0+l_p
      
    }
    
    coul <- viridis(100)
    
    P4_IND<-levelplot(t(M_grid4_IND), col.regions = coul, main="CLIMA-HYDRO-METEO", 
                                      xlab="Climatological Index",ylab="Hydrological Index",at=c(seq(0,1,0.05)))
    
    Density_perc$IND_3Ind[xxx]<-length(which(is.na(M_grid4_IND[1:10,1:10])==FALSE))/length(which(is.na(M_grid4_IND[,])==FALSE))*100
    
    
    
    
    ##############depandind
    
    nam<-paste(nam0, "  Number of points: DEP:",final_length_dep," IND:", final_length_ind,"Depdep=", 
               final_length_depdep,"DepInd=",final_length_depind, "CODE:",paste(hh,ff))
    text.p <- ggparagraph(text =nam, face = "italic", size = 12, color = "black")
    
    #grid.arrange(P1_DEP,P2_DEP,P3_DEP, P4_DEP,text.p, nrow=3,ncol=2,
    #             top = textGrob("DEPENDENT",gp=gpar(fontsize=20,font=3)),
    #             heights = c(0.7, 0.7, 0.2))
    
    #grid.arrange(P1_IND,P2_IND,P3_IND,P4_IND, text.p,nrow=3,ncol=2, 
    #            top = textGrob("INDEPENDENT",gp=gpar(fontsize=20,font=3)),
    #            heights = c(0.7, 0.7, 0.2))
    
    ###################### 6. Group in DD maps #################################
    
    #GG1<-ggplot(M1_DEP,aes(x=Meteo,y=Climatology, colour=Class,shape=Class,size=Class))+
    #  geom_point()  +
    # scale_size_manual(values=c(4,4,1))+
    #theme(text=element_text(size=16,family="Comic Sans MS"))+ xlim(0, 1) + ylim(0, 1)+ 
    #          labs(title = "Meteo-Clima")  + scale_color_manual(values=c("deepskyblue4", "green", "red"))+ 
    #           guides(shape = guide_legend(override.aes = list(size = 5)))
    
    #ggsave('3.Group_Distance_Meteo-Climav0.png', width =11, height = 8, dpi = 100)
    
    
    # GG2<-ggplot(M1_DEP,aes(x=Meteo,y=Hydrology, colour=Class,shape=Class,size=Class))+
    #  geom_point()  +
    #  scale_size_manual(values=c(4,4,1))+ xlim(0, 1) + ylim(0, 1)+ 
    # labs(title = "Meteo-Hydro")  + scale_color_manual(values=c("deepskyblue4", "green", "red"))+ 
    # guides(shape = guide_legend(override.aes = list(size = 5)))
    
    #ggsave('3.Group_Distance_Meteo-Hydrov0.png', width =11, height = 8, dpi = 100)
    
    ############SCATTERPLOT WITH GROUPS DEPDEP- DEIND
    #GG3<-
    #GG_GROUP<-ggplot(M1_DEP,aes(x=Climatology,y=Hydrology, colour=Class,shape=Class,size=Class))+
    #geom_point()  +
    #scale_size_manual(values=c(4,4,1))+ xlim(0, 1) + ylim(0, 1)+ 
    #labs(title = "Clima-Hydro")  + scale_color_manual(values=c("deepskyblue4", "green", "red"))+ 
    #guides(shape = guide_legend(override.aes = list(size =2)))
    
    
    ########### 3 D SCATTERPLOT    ##################
    par(mfrow=c(1,1))
    dfm<-M1_DEP[which(M1_DEP$X.1==1 | M1_DEP$X.1==2 |  M1_DEP$X.1==3),]
    
    mycolors <- c('darkred', 'darkcyan','yellow')
    dfm$color <- mycolors[ as.numeric(dfm$X.1) ]
    

    #Dependent
    grid.arrange(P1_DEP,P2_DEP, P3_DEP,P4_DEP,text.p, nrow=3,ncol=2,
                 top = textGrob("DEPENDENT",gp=gpar(fontsize=20,font=3)),
                 heights = c(0.7, 0.7, 0.2))
    # Scatterplot
    par(mfrow=c(1,1))
    scatterplot3d(  x=dfm$Meteo, y=dfm$Hydrology, z=dfm$Climatology, 
                         color = dfm$color, 
                         xlim=c(0,1),
                         ylim=c(0,1),
                         zlim=c(0,1),
                         xlab="Metereology", ylab="Hydrology", zlab="Climatology",
                         cex.symbols = 2, pch = 19, angle = +120)
    
    
    legend("topright",s3d$xyz.convert(18, 0, 12), pch = 19, yjust=0,
           # here you define the labels in the legend
               legend = c('DepDep','DepInd','High Syn'),col=c( 'darkcyan','darkred','yellow'), cex = 1)
           
    par(mfrow=c(1,1))      
    
    #Independent
    grid.arrange(P1_IND,P2_IND,P3_IND,P4_IND, text.p,nrow=3,ncol=2, 
                 top = textGrob("INDEPENDENT",gp=gpar(fontsize=20,font=3)),
                 heights = c(0.7, 0.7, 0.2))
    
    #ggplot(M1_DEP,aes(x=Climatology,y=Hydrology, colour=Class,shape=Class,size=Class))+
    # geom_point(fill=Class)  +
    #  scale_size_manual(values=c(4,4,1))+ xlim(0, 1) + ylim(0, 1)+ 
    # labs(title = "Clima-Hydro") + scale_color_manual(values=c("deepskyblue4", "green", "red")) +
    #  guides(shape = guide_legend(override.aes = list(size =2)))+
    # facet_grid(. ~ Climatology ,space = "free", scales = "free", margins = T)
    
    ################BOXPLOT##########################################
    #Depdep-DepInd
    par(mfrow=c(2,2))
    
    boxplot(M1_DEP$Climatology[M1_DEP$Class=="Depdep"],M1_DEP$Climatology[M1_DEP$Class=="Depind"],M1_DEP$Climatology[M1_DEP$Class=="High Syn"]
            ,names=c("DepDep","DepInd","High Syn"), main="Clima",ylim=c(0,1))
    
    boxplot(M1_DEP$Meteo[M1_DEP$Class=="Depdep"],M1_DEP$Meteo[M1_DEP$Class=="Depind"],M1_DEP$Meteo[M1_DEP$Class=="High Syn"]
            ,names=c("DepDep","DepInd","High Syn"), main="Meteo",ylim=c(0,1))
    
    boxplot(M1_DEP$Hydrology[M1_DEP$Class=="Depdep"],M1_DEP$Hydrology[M1_DEP$Class=="Depind"],M1_DEP$Hydrology[M1_DEP$Class=="High Syn"]
            ,names=c("DepDep","DepInd","High Syn"), main="Hydro",ylim=c(0,1))
    
    boxplot(M1_DEP$KendallTau[M1_DEP$Class=="Depdep"],M1_DEP$KendallTau[M1_DEP$Class=="Depind"],M1_DEP$KendallTau[M1_DEP$Class=="High Syn"]
            ,names=c("DepDep","DepInd","High Syn"), main="KendallTau",ylim=c(0,1))
    
    #Dependent- Independent
    par(mfrow=c(2,2))
    
    boxplot(M1_DEP$Distance[which(M1_DEP$Class=="Depdep")],M1_DEP$Distance[which(M1_DEP$Class=="Depind")],M1_DEP$Distance[which(M1_DEP$Class=="High Syn")],
            names=c("DepDep","Depind","High Syn"), main="Distance Groups")
    
    boxplot(M1_DEP$Meteo,M1_IND$Meteo
            ,names=c("Dependent","Independent"), main="ALL: Meteo",outline=FALSE)
    
    boxplot(M1_DEP$Climatology,M1_IND$Climatology
            ,names=c("Dependent","Independent"), main="ALL: Climatology",outline=FALSE)
    
    boxplot(M1_DEP$Hydrology,M1_IND$Hydrology
            ,names=c("Dependent","Independent"), main="ALL: Hydrology",outline=FALSE)
    
    xxx<-xxx+1
  }
}

dev.off ()

write.table(Density_perc, file=paste0("Percentages_.txt"))

par(mfrow=c(2,2))

boxplot(M1_DEP$Climatology[M1_DEP$Class=="Depdep"],M1_DEP$Climatology[M1_DEP$Class=="Depind"],M1_DEP$Climatology[M1_DEP$Class=="High Syn"]
        ,names=c("DepDep","DepInd","High Syn"), main="Clima",ylim=c(0,1))

boxplot(M1_DEP$Meteo[M1_DEP$Class=="Depdep"],M1_DEP$Meteo[M1_DEP$Class=="Depind"],M1_DEP$Meteo[M1_DEP$Class=="High Syn"]
        ,names=c("DepDep","DepInd","High Syn"), main="Meteo",ylim=c(0,1))

boxplot(M1_DEP$Hydrology[M1_DEP$Class=="Depdep"],M1_DEP$Hydrology[M1_DEP$Class=="Depind"],M1_DEP$Hydrology[M1_DEP$Class=="High Syn"]
        ,names=c("DepDep","DepInd","High Syn"), main="Hydro",ylim=c(0,1))

boxplot(M1_DEP$KendallTau[M1_DEP$Class=="Depdep"],M1_DEP$KendallTau[M1_DEP$Class=="Depind"],M1_DEP$KendallTau[M1_DEP$Class=="High Syn"]
        ,names=c("DepDep","DepInd","High Syn"), main="KendallTau",ylim=c(0,1))



par(mfrow=c(2,2))
boxplot(M1_DEP$Meteo,M1_IND$Meteo
        ,names=c("Dependent","Independent"), main="Meteo")

boxplot(M1_DEP$Climatology,M1_IND$Climatology
        ,names=c("Dependent","Independent"), main="Climatology")

boxplot(M1_DEP$Hydrology,M1_IND$Hydrology
        ,names=c("Dependent","Independent"), main="Hydrology")

par(mfrow=c(2,2))
boxplot(M1_DEP$Distance[which(M1_DEP$X.1==1)],M1_DEP$Distance[which(M1_DEP$X.1==2)],names=c("DepInd","Depdep"))

boxplot(M1_DEP$Climatology,M1_IND$Climatology
        ,names=c("Dependent","Independent"), main="Climatology")

boxplot(M1_DEP$Hydrology,M1_IND$Hydrology
        ,names=c("Dependent","Independent"), main="Hydrology")

