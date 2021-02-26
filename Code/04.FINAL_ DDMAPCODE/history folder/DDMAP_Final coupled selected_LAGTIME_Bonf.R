#################################################################
############################DDMAP##############################
################################QQWORK###########################
###################Author: Cristina Deidda ####################
###############################################################
###############################################################
################################################################


#In this code it is computed the DDMAP for Dependent and Independent couples #
# Taking into account all possible combination for Hydro index######
# DDMAP, 3D plot and BOXPLOT#

#Added: Bonferroni for Asynchrony part #

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
library(pals)

#CHOOSE LAG TIME
#### LAG TIME #########

Lag_time<-5

#######################################################################
#Load Workspace

#setwd(paste0("C:/Users/39349/Documents/Regional/Lag time/Lagtime_",Lag_time,"/Workspace"))


#Workspace needed:
#01)M_ALL_perc_LAG_%% -> ALL M_ALL and M_ALL_perc
#02)M_ALL_IND_perc_LAG_%% -> M_ALL for indipendent couples with selected pvalue
#03)M_ALL_0.01_perc_LAG_%% -> M_ALL for dependent couples with selected pvalue and positive

##Choose directory

#pc ufficio
#setwd("D:/PROJECTS/Regional/DISTANCE_selection/Data")
path<-c("C:/PROJECTS 2021/QQ")
#pc portatile
#path<-c("C:/Users/39349/Documents/Regional")


setwd(paste0(path,"/Lag time/Lagtime_",Lag_time,"/Workspace_Bonf"))
load(paste0("M_ALLbonf_perc_LAG_",Lag_time,".RData"))


############# Load all the data ##########################


load("Pvalue.RData")

load(file=paste0("M_ALLbonf_perc_LAG_",Lag_time,".RData"))
load(file=paste0("M_ALLbonf_DEP_perc_LAG_",Lag_time,".RData"))
load(file=paste0("M_ALLbonf_IND_perc_LAG_",Lag_time,".RData"))


#load("ALLDATA01_andonlyPOSITIVE.RData")
#load("M_ALL_001.RData")
#load("C:/Users/39349/Documents/Regional/Workspace/M_ALL_INDIPENDENT_perc.RData")



setwd(paste0(path,"/Lag time/Lagtime_",Lag_time,"/Plot_Bonf"))

####THE PVALUE IS CHOOSEN IN THE PREVIOUS CODE ############
################## CODE FOR PVALUE= 0.01 ################################

print( paste("The pvalue selected is",Pv_th))

#path<-paste0("pvalue",pvalue)


# ###########INFO The final matrix are ########### #
#M_ALL_IND_perc: for indipendent couples
#M_ALL_0.01_POS_perc : for dependent couples

########## IF YOU WANT TO CHANGE THRESHOLD PVALUE RUN THIS PART ##################

# ############################# CHANGING THRESHOLD PVALUE #################################
# #Selection as dependent couples with pvalue:0.015
# 
# for( i in 1:nrow(M_ALL_0.01_POS_perc))
# {
#   if(M_ALL_0.01_POS_perc$KendalT.p.value[i]<=pvalue){M_ALL_0.01_POS_perc$X[i]<-2}
#   else{M_ALL_0.01_POS_perc$X[i]<-1}
# }
# 
# 
# ############################# ASY PART #################################
# #Selection of Async data with more than 20 data and pvalue 0.01 #
# # 
# for( i in 1:nrow(M_ALL_0.01_POS_perc))
#  {
#   if(M_ALL_0.01_POS_perc$Num_Asyncr_occ[i]<20){M_ALL_0.01_POS_perc$X.1[i]<-NA}
#   else{ if(M_ALL_0.01_POS_perc$KT_pvalue_Asy[i]<=pvalue){M_ALL_0.01_POS_perc$X.1[i]<-2} else{M_ALL_0.01_POS_perc$X.1[i]<-1}
#   }
# }
################################################################################################

# 
# for( i in 1:nrow(M_ALL_IND_perc))
# {
#   if( M_ALL_IND_perc$Num_Asyncr_occ[i]<20){M_ALL_IND_perc$X.1[i]<-NA}
#   else{ if(M_ALL_IND_perc$KT_pvalue_Asy[i]<=0.01){M_ALL_IND_perc$X.1[i]<-2} else{M_ALL_IND_perc$X.1[i]<-1}
#   }
# }

#BONFERRONI ADJUSTMENT ######################

M_ALL_DEP_Asy20<-M_ALL_DEP[which(M_ALL_DEP$Num_Asyncr_occ>=20),]

M_ALLch<-M_ALL_DEP_Asy20[order(M_ALL_DEP_Asy20$KT_pvalue_Asy,decreasing=FALSE),]

M_ALLch$Num<-seq(1,nrow(M_ALLch),1)

M_ALLch$Bonpv<-0.05*M_ALLch$Num/nrow(M_ALLch)

M_ALLch$CHECK<- M_ALLch$KT_pvalue_Asy<=M_ALLch$Bonpv

length(which(M_ALLch$CHECK=="TRUE"))

ok<-which(M_ALLch$CHECK=="TRUE")

Pv_Asy_th<-M_ALLch$KT_pvalue_Asy[ok[length(ok)]] 

####################
M_ALL_0.01_POS_perc<-M_ALL_DEP_perc

M_ALL_IND_perc0.01<-M_ALL_IND_perc


# ############################# ASY PART #################################
# #Selection of Async data with more than 20 data and pvalue 0.01 #
# # 
 for( i in 1:nrow(M_ALL_0.01_POS_perc))
{
 if(M_ALL_0.01_POS_perc$Num_Asyncr_occ[i]<20){M_ALL_0.01_POS_perc$X.1[i]<-NA}
  else{ if(M_ALL_0.01_POS_perc$KT_pvalue_Asy[i]<=Pv_Asy_th){M_ALL_0.01_POS_perc$X.1[i]<-2} else{M_ALL_0.01_POS_perc$X.1[i]<-1}
  }
 }


M_ALL_0.01_POS_perc$Class[M_ALL_0.01_POS_perc$N.SY.N.ALL>=0.60]<-"High Syn"
M_ALL_0.01_POS_perc$X.1[M_ALL_0.01_POS_perc$N.SY.N.ALL>=0.60]<-c(3)
M_ALL_0.01_POS_perc$Class[M_ALL_0.01_POS_perc$X.1==1]<-"Depind"
M_ALL_0.01_POS_perc$Class[M_ALL_0.01_POS_perc$X.1==2]<-"Depdep"



###################### 1. DISSIMILARITY INDEXES ################################
###########################CHOOSE INITIAL MATRIX#################################

## DEPENDENT COUPLES
Var_1<-c("N.SY.N.ALL","BFIHOST....", "SAAR_61-90","KendalT.value", "Class",        
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

M1_DEP_0[,3]<- M_ALL_0.01_POS_perc$'SAAR_61-90'
M1_IND_0[,3]<- M_ALL_IND_perc0.01$'SAAR_61-90'


#setwd(paste0("C:/Users/39349/Documents/Regional/Lag time/Lagtime_",Lag_time,"/Plot"))
setwd("C:/PROJECTS 2021/QQ/Results_update")

xxx<-1

Predictors<-c("Maximum.altitude","Arable.horticultural", "Grassland", "Mountain.heath.bog","Urban.extent","ASPBAR..?..",         
              "BFIHOST....", "DPSBAR..m.km.","PROPWET")


v1<-match(Predictors,colnames(M_ALL_0.01_perc))

#for(hh in 1:8){
 # hh<-c(3,4,1)
hh<-4
  k<-combinations(8, hh, v=c(52,53,54,55,56,57,58,60), set=TRUE, repeats.allowed=FALSE)
  
  #for(ff in 1:nrow(k)){
  
    #ff<-c(8,24,8)
  ff<-24
  
  
  Density_perc<-data.frame(matrix(,nrow=1006,ncol=10))
  colnames(Density_perc)<-c("CODE","Selected","DEP_MeteoHydro","DEP_MeteoClima","DEP_ClimaHydro","DEP_3Ind","IND_MeteoHydro","IND_MeteoClima","IND_ClimaHydro","IND_3Ind")
  
  
    #Density_perc$CODE[xxx]<-paste(hh,ff)
    #Density_perc$Selected[xxx]<-paste(colnames(M_ALL_0.01_POS_perc)[c(50, k[ff,])], collapse = ' , ')
    
    ############ FINAL DRIVER SELECTED FOR HYDROLOGICAL INDEX #############
    
    
    Final_D<-c("50 Altitude","BFIHOST....","SPRHOST","LDP..km.", "Mountain.heath.bog" , 
               "Arable.horticultural","Catchment.area","PROPWET")
    
    #Final_D<-c("50 Altitude","BFIHOST....","SPRHOST","LDP..km.","Catchment.area","PROPWET")
  
    M1_DEP_0[,2]<- apply(M_ALL_0.01_POS_perc[,Final_D],1,mean)   
    M1_IND_0[,2]<- apply(M_ALL_IND_perc0.01[,Final_D],1,mean)   
    
    ############# Not consider NA in Hydro Index ##################################
    
    M1_DEP<-M1_DEP_0[complete.cases(M1_DEP_0[,1:3]),]
    M1_IND<-M1_IND_0[complete.cases(M1_IND_0[,1:3]),]
    
    final_length_dep<-nrow(M1_DEP)
    final_length_ind<-nrow(M1_IND)
    #Print indexes
    
    title=print(paste0("Hydrological Indexes selected: ",colnames(M_ALL_0.01_POS_perc[, Final_D])))
    
#    M1_DEP$Eucl_dist<-sqrt((1/4)*(M1_DEP$Hydrology)^2+(3/4)*(M1_DEP$Climatology)^2)
    
    
    
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
                      xlab="Meteorological Dissimilarity Index",ylab="Hydrological Dissimilarity Index",at=c(seq(-1,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)))
    
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
                      xlab="Meteorological Dissimilarity Index",ylab="Climatological Dissimilarity Index",at=c(seq(-1,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45))) 
    
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
                      xlab="Climatological Index",ylab="Hydrological Index",at=c(seq(-1,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)))
    
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
    
    P4_DEP<-levelplot(t(M_grid4_DEP), col.regions = coul, main="DEPENDENT", 
                      xlab="Climatological Dissimilarity Index",ylab="Hydrological Dissimilarity Index",at=c(seq(0,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)))
    
    # P4_DEP<-levelplot(t(M_grid4_DEP), col.regions = coul, main="DEPENDENT", 
    #                   xlab="Climatological DEPex",ylab="Hydrological DEPex",at=c(seq(0,1,0.05)),colorkey=FALSE, scales=list( y=list(rot=0), x=list(rot=45)))
    # 
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
                      xlab="Meteorological Dissimilarity Index",ylab="Hydrological Dissimilarity Index",at=c(seq(-1,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)))
    
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
                      xlab="Meteorological Dissimilarity Index",ylab="Climatological Dissimilarity Index",at=c(seq(-1,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)))
    
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
                      xlab="Climatological Index",ylab="Hydrological Index",at=c(seq(-1,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)))
    
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
    
    #CHOOSE COLORSCALE
    #coul <- viridis(100)
    #coul<- rainbow(100)
    coul<- tol.rainbow(199)
    
    
    P4_IND<-levelplot(t(M_grid4_IND), col.regions = coul, main="INDEPENDENT", 
                      xlab="Climatological Dissimilarity Index",ylab="Hydrological Dissimilarity Index",at=c(seq(0,1,0.05)) , scales=list( y=list(rot=0), x=list(rot=45)),
                      panel = panel.levelplot.raster)
    
    Density_perc$IND_3Ind[xxx]<-length(which(is.na(M_grid4_IND[1:10,1:10])==FALSE))/length(which(is.na(M_grid4_IND[,])==FALSE))*100
    
    ##################FINAL PLOT FOR PAPER#########################################################
    
    ##################WITH LETTERS#################
    
    P1_DEP<-levelplot(t(M_grid1_DEP), col.regions = coul, cex.main=0.05, 
                      xlab=expression('I '[A]),ylab=expression('I '[H]),
                      at=c(seq(0,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)),colorkey=FALSE )
    #colorkey=FALSE , 
    P2_DEP<-levelplot(t(M_grid2_DEP), col.regions = coul,cex.main=0.05, 
                      xlab=expression('I '[A]),ylab=expression('I '[C]),
                      at=c(seq(0,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)),colorkey=FALSE ) 
    
    P3_DEP<-levelplot(t(M_grid3_DEP), col.regions = coul,cex.main=0.05 ,
                      xlab=expression('I '[C]),ylab=expression('I '[H]),
                      at=c(seq(0,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)),colorkey=FALSE )
    
    P1_IND<-levelplot(t(M_grid1_IND), col.regions = coul, main="",cex=0.05,
                      xlab=expression('I '[A]),ylab=expression('I '[H]),
                      at=c(seq(0,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)),colorkey=FALSE  )
    
    P2_IND<-levelplot(t(M_grid2_IND), col.regions = coul,  main="",cex.main=0.05,
                      xlab=expression('I '[A]),ylab=expression('I '[C]),
                      at=c(seq(0,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)),
                      colorkey=FALSE )
    
    P3_IND<-levelplot(t(M_grid3_IND), col.regions = coul, main=" ",cex.main=0.05, 
                      xlab=expression('I '[C]),ylab=expression('I '[H]),at=c(seq(0,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)),
                      colorkey=FALSE )
    
    #easy way
    G1<-grid.arrange(P1_DEP, P1_IND, P2_DEP, P2_IND, P3_DEP, P3_IND,nrow=4,ncol=2)
    
    #G2<-grid.arrange(P1_DEP, P1_IND, P2_DEP, P2_IND, P3_DEP, P3_IND,nrow=3,ncol=2,labels=c("a)","b)","c","d","e","f"))
    
    #final plot works
    G1<-grid.arrange(P1_DEP, P1_IND, P2_DEP, P2_IND, P3_DEP, P3_IND,nrow=3,ncol=2, 
                     top = textGrob("DEPENDENT                                                                     INDEPENDENT",gp=gpar(fontsize=18)), heights=c(0.3,0.3,0.3))
    
    
    #ggsave("C:/Users/39349/Documents/Regional/Plot/DDMAP_AFTERREVIEW_coulrain.pdf",G1, width = 29, height = 34, units = "cm")
    
    ggsave("07.DDMAP2_new.jpeg",G1, units="in", dpi=400, width=10.28,height=16.76)

    
     ###PUT IN THIS SITE:https://pdftoimage.com/it/##
    #######    save plot and then put in pAINT
    
  
    
    ####PLOT FOR COLORSCALE, that has to be added
    
    G4<-levelplot(t(M_grid3_DEP), col.regions = coul, cex.main=0.05, 
                      xlab="Kendall's Tau",ylab=expression('I '[H]),
                      at=c(seq(0,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)),colorkey=list(space="bottom",tick.number=20),
                      par.settings = list(layout.heights=list(xlab.key.padding=1)))
    
    fg<-ggarrange(G4,nrow=1)
    ggsave("LEGEND_AFTERREVIEW_coulrain.pdf",fg, width = 20, height = 36, units = "cm")
    ggsave("07.DDMAP_LEGEND.jpeg",fg, units="in", dpi=400, width=10.28,height=16.76)
    
    
    ###########JUST METEO HYDRO CLIMA############
    
    
    P4_DEP<-levelplot(t(M_grid4_DEP), col.regions = coul, main="DEPENDENT", 
                      xlab=expression('I '[C]),ylab=expression('I '[H]),at=c(seq(-1,1,0.05)), 
                      scales=list( y=list(rot=0), x=list(rot=45)), cex.axis=1.2)
    
    
    P4_IND<-levelplot(t(M_grid4_IND), col.regions = coul, main="INDEPENDENT", 
                      xlab=expression('I '[C]),ylab=expression('I '[H]),at=c(seq(-1,1,0.05)) , 
                      scales=list( y=list(rot=0), x=list(rot=45)), cex.axis=1.2)
    
    
    grid.arrange(P4_DEP,P4_IND, nrow=1,ncol=2)
    
    #save.image(im, file, quality = 0.7)
    #SAVE IMAGE
    
    
    ######FINAL PLOT##############
    G1<-grid.arrange(P1_DEP, P1_IND, P2_DEP, P2_IND, P3_DEP, P3_IND,nrow=3,ncol=2, 
                     top = textGrob("DEPENDENT                                                                     INDEPENDENT",gp=gpar(fontsize=18)), heights=c(0.3,0.3,0.3) )
    
    ggsave("DDMAP_DEP_IND8_new.pdf",G1, width = 29, height = 34, units = "cm")
    
    grid.arrange(P4_DEP,P4_IND, nrow=1,ncol=2)
    
    save.image(im, file, quality = 0.7)
    
    
    
    ####BOXPLOT#########
    
    png(paste0("05.BOXPLOT_6_lag",Lag_time,".jpeg"), units="in", res=400, height=6.28,width =9.3)
    
    #Depdep-DepInd
    par(mfrow=c(2,3))
    
    boxplot(M1_DEP$Meteo[M1_DEP$Class=="Depdep"],M1_DEP$Meteo[M1_DEP$Class=="Depind"],M1_DEP$Meteo[M1_DEP$Class=="High Syn"]
            ,names=c("Dep-Dep","Dep-Ind","High Syn"),ylim=c(0,1),outline=FALSE, cex.main=1.5,cex.axis=1.3, 
            cex.lab=1.5,ylab="Index")
    
    title("a) Asynchrony", adj =0.97, line = -1)
    
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
    
    #    boxplot(M1_DEP$Eucl_dist[M1_DEP$Class=="Depdep"],M1_DEP$Eucl_dist[M1_DEP$Class=="Depind"],M1_DEP$Eucl_dist[M1_DEP$Class=="High Syn"]
    #            ,names=c("DepDep","DepInd","High Syn"), main="Euclidean Distance",ylim=c(0,1),outline=FALSE, cex.main=1.5,cex.lab=1.1)
    
    
    
    dev.off()
    
    #Dependent- Independent
    
    ######MAKE transparent colors ################
    add.alpha <- function(col, alpha=1){
      if(missing(col))
        stop("Please provide a vector of colours.")
      apply(sapply(col, col2rgb)/255, 2, 
            function(x) 
              rgb(x[1], x[2], x[3], alpha=alpha))  
    }
    
    myColours<-c("#33CCCC","#FF3333")
    
    myColoursAlpha <- add.alpha(myColours, alpha=0.3)
    
    ##########################################
    
    
    png(paste0("05.BOXPLOT_3_lag",Lag_time,".jpeg"), units="in", res=400, height=6.28,width =8.7)
    
    par(mfrow=c(1,3))
    
    boxplot(M1_DEP$Meteo,M1_IND$Meteo
            ,names=c("Dependent","Independent"), main="a) Asynchrony",ylim=c(0,1),outline=FALSE, ylab="Index", cex.main=1.5, alpha=0.3,
            cex.lab=1.5 ,cex.axis=1.3,col= myColoursAlpha)
    
    boxplot(M1_DEP$Climatology,M1_IND$Climatology
            ,names=c("Dependent","Independent"), main="b) Climatology",ylim=c(0,1),outline=FALSE,ylab="Dissimilarity Index", cex.main=1.5, 
            cex.lab=1.5,cex.axis=1.3,col= myColoursAlpha)
    
    boxplot(M1_DEP$Hydrology,M1_IND$Hydrology
            ,names=c("Dependent","Independent"), main="c) Hydrology",ylim=c(0,1),outline=FALSE,ylab="Dissimilarity Index", cex.main=1.5, 
            cex.lab=1.5,cex.axis=1.3,col=myColoursAlpha)
    
    ##Euclidean Distanc3
    dev.off()
    
    
 ############################################################################################################?????   
    
    
    
    
    
    
    
    
    #############old code##########################
    
    
    
    

    ###############################################???????????????????????????????????????????????????????????
    
    
    #Dependent
    grid.arrange(P1_DEP,P2_DEP, P3_DEP,P4_DEP,text.p, nrow=3,ncol=2,
                 top = textGrob("DEPENDENT",gp=gpar(fontsize=20,font=3)),
                 heights = c(0.7, 0.7, 0.2))
    #Independent
    grid.arrange(P1_IND,P2_IND,P3_IND,P4_IND,nrow=2,ncol=2, 
                 top = textGrob("INDEPENDENT COUPLES",gp=gpar(fontsize=18,font=3)))
    
    g1<- grid.arrange(P1_DEP, P1_IND, P2_DEP, P2_IND, P3_DEP, P4_IND,nrow=3,ncol=2, 
                 top = textGrob("INDEPENDENT COUPLES",gp=gpar(fontsize=18,font=3)) )
    
    ggsave("DDMAP_DEPIND.pdf",g1, width = 32, height = 34, units = "cm")
    
    
    
    
    
    ########### 3 D SCATTERPLOT    ##################
    par(mfrow=c(1,1))
    dfm<-M1_DEP[which(M1_DEP$X.1==1 | M1_DEP$X.1==2 |  M1_DEP$X.1==3),]
    
    mycolors <- c('darkred', 'darkcyan','yellow')
    dfm$color <- mycolors[ as.numeric(dfm$X.1) ]
    
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
    grid.arrange(P1_IND,P2_IND,P3_IND,P4_IND,nrow=2,ncol=2, 
                 top = textGrob("INDEPENDENT COUPLES",gp=gpar(fontsize=18,font=3)))
    
    #grid.arrange(P1_IND,P2_IND,P3_IND,P4_IND, text.p,nrow=3,ncol=2, 
    #             top = textGrob("INDEPENDENT",gp=gpar(fontsize=20,font=3)),
    #             heights = c(0.7, 0.7, 0.2))
    
    #ggplot(M1_DEP,aes(x=Climatology,y=Hydrology, colour=Class,shape=Class,size=Class))+
    # geom_point(fill=Class)  +
    #  scale_size_manual(values=c(4,4,1))+ xlim(0, 1) + ylim(0, 1)+ 
    # labs(title = "Clima-Hydro") + scale_color_manual(values=c("deepskyblue4", "green", "red")) +
    #  guides(shape = guide_legend(override.aes = list(size =2)))+
    # facet_grid(. ~ Climatology ,space = "free", scales = "free", margins = T)
    
    ################BOXPLOT##########################################
    setwd("C:/Users/39349/Documents/Regional/Plot/FinalPlot")
    
    #png("BoxplotDepdepDepInd.png", width=16, height=10, units="in", res=300)
    

    
    par(mfrow=c(1,2))   
    
    hist(M1_DEP$Eucl_dist[M1_DEP$Class=="Depdep"])
    hist(M1_DEP$Eucl_dist[M1_DEP$Class=="Depind"])
    
    M1_DEP$Combin<-M1_DEP$Hydrology*M1_DEP$Climatology
    
    boxplot(M1_DEP$Combin[M1_DEP$Class=="Depdep"],M1_DEP$Combin[M1_DEP$Class=="Depind"],M1_DEP$Combin[M1_DEP$Class=="High Syn"]
            ,names=c("DepDep","DepInd","High Syn"), main="Combination",ylim=c(0,1),outline=FALSE, cex.main=1.5)
    
    
    
    
    
    
 #   xxx<-xxx+1
#  }
#}

dev.off ()

write.table(Density_perc, file=paste0("Percentages_",pvalue,vv,".txt"))

par(mfrow=c(2,3))

boxplot(M1_DEP$Climatology[M1_DEP$Class=="Depdep"],M1_DEP$Climatology[M1_DEP$Class=="Depind"],M1_DEP$Climatology[M1_DEP$Class=="High Syn"]
        ,names=c("DepDep","DepInd","High Syn"), main="Clima",ylim=c(0,1),outline=FALSE)

boxplot(M1_DEP$Meteo[M1_DEP$Class=="Depdep"],M1_DEP$Meteo[M1_DEP$Class=="Depind"],M1_DEP$Meteo[M1_DEP$Class=="High Syn"]
        ,names=c("DepDep","DepInd","High Syn"), main="Meteo",ylim=c(0,1),outline=FALSE)

boxplot(M1_DEP$Hydrology[M1_DEP$Class=="Depdep"],M1_DEP$Hydrology[M1_DEP$Class=="Depind"],M1_DEP$Hydrology[M1_DEP$Class=="High Syn"]
        ,names=c("DepDep","DepInd","High Syn"), main="Hydro",ylim=c(0,1),outline=FALSE)

boxplot(M1_DEP$KendallTau[M1_DEP$Class=="Depdep"],M1_DEP$KendallTau[M1_DEP$Class=="Depind"],M1_DEP$KendallTau[M1_DEP$Class=="High Syn"]
        ,names=c("DepDep","DepInd","High Syn"), main="KendallTau",ylim=c(0,1),outline=FALSE)

boxplot(M1_DEP$Distance[which(M1_DEP$X.1==1)],M1_DEP$Distance[which(M1_DEP$X.1==2)],names=c("DepInd","Depdep"))



par(mfrow=c(2,2))
boxplot(M1_DEP$Meteo,M1_IND$Meteo
        ,names=c("Dependent","Independent"), main="Meteo",outline=FALSE)

boxplot(M1_DEP$Climatology,M1_IND$Climatology
        ,names=c("Dependent","Independent"), main="Climatology",outline=FALSE)

boxplot(M1_DEP$Hydrology,M1_IND$Hydrology
        ,names=c("Dependent","Independent"), main="Hydrology",outline=FALSE)

par(mfrow=c(2,2))

boxplot(M1_DEP$Climatology,M1_IND$Climatology
        ,names=c("Dependent","Independent"), main="Climatology",outline=FALSE)

boxplot(M1_DEP$Hydrology,M1_IND$Hydrology
        ,names=c("Dependent","Independent"), main="Hydrology",outline=FALSE)

