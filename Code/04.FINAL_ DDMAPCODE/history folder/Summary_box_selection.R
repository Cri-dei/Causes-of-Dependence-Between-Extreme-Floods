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

print( "The pvalue selected is 0.01")
pvalue<-0.01
path<-paste0("pvalue",pvalue)

# Write the name of version you are running##########
vv<-c("MaxAlt_fixed_Final2")

#Cristina
load("C:/Users/39349/Documents/Regional/Workspace/ALLDATA01_andonlyPOSITIVE.RData")
load("C:/Users/39349/Documents/Regional/Workspace/M_ALL_INDIPENDENT_perc.RData")
#load("C:/Users/39349/Documents/Regional/Workspace/M_ALL_0015.RData")

#Leila
#load("C:/Users/EMERTAT-pc/Downloads/ALLDATA01_andonlyPOSITIVE.RData")
#load("C:/Users/EMERTAT-pc/Downloads/M_ALL_INDIPENDENT_perc.RData")



# ###########INFO The final matrix are ########### #
#M_ALL_IND_perc: for indipendent couples
#M_ALL_0.01_POS_perc : for dependent couples

##################################
############################# CHANGING THRESHOLD PVALUE #################################
#Selection as dependent couples with pvalue:0.015

for( i in 1:nrow(M_ALL_0.01_POS_perc))
{
  if(M_ALL_0.01_POS_perc$KendalT.p.value[i]<=pvalue){M_ALL_0.01_POS_perc$X[i]<-2}
  else{M_ALL_0.01_POS_perc$X[i]<-1}
}


############################# ASY PART #################################
#Selection of Async data with more than 20 data and pvalue 0.01 #
# 
# for( i in 1:nrow(M_ALL_0.01_POS_perc))
# {
#   if(M_ALL_0.01_POS_perc$Num_Asyncr_occ[i]<20){M_ALL_0.01_POS_perc$X.1[i]<-NA}
#   else{ if(M_ALL_0.01_POS_perc$KT_pvalue_Asy[i]<=0.015){M_ALL_0.01_POS_perc$X.1[i]<-2} else{M_ALL_0.01_POS_perc$X.1[i]<-1}
#   }
# }
# 
# for( i in 1:nrow(M_ALL_IND_perc))
# {
#   if( M_ALL_IND_perc$Num_Asyncr_occ[i]<20){M_ALL_IND_perc$X.1[i]<-NA}
#   else{ if(M_ALL_IND_perc$KT_pvalue_Asy[i]<=0.01){M_ALL_IND_perc$X.1[i]<-2} else{M_ALL_IND_perc$X.1[i]<-1}
#   }
# }

M_ALL_0.01_POS_perc$Class[M_ALL_0.01_POS_perc$N.SY.N.ALL>=0.60]<-"High Syn"
M_ALL_0.01_POS_perc$X.1[M_ALL_0.01_POS_perc$N.SY.N.ALL>=0.60]<-c(3)
M_ALL_0.01_POS_perc$Class[M_ALL_0.01_POS_perc$X.1==1]<-"Depind"
M_ALL_0.01_POS_perc$Class[M_ALL_0.01_POS_perc$X.1==2]<-"Depdep"


M_ALL_IND_perc0.01<-M_ALL_IND_perc[which(M_ALL_IND_perc$KendalT.p.value>pvalue),]

###################### 1. DISSIMILARITY INDEXES ################################

###########################CHOOSE INITIAL MATRIX#################################

## DEPENDENT COUPLES
M1_DEP_0<-M_ALL_0.01_POS_perc[,c(15,57,69,4,68,18,25)]

colnames(M1_DEP_0)<-c("Meteo","Hydrology","Climatology","KendallTau","Class","X.1","Distance")


## DEPENDENT COUPLES
M1_IND_0<-M_ALL_IND_perc0.01[,c(15,57,69,4,68,18,25)]

colnames(M1_IND_0)<-c("Meteo","Hydrology","Climatology","KendallTau","Class","X.1","Distance")



########## Choose Meterological Index  ############
#Number of syncrony /N all
M1_DEP_0[,1]<- 1-M1_DEP_0[,1]  
M1_IND_0[,1]<- 1-M1_IND_0[,1]  

######## Choose Climatological Index
# SAAR

M1_DEP_0[,3]<- M_ALL_0.01_POS_perc$SAAR_61.90
M1_IND_0[,3]<- M_ALL_IND_perc0.01$SAAR_61.90





Summary_boxplot<-data.frame(matrix(,nrow=256,ncol=20))
colnames(Summary_boxplot)<-c("CODE","Selected","ALL_IND-DEP:METEO_MEDIAN","ALL_IND-DEP:METEO_1QUANT","ALL_IND-DEP:METEO_3QUANT",
                             "ALL_IND-DEP:Hydrology_MEDIAN","ALL_IND-DEP:Hydrology_1QUANT","ALL_IND-DEP:Hydrology_3QUANT",
                             "ALL_IND-DEP:Climatology_MEDIAN","ALL_IND-DEP:Climatology_1QUANT","ALL_IND-DEP:Climatology_3QUANT",
                              "DEPIND-DEPDEP:METEO_MEDIAN","DEPIND-DEPDEP:METEO_1QUANT","DEPIND-DEPDEP:METEO_3QUANT",
                             "DEPIND-DEPDEP:Hydrology_MEDIAN","DEPIND-DEPDEP:Hydrology_1QUANT","DEPIND-DEPDEP:Hydrology_3QUANT",
                             "DEPIND-DEPDEP:Climatology_MEDIAN","DEPIND-DEPDEP:Climatology_1QUANT","DEPIND-DEPDEP:Climatology_3QUANT")

Summary_boxplot2<-data.frame(matrix(,nrow=256,ncol=14))
colnames(Summary_boxplot2)<-c("CODE","Selected","ALL_IND-DEP_METEO","ALL_IND-DEP_Hydrology","ALL_IND-DEP_Climatology",
                              "DEPIND-DEPDEP:METEO_MEDIAN","DEPIND-DEPDEP:METEO_1QUANT","DEPIND-DEPDEP:METEO_3QUANT",
                              "DEPIND-DEPDEP:Hydrology_MEDIAN","DEPIND-DEPDEP:Hydrology_1QUANT","DEPIND-DEPDEP:Hydrology_3QUANT",
                              "DEPIND-DEPDEP:Climatology_MEDIAN","DEPIND-DEPDEP:Climatology_1QUANT","DEPIND-DEPDEP:Climatology_3QUANT")



#setwd("C:/Users/EMERTAT-pc/Downloads/Graph")
setwd(paste0("C:/Users/39349/Documents/Regional/Plot/DDMAP/",path))
pdf(file=paste0("02.BOXPLOT_pv",pvalue,"_",vv,".pdf"),width=9, height=9)  
xxx<-1


for(hh in 1:8){
  k<-combinations(8, hh, v=c(52,53,54,55,56,57,58,60), set=TRUE, repeats.allowed=FALSE)
  
  for(ff in 1:nrow(k)){
    
    Summary_boxplot$CODE[xxx]<-paste(hh,ff)
    Summary_boxplot$Selected[xxx]<-paste(colnames(M_ALL_0.01_POS_perc)[c(50, k[ff,])], collapse = ' , ')
    colnames(M_ALL_0.01_POS_perc)[c(50, k[ff,])]
    
    Summary_boxplot2$CODE[xxx]<-paste(hh,ff)
    Summary_boxplot2$Selected[xxx]<-paste(colnames(M_ALL_0.01_POS_perc)[c(50, k[ff,])], collapse = ' , ')
    colnames(M_ALL_0.01_POS_perc)[c(50, k[ff,])]
    
    M1_DEP_0[,2]<- apply(M_ALL_0.01_POS_perc[,c(50, k[ff,])],1,mean)   
    M1_IND_0[,2]<- apply(M_ALL_IND_perc0.01[,c(50, k[ff,])],1,mean)   
    
    ############# Not consider NA in Hydro Index ##################################
    
    M1_DEP<-M1_DEP_0[complete.cases(M1_DEP_0[,1:3]),]
    M1_IND<-M1_IND_0[complete.cases(M1_IND_0[,1:3]),]
    
    final_length_dep<-nrow(M1_DEP)
    final_length_ind<-nrow(M1_IND)
    #Print indexes
    
    title=print(paste0("Hydrological Indexes selected: ",colnames(M_ALL_0.01_POS_perc[,c(50, k[ff,])])))
    
    ################BOXPLOT##########################################
    #Dependent- Independent
    par(mfrow=c(2,2))
    
    boxplot(M1_DEP$Distance[which(M1_DEP$Class=="Depdep")],M1_DEP$Distance[which(M1_DEP$Class=="Depind")],M1_DEP$Distance[which(M1_DEP$Class=="High Syn")],
            names=c("DepDep","DepInd","High Syn"), main=paste(Summary_boxplot$CODE[xxx],"Distance Groups"))
    
    boxplot(M1_DEP$Meteo,M1_IND$Meteo
            ,names=c("Dependent","Independent"), main="ALL: Meteo",ylim=c(0,1))
    
    boxplot(M1_DEP$Climatology,M1_IND$Climatology
            ,names=c("Dependent","Independent"), main="ALL: Climatology",ylim=c(0,1))
    
    boxplot(M1_DEP$Hydrology,M1_IND$Hydrology
            ,names=c("Dependent","Independent"), main="ALL: Hydrology",ylim=c(0,1))
    
    
    Summary_boxplot$`ALL_IND-DEP:METEO_MEDIAN`[xxx]<- summary(M1_IND$Meteo)[3]-summary(M1_DEP$Meteo)[3]
    Summary_boxplot$`ALL_IND-DEP:METEO_1QUANT`[xxx]<- summary(M1_IND$Meteo)[2]-summary(M1_DEP$Meteo)[2]
    Summary_boxplot$`ALL_IND-DEP:METEO_3QUANT`[xxx]<- summary(M1_IND$Meteo)[5]-summary(M1_DEP$Meteo)[5]
    
    Summary_boxplot$`ALL_IND-DEP:Hydrology_MEDIAN`[xxx]<- summary(M1_IND$Hydrology)[3]-summary(M1_DEP$Hydrology)[3]
    Summary_boxplot$`ALL_IND-DEP:Hydrology_1QUANT`[xxx]<- summary(M1_IND$Hydrology)[2]-summary(M1_DEP$Hydrology)[2]
    Summary_boxplot$`ALL_IND-DEP:Hydrology_3QUANT`[xxx]<- summary(M1_IND$Hydrology)[5]-summary(M1_DEP$Hydrology)[5]
    
    Summary_boxplot$`ALL_IND-DEP:Climatology_MEDIAN`[xxx]<- summary(M1_IND$Climatology)[3]-summary(M1_DEP$Climatology)[3]
    Summary_boxplot$`ALL_IND-DEP:Climatology_1QUANT`[xxx]<- summary(M1_IND$Climatology)[2]-summary(M1_DEP$Climatology)[2]
    Summary_boxplot$`ALL_IND-DEP:Climatology_3QUANT`[xxx]<- summary(M1_IND$Climatology)[5]-summary(M1_DEP$Climatology)[5]
    
    #DIFFERENCE 3QUART IND- 1QUART DEP
    
    Summary_boxplot2$`ALL_IND-DEP_METEO`[xxx]<-summary(M1_IND$Meteo)[2]-summary(M1_DEP$Meteo)[5]
    Summary_boxplot2$`ALL_IND-DEP_Hydrology`[xxx]<-summary(M1_IND$Hydrology)[2]-summary(M1_DEP$Hydrology)[5]
    Summary_boxplot2$`ALL_IND-DEP_Climatology`[xxx]<-summary(M1_IND$Climatology)[2]-summary(M1_DEP$Climatology)[5]
    
    
    #Depdep-DepInd
    par(mfrow=c(2,2))
    
    boxplot(M1_DEP$Climatology[M1_DEP$Class=="Depdep"],M1_DEP$Climatology[M1_DEP$Class=="Depind"],M1_DEP$Climatology[M1_DEP$Class=="High Syn"]
            ,names=c("DepDep","DepInd","High Syn"), main=paste(Summary_boxplot$CODE[xxx],"Clima"),ylim=c(0,1))
    
    boxplot(M1_DEP$Meteo[M1_DEP$Class=="Depdep"],M1_DEP$Meteo[M1_DEP$Class=="Depind"],M1_DEP$Meteo[M1_DEP$Class=="High Syn"]
            ,names=c("DepDep","DepInd","High Syn"), main="Meteo",ylim=c(0,1))
    
    boxplot(M1_DEP$Hydrology[M1_DEP$Class=="Depdep"],M1_DEP$Hydrology[M1_DEP$Class=="Depind"],M1_DEP$Hydrology[M1_DEP$Class=="High Syn"]
            ,names=c("DepDep","DepInd","High Syn"), main="Hydro",ylim=c(0,1))
    
    boxplot(M1_DEP$KendallTau[M1_DEP$Class=="Depdep"],M1_DEP$KendallTau[M1_DEP$Class=="Depind"],M1_DEP$KendallTau[M1_DEP$Class=="High Syn"]
            ,names=c("DepDep","DepInd","High Syn"), main="KendallTau",ylim=c(0,1))
    
    Summary_boxplot$`DEPIND-DEPDEP:METEO_MEDIAN`[xxx]<- summary(M1_DEP$Meteo[M1_DEP$Class=="Depind"])[3]-summary(M1_DEP$Meteo[M1_DEP$Class=="Depdep"])[3]
    Summary_boxplot$`DEPIND-DEPDEP:METEO_1QUANT`[xxx]<- summary(M1_DEP$Meteo[M1_DEP$Class=="Depind"])[2]-summary(M1_DEP$Meteo[M1_DEP$Class=="Depdep"])[2]
    Summary_boxplot$`DEPIND-DEPDEP:METEO_3QUANT`[xxx]<- summary(M1_DEP$Meteo[M1_DEP$Class=="Depind"])[5]-summary(M1_DEP$Meteo[M1_DEP$Class=="Depdep"])[5]
    
    Summary_boxplot$`DEPIND-DEPDEP:Hydrology_MEDIAN`[xxx]<- summary(M1_DEP$Hydrology[M1_DEP$Class=="Depind"])[3]-summary(M1_DEP$Hydrology[M1_DEP$Class=="Depdep"])[3]
    Summary_boxplot$`DEPIND-DEPDEP:Hydrology_1QUANT`[xxx]<- summary(M1_DEP$Hydrology[M1_DEP$Class=="Depind"])[2]-summary(M1_DEP$Hydrology[M1_DEP$Class=="Depdep"])[2]
    Summary_boxplot$`DEPIND-DEPDEP:Hydrology_3QUANT`[xxx]<- summary(M1_DEP$Hydrology[M1_DEP$Class=="Depind"])[5]-summary(M1_DEP$Hydrology[M1_DEP$Class=="Depdep"])[5]
    
    Summary_boxplot$`DEPIND-DEPDEP:Climatology_MEDIAN`[xxx]<- summary(M1_DEP$Climatology[M1_DEP$Class=="Depind"])[3]-summary(M1_DEP$Climatology[M1_DEP$Class=="Depdep"])[3]
    Summary_boxplot$`DEPIND-DEPDEP:Climatology_1QUANT`[xxx]<- summary(M1_DEP$Climatology[M1_DEP$Class=="Depind"])[2]-summary(M1_DEP$Climatology[M1_DEP$Class=="Depdep"])[2]
    Summary_boxplot$`DEPIND-DEPDEP:Climatology_3QUANT`[xxx]<- summary(M1_DEP$Climatology[M1_DEP$Class=="Depind"])[5]-summary(M1_DEP$Climatology[M1_DEP$Class=="Depdep"])[5]
    
    #DIFFERENCE 3QUART DEPIND- 1QUART DEPDEP
    
    #Summary_boxplot2$`DEPIND-DEPDEP_METEO`[xxx]<-summary(M1_DEP$Meteo[M1_DEP$Class=="Depind"])[2]-summary(M1_DEP$Meteo[M1_DEP$Class=="Depdep"])[5]
    #Summary_boxplot2$`DEPIND-DEPDEP_Hydrology`[xxx]<-summary(M1_DEP$Hydrology[M1_DEP$Class=="Depind"])[2]-summary(M1_DEP$Hydrology[M1_DEP$Class=="Depdep"])[5]
    #Summary_boxplot2$`DEPIND-DEPDEP_Climatology`[xxx]<-summary(M1_DEP$Climatology[M1_DEP$Class=="Depind"])[2]-summary(M1_DEP$Climatology[M1_DEP$Class=="Depdep"])[5]
    
    Summary_boxplot2$`DEPIND-DEPDEP:METEO_MEDIAN`[xxx]<- summary(M1_DEP$Meteo[M1_DEP$Class=="Depind"])[3]-summary(M1_DEP$Meteo[M1_DEP$Class=="Depdep"])[3]
    Summary_boxplot2$`DEPIND-DEPDEP:METEO_1QUANT`[xxx]<- summary(M1_DEP$Meteo[M1_DEP$Class=="Depind"])[2]-summary(M1_DEP$Meteo[M1_DEP$Class=="Depdep"])[2]
    Summary_boxplot2$`DEPIND-DEPDEP:METEO_3QUANT`[xxx]<- summary(M1_DEP$Meteo[M1_DEP$Class=="Depind"])[5]-summary(M1_DEP$Meteo[M1_DEP$Class=="Depdep"])[5]
    
    Summary_boxplot2$`DEPIND-DEPDEP:Hydrology_MEDIAN`[xxx]<- summary(M1_DEP$Hydrology[M1_DEP$Class=="Depind"])[3]-summary(M1_DEP$Hydrology[M1_DEP$Class=="Depdep"])[3]
    Summary_boxplot2$`DEPIND-DEPDEP:Hydrology_1QUANT`[xxx]<- summary(M1_DEP$Hydrology[M1_DEP$Class=="Depind"])[2]-summary(M1_DEP$Hydrology[M1_DEP$Class=="Depdep"])[2]
    Summary_boxplot2$`DEPIND-DEPDEP:Hydrology_3QUANT`[xxx]<- summary(M1_DEP$Hydrology[M1_DEP$Class=="Depind"])[5]-summary(M1_DEP$Hydrology[M1_DEP$Class=="Depdep"])[5]
    
    Summary_boxplot2$`DEPIND-DEPDEP:Climatology_MEDIAN`[xxx]<- summary(M1_DEP$Climatology[M1_DEP$Class=="Depind"])[3]-summary(M1_DEP$Climatology[M1_DEP$Class=="Depdep"])[3]
    Summary_boxplot2$`DEPIND-DEPDEP:Climatology_1QUANT`[xxx]<- summary(M1_DEP$Climatology[M1_DEP$Class=="Depind"])[2]-summary(M1_DEP$Climatology[M1_DEP$Class=="Depdep"])[2]
    Summary_boxplot2$`DEPIND-DEPDEP:Climatology_3QUANT`[xxx]<- summary(M1_DEP$Climatology[M1_DEP$Class=="Depind"])[5]-summary(M1_DEP$Climatology[M1_DEP$Class=="Depdep"])[5]
    
    
    xxx<-xxx+1
  }
}

dev.off ()

write.table(Summary_boxplot, file=paste0("Summary_Boxplot_",pvalue,".txt"))
write.table(Summary_boxplot2, file=paste0("Difference_13quart_",pvalue,".txt"))


