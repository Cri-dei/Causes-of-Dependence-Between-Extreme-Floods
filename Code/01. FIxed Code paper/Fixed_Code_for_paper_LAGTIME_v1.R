#######Author: Cristina Deidda ##############
#######QQ DATA PROCESSING CODE##############
#What is done:
#1) Load Matrix KT for all the couples
#2) Eliminate column for which we don't have coordinates
#3) Select just the couples with 20 data in commmon
#3) create m_dep perc and m_ind perc
#4) Save M_all for each Lag time selected
# Added Bonferroni adjustment

library(plotly)
library(gridExtra)
library(ggpubr)

#pc ufficio
#setwd("D:/PROJECTS/Regional/DISTANCE_selection/Data")
path<-c("C:/PROJECTS 2021/QQ")
#pc portatile
#path<-c("C:/Users/39349/Documents/Regional")

setwd(paste0(path,"/Data"))

#Load initial data
M_ALL_altitude<- read.table("Catch_info_Altitude.csv", header = TRUE, sep=",")
SAAR<-  read.table("SAAR.csv", header = TRUE, sep=",")
M_in<- read.table("Catch_info_final.csv", header = TRUE, sep=";")
SPRHOST<- data.frame(read.table("SPRHOST_coeff.csv", header = TRUE, sep=","))
                                                       

#Possible lag time= 5,7 #

################!  CHOOOSE LAG TIME  !##################?
#Lag_time<-c(5,7)

Lag_time<-10
Lag_time<-5
##############################################

setwd(paste0(path,"/Lag time/Lagtime_",Lag_time,"/InitialData"))
KT_M_Final_available_20_Num<-read.table("KT_M_Final_available_20_Num.csv", header = TRUE, sep=";") 


print(paste("Selected lag time is:",Lag_time, "days"))

M_ALL<-data.frame(KT_M_Final_available_20_Num)
#M_ALL$Distance<-as.numeric(as.character(M_ALL$Distance))
M_ALL$Distance<- M_ALL$Distance/1000 


##NOW M_ALL it is the same of KT_M_Final_available_20data_ok ##############
#FROM: load("C:/Users/39349/Documents/Regional/Data/Processed Data/Processed_KT_Matrix.RData")##
##We preferred import it from excel to have Numerical matrix##################################

#Wrong Data Station
Wrongst<- which(M_ALL$ID_Station_1=="39004" | M_ALL$ID_Station_2=="39004" )

M_ALL<- M_ALL[-Wrongst,]


### CATCHMENT INFO FOR ALL THE COUPLES: DEP AND IND ##############
   ######## CATCHMENT INFO #####################
######################################################################


#it is called dep_m but it is for all
DEP_m<-data.frame(M_ALL)


############# CREATION CATCHMENT INFO MATRIX ##################


CATCHM_DEPDEP<-data.frame(matrix( , nrow =nrow(DEP_m)*2, ncol =ncol(M_in)+5+3+1))
colnames(CATCHM_DEPDEP)<-c(colnames(M_in),"N_couple","Distance","Class","SAAR_61-90","SAAR_41-70","Min Altitude","50 Altitude","Max-Min Altitude","SPRHOST")
x<-1

for(i in 1:nrow(DEP_m))
{

  D1<- which(DEP_m$ID_Station_1[i]==M_in[,1])
  D2<- which(DEP_m$ID_Station_2[i]==M_in[,1])
  D3<- which(DEP_m$ID_Station_1[i]==SAAR[,1])
  D4<- which(DEP_m$ID_Station_2[i]==SAAR[,1])
  if(length(D1)!=0 & length(D2)!=0){
  CATCHM_DEPDEP[x,1:ncol(M_in)]<-as.numeric(M_in[D1,])
  CATCHM_DEPDEP[x+1,1:ncol(M_in)]<-as.numeric(M_in[D2,])
  CATCHM_DEPDEP$N_couple[x]<-DEP_m$CODE[i]
  CATCHM_DEPDEP$N_couple[x+1]<-DEP_m$CODE[i]
  CATCHM_DEPDEP$Distance[x]<-DEP_m$Distance[i]
  CATCHM_DEPDEP$Distance[x+1]<-DEP_m$Distance[i]
  CATCHM_DEPDEP$'SAAR_61-90'[x]<- SAAR[D3,5]
  CATCHM_DEPDEP$'SAAR_61-90'[x+1]<- SAAR[D4,5] 
  CATCHM_DEPDEP$'SAAR_41-70'[x]<- SAAR[D3,6]
  CATCHM_DEPDEP$'SAAR_41-70'[x+1]<- SAAR[D4,6]
  #add altitude
  D5<- which(DEP_m$ID_Station_1[i]==M_ALL_altitude[,1])
  D6<- which(DEP_m$ID_Station_2[i]==M_ALL_altitude[,1])
  if(length(D5)>0 & length(D6)>0){
  CATCHM_DEPDEP$`Min Altitude`[x]<- M_ALL_altitude[D5,4]
  CATCHM_DEPDEP$`Min Altitude`[x+1]<- M_ALL_altitude[D6,4]
  CATCHM_DEPDEP$`50 Altitude`[x]<- M_ALL_altitude[D5,6]
  CATCHM_DEPDEP$`50 Altitude`[x+1]<- M_ALL_altitude[D6,6]  
  CATCHM_DEPDEP$`Max-Min Altitude`[x]<- M_ALL_altitude[D5,8]- M_ALL_altitude[D5,4]
  CATCHM_DEPDEP$`Max-Min Altitude`[x+1]<- M_ALL_altitude[D6,8] -  M_ALL_altitude[D6,4] 
  #Add SPRHOST
  D7<- which(DEP_m$ID_Station_1[i]==SPRHOST$Station.number)
  D8<- which(DEP_m$ID_Station_2[i]==SPRHOST$Station.number)
  if(length(D7)>0& length(D8)>0)
  {CATCHM_DEPDEP$SPRHOST[x]<-SPRHOST$SPRHOST....[D7]
  CATCHM_DEPDEP$SPRHOST[x+1]<-SPRHOST$SPRHOST....[D8]} } 
    x<-x+2} 
}  
###########################################################



##############COMPUTATION #######################                 
       #### Percentage index#######
       
       
       
MATRIX2<-CATCHM_DEPDEP


r2<-(length(which(is.na(MATRIX2[,1])=="FALSE")))

NC<-which(colnames(MATRIX2)=="N_couple")
Cl<-which(colnames(MATRIX2)=="Class")
Bv<-nrow(MATRIX2)/2
Diff2<-MATRIX2[1:Bv,]

x<-1
for (i in 2:r2)
{
if (MATRIX2[i,NC]==MATRIX2[i-1,NC] )
{                      
 Diff2[x,]<-1-(pmin(MATRIX2[i,],MATRIX2[i-1,])/pmax(MATRIX2[i,],MATRIX2[i-1,]))
#Diff2[x,]<-abs((MATRIX2[i,]-MATRIX2[i-1,])/pmax(MATRIX2[i,],MATRIX2[i-1,]))
#Diff2[x,1:41]<-DEPDEP[i,]-DEPDEP[i-1,]
Diff2[x,NC]<-MATRIX2[i,NC]
Diff2[x,NC+1]<-MATRIX2[i,NC+1]
Diff2[x,NC+1]<-MATRIX2[i,NC+1]
Diff2[x,Cl]<-MATRIX2[i,Cl]
}else{x<-x+1}
}

Diff2<-data.frame(Diff2)

################# M_ALL ADDING NSY/NALL AND X, X.1########################?

pvalue<-0.01
#PVALUE:0.01

M_ALL$N.SY.N.ALL<-M_ALL$Num_Syncr_occ/M_ALL$Number_data

############################DEP AND INDEP ###############################
#M_ALL$X : 2 if dependent 1 if independent

for( i in 1:nrow(M_ALL))
{
  if( M_ALL$Number_data[i]<20){M_ALL$X[i]<-NA}
  else{ if(M_ALL$KendalT.p.value[i]<=pvalue){M_ALL$X[i]<-2} else{M_ALL$X[i]<-1}
  }
}

############################# ASY PART #################################
#Selection of Async data with more than 20 data and pvalue 0.01 #
#M_ALL$X.1 : 2 if ASY-dependent 1 if ASY-independent

for( i in 1:nrow(M_ALL))
{
  if( M_ALL$Num_Asyncr_occ[i]<20){M_ALL$X.1[i]<-NA}
  else{ if(M_ALL$KT_pvalue_Asy[i]<=pvalue){M_ALL$X.1[i]<-2} else{M_ALL$X.1[i]<-1}
  }
}


######## Merge part #######################?

M_ALL_perc <- merge(M_ALL, Diff2, by.x = "CODE", by.y = "N_couple") 

######## FOR BONFERRONI Adjustment ###########################

############### Bonferroni - False Discovery Rate ############################


M_ALL_adj<-M_ALL[order(M_ALL$KendalT.p.value,decreasing=FALSE),]

M_ALL_adj$Num<-seq(1,nrow(M_ALL_adj),1)

M_ALL_adj$Bonpv<-0.05*M_ALL_adj$Num/nrow(M_ALL_adj)

M_ALL_adj$CHECK<- M_ALL_adj$KendalT.p.value<=M_ALL_adj$Bonpv

length(which(M_ALL_adj$CHECK=="TRUE"))

Check_bonf<-which(M_ALL_adj$CHECK=="TRUE")

Pv_th<-M_ALL_adj$KendalT.p.value[Check_bonf[length(Check_bonf)]] 


M_ALL_adj$CHECK2<- M_ALL_adj$KendalT.p.value<=Pv_th


DEP<- M_ALL_adj[which(M_ALL_adj$CHECK2=="TRUE"),]

# CODE STATIONS with adjusted pvalue

Code_stat_ok<-  M_ALL_adj$CODE[which(M_ALL_adj$CHECK2=="TRUE")]


############# FOR FALSE DISCOVERY RATE ADJUSTED PVALUE ####################


M_ALL_DEP<- M_ALL[which(M_ALL$CODE%in%Code_stat_ok),]
M_ALL_DEP_perc<-M_ALL_perc[which(M_ALL_perc$CODE%in%Code_stat_ok),]

M_ALL_DEP_POS<-M_ALL_DEP[M_ALL_DEP$KendalT.value>0,]
M_ALL_DEP_POS_perc<-M_ALL_DEP_perc[M_ALL_DEP_perc$KendalT.value>0,]

M_ALL_IND<- M_ALL[-which(M_ALL$CODE%in%Code_stat_ok),]
M_ALL_IND_perc<- M_ALL_perc[-which(M_ALL_perc$CODE%in%Code_stat_ok),]


#########SAVE ALL DATAFRAME ############################

setwd(paste0(path,"/Lag time/Lagtime_",Lag_time,"/Workspace_Bonf"))

save(Pv_th,file="Pvalue.RData")
save(M_ALL,M_ALL_perc,M_ALL_adj,file=paste0("M_ALLbonf_perc_LAG_",Lag_time,".RData"))
save(M_ALL_IND,M_ALL_IND,Pv_th,file=paste0("M_ALLbonf_IND_perc_LAG_",Lag_time,".RData"))
save(M_ALL_DEP,M_ALL_DEP_perc,M_ALL_DEP_POS_perc,M_ALL_DEP_POS,Pv_th,file=paste0("M_ALLbonf_DEP_perc_LAG_",Lag_time,".RData"))

print(paste0("BONFERRONI ADJ PVALUE: Everything have been saved in: ",path,"/Lag time/Lagtime_",Lag_time,"/Workspace_Bonf"))

print(paste0("pvalue selected for Bonferroni is:",Pv_th))

##########################################################################################


############# FOR FIXED PVALUE ####################

print(paste0("BE CAREFUL: The pvalue selected is:",pvalue))

########### FOR DEPENDENT AND INDEPENDENT ##########################?
#Save possibility


###### ATTENTION: NAME OF VARIABLE ARE FOR 0.01 

M_ALL_0.01<- M_ALL[M_ALL$KendalT.p.value<=pvalue,]
M_ALL_0.01_perc<-M_ALL_perc[M_ALL_perc$KendalT.p.value<=pvalue,]

M_ALL_0.01_POS<-M_ALL_0.01[M_ALL_0.01$KendalT.value>0,]
M_ALL_0.01_POS_perc<-M_ALL_0.01_perc[M_ALL_0.01_perc$KendalT.value>0,]

M_ALL_IND_0.01<- M_ALL[M_ALL$KendalT.p.value>pvalue,]
M_ALL_IND_perc0.01<- M_ALL_perc[M_ALL_perc$KendalT.p.value>pvalue,]

#M_ALL_0.01_negative<-M_ALL_0.01[M_ALL_0.01$KendalT.value<0,]


#########SAVE ALL DATAFRAME ############################

setwd(paste0(path,"/Lag time/Lagtime_",Lag_time,"/Workspace"))

save(pvalue,file="Pvalue.RData")
save(M_ALL,M_ALL_perc,file=paste0("M_ALL_perc_LAG_",Lag_time,".RData"))
save(M_ALL_IND_0.01,M_ALL_IND_perc0.01,pvalue,file=paste0("M_ALL_IND_perc_",pvalue,"_LAG_",Lag_time,".RData"))
save(M_ALL_0.01,M_ALL_0.01_perc,M_ALL_0.01_POS_perc,M_ALL_0.01_POS,pvalue,file=paste0("M_ALL_",pvalue,"_perc_LAG_",Lag_time,".RData"))

print(paste0("Everything have been saved in: ",path,"/Lag time/Lagtime_",Lag_time,"/Workspace"))
##########################################################################################