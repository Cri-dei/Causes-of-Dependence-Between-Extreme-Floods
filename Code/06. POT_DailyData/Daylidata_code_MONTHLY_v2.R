###########  PAPER QQ ########
#### CODE FOR DAILY DISCHARGE POT ##########
######Author: Cristina Deidda ##########


#################################################################

########IN THIS CODE ARE 

rm(list = ls())

Lag_time<-5
############################################################
##### Enjoy :) #############

library(date)
library(lubridate)

# CHOOSE DIRECTORY

#pc ufficio
#setwd("D:/PROJECTS/Regional/DISTANCE_selection/Data")
path<-c("C:/PROJECTS 2021/QQ")
#pc portatile
#path<-c("C:/Users/39349/Documents/Regional")




#############################0. IMPORT FILE ######################################

# Import geographical info

setwd(paste0(path,"/Data"))
Data_INFO<-read.csv("Catch_info_final.csv",sep=";")

#Import final data-set

setwd(paste0(path,"/Lag time/Lagtime_",Lag_time,"/Workspace_Bonf"))
#load(paste0("M_ALLbonf_perc_LAG_",Lag_time,".RData"))

load(paste0("M_ALLbonf_DEP_perc_LAG_",Lag_time,".RData"))

#Import Daily Flow Datas

setwd(paste0(path,"/Data/DAILYFLOWDATA"))

Available_st<-read.table("Elencofile.txt")

############################# Quantile and Initial Couples #######################################################

## Choose quantile for POT ##

pTH<-0.95

DEpdep<-M_ALL_DEP_POS_perc[which(M_ALL_DEP_POS_perc$X.1==2),]

## Choose matrix for POT


Couples_investigate<-DEpdep
#Couples_investigate<-M_ALL_DEP_POS_perc
  
  
############################# Initialize POT matrix ##################################


POT_matrix<-as.data.frame(matrix(, nrow = nrow(Couples_investigate), ncol = 9))

colnames(POT_matrix)<-c("CODE","KendalT.value","KendalT.p.value", "ID_Station_1",    "ID_Station_2",    "POT_KT_12",      
                        "POT_pvalue_12",   "POT_KT_21",       "POT_pvalue_21")

POT_matrix$CODE<-Couples_investigate$CODE
POT_matrix$KendalT.value<-Couples_investigate$KendalT.value
POT_matrix$KendalT.p.value<-Couples_investigate$KendalT.p.value
POT_matrix$ID_Station_1<-Couples_investigate$ID_Station_1
POT_matrix$ID_Station_2<-Couples_investigate$ID_Station_2
POT_matrix$Distance<-Couples_investigate$Distance.x

#############################1. CYCLE FOR ALL THE COUPLES ######################################


List_couple<- vector(mode = "list", length = nrow(Couples_investigate))

for (xx in 1:nrow(Couples_investigate))
{

  if(any(Couples_investigate$ID_Station_1[xx]==Available_st) && any(Couples_investigate$ID_Station_2[xx]==Available_st))
  {
  
  
  #Part 1: 
  # Read Discharge data and extract just data for year in common
  
  #Read daily discharge for Station 1
    
  Station_1<- read.csv(paste0("nrfa-public-17_",Couples_investigate$ID_Station_1[xx],"_gdf.csv"),skip=20,sep=",", header=FALSE)
  colnames(Station_1)<-c("Data_1","Discharge_1")
  
  #Read daily discharge for Station 2
  
  Station_2<- read.csv(paste0("nrfa-public-17_",Couples_investigate$ID_Station_2[xx],"_gdf.csv"),skip=20,sep=",", header=FALSE)
  colnames(Station_2)<-c("Data_2","Discharge_2")

  
  Station_1$Data_1<-as.Date(as.POSIXct(Station_1$Data_1, format="%Y-%m-%d",tz="UTC"))
  Station_2$Data_2<-as.Date(as.POSIXct(Station_2$Data_2, format="%Y-%m-%d",tz="UTC"))
  
  
  Station_1$Year<-year(as.Date(Station_1$Data_1, '%Y-%m-%d'))
  Station_2$Year<-year(as.Date(Station_2$Data_2, '%Y-%m-%d'))
  
  Station_1$Month<-month(as.Date(Station_1$Data_1, '%Y-%m-%d'))
  Station_2$Month<-month(as.Date(Station_2$Data_2, '%Y-%m-%d'))
  
  X1<-data.frame(table(Station_1$Year))
  X2<-data.frame(table(Station_2$Year))
  
  #Year in common between the two couples
  
  Year_final<- merge(X1,X2, by.x = "Var1", by.y = "Var1")
  Final_merged<-na.omit(merge(Station_1,Station_2, by.x = "Data_1", by.y = "Data_2"))
  
  if(nrow(Final_merged)>0){
  
  Y_st<- Final_merged$Data_1[1]
  Y_end<-Final_merged$Data_1[nrow(Final_merged)]
  
  ##################### DISCHARGE FOR COMMON YEAR ##########################
  
  Stat_1_adj<-Station_1[which(Station_1$Data_1==Y_st):which(Station_1$Data_1==Y_end),]
  Stat_2_adj<-Station_2[which(Station_2$Data_2==Y_st):which(Station_2$Data_2==Y_end),]
  

  # Quantile of the two stations
  
  Thereshold_1 <-c(quantile(Station_1$Discharge_1, pTH, na.rm="TRUE"))
  Thereshold_2 <-c(quantile(Station_2$Discharge_2, pTH, na.rm="TRUE"))
  
  ##########################################################################  
  
  # PART 2: EVENT CALCULATION
  # Extraction of event over threshold, volume and peak, Initial and end of event
  
                      ##############################
######################### FLOOD EVENT CALCULATION ############################
                        ##############################
  
      
      #Calculate flood event for Station 1 of couple
  
      source(paste0(path,"/Code/Functions/","Event_selection.R"))
      
      Thereshold_1 <-c(quantile(Station_1$Discharge_1, pTH, na.rm="TRUE")) 
      
      Stat_1_adj$Disc_1_POT<- ifelse( Stat_1_adj$Discharge_1 -Thereshold_1>0,
                                      Stat_1_adj$Discharge_1 -Thereshold_1,0) 
      
      ID_Station_1<- Couples_investigate$ID_Station_1[xx]
      Basin_area_1<- Data_INFO$Catchment.area[which(Data_INFO$Station.number==ID_Station_1)]
      
      Event_selection_1<-Event_selection(Stat_1_adj$Discharge_1,Thereshold_1,Basin_area_1) 
      
      #Calculate flood event for Station 2 of couple
      
      
      Thereshold_2 <-c(quantile(Station_2$Discharge_2, pTH, na.rm="TRUE")) 
      
      Stat_2_adj$Disc_2_POT<- ifelse( Stat_2_adj$Discharge_2 -Thereshold_2>0,
                                      Stat_2_adj$Discharge_2 -Thereshold_2,0)
      
      
      ID_Station_2<- Couples_investigate$ID_Station_2[xx]
      Basin_area_2<- Data_INFO$Catchment.area[which(Data_INFO$Station.number==ID_Station_2)]
      
      Event_selection_2<-Event_selection(Stat_2_adj$Discharge_2,Thereshold_2,Basin_area_2)
      
    ############################################################################################
    
      ## Adding data Q_peak #######
      
      Event_selection_1$Data_Qpeak<- as.Date(Stat_1_adj$Data_1[Event_selection_1$Max_Event],"%Y-%m-%d")
      Event_selection_1$Discharge_1<-Stat_1_adj$Discharge_1[Event_selection_1$Max_Event]
      Event_selection_1$Month<-Stat_1_adj$Month[Event_selection_1$Max_Event]
      Event_selection_1$Year<-Stat_1_adj$Year[Event_selection_1$Max_Event]

      Event_selection_2$Data_Qpeak<- as.Date(Stat_2_adj$Data_2[Event_selection_2$Max_Event],"%Y-%m-%d")      
      Event_selection_2$Discharge_2<-Stat_2_adj$Discharge_2[Event_selection_2$Max_Event]      
      Event_selection_2$Month<-Stat_2_adj$Month[Event_selection_2$Max_Event]
      Event_selection_2$Year<-Stat_2_adj$Year[Event_selection_2$Max_Event]
      
      
    #1. Scenario
      
      # Station 1 fixed
      
      # Station2 || Station1_max_POT
      
    lag_time<-3
      
    
#########POT ON MONTHLY MAX ################
    
    #source(paste0(path,"/Code/Functions/","POT_monthly_1max.R"))
    Dep_12M<-POT_monthly_1max(Event_selection_1,Event_selection_2)
    
    #source(paste0(path,"/Code/Functions/","POT_monthly_variable_max.R"))
   #Dep_12M<-POT_monthly_variable_max(Event_selection_1,Event_selection_2)
    
    #source(paste0(path,"/Code/Functions/","POT_annualmax.R"))
    #Dep_12M<-POT_annualmax(Event_selection_1,Event_selection_2)
    
    
    List_couple[[xx]]<- list(Event_selection_1,Event_selection_2,Dep_12M)
    names(List_couple)[[xx]]<-Couples_investigate$CODE[xx]
    names(List_couple[[xx]])[1]<-Couples_investigate$ID_Station_1[xx]
    names(List_couple[[xx]])[[2]]<-Couples_investigate$ID_Station_2[xx]
    
    
    
      ################## Randomization ##############################################
      
      source(paste0(path,"/Code/Functions/","Randomization.R"))
      
    
      Dep_12_Discharge<-Dep_12M[,c("QPeak_1","QPeak_2")]
      
      Dep_12_Disc_R<-Randomization(Dep_12_Discharge,0.1)
      
      if(nrow(Dep_12_Disc_R)>=20)
      {
        KT.test_12 <- cor.test(Dep_12_Disc_R[,1],Dep_12_Disc_R[,2],method="kendall") 
        
        POT_matrix$POT_KT_12[xx]<- KT.test_12$estimate
        POT_matrix$POT_pvalue_12[xx]<- KT.test_12$p.value 
        
        
      }
      
    
      
       } }  
}

######### If set variable #################

setwd(paste0(path,"/Results/POT_Daily/Max_Monthly_variable"))

save.image(paste0("MMAXv_DEPDEP_POT_",pTH,"_lag",lag_time,".RData"))

write.table(POT_matrix,paste0("MMAXv_DEPDEP_POT_",pTH,"_lag",lag_time,".csv"), row.names=F, col.names=T)
#write.table(POT_matrix,paste0("ALLDEP_POT_lag",lag_time,".csv"), row.names=F, col.names=T) 

######### If set 1 max value #######################

# setwd(paste0(path,"/Results/POT_Daily/Max_Monthly_1"))
# 
# save.image(paste0("MMAX1_DEPDEP_POT_",pTH,"_lag",lag_time,".RData"))
# 
# write.table(POT_matrix,paste0("MMAX1_DEPDEP_POT_",pTH,"_lag",lag_time,".csv"), row.names=F, col.names=T)
# #write.table(POT_matrix,paste0("ALLDEP_POT_lag",lag_time,".csv"), row.names=F, col.names=T)      
#       
#       
#       
      
      
      