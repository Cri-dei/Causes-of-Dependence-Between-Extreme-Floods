###########  PAPER QQ ########
#### CODE FOR DAILY DISCHARGE POT ##########
######Author: Cristina Deidda ##########


#################################################################

######## IN THIS CODE ARE ################# 

rm(list = ls())


############################################################
##### Enjoy :) #############

library(date)
library(lubridate)
library(tidyverse)
library(gtools)
library(geosphere)

# CHOOSE DIRECTORY

#pc ufficio
#setwd("D:/PROJECTS/Regional/DISTANCE_selection/Data")
path<-c("C:/PROJECTS 2021/QQ")
#pc portatile
#path<-c("C:/Users/39349/Documents/Regional")


#############################0. IMPORT FILE ######################################

source(paste0(path,"/Code/Functions/","Randomization.R"))
source(paste0(path,"/Code/Functions/","POT_annualmax.R"))
source(paste0(path,"/Code/Functions/","POT_monthly_variable_max.R"))

# Import geographical info

setwd(paste0(path,"/Data"))
Data_INFO<-read.csv("Catch_info_final.csv",sep=";")
M_coord<- read.table("nrfa-coords3.csv", header = TRUE, sep=",")    #table excel with value

### Set English language for date format

Sys.setlocale('LC_ALL','en_CA.utf-8');
Sys.setlocale('LC_ALL','English');

#Import POT FILE

###### File available #########

setwd(paste0(path,"/Data/POT_data/Suitable"))

File_st<-list.files(path = ".")
Available_st<-sapply(1:length(File_st),function(x){as.numeric(str_split(File_st[x], "\\.")[[1]][1])})
Available_st<-unique(Available_st)


############# All combination between stations ########

Allcomb<-data.frame(combinations(length(Available_st), 2, v=Available_st, set=TRUE, repeats.allowed=FALSE))
colnames(Allcomb)<-c("ID_Station_1","ID_Station_2")


## Choose matrix for POT


Couples_investigate<-Allcomb


              ########## CHOOSE TYPE OF ANALYSIS ###############

TYPE_ANALYSIS<-"Max Annual"
#TYPE_ANALYSIS<-"Variable POT Month"

############################# Initialize POT matrix ##################################


POT_matrix<-as.data.frame(matrix(, nrow = nrow(Allcomb), ncol = 17))

colnames(POT_matrix)<-c("CODE", "ID_Station_1",  "ID_Station_2","KendalT.value","KendalT.p.value","Number_data",
                        "Num_Syncr_occ","Num_Asyncr_occ","KT_Asyncrony", "KT_pvalue_Asy",
                        "Easting_1","Northing_1","Easting_2","Northing_2","Distance","Year_st","Year_end")

# colnames(KT_matrix)=c("River_St_1","River_St_2","KendalT.value", "KendalT.p.value",
#                       "N_ties Station1","N_ties Station2","Number_data", "R_Ties_sum","ID_Station_1", 
#                       "ID_Station_2","Num_Syncr_occ","Num_Asyncr_occ","KT_ Asyncrony", "KT_pvalue_Asy",
#                       "KT_ Syncrony", "KT_pvalue_Sy","Easting_1","Northing_1","Easting_2","Northing_2")                  # Kendall tau matrix
# 


#############################1. CYCLE FOR ALL THE COUPLES ######################################

#Lag_time selected

lag_time<-3

List_couple<- vector(mode = "list", length = nrow(Couples_investigate))
#List_couple_R<- vector(mode = "list", length = nrow(Couples_investigate))
comb<-c(seq(377726,388521,200),388521)

Threshold<-list()
Th_num<-list()

xx<-1

for (yy in 380948:nrow(Couples_investigate))
{

  
    ######
    ID_Station_1<-Couples_investigate$ID_Station_1[yy]
    ID_Station_2<-Couples_investigate$ID_Station_2[yy]
  
  
    ##Extract value of threshold for that station 
    
    
    #Station1
    name1<- ifelse(nchar(ID_Station_1)==4, paste0("00",ID_Station_1),if(nchar(ID_Station_1)==5){paste0("0",ID_Station_1)}else{ID_Station_1})
    #Station2
    name2<- ifelse(nchar(ID_Station_2)==4, paste0("00",ID_Station_2),if(nchar(ID_Station_2)==5){paste0("0",ID_Station_2)}else{ID_Station_2})
    
    if(any(paste0(name1,".pt")==File_st) && any(paste0(name2,".pt")==File_st))
    { 
      
    #Station1: read file preparing
    con1 <- file(paste0(name1,".pt"),"r") 
    tt<-readLines(con1,n=1000)
    num_st1<-which(tt=="[POT Values]")
    tnm1<-tt[6]
    close(con1)   
    
    
    #Station2: read file preparing
    name2<- ifelse(nchar(ID_Station_2)==4, paste0("00",ID_Station_2),if(nchar(ID_Station_2)==5){paste0("0",ID_Station_2)}else{ID_Station_2})
    
    con2 <- file(paste0(name2,".pt"),"r") 
    tt<-readLines(con2,n=1000)
    num_st2<-which(tt=="[POT Values]")
    tnm2<-tt[6]
    close(con2)   
    

    Threshold[[xx]]<- list(tnm1,tnm2)
    Th_num[[xx]]<-list(as.numeric(as.character(str_split(tnm1,"\\,")[[1]][2])),as.numeric(as.character(str_split(tnm2,"\\,")[[1]][2])))
    
    names(Threshold)[[xx]]<-Couples_investigate$CODE[xx]
    names(Th_num)[[xx]]<-Couples_investigate$CODE[xx]
    
  
      
    #Part 1: 
    # Read Discharge data and extract just data for year in common
    
    #Read daily discharge for Station 1
    
    Station_1<- read.csv(paste0(name1,".pt"),sep=",", skip=num_st1,header=FALSE)
    colnames(Station_1)<-c("Data","Discharge_1","bas")
    
    #Read daily discharge for Station 2
    
    Station_2<- read.csv(paste0(name2,".pt"),skip=num_st2,sep=",", header=FALSE)
    colnames(Station_2)<-c("Data","Discharge_2","bas")
    
    
    Station_1$Data_1<-as.Date(as.POSIXct(Station_1$Data, format="%d %b %Y",tz="UTC"))
    Station_2$Data_2<-as.Date(as.POSIXct(Station_2$Data, format="%d %b %Y",tz="UTC"))
    
    
    Station_1$Year<-year(as.Date(Station_1$Data_1, format="%d %b %Y"))
    Station_2$Year<-year(as.Date(Station_2$Data_2, '%Y-%m-%d'))
    
    Station_1$Month<-month(as.Date(Station_1$Data_1, '%Y-%m-%d'))
    Station_2$Month<-month(as.Date(Station_2$Data_2, '%Y-%m-%d'))
    
    X1<-data.frame(table(Station_1$Year))
    X2<-data.frame(table(Station_2$Year))
    
    #Year in common between the two couples
    
    Year_final<- merge(X1,X2, by.x = "Var1", by.y = "Var1")
    Final_merged<-na.omit(merge(Station_1,Station_2, by.x = "Year", by.y = "Year"))
    
    if(yy%in%comb)
    {print(paste("The code is running: couple",yy,"of",nrow(Couples_investigate)))}
    
    
    if(length(unique(Final_merged$Year))>5){
    
      Y_st<- as.numeric(as.character(Year_final$Var1[1]))
      Y_end<-as.numeric(as.character(Year_final$Var1[nrow(Year_final)]))
      
      #Y_st<- Final_merged$Data_1[1]
      #Y_end<-Final_merged$Data_1[nrow(Final_merged)]
      
      ##################### DISCHARGE FOR COMMON YEAR ##########################
      
      Stat_1_adj<-Station_1[which(Station_1$Year==Y_st)[1]:last(which(Station_1$Year==Y_end)),]
      Stat_2_adj<-Station_2[which(Station_2$Year==Y_st)[1]:last(which(Station_2$Year==Y_end)),]
      
      
      # Quantile of the two stations
      # 
      # Thereshold_1 <-c(quantile(Station_1$Discharge_1, pTH, na.rm="TRUE"))
      # Thereshold_2 <-c(quantile(Station_2$Discharge_2, pTH, na.rm="TRUE"))
      # 
      ##########################################################################  
      
      # PART 2: EVENT CALCULATION
      # Extraction of event over threshold, volume and peak, Initial and end of event
      
      ##############################
      ######################### FLOOD EVENT CALCULATION ############################
      ##############################
      
      
      #Calculate flood event for Station 1 of couple
      
      #source(paste0(path,"/Code/Functions/","Event_selection.R"))

      # ID_Station_1<- Couples_investigate$ID_Station_1[xx]
      # Basin_area_1<- Data_INFO$Catchment.area[which(Data_INFO$Station.number==ID_Station_1)]
      # 
      # Event_selection_1<-Event_selection(Stat_1_adj$Discharge_1,Thereshold_1,Basin_area_1) 
      # 
      # #Calculate flood event for Station 2 of couple
      # 
      # 
      # Thereshold_2 <-c(quantile(Station_2$Discharge_2, pTH, na.rm="TRUE")) 
      # 
      # Stat_2_adj$Disc_2_POT<- ifelse( Stat_2_adj$Discharge_2 -Thereshold_2>0,
      #                                 Stat_2_adj$Discharge_2 -Thereshold_2,0)
      # 
      # 
      # ID_Station_2<- Couples_investigate$ID_Station_2[xx]
      # Basin_area_2<- Data_INFO$Catchment.area[which(Data_INFO$Station.number==ID_Station_2)]
      # 
      # Event_selection_2<-Event_selection(Stat_2_adj$Discharge_2,Thereshold_2,Basin_area_2)
      # 
      # ############################################################################################
      
      ## Adding data Q_peak #######
      
      Event_selection_1<-Stat_1_adj
      Event_selection_2<-Stat_2_adj
      
      Event_selection_1$Qpeak<-Event_selection_1$Discharge_1
      Event_selection_2$Qpeak<-Event_selection_2$Discharge_2
      
      #1. Scenario
      
      # Station 1 fixed
      
      # Station2 || Station1_max_POT
      
      #lag_time<-3
      
      
      #########POT ON MONTHLY MAX ################
      
      #source(paste0(path,"/Code/Functions/","POT_monthly_1max.R"))
      #Dep_12M<-POT_monthly_1max(Event_selection_1,Event_selection_2)
      
      #source(paste0(path,"/Code/Functions/","POT_monthly_variable_max.R"))
      # Dep_12M<-POT_monthly_variable_max(Event_selection_1,Event_selection_2)
       

      # 
      # if(TYPE_ANALYSIS== "Max Annual")
      # 
      # 
      # { 
        # ANNUAL MAX FROM POT
        Dep_12A<-POT_annualmax(Event_selection_1,Event_selection_2)
        Dep_12_Discharge<-Dep_12A[,c("QPeak_1","QPeak_2")]
        Dep_12_select<-Dep_12A
      # }
      # else if(TYPE_ANALYSIS== "Variable POT Month")
      # {
      #   # POT VARIABLE FOR MONTH
      #   Dep_12M<-POT_monthly_variable_max(Event_selection_1,Event_selection_2)
      #   Dep_12_Discharge<-Dep_12M[,c("QPeak_1","QPeak_2")]
      #   Dep_12_select<-Dep_12M
      # }
      # 
      if(nrow(Dep_12_select)>0)
      {
          
      List_couple[[xx]]<- list(Event_selection_1,Event_selection_2,Dep_12_select)
      names(List_couple)[xx]<-yy
      names(List_couple[[xx]])[1]<-ID_Station_1
      names(List_couple[[xx]])[[2]]<-ID_Station_2
      
    
      
      ################## Randomization ##############################################

      Dep_12_Disc_R<-Randomization(Dep_12_Discharge,0.1)
      
      #List_couple_R[[xx]]<-Dep_12_Disc_R
      #names(List_couple_R)[xx]<-yy
      
      if(nrow(Dep_12_Disc_R)>=20)
      {
        KT.test_12 <- cor.test(Dep_12_Disc_R[,1],Dep_12_Disc_R[,2],method="kendall") 
        
        POT_matrix$KendalT.value[xx]<- KT.test_12$estimate
        POT_matrix$KendalT.p.value[xx]<- KT.test_12$p.value 
        
        
        POT_matrix$CODE[xx]<-yy
        POT_matrix$ID_Station_1[xx]<-ID_Station_1
        POT_matrix$ID_Station_2[xx]<-ID_Station_2

        
        cc_l1<- which(M_coord$Station.number==ID_Station_1)
        cc_l2<- which(M_coord$Station.number==ID_Station_2)
        
        POT_matrix$Easting_1[xx]<-M_coord$Easting[cc_l1]
        POT_matrix$Northing_1[xx]<-M_coord$Northing[cc_l1]
        
        POT_matrix$Easting_2[xx]<-M_coord$Easting[cc_l2]
        POT_matrix$Northing_2[xx]<-M_coord$Northing[cc_l2]       
        
        POT_matrix$Distance[xx]<-distGeo(c(M_coord$Longitude[cc_l1], M_coord$Latitude[cc_l1]), c(M_coord$Longitude[cc_l2], M_coord$Latitude[cc_l2]))
        POT_matrix$Number_data[xx]<-nrow(Dep_12_select)
        
        Syncrony<-which(Dep_12_select$Diff>=-lag_time & Dep_12_select$Diff<=lag_time)
        
        POT_matrix$Num_Syncr_occ[xx]<-length(Syncrony)
        POT_matrix$Num_Asyncr_occ[xx]<-POT_matrix$Number_data[xx]-length(Syncrony)
        
        POT_matrix$Year_st[xx]<-Y_st
        POT_matrix$Year_end[xx]<-Y_end
        
        if (POT_matrix$Num_Asyncr_occ[xx]>=20){
          
          if(length(Syncrony)>0){
          M_Asyncrony<-Dep_12_Disc_R[-Syncrony,]}
          else
          {
            M_Asyncrony<-Dep_12_Disc_R
          }
          
          KT.test_Asy <- cor.test(M_Asyncrony[,1],M_Asyncrony[,2],method="kendall")       #without syncronized event
          POT_matrix$KT_Asyncrony[xx]<- KT.test_Asy$estimate
          POT_matrix$KT_pvalue_Asy[xx]<- KT.test_Asy$p.value
          }
          
        
        xx<-xx+1

          }        
      
      
    } }  
}}

print(paste0("You run the analysis:",TYPE_ANALYSIS))

na_pos<-which(is.na(POT_matrix$CODE))

POT_matrix2<-POT_matrix

POT_matrix<-POT_matrix[-na_pos,]

######### If set variable #################


if(TYPE_ANALYSIS== "Max Annual")
  
{
#setwd(paste0(path,"/Results/POT_max"))
#FOR CHECKING:

setwd(paste0(path,"/Results_update/Max_annual_by_POT"))  
  
save.image(paste0("POT_MAX_lag",lag_time,"part2.RData"))
save(POT_matrix,file=paste0("Final_matrix_POTMAX_lag",lag_time,"part2.RData"))

write.table(POT_matrix,paste0("POT_MAX_lag",lag_time,"part2.csv"), row.names=F, col.names=T)

}else if(TYPE_ANALYSIS== "Variable POT Month")

  ######### If set variable #################

  
{
setwd(paste0(path,"/Results/POT_monthly_var"))

save.image(paste0("POT_MAX_lag",lag_time,".RData"))
save(POT_matrix,file=paste0("Final_matrix_POTMAX_lag",lag_time,".RData"))

write.table(POT_matrix,paste0("POT_MAX_lag",lag_time,".csv"), row.names=F, col.names=T)

}


print(paste0("You run the analysis:",TYPE_ANALYSIS))


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


