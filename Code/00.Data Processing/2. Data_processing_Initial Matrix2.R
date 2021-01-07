#######Author: Cristina Deidda ##############
#######QQ DATA PROCESSING CODE##############
#What is done:
#1) Load Matrix KT for all the couples
#2) Eliminate column for which we don't have coordinates
#3) Select just the couples with 20 data in commmon
#4) OUTPUT: Two workspace: KT_M_Final_available_20data_ok,Difference_M_Final_available_20data_ok
#           A excel Numerical table: KT_M_Final_available_20_Num

##For applying successive code is better use the .csv file for###
## do not have problem of not numeric data ##
#Last update: 28/10/20 after 1 revision of QQ


## LOAD FILE YOU WANT TO PRE PROCESSING ##########

############### LAG 10 ##################################
Lag_time<-10

#Load the results of the code:1. UK_V8_QEXTREME_10PERCENT_LAG10
setwd('C:/Users/39349/Documents/Regional/Code/00.Data Processing')
load("C:/Users/39349/Documents/Regional/Check/InitialKT.RData")

############# OTHERS LAG TIME ##################################

print("Are you checking for Lag time?"); 

Lag_time<-10

setwd(paste0("C:/Users/39349/Documents/Regional/Lag time/Lagtime_",Lag_time,"/InitialData"))
load(paste0("UK_KT_Lag_time_",Lag_time,".Rdata"))

print(paste("Attention: lag time selected is:",Lag_time,"days")); 


######################################################################

KT_matrix_dt<-data.frame(KT_matrix)
Difference_dt<-data.frame(Difference)
#Remove NA in the matrix

Pos_na<-which(is.na(KT_matrix_dt[,3]))
KT_M_Final<-KT_matrix_dt[-Pos_na,]
Difference_M_Final<-Difference_dt[-Pos_na,]

Table_summary<- matrix(0,1,3)

colnames(Table_summary)<- c("Total Couple", "Couples with catch data", "Couple with length>=20")

Table_summary[1,1]<-nrow(KT_M_Final)

#Data for which we don't have coordinates

Col_data<- which(KT_M_Final[,1]=="" | KT_M_Final[,2]=="")

KT_M_Final_available<-KT_M_Final[-Col_data,]
Difference_M_Final_available<- Difference_M_Final[-Col_data,]

KT_M_Final_available$Number_data<-as.numeric(as.character(KT_M_Final_available$Number_data))

#Select only 20 data length 

Coup_more20<-which(KT_M_Final_available$Number_data>=20)

KT_M_Final_available_20data<- KT_M_Final_available[Coup_more20,]
Difference_M_Final_available_20data<-Difference_M_Final_available[Coup_more20,]

Table_summary[1,2]<-nrow(KT_M_Final_available)
Table_summary[1,3]<-nrow(KT_M_Final_available_20data)

#Adding the code of the couple

CODE<-as.numeric(row.names(KT_M_Final_available_20data))

KT_M_Final_available_20data_ok<-cbind(CODE,KT_M_Final_available_20data)
Difference_M_Final_available_20data_ok<-cbind(CODE,Difference_M_Final_available_20data)

#Saving workspace
save(KT_M_Final_available_20data_ok, file="Processed_KT_Matrix.RData")
save(Difference_M_Final_available_20data_ok, file="Processed_Difference.RData")

#Saving final matrix as numeric
KT_M_Final_available_20_Num<-KT_M_Final_available_20data_ok[,-c(2,3)]
write.table(KT_M_Final_available_20_Num, file="KT_M_Final_available_20_Num.csv", sep=";",dec=",", na="", row.names=F, col.names=T)

###################################################################################################
print(paste("Attention: lag time selected is:",Lag_time,"days"))
#########################################################################à