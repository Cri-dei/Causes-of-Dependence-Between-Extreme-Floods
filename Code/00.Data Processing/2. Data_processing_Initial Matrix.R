#######Author: Cristina Deidda ##############
#######QQ DATA PROCESSING CODE##############
#What is done:
#1) Load Matrix KT for all the couples
#2) Eliminate column for which we don't have coordinates
#3) Select just the couples with 20 data in commmon

#Load the results of the code:1. UK_V8_QEXTREME_10PERCENT_LAG10
load("C:/Users/39349/Documents/Regional/Check/InitialKT.RData")

KT_matrix_dt<-data.frame(KT_matrix)

#Remove NA in the matrix

Pos_na<-which(is.na(KT_matrix_dt[,3]))
KT_M_Final<-KT_matrix_dt[-Pos_na,]

Table_summary<- matrix(0,1,3)

colnames(Table_summary)<- c("Total Couple", "Couples with catch data", "Couple with length>=20")

Table_summary[1,1]<-nrow(KT_M_Final)

#Data for which we don't have coordinates

Col_data<- which(KT_M_Final[,1]=="" | KT_M_Final[,2]=="")

KT_M_Final_available<-KT_M_Final[-Col_data,]

KT_M_Final_available$Number_data<-as.numeric(as.character(KT_M_Final_available$Number_data))

#Select only 20 data length 

KT_M_Final_available_20data<- KT_M_Final_available[which(KT_M_Final_available$Number_data>=20),]


Table_summary[1,2]<-nrow(KT_M_Final_available)
Table_summary[1,3]<-nrow(KT_M_Final_available_20data)

#Adding the code of the couple

CODE<-as.numeric(row.names(KT_M_Final_available_20data))

KT_M_Final_available_20data_ok<-cbind(CODE,KT_M_Final_available_20data)

save(KT_M_Final_available_20data_ok, file="Processed_KT_Matrix.RData")