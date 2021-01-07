rm(list = ls())
library(geosphere)
################################################################################
################ All events in the same year between two couple ################
################################################################################


######################### Initialization of matrix ##############################

setwd("D:/PROJECTS/EGU project/DATA_max_flood")

I_matrix_extreme<- read.table("Summary_Results.csv", header = TRUE, sep=";") 
#Summary_Extrem<-matrix (,nc=3,byrow=F) 
#XX<-1

I_matrix<- read.table("Summary_Results.csv", header = TRUE, sep=";") 

M_coord<- read.table("nrfa-coords2.csv", header = TRUE, sep=";")    #table excel with value

################################################################################

setwd("D:/PROJECTS/Regional/RESULTS/UK")



Id_stazioni<-I_matrix[1,]
Num_station<-(ncol(I_matrix)-1)/3
l_m<-Num_station*(Num_station-1)
FinalN<-0

KT_matrix<- matrix(, nrow =l_m , ncol = 23)                                      # Kendall tau matrix
colnames(KT_matrix)=c("River_St_1","River_St_2","KendalT value", "KendalT p.value","N_ties Station1","N_ties Station2","Number_data", "R_Ties_sum","ID_Station_1", "ID_Station_2","Num_Syncr_occ","Num_Asyncr_occ","KT_ Asyncrony", "KT_pvalue_Asy","KT_ Syncrony", "KT_pvalue_Sy","Easting_1","Northing_1","Easting_2","Northing_2","Distance","Area_1","Area_2")                  # Kendall tau matrix

Difference<- matrix( , nrow =l_m , ncol =13 )
colnames(Difference)<- c("Station_1","Station_2","Media","Max","Min","lag0","lag1","lag2","lag3","lag4","lag5","lag5_10","lag10")

xx<-1
#Station[,1]<-I_matrix[,1]
b<- ncol(I_matrix)
Stat<-seq(4,1+Num_station*3,3)                                                  #position of column with Discharge data
Stat_id<-seq(2,1+Num_station*3,3)                                               #Position of ID



################################################################################
 
for (f in 1:(Num_station-1)){

    Id_Station1<-Id_stazioni[1,Stat_id[f]]
    cc_f<-which( M_coord[,1]==Id_Station1)


for (x in (f+1):Num_station){
                                  station_1<-I_matrix_extreme[2:nrow(I_matrix),(f*3-1):(f*3+1)]
                                  station_2<-I_matrix_extreme[2:nrow(I_matrix),(x*3-1):(x*3+1)]
                                  year<- matrix(I_matrix_extreme[2:nrow(I_matrix_extreme),1],nc=1)
                                  
                                  Id_Station2<-Id_stazioni[1,Stat_id[x]]
                                  cc_x<-which( M_coord[,1]==Id_Station2)                                 

############ FOUNDING PART IN COMMON BETWEEN TWO STATIONS ######################
                                  
Couple<- matrix(,nrow=500,ncol=7,byrow=F)
colnames(Couple)<- c("Year","Day_station_1","Month_station_1","Q_station_1","Day_station_2","Month_station_2","Q_station_2")
X<-1                                  
for (dd in 1:(nrow(year)-1)){if(year[dd,1]==year[dd+1,1])
{check3<-is.na(station_1[dd,3])
check4<-is.na(station_2[dd,3])
check5<-is.na(station_1[dd+1,3])
check6<-is.na(station_2[dd+1,3])

if (check3==FALSE & check4==FALSE & check5==TRUE & check6==TRUE){
Couple[X,1]<-year[dd,1]
Couple[X,2]<-station_1[dd,1]
Couple[X,3]<-station_1[dd,2]
Couple[X,4]<-station_1[dd,3]
Couple[X,5]<-station_2[dd,1]
Couple[X,6]<-station_2[dd,2]
Couple[X,7]<-station_2[dd,3]
X<-X+1}else{

if (check3==FALSE & check4==FALSE & check5==FALSE & check6==FALSE){
Couple[X,1]<-year[dd,1]
Couple[X+1,1]<-year[dd+1,1]
Couple[X,2]<-station_1[dd,1]
Couple[X+1,2]<-station_1[dd+1,1]
Couple[X,3]<-station_1[dd,2]
Couple[X+1,3]<-station_1[dd+1,2]
Couple[X,4]<-station_1[dd,3]
Couple[X+1,4]<-station_1[dd+1,3]
Couple[X,5]<-station_2[dd,1]
Couple[X+1,5]<-station_2[dd+1,1]
Couple[X,6]<-station_2[dd,2]
Couple[X+1,6]<-station_2[dd+1,2]
Couple[X,7]<-station_2[dd,3]
Couple[X+1,7]<-station_2[dd+1,3]


X<-X+2}

if (check3==FALSE & check4==FALSE & check5==FALSE & check6==TRUE){
Couple[X,1]<-year[dd,1]
Couple[X+1,1]<-year[dd+1,1]
Couple[X,2]<-station_1[dd,1]
Couple[X+1,2]<-station_1[dd+1,1]
Couple[X,3]<-station_1[dd,2]
Couple[X+1,3]<-station_1[dd+1,2]
Couple[X,4]<-station_1[dd,3]
Couple[X+1,4]<-station_1[dd+1,3]
Couple[X,5]<-station_2[dd,1]
Couple[X+1,5]<-station_2[dd+1,1]
Couple[X,6]<-station_2[dd,2]
Couple[X+1,6]<-station_2[dd+1,2]
Couple[X,7]<-station_2[dd,3]
Couple[X+1,7]<-station_2[dd+1,3]
X<-X+2}

if (check3==FALSE & check4==FALSE & check5==TRUE & check6==FALSE){
Couple[X,1]<-year[dd,1]
Couple[X+1,1]<-year[dd+1,1]
Couple[X,2]<-station_1[dd,1]
Couple[X+1,2]<-station_1[dd+1,1]
Couple[X,3]<-station_1[dd,2]
Couple[X+1,3]<-station_1[dd+1,2]
Couple[X,4]<-station_1[dd,3]
Couple[X+1,4]<-station_1[dd+1,3]
Couple[X,5]<-station_2[dd,1]
Couple[X+1,5]<-station_2[dd+1,1]
Couple[X,6]<-station_2[dd,2]
Couple[X+1,6]<-station_2[dd+1,2]
Couple[X,7]<-station_2[dd,3]
Couple[X+1,7]<-station_2[dd+1,3]
X<-X+2}
}                                                                                                                               
}
}

FinalN<-FinalN+1                                                                                            

############################ PREPARATION MATRIX    ############################
L_coup<- length(which(Couple[,1]>0))

if(L_coup>14)
{

Final_Couple<- matrix(, nrow = L_coup, ncol = 9)                                        # Kendall tau matrix

colnames(Final_Couple)=c("Year","Day_occur_1","Month_occur_1","Julian_day_1" ,"QStation_1","Day_occur_2","Month_occur_2","Julian_day_2" ,"QStation_2")


Final_Couple[,1]<- Couple[1:L_coup,1]
Final_Couple[,2]<- Couple[1:L_coup,2]
Final_Couple[,3]<- Couple[1:L_coup,3]
Final_Couple[,5]<- Couple[1:L_coup,4]
Final_Couple[,6]<- Couple[1:L_coup,5]
Final_Couple[,7]<- Couple[1:L_coup,6]
Final_Couple[,9]<- Couple[1:L_coup,7]                                                                                               
                             
#############################JULIAN DAY ########################################                             
                             
for (jj in 1:2)
{
p_d<-c(2,6)
p_M<-c(3,7)
p_Jd<-c(4,8)


DATA<-matrix(,nrow=length(Final_Couple[,1]), ncol=3)                               # New matrix to calculate Julian Day
 colnames(DATA)<- c("Year","Month","Day")

DATA[,1]<-Final_Couple[,1]       #  year  
DATA[,2]<-Final_Couple[,p_M[jj]]       # MONTHS
DATA[,3]<-Final_Couple[,p_d[jj]]       # day    

  a<-0                                                                          # Variables to calculate Julian Day
  b<-0
  c0<-0
  e<-0
  ff<-0
  year<-0
  month<-0
  
  for (i in 1:length(DATA[,1]))                                             # Loop to calculate the number of the day
  { 
  ch4<- is.na(DATA[i,3])
  if( ch4==FALSE){
  
    if ((DATA[i,2]==1)|(DATA[i,2]==2))                                          # Loop to calculate the Julian day
    {
      year[i]<-DATA[i,1]-1
      month[i]<-DATA[i,2]+12
    } 
    else
    {
      year[i]<-DATA[i,1]
      month[i]<-DATA[i,2]
    }
    
    a[i]<-floor(year[i]/100)
    b[i]<-floor(a[i]/4)
    c0[i]<-2-a[i]+b[i]
    e[i]<-floor(365.25*(year[i]+4716))
    ff[i]<-floor(30.6001*(month[i]+1))
    
    options(digits=8)
    Final_Couple[i,p_Jd[jj]]<- c0[i]+e[i]+ff[i]+DATA[i,3]-1524.5     # Formula to calculate Julian Day
  }}}     
                             
########## ORDERING FOR SIMILAR DATA THE INFORMATION FOR EACH YEAR ################                                                   

Final_matrix<-Final_Couple

for (hh in 1: (nrow(Final_Couple)-1))
{
 if( Final_Couple[hh,1]== Final_Couple[hh+1,1])
     {
      ch_st1<-is.na( Final_Couple[hh+1,4])
      ch_st2<-is.na( Final_Couple[hh+1,8])
      
      if( ch_st1==FALSE & ch_st2==FALSE ){
      
      AC<-abs(Final_Couple[hh,4]- Final_Couple[hh,8])
      AD<-abs(Final_Couple[hh,4]- Final_Couple[hh+1,8])
      BC<-abs(Final_Couple[hh+1,4]- Final_Couple[hh,8])
      BD<-abs(Final_Couple[hh+1,4]- Final_Couple[hh+1,8])
      
       
      Min_Delta<- min(AC,AD,BC,BD)
                                                   
      if(AC == Min_Delta)
             {Final_matrix[hh,]<- Final_Couple[hh,]}
             else{ Final_matrix[hh+1,]<- NA}
      
      if(BD == Min_Delta)
             {Final_matrix[hh,]<- Final_Couple[hh+1,]}
             else{ Final_matrix[hh+1,]<- NA}
      
      if(AD == Min_Delta)
             {Final_matrix[hh,6:9]<- Final_Couple[hh+1,6:9]}
             else{ Final_matrix[hh+1,]<- NA}
      
      if(BC == Min_Delta)
             {Final_matrix[hh,1:5]<- Final_Couple[hh+1,1:5]}
             else{ Final_matrix[hh+1,]<- NA}             
     }
     
     if(ch_st1==TRUE & ch_st2==FALSE)
     {AB<-abs(Final_Couple[hh,4]- Final_Couple[hh,8]) 
      Min_Delta1<- min(abs(Final_Couple[hh,4]-Final_Couple[hh,8]),abs(Final_Couple[hh,4]-Final_Couple[hh+1,8]))
      if(AB==  Min_Delta1)
             {Final_matrix[hh,]<- Final_Couple[hh,]
             Final_matrix[hh+1,6:9]<- NA}
             else{Final_matrix[hh,6:9]<-Final_Couple[hh+1,6:9]
                  Final_matrix[hh+1,6:9]<- NA}
           } 
       if(ch_st1==FALSE & ch_st2==TRUE)
     {
      ABCB<-abs(Final_Couple[hh,4]- Final_Couple[hh,8]) 
      Min_Delta1<- min(abs(Final_Couple[hh,4]-Final_Couple[hh,8]),abs(Final_Couple[hh+1,4]-Final_Couple[hh,8]))
      
      if(ABCB==  Min_Delta1)
             {Final_matrix[hh,]<- Final_Couple[hh,]
              Final_matrix[hh+1,1:5]<- NA}
             else{Final_matrix[hh,1:5]<-Final_Couple[hh+1,1:5]
                  Final_matrix[hh+1,1:5]<- NA}
     } 
   }
 }


############################  RANDOMIZATION matrix     #########################

Real_matrix<-na.omit(Final_matrix)


Real_Couple<- matrix(, nrow = nrow(Real_matrix), ncol = 2)                                     
colnames(Real_Couple)=c("RStation_1", "RStation_2")

Real_Couple[,1]<-Real_matrix[,5]
Real_Couple[,2]<-Real_matrix[,9]

Ties_1<-length(Real_Couple[,1])-length(unique(Real_Couple[,1]))
Ties_2<-length(Real_Couple[,2])-length(unique(Real_Couple[,2]))



####################### RANDOMIZATION ##########################################
set.seed(12345)

kk<-1
ii<-1                                                                       
jj<-1

RR_Couple<-Real_Couple
                                                       
for(ii in 1:2) {
n_occur<-data.frame(table(Real_Couple[,ii]))
n_occur2<- n_occur[n_occur$Freq>1,]

 n_ties<- sum(n_occur2$Freq)                                  
if(n_ties>0){ 
  for (i in 1:length(n_occur[,2]))
 {if(n_occur$Freq[i]>1)
 {min_n_occurs<-as.numeric(as.character(n_occur$Var1[i]))-0.5
  max_n_occurs<-as.numeric(as.character(n_occur$Var1[i]))+0.5
  Random_unif<-runif((n_occur$Freq[i]-1), min=min_n_occurs, max=max_n_occurs)
  Random_num<-c(as.numeric(as.character(n_occur$Var1[i])),Random_unif)
   ll<-1 
  for(kk in 1:length(Real_Couple[,1]))
  {if(Real_Couple[kk,ii]==as.character(n_occur$Var1[i]))
  {RR_Couple[kk,ii]<-Random_num[ll]
   ll<-ll+1}}}
}}}


R_Ties_1<-length(RR_Couple[,1])-length(unique(RR_Couple[,1]))
R_Ties_2<-length(RR_Couple[,2])-length(unique(RR_Couple[,2]))


Real_matrix[,5]<-RR_Couple[,1]
Real_matrix[,9]<-RR_Couple[,2]

############################ SYNCRONIZATION      ###############################
################################################################################

Lag_time<-7
                                                                            
Syncrony<- which( Real_matrix[,4] <= Real_matrix[,8]+Lag_time & Real_matrix[,4] >= Real_matrix[,8]-Lag_time )
                                                
Diff<-abs(Real_matrix[,4]-Real_matrix[,8])

Difference[xx,1]<-as.numeric(as.character(Id_stazioni[1,Stat_id[f]]))
Difference[xx,2]<-as.numeric(as.character(Id_stazioni[1,Stat_id[x]]))
Difference[xx,3]<-median(Diff)
Difference[xx,4]<-max(Diff)
Difference[xx,5]<-min(Diff)
Difference[xx,6]<- length(which(Diff==0))
Difference[xx,7]<- length(which(Diff==1))
Difference[xx,8]<- length(which(Diff==2))
Difference[xx,9]<- length(which(Diff==3))
Difference[xx,10]<- length(which(Diff==4))
Difference[xx,11]<- length(which(Diff==5))
Difference[xx,12]<- length(which(Diff>5))
Difference[xx,13]<- length(which(Diff==10))

M_Syncrony<-RR_Couple[Syncrony,]
if (length(which(M_Syncrony>0))==0 )
{
 M_Asyncrony<-RR_Couple
}else
{ M_Asyncrony<-RR_Couple[-Syncrony,]}

########################### KT MATRIX ###########################################

KT.test_all <- cor.test(RR_Couple[,1],RR_Couple[,2],method="kendall")           #all data

if ((nrow(RR_Couple) -length(which(M_Syncrony>0))/2)>14){
#if(nrow(M_Asyncrony)>8){
KT.test_Asy <- cor.test(M_Asyncrony[,1],M_Asyncrony[,2],method="kendall")       #without syncronized event
KT_matrix[xx,13]<- KT.test_Asy$estimate
KT_matrix[xx,14]<- KT.test_Asy$p.value}



if(length(Syncrony)>14){KT.test_Sy <- cor.test(M_Syncrony[,1],M_Syncrony[,2],method="kendall") 
KT_matrix[xx,15]<- KT.test_Sy$estimate
KT_matrix[xx,16]<- KT.test_Sy$p.value}


KT_matrix[xx,1] <- ifelse(length(cc_f)!=0,as.character(M_coord[cc_f,2]),"")
KT_matrix[xx,2] <- ifelse(length(cc_x)!=0,as.character(M_coord[cc_x,2]),"") 

KT_matrix[xx,3] <- KT.test_all$estimate
KT_matrix[xx,4]<- KT.test_all$p.value
KT_matrix[xx,5]<- Ties_1 
KT_matrix[xx,6]<- Ties_2 
KT_matrix[xx,7]<- nrow(Real_matrix)
KT_matrix[xx,8]<- R_Ties_1+ R_Ties_2              
KT_matrix[xx,9]<- as.numeric(as.character(Id_stazioni[1,Stat_id[f]]))
KT_matrix[xx,10]<-as.numeric(as.character(Id_stazioni[1,Stat_id[x]]))
KT_matrix[xx,11]<- length(Syncrony)
KT_matrix[xx,12]<- nrow(Real_matrix)- length(Syncrony)

if(length(cc_f)!=0){
KT_matrix[xx,17]<- M_coord[cc_f,5]
KT_matrix[xx,18]<- M_coord[cc_f,6]}
if(length(cc_x)!=0){
KT_matrix[xx,19]<- M_coord[cc_x,5]
KT_matrix[xx,20]<- M_coord[cc_x,6]}

Coord_1<-c(  M_coord[cc_x,5], M_coord[cc_x,6])
Coord_2<-c(  M_coord[cc_f,5], M_coord[cc_f,6])

if( length(M_coord[cc_f,8])>0 & length(M_coord[cc_f,7])>0  & length(M_coord[cc_x,8])>0 & length(M_coord[cc_x,7])>0){
KT_matrix[xx,21]<- distGeo(c(M_coord[cc_f,8],M_coord[cc_f,7]),c(M_coord[cc_x,8],M_coord[cc_x,7]))  
KT_matrix[xx,22]<- M_coord[cc_f,9]
KT_matrix[xx,23]<- M_coord[cc_x,9]}

xx<-xx+1} 
} }




setwd(paste0("D:/PROJECTS/Regional/0.Revision_QQ/Lagtime_",Lag_time))

write.table(KT_matrix, file=paste0("UK__CORR_lagtime_",Lag_time,".csv"), sep=";",dec=",", na="", row.names=F, col.names=T)
save(KT_matrix, FinalN, file= paste0("UK__CORR_lagtime_",Lag_time,".RData"))

write.table(Difference, file=paste0("UK__DIFF_lagtime_",Lag_time,".csv"), sep=";",dec=",", na="", row.names=F, col.names=T)
save(Difference,file= paste0("UK__DIFF_lagtime_",Lag_time,".RData"))

############# post elaboration #################################################

#KT_matrix<- read.table("UK__CORR_LAG10.csv", header = TRUE, sep=";") 


LK<- length(which(KT_matrix[,1]!=0))
#LK<-111638

KT_matrix2<-KT_matrix[1:LK,]

ELAB_KT_matrix<- matrix(, nrow = LK , ncol = 23)                                      # Kendall tau matrix
colnames(ELAB_KT_matrix)=c("River_St_1","River_St_2","KendalT value", "KendalT p.value","ALL STATUS","N_ties Station1","N_ties Station2","Number_data", "R_Ties_sum","ID_Station_1", "ID_Station_2","Num_Syncr_occ","Num_Asyncr_occ","KT_ Asyncrony", "KT_pvalue_Asy","ASY STATUS","KT_ Syncrony", "KT_pvalue_Sy","SY STATUS","Easting_1","Northing_1","Easting_2","Northing_2")                  # Kendall tau matrix

ELAB_KT_matrix[,3:4]<-as.numeric(as.character(KT_matrix2[,3:4]))
ELAB_KT_matrix[,3:4]<-as.numeric(as.character(KT_matrix2[,3:4]))
ELAB_KT_matrix[,5]<-ifelse( ELAB_KT_matrix[,4]>0.01,"IND","DEP")
 
ELAB_KT_matrix[,6:15]<-as.numeric(as.character(KT_matrix2[,5:14]))
ELAB_KT_matrix[,16]<-ifelse( ELAB_KT_matrix[,15]>0.01,"IND","DEP")                                      
ELAB_KT_matrix[,17:18]<-as.numeric(as.character(KT_matrix2[,15:16]))

ELAB_KT_matrix[,19]<-ifelse( ELAB_KT_matrix[,18]>0.01,"IND","DEP")
ELAB_KT_matrix[,20:23]<-as.numeric(as.character(KT_matrix2[,17:20]))


write.table(ELAB_KT_matrix, file=paste0("ELAB_KT_lagtime_",Lag_time,".csv"), sep=";",dec=",", na="", row.names=F, col.names=T)
save(ELAB_KT_matrix,file= paste0("ELAB_KT_matrix_",Lag_time,".RData"))


SUM_table<- matrix(, nrow = 2 , ncol = 3) 

SUM_table[1,1]<- round( nrow(ELAB_KT_matrix) )
SUM_table[1,2]<- round(length(which(ELAB_KT_matrix[,5]=="IND")))
SUM_table[1,3]<- round(length(which(ELAB_KT_matrix[,5]=="DEP")))


SUM_table[2,2]<-100*length(which(ELAB_KT_matrix[,5]=="IND"))/nrow(ELAB_KT_matrix)
SUM_table[2,3]<-100*length(which(ELAB_KT_matrix[,5]=="DEP"))/nrow(ELAB_KT_matrix)














