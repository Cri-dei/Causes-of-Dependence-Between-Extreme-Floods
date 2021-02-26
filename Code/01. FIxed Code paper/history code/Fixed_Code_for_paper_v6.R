library(plotly)
library(gridExtra)
library(ggpubr)

#pc ufficio
#setwd("D:/PROJECTS/Regional/DISTANCE_selection/Data")
#pc portatile
setwd("C:/Users/39349/Documents/Regional/Data")

#M_ALL<- read.table("ELAB_UK_CORR_LAG10_20_Rformat_3008.csv", header = TRUE, sep=";")     #1 =IND, 2=DEP
M_ALL<- read.table("ELAB_UK_CORR_LAG10_20_Rformat_3008_noempty.csv", header = TRUE, sep=";")
M_ALL_altitude<- read.table("Catch_info_Altitude.csv", header = TRUE, sep=",")
SAAR<-  read.table("SAAR.csv", header = TRUE, sep=",")

#pc ufficio
#setwd("D:/PROJECTS/Regional/RESULTS/UK/200619/UK_LAG10_CSV")

#Dep_A<- read.table("PARAM_UK_LAG10_ALL 20 DEP.csv", header = TRUE, sep=";")
#Ind_A<- read.table("PARAM_UK_LAG10_ALL 20 IND.csv", header = TRUE, sep=";")




M_ALL$Distance<- M_ALL$Distance/1000 

CC<-which(is.na(M_ALL[,1])=="FALSE")
M_ALL<- M_ALL[CC,]

#Remove 2 couple with NA
EmptySt<-which((M_ALL[,3]==""))

M_ALL<- M_ALL[-EmptySt,]

##NOW M_ALL it is the same of KT_M_Final_available_20data_ok ##############
#FROM: load("C:/Users/39349/Documents/Regional/Data/Processed Data/Processed_KT_Matrix.RData")##
##We preferred import it from excel to have Numerical matrix##################################

#Wrong Data Station
Wrongst<- which(M_ALL$ID_Station_1=="39004" | M_ALL$ID_Station_2=="39004" )

M_ALL<- M_ALL[-Wrongst,]


      ######## CATCHMENT INFO #####################
######################################################################
############### 29/10/19 ############################
     
     
#setwd("D:/PROJECTS/Regional/RESULTS/UK/200619")     
M_in<- read.table("Catch_info_final.csv", header = TRUE, sep=";")



DEP_m<-M_ALL


############# CREATION CATCHMENT INFO MATRIX ##################


CATCHM_DEPDEP<-matrix( , nrow =nrow(DEP_m)*2, ncol =ncol(M_in)+5+3)
colnames(CATCHM_DEPDEP)<-c(colnames(M_in),"N_couple","Distance","Class","SAAR_61-90","SAAR_41-70","Min Altitude","50 Altitude","Max-Min Altitude")
x<-1

for(i in 1:nrow(DEP_m))
{

  D1<- which(DEP_m[i,11]==M_in[,1])
  D2<- which(DEP_m[i,12]==M_in[,1])
  D3<- which(DEP_m[i,11]==SAAR[,1])
  D4<- which(DEP_m[i,12]==SAAR[,1])
  if(length(D1)!=0 & length(D2)!=0){
  CATCHM_DEPDEP[x,1:ncol(M_in)]<-as.numeric(M_in[D1,])
  CATCHM_DEPDEP[x+1,1:ncol(M_in)]<-as.numeric(M_in[D2,])
  CATCHM_DEPDEP[x,25]<-DEP_m[i,1]
  CATCHM_DEPDEP[x+1,25]<-DEP_m[i,1]
  CATCHM_DEPDEP[x,26]<-DEP_m[i,25]
  CATCHM_DEPDEP[x+1,26]<-DEP_m[i,25]
  CATCHM_DEPDEP[x,27]<-DEP_m[i,18]
  CATCHM_DEPDEP[x+1,27]<-DEP_m[i,18]
  CATCHM_DEPDEP[x,28]<- SAAR[D3,5]
  CATCHM_DEPDEP[x+1,28]<- SAAR[D4,5] 
  CATCHM_DEPDEP[x,29]<- SAAR[D3,6]
  CATCHM_DEPDEP[x+1,29]<- SAAR[D4,6]
  #add altitude
    D5<- which(DEP_m[i,11]==M_ALL_altitude[,1])
  D6<- which(DEP_m[i,12]==M_ALL_altitude[,1])
  if(length(D5)>0 & length(D6)>0){
  CATCHM_DEPDEP[x,30]<- M_ALL_altitude[D5,4]
  CATCHM_DEPDEP[x+1,30]<- M_ALL_altitude[D6,4]
  CATCHM_DEPDEP[x,31]<- M_ALL_altitude[D5,6]
  CATCHM_DEPDEP[x+1,31]<- M_ALL_altitude[D6,6]  
  CATCHM_DEPDEP[x,32]<- M_ALL_altitude[D5,8]- M_ALL_altitude[D5,4]
  CATCHM_DEPDEP[x+1,32]<- M_ALL_altitude[D6,8] -  M_ALL_altitude[D6,4] 
  }  
    x<-x+2} 
}  
###########################################################


CATCHM_DEPDEP<-data.frame(CATCHM_DEPDEP)

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

M_ALL_perc <- merge(M_ALL, Diff2, by.x = "CODE", by.y = "N_couple") 

#################END DATA PROCESSING ######################################

#########SAVE ALL DATAFRAME ############################

setwd("C:/Users/39349/Documents/Regional/Workspace")

save(M_ALL,M_ALL_perc,file="M_ALL_perc.RData")


###############SELECTION OF PVALUE #########################################



##################################
############################# ASY PART #################################
#Selection of Async data with more than 20 data and pvalue 0.01 #

for( i in 1:nrow(M_ALL))
{
  if( M_ALL$Num_Asyncr_occ[i]<20){M_ALL$X.1[i]<-NA}
  else{ if(M_ALL$KT_pvalue_Asy[i]<0.01){M_ALL$X.1[i]<-2} else{M_ALL$X.1[i]<-1}
  }
}

M_ALL_0.01<- M_ALL[M_ALL$KendalT.p.value<=0.01,]
M_ALL_0.01_negative<-M_ALL_0.01[M_ALL_0.01$KendalT.value<0,]

#########################################################################
Depdep_pos<-which(M_ALL_0.01$X.1==2)
Depind_pos<-which(M_ALL_0.01$X.1==1)

DepDep_0.01<-M_ALL_0.01[Depdep_pos,]
DepInd_0.01<-M_ALL_0.01[Depind_pos,]


################## DISTANCE SELECTION -> KM 230 ###############################

Dist_230<-which( M_ALL$Distance<=230)
M_near<-M_ALL[ Dist_230,]
M_far<- M_ALL[-Dist_230,]

M_near_01<-M_near[which(M_near$KendalT.p.value<=0.01),] 
M_far_01<-M_far[which(M_far$KendalT.p.value<=0.01),]




######################### SAVING POSITIVE DATA #################################
#ALL 1456 DATA 
M_ALL_0.01_perc<- rbind(Far_perc,Near_perc)
#ONLY POSITIVE ONES 1433
M_ALL_0.01_POS<-M_ALL_0.01[which(M_ALL_0.01$KendalT.value>0),]
#INFO MATRIX JUST FOR POSITIVE
M_ALL_0.01_POS_perc<-  M_ALL_0.01_perc[which(M_ALL_0.01_perc$KendalT.value>0),]
#Far and Near positive
Far_perc_POS<-Far_perc[which(Far_perc$KendalT.value>0),]
Near_perc_POS<-Near_perc[which(Near_perc$KendalT.value>0),]

M_near_01_POS<-M_near_01[which(M_near_01$KendalT.value>0),]
M_far_01_POS<-M_far_01[which(M_far_01$KendalT.value>0),]

setwd("C:/Users/39349/Documents/Regional/Workspace")

save(M_ALL_0.01_perc,M_ALL_0.01,M_ALL_0.01_POS,M_ALL_0.01_POS_perc,Far_perc_POS, Near_perc_POS,
     M_near_01_POS,M_far_01_POS,file="ALLDATA01_andonlyPOSITIVE.RData")


save(M_ALL_0.01_perc,M_ALL_0.01,file="M_ALL_001.RData")
############################################################################################################


############# CREATION CATCHMENT INFO MATRIX ##################

M_ALL_IND<-M_ALL[M_ALL$KendalT.p.value>0.015,]

IND_m<-M_ALL_IND

CATCHM_IND<-matrix( , nrow =nrow(IND_m)*2, ncol =ncol(M_in)+5+3)
colnames(CATCHM_IND)<-c(colnames(M_in),"N_couple","Distance","Class","SAAR_61-90","SAAR_41-70","Min Altitude","50 Altitude","Max-Min Altitude")
x<-1

for(i in 1:nrow(IND_m))
{
  
  D1<- which(IND_m[i,11]==M_in[,1])
  D2<- which(IND_m[i,12]==M_in[,1])
  D3<- which(IND_m[i,11]==SAAR[,1])
  D4<- which(IND_m[i,12]==SAAR[,1])
  if(length(D1)!=0 & length(D2)!=0){
    CATCHM_IND[x,1:ncol(M_in)]<-as.numeric(M_in[D1,])
    CATCHM_IND[x+1,1:ncol(M_in)]<-as.numeric(M_in[D2,])
    CATCHM_IND[x,25]<-IND_m[i,1]
    CATCHM_IND[x+1,25]<-IND_m[i,1]
    CATCHM_IND[x,26]<-IND_m[i,25]
    CATCHM_IND[x+1,26]<-IND_m[i,25]
    CATCHM_IND[x,27]<-IND_m[i,18]
    CATCHM_IND[x+1,27]<-IND_m[i,18]
    CATCHM_IND[x,28]<- SAAR[D3,5]
    CATCHM_IND[x+1,28]<- SAAR[D4,5] 
    CATCHM_IND[x,29]<- SAAR[D3,6]
    CATCHM_IND[x+1,29]<- SAAR[D4,6]
    #add altitude
    D5<- which(IND_m[i,11]==M_ALL_altitude[,1])
    D6<- which(IND_m[i,12]==M_ALL_altitude[,1])
    if(length(D5)>0 & length(D6)>0){
      CATCHM_IND[x,30]<- M_ALL_altitude[D5,4]
      CATCHM_IND[x+1,30]<- M_ALL_altitude[D6,4]
      CATCHM_IND[x,31]<- M_ALL_altitude[D5,6]
      CATCHM_IND[x+1,31]<- M_ALL_altitude[D6,6]  
      CATCHM_IND[x,32]<- M_ALL_altitude[D5,8]- M_ALL_altitude[D5,4]
      CATCHM_IND[x+1,32]<- M_ALL_altitude[D6,8] -  M_ALL_altitude[D6,4] 
    }  
    x<-x+2} 
}  
###########################################################


CATCHM_IND<-data.frame(CATCHM_IND)


############ Option 2 #################                     
#### Percentage index#######



MATRIX2<-CATCHM_IND


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

M_ALL_IND_perc <- merge(IND_m, Diff2, by.x = "CODE", by.y = "N_couple")

setwd("C:/Users/39349/Documents/Regional/Workspace")

save(M_ALL_IND_perc,M_ALL_IND, file="M_ALL_INDIPENDENT_perc.RData")



###end###

###### HYSTOGRAM ##################

par(mfrow=c(1,1)) 
hist(M_near_01$N.SY.N.ALL, col=rgb(0,0,1,1/4), xlab="Number Syn/N All" , main="Syncrony Histogram Comparison")  
hist(M_far_01$N.SY.N.ALL, add=T,col=rgb(1,0,0,1/4), xlab="Number Syn/N All")
legend("topright", legend=c("Near","Far"), col=c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)), pt.cex=2, pch=15)
#hist(M_far_01$N.SY.N.ALL,col=rgb(1,0,0,1/4), xlab="Number Syn/N All", main="Syncrony Histogram: far couple") 



##################################################################################

fp<-which(Far_perc$KendalT.value>0)
np<-which(Near_perc$KendalT.value>0)
Far_pos<-Far_perc[fp,]
Near_pos<-Near_perc[np,]

Far_neg<-Far_perc[-fp,]
Near_neg<-Near_perc[-np,]

fn<-which(Far_perc$KendalT.value<0)
nn<-which(Near_perc$KendalT.value<0)
Far_neg<-Far_perc[fn,]
Near_neg<-Near_perc[nn,]


        ######PLOT NEAR FAR DEPDEP DEPIND ########################
#####MNEAR######
setwd("D:/PROJECTS/Regional/Plot")

pdf(file=paste("NEAR_FARDEPDEPDEPIND_1006.pdf"),width=9, height=9, onefile=TRUE, family="Times", title="Trend variance_ALTITUDE",   
    fonts=NULL, version="1.4", paper="special", encoding="default",bg="transparent", fg="black", pointsize=16,                             
    pagecentre=TRUE, colormodel="rgb",useDingbats=TRUE, useKerning=TRUE, fillOddEven=FALSE)   


par(mfrow=c(2,1))

#Positive

P1<-which(Near_perc$X.1==1 & Near_perc$KendalT.value>0 )
P2<-which(Near_perc$X.1==2 & Near_perc$KendalT.value>0)
NearDepind_pos<-Near_perc[P1,]
NearDepdep_pos<-Near_perc[P2,]

par(mfrow=c(2,1))
boxplot(NearDepind_pos[,c(57,60,69)],ylim=c(0,1),col=c("green"),main="Near DepInd positive couple")
boxplot(NearDepdep_pos[,c(57,60,69)],ylim=c(0,1),col=c("red"),main="Near DepDep positive couple")

par(mfrow=c(2,1))
boxplot(NearDepind_pos[,c(57,58,69)],ylim=c(0,1),col=c("green"),main="Near DepInd positive couple")
boxplot(NearDepdep_pos[,c(57,58,69)],ylim=c(0,1),col=c("red"),main="Near DepDep positive couple")


#Negative

N1<-which(Near_perc$X.1==1 & Near_perc$KendalT.value<0 )
N2<-which(Near_perc$X.1==2 & Near_perc$KendalT.value<0)
NearDepind_neg<-Near_perc[N1,]
NearDepdep_neg<-Near_perc[N2,]

par(mfrow=c(2,1))
boxplot(NearDepind_neg[,c(57,60,69)],ylim=c(0,1),col=c("green"),main="Near DepInd negative couple")
boxplot(NearDepdep_neg[,c(57,60,69)],ylim=c(0,1),col=c("red"),main="Near DepDep negative couple")

par(mfrow=c(2,1))
boxplot(NearDepind_neg[,c(57,58,69)],ylim=c(0,1),col=c("green"),main="Near DepInd negative couple")
boxplot(NearDepdep_neg[,c(57,58,69)],ylim=c(0,1),col=c("red"),main="Near DepDep negative couple")

##############################################################################################################


#####MFAR######

par(mfrow=c(2,1))

#Positive
Fp1<-which(Far_perc$X.1==1 & Far_perc$KendalT.value>0 )
Fp2<-which(Far_perc$X.1==2 & Far_perc$KendalT.value>0)
FarDepind_pos<-Far_perc[Fp1,]
FarDepdep_pos<-Far_perc[Fp2,]

par(mfrow=c(2,1))
boxplot(FarDepind_pos[,c(57,60,69)],ylim=c(0,1),col=c("green"),main="Far DepInd positive couple")
boxplot(FarDepdep_pos[,c(57,60,69)],ylim=c(0,1),col=c("red"),main="Far DepDep positive couple")



#Negative
Fn1<-which(Far_perc$X.1==1 & Far_perc$KendalT.value<0 )
Fn2<-which(Far_perc$X.1==2 & Far_perc$KendalT.value<0)
FarDepind_neg<-Far_perc[Fn1,]
FarDepdep_neg<-Far_perc[Fn2,]

par(mfrow=c(2,1))
boxplot(FarDepind_neg[,c(57,60,69)],ylim=c(0,1),col=c("green"),main="Far DepInd negative couple")
boxplot(FarDepdep_neg[,c(57,60,69)],ylim=c(0,1),col=c("red"),main="Far DepDep negative couple")



dev.off()

########################### COMPOUND #########################################


FarDepind_pos
FarDepdep_pos
FarDepind_neg
FarDepdep_neg

NearDepind_pos
NearDepdep_pos
NearDepind_neg
NearDepdep_neg

par(mfrow=c(2,2))
boxplot( NearDepdep_pos[,58]*NearDepdep_pos[,57]* NearDepdep_pos[,60]* NearDepdep_pos[,69],col="red",ylim=c(0,0.05),main="Near Dep Dep couple BFIHOST*DSB*Propw*SAAR")
boxplot( NearDepind_pos[,58]* NearDepind_pos[,57]* NearDepind_pos[,60]* NearDepind_pos[,69],col="green",ylim=c(0,0.05),main="Near Dep ind couple BFIHOST*DSB*Propw*SAAR")

par(mfrow=c(2,2))
boxplot( FarDepdep_pos[,58]*FarDepdep_pos[,57]* FarDepdep_pos[,60]* FarDepdep_pos[,69],col="red",ylim=c(0,0.05),main="Far Dep Dep couple BFIHOST*DSB*Propw*SAAR")
boxplot( FarDepind_pos[,58]* FarDepind_pos[,57]* FarDepind_pos[,60]* FarDepind_pos[,69],col="green",ylim=c(0,0.05),main="Far Dep ind couple BFIHOST*DSB*Propw*SAAR")


par(mfrow=c(2,2))
boxplot( NearDepdep_pos[,57]* NearDepdep_pos[,69],col="red",ylim=c(0,0.4),main="Near Dep Dep couple BFIHOST*Propw*SAAR")
boxplot( NearDepind_pos[,57]* NearDepind_pos[,69],col="green",ylim=c(0,0.4),main="Near Dep ind couple BFIHOST*Propw*SAAR")

boxplot( FarDepdep_pos[,57]* FarDepdep_pos[,69],col="red",ylim=c(0,0.4),main="Far Dep Dep couple BFIHOST*Propw*SAAR")
boxplot( FarDepind_pos[,57]* FarDepind_pos[,69],col="green",ylim=c(0,0.4),main="Far Dep ind couple BFIHOST*Propw*SAAR")

############### ANALYSIS 08/01 ###################################
par(mfrow=c(1,2))

boxplot(NearDepdep_pos$N.SY.N.ALL,NearDepind_pos$N.SY.N.ALL ,main="Syncronization",col=c("green","red"), names=c("Dep Dep","Dep Ind"), ylim=c(0,1) )

boxplot(FarDepdep_pos$N.SY.N.ALL,FarDepind_pos$N.SY.N.ALL ,main="Syncronization",col=c("green","red"), names=c("Dep Dep","Dep Ind"), ylim=c(0,1) )

Table<-matrix (,nrow=4,ncol=5)
rownames(Table)<-c("Near Dep-Dep","Near Dep- Ind","Far Dep-Dep", "Far Dep-Ind")
colnames(Table)<-c("Kendall Tau","N Syn/Tot","BFHOST","Propwet","SAAR")

Table[1,1]<-nrow(NearDepdep_pos)
Table[2,1]<-nrow(NearDepind_pos)
Table[3,1]<-nrow(FarDepdep_pos)
Table[4,1]<-nrow(FarDepind_pos)

Table[1,2]<-nrow(NearDepdep_neg)
Table[2,2]<-nrow(NearDepind_neg)
Table[3,2]<-nrow(FarDepdep_neg)
Table[4,2]<-nrow(FarDepind_neg)

Table2<-Table[1:4,1:2]



######Table positive ###

HY_Syn<-Near_perc$N.SY.N.ALL>0.60
HY_Syn<-Near_perc[Near_perc$N.SY.N.ALL>0.60,]

Table_p<-matrix (,nrow=5,ncol=8)
rownames(Table_p)<-c("Hy_Syn","Near Dep-Dep","Near Dep- Ind","Far Dep-Dep", "Far Dep-Ind")
colnames(Table_p)<-c("Kendall Tau","N Syn/Tot","BFHOST","DSPBAR","Max Altitude","Propwet","SAAR","Distance")

Coef<-c(4,15,57,58,50,60,69,44)
xx<-1
for( i in 1:ncol(Table_p))
{

x<-Coef[xx]
Table_p[1,i]<-round(median(HY_Syn[,x]),digit=3)
Table_p[2,i]<-round(median(NearDepdep_pos[,x]),digit=3)
Table_p[3,i]<-round(median(NearDepind_pos[,x]),digit=3)
Table_p[4,i]<-round(median(FarDepdep_pos[,x]),digit=3)
Table_p[5,i]<-round(median(FarDepind_pos[,x]),digit=3)
xx<-xx+1
}

df_p <- data.frame(Table_p)
png("Table_positive.png", height = 50*nrow(df_p ), width = 150*ncol(df_p ))
p_p<-tableGrob(df_p)
grid.arrange(p_p)
dev.off()

######Table negative ###


Table_n<-matrix (,nrow=4,ncol=5)
rownames(Table_n)<-c("Near Dep-Dep","Near Dep- Ind","Far Dep-Dep", "Far Dep-Ind")
colnames(Table_n)<-c("Kendall Tau","N Syn/Tot","BFHOST","Propwet","SAAR")


Coef<-c(4,15,57,60,69)
xx<-1
for( i in 1:5)
{

x<-Coef[xx]
Table_n[1,i]<-round(median(NearDepdep_neg[,x]),digit=3)
Table_n[2,i]<-round(median(NearDepind_neg[,x]),digit=3)
Table_n[3,i]<-round(median(FarDepdep_neg[,x]),digit=3)
Table_n[4,i]<-round(median(FarDepind_neg[,x]),digit=3)
xx<-xx+1
}

df_n <- data.frame(Table_n)
png("Table_negative.png")
p_n<-tableGrob(df_n)
grid.arrange(p_n)
dev.off()

###### Table summary################J
setwd("C:/Users/39349/Documents/Regional/Summary tables")


Table_s<-matrix (,nrow=2,ncol=6)
rownames(Table_s)<-c("Near","Far")
colnames(Table_s)<-c("All","%SY>60%","Positive Dep Dep","Positive Dep Ind","Negative Dep Dep","Negative Dep Ind")

Table_s[1,1]<-nrow(Near_perc)
Table_s[1,2]<-length(which(Near_perc$N.SY.N.ALL>0.60))
Table_s[1,3]<-nrow(NearDepdep_pos)
Table_s[1,4]<-nrow(NearDepind_pos)
Table_s[1,5]<-nrow(NearDepdep_neg)
Table_s[1,6]<-nrow(NearDepind_neg)

Table_s[2,1]<-nrow(Far_perc)
Table_s[2,2]<-length(which(Far_perc$N.SY.N.ALL>0.60))
Table_s[2,3]<-nrow(FarDepdep_pos)
Table_s[2,4]<-nrow(FarDepind_pos)
Table_s[2,5]<-nrow(FarDepdep_neg)
Table_s[2,6]<-nrow(FarDepind_neg)

df_s <- data.frame(Table_s)
png("Table_summary tot2.png", height = 50*nrow(df), width = 100*ncol(df))
p_s<-tableGrob(df_s)
grid.arrange(p_s)
dev.off()
 #########################
 
 Table_n2<-matrix (,nrow=4,ncol=7)
rownames(Table_n2)<-c("Near Dep-Dep","Near Dep- Ind","Far Dep-Dep", "Far Dep-Ind")
colnames(Table_n2)<-c("Kendall Tau","N Syn/Tot","BFHOST","DSPBAR","Max Altitude","Propwet","SAAR")

Coef<-c(4,15,57,58,50,60,69)
xx<-1
for( i in 1:ncol(Table_n2))
{

x<-Coef[xx]
Table_n2[1,i]<-round(median(NearDepdep_neg[,x]),digit=3)
Table_n2[2,i]<-round(median(NearDepind_neg[,x]),digit=3)
Table_n2[3,i]<-round(median(FarDepdep_neg[,x]),digit=3)
Table_n2[4,i]<-round(median(FarDepind_neg[,x]),digit=3)
xx<-xx+1
}

df_n2 <- data.frame(Table_n2)
png("Table_negative2.png", height = 50*nrow(df_n2 ), width = 150*ncol(df_n2 ))
p_n2<-tableGrob(df_n2)
grid.arrange(p_n2)
dev.off()



#######################################################################



v1<-summary(NearDepdep_pos[,57])
v1<-summary(NearDepdep_pos[,57])

# useless #
#Near
boxplot(NearDepdep_pos$N.SY.N.ALL,NearDepind_pos$N.SY.N.ALL ,main="Syncronization",col=c("green","red"), names=c("Dep Dep","Dep Ind"), ylim=c(0,1) )

boxplot( NearDepdep_pos[,60]*NearDepdep_pos[,57]* NearDepdep_pos[,69],col="red",ylim=c(0,0.05),main="Dep Dep couple")
boxplot( NearDepind_pos[,60]* NearDepind_pos[,57]*  NearDepind_pos[,69],col="green",ylim=c(0,0.05),main="Dep ind couple")

#Far
boxplot(FarDepdep_pos$N.SY.N.ALL,FarDepind_pos$N.SY.N.ALL ,main="Syncronization",col=c("green","red"), names=c("Dep Dep","Dep Ind"), ylim=c(0,1) )

boxplot( FarDepdep_pos[,60]*FarDepdep_pos[,57]* FarDepdep_pos[,69],col="red",ylim=c(0,0.05),main="Dep Dep couple")
boxplot( FarDepind_pos[,60]* FarDepind_pos[,57]*  FarDepind_pos[,69],col="green",ylim=c(0,0.05),main="Dep ind couple")




########################################################################

pdf(file=paste("Boxplot_3group.pdf"),width=9, height=9, onefile=TRUE, family="Times", title="Trend variance_ALTITUDE",   
    fonts=NULL, version="1.4", paper="special", encoding="default",bg="transparent", fg="black", pointsize=16,                                pagecentre=TRUE, colormodel="rgb",useDingbats=TRUE, useKerning=TRUE, fillOddEven=FALSE)   

p<-c(51,52,53,58,57,60,69)

par(mfrow=c(2,2))

for (i in p)

{
boxplot(Group1ALL[,i],Group2ALL[,i],Group3ALL[,i],col=c("red","yellow","green"),names=c("Group1","Group2","Group3"))

}
dev.off()

#####################BOXPLOT 4 GROUP ########################################

p<-c(51,52,53,58,57,60,69)

pdf(file="Boxplot_4GROUP_solopos.pdf",width=9, height=9, onefile=TRUE, family="Times", title="Trend variance_ALTITUDE",
    fonts=NULL, version="1.4", paper="special", encoding="default",bg="transparent", fg="black", pointsize=16,
    pagecentre=TRUE, colormodel="rgb",useDingbats=TRUE, useKerning=TRUE, fillOddEven=FALSE)
  par(mfrow=c(2,2))

for (i in p)
    
{


boxplot(Group1ALL[,i],Group2ALL[,i],Group3ALL[,i][Group3ALL$X.1==1],Group3ALL[,i][Group3ALL$X.1==2],col=c("red","yellow","green","purple","blue"),names=c("Group1","Group2","Gr3-DepInd","Gr3-DepDep"),ylim=c(0,1), main=c(Names[i]))
    
}
dev.off() 


Group3ALL <-Group3ALL[which(Group3ALL$KendalT.value>0),]

plot(Group3ALL[,69][Group3ALL$X.1==1]* Group3ALL[,60][Group3ALL$X.1==1]*Group3ALL[,58][Group3ALL$X.1==1]* Group3ALL[,53][Group3ALL$X.1==1] )

boxplot(Group3ALL[,69][Group3ALL$X.1==2]* Group3ALL[,60][Group3ALL$X.1==2]*Group3ALL[,58][Group3ALL$X.1==2]* Group3ALL[,53][Group3ALL$X.1==2] , Group3ALL[,69][Group3ALL$X.1==1]* Group3ALL[,60][Group3ALL$X.1==1]*Group3ALL[,58][Group3ALL$X.1==1]* Group3ALL[,53][Group3ALL$X.1==1] , col=c("red","green"))
                                       
                                       
#############################################

FAR_DEPDEP<-FarDepdep_pos[,c(1,4,13,15,25,44:60,69)]
FAR_DEPIND<-FarDepind_pos[,c(1,4,13,15,25,44:60,69)]
NEAR_DEPDEP<-NearDepdep_pos[,c(1,4,13,15,25,44:60,69)]
NEAR_DEPIND<-NearDepind_pos[,c(1,4,13,15,25,44:60,69)]

setwd("D:/PROJECTS/Regional/DISTANCE_selection/Asy_Analysis")

#p<-c(51,52,53,58,57,60,69)

Names<-colnames(FAR_DEPDEP)

pdf(file="Boxplot_far_asy.pdf",width=9, height=9, onefile=TRUE, family="Times", title="Trend variance_ALTITUDE",
    fonts=NULL, version="1.4", paper="special", encoding="default",bg="transparent", fg="black", pointsize=16,
    pagecentre=TRUE, colormodel="rgb",useDingbats=TRUE, useKerning=TRUE, fillOddEven=FALSE)
  par(mfrow=c(2,2))

for (i in 1:ncol(FAR_DEPDEP))
    
{


boxplot(FAR_DEPDEP[,i],FAR_DEPIND[,i],col=c("red","green"),names=c("FAR DEPDEP","FAR DEPIND"),ylim=c(0,1), main=c(Names[i]))
    
}
dev.off() 
###############################################

pdf(file="Boxplot_near_asy.pdf",width=9, height=9, onefile=TRUE, family="Times", title="Trend variance_ALTITUDE",
    fonts=NULL, version="1.4", paper="special", encoding="default",bg="transparent", fg="black", pointsize=16,
    pagecentre=TRUE, colormodel="rgb",useDingbats=TRUE, useKerning=TRUE, fillOddEven=FALSE)
  par(mfrow=c(2,2))

for (i in 1:ncol(NEAR_DEPDEP))
    
{
boxplot(NEAR_DEPDEP[,i],NEAR_DEPIND[,i],col=c("red","green"),names=c("NEAR DEPDEP","NEAR DEPIND"),ylim=c(0,1), main=c(Names[i]))
   
}
dev.off() 


########################################

pdf(file="Boxplot_hysyn.pdf",width=9, height=9, onefile=TRUE, family="Times", title="Trend variance_ALTITUDE",
    fonts=NULL, version="1.4", paper="special", encoding="default",bg="transparent", fg="black", pointsize=16,
    pagecentre=TRUE, colormodel="rgb",useDingbats=TRUE, useKerning=TRUE, fillOddEven=FALSE)
  par(mfrow=c(2,2))

for (i in 1:ncol(NEAR_DEPDEP))
    
{
boxplot(HY_SYN[,i],col=c("green"),names=c("HY_SYN DEPDEP"),ylim=c(0,1), main=c(Names[i]))
   
}
dev.off() 

#######################################


GROUPA<-Near_perc[which(Near_perc$KendalT.value>=0.6 & Near_perc$N.SY.N.ALL >=0.6),c(1,4,11,12,13,15,25,44:60,69)]
GROUPB<-Near_perc[which(Near_perc$KendalT.value<0.6 & Near_perc$N.SY.N.ALL >=0.6),c(1,4,11,12,13,15,25,44:60,69)]
GROUPC<-Near_perc[which(Near_perc$KendalT.value>=0.6 & Near_perc$N.SY.N.ALL <0.6),c(1,4,11,12,13,15,25,44:60,69)]
GROUPD<-Near_perc[which(Near_perc$KendalT.value<0.6 & Near_perc$N.SY.N.ALL <0.6),c(1,4,11,12,13,15,25,44:60,69)]




Names1<-colnames(GROUPA)
pdf(file="GROUP_symil3.pdf",width=9, height=9, onefile=TRUE, family="Times", title="Trend variance_ALTITUDE",
    fonts=NULL, version="1.4", paper="special", encoding="default",bg="transparent", fg="black", pointsize=16,
    pagecentre=TRUE, colormodel="rgb",useDingbats=TRUE, useKerning=TRUE, fillOddEven=FALSE)
  par(mfrow=c(2,2))



for (i in 1:ncol(GROUPA))
    
{
boxplot(GROUPA[,i],GROUPB[,i],GROUPC[,i],GROUPD[,i],col=c("red","green","yellow","blue"),names=c("GR. A","GR. B","GR. C","GR. D"), main=c(Names1[i]))
   
}
dev.off() 

################################################

