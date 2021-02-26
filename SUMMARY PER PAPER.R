
load("C:/PROJECTS 2021/QQ/Results_update/Max_annual_by_POT/lag3/M_ALL_newdataset.RData")


#### FINAL NUMBERS FOR PAPER

#num station considered

length(Available_st)

#Overall couple

nrow(POT_matrix)

#Couple selected

nrow(M_ALL)

#Negative values

length(which(M_ALL$KendalT.value<0))

100*length(which(M_ALL$KendalT.value<0))/ nrow(M_ALL)

M_ALL_dep<-M_ALL[which(M_ALL$KendalT.p.value<=0.0017),]

#Pvalue bonf

Pv_th

#Dependent couple

N_dep<-nrow(M_ALL_DEP)
N_ind<-nrow(M_ALL_IND)


nrow(M_ALL_DEP)+nrow(M_ALL_IND)== nrow(M_ALL)

100*nrow(M_ALL_DEP)/ nrow(M_ALL)

100*nrow(M_ALL_IND)/ nrow(M_ALL)

length(which(M_ALL_DEP$KendalT.value<0))


DEP<-M_ALL_DEP

100*length(which(M_ALL_DEP$N.SY.N.ALL>=0.6))/nrow(M_ALL_DEP)
100*length(which(M_ALL_DEP$N.SY.N.ALL>=0.4))/nrow(M_ALL_DEP)

DEP_POS<-DEP[which(DEP$KendalT.value>0),]

nrow(DEP_POS)

Dist_sel<-210

DEP_near<-M_ALL_DEP_POS[which(M_ALL_DEP_POS$Distance<=210),]
DEP_far<-M_ALL_DEP_POS[which(M_ALL_DEP_POS$Distance>210),]


summary(DEP_near$N.SY.N.ALL)
summary(DEP_far$N.SY.N.ALL)

summary(DEP_far$KendalT.value)
summary(DEP_near$KendalT.value)


DEP_asy20<-M_ALL_DEP_POS[which(M_ALL_DEP_POS$Num_Asyncr_occ>=20),]

DEP_asy_less20<-M_ALL_DEP_POS[which(M_ALL_DEP_POS$Num_Asyncr_occ<20),]
 
max(DEP_asy20$N.SY.N.ALL[which(DEP_asy20$X.1==1 | DEP_asy20$X.1==2 )])

length(which(DEP_asy20$X.1==1))

length(which(DEP_asy20$X.1==2))

nrow(DEP_asy20)



100*length(which(DEP_asy20$X.1==2))/
  nrow(DEP_asy20)



100*length(which(DEP_asy20$X.1==1))/
  nrow(DEP_asy20)





