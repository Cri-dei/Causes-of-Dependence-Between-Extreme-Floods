
#### FINAL NUMBERS FOR PAPER

#num station considered

length(Available_st)

#Overall couple

nrow(POT_matrix)

#Couple selected

nrow(M_ALL)

#Pvalue bonf

Pv_th

#Dependent couple

N_dep<-nrow(DEP)
N_ind<-nrow(IND)


nrow(DEP)+nrow(IND)== nrow(M_ALL)

100*nrow(DEP)/ nrow(M_ALL)

100*nrow(IND)/ nrow(M_ALL)

length(which(DEP$KendalT.value<0))


DEP

100*length(which(DEP$N.SY.N.ALL>=0.6))/nrow(DEP)
100*length(which(DEP$N.SY.N.ALL>=0.4))/nrow(DEP)

DEP_POS<-DEP[which(DEP$KendalT.value>0),]

nrow(DEP_POS)

DEP_near<-DEP_POS[which(DEP_POS$Distance<=230),]
DEP_far<-DEP_POS[which(DEP_POS$Distance>230),]

DEP_asy20<-DEP_POS[which(DEP_POS$Num_Asyncr_occ>=20),]

max(DEP_asy20$N.SY.N.ALL)

length(which(DEP_asy20$))

