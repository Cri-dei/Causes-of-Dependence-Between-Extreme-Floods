##############à




load("C:/Users/39349/Documents/Regional/Code/00.Data Processing/Processed_Difference.RData")
load("C:/Users/39349/Documents/Regional/Code/00.Data Processing/Processed_KT_Matrix.RData")

#Delete two stations with problems
Wrongst1<- which(Difference_M_Final_available_20data_ok$Station_1=="39004" | Difference_M_Final_available_20data_ok$Station_2=="39004" )

Difference<-Difference_M_Final_available_20data_ok[-Wrongst1,]

####Plot: somma dei lag time di tutti gli eventi considerati###############
setwd("C:/Users/39349/Documents/Regional/CODE/01. FIxed Code paper/Difference_analysis")


Somma<- colSums(Difference[,c(7:14)])
Somma<-t(Somma)

########### LAG TIME FOR EACH EVENTS###

png(filename = "Lagtime.png",
    width = 10.33, height = 7, units = "in",   res =400) 

plot(Somma, xlab="Lag time [days]", ylab="Total number of events", xaxt = "n")
axis(1, at=1:8, labels=c(0,1,2,3,4,5,"5-10",10))

dev.off()

######## SUMMARY LAG TIME ###############

png("Summary_lagtime.png", height =50, width = 50*length(Somma))
grid.table(Somma, cols = colnames(Somma))
dev.off()

############## MINIMUM LAG TIME zoom #########################

png(filename = "Minimum_ Lagtime_zoom.png",
    width = 10.33, height = 7, units = "in",   res =400) 

Minimum<-which(Difference$Min<30)

hist(Difference$Min[Minimum], xlab=c("Minimum Lag time [days]"), 
     main="Zoom: Histogram of minimum lag time", ylab="Number of couples")

dev.off()

#################### MINIMUM LAG TIME all #############################


png(filename = "Minimum_ Lagtime_all.png",
    width = 10.33, height = 7, units = "in",   res =400) 

hist(Difference$Min, xlab=c("Minimum Lag time [days]"), 
     main="Histogram of minimum lag time", ylab="Number of couples")
dev.off()
###################  MAXIMUM LAG TIME ####################################àà

png(filename = "Maximum_ Lagtime_all.png",
    width = 10.33, height = 7, units = "in",   res =400) 

hist(Difference$Max, xlab=c("Maximum Lag time [days]"),
     main="Histogram of maximum lag time",xlim=c(0,400), ylab="Number of couples")
dev.off()

########################################################################

hist(Difference$Media, xlab=c("Maximum Lag time [days]"),
     main="Histogram of maximum lag time",xlim=c(0,400))

#################################################################
#Plot zoom #
plot(Somma, xlab="Lag time [days]", ylab="Total number of events",ylim=c(0,50000))



#Difference$sum <- rowSums(Difference[,c(3,13)])

par(mfrow=c(2,1)) 

plot(Difference$lag5_10)
hist(Somma)



