P1_DEP<-levelplot(t(M_grid1_DEP), col.regions = coul, cex.main=0.05, 
                  xlab="Meteorological Dissimilarity Index",ylab="Hydrological Dissimilarity Index",
                  at=c(seq(-1,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)),colorkey=list(space="bottom"),
                  par.settings = list(layout.heights=list(xlab.key.padding=1)))
#colorkey=FALSE , 
P2_DEP<-levelplot(t(M_grid2_DEP), col.regions = coul,cex.main=0.05, 
                  xlab="Meteorological Dissimilarity Index",ylab="Climatological Dissimilarity Index",
                  at=c(seq(-1,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)),colorkey=list(space="bottom"),
                  par.settings = list(layout.heights=list(xlab.key.padding=1))) 

P3_DEP<-levelplot(t(M_grid3_DEP), col.regions = coul,cex.main=0.05 ,
                  xlab="Climatological Index",ylab="Hydrological Index",at=c(seq(-1,1,0.05)), 
                  scales=list( y=list(rot=0), x=list(rot=45)),colorkey=list(space="bottom"),
                  par.settings = list(layout.heights=list(xlab.key.padding=1)))

P1_IND<-levelplot(t(M_grid1_IND), col.regions = coul, main="",cex=0.05,
                  xlab="Meteorological Dissimilarity Index",ylab="Hydrological Dissimilarity Index",
                  at=c(seq(-1,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)),colorkey=list(space="bottom"),
                  par.settings = list(layout.heights=list(xlab.key.padding=1)) )

P2_IND<-levelplot(t(M_grid2_IND), col.regions = coul,  main="",cex.main=0.05,
                  xlab="Meteorological Dissimilarity Index",ylab="Climatological Dissimilarity Index",
                  at=c(seq(-1,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)),
                  colorkey=list(space="bottom"),
                  par.settings = list(layout.heights=list(xlab.key.padding=1)))

P3_IND<-levelplot(t(M_grid3_IND), col.regions = coul, main=" ",cex.main=0.05, 
                  xlab="Climatological Index",ylab="Hydrological Index",at=c(seq(-1,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)),
                  colorkey=list(space="bottom"),
                  par.settings = list(layout.heights=list(xlab.key.padding=1)))
 ######################
 
 
 
 P1_DEP<-levelplot(t(M_grid1_DEP), col.regions = coul, cex.main=0.05, 
                  xlab="Meteorological Dissimilarity Index",ylab="Hydrological Dissimilarity Index",
                  at=c(seq(-1,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)),colorkey=FALSE )
#colorkey=FALSE , 
P2_DEP<-levelplot(t(M_grid2_DEP), col.regions = coul,cex.main=0.05, 
                  xlab="Meteorological Dissimilarity Index",ylab="Climatological Dissimilarity Index",
                  at=c(seq(-1,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)),colorkey=FALSE ) 

P3_DEP<-levelplot(t(M_grid3_DEP), col.regions = coul,cex.main=0.05 ,
                  xlab="Climatological Index",ylab="Hydrological Index",at=c(seq(-1,1,0.05)), 
                  scales=list( y=list(rot=0), x=list(rot=45)),colorkey=FALSE ))

P1_IND<-levelplot(t(M_grid1_IND), col.regions = coul, main="",cex=0.05,
                  xlab="Meteorological Dissimilarity Index",ylab="Hydrological Dissimilarity Index",
                  at=c(seq(-1,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)),colorkey=FALSE  )

P2_IND<-levelplot(t(M_grid2_IND), col.regions = coul,  main="",cex.main=0.05,
                  xlab="Meteorological Dissimilarity Index",ylab="Climatological Dissimilarity Index",
                  at=c(seq(-1,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)),
                 colorkey=FALSE )

P3_IND<-levelplot(t(M_grid3_IND), col.regions = coul, main=" ",cex.main=0.05, 
                  xlab="Climatological Index",ylab="Hydrological Index",at=c(seq(-1,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)),
                  colorkey=FALSE )

















colorkey=FALSE
GMOD<-grid.arrange(P1_DEP, P1_IND, P2_DEP, P2_IND, P3_DEP, P3_IND,nrow=3,ncol=2, 
             top = textGrob("DEPENDENT                                                                     INDEPENDENT",gp=gpar(fontsize=18,font=3)), heights=c(0.9,0.9,0.9) )

ggsave("DDMAP_DEPINDMOD3.pdf",GMOD, width = 32, height = 34,  units = "cm")

latticeCombineGrid(P1_DEP, P1_IND, P2_DEP, P2_IND, P3_DEP, P3_IND, layout = c(3, 8))

G1<-grid.arrange(P1_DEP, P1_IND, P2_DEP, P2_IND, P3_DEP, P3_IND,nrow=3,ncol=2, 
                 top = textGrob("DEPENDENT                                                                     INDEPENDENT",gp=gpar(fontsize=18,font=3)), heights=c(0.9,0.9,0.9) )

ggsave("DDMAP_DEP_IND.pdf",G1, width = 33, height = 36, units = "cm")




G1<-grid.arrange(arrangeGrob(P1_DEP, P2_DEP, P3_DEP, top="Dependent"),
                 arrangeGrob(P1_IND, P2_IND, P3_IND, top="Independent"), 
                 ncol=2)

ggsave("mtcars7.pdf",G1, width = 33, height = 36, units = "cm")

library(ggplot2)
library(patchwork)

install.packages("patchwork")



comb_levObj <- c(P1_DEP, P2_DEP, P3_DEP, P1_IND,P2_IND,P3_IND, layout = c(2, 3), merge.legends = FALSE)
print(comb_levObj)

)

comb_levObj <- c(P1_DEP, P2_DEP, layout = c(1, 2), merge.legends = TRUE)
print(comb_levObj)

update(comb_levObj, scales = list(y = list(rot = 0)),
       ylab = c("proportional distribution", "number of phones"))


      

Another option would be to define grid viewports and insert the colorkey manually using draw.colorkey. Based on the solution suggested by @rcs, the code would then roughly look as follows.

library(grid)

## breaks and colors
at <- seq(-1.1, 1.1, .01)
cols <- terrain.colors(350)

## create plot
P2_IND<-levelplot(t(M_grid2_IND), col.regions = coul,  main="",cex.main=0.05,
                  xlab="Meteorological Dissimilarity Index",ylab="Climatological Dissimilarity Index",
                  at=c(seq(-1,1,0.05)), scales=list( y=list(rot=0), x=list(rot=45)),colorkey=FALSE )
## start png device
png("~/KKKK.png", width = 8, height = 9, units = "cm", res = 150)

## insert plot
grid.newpage()
vp_fig <- viewport(x = 0, y = .1, width = 1, height = .9, 
                   just = c("left", "bottom"))
pushViewport(vp_fig)
print(P2_IND, newpage = FALSE)

## insert colorkey
downViewport(trellis.vpname("figure"))
vp_key <- viewport(x = .5, y = -.4)
pushViewport(vp_key)
draw.colorkey(key = list(col = cols, at = at, width = .6, height = .6, 
                         space = "bottom"), draw = TRUE)
dev.off()













