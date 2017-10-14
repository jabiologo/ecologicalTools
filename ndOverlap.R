library(ggplot2)#
library(grid)#
library(gridExtra)#
library(gtable)
library(dismo)

scores_lav<-rbind(scores_ace,scores_mar)
write.csv(scores_lav, "plot_niche/scores_lav.csv")
scores<-read.csv("plot_niche/scores_lav.csv")

sc1<- data.frame(cbind( rep(runif(20,0.5,1.5)) , rep(runif(20,0,2.2)) ))
sc2<- data.frame(cbind( rep(runif(20,0,1)) , rep(runif(20,-1,1)) ))
names(sc1)<-c("Axis1","Axis2")
names(sc2)<-c("Axis1","Axis2")

niceOverPlot<-function(sc1,sc2=NULL,n1=NULL,n2=NULL, plot.axis = TRUE, bw = NULL, b=NULL, a1cont=NULL, a2cont=NULL){
 
  if (is.null(sc2))
   {sc_1<-sc1
    sc_2<-sc1
    sc1<- sc_1$li[1:n1,]
    sc2<- sc_1$li[(n1+1):(n1+n2),]
   }
    
  if (class(sc1)==c("pca","dudi") && class(sc2)==c("pca","dudi")) 
   {sc_1<-sc1
    sc_2<-sc1
    sc1<- sc1$li
    sc2<- sc2$li}
  
  scores<-rbind(sc1,sc2)
  g<-c(rep(0,nrow(sc1)),rep(1,nrow(sc2)))
  df<-data.frame(cbind(scores$Axis1,scores$Axis2,g))
  names(df)<-c("x","y","g")
  df$g<-as.factor(df$g)
  
  #placeholder plot - prints nothing at all
  empty <- ggplot()+geom_point(aes(1,1), colour="white") +
    theme(                              
      plot.background = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.border = element_blank(), 
      panel.background = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )
  # plot with red fill
  p1 <- ggplot(data = df, aes(x, y,color = as.factor(g))) +
    stat_density2d(aes(fill = ..level..), alpha = 0.2, bins=b, geom = "polygon", h=c(bw,bw)) +
    scale_fill_continuous(low = "orange", high = "orange4", space = "Lab") +
    scale_colour_discrete(guide = FALSE) + scale_x_continuous(name = "axis1", limits= c(min(df$x)-100, max(df$x)+200))+
    scale_y_continuous(name = "axis2", limits= c(min(df$y)-100, max(df$y)+200))+
    theme(legend.position="none")
  # plot with blue fill
  p2 <- ggplot(data = df, aes(x, y, color = as.factor(g))) +
    stat_density2d(aes(fill = ..level..), alpha = 0.2, bins=b, geom = "polygon", h=c(bw,bw)) +
    scale_fill_continuous(low = "purple", high = "purple4", space = "Lab") +
    scale_colour_discrete(guide = FALSE) +  scale_x_continuous(name = "axis1", limits=c(min(df$x)-100, max(df$x)+200))+
    scale_y_continuous(name = "axis2", limits= c(min(df$y)-100, max(df$y)+200))+
    theme(legend.position="none")
  # grab plot data
  pp1 <- ggplot_build(p1)
  ppp1 <- ggplot_build(p1 + aes(alpha=0.15) + theme_classic() + theme(legend.position="none") + theme(text = element_text(size=15)) + xlab("axis1") + ylab("axis2") + xlim(c(min(pp1$data[[1]]$x)-0.5,max(pp1$data[[1]]$x)+0.5)) + ylim(c(min(pp1$data[[1]]$y)-0.5,max(pp1$data[[1]]$y)+0.5)))
  pp2 <- ggplot_build(p2 + aes(alpha=0.15) + theme_classic() + theme(legend.position="none")+ xlab("axis1") + ylab("axis2")  + xlim(c(min(pp1$data[[1]]$x)-0.5,max(pp1$data[[1]]$x)+0.5)) + ylim(c(min(pp1$data[[1]]$y)-0.5,max(pp1$data[[1]]$y)+0.5)))$data[[1]]
  # replace red fill colours in pp1 with blue colours from pp2 when group is 2
  ppp1$data[[1]]$fill[grep(pattern = "^2", pp2$group)] <- pp2$fill[grep(pattern = "^2", pp2$group)]
  # build plot grobs
  grob1 <- ggplot_gtable(ppp1)
  grob2 <- ggplotGrob(p2)
  # build legend grobs
  #leg1 <- gtable_filter(grob1, "guide-box") 
  #leg2 <- gtable_filter(grob2, "guide-box") 
  #leg <- gtable:::rbind_gtable(leg1[["grobs"]][[1]],  leg2[["grobs"]][[1]], "first")
  # replace legend in 'red' plot
  #grob1$grobs[grob1$layout$name == "guide-box"][[1]] <- leg
  # plot
  #grob1$grobs[grob1$layout$name == "guide-box"][[1]]
  grid.newpage()
  grid.draw(grob1)
  
  #marginal density of x - plot on top
  
  if (class(sc1)==c("pca","dudi") && class(sc_2)==c("pca","dudi")) 
  {plot_top <- ggplot(df, aes(x, y=..scaled..,fill=g)) + 
    geom_density(position="identity",alpha=.5) +
    scale_x_continuous(name = paste("Contribution ",(round((sc_1$eig[1]*100)/sum(sc_1$eig),2)),"%",sep=""), limits=c(min(pp1$data[[1]]$x)-0.5,max(pp1$data[[1]]$x)+0.5))+
    scale_fill_manual(values = c("orange", "purple")) + 
    theme_classic() + theme(legend.position = "none")  #+ ggtitle( "Axis1 = 50.37%")
  }
  
  else {
  
  if(is.null(a1cont)) plot_top <- ggplot(df, aes(x, y=..scaled..,fill=g)) + 
                      geom_density(position="identity",alpha=.5) +
                      scale_x_continuous(name = "axis1", limits=c(min(pp1$data[[1]]$x)-0.5,max(pp1$data[[1]]$x)+0.5))+
                      scale_fill_manual(values = c("orange", "purple")) + 
                      theme_classic() + theme(legend.position = "none")  #+ ggtitle( "Axis1 = 50.37%")
  
  
  
 else plot_top <- ggplot(df, aes(x, y=..scaled..,fill=g)) + 
      geom_density(position="identity",alpha=.5) +
      scale_x_continuous(name = paste("Contribution ",a1cont,"%",sep=""), limits=c(min(pp1$data[[1]]$x)-0.5,max(pp1$data[[1]]$x)+0.5))+
      scale_fill_manual(values = c("orange", "purple")) + 
      theme_classic() + theme(legend.position = "none")  #+ ggtitle( "Axis1 = 50.37%")
    
  }
  #marginal density of y - plot on the right
  
  if (class(sc1)==c("pca","dudi") && class(sc_2)==c("pca","dudi")) 
  {plot_right <- ggplot(df, aes(y, y=..scaled.., fill=g)) + 
    geom_density(position="identity",alpha=.5) + 
    scale_x_continuous(name = paste("Contribution ",(round((sc_1$eig[2]*100)/sum(sc_1$eig),2)),"%",sep=""), limits= c(min(pp1$data[[1]]$y)-0.5,max(pp1$data[[1]]$y)+0.5)) +
    coord_flip() + 
    scale_fill_manual(values = c("orange", "purple")) + 
    theme_classic() + theme(legend.position = "none") 
  }
  
  else {
  
 if(is.null(a2cont)) plot_right <- ggplot(df, aes(y, y=..scaled.., fill=g)) + 
                      geom_density(position="identity",alpha=.5) + 
                      scale_x_continuous(name = "axis2", limits= c(min(pp1$data[[1]]$y)-0.5,max(pp1$data[[1]]$y)+0.5)) +
                      coord_flip() + 
                      scale_fill_manual(values = c("orange", "purple")) + 
                      theme_classic() + theme(legend.position = "none") 
  
  
 else plot_right <- ggplot(df, aes(y, y=..scaled.., fill=g)) + 
    geom_density(position="identity",alpha=.5) + 
    scale_x_continuous(name = paste("Contribution ",a2cont,"%",sep=""), limits= c(min(pp1$data[[1]]$y)-0.5,max(pp1$data[[1]]$y)+0.5)) +
    coord_flip() + 
    scale_fill_manual(values = c("orange", "purple")) + 
    theme_classic() + theme(legend.position = "none") 
  
  }
  
  #arrange the plots together, with appropriate height and width for each row and column
  if (plot.axis == TRUE) grid.arrange(plot_top, empty , grob1, plot_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
  else grid.draw(grob1)
 
}
  

######################################################################################################
######################################################################################################
aa<-data.frame()
for (i in 1:18){
  a<-runif(1,-0.1,0.1)
  b<-runif(1,-0.1,0.1)
  scores_namerica_C$Axis1[i]<-scores_namerica_C$Axis1[i]+a
  scores_namerica_C$Axis2[i]<-scores_namerica_C$Axis2[i]+a
  }

scores_namerica_C$Axis1<-scores_namerica_C$Axis1+aa
names(scores_namerica_C)<-c("Axis1", "Axis2")


scores_europa_C
scores_namerica_C
scores<-rbind(scores_europa_C,scores_namerica_C)
g<-c(rep(0,nrow(scores_europa_C)),rep(1,nrow(scores_namerica_C)))
#g<-c(rep(0,22),rep(1,6))
scores<-data.frame(cbind(scores$Axis1,scores$Axis2,g))
scores
names(scores)<-c("Axis1", "Axis2","sp")
#scores<-as.matrix(scores)
png("hola.png")
#placeholder plot - prints nothing at all
empty <- ggplot()+geom_point(aes(1,1), colour="white") +
  theme(                              
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

#scatterplot of x and y variables


df<-cbind(scores$Axis1,scores$Axis2,g)
df<-data.frame(df)
names(df)<-c("x","y","g")
df$g<-as.factor(df$g)
# plot with red fill
p1 <- ggplot(data = df, aes(x, y,color = as.factor(g))) +
  stat_density2d(aes(fill = ..level..), alpha = 0.3, geom = "polygon") +
  scale_fill_continuous(low = "orange", high = "orange4", space = "Lab", name = "L. acerifolia") +
  scale_colour_discrete(guide = FALSE) + scale_x_continuous(name = "axis1", limits= c(min(df$x)-10, max(df$x)+10)) +
  scale_y_continuous(name = "axis2", limits= c(min(df$y)-10, max(df$y)+10)+
  theme(legend.position="none")
# plot with blue fill
p2 <- ggplot(data = df, aes(x, y, color = as.factor(g))) +
  stat_density2d(aes(fill = ..level..), alpha = 0.3,geom = "polygon") +
  scale_fill_continuous(low = "purple", high = "purple4", space = "Lab", name = "L. maritima") +
  scale_colour_discrete(guide = FALSE) +  scale_x_continuous(name = "axis1", limits= c(-2, 7))+
  scale_y_continuous(name = "axis2", limits= c(min(density(df$y)$x), max(density(df$y)$x)))+
  theme(legend.position="none")
# grab plot data
pp1 <- ggplot_build(p2)
ppp1 <-ggplot_build(p1 + xlim(min(pp1$data[[1]]$x)-0.1,max(pp1$data[[1]]$x)+0.1) + ylim(min(pp1$data[[1]]$y)-0.1,max(pp1$data[[1]]$y)+0.1))
pp2 <- ggplot_build(p2)$data[[1]]
# replace red fill colours in pp1 with blue colours from pp2 when group is 2
pp1$data[[1]]$fill[grep(pattern = "^2", pp2$group)] <- pp2$fill[grep(pattern = "^2", pp2$group)]
# build plot grobs
grob1 <- ggplot_gtable(pp1)
grob2 <- ggplotGrob(p2)
# build legend grobs
#leg1 <- gtable_filter(grob1, "guide-box") 
#leg2 <- gtable_filter(grob2, "guide-box") 
#leg <- gtable:::rbind_gtable(leg1[["grobs"]][[1]],  leg2[["grobs"]][[1]], "first")
# replace legend in 'red' plot
#grob1$grobs[grob1$layout$name == "guide-box"][[1]] <- leg
# plot
#grob1$grobs[grob1$layout$name == "guide-box"][[1]]
grid.newpage()
grid.draw(grob1)

#marginal density of x - plot on top
plot_top <- ggplot(df, aes(x, y=..scaled..,fill=g)) + 
  geom_density(position="identity",alpha=.5) +
  scale_x_continuous(name = "axis1", limits= c(min(density(df$x)$x), max(density(df$x)$x)+1))+
  scale_fill_manual(values = c("orange", "purple")) + 
  theme(legend.position = "none") #+ ggtitle( "Axis1 = 50.37%")

#marginal density of y - plot on the right
plot_right <- ggplot(df, aes(y, y=..scaled.., fill=g)) + 
  geom_density(position="identity",alpha=.5) + 
  scale_x_continuous(name = "axis2", limits= c(min(density(df$y)$x), max(density(df$y)$x)))+
  coord_flip() + 
  scale_fill_manual(values = c("orange", "purple")) + 
  theme(legend.position = "none") 

#arrange the plots together, with appropriate height and width for each row and column
grid.arrange(plot_top, empty , grob1, plot_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))


dev.off()

data(Anguilla_train)
data(Anguilla_test)


dat_pres<-Anguilla_train[Anguilla_train$Angaus==1,]
pres<-dat_pres[,3:11]
n1<-nrow(pres)

dat_abs<-Anguilla_train[Anguilla_train$Angaus==0,]
abs<-dat_abs[,3:11]
n2<-nrow(abs)

dat<-rbind(abs,pres)

pca1 <-dudi.pca(na.omit(dat, center = T, scale = T, scannf = F, nf = 2))
2

###########################################################################

require(ks)
require(spatstat)

# load ks, spatstat
# three-dimensional kernel density of B
B <- pp3(runif(300), runif(300), runif(300), box3(c(0,1)))
x <- unclass(B$data)$df
H <- Hpi(x)
fhat <- kde(x, H=H)
plot(fhat,box=FALSE)
plot(fhat, axes=FALSE, box=FALSE, drawpoints=TRUE); axes3d(c('x','y','z')) 

iris3
fhat <- kde(x=iris[1:50,2:4])
fhat1 <- kde(x=iris[51:100,2:4])
fhat2 <- kde(x=iris[101:150,2:4])

naranja<-colorRampPalette(c("orange", "orange4"))
purpura<-colorRampPalette(c("purple", "purple4"))

plot(fhat, drawpoints=TRUE, colors=naranja(10), box=FALSE, col.pt="orange", xlab="axis1", ylab="axis2", zlab="axis3")
plot(fhat1, drawpoints=TRUE,add=TRUE, colors=purpura(10), box=FALSE, col.pt="purple")


eu<-pca.cal_C$li[1:22,]
na<-pca.cal_C$li[23:27,]
H <- Hpi(eu)
keu<-kde(eu, h=0.5)
kna<-kde(na,  h=0.5)
plot(keu, drawpoints=TRUE, colors=naranja(10), box=FALSE, col.pt="orange", xlab="axis1", ylab="axis2", zlab="axis3")
plot(kna, drawpoints=TRUE,add=TRUE, colors=purpura(10), box=FALSE, col.pt="purple")
     
###########################################
######  Ejemplo Podarcis  #################
###########################################

dir_trabajo="/home/javifl/blog/"
setwd(dir_trabajo)
lista_variables = list.files(path="variables",pattern='*.asc', full.names=TRUE)
variables <- stack(lista_variables)


Ph<-read.csv("Ph.csv")
Pl<-read.csv("Pl.csv")
Pt<-read.csv("Pt.csv")
Ph<-cbind(Ph$x,Ph$y)
Pl<-cbind(Pl$x,Pl$y)
Pt<-cbind(Pt$x,Pt$y)

buffer<-gBuffer(SpatialPoints(rbind(Ph,Pl,Pt)),width = 4)
#lines(buffer)

m<-mask(variables$bio1, buffer)

bg_buffer <- randomPoints(mask=m, n=10000)
#bg_completo <- randomPoints(mask=variables$bio1, n=10000)

#test_bg_buffer<-extract(variables$bio1,bg_buffer)
#test_bg_completo<-extract(variables$bio1,bg_completo)

#plot(density(test_bg_buffer), xlim=c(-75,350), col="blue")
#lines(density(test_bg_completo), col="red")


Ph<-na.omit(extract(variables, Ph))
Pl<-na.omit(extract(variables, Pl))
Pt<-na.omit(extract(variables, Pt))

tabla_bg_buffer<-extract(variables, bg_buffer)
#tabla_bg_completo<-extract(variables, bg_completo)

#data_buffer<-rbind(tabla_europa,tabla_namerica,tabla_bg_buffer)
data_Ph_Pl<-rbind(Ph,Pl,tabla_bg_buffer)
data_Ph_Pt<-rbind(Ph,Pt,tabla_bg_buffer)
data_Pl_Pt<-rbind(Pl,Pt,tabla_bg_buffer)

pca_Ph_Pl <-dudi.pca(na.omit(data_Ph_Pl, center = T, scale = T, scannf = F, nf = 2))
2
pca_Ph_Pt <-dudi.pca(na.omit(data_Ph_Pt, center = T, scale = T, scannf = F, nf = 2))
2
pca_Pl_Pt <-dudi.pca(na.omit(data_Pl_Pt, center = T, scale = T, scannf = F, nf = 2))
2

scores.clim1<- na.omit(data.frame(pca_Ph_Pl$li[nrow(Ph)+nrow(Pl)+1:(nrow(pca_Ph_Pl$li)),]))
scores.clim2<- na.omit(data.frame(pca_Ph_Pt$li[nrow(Ph)+nrow(Pt)+1:(nrow(pca_Ph_Pt$li)),]))
scores.clim3<- na.omit(data.frame(pca_Pl_Pt$li[nrow(Pl)+nrow(Pt)+1:(nrow(pca_Pl_Pt$li)),]))


scores1_Ph<- pca_Ph_Pl$li[1:nrow(Ph),]
scores1_Pl<- pca_Ph_Pl$li[(nrow(Ph)+1):(nrow(Ph)+nrow(Pl)),]

scores2_Ph<- pca_Ph_Pt$li[1:nrow(Ph),]
scores2_Pt<- pca_Ph_Pt$li[(nrow(Ph)+1):(nrow(Ph)+nrow(Pt)),]

scores3_Pl<- pca_Pl_Pt$li[1:nrow(Pl),]
scores3_Pt<- pca_Pl_Pt$li[(nrow(Pl)+1):(nrow(Pl)+nrow(Pt)),]

k1_Ph<-ecospat.grid.clim.dyn (scores.clim1, scores.clim1, scores1_Ph, R=100)
k1_Pl<-ecospat.grid.clim.dyn (scores.clim1, scores.clim1, scores1_Pl, R=100)

k2_Ph<-ecospat.grid.clim.dyn (scores.clim2, scores.clim2, scores2_Ph, R=100)
k2_Pt<-ecospat.grid.clim.dyn (scores.clim2, scores.clim2, scores2_Pt, R=100)

k3_Pl<-ecospat.grid.clim.dyn (scores.clim3, scores.clim3, scores3_Pl, R=100)
k3_Pt<-ecospat.grid.clim.dyn (scores.clim3, scores.clim3, scores3_Pt, R=100)

ecospat.niche.overlap (z1=k1_Ph, z2=k1_Pl, cor=TRUE)
ecospat.niche.overlap (z1=k2_Ph, z2=k2_Pt, cor=TRUE)
ecospat.niche.overlap (z1=k3_Pl, z2=k3_Pt, cor=TRUE)

nrow(Ph)
nrow(Pl)
nrow(Pt)

niceOverPlot(pca_Ph_Pl, n1=1125 , n2=44)
niceOverPlot(pca_Ph_Pt, n1=1125 , n2=119)
niceOverPlot(pca_Pl_Pt, n1=44 , n2=119)

Ph_Pl<-ecospat.plot.niche.dyn(k1_Ph, k1_Pl, title="Niche Categories and Species Density",quant=0.75)
Ph_Pt<-ecospat.plot.niche.dyn(k2_Ph, k2_Pt, title="Niche Categories and Species Density",quant=0.75)
Pl_Pt<-ecospat.plot.niche.dyn(k3_Pl, k3_Pt, title="Niche Categories and Species Density",quant=0.75)

ndOverlap(pca_Ph_Pl, n1=1125 , n2=44)
ndOverlap(pca_Ph_Pt, n1=1125 , n2=119)
ndOverlap(pca_Pl_Pt, n1=44 , n2=119)


H_a<-pca.cal$li[1:1125,]
H_m<-pca.cal$li[1126:1169,]
b1 <- Hpi(H_a)
b2 <- Hpi(H_m)
keu<-kde(H_a, H=b1)
kna<-kde(H_m, H=b2)

plot(keu, drawpoints=FALSE, colors=naranja(10), box=FALSE, col.pt="orange", xlab="axis1", ylab="axis2", zlab="axis3")
plot(kna, drawpoints=FALSE,add=TRUE, colors=purpura(10), box=FALSE, col.pt="purple")



############# OLVERLAP 2D

Ha_k2d<-kde2d(scores_Ha$Axis1,scores_Ha$Axis2, n=100, lims=c(-10, 10, -10, 10))
Hm_k2d<-kde2d(scores_Hm$Axis1,scores_Hm$Axis2, n=100, lims=c(-10, 10, -10, 10))
p1<-as.matrix(Ha_k2d$z)/sum(as.matrix(Ha_k2d$z))
p2<-as.matrix(Hm_k2d$z)/sum(as.matrix(Hm_k2d$z))
D <- 1 - (0.5 * (sum(abs(p1 - p2))))
H <- sqrt(sum((sqrt(p1) - sqrt(p2))^2))
I <- 1 - (H^2)/2

rHa<-raster(Ha_k2d)
rHm<-raster(Hm_k2d)

############# OLVERLAP 1D

Ha_k1d<-density(scores_Ha, from=-10, to=20, cut=1)
Hm_k1d<-density(scores_Hm, from=-10, to=20, cut=0)
p1<-as.matrix(Ha_k1d$y)/sum(as.matrix(Ha_k1d$y))
p2<-as.matrix(Hm_k1d$y)/sum(as.matrix(Hm_k1d$y))
D <- 1 - (0.5 * (sum(abs(p1 - p2))))
H <- sqrt(sum((sqrt(p1) - sqrt(p2))^2))
I <- 1 - (H^2)/2


####################################
######    #################
####################################

grid.clim_1D <-function (glob, glob1, sp, R, th.sp = 0, th.env = 0, geomask = NULL) {
  
  glob <- as.matrix(glob)
  glob1 <- as.matrix(glob1)
  sp <- as.matrix(sp)
  l <- list()
  
  xmax <- max(glob[, 1])
  xmin <- min(glob[, 1])
  x <- seq(from = min(glob[, 1]), to = max(glob[, 1]), 
           length.out = R)
  sp.dens <- density(sp[, 1], kernel = "gaussian", from = xmin, 
                     to = xmax, n = R, cut = 0)
  glob1.dens <- density(glob1[, 1], kernel = "gaussian", 
                        from = xmin, to = xmax, n = R, cut = 0)
  z <- sp.dens$y * nrow(sp)/sum(sp.dens$y)
  Z <- glob1.dens$y * nrow(glob)/sum(glob1.dens$y)
  glob1r <- sapply(glob1, findInterval, glob1.dens$x)
  th.env <- quantile(glob1.dens$y[glob1r], th.env)
  glob1rm <- which(Z < th.env)
  spr <- sapply(sp, findInterval, sp.dens$x)
  th.sp <- quantile(sp.dens$y[spr], th.sp)
  sprm <- which(z < th.sp)
  z[sprm] <- 0
  Z[glob1rm] <- 0
  z.uncor <- z/max(z)
  z.cor <- z/Z
  z.cor[is.na(z.cor)] <- 0
  z.cor[z.cor == "Inf"] <- 0
  z.cor <- z.cor/max(z.cor)
  w <- z.uncor
  w[w > 0] <- 1
  l$x <- x
  l$z <- z
  l$z.uncor <- z.uncor
  l$z.cor <- z.cor
  l$Z <- Z
  l$glob <- glob
  l$glob1 <- glob1
  l$sp <- sp
  l$w <- w
  
  return(l)
}

Ha1<-grid.clim_1D (scores.clim1, scores.clim1, scores_Ha , R=100)
Hm1<-grid.clim_1D (scores.clim1, scores.clim1, scores_Hm , R=100)

p11<-as.matrix(Ha1$z.uncor)/sum(as.matrix(Ha1$z.uncor))
p12<-as.matrix(Hm1$z.uncor)/sum(as.matrix(Hm1$z.uncor))
D1 <- 1 - (0.5 * (sum(abs(p11 - p12))))

Ha2<-grid.clim_1D (scores.clim1, scores.clim1, scores_Ha$Axis2 , R=100)
Hm2<-grid.clim_1D (scores.clim1, scores.clim1, scores_Hm$Axis2 , R=100)

p21<-as.matrix(Ha1D$z.uncor)/sum(as.matrix(Ha1D$z.uncor))
p22<-as.matrix(Hm1D$z.uncor)/sum(as.matrix(Hm1D$z.uncor))
D2 <- 1 - (0.5 * (sum(abs(p1 - p2))))

c1<-pca.cal$eig[1]/sum(pca.cal$eig)
c2<-pca.cal$eig[2]/sum(pca.cal$eig)
o1<-D1*c1
o2<-D2*c2
oP<-o1+o2
ct<-c1+c2


sp1<-data.frame(rnorm(50,5,sd=1), rnorm(50,30,sd=3))
sp2<-data.frame(rnorm(50,5,sd=1), rnorm(50,50,sd=3))
sp3<-data.frame(rnorm(50,10,sd=1), rnorm(50,70,sd=3))
sp4<-data.frame(rnorm(50,5,sd=1), rnorm(50,30,sd=3))
names(sp1)<-c("Axis1","Axis2")
names(sp2)<-c("Axis1","Axis2")
names(sp3)<-c("Axis1","Axis2")
names(sp4)<-c("Axis1","Axis2")

scores.clim<-rbind(sp1,sp2,sp3,sp4)
sp1_grid<-ecospat.grid.clim.dyn (scores.clim, scores.clim, sp1, R=100)
sp2_grid<-ecospat.grid.clim.dyn (scores.clim, scores.clim, sp2, R=100)
sp3_grid<-ecospat.grid.clim.dyn (scores.clim, scores.clim, sp3, R=100)
sp4_grid<-ecospat.grid.clim.dyn (scores.clim, scores.clim, sp4, R=100)

ecospat.plot.niche.dyn(sp1_grid, sp2_grid, title="Niche Categories and Species Density",quant=0.75)#,
ecospat.plot.niche.dyn(sp1_grid, sp3_grid, title="Niche Categories and Species Density",quant=0.75)#,
ecospat.plot.niche.dyn(sp1_grid, sp4_grid, title="Niche Categories and Species Density",quant=0.75)#,


ecospat.niche.overlap (z1=sp1_grid, z2=sp2_grid, cor=TRUE)
ecospat.niche.overlap (z1=sp1_grid, z2=sp3_grid, cor=TRUE)
ecospat.niche.overlap (z1=sp1_grid, z2=sp4_grid, cor=TRUE)

sp11<-grid.clim_1D (scores.clim, scores.clim, sp1$Axis1 , R=100)
sp21<-grid.clim_1D (scores.clim, scores.clim, sp2$Axis1 , R=100)
sp31<-grid.clim_1D (scores.clim, scores.clim, sp3$Axis1 , R=100)
sp41<-grid.clim_1D (scores.clim, scores.clim, sp4$Axis1 , R=100)
sp12<-grid.clim_1D (scores.clim$Axis2, scores.clim, sp1$Axis2 , R=100)
sp22<-grid.clim_1D (scores.clim$Axis2, scores.clim, sp2$Axis2 , R=100)
sp32<-grid.clim_1D (scores.clim$Axis2, scores.clim, sp3$Axis2 , R=100)
sp42<-grid.clim_1D (scores.clim$Axis2, scores.clim, sp4$Axis2 , R=100)

p11<-as.matrix(sp11$z.uncor)/sum(as.matrix(sp11$z.uncor))
p21<-as.matrix(sp21$z.uncor)/sum(as.matrix(sp21$z.uncor))
p31<-as.matrix(sp31$z.uncor)/sum(as.matrix(sp31$z.uncor))
p41<-as.matrix(sp41$z.uncor)/sum(as.matrix(sp41$z.uncor))
D1 <- 1 - (0.5 * (sum(abs(p11 - p41))))
p12<-as.matrix(sp12$z.uncor)/sum(as.matrix(sp12$z.uncor))
p22<-as.matrix(sp22$z.uncor)/sum(as.matrix(sp22$z.uncor))
p32<-as.matrix(sp32$z.uncor)/sum(as.matrix(sp32$z.uncor))
p42<-as.matrix(sp42$z.uncor)/sum(as.matrix(sp42$z.uncor))
D2 <- 1 - (0.5 * (sum(abs(p12 - p42))))

#c1<-pca.cal$eig[1]/sum(pca.cal$eig)
#c2<-pca.cal$eig[2]/sum(pca.cal$eig)
c1=0.47
c2=0.27
o1<-D1*c1
o2<-D2*c2
oP<-o1+o2
ct<-c1+c2
Overlap<-oP/ct
Overlap

####################################
######  ndOverlap  #################
####################################


ndOverlap<-function(pca, n1, n2) {
  resultado<-list()
  scores.clim<- na.omit(data.frame(pca$li[(n1+n2)+1:(nrow(pca$li)),]))
  sp1<- pca$li[1:n1,]
  sp2<- pca$li[(n1+1):(n1+n2),]
  sp1_a1<-grid.clim_1D (scores.clim, scores.clim, sp1$Axis1 , R=100)
  sp2_a1<-grid.clim_1D (scores.clim, scores.clim, sp2$Axis1 , R=100)
  sp1_a2<-grid.clim_1D (scores.clim, scores.clim, sp1$Axis2 , R=100)
  sp2_a2<-grid.clim_1D (scores.clim, scores.clim, sp2$Axis2 , R=100)
  
  sp1_a1$glob<-sp1_a1$glob[,1]
  sp2_a1$glob<-sp2_a1$glob[,1]
  sp1_a2$glob<-sp1_a2$glob[,1]
  sp2_a2$glob<-sp2_a2$glob[,1]
  
  resultado[[1]]<-sp1_a1
  resultado[[2]]<-sp2_a1
  resultado[[3]]<-sp1_a2
  resultado[[4]]<-sp2_a2
  
  p1_a1<-as.matrix(sp1_a1$z.cor)/sum(as.matrix(sp1_a1$z.cor))
  p2_a1<-as.matrix(sp2_a1$z.cor)/sum(as.matrix(sp2_a1$z.cor))
  D1 <- 1 - (0.5 * (sum(abs(p1_a1 - p2_a1))))
  
  p1_a2<-as.matrix(sp1_a2$z.cor)/sum(as.matrix(sp1_a2$z.cor))
  p2_a2<-as.matrix(sp2_a2$z.cor)/sum(as.matrix(sp2_a2$z.cor))
  D2 <- 1 - (0.5 * (sum(abs(p1_a2 - p2_a2))))
  
  c1<-pca$eig[1]/sum(pca$eig)
  c2<-pca$eig[2]/sum(pca$eig)
  o1<-D1*c1
  o2<-D2*c2
  oP<-o1+o2
  ct<-c1+c2
  Overlap<-oP/ct
  result<-data.frame(1:3, 1:3)
  row.names(result)<-c(paste("D-index for Axis-1 (",(round(c1, 2)*100),"% of total contribution)",sep=""),
                       paste("D-index for Axis-2 (",(round(c2, 2)*100),"% of total contribution)",sep=""),
                       paste("Sum of weighted D-index. (Maximum value: ",(round(ct, 2))," over 1)",sep=""))
  names(result)<-c("value", "weighted")
  result[1,1] <- round(D1, 4)
  result[1,2] <- round(o1,4)
  result[2,1] <- round(D2,4)
  result[2,2] <- round(o2,4)
  result[3,1] <- ""
  result[3,2] <- round(oP,4)
  print(result)
  return(resultado)
}

####################################
######  grid.clim_1D  ##############
####################################

grid.clim_1D <-function (glob, glob1, sp, R, th.sp = 0, th.env = 0, geomask = NULL) {
  
  glob <- as.matrix(glob)
  glob1 <- as.matrix(glob1)
  sp <- as.matrix(sp)
  l <- list()
  
  xmax <- max(glob[, 1])
  xmin <- min(glob[, 1])
  x <- seq(from = min(glob[, 1]), to = max(glob[, 1]), 
           length.out = R)
  sp.dens <- density(sp[, 1], kernel = "gaussian", from = xmin, 
                     to = xmax, n = R, cut = 0)
  glob1.dens <- density(glob1[, 1], kernel = "gaussian", 
                        from = xmin, to = xmax, n = R, cut = 0)
  z <- sp.dens$y * nrow(sp)/sum(sp.dens$y)
  Z <- glob1.dens$y * nrow(glob)/sum(glob1.dens$y)
  glob1r <- sapply(glob1, findInterval, glob1.dens$x)
  th.env <- quantile(glob1.dens$y[glob1r], th.env)
  glob1rm <- which(Z < th.env)
  spr <- sapply(sp, findInterval, sp.dens$x)
  th.sp <- quantile(sp.dens$y[spr], th.sp)
  #sprm <- which(z < th.sp)
  #z[sprm] <- 0
  #Z[glob1rm] <- 0
  z.uncor <- z/max(z)
  z.cor <- z/Z
  z.cor[is.na(z.cor)] <- 0
  z.cor[z.cor == "Inf"] <- 0
  z.cor <- z.cor/max(z.cor)
  w <- z.uncor
  w[w > 0] <- 1
  l$x <- x
  l$z <- z
  l$z.uncor <- z.uncor
  l$z.cor <- z.cor
  l$Z <- Z
  l$glob <- glob
  l$glob1 <- glob1
  l$sp <- sp
  l$w <- w
  
  return(l)
}



####################################
######  3D kernel     ##############
####################################

library("ks")
library("ggplot2")
library(grid)
library(gridExtra)

sp1<-data.frame(rnorm(50,5,sd=2), rnorm(50,30,sd=3), rnorm(50,10,sd=2))
sp2<-data.frame(rnorm(50,5,sd=2), rnorm(50,30,sd=3), rnorm(50,5,sd=2))
#sp3<-data.frame(rnorm(50,10,sd=1), rnorm(50,70,sd=3))


names(sp1)<-c("temperature","precipitation","seasonality")
names(sp2)<-c("temperature","precipitation","seasonality")
#names(sp3)<-c("Axis1","Axis2")

sp1_3D<-kde(x=sp1)
sp2_3D<-kde(x=sp2)

naranja<-colorRampPalette(c("orange", "orange4"))
purpura<-colorRampPalette(c("purple", "purple4"))

plot(sp1_3D, drawpoints=TRUE, colors=naranja(10), box=FALSE, col.pt="orange", xlab="temperature", ylab="precipitation", zlab="seasonality")
plot(sp2_3D, drawpoints=TRUE,add=TRUE, colors=purpura(10), box=FALSE, col.pt="purple")

niceOverPlot(sc1=sp1[,2:3] ,sc2=sp2[,2:3], plot.axis = TRUE)


dir_trabajo="/home/javifl/blog/ndOverlap"
setwd(dir_trabajo)
for (i in 1:90) {
  view3d(userMatrix=rotationMatrix(2*pi * i/90, 1, -1, -1))
  rgl.snapshot(filename=paste("frame-",
                              sprintf("%03d", i), ".png", sep=""))
}


Ph3D<-as.data.frame(Ph)
Ph3D<-as.data.frame(cbind(Ph3D$bio5,Ph3D$bio18))
Ph3D_3<-as.data.frame(cbind(Ph3D$bio5,Ph3D$bio18, Ph3D$bio4))
names(Ph3D)<-c("Axis1", "Axis2")
names(Ph3D_3)<-c("bio5", "bio18", "bio4")
Pt3D<-as.data.frame(Pt)
Pt3D<-as.data.frame(cbind(Pt3D$bio5,Pt3D$bio18))
Pt3D_3<-as.data.frame(cbind(Pt3D$bio5,Pt3D$bio18, Pt3D$bio4))
names(Pt3D)<-c("Axis1", "Axis2")
names(Pt3D_3)<-c("bio5", "bio18", "bio4")

niceOverPlot(sc1=Ph3D ,sc2=Pt3D, plot.axis = TRUE)

sp1_3D<-kde(x=Ph3D_3)
sp2_3D<-kde(x=Pt3D_3)


plot(sp1_3D, drawpoints=FALSE, colors=naranja(10), box=FALSE, xlim=c(250,350), ylim=c(0,400), col.pt="orange", xlab="temperature", ylab="precipitation", zlab="seasonality")
plot(sp2_3D, drawpoints=FALSE,add=TRUE, colors=purpura(10), xlim=c(-2,2), ylim=c(0,150), box=FALSE, col.pt="purple")
