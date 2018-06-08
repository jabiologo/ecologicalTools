# First, we will create a raster as a random environmental variable, for example
# tree cover.


library (raster)
library (dismo)

tc <- raster(nrows=100, ncols=100, xmn=0, xmx=100, ymn=0,ymx=100)
tc[] <- runif(10000, -80, 180)
tc <- focal(tc, w=matrix(1, 5, 5), mean)
tc <- focal(tc, w=matrix(1, 5, 5), mean)
plot(tc)

# Second, we should define the species. The species is defined by their position
# (coordinates), and by their optimum for any environmental variable

species <- setClass("species", slots=c(x="numeric", y="numeric", opt="numeric"))

# Here we will define the Red deer species as a specimen in the coordinates
# 50,50 and an optimun of 80 for the environmental variable (tree cover, for 
# example). In the same way, we will define the Egyptian mongoose as a specimen
# in the coordinates 50,50 and an optimun of 30 for the environmental variable
# (tree cover)

Red_deer <- species(x= 50, y =50, opt= 90)
Egyptian_mongoose <- species(x= 50, y =50, opt= 30)

# "go" function require a species (sp), a raster layer with any environmental
# variable (env), number of iterations (n), a Brownian motion parameter (that
# is, how random is the movement of your species), a geographical optimum (the
# wanted destination of your species theta_x and theta_y), and the interest of
# the species to get this position (alpha_x and alpha_y). 

go <- function (sp, env, n, sigma, theta_x, alpha_x, theta_y, alpha_y) {
  track <- data.frame() #create an empty data.frame to store each step coordinates
  track[1,1] <- sp@x    #the first position will be the initial position of the specimen defined by the species class
  track[1,2] <- sp@y    #the first position will be the initial position of the specimen defined by the species class
  
  # First, the function searches in the adjacent cells what have the value that best fits with the species requirements
  
  
  for (step in 2:n) {
    neig <- adjacent(env, 
                     cellFromXY(env, matrix(c(track[step-1,1],
                                              track[step-1,2]), 1,2)), 
                     directions=8, pairs=FALSE )
    options <- data.frame()
    for (i in 1:length(neig)){
      options[i,1]<-neig[i]
      options[i,2]<- sp@opt - env[neig[i]]
    }
    option <- c(options[abs(na.omit(options$V2)) == min(abs(na.omit(options$V2))), 1 ], 
                options[abs(na.omit(options$V2)) == min(abs(na.omit(options$V2))), 1 ])
    new_cell <- sample(option,1)
    new_coords <- xyFromCell(env,new_cell)
    lon_candidate<--9999
    lat_candidate<--9999
    
    while ( is.na(extract(env, matrix(c(lon_candidate,lat_candidate),1,2)))) {
      lon_candidate <- new_coords[1]+ (sigma * rnorm(1)) + (alpha_x * ( theta_x - new_coords[1]))
      lat_candidate <- new_coords[2]+ (sigma * rnorm(1)) + (alpha_y * ( theta_y - new_coords[2]))
    }
    track[step,1] <- lon_candidate
    track[step,2] <- lat_candidate
  }
  return(track)
}

# Experiment 1: In the first experiment we simulate random movement of these two
# species having into account their environmental optimuns

deer_simul <- go (Red_deer, tc, 100, 2, 90, 0, 90, 0)
mongoose_simul <- go (Egyptian_mongoose, tc, 100, 2, 90, 0, 90, 0)

# We can plot their paths...

plot(tc)
lines(deer_simul, lwd=1.5, col="red")
points(deer_simul, cex=0.3, col="red")
lines(mongoose_simul, lwd=1.5, col="blue")
points(mongoose_simul, cex=0.3, col="blue")
legend("topleft", legend=c("deer","mongoose"), col=c("red","blue"),
       lty=c(1,1), lwd=c(2,2))

# To test if each species is "searching" their environmental optimum, we can
# extract the environental values by step for each species and plot their
# density distributions.

plot(density(extract(tc, deer_simul)),lwd=3, col="red", xlim=c(20,80), 
     ylim=c(0,max(c(density(extract(tc, deer_simul))$y,
                    density(extract(tc, mongoose_simul))$y))),
     main="locations density distribution", xlab="tree cover")
lines(density(extract(tc, mongoose_simul)),lwd=3, col="blue")
legend("topleft", legend=c("deer","mongoose"), col=c("red","blue"),
       lty=c(1,1), lwd=c(3,3))

# We can see that the deer uses patches with a higer value of tree cover than
# the mongoose



## GIF PATH
setwd("./blog/move/gif")

for (i in 1:nrow(deer_simul)) {
  id <- sprintf("%03d", i)
  png(paste("path_",id,".png", sep=""), width=800, height=831, units="px",   
      pointsize=18)  
  plot(tc)
  legend("topleft", legend=c("deer","mongoose"), col=c("red","blue"),
         lty=c(1,1), lwd=c(2,2))
  lines(deer_simul[1:i,1:2], col="red", lwd=2.5)
  points(deer_simul[1:i,1:2], cex=0.5, col="red")
  lines(mongoose_simul[1:i,1:2], col="blue", lwd=2.5)
  points(mongoose_simul[1:i,1:2], cex=0.5, col="blue")
  
  dev.off()
}


# In this second

sp1 <- species(x= 50, y =50, opt= 80)

path1 <- go (sp1, tc, 100, 1, 0, 0, 0, 0)
path2 <- go (sp1, tc, 100, 2, 0, 0, 0, 0)
path3 <- go (sp1, tc, 100, 3, 0, 0, 0, 0)

plot(tc)
points(path1)
points(path2, col="red")
points(path3, col="blue")

p1<-SpatialPoints(path1)
p2<-SpatialPoints(path2)
p3<-SpatialPoints(path3)

lines(mcp(p1, percent=95), lwd=2.5, lty=2)
lines(mcp(p2, percent=95), col="red", lwd=2.5, lty=2)
lines(mcp(p3, percent=95), col="blue", lwd=2.5, lty=2)

legend("topleft", legend=c(expression(paste(sigma, " = ", 1)),
                           expression(paste(sigma, " = ", 2)),
                           expression(paste(sigma, " = ", 3))), col=c("black","red","blue"),
       lty=c(1,1,1), lwd=c(2,2,2), title= "Brownian motion rate")


plot(density(extract(tc, path1)),lwd=3, col="black", xlim=c(20,90), 
     ylim=c(0,max(c(density(extract(tc, path1))$y,
                    density(extract(tc, path2))$y,
                    density(extract(tc, path3))$y))),
     main="locations density distribution", xlab="tree cover")
lines(density(extract(tc, path2)),lwd=3, col="red")
lines(density(extract(tc, path3)),lwd=3, col="blue")
legend("topleft", legend=c(expression(paste(sigma, " = ", 1)),
                           expression(paste(sigma, " = ", 2)),
                           expression(paste(sigma, " = ", 3))), col=c("black","red","blue"),
       lty=c(1,1,1), lwd=c(2,2,2), title= "Brownian motion rate")


sp1 <- species(x= 10, y =10, opt= 80)

path1 <- go (sp1, tc, 100, 1, 90, 0.05, 90, 0.05)
path2 <- go (sp1, tc, 100, 1, 90, 0.1, 90, 0.1)
path3 <- go (sp1, tc, 100, 1, 90, 0.5, 90, 0.5)

par(mfrow=c(2,1)) 

plot(density(extract(tc,path1)), xlim=c(minValue(tc), maxValue(tc)), ylim = c(0,0.1))
lines(density(extract(tc,path2)), col = "red")
lines(density(extract(tc,path3)), col = "blue")

plot(tc)
lines(path1)
lines(path2, col="red")
lines(path3, col="blue")

