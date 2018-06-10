#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#
# FileName:
# FileDescription:
# The following times series are generated using /timeseries/periodicTimeSeries/periodic03.R
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#
#
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Miguel P. Xochicale [http://mxochicale.github.io]
# Doctoral Researcher in Human-Robot Interaction
# University of Birmingham, U.K. (2014-2018)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#################
# Start the clock!
start.time <- Sys.time()


#################
### Load libraries

library(deSolve)
library(data.table)
library(ggplot2)


#library(nonlinearTseries)

library(devtools)
load_all('~/mxochicale/github/nonlinearTseries')




################################################################################
# (1) Defining paths for main_path, r_scripts_path, ..., etc.
r_scripts_path <- getwd()
plots_path <- paste(r_scripts_path,"/plots_path",sep="")


###################
## Main R Script




######################
### Lorenz Function
###
Lorenz <- function(t, state, parameters){
          with(as.list( c(state,parameters)),
              {
              #rate of change
              dX <- sigma*(Y-X)
              dY <- rho*X - X*Z - Y
              dZ <- X*Y - beta*Z

              # return the rate of change
              list( c(dX, dY, dZ) )
              }
          )# end with(as.list...
}

########################
### define controlling parameters
# rho     - Scaled Rayleigh number.
# sigma   - Prandtl number.
# beta   - Geometry ascpet ratio.
parameters <- c(rho=28, sigma= 10, beta=8/3)

########################
### define initial state
# state <- c(X=1, Y=1, Z=1)
state <- c(X=20, Y=41, Z=20)

########################
### define integrations times
# times <- seq(0,100, by=0.001)
times <- seq(0,100, by=0.01)

########################
### perform the integration and assign it to variable 'out'
out <- ode(y=state, times= times, func=Lorenz, parms=parameters)

########################
### Lorenz State Space lss DATA TABLE
lss <- as.data.table(out)

fsNNtmp <-function(x) {list("Lorenz")}
lss[,c("type"):=fsNNtmp(), ]
lss[,sample:=seq(.N)]
setcolorder(lss, c(5,6,1:4))

inputtimseries <- lss$X

################################
### (4.2) Windowing Data

windowstar <- 2000
windowend <- 4000
windowLenght <- windowend - windowstar


windowframe = windowstar:windowend
# windowframe = 2000:3000
# windowframe = 00:10000
lss <- lss[,.SD[windowframe]]





#
################################################################################
## nonlinearTseries method

input_timeseries <- lss$X

maxtau <- 100
numberOFpartitions  <- 1000


# tau-delay estimation based on the mutual information function
tau.ami <- timeLag(time.series = inputtimseries, technique = "ami",
                 selection.method = "first.minimum", lag.max = maxtau,
                 do.plot = F, n.partitions = numberOFpartitions,
                 units = "Bits")

tauamilag <- tau.ami[[1]]
tauamifx <- tau.ami[[2]]

ami <- as.data.table(tauamifx)


fsNNtmp <-function(x) {list("nonlinearTseries")}
ami[,c("source"):=fsNNtmp(), ]
ami[, tau := 0:(.N-1), ]
setcolorder(ami, c(3,2,1))
colnames(ami) <- c('tau', 'source', 'mi')


#MI <- rbind(mi,ami,ctsmi)



####################
### Save Picture

## Setting up plots_path
if (file.exists(plots_path)){
   setwd(file.path(plots_path))
} else {
 dir.create(plots_path, recursive=TRUE)
 setwd(file.path(plots_path))
}


pmi <- ggplot(ami, aes(x=tau, y=mi, colour=source, group=source))+
  geom_line(
      size=3)+
  geom_point(aes(shape=source),   # Shape depends on Source
           fill = "white",    # White fill
           size = 2)  +       # Large points
  scale_shape_manual(values=c(21,24,25))+  # Shapes: Filled circle, triangle
  labs(x='TAU', y='AMI' )+
  theme_bw(20)+
  theme(legend.position = c(0.8, 0.8))


width = 500
height = 500
text.factor = 1
dpi <- text.factor * 100
width.calc <- width / dpi
height.calc <- height / dpi

filenameimage <- paste("example_nonlinearTseries",".png",sep="")

ggsave(filename = filenameimage,
             dpi = dpi,
             width = width.calc,
             height = height.calc,
             units = 'in',
             bg = "transparent",
             device = "png"
             , pmi)






################################################################################
################################################################################
# Stop the clock!
end.time <- Sys.time()
end.time - start.time
################################################################################
################################################################################

################################################################################
setwd(r_scripts_path) ## go back to the r-script source path
