################# Libraries
library(apsimr)
#################### Setting up the eniviroment for apsim r
apsimExe <-"C:/Program Files (x86)/Apsim77-r3615/Model/Apsim.exe"
apsimWd <- "~/APSIM"
apsimmod<-"C:/Program Files (x86)/Apsim77-r3615/Model/"
apsimFile <- "Centro.apsim"
obs<-c(900,1200) ### Observed yield
#########################################objective function
#  
## This function just take out the yield from the output
meanCowpea<-function(X){
  return((X$yield))
}

######## Objective function
objfun <- function(prms, odata,apsimVar){
  
  ## I need this to run the apsim for every try
  apsimValue<-matrix(sapply(prms,function(x){rep(x,length(apsimVar))}),nrow=length(apsimVar),byrow=T)
  uniRes <- apsim_vector(X = apsimValue, exe = apsimExe, wd = apsimWd, vars = apsimVar,
                         to.run = apsimFile, to.edit = apsimFile, g = meanCowpea)
  ### Arbitary index
  rss<-sum(((uniRes)-odata)^2)
  rss
}


ap.var<- c("SoilWater/SummerCona", "SoilWater/SummerU")
### just to check and see how the function works
objfun(c(2,3),odata = obs,ap.var) 

### real optim function
op <- optim(c(2,3), objfun, odata = obs,apsimVar=ap.var,control = list(trace = 1))
