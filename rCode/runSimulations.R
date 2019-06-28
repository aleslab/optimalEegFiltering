library(geigen)

wd<-getwd()

fname<-file.path(wd,"Data","signal_v1.R")
source(fname)

fname<-file.path(wd,"Data","Noise_v2.R")
source(fname)

fname<-file.path(wd,"Data","simulation_v1.R")
source(fname)

fname<-file.path(wd,"Data","simulation_v2.R")
source(fname)

fname<-file.path(wd,"R Functions","GSVD_filter.R")
source(fname)

fname<-file.path(wd,"R Functions","MSE.R")
source(fname)

fname<-file.path(wd,"Data","average_v_filter.R")
source(fname)

