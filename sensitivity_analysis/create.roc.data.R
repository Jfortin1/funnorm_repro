k=as.numeric(commandArgs(TRUE)[1])



for (i in 1:3){

	funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
	rawDir <- paste0(funnormDir,"/raw_datasets")
	disValDir <- paste0(funnormDir,"/dis_val_datasets")
	designDir <- paste0(funnormDir,"/designs")
	normDir   <- paste0(funnormDir,"/norm_datasets")
	scriptDir <- paste0(funnormDir,"/scripts")
	dmpsDir   <- paste0(funnormDir,"/dmps")
	rocDir    <- paste0(funnormDir,"/sensitivity_analysis/roc_data")


}
funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
normDir   <- paste0(funnormDir,"/norm_datasets")
scriptDir <- paste0(funnormDir,"/scripts")
dmpsDir   <- paste0(funnormDir,"/dmps")
rocDir    <- paste0(funnormDir,"/roc_data")

dis.files <- paste0("dmps_dis",1:5,".Rda")
dis.files <- rep(dis.files,5)

val.files <- paste0("dmps_val",1:5,".Rda")
val.files <- rep(val.files, each=5)


sensitivityDir <- paste0(funnormDir,"/sensitivity_analysis")


setwd(sensitivityDir)
load(dis.files[i])
dis <- dmps
load(val.files[i])
val <- dmps

setwd(scriptDir)
source("generateROCData.R")

setwd(sensitivityDir)


vector1 <- rep(1:5,5)
vector2 <- rep(1:5, each=5)

### To generate ROC data for 100K for all methods           
rocData <- generateROCData(dis,val,100000)
save(rocData, file=paste0("rocData_",vector1[i],"_",vector2[i],".Rda"))




