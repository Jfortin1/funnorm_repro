i=as.numeric(commandArgs(TRUE)[1])

funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
normDir   <- paste0(funnormDir,"/norm_datasets")
scriptDir <- paste0(funnormDir,"/scripts")
dmpsDir   <- paste0(funnormDir,"/dmps")
rocDir    <- paste0(funnormDir,"/roc_data_filtered")
badDir    <- paste0(funnormDir,"/bad_probes")



dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")

dis.index <- i
val.index <- i+3
data.file.dis <- paste0("dmps_",dataset_names[dis.index],".Rda")
data.file.val <- paste0("dmps_",dataset_names[val.index],".Rda")


setwd(dmpsDir)
load(data.file.dis)
dis <- dmps
load(data.file.val)
val <- dmps

# Filtering bad probes:
load(file.path(badDir, "bad.probes.rda"))

for (i in 1:length(dis)){
	dis[[i]] <- dis[[i]][!((rownames(dis[[i]]) %in% bad.probes)),]
	val[[i]] <- val[[i]][!((rownames(val[[i]]) %in% bad.probes)),]
	print(i)
}

setwd(scriptDir)
source("generateOverlapData.R")

setwd(rocDir)
dataset_names <- c("ontario_ebv","ontario_blood","kirc")


### To generate ROC data for 100K for all methods           
overlapData1000 <- generateOverlapData(dis,val,1000)
save(overlapData1000, file=paste0("overlapData_1000_",dataset_names[i],".Rda"))

overlapData100 <- generateOverlapData(dis,val,100)
save(overlapData100, file=paste0("overlapData_100_",dataset_names[i],".Rda"))




