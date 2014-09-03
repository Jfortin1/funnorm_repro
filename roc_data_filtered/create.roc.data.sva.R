for (k in 1:3){

funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
normDir   <- paste0(funnormDir,"/norm_datasets")
scriptDir <- paste0(funnormDir,"/scripts")
dmpsDir   <- paste0(funnormDir,"/dmps")
rocDir    <- paste0(funnormDir,"/roc_data_filtered")
svaDir    <- paste0(funnormDir,"/sva_results")
badDir    <- paste0(funnormDir,"/bad_probes")


dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")

dis.index <- k
val.index <- k+3
data.file.dis <- paste0("sva_dmps_",dataset_names[dis.index],".Rda")
data.file.val <- paste0("sva_dmps_",dataset_names[val.index],".Rda")


setwd(svaDir)
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
source("generateROCData.R")

setwd(rocDir)
dataset_names <- c("ontario_ebv","ontario_blood","kirc")

k_vector <- c(100000,100,100000)

### To generate ROC data for 100K for all methods           
rocData <- generateROCData(dis,val,k_vector[k])
save(rocData, file=paste0("sva_rocData_",k_vector[k]/1000,"K_",dataset_names[k],".Rda"))
}






