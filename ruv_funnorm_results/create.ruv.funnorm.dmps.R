# This script will create a RGSet for the discovery cohort and a RGSet for the validation cohort

funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
normDir   <- paste0(funnormDir,"/norm_datasets")
scriptDir <- paste0(funnormDir,"/scripts")
svaDir    <- paste0(funnormDir,"/sva_results")
ruvFunnormDir    <- paste0(funnormDir,"/ruv_funnorm_results")

dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")



k_vector <- c(25,3,0,6,1,3,0,18)
setwd(ruvFunnormDir)
for (i in 1:8){
	k <- k_vector[i]
	data.file <- paste0("ruv_funnorm_results_",dataset_names[i],"_k_",k,".Rda")
	load(data.file)
	object <- ruv.results
	p <- t(object$p)
	dmps <- cbind(t(object$t),p)
	dmps <- as.data.frame(dmps)
	colnames(dmps) <- c("f","p.val")
	dmps <- dmps[order(dmps$p.val),]
	dmps <- list(ruv=dmps)
	save(dmps, file=paste0("ruv_funnorm_dmps_",dataset_names[i],".Rda"))
	print(i)
}

