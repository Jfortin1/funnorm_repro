i=as.numeric(commandArgs(TRUE)[1]) # Dataset
funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
normDir   <- paste0(funnormDir,"/norm_datasets")
scriptDir <- paste0(funnormDir,"/scripts")
ruvFunnormDirTuning    <- paste0(funnormDir,"/ruv_funnorm_results/sex_tuning")
ruvFunnormDirTuningPlotData <- paste0(funnormDir, "/ruv_funnorm_results/sex_tuning_plot_data")

dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")

data.file <- paste0("rgset_",dataset_names[i],".Rda")

setwd(ruvFunnormDirTuning)

p.matrix <- NULL
for (k in 0:40){
	load(paste0("ruv_funnorm_results_sex_",dataset_names[i],"_k_",k,".Rda"))
	p.matrix <- cbind(p.matrix,t(ruv.results$p))
	print(k)
}

bad <- grepl("Control",rownames(p.matrix))
p.matrix <- p.matrix[!bad,]

setwd(ruvFunnormDirTuningPlotData)

save(p.matrix, file=paste0("p.matrix_",dataset_names[i],".Rda" ))

