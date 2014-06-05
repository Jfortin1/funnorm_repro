i=as.numeric(commandArgs(TRUE)[1]) # Dataset
funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
normDir   <- paste0(funnormDir,"/norm_datasets")
scriptDir <- paste0(funnormDir,"/scripts")
ruvDir    <- paste0(funnormDir,"/ruv_results")
ruvDirTuning    <- paste0(funnormDir,"/ruv_results/sex_tuning")
ruvDirTuningPlotData <- paste0(funnormDir, "/ruv_results/sex_tuning_plot_data")


dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")

data.file <- paste0("rgset_",dataset_names[i],".Rda")


setwd(ruvDirTuningPlotData)
load(paste0("p.matrix_",dataset_names[i],".Rda" ))

# We need to load the true sex chromosomes probes:
load(paste0(funnormDir,"/sex_analysis/x.probes.status.Rda"))
truth <- inactivated.x.probes # 1678 probes

cutoff.vector = c(100,1000,10000,100000)

counts.matrix <- matrix(NA, length(cutoff.vector), ncol(p.matrix))
for (l in 1:length(cutoff.vector)){
	for (k in 1:ncol(p.matrix)){
		p <- p.matrix[,k, drop=FALSE]
		p <- rownames(p.matrix)[order(p)][1:cutoff.vector[l]]
		counts.matrix[l,k] <- sum(truth %in% p)
		print(k)
	}
}


i=4
setwd(ruvDirTuningPlotData)
load(file=paste0("counts.matrix_",dataset_names[i],".Rda"))


pdf("try.pdf")
plot(1:40, counts.matrix[1,], pch=20)
plot(1:40, counts.matrix[2,], pch=20)
plot(1:40, counts.matrix[3,], pch=20)
plot(1:40, counts.matrix[4,], pch=20)
dev.off()



