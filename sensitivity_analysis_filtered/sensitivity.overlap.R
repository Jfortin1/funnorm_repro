
funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
scriptDir <- paste0(funnormDir,"/scripts")
sensitivityDir <- paste0(funnormDir,"/sensitivity_analysis")
sensitivityDir2 <- paste0(funnormDir,"/sensitivity_analysis_filtered")
dmpsDir    <- paste0(sensitivityDir,"/dmps")
normDir   <- paste0(sensitivityDir,"/norm_datasets")
overlapDir   <- paste0(sensitivityDir2,"/overlap_data")
aml27kDir <- paste0(funnormDir,"/aml_27k")
badDir    <- paste0(funnormDir,"/bad_probes")

dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")




setwd(dmpsDir)

k=1
data.file.dis <- paste0("dmps_aml_k_",k,".Rda")
load(data.file.dis)
dmps450k <- dmps

for (k in 2:10){
	data.file.dis <- paste0("dmps_aml_k_",k,".Rda")
	load(data.file.dis)
	dmps450k <- c(dmps450k,dmps)
	print(k)
}

names(dmps450k) <- 1:10

# Let's load the 27k dmps:
setwd(aml27kDir)
load("dmps_27k_aml_plate_adjusted.Rda")
dmps27k <- list(dmps=dmps)
for (i in 1:length(dmps450k)){
	dmps27k[[i]] <- dmps
}

n <- length(dmps450k)
for (i in 1:n){
	overlap <- intersect(rownames(dmps450k[[i]]), rownames(dmps27k[[i]]))
	dmps450k[[i]] <- dmps450k[[i]][match(overlap, rownames(dmps450k[[i]])),]
	dmps450k[[i]] <- dmps450k[[i]][order(abs(dmps450k[[i]]$f), decreasing=TRUE),]
	dmps27k[[i]] <- dmps27k[[i]][match(overlap, rownames(dmps27k[[i]])),]
	dmps27k[[i]] <- dmps27k[[i]][order(abs(dmps27k[[i]]$f), decreasing=TRUE),]
	dmps450k[[i]] <- na.omit(dmps450k[[i]])
	dmps27k[[i]] <- na.omit(dmps27k[[i]])
	print(i)
}



names(dmps27k) <- names(dmps450k)

# Filtering the bad probes:
load(file.path(badDir, "bad.probes.rda"))

	for (j in 1:length(dmps450k)){
		dmps450k[[j]] <- dmps450k[[j]][!(rownames(dmps450k[[j]]) %in% bad.probes),]
		dmps27k[[j]] <- dmps27k[[j]][!(rownames(dmps27k[[j]]) %in% bad.probes),]
		print(j)
	}



setwd(scriptDir)
source("generateOverlapData.R")

setwd(overlapDir)
overlapData1K <- generateOverlapData(dmps27k, dmps450k, step=10)
save(overlapData1K, file="overlapData1K_aml.Rda")



