# This script will create a RGSet for the discovery cohort and a RGSet for the validation cohort

funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
normDir   <- paste0(funnormDir,"/norm_datasets")
scriptDir <- paste0(funnormDir,"/scripts")
svaDir    <- paste0(funnormDir,"/sva_results")
combatDir <- paste0(funnormDir,"/combat_results")


dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")




setwd(combatDir)
for (i in c(3,6,7)){
	data.file <- paste0("combat_results_",dataset_names[i],".Rda")
	load(data.file)
	object <- combat.results$results.combat
	object <- sort(object, decreasing=TRUE)
	dmps <- as.matrix(object)
	colnames(dmps)[1] <- "f"
	dmps <- as.data.frame(dmps)
	dmps$f2 <- dmps[,1]
	dmps <- list(combat=dmps)
	save(dmps, file=paste0("combat_dmps_",dataset_names[i],".Rda"))
	print(i)
}



