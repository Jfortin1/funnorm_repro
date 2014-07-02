# Create the differentially methylation positions statistics for each dataset 
#i=as.numeric(commandArgs(TRUE)[1])

funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
normDir   <- paste0(funnormDir,"/norm_datasets")
scriptDir <- paste0(funnormDir,"/scripts")
effectSizeDir    <- paste0(funnormDir,"/effect_size_analysis")

dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")


for (i in 1:6){


data.file <- paste0("all_norm_",dataset_names[i],".Rda")

setwd(normDir)
load(data.file) # load norm.matrices list

# Creation of the phenotype: 
	sampleNames <- colnames(norm.matrices[[1]])

	design_names <- c("ontario_ebv","ontario_blood","kirc")
	design_names <- c(paste0("design_",design_names))
	design_names <- c(design_names,design_names,"design_aml","design_ontario_gender")

	setwd(designDir)
	design_file <- paste0(design_names[i],".Rda")
	load(design_file)
	design <- get(design_names[i])

	pheno <- as.character(design$group[match(sampleNames, design$sampleName)])
	names(pheno) <- design$sampleName[match(sampleNames, design$sampleName)]

# Need to calculate effect sizes. 

	pheno.u <- unique(pheno)
	gr1 <- pheno.u[1]
	gr2 <- pheno.u[2]
	names1 <- names(pheno)[pheno==gr1]
	names2 <- names(pheno)[pheno==gr2]

	diffs <- vector("list", length(norm.matrices))
	for (j in 1:length(norm.matrices)){
		crt <- norm.matrices[[j]]
		mean1 <- rowMeans(crt[, match(names1, colnames(crt))])
		mean2 <- rowMeans(crt[, match(names2, colnames(crt))])
		diffs[[j]] <- mean1 - mean2 
		#print(j)
	}



	setwd(effectSizeDir)
	save(diffs, file=paste0("diffs_", dataset_names[i], ".Rda"))
	print(i)
}


	