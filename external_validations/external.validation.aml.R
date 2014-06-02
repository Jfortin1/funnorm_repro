funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
aml27kDir <- paste0(funnormDir,"/aml_27k")
rawDir     <- paste0(funnormDir,"/raw_datasets")
metaDir    <- paste0(funnormDir,"/metadata")
dmpsDir    <- paste0(funnormDir,"/dmps")
svaDir     <- paste0(funnormDir,"/sva_results")
svaFunnormDir <- paste0(funnormDir,"/sva_funnorm_results")
combatDir  <- paste0(funnormDir,"/combat_results")
scriptDir  <- paste0(funnormDir,"/scripts")
externalValDir <- paste0(funnormDir,"/external_validations")


# Let's load the 450k dmps:
setwd(dmpsDir)
load("dmps_aml.Rda")
dmps450k <- dmps

# Need to add dmps for sva, sva+funnorm and combat
setwd(svaDir)
load("sva_dmps_aml.Rda")
dmps450k <- c(dmps450k,dmps)

setwd(svaFunnormDir)
load("sva_funnorm_dmps_aml.Rda")
dmps450k <- c(dmps450k,dmps)

setwd(combatDir)
load("combat_dmps_aml.Rda")
dmps450k <- c(dmps450k,dmps)

# Let's load the 27k dmps:
setwd(aml27kDir)
load("dmps_27k_aml_plate_adjusted.Rda")
dmps27k <- list(dmps=dmps)
for (i in 1:length(dmps450k)){
	dmps27k[[i]] <- dmps
}


# For each dataset, need to take the overlap between the 27k data and 450k data:
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

setwd(scriptDir)
source("generateOverlapData.R")

setwd(externalValDir)
overlapData1K <- generateOverlapData(dmps27k, dmps450k, step=10)
save(overlapData1K, file="overlapData1K_aml.Rda")




# Now let's create ROC data from external truth:
# ROC curve with the 27k data as the truth:

# Let's load the 27k dmps:
p.list <- dmps27k
cutoff = 0.05
for (i in 1:length(p.list)){
	p.list[[i]] <- p.list[[i]]$pval
	p.list[[i]] <- p.adjust(p.list[[i]],method="fdr")
	names(p.list[[i]]) <- rownames(dmps27k[[i]])
	p.list[[i]] <- names(p.list[[i]])[p.list[[i]]<=cutoff]
	print(i)
}

setwd(scriptDir)
source("generateROCDataFromExternalTruth.R")
rocdata_27k_aml <- generateROCDataFromExternalTruth(p.list, dmps450k)
	
setwd(externalValDir)	
save(rocdata_27k_aml , file="rocdata_27k_aml.Rda")






