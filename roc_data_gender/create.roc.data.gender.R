

funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
normDir   <- paste0(funnormDir,"/norm_datasets")
scriptDir <- paste0(funnormDir,"/scripts")
dmpsDir   <- paste0(funnormDir,"/dmps")
rocGenderDir    <- paste0(funnormDir,"/roc_data_gender")
svaDir    <- paste0(funnormDir,"/sva_results")


setwd(dmpsDir)
load("dmps_ontario_gender.Rda")


setwd(scriptDir)
source("generateROCDataGender.R")

setwd(rocGenderDir)
### To generate ROC data for gender:
rocDataGender <- generateROCDataGender(dir=funnormDir, dataset=dmps)
save(rocDataGender, file="rocDataGender.Rda")







