funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
normDir   <- paste0(funnormDir,"/norm_datasets")
scriptDir <- paste0(funnormDir,"/scripts")
dmpsDir   <- paste0(funnormDir,"/dmps")
rocDir    <- paste0(funnormDir,"/roc_data")
rocPlotDir <- paste0(funnormDir,"/roc_plots")


k_vector <- c(100000,100,100000)

dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")

setwd(scriptDir)
source("printROCFromROCData.R")

# Let's load the ROC data
setwd(rocDir)
dataset_names <- c("ontario_ebv","ontario_blood","kirc")
load(paste0("rocData_",k_vector[i]/1000,"K_",dataset_names[i],".Rda"))
roc1 <- rocData
load(paste0("sva_rocData_",k_vector[i]/1000,"K_",dataset_names[i],".Rda"))
roc2 <- rocData
load(paste0("sva_funnorm_rocData_",k_vector[i]/1000,"K_",dataset_names[i],".Rda"))
roc3 <- rocData
spec <- c(roc1$spec,roc2$spec,roc3$spec)[c(1,2,3,6,8,9)]
sens <- c(roc1$sens,roc2$sens,roc3$sens)[c(1,2,3,6,8,9)]
roc <- list(spec=spec,sens=sens)
colors <- c("black","deepskyblue3","deeppink3","orange","grey","olivedrab")


setwd(rocPlotDir)

printROCFromROCData(roc, xcutoff=0.1, main="", colors=colors, names=as.character(1:9), lty=rep(1,9))
dev.off()

