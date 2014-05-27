# We will focus on the EBV dataset 
k=as.numeric(commandArgs(TRUE)[1])
j=as.numeric(commandArgs(TRUE)[2])

funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
normDir   <- paste0(funnormDir,"/norm_datasets")
scriptDir <- paste0(funnormDir,"/scripts")
sampleSizeDir <- paste0(funnormDir, "/simulation_samplesize")
dmpsDir <- paste0(sampleSizeDir, "/dmps_noob")

dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")


n.vector <- c(10,20,30,50,80)


# Download the dmps file for validation:
file = paste0("dmps_ontario_ebv_val_n_",n.vector[k],"_B_",j,".Rda")
setwd(dmpsDir)
load(file)
val = dmps

# Download the dmps file for discovery:
dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")
dis.index <- 1
data.file.dis <- paste0("dmps_",dataset_names[dis.index],".Rda")
setwd(paste0(funnormDir,"/dmps"))
load(data.file.dis)
dis <- dmps[6]

setwd(scriptDir)
source("generateROCData.R")

setwd(paste0(sampleSizeDir,"/rocData_noob"))
k_vector <- c(100000,100,100000)
rocData <- generateROCData(dis,val,k_vector[1])
save(rocData, file=paste0("rocData_ontario_ebv_n_",n.vector[k],"_B_",j,".Rda"))


