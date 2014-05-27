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
dmpsDir <- paste0(sampleSizeDir,"/dmps_raw")
normDir2 <- paste0(sampleSizeDir,"/raw")



library(minfi)
setwd(designDir)
load("design_ontario_ebv.Rda")
design <- design_ontario_ebv
design <- design[design$set=="Validation",]



n.vector <- c(10,20,30,50,80)

file=paste0("ontario_ebv_val_n_",n.vector[k],"_B_",j,".Rda")
setwd(normDir2)
load(file)
#Normalized matrix is stored in "funnorm"


n <- n.vector[k]
m <- n/2
pheno <- c(rep(1,m),rep(2,m))
names(pheno) <- colnames(raw)



# Creation of the dmps:
setwd(scriptDir)
source("returnDMPSFromNormMatrices.R")

setwd(dmpsDir)
norm.matrices <- list(raw=raw)
dmps <- returnDmpsFromNormMatrices(normMatrices = norm.matrices, pheno = pheno)
save(dmps, file=paste0("dmps_ontario_ebv_val_n_",n.vector[k],"_B_",j,".Rda"))
