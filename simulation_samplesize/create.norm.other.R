
# We will focus on the EBV dataset 
k=as.numeric(commandArgs(TRUE)[1]) # k: specifies the sample size
j=as.numeric(commandArgs(TRUE)[2]) # B: specifies the bootstraped samples

# Other normalization:
# Quantile
# SWAN
# Dasen 

funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
normDir   <- paste0(funnormDir,"/norm_datasets")
scriptDir <- paste0(funnormDir,"/scripts")
sampleSizeDir <- paste0(funnormDir, "/simulation_samplesize")
normDir3 <- paste0(sampleSizeDir, "/norm_other")

dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")


setwd(sampleSizeDir)
load("subsamples.matrices.Rda")
samples <- subsamples.matrices[[k]][,j]


library(minfi)
i = 4
data.file <- paste0("rgset_",dataset_names[i],".Rda")

setwd(disValDir)
load(data.file)
rgset <- get("rgset_val")
rgset <- rgset[,samples]

n.vector <- c(10,20,30,50,80)


library(minfi)
library(wateRmelon)

normalizeDasen <- function(RGSet){
		
		mset <- preprocessRaw(RGSet)
		meth <- getMeth(mset)
		unmeth <- getUnmeth(mset)
		
		# Design:
		typeI <-  minfi::getProbeInfo(mset,type="I")$Name
		typeII <-  minfi::getProbeInfo(mset,type="II")$Name
		onetwo <- rep("I", nrow(meth))
		onetwo[rownames(meth) %in% typeII] <- "II"
		
		norm <-  wateRmelon:::dasen(mns = meth, uns = unmeth, onetwo = onetwo, fudge =100, ret2 = FALSE)
		return(norm)	
}


normalizeSwan <- function(RGSet){
		return(getBeta(preprocessSWAN(RGSet)))
}

normalizeQuantile <- function(RGSet){
		return(getBeta(preprocessQuantile(RGSet)))
}

rgset <- updateObject(rgset)

# Quantile normalization
quantile.norm <- normalizeQuantile(rgset)

# SWAN
swan.norm <- normalizeSwan(rgset)

# DASEN
dasen.norm <- normalizeDasen(rgset)

file.name <- paste0("ontario_ebv_val_n_",n.vector[k],"_B_",j,".Rda")
setwd(normDir3)
save(quantile.norm, swan.norm, dasen.norm, file=file.name)


