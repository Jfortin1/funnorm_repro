
funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
scriptDir <- paste0(funnormDir,"/scripts")
sensitivityDir <- paste0(funnormDir,"/sensitivity_analysis")
dmpsDir    <- paste0(sensitivityDir,"/dmps")
normDir   <- paste0(sensitivityDir,"/norm_datasets")
rocDir   <- paste0(sensitivityDir,"/roc_data")

dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")


for (i in c(1,3)){


	dis.index <- i
	val.index <- i+3

	k=1
	data.file.dis <- paste0("dmps_",dataset_names[dis.index],"_k_",k,".Rda")
	data.file.val <- paste0("dmps_",dataset_names[val.index],"_k_",k,".Rda")

	setwd(dmpsDir)
	load(data.file.dis)
	dis <- dmps
	load(data.file.val)
	val <- dmps

	for (k in 2:10){
		data.file.dis <- paste0("dmps_",dataset_names[dis.index],"_k_",k,".Rda")
		data.file.val <- paste0("dmps_",dataset_names[val.index],"_k_",k,".Rda")
		load(data.file.dis)
		dis <- c(dis,dmps)
		load(data.file.val)
		val <- c(val,dmps)
		print(k)
	}

	names(dis) <- names(val) <- 1:10

	setwd(scriptDir)
	source("generateROCData.R")

	setwd(rocDir)
	dataset_names <- c("ontario_ebv","ontario_blood","kirc")

	k_vector <- c(100000,100,100000)

	rocData <- generateROCData(dis,val,k_vector[i])

	save(rocData, file=paste0("rocData_",k_vector[k]/1000,"K_",dataset_names[i],".Rda"))

}









