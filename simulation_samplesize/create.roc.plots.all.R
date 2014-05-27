funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
normDir   <- paste0(funnormDir,"/norm_datasets")
scriptDir <- paste0(funnormDir,"/scripts")
dmpsDir   <- paste0(funnormDir,"/dmps")
rocDir    <- paste0(funnormDir,"/roc_data")
rocPlotDir <- paste0(funnormDir,"/roc_plots")
sampleSizeDir <- paste0(funnormDir, "/simulation_samplesize")
rocDir <- paste0(sampleSizeDir, "/rocData")
rocPlotDir <- paste0(sampleSizeDir,"/roc_plots")

n.vector <- c(10,20,30,50,80)

setwd(scriptDir)
source("printROCFromROCData.R")

B=100
colors <- rep("grey85",B)
colors <- c(colors, "deeppink3","deeppink3","deeppink3","black","orange","deeppink3")
lty <- rep(1,B)
lty <- c(lty,1,3,3,1,1,1)
names <- rep("",B)



for (i in 1:5){



	setwd(rocPlotDir)
	load(paste0("data.n.",n.vector[i],".Rda"))
	rocData <- totalRocData
	
	load(paste0("data.raw.n.",n.vector[i],".Rda"))
	
	rocData[[1]] <- c(rocData[[1]],totalRocData[[1]][101])
	rocData[[2]] <- c(rocData[[2]],totalRocData[[2]][101])
	load(paste0("data.noob.n.",n.vector[i],".Rda"))
	rocData[[1]] <- c(rocData[[1]],totalRocData[[1]][101])
	rocData[[2]] <- c(rocData[[2]],totalRocData[[2]][101])
	load(paste0("data.n.",n.vector[i],".Rda"))
	rocData[[1]] <- c(rocData[[1]],totalRocData[[1]][101])
	rocData[[2]] <- c(rocData[[2]],totalRocData[[2]][101])

	setwd(rocPlotDir)
	pdf(paste0("roc.plot.n.all",n.vector[i],".pdf"))
	printROCFromROCData(rocData, xcutoff=0.1, main="", colors=colors, names=names, lty=lty)
	dev.off()

}

# Alternative
for (i in 1:5){

	B=100
	colors <- rep("grey85",B)
	colors <- c(colors, "black","black","black","orange","deeppink3")
	lty <- rep(1,B)
	lty <- c(lty,1,3,3,1,1,1)
	names <- rep("",B)


	setwd(rocPlotDir)
	load(paste0("data.raw.n.",n.vector[i],".Rda"))
	rocData <- totalRocData
	
	load(paste0("data.noob.n.",n.vector[i],".Rda"))
	rocData[[1]] <- c(rocData[[1]],totalRocData[[1]][101])
	rocData[[2]] <- c(rocData[[2]],totalRocData[[2]][101])
	load(paste0("data.n.",n.vector[i],".Rda"))
	rocData[[1]] <- c(rocData[[1]],totalRocData[[1]][101])
	rocData[[2]] <- c(rocData[[2]],totalRocData[[2]][101])

	setwd(rocPlotDir)
	pdf(paste0("roc.plot.n.all",n.vector[i],".pdf"))
	printROCFromROCData(rocData, xcutoff=0.1, main="", colors=colors, names=names, lty=lty)
	dev.off()

}

# Alternative 2
for (i in 1:5){

	B=100
	colors <- rep("grey85",B)
	colors <- c(colors, "black","black","black","orange","orange","orange","deeppink3","deeppink3","deeppink3")
	lty <- rep(1,B)
	lty <- c(lty,1,3,3,1,3,3,1,3,3)
	names <- rep("",B)


	setwd(rocPlotDir)
	load(paste0("data.raw.n.",n.vector[i],".Rda"))
	rocData <- totalRocData
	
	load(paste0("data.noob.n.",n.vector[i],".Rda"))
	rocData[[1]] <- c(rocData[[1]],totalRocData[[1]][101:103])
	rocData[[2]] <- c(rocData[[2]],totalRocData[[2]][101:103])
	load(paste0("data.n.",n.vector[i],".Rda"))
	rocData[[1]] <- c(rocData[[1]],totalRocData[[1]][101:103])
	rocData[[2]] <- c(rocData[[2]],totalRocData[[2]][101:103])

	setwd(rocPlotDir)
	pdf(paste0("roc.plot.n.all",n.vector[i],".pdf"))
	printROCFromROCData(rocData, xcutoff=0.1, main="", colors=colors, names=names, lty=lty)
	dev.off()

}


# Alternative 3
for (i in 1:5){

	B=100
	colors <- rep("grey92",B)
	colors <- c(colors, "black","black","black","deeppink3","deeppink3","deeppink3")
	lty <- rep(1,B)
	lty <- c(lty,1,3,3,1,3,3)
	lwd <- rep(1,B)
	lwd <- c(lwd, 2,1,1,2,1,1)
	names <- rep("",B)


	setwd(rocPlotDir)
	load(paste0("data.raw.n.",n.vector[i],".Rda"))
	rocData <- totalRocData

	load(paste0("data.n.",n.vector[i],".Rda"))
	rocData[[1]] <- c(rocData[[1]],totalRocData[[1]][101:103])
	rocData[[2]] <- c(rocData[[2]],totalRocData[[2]][101:103])

	setwd(rocPlotDir)
	pdf(paste0("roc.plot.n.all.partial",n.vector[i],".pdf"))
	printROCFromROCData(rocData, xcutoff=0.1, main="", colors=colors, names=names, lty=lty, lwd=lwd)
	dev.off()

}





