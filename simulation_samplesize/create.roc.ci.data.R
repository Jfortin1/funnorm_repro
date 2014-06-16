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

setwd(scriptDir)


return.ci.roc.curves <- function(matrix.x, matrix.y){
	grid <- seq(0,1,0.001)
	g <- length(grid)
	new.matrix.y <- matrix(NA,g,B)
	for (b in 1:B){
		new.matrix.y[,b] <- approx(x=1-matrix.x[,b], y=matrix.y[,b], xout=grid)$y 
	}

	my.quantile <- function(x){
		quantile(x, prob=c(0.025,0.975),na.rm=TRUE)
	}
	mean <- rowMeans(new.matrix.y)
	up <- apply(new.matrix.y, 1, my.quantile)[2,]
	down <- apply(new.matrix.y,1,my.quantile)[1,]
	return(list(grid=grid, mean=mean,up=up, down=down))
}


n.vector <- c(10,20,30,50,80)
B=100
k_vector <- c(100000,100,100000)
ci_list  <- vector("list", length(n.vector))



for (i in 1:5){
	n <- n.vector[i]
	setwd(rocDir)
	j=1
	load(paste0("rocData_ontario_ebv_n_",n,"_B_",j,".Rda"))
	totalRocData <- rocData
	for (j in 2:B){
		load(paste0("rocData_ontario_ebv_n_",n,"_B_",j,".Rda"))
		totalRocData[[1]] <- c(totalRocData[[1]],rocData[[1]])
		totalRocData[[2]] <- c(totalRocData[[2]],rocData[[2]])
		#print(j)
	}

	l <- length(totalRocData[[1]][[1]])
	matrix.y <- matrix(unlist(totalRocData[[2]]),nrow=l)
	matrix.x <- matrix(unlist(totalRocData[[1]]),nrow=l)
	ci_list[[i]] <- return.ci.roc.curves(matrix.x=matrix.x,matrix.y=matrix.y)

}

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

# # Alternative
# for (i in 1:5){

# 	B=100
# 	colors <- rep("grey85",B)
# 	colors <- c(colors, "black","black","black","orange","deeppink3")
# 	lty <- rep(1,B)
# 	lty <- c(lty,1,3,3,1,1,1)
# 	names <- rep("",B)


# 	setwd(rocPlotDir)
# 	load(paste0("data.raw.n.",n.vector[i],".Rda"))
# 	rocData <- totalRocData
	
# 	load(paste0("data.noob.n.",n.vector[i],".Rda"))
# 	rocData[[1]] <- c(rocData[[1]],totalRocData[[1]][101])
# 	rocData[[2]] <- c(rocData[[2]],totalRocData[[2]][101])
# 	load(paste0("data.n.",n.vector[i],".Rda"))
# 	rocData[[1]] <- c(rocData[[1]],totalRocData[[1]][101])
# 	rocData[[2]] <- c(rocData[[2]],totalRocData[[2]][101])

# 	setwd(rocPlotDir)
# 	pdf(paste0("roc.plot.n.all",n.vector[i],".pdf"))
# 	printROCFromROCData(rocData, xcutoff=0.1, main="", colors=colors, names=names, lty=lty)
# 	dev.off()

# }

# # Alternative 2
# for (i in 1:5){

# 	B=100
# 	colors <- rep("grey85",B)
# 	colors <- c(colors, "black","black","black","orange","orange","orange","deeppink3","deeppink3","deeppink3")
# 	lty <- rep(1,B)
# 	lty <- c(lty,1,3,3,1,3,3,1,3,3)
# 	names <- rep("",B)


# 	setwd(rocPlotDir)
# 	load(paste0("data.raw.n.",n.vector[i],".Rda"))
# 	rocData <- totalRocData
	
# 	load(paste0("data.noob.n.",n.vector[i],".Rda"))
# 	rocData[[1]] <- c(rocData[[1]],totalRocData[[1]][101:103])
# 	rocData[[2]] <- c(rocData[[2]],totalRocData[[2]][101:103])
# 	load(paste0("data.n.",n.vector[i],".Rda"))
# 	rocData[[1]] <- c(rocData[[1]],totalRocData[[1]][101:103])
# 	rocData[[2]] <- c(rocData[[2]],totalRocData[[2]][101:103])

# 	setwd(rocPlotDir)
# 	pdf(paste0("roc.plot.n.all",n.vector[i],".pdf"))
# 	printROCFromROCData(rocData, xcutoff=0.1, main="", colors=colors, names=names, lty=lty)
# 	dev.off()

# }


# # Alternative 3
# for (i in 1:5){

# 	B=100
# 	colors <- rep("grey92",B)
# 	colors <- c(colors, "black","black","black","deeppink3","deeppink3","deeppink3")
# 	lty <- rep(1,B)
# 	lty <- c(lty,1,3,3,1,3,3)
# 	lwd <- rep(1,B)
# 	lwd <- c(lwd, 2,1,1,2,1,1)
# 	names <- rep("",B)


# 	setwd(rocPlotDir)
# 	load(paste0("data.raw.n.",n.vector[i],".Rda"))
# 	rocData <- totalRocData

# 	load(paste0("data.n.",n.vector[i],".Rda"))
# 	rocData[[1]] <- c(rocData[[1]],totalRocData[[1]][101:103])
# 	rocData[[2]] <- c(rocData[[2]],totalRocData[[2]][101:103])

# 	setwd(rocPlotDir)
# 	pdf(paste0("roc.plot.n.all.partial",n.vector[i],".pdf"))
# 	printROCFromROCData(rocData, xcutoff=0.1, main="", colors=colors, names=names, lty=lty, lwd=lwd)
# 	dev.off()

# }





