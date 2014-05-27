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
rocDir <- paste0(sampleSizeDir, "/rocData_raw")
rocPlotDir <- paste0(sampleSizeDir,"/roc_plots")

n.vector <- c(10,20,30,50,80)

setwd(scriptDir)
source("printROCFromROCData.R")

B=100
colors <- rep("grey85",B)
colors <- c(colors, "deeppink3","deeppink3","deeppink3")
lty <- rep(1,B)
lty <- c(lty,1,3,3)
names <- rep("",B)



for (i in 1:5){

	n <- n.vector[i]
	setwd(rocDir)
	k_vector <- c(100000,100,100000)

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
	matrix.y <- matrix(unlist(totalRocData[[2]]),nrow=501)
	matrix.x <- matrix(unlist(totalRocData[[1]]),nrow=501)


	if (i %in% 1:4){
		span=0.05
	} else {
		span=0.1
	}

	mean.list <- return.mean(matrix.x=matrix.x,matrix.y=matrix.y, span=span)
	mean <- mean.list$mean
	x <- 1-mean.list$grid
	up = mean.list$up
	down =mean.list$down
	totalRocData[[1]] <- c(totalRocData[[1]], list(x,x,x))
	totalRocData[[2]] <- c(totalRocData[[2]], list(mean,up,down))
	setwd(rocPlotDir)
	save(totalRocData, file=paste0("data.raw.n.",n.vector[i],".Rda"))
	pdf(paste0("roc.plot.raw.n.",n.vector[i],".pdf"))
	printROCFromROCData(totalRocData, xcutoff=0.1, main="", colors=colors, names=names, lty=lty)
	dev.off()

}

return.mean <- function(matrix.x, matrix.y, span=0.05){

	grid <- seq(0,1,0.001)
	g <- length(grid)
	new.matrix.y <- matrix(NA,g,B)
	for (b in 1:B){
		#l <- loess(matrix.y[,b]~matrix.x[,b], span=span)
		#new.matrix.y[,b] <- predict(l, grid)
		new.matrix.y[,b] <- approx(x=1-matrix.x[,b], y=matrix.y[,b], xout=grid)$y 
	}

	my.quantile <- function(x){
		quantile(x, prob=c(0.025,0.975),na.rm=TRUE)
	}
	mean <- rowMeans(new.matrix.y)
	#var  <- 1.96*sqrt(apply(new.matrix.y, 1, my.variance))
	up <- apply(new.matrix.y, 1, my.quantile)[2,]
	down <- apply(new.matrix.y,1,my.quantile)[1,]
	return(list(grid=grid, mean=mean,up=up, down=down))
}




