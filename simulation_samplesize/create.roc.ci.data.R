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

# There are 6 different folders:
# 1. rocData_raw
# 2. rocData
# 3. rocData_noob
# 4. rocData_bmiq
# 5. rocData_norm_other
# 6. rocData_norm_new

names <- c("_raw","","_noob","_bmiq","_norm_other","_norm_new")
dir   <- paste0(sampleSizeDir,"/rocData",names)

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


method.names <- c("raw","funnorm","noob","bmiq","quantile","swan","dasen","funnorm.noob")


results <- vector("list",5)
for (i in 1:5){
	results[[i]] <- vector("list", length(method.names))
}
for (i in 1:5){
	n <- n.vector[i]
	j=1
	kk=1
	setwd(dir[kk])
	load(paste0("rocData_ontario_ebv_n_",n,"_B_",j,".Rda"))
	totalRocData <- rocData
	for (kk in 2:length(dir)){
		setwd(dir[kk])
		load(paste0("rocData_ontario_ebv_n_",n,"_B_",j,".Rda"))
		totalRocData[[1]] <- c(totalRocData[[1]],rocData[[1]])
		totalRocData[[2]] <- c(totalRocData[[2]],rocData[[2]])
	}
	for (kk in 1:length(dir)){
		for (j in 2:B){
			setwd(dir[[kk]])
			load(paste0("rocData_ontario_ebv_n_",n,"_B_",j,".Rda"))
			totalRocData[[1]] <- c(totalRocData[[1]],rocData[[1]])
			totalRocData[[2]] <- c(totalRocData[[2]],rocData[[2]])
		}
	}
	
	l <- length(totalRocData[[1]][[1]])
	for (kk in 1:length(method.names)){
		indices <- names(totalRocData[[1]]) %in% method.names[kk]
		matrix.y <- matrix(unlist(totalRocData[[2]][indices]),nrow=l)
		matrix.x <- matrix(unlist(totalRocData[[1]][indices]),nrow=l)
		ci <- return.ci.roc.curves(matrix.x=matrix.x,matrix.y=matrix.y)
		results[[i]][[kk]] <- ci
	}
	print(i)
}

names(results) <- n.vector
for (i in 1:5){
	names(results[[i]]) <- method.names
}

setwd(sampleSizeDir)
ci.data <- results
save(results, file="ci.data.Rda")






