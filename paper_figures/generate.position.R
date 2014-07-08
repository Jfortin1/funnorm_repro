



create.position.plots <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", 
	output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = TRUE, save = TRUE) {

	# Toronto:

	setwd(file.path(input.dir,"paper_figures/data"))
	load("quantiles.raw.lympho.Rda")
	load("quantiles.norm.lympho.Rda")
	sampleNames <- colnames(quantiles.raw[[1]])
	position <- substr(sampleNames,12,17)
	row <- substr(sampleNames,14,14)
	column <- substr(sampleNames,17,17)

	### We need to demean:
	chip.names <- substr(sampleNames,1,10)
	chips <- unique(chip.names)
	for (i in 1:length(chips)){
		for (k in 1:2){
			indices <- chip.names == chips[i]
			if (sum(indices)>1){
				mean <- apply(quantiles.raw[[k]][,indices],1,mean)
			} else {
				mean <- quantiles.raw[[k]][,indices]
			}
			
			quantiles.raw[[k]][,indices] <- quantiles.raw[[k]][,indices]-mean
			if (sum(indices)>1){
				mean <- apply(quantiles.norm[[k]][,indices],1,mean)
			} else {
				mean <- quantiles.norm[[k]][,indices]
			}
			quantiles.norm[[k]][,indices] <- quantiles.norm[[k]][,indices]- mean
		}
	}
	
	quantiles.raw.toronto <- quantiles.raw
	quantiles.norm.toronto <- quantiles.norm


	load("quantiles.raw.Rda")
	load("quantiles.norm.Rda")
	sampleNames <- colnames(quantiles.raw[[1]])
	position <- substr(sampleNames,12,17)
	row <- substr(sampleNames,14,14)
	column <- substr(sampleNames,17,17)

		### We need to demean:
	chip.names <- substr(sampleNames,1,10)
	chips <- unique(chip.names)
	for (i in 1:length(chips)){
		for (k in 1:2){
			indices <- chip.names == chips[i]
			mean <- apply(quantiles.raw[[k]][,indices],1,mean)
			quantiles.raw[[k]][,indices] <- quantiles.raw[[k]][,indices]-mean
			mean <- apply(quantiles.norm[[k]][,indices],1,mean)
			quantiles.norm[[k]][,indices] <- quantiles.norm[[k]][,indices]- mean
		}
	}





	if (save){

		setwd(output.dir)

		pdf("Position_Raw_1.pdf", width=5, height = 5)
		drawPositionPlot(quantiles.raw[[2]], index=2, y=0.05)
		dev.off()

		pdf("Position_Raw_2.pdf", width=5, height = 5)
		drawPositionPlot(quantiles.raw[[1]], index=3, y=0.04)
		dev.off()

		pdf("Position_Raw_3.pdf", width=5, height = 5)
		drawPositionPlot(quantiles.raw.toronto[[2]], index=17, y=0.07)
		dev.off()

		pdf("Position_Norm_1.pdf", width=5, height = 5)
		drawPositionPlot(quantiles.norm[[2]], index=2, y=0.05)
		dev.off()

		pdf("Position_Norm_2.pdf", width=5, height = 5)
		drawPositionPlot(quantiles.norm[[1]], index=3, y=0.04)
		dev.off()

		pdf("Position_Norm_3.pdf", width=5, height = 5)
		drawPositionPlot(quantiles.norm.toronto[[2]], index=17, y=0.07)
		dev.off()

	}

	if (print){
		drawPositionPlot(quantiles.raw[[2]], index=2, y=0.05)
		drawPositionPlot(quantiles.raw[[1]], index=3, y=0.04)
		drawPositionPlot(quantiles.raw.toronto[[2]], index=17, y=0.07)
		drawPositionPlot(quantiles.norm[[2]], index=2, y=0.05)
		drawPositionPlot(quantiles.norm[[1]], index=3, y=0.04)
		drawPositionPlot(quantiles.norm.toronto[[2]], index=17, y=0.07)
	}


}



	### To draw positions plots
	drawPositionPlot <- function(quantiles, index, y){
		sampleNames <- colnames(quantiles)
		position <- substr(sampleNames,12,17)
		row    <- substr(sampleNames,14,14)
		column <- substr(sampleNames,17,17)
		o      <- order(column, row)
		pos    <- position[o]
		counts <- table(pos)[unique(pos)]
		counts <- as.numeric(counts)
		ticks  <- cumsum(counts)
		
		quantiles <- quantiles[,o]
		
		plot(quantiles[index,], 
		     ylim=c(-y,y), 
		     pch=20, 
		     cex=1.5, 
		     xlab="Position", 
		     ylab="Intensities", 
		     bty="n", xaxt="n"
		)
		
		for (j in 1:length(ticks)){
			abline(v=ticks[j], lty=3)
		}
		abline(h=0)
	}


	