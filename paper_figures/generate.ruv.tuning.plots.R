


create.ruv.tuning.plots <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", 
	output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = TRUE, save = TRUE) {


	dataDir <- paste0(input.dir,"/ruv_results/sex_tuning_plot_data")
	plotDir <- output.dir

	dataset_names <- c("ontario_ebv","ontario_blood","kirc")
	dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
	dataset_names <- c(dataset_names,"aml","ontario_gender")

	colors <- c("black","orange","deeppink3")
	pch <- c(20,3,5)
	lines <- c(14,36,0,11,5,3,0,21)
	inf <- c(1350,1500,1350,1300,1400,1410,1400,1000)
	sup <- c(1530,1640,1570,1530,1630,1570,1610,1330)

	if (save){

		for (i in 1:8){
			setwd(dataDir)
			load(paste0("counts.matrix_",dataset_names[i],".Rda"))
			counts.matrix <- counts.matrix[2:4,]
			setwd(plotDir)
			pdf(paste0("RUV_Tuning_",dataset_names[i],".pdf"),width=5, height=5)
			plot(0:40, counts.matrix[1,], 
				col=colors[1], 
				pch=pch[1],
				ylim=c(inf[i],sup[i]),
				bty="n",
				xlab="K",
				ylab="Number of discovered probes",
				main="")
			for (k in 2:3){
				points(0:40, counts.matrix[k,], col=colors[k], pch=pch[k], lwd=2)
			}
			abline(v=lines[i], lty=3)
			legend("topright",c("Top 25000","Top 50000","Top 100000"), col=colors, pch=pch, bty="n")
			dev.off()
		}

	}

	if (print){

		for (i in 1:8){
			setwd(dataDir)
			load(paste0("counts.matrix_",dataset_names[i],".Rda"))
			counts.matrix <- counts.matrix[2:4,]

			plot(0:40, counts.matrix[1,], 
				col=colors[1], 
				pch=pch[1],
				ylim=c(inf[i],sup[i]),
				bty="n",
				xlab="K",
				ylab="Number of discovered probes",
				main="")
			for (k in 2:3){
				points(0:40, counts.matrix[k,], col=colors[k], pch=pch[k], lwd=2)
			}
			abline(v=lines[i], lty=3)
			legend("topright",c("Top 25000","Top 50000","Top 100000"), col=colors, pch=pch, bty="n")

		}

	}





}



# # To generate the RUV-Funnorm tuning plots:
# setwd(paste0(funnormDir,"/ruv_funnorm_results/sex_tuning_plot_data"))
# dataset_names <- c("ontario_ebv","ontario_blood","kirc")
# dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
# dataset_names <- c(dataset_names,"aml","ontario_gender")
# colors <- c("black","orange","deeppink3")
# pch <- c(20,3,5)
# lines <- c(25,3,0,6,1,3,0,18)
# inf <- c(1300,1500,1350,1270,1400,1410,1400,1000)
# sup <- c(1530,1640,1570,1530,1630,1570,1610,1500) 
# for (i in 1:8){
# 	setwd(paste0(funnormDir,"/ruv_funnorm_results/sex_tuning_plot_data"))
# 	load(paste0("counts.matrix_",dataset_names[i],".Rda"))
# 	counts.matrix <- counts.matrix[2:4,]
# 	setwd(plotDir)
# 	pdf(paste0("RUV_Funnorm_Tuning_",dataset_names[i],".pdf"),width=5, height=5)
# 	plot(0:40, counts.matrix[1,], 
# 		col=colors[1], 
# 		pch=pch[1],
# 		ylim=c(inf[i],sup[i]),
# 		bty="n",
# 		xlab="K",
# 		ylab="Number of discovered probes",
# 		main="")
# 	for (k in 2:3){
# 		points(0:40, counts.matrix[k,], col=colors[k], pch=pch[k], lwd=2)
# 	}
# 	abline(v=lines[i], lty=3)
# 	legend("topright",c("Top 25000","Top 50000","Top 100000"), col=colors, pch=pch, bty="n")
# 	dev.off()
# }













