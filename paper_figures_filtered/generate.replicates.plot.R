


colors7 <- c("black", "deepskyblue2", "orange", "olivedrab", "red", "slateblue4", "deeppink4","deeppink1")
names7 <- c("Raw", "Quantile", "noob","dasen", "SWAN", "BMIQ", "Funnorm","Funnorm w noob")
vector7 <- c(1, 2, 4, 5, 6, 7, 3,8)



load(file.path(input.dir,"technical_replicates/technicalVariances.Rda"))
 # Need to filter bad probes:
load("/Users/Jean-Philippe/funnorm_repro/bad_probes/bad.probes.rda")
for (i in 1:length(technicalVariances)){
	technicalVariances[[i]] <- technicalVariances[[i]][!(names(technicalVariances[[i]]) %in% bad.probes)]
}

setwd(file.path(input.dir, "technical_replicates/"))
save(technicalVariances, file="technicalVariances.filtered.Rda")




create.replicates <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", 
	output.dir = "/Users/Jean-Philippe/funnorm_repro/paper_figures_filtered", 
	print = TRUE, save = TRUE) {


	load(file.path(input.dir,"technical_replicates/technicalVariances.filtered.Rda"))

	if (save) {
		setwd(output.dir)

		pdf("Technical_Replicates_Figure_AddMethods.pdf", width=12, height=5)
		par(bty="l")
		boxplot(technicalVariances[vector7], ylim=c(0,0.002), 
				names= names7,
				col = colors7, outline=FALSE, ylab= "Variances",xlab="")
		#axis(side=1,labels=names7,las=2, at=1:8, cex.axis=0.6)
		dev.off() 
	}

	if (print) {
			
		par(bty="l")
		boxplot(technicalVariances[vector7], ylim=c(0,0.002), 
				names= names7,
				col = colors7, outline=FALSE, ylab= "Variances")

	}
}