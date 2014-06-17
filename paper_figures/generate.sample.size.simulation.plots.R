funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
sampleSizeDir <- paste0(funnormDir, "/simulation_samplesize")

n.vector <- c(10,20,30,50,80)

setwd(sampleSizeDir)
load("ci.data.Rda")
library(scales)

#names(ci.data[[1]])
#[1] "raw"          "funnorm"      "noob"         "bmiq"         "quantile"    
#[6] "swan"         "dasen"        "funnorm.noob"

for (i in 1:5){
	xlim=c(0,0.1)
	plot(ci.data[[i]][[8]]$grid, 
		ci.data[[i]][[8]]$mean, 
		type="l", 
		lty=1, 
		col="white", 
		xlim=xlim,
		bty="n",
		xlab="1-Specificity",
		ylab="Sensitivity",
		main=paste0("n = ",n.vector[i]))
	j=8
	color = "deeppink2"
	polygon(x=c(rev(ci.data[[i]][[j]]$grid),(ci.data[[i]][[j]]$grid)), 
		y=c(rev(ci.data[[i]][[j]]$down),(ci.data[[i]][[j]]$up)),, 
		col = alpha(color,0.2), border = NA)

	lines(ci.data[[i]][[j]]$grid, ci.data[[i]][[j]]$mean, col=color, lty=1) 
	lines(ci.data[[i]][[j]]$grid, ci.data[[i]][[j]]$up, col=color, lty=3)
	lines(ci.data[[i]][[j]]$grid, ci.data[[i]][[j]]$down, col=color, lty=3)

	j=3
	color = "black"
	polygon(x=c(rev(ci.data[[i]][[j]]$grid),(ci.data[[i]][[j]]$grid)), 
		y=c(rev(ci.data[[i]][[j]]$down),(ci.data[[i]][[j]]$up)),, 
		col = alpha(color,0.2), border = NA)

	lines(ci.data[[i]][[j]]$grid, ci.data[[i]][[j]]$mean, col=color, lty=1) 
	lines(ci.data[[i]][[j]]$grid, ci.data[[i]][[j]]$up, col=color, lty=3)
	lines(ci.data[[i]][[j]]$grid, ci.data[[i]][[j]]$down, col=color, lty=3)

	legend("bottomright",c("Raw","Funnorm + Noob"), col=c("black","deeppink2"), lty=1, bty="n")
}
dev.off()


