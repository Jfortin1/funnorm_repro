# Let's define the colors for the Add Methods:
colors7 <- c("black", "deepskyblue2", "orange", "olivedrab", "red", "slateblue4", "deeppink3")
colors7 <- c("black", "deepskyblue2", "deeppink2", "olivedrab", "red", "slateblue4", "deeppink4")
colors7 <- c("black", "deepskyblue2", "orange", "olivedrab", "red", "slateblue4", "deeppink2")
colors7 <- c("black", "deepskyblue2", "deeppink2","orange", "olivedrab", "red", "slateblue4")
names7 <- c("Raw", "Quantile", "noob", "dasen", "SWAN", "BMIQ", "Funnorm")
names7 <- c("Raw", "Quantile", "Funnorm", "dasen", "SWAN", "BMIQ", "Funnorm+Noob")
names7 <- c("Raw", "Quantile", "noob","dasen", "SWAN", "BMIQ", "Funnorm")
names7 <- c("Raw", "Quantile", "Funnorm","noob","dasen", "SWAN", "BMIQ")
vector7 <- c(1, 2, 6, 4, 5, 7, 8)
vector7 <- c(1, 2, 6, 4, 5, 7, 8)
vector7 <- c(1, 2, 8, 6, 4, 5, 7)
vector7 <- c(1, 2, 3, 8, 6, 4, 5, 7)
names7 <- c("Raw", "Quantile","Funnorm", "Funnorm w/noob","noob","dasen", "SWAN", "BMIQ")
colors7 <- c("black", "deepskyblue2", "deeppink4","deeppink1","orange", "olivedrab", "red", "slateblue4")




create.dot.plot <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", 
	output.dir = "/Users/Jean-Philippe/funnorm_repro/paper_figures_filtered", 
	print = TRUE, save = TRUE) {

load(file.path(input.dir,"external_validations_filtered/data.wgbs.replication.Rda"))



setwd(output.dir)
if (save){
	pdf("Lympho_Dotplot.pdf", width=3, height=4)
	plot(1:8, 
	     data.wgbs.replication[vector7,4], 
	     bty="n", 
	     pch=20, 
	     cex=2.5, 
	     col=colors7, 
	     ylim=c(0.4,0.7)*100, 
	     xaxt="n", ylab="Overlap percentage", xlab="")
	     axis(side=1,labels=names7,las=2, at=1:8, cex.axis=0.7)
	for (i in 1:8){
		abline(v=i, lty=3, col="gray55")
	}
	abline(h= data.wgbs.replication[1,4], lty=3, lwd=2)
	dev.off()
}

if (print){
	plot(1:8, 
	     data.wgbs.replication[vector7,4], 
	     bty="n", 
	     pch=20, 
	     cex=2.5, 
	     col=colors7, 
	     ylim=c(0.4,0.7)*100, 
	     xaxt="n", ylab="Overlap percentage", xlab="")
	     axis(side=1,labels=names7,las=2, at=1:8, cex=0.1)
	for (i in 1:8){
		abline(v=i, lty=3, col="gray55")
	}
	abline(h= data.wgbs.replication[1,4], lty=3, lwd=2)
}

}


