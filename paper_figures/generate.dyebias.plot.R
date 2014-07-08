


create.dyebias <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", 
	output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = TRUE, save = TRUE) {


	load(file.path(input.dir,"shinyMethylSets/shiny_aml.Rda"))
	library(minfi)
	library(shinyMethyl)
	library(RColorBrewer)

	extractedData <- shinyMethylSet

	sampleNames <- shinyMethylSet@sampleNames
	plate <- as.numeric(as.factor(substr(sampleNames,1,6)))
	chip <- as.numeric(as.factor(substr(sampleNames,1,10)))

	negative.green <- extractedData@greenControls$NEGATIVE
	negative.green <- colMeans(negative.green)

	negative.red <- extractedData@redControls$NEGATIVE
	negative.red <- colMeans(negative.red)

	dye.bias <- negative.green/negative.red




	if (print){
		palette(c("black","deepskyblue3"))
		plot(dye.bias, 
		     pch=20, 
		     cex=1.2, 
		     col=chip,
		     bty="n",
		     xaxt="n", ylab="Dye bias", xlab="Sample")
		abline(h=1, lty=3, lwd=2)

		palette(brewer.pal(8,"Set1"))
		# Plate effects:
		plot(negative.green, 
		     col = plate, 
		     pch=20, 
		     ylim=c(0,1200),
		     cex=1.2, 
		     bty="n", 
		     xaxt="n",
		     ylab="Mean of the negative control probes",
		     xlab="Sample")
		     
		legend(x=20, y=1200, 
		       c("Plate 1","Plate 2", "Plate 3"), 
		       col=unique(plate), 
		       pch=20,
		       bty="n")

	}

	if (save){
		setwd(output.dir)

		palette(c("black","deepskyblue3"))

		pdf("LAML_Dyebias_Effect.pdf", width=5, height = 5)
		palette(c("black","deepskyblue3"))
		# Chip effect:
		plot(dye.bias, 
		     pch=20, 
		     cex=1.2, 
		     col=chip,
		     bty="n",
		     xaxt="n", ylab="Dye bias", xlab="Sample")
		abline(h=1, lty=3, lwd=2)
		dev.off()

		pdf("LAML_Plate_Effect.pdf", width=5, height = 5)
		palette(brewer.pal(8,"Set1"))
		# Plate effects:
		plot(negative.green, 
		     col = plate, 
		     pch=20, 
		     ylim=c(0,1200),
		     cex=1.2, 
		     bty="n", 
		     xaxt="n",
		     ylab="Mean of the negative control probes",
		     xlab="Sample")
		     
		legend(x=20, y=1200, 
		       c("Plate 1","Plate 2", "Plate 3"), 
		       col=unique(plate), 
		       pch=20,
		       bty="n")
		dev.off()
	}


}