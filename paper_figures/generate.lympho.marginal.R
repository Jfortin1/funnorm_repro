

create.main.ebv <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", 
	output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = TRUE, save = TRUE) {

	
	library(shinyMethyl)
	library(minfi)

	load(file.path(inputDir,"designs","design_ontario_ebv.Rda"))
	design <- design_ontario_ebv

	lymphoblastoid <- as.character(design$sampleName[design$group=="Lymphoblastoid"])
	lymphocyte <- as.character(design$sampleName[design$group=="Lymphocyte"])

	load(file.path(inputDir,"shinyMethylSets","shiny_dis_ontario_ebv.Rda"))
	shinyMethylSet1 <- shinyMethylSet
	load(file.path(inputDir,"shinyMethylSets","shiny_val_ontario_ebv.Rda"))
	shinyMethylSet2 <- shinyMethylSet
	shinyMethylSet <- shinyMethyl:::.shinyCombine(shinyMethylSet1, shinyMethylSet2)

	library(minfi)
	betas <- getBeta(shinyMethylSet)$II
	betas.case <- betas[, match(lymphoblastoid,colnames(betas))]
	betas.control <- betas[, match(lymphocyte,colnames(betas))]

	mean.case <- apply(betas.case, 1, mean)
	mean.control <- apply(betas.control, 1,mean)

	n=500
	target.case <- sapply(1:499, function(j) {
	        start <- mean.case[j]
	        end <- mean.case[j+1]
	        sequence <- seq(start, end,( end-start)/n)[-n]
	        return(sequence)
	    })
	        
	target.control <- sapply(1:499, function(j) {
	    start <- mean.control[j]
	    end <- mean.control[j+1]
	    sequence <- seq(start, end,( end-start)/n)[-n]
	    return(sequence)
	})

	color2 =  "deepskyblue4"
	color1 =  "orange"

	if (save) {

		setwd(output.dir)

	
		pdf("Lympho_Distributions.pdf", width = 5, height = 5)
		plot(density(target.case, from=0, to=1), ylim=c(0,5), col="white", lwd=4, bty="l" ,main="", xlab="Beta value")
		lines(density(target.control, from=0, to=1), lwd=4, col=color2)
		lines(density(target.case, from=0, to=1), lwd=4, col=color1)
		legend("topleft", c("EBV-transformed","Lymphocyte"), col=c(color1,color2), lwd=4, bty="n",cex=1.4)
		dev.off()

	}


	if (print) {

		plot(density(target.case, from=0, to=1), ylim=c(0,5), col="white", lwd=4, bty="l" ,main="", xlab="Beta value")
		lines(density(target.control, from=0, to=1), lwd=4, col=color2)
		lines(density(target.case, from=0, to=1), lwd=4, col=color1)
		legend("topleft", c("EBV-transformed","Lymphocyte"), col=c(color1,color2), lwd=4, bty="n",cex=1.4)
		
	}


}


